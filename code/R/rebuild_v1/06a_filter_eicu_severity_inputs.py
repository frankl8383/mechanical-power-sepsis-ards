#!/usr/bin/env python3
"""CSV-aware, outcome-blind eICU severity-input filter.

The eICU CSV files contain quoted commas. They must never be filtered with a
delimiter-naive awk pipeline. This helper uses Python's standard-library CSV
parser, scans every logical record to EOF, and atomically publishes canonical
gzip CSV caches plus a provenance manifest and completion gate.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import io
import os
from pathlib import Path
import re
import sys
import time
from typing import Callable, Dict, Iterable, List, Sequence, Set


VERSION = "eicu_severity_csv_filter_v1"
MANIFEST_NAME = "filter_manifest_v1.csv"
GATE_NAME = "severity_input_cache_complete_v1.csv"

NURSE_LABELS = {
    "Glasgow coma score",
    "Score (Glasgow Coma Scale)",
    "Non-Invasive BP",
    "Invasive BP",
    "MAP (mmHg)",
    "Arterial Line MAP (mmHg)",
}
LAB_NAMES = {"creatinine", "platelets x 1000"}
PRESSOR_NAME_RE = re.compile(
    r"norepinephrine|levophed|epineph|adrenalin|^epi(?:\s|\()|"
    r"vasopressin|dopamine|inotropin|dobu|phenylephrine|neo[- ]?synephrine|"
    r"neosynsprine|nss with levo|nss w. levo.vaso",
    re.IGNORECASE,
)
PRESSOR_HICL = {
    "37410", "36346", "2051",  # norepinephrine
    "37407", "39089", "36437", "34361", "2050",  # epinephrine
    "8777", "40",  # dobutamine
    "2060", "2059",  # dopamine
    "38884", "38883", "2839",  # vasopressin
    "37028", "35517", "35587", "2087",  # phenylephrine
}


def sha256_file(path: Path, chunk_size: int = 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(chunk_size)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def sha256_lines(values: Iterable[str]) -> str:
    digest = hashlib.sha256()
    for value in sorted(set(values), key=lambda x: int(x)):
        digest.update(value.encode("ascii"))
        digest.update(b"\n")
    return digest.hexdigest()


def load_ids(path: Path) -> Set[str]:
    ids: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        for line in handle:
            value = line.strip()
            if not value:
                continue
            if not value.isdigit():
                raise ValueError(f"Non-numeric patientunitstayid in {path}: {value!r}")
            ids.add(value)
    if not ids:
        raise ValueError("Strict-ID file is empty")
    return ids


def canonical_gzip_writer(path: Path):
    raw = path.open("wb")
    gz = gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0)
    text = io.TextIOWrapper(gz, encoding="utf-8", newline="")
    return raw, gz, text


def scan_filter(
    raw_path: Path,
    output_path: Path,
    strict_ids: Set[str],
    output_columns: Sequence[str],
    predicate: Callable[[Dict[str, str]], bool],
) -> Dict[str, object]:
    tmp_path = output_path.with_suffix(output_path.suffix + ".tmp")
    tmp_path.unlink(missing_ok=True)
    scanned_rows = 0
    kept_rows = 0
    reached_eof = False
    started = time.time()

    raw_handle = gz_handle = text_handle = None
    try:
        with gzip.open(
            raw_path, "rt", encoding="utf-8", errors="strict", newline=""
        ) as source:
            reader = csv.reader(source, strict=True)
            try:
                header = next(reader)
            except StopIteration as exc:
                raise ValueError(f"Empty gzip CSV: {raw_path}") from exc
            if len(header) != len(set(header)):
                raise ValueError(f"Duplicate header field in {raw_path}")
            missing = [name for name in output_columns if name not in header]
            required = ["patientunitstayid"]
            missing += [name for name in required if name not in header]
            if missing:
                raise ValueError(f"Missing fields in {raw_path}: {sorted(set(missing))}")
            positions = {name: header.index(name) for name in header}
            selected_positions = [positions[name] for name in output_columns]

            raw_handle, gz_handle, text_handle = canonical_gzip_writer(tmp_path)
            writer = csv.writer(
                text_handle, quoting=csv.QUOTE_MINIMAL, lineterminator="\n"
            )
            writer.writerow(output_columns)

            for row in reader:
                scanned_rows += 1
                if len(row) != len(header):
                    raise ValueError(
                        f"CSV width mismatch in {raw_path} at logical row "
                        f"{scanned_rows + 1}: {len(row)} != {len(header)}"
                    )
                stay_id = row[positions["patientunitstayid"]]
                if stay_id not in strict_ids:
                    continue
                record = {name: row[pos] for name, pos in positions.items()}
                if not predicate(record):
                    continue
                writer.writerow([row[pos] for pos in selected_positions])
                kept_rows += 1
            reached_eof = True

            text_handle.flush()
            text_handle.detach()
            text_handle = None
            gz_handle.close()
            gz_handle = None
            raw_handle.close()
            raw_handle = None

        if not reached_eof:
            raise RuntimeError(f"EOF was not reached for {raw_path}")
        os.replace(tmp_path, output_path)
    except Exception:
        if text_handle is not None:
            try:
                text_handle.close()
            except Exception:
                pass
        if gz_handle is not None:
            try:
                gz_handle.close()
            except Exception:
                pass
        if raw_handle is not None:
            try:
                raw_handle.close()
            except Exception:
                pass
        tmp_path.unlink(missing_ok=True)
        raise

    stat = raw_path.stat()
    return {
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "scanned_rows": scanned_rows,
        "kept_rows": kept_rows,
        "reached_eof": "TRUE" if reached_eof else "FALSE",
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def manifest_is_valid(
    manifest_path: Path,
    gate_path: Path,
    expected_specs: Sequence[Dict[str, object]],
    ids_sha256: str,
    helper_sha256: str,
) -> bool:
    if not manifest_path.exists() or not gate_path.exists():
        return False
    try:
        with gate_path.open("r", encoding="utf-8", newline="") as handle:
            gate_rows = list(csv.DictReader(handle))
        if len(gate_rows) != 1 or gate_rows[0].get("status") != "PASS":
            return False
        gate = gate_rows[0]
        if gate.get("strict_ids_sha256") != ids_sha256:
            return False
        if gate.get("helper_sha256") != helper_sha256:
            return False
        if int(gate.get("spec_count", "-1")) != len(expected_specs):
            return False

        with manifest_path.open("r", encoding="utf-8", newline="") as handle:
            rows = list(csv.DictReader(handle))
        by_name = {row["source_name"]: row for row in rows}
        if len(by_name) != len(expected_specs):
            return False
        for spec in expected_specs:
            row = by_name.get(str(spec["source_name"]))
            if row is None or row.get("status") != "PASS":
                return False
            raw_path = Path(str(spec["raw_path"]))
            output_path = Path(str(spec["output_path"]))
            if not raw_path.exists() or not output_path.exists():
                return False
            stat = raw_path.stat()
            if int(row["raw_size"]) != stat.st_size:
                return False
            if int(row["raw_mtime_ns"]) != stat.st_mtime_ns:
                return False
            if row.get("filter_spec") != spec["filter_spec"]:
                return False
            if row.get("output_columns") != ";".join(spec["output_columns"]):
                return False
            if row.get("reached_eof") != "TRUE":
                return False
            if int(row["output_size"]) != output_path.stat().st_size:
                return False
            if row["output_sha256"] != sha256_file(output_path):
                return False
        return True
    except Exception:
        return False


def atomic_write_csv(path: Path, rows: Sequence[Dict[str, object]], fields: Sequence[str]):
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.unlink(missing_ok=True)
    with tmp.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(tmp, path)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--ids", required=True, type=Path)
    parser.add_argument("--eicu-root", required=True, type=Path)
    parser.add_argument("--cache-dir", required=True, type=Path)
    args = parser.parse_args()

    cache_dir = args.cache_dir.resolve()
    cache_dir.mkdir(parents=True, exist_ok=True)
    strict_ids = load_ids(args.ids)
    ids_sha256 = sha256_lines(strict_ids)
    helper_path = Path(__file__).resolve()
    helper_sha256 = sha256_file(helper_path)

    def nurse_predicate(row: Dict[str, str]) -> bool:
        return row["nursingchartcelltypevallabel"] in NURSE_LABELS

    def lab_predicate(row: Dict[str, str]) -> bool:
        return row["labname"] in LAB_NAMES

    def infusion_predicate(row: Dict[str, str]) -> bool:
        return PRESSOR_NAME_RE.search(row["drugname"] or "") is not None

    def medication_predicate(row: Dict[str, str]) -> bool:
        name_hit = PRESSOR_NAME_RE.search(row["drugname"] or "") is not None
        hicl_hit = (row.get("drughiclseqno") or "").strip() in PRESSOR_HICL
        return name_hit or hicl_hit

    specs: List[Dict[str, object]] = [
        {
            "source_name": "nurseCharting",
            "raw_path": args.eicu_root / "nurseCharting.csv.gz",
            "output_path": cache_dir / "nurse_severity_candidates_v1.csv.gz",
            "output_columns": [
                "nursingchartid", "patientunitstayid", "nursingchartoffset",
                "nursingchartentryoffset", "nursingchartcelltypecat",
                "nursingchartcelltypevallabel", "nursingchartcelltypevalname",
                "nursingchartvalue",
            ],
            "filter_spec": "strict_id AND exact label in NURSE_LABELS_v1",
            "predicate": nurse_predicate,
        },
        {
            "source_name": "lab",
            "raw_path": args.eicu_root / "lab.csv.gz",
            "output_path": cache_dir / "lab_severity_candidates_v1.csv.gz",
            "output_columns": [
                "labid", "patientunitstayid", "labresultoffset", "labname",
                "labresult", "labmeasurenamesystem", "labmeasurenameinterface",
                "labresultrevisedoffset",
            ],
            "filter_spec": "strict_id AND exact labname in LAB_NAMES_v1",
            "predicate": lab_predicate,
        },
        {
            "source_name": "infusionDrug",
            "raw_path": args.eicu_root / "infusionDrug.csv.gz",
            "output_path": cache_dir / "infusion_severity_candidates_v1.csv.gz",
            "output_columns": [
                "infusiondrugid", "patientunitstayid", "infusionoffset",
                "drugname", "drugrate", "infusionrate", "drugamount",
                "volumeoffluid",
            ],
            "filter_spec": "strict_id AND PRESSOR_NAME_RE_v1",
            "predicate": infusion_predicate,
        },
        {
            "source_name": "medication",
            "raw_path": args.eicu_root / "medication.csv.gz",
            "output_path": cache_dir / "medication_severity_candidates_v1.csv.gz",
            "output_columns": [
                "medicationid", "patientunitstayid", "drugorderoffset",
                "drugstartoffset", "drugivadmixture", "drugordercancelled",
                "drugname", "drughiclseqno", "dosage", "routeadmin", "prn",
                "drugstopoffset",
            ],
            "filter_spec": "strict_id AND (PRESSOR_NAME_RE_v1 OR PRESSOR_HICL_v1)",
            "predicate": medication_predicate,
        },
    ]

    for spec in specs:
        raw_path = Path(str(spec["raw_path"]))
        if not raw_path.exists():
            raise FileNotFoundError(raw_path)

    manifest_path = cache_dir / MANIFEST_NAME
    gate_path = cache_dir / GATE_NAME
    if manifest_is_valid(
        manifest_path, gate_path, specs, ids_sha256, helper_sha256
    ):
        print(f"CACHE_HIT {gate_path}")
        return 0

    gate_path.unlink(missing_ok=True)
    manifest_path.unlink(missing_ok=True)
    (cache_dir / (GATE_NAME + ".tmp")).unlink(missing_ok=True)
    (cache_dir / (MANIFEST_NAME + ".tmp")).unlink(missing_ok=True)

    results: List[Dict[str, object]] = []
    try:
        for spec in specs:
            result = scan_filter(
                Path(str(spec["raw_path"])),
                Path(str(spec["output_path"])),
                strict_ids,
                list(spec["output_columns"]),
                spec["predicate"],
            )
            result.update(
                {
                    "source_name": spec["source_name"],
                    "filter_spec": spec["filter_spec"],
                    "output_columns": ";".join(spec["output_columns"]),
                    "strict_id_count": len(strict_ids),
                    "strict_ids_sha256": ids_sha256,
                    "helper_version": VERSION,
                    "helper_sha256": helper_sha256,
                }
            )
            results.append(result)
            print(
                f"FILTERED {spec['source_name']} scanned={result['scanned_rows']} "
                f"kept={result['kept_rows']}",
                flush=True,
            )
    except Exception:
        gate_path.unlink(missing_ok=True)
        raise

    manifest_fields = [
        "source_name", "raw_path", "raw_size", "raw_mtime_ns",
        "scanned_rows", "kept_rows", "reached_eof", "filter_spec",
        "output_columns", "output_path", "output_size", "output_sha256",
        "strict_id_count", "strict_ids_sha256", "helper_version",
        "helper_sha256", "elapsed_seconds", "status",
    ]
    atomic_write_csv(manifest_path, results, manifest_fields)
    gate = {
        "status": "PASS",
        "completed_at": time.strftime("%Y-%m-%d %H:%M:%S %z"),
        "strict_id_count": len(strict_ids),
        "strict_ids_sha256": ids_sha256,
        "helper_version": VERSION,
        "helper_sha256": helper_sha256,
        "spec_count": len(specs),
        "manifest_sha256": sha256_file(manifest_path),
    }
    atomic_write_csv(gate_path, [gate], list(gate.keys()))
    print(f"CACHE_COMPLETE {gate_path}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"FATAL: {type(exc).__name__}: {exc}", file=sys.stderr)
        raise
