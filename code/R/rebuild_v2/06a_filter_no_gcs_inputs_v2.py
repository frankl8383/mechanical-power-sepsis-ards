#!/usr/bin/env python3
"""Outcome-blind, EOF-validated raw-input filter for rebuild_v2 no-GCS core.

The filter preserves only MAP, platelet, creatinine, and six vasoactive-drug
candidate records for the fixed-landmark at-risk targets.  MIMIC-IV's very
large tables use the audited early-column awk strategy from rebuild_v1, then
every retained gzip CSV is validated with Python's strict CSV parser.  eICU is
parsed record-by-record because quoted commas make delimiter-naive filtering
unsafe.  No mortality, discharge, status, or outcome table is opened.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import io
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time
from typing import Callable, Dict, Iterable, List, Sequence, Set, Tuple


VERSION = "rebuild_v2_no_gcs_filter_1.0.0"
MANIFEST_NAME = "filter_manifest_v2.csv"
GATE_NAME = "no_gcs_input_cache_complete_v2.csv"

MIMIC_EXPECTED_SHA256 = {
    "chartevents": "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098",
    "labevents": "2cd5e09b7e48c0189828854221f97e2e8568268eda82157c6933bf8e674d08d5",
    "inputevents": "09ffc40fade12d017debcb939225ab3924ac807625fdae27f09a260a8ee0ce48",
}
MIMIC_CHART_ITEMS = {"220052", "220181"}
MIMIC_LAB_ITEMS = {"51265", "50912"}
MIMIC_INPUT_ITEMS = {
    "221906", "221289", "222315", "221662", "221653", "221749"
}

EICU_NURSE_LABELS = {
    "Non-Invasive BP",
    "Invasive BP",
    "MAP (mmHg)",
    "Arterial Line MAP (mmHg)",
}
EICU_LAB_NAMES = {"creatinine", "platelets x 1000"}
PRESSOR_NAME_RE = re.compile(
    r"norepinephrine|levophed|epineph|adrenalin|^epi(?:\s|\()|"
    r"vasopressin|dopamine|inotropin|dobu|phenylephrine|neo[- ]?synephrine|"
    r"neosynsprine|nss with levo|nss w. levo.vaso",
    re.IGNORECASE,
)
PRESSOR_HICL = {
    "37410", "36346", "2051",
    "37407", "39089", "36437", "34361", "2050",
    "8777", "40", "2060", "2059",
    "38884", "38883", "2839",
    "37028", "35517", "35587", "2087",
}


def sha256_file(path: Path, chunk_size: int = 8 * 1024 * 1024) -> str:
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
    for value in sorted(set(values)):
        digest.update(value.encode("utf-8"))
        digest.update(b"\n")
    return digest.hexdigest()


def atomic_write_csv(
    path: Path, rows: Sequence[Dict[str, object]], fields: Sequence[str]
) -> None:
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.unlink(missing_ok=True)
    with tmp.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(tmp, path)


def validate_cache(path: Path, expected_header: Sequence[str]) -> int:
    rows = 0
    with gzip.open(path, "rt", encoding="utf-8", errors="strict", newline="") as src:
        reader = csv.reader(src, strict=True)
        try:
            header = next(reader)
        except StopIteration as exc:
            raise ValueError(f"Empty cache: {path}") from exc
        if header != list(expected_header):
            raise ValueError(f"Header mismatch for {path}: {header!r}")
        for row in reader:
            rows += 1
            if len(row) != len(header):
                raise ValueError(
                    f"CSV width mismatch in {path} row {rows + 1}: "
                    f"{len(row)} != {len(header)}"
                )
    return rows


def load_mimic_keys(path: Path) -> Tuple[Set[str], Set[str], Set[Tuple[str, str]]]:
    subjects: Set[str] = set()
    stays: Set[str] = set()
    admissions: Set[Tuple[str, str]] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, strict=True)
        if set(reader.fieldnames or []) != {"subject_id", "hadm_id", "stay_id"}:
            raise ValueError(f"Unexpected MIMIC key header: {reader.fieldnames!r}")
        for row in reader:
            s, h, u = (row["subject_id"], row["hadm_id"], row["stay_id"])
            if not s.isdigit() or not h.isdigit() or not u.isdigit():
                raise ValueError(f"Invalid MIMIC target key: {row!r}")
            subjects.add(s)
            stays.add(u)
            admissions.add((s, h))
    if not stays:
        raise ValueError("MIMIC target key file is empty")
    return subjects, stays, admissions


def load_eicu_ids(path: Path) -> Set[str]:
    ids: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        for line in handle:
            value = line.strip()
            if value:
                if not value.isdigit():
                    raise ValueError(f"Invalid eICU target ID: {value!r}")
                ids.add(value)
    if not ids:
        raise ValueError("eICU target ID file is empty")
    return ids


MIMIC_AWK = r'''
BEGIN {
  FS=",";
  while ((getline line < keyfile) > 0) {
    if (line ~ /^subject_id,hadm_id,stay_id\r?$/) continue;
    n=split(line, a, ",");
    if (n != 3 || a[1] !~ /^[0-9]+$/ || a[2] !~ /^[0-9]+$/ ||
        a[3] !~ /^[0-9]+$/) exit 41;
    stays[a[3]]=1;
    admissions[a[1] SUBSEP a[2]]=1;
  }
  close(keyfile);
  while ((getline item < itemfile) > 0) {
    sub(/\r$/, "", item);
    if (item !~ /^[0-9]+$/) exit 42;
    items[item]=1;
  }
  close(itemfile);
}
NR == 1 {
  sub(/\r$/, "");
  n=split($0, h, ",");
  if (mode == "chartevents") {
    if (n < 7 || h[1] != "subject_id" || h[2] != "hadm_id" ||
        h[3] != "stay_id" || h[7] != "itemid") exit 51;
  } else if (mode == "labevents") {
    if (n < 5 || h[1] != "labevent_id" || h[2] != "subject_id" ||
        h[3] != "hadm_id" || h[5] != "itemid") exit 52;
  } else if (mode == "inputevents") {
    if (n < 8 || h[1] != "subject_id" || h[2] != "hadm_id" ||
        h[3] != "stay_id" || h[8] != "itemid") exit 53;
  } else exit 54;
  print $0;
  next;
}
{
  scanned++;
  hit=0;
  if (mode == "chartevents") hit=(($3 in stays) && ($7 in items));
  else if (mode == "labevents")
    hit=((($2 SUBSEP $3) in admissions) && ($5 in items));
  else if (mode == "inputevents") hit=(($3 in stays) && ($8 in items));
  if (hit) { print $0; kept++; }
}
END {
  print scanned "," kept > statsfile;
  close(statsfile);
}
'''


def mimic_filter_one(
    source_name: str,
    raw_path: Path,
    output_path: Path,
    itemids: Set[str],
    key_path: Path,
    expected_header: Sequence[str],
) -> Dict[str, object]:
    started = time.time()
    output_tmp = output_path.with_suffix(output_path.suffix + ".tmp")
    stats_tmp = output_path.with_suffix(output_path.suffix + ".stats.tmp")
    output_tmp.unlink(missing_ok=True)
    stats_tmp.unlink(missing_ok=True)
    with tempfile.TemporaryDirectory(prefix="no_gcs_mimic_filter_") as td:
        td_path = Path(td)
        awk_path = td_path / "filter.awk"
        item_path = td_path / "itemids.txt"
        awk_path.write_text(MIMIC_AWK, encoding="utf-8")
        item_path.write_text("\n".join(sorted(itemids, key=int)) + "\n", encoding="ascii")
        command = (
            "set -euo pipefail; gzip -cd \"$1\" | "
            "LC_ALL=C awk -v keyfile=\"$2\" -v itemfile=\"$3\" "
            "-v mode=\"$4\" -v statsfile=\"$5\" -f \"$6\" | "
            "gzip -n > \"$7\""
        )
        completed = subprocess.run(
            [
                "/bin/bash", "-c", command, "no-gcs-filter",
                str(raw_path), str(key_path), str(item_path), source_name,
                str(stats_tmp), str(awk_path), str(output_tmp),
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if completed.returncode != 0:
            output_tmp.unlink(missing_ok=True)
            stats_tmp.unlink(missing_ok=True)
            raise RuntimeError(
                f"MIMIC filter failed ({source_name}, {completed.returncode}): "
                f"{completed.stderr.strip()}"
            )
    parts = stats_tmp.read_text(encoding="ascii").strip().split(",")
    stats_tmp.unlink()
    if len(parts) != 2 or not all(x.isdigit() for x in parts):
        raise ValueError(f"Malformed MIMIC filter stats: {parts!r}")
    scanned, kept = map(int, parts)
    logical_kept = validate_cache(output_tmp, expected_header)
    if logical_kept != kept:
        raise RuntimeError(f"Physical/logical retained mismatch: {kept} != {logical_kept}")
    raw_sha = sha256_file(raw_path)
    expected_sha = MIMIC_EXPECTED_SHA256[source_name]
    if raw_sha != expected_sha:
        raise RuntimeError(f"Official MIMIC SHA mismatch for {source_name}")
    os.replace(output_tmp, output_path)
    stat = raw_path.stat()
    return {
        "source_name": source_name,
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "raw_sha256": raw_sha,
        "official_sha256_match": "TRUE",
        "scanned_rows": scanned,
        "kept_rows": logical_kept,
        "reached_eof": "TRUE",
        "filter_spec": f"target_key AND itemid IN {sorted(itemids, key=int)}",
        "output_columns": ";".join(expected_header),
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def canonical_gzip_writer(path: Path):
    raw = path.open("wb")
    gz = gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0)
    text = io.TextIOWrapper(gz, encoding="utf-8", newline="")
    return raw, gz, text


def eicu_filter_one(
    source_name: str,
    raw_path: Path,
    output_path: Path,
    target_ids: Set[str],
    output_columns: Sequence[str],
    predicate: Callable[[Dict[str, str]], bool],
    filter_spec: str,
) -> Dict[str, object]:
    started = time.time()
    tmp = output_path.with_suffix(output_path.suffix + ".tmp")
    tmp.unlink(missing_ok=True)
    scanned = kept = 0
    raw_handle = gz_handle = text_handle = None
    try:
        with gzip.open(raw_path, "rt", encoding="utf-8", errors="strict", newline="") as src:
            reader = csv.reader(src, strict=True)
            header = next(reader)
            if len(header) != len(set(header)):
                raise ValueError(f"Duplicate eICU header in {raw_path}")
            missing = [x for x in output_columns if x not in header]
            if "patientunitstayid" not in header:
                missing.append("patientunitstayid")
            if missing:
                raise ValueError(f"Missing columns in {raw_path}: {sorted(set(missing))}")
            positions = {name: i for i, name in enumerate(header)}
            selected = [positions[x] for x in output_columns]
            raw_handle, gz_handle, text_handle = canonical_gzip_writer(tmp)
            writer = csv.writer(text_handle, lineterminator="\n")
            writer.writerow(output_columns)
            for row in reader:
                scanned += 1
                if len(row) != len(header):
                    raise ValueError(f"CSV width mismatch in {raw_path} row {scanned + 1}")
                if row[positions["patientunitstayid"]] not in target_ids:
                    continue
                record = {name: row[i] for name, i in positions.items()}
                if predicate(record):
                    writer.writerow([row[i] for i in selected])
                    kept += 1
            text_handle.flush()
            text_handle.detach()
            text_handle = None
            gz_handle.close()
            gz_handle = None
            raw_handle.close()
            raw_handle = None
        logical_kept = validate_cache(tmp, output_columns)
        if logical_kept != kept:
            raise RuntimeError(f"eICU retained-row mismatch: {logical_kept} != {kept}")
        os.replace(tmp, output_path)
    except Exception:
        for handle in (text_handle, gz_handle, raw_handle):
            if handle is not None:
                try:
                    handle.close()
                except Exception:
                    pass
        tmp.unlink(missing_ok=True)
        raise
    stat = raw_path.stat()
    return {
        "source_name": source_name,
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "raw_sha256": sha256_file(raw_path),
        "official_sha256_match": "NA",
        "scanned_rows": scanned,
        "kept_rows": kept,
        "reached_eof": "TRUE",
        "filter_spec": filter_spec,
        "output_columns": ";".join(output_columns),
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def manifest_valid(
    manifest_path: Path,
    gate_path: Path,
    specs: Sequence[Dict[str, object]],
    keys_sha: str,
    helper_sha: str,
) -> bool:
    if not manifest_path.exists() or not gate_path.exists():
        return False
    try:
        with gate_path.open("r", encoding="utf-8", newline="") as handle:
            gates = list(csv.DictReader(handle, strict=True))
        if len(gates) != 1 or gates[0].get("status") != "PASS":
            return False
        gate = gates[0]
        if gate.get("target_keys_sha256") != keys_sha:
            return False
        if gate.get("helper_sha256") != helper_sha:
            return False
        with manifest_path.open("r", encoding="utf-8", newline="") as handle:
            rows = list(csv.DictReader(handle, strict=True))
        by_name = {row["source_name"]: row for row in rows}
        if len(by_name) != len(specs):
            return False
        for spec in specs:
            name = str(spec["source_name"])
            row = by_name.get(name)
            if row is None or row.get("status") != "PASS":
                return False
            raw_path = Path(str(spec["raw_path"]))
            out_path = Path(str(spec["output_path"]))
            if not raw_path.exists() or not out_path.exists():
                return False
            stat = raw_path.stat()
            if int(row["raw_size"]) != stat.st_size:
                return False
            if int(row["raw_mtime_ns"]) != stat.st_mtime_ns:
                return False
            if row["raw_sha256"] != sha256_file(raw_path):
                return False
            if row["output_sha256"] != sha256_file(out_path):
                return False
            if validate_cache(out_path, spec["output_columns"]) != int(row["kept_rows"]):
                return False
        return True
    except Exception:
        return False


def make_specs(database: str, raw_root: Path, cache_dir: Path):
    if database == "mimic":
        return [
            {
                "source_name": "chartevents",
                "raw_path": raw_root / "icu" / "chartevents.csv.gz",
                "output_path": cache_dir / "chartevents_map_candidates_v2.csv.gz",
                "itemids": MIMIC_CHART_ITEMS,
                "output_columns": [
                    "subject_id", "hadm_id", "stay_id", "caregiver_id", "charttime",
                    "storetime", "itemid", "value", "valuenum", "valueuom", "warning",
                ],
            },
            {
                "source_name": "labevents",
                "raw_path": raw_root / "hosp" / "labevents.csv.gz",
                "output_path": cache_dir / "labevents_core_candidates_v2.csv.gz",
                "itemids": MIMIC_LAB_ITEMS,
                "output_columns": [
                    "labevent_id", "subject_id", "hadm_id", "specimen_id", "itemid",
                    "order_provider_id", "charttime", "storetime", "value", "valuenum",
                    "valueuom", "ref_range_lower", "ref_range_upper", "flag", "priority",
                    "comments",
                ],
            },
            {
                "source_name": "inputevents",
                "raw_path": raw_root / "icu" / "inputevents.csv.gz",
                "output_path": cache_dir / "inputevents_pressor_candidates_v2.csv.gz",
                "itemids": MIMIC_INPUT_ITEMS,
                "output_columns": [
                    "subject_id", "hadm_id", "stay_id", "caregiver_id", "starttime",
                    "endtime", "storetime", "itemid", "amount", "amountuom", "rate",
                    "rateuom", "orderid", "linkorderid", "ordercategoryname",
                    "secondaryordercategoryname", "ordercomponenttypedescription",
                    "ordercategorydescription", "patientweight", "totalamount",
                    "totalamountuom", "isopenbag", "continueinnextdept",
                    "statusdescription", "originalamount", "originalrate",
                ],
            },
        ]
    return [
        {
            "source_name": "nurseCharting",
            "raw_path": raw_root / "nurseCharting.csv.gz",
            "output_path": cache_dir / "nurse_map_candidates_v2.csv.gz",
            "output_columns": [
                "nursingchartid", "patientunitstayid", "nursingchartoffset",
                "nursingchartentryoffset", "nursingchartcelltypecat",
                "nursingchartcelltypevallabel", "nursingchartcelltypevalname",
                "nursingchartvalue",
            ],
            "predicate": lambda row: row["nursingchartcelltypevallabel"] in EICU_NURSE_LABELS,
            "filter_spec": "target ID AND exact MAP label",
        },
        {
            "source_name": "lab",
            "raw_path": raw_root / "lab.csv.gz",
            "output_path": cache_dir / "lab_core_candidates_v2.csv.gz",
            "output_columns": [
                "labid", "patientunitstayid", "labresultoffset", "labname",
                "labresult", "labmeasurenamesystem", "labmeasurenameinterface",
                "labresultrevisedoffset",
            ],
            "predicate": lambda row: row["labname"] in EICU_LAB_NAMES,
            "filter_spec": "target ID AND exact platelet/creatinine labname",
        },
        {
            "source_name": "infusionDrug",
            "raw_path": raw_root / "infusionDrug.csv.gz",
            "output_path": cache_dir / "infusion_pressor_candidates_v2.csv.gz",
            "output_columns": [
                "infusiondrugid", "patientunitstayid", "infusionoffset",
                "drugname", "drugrate", "infusionrate", "drugamount",
                "volumeoffluid",
            ],
            "predicate": lambda row: PRESSOR_NAME_RE.search(row["drugname"] or "") is not None,
            "filter_spec": "target ID AND audited six-drug name regex",
        },
        {
            "source_name": "medication",
            "raw_path": raw_root / "medication.csv.gz",
            "output_path": cache_dir / "medication_pressor_candidates_v2.csv.gz",
            "output_columns": [
                "medicationid", "patientunitstayid", "drugorderoffset",
                "drugstartoffset", "drugivadmixture", "drugordercancelled",
                "drugname", "drughiclseqno", "dosage", "routeadmin", "prn",
                "drugstopoffset",
            ],
            "predicate": lambda row: (
                PRESSOR_NAME_RE.search(row["drugname"] or "") is not None
                or (row.get("drughiclseqno") or "").strip() in PRESSOR_HICL
            ),
            "filter_spec": "target ID AND (audited name regex OR audited HICL)",
        },
    ]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--database", choices=["mimic", "eicu"], required=True)
    parser.add_argument("--keys", type=Path, required=True)
    parser.add_argument("--raw-root", type=Path, required=True)
    parser.add_argument("--cache-dir", type=Path, required=True)
    args = parser.parse_args()

    cache_dir = args.cache_dir.resolve()
    cache_dir.mkdir(parents=True, exist_ok=True)
    helper_sha = sha256_file(Path(__file__).resolve())
    if args.database == "mimic":
        _, stays, admissions = load_mimic_keys(args.keys)
        target_count = len(stays)
        keys_sha = sha256_lines(
            [f"stay:{x}" for x in stays]
            + [f"admission:{x}:{y}" for x, y in admissions]
        )
        target_ids = None
    else:
        target_ids = load_eicu_ids(args.keys)
        target_count = len(target_ids)
        keys_sha = sha256_lines(target_ids)

    specs = make_specs(args.database, args.raw_root.resolve(), cache_dir)
    for spec in specs:
        if not Path(str(spec["raw_path"])).exists():
            raise FileNotFoundError(spec["raw_path"])
    manifest_path = cache_dir / MANIFEST_NAME
    gate_path = cache_dir / GATE_NAME
    if manifest_valid(manifest_path, gate_path, specs, keys_sha, helper_sha):
        print(f"CACHE_HIT {gate_path}")
        return 0

    gate_path.unlink(missing_ok=True)
    manifest_path.unlink(missing_ok=True)
    results: List[Dict[str, object]] = []
    for spec in specs:
        if args.database == "mimic":
            result = mimic_filter_one(
                str(spec["source_name"]),
                Path(str(spec["raw_path"])),
                Path(str(spec["output_path"])),
                set(spec["itemids"]),
                args.keys,
                list(spec["output_columns"]),
            )
        else:
            assert target_ids is not None
            result = eicu_filter_one(
                str(spec["source_name"]),
                Path(str(spec["raw_path"])),
                Path(str(spec["output_path"])),
                target_ids,
                list(spec["output_columns"]),
                spec["predicate"],
                str(spec["filter_spec"]),
            )
        result.update(
            {
                "database": args.database,
                "target_count": target_count,
                "target_keys_sha256": keys_sha,
                "helper_version": VERSION,
                "helper_sha256": helper_sha,
            }
        )
        results.append(result)
        print(
            f"FILTERED {spec['source_name']} scanned={result['scanned_rows']} "
            f"kept={result['kept_rows']}",
            flush=True,
        )

    fields = [
        "database", "source_name", "raw_path", "raw_size", "raw_mtime_ns",
        "raw_sha256", "official_sha256_match", "scanned_rows", "kept_rows",
        "reached_eof", "filter_spec", "output_columns", "output_path",
        "output_size", "output_sha256", "target_count", "target_keys_sha256",
        "helper_version", "helper_sha256", "elapsed_seconds", "status",
    ]
    atomic_write_csv(manifest_path, results, fields)
    gate = {
        "status": "PASS",
        "database": args.database,
        "completed_at": time.strftime("%Y-%m-%d %H:%M:%S %z"),
        "target_count": target_count,
        "target_keys_sha256": keys_sha,
        "helper_version": VERSION,
        "helper_sha256": helper_sha,
        "source_count": len(specs),
        "manifest_sha256": sha256_file(manifest_path),
        "mapping_source_v1": json.dumps(
            {
                "mimic_R": "code/R/rebuild_v1/05_build_mimic_severity_core.R",
                "eicu_R": "code/R/rebuild_v1/06_build_eicu_severity_core.R",
            },
            separators=(",", ":"),
        ),
    }
    atomic_write_csv(gate_path, [gate], list(gate))
    print(f"CACHE_COMPLETE {gate_path}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:
        print(f"FATAL: {type(exc).__name__}: {exc}", file=os.sys.stderr)
        raise
