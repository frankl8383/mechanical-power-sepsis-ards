#!/usr/bin/env python3
"""Outcome-blind raw-source filter for rebuild-v2 complete-GCS sensitivity.

Only GCS candidate records for the full fixed-6-hour tuple targets are
retained. MIMIC-IV is filtered with the already audited early-column awk
strategy and eICU is parsed with Python's CSV reader because quoted commas
make delimiter-naive filtering unsafe. Both source tables are read through
EOF, source hashes are checked against the local official manifests, and every
published gzip CSV is reparsed strictly before its completion gate is written.
No outcome, death, discharge, or status table is opened.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import io
import os
from pathlib import Path
import subprocess
import tempfile
import time
from typing import Dict, Iterable, List, Sequence, Set


VERSION = "rebuild_v2_complete_gcs_filter_1.0.0"
MANIFEST_NAME = "complete_gcs_filter_manifest_v2.csv"
GATE_NAME = "complete_gcs_filter_complete_v2.csv"

MIMIC_EXPECTED_SHA256 = (
    "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098"
)
EICU_EXPECTED_SHA256 = (
    "d10444dc6b530dfd198b42f2841de7b76045570fbf10b08e991c133006661c2c"
)
MIMIC_GCS_ITEMS = {"220739", "223900", "223901"}
EICU_GCS_LABEL_NAME = {
    ("Glasgow coma score", "GCS Total"),
    ("Score (Glasgow Coma Scale)", "Value"),
    ("Glasgow coma score", "Eyes"),
    ("Glasgow coma score", "Verbal"),
    ("Glasgow coma score", "Motor"),
}

MIMIC_OUTPUT_COLUMNS = [
    "subject_id",
    "hadm_id",
    "stay_id",
    "caregiver_id",
    "charttime",
    "storetime",
    "itemid",
    "value",
    "valuenum",
    "valueuom",
    "warning",
]
EICU_OUTPUT_COLUMNS = [
    "nursingchartid",
    "patientunitstayid",
    "nursingchartoffset",
    "nursingchartentryoffset",
    "nursingchartcelltypecat",
    "nursingchartcelltypevallabel",
    "nursingchartcelltypevalname",
    "nursingchartvalue",
]


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
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.unlink(missing_ok=True)
    with temporary.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle, fieldnames=fields, lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(rows)
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, path)


def validate_cache(path: Path, expected_header: Sequence[str]) -> int:
    rows = 0
    with gzip.open(
        path, "rt", encoding="utf-8", errors="strict", newline=""
    ) as source:
        reader = csv.reader(source, strict=True)
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


def canonical_gzip_writer(path: Path):
    raw = path.open("wb")
    compressed = gzip.GzipFile(
        filename="", mode="wb", fileobj=raw, mtime=0
    )
    text = io.TextIOWrapper(compressed, encoding="utf-8", newline="")
    return raw, compressed, text


def load_mimic_keys(path: Path) -> Set[str]:
    stays: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, strict=True)
        if list(reader.fieldnames or []) != [
            "subject_id", "hadm_id", "stay_id"
        ]:
            raise ValueError(
                f"Unexpected MIMIC target-key header: {reader.fieldnames!r}"
            )
        for row in reader:
            values = [row["subject_id"], row["hadm_id"], row["stay_id"]]
            if any(not value.isdigit() for value in values):
                raise ValueError(f"Invalid MIMIC target key: {row!r}")
            stays.add(row["stay_id"])
    if not stays:
        raise ValueError("MIMIC target-key file is empty")
    return stays


def load_eicu_ids(path: Path) -> Set[str]:
    ids: Set[str] = set()
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            value = line.strip()
            if value:
                if not value.isdigit():
                    raise ValueError(f"Invalid eICU target ID: {value!r}")
                ids.add(value)
    if not ids:
        raise ValueError("eICU target-ID file is empty")
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
  if (n < 7 || h[1] != "subject_id" || h[2] != "hadm_id" ||
      h[3] != "stay_id" || h[7] != "itemid") exit 51;
  print $0;
  next;
}
{
  scanned++;
  if (($3 in stays) && ($7 in items)) {
    print $0;
    kept++;
  }
}
END {
  print scanned "," kept > statsfile;
  close(statsfile);
}
'''


def filter_mimic(
    raw_path: Path,
    output_path: Path,
    key_path: Path,
) -> Dict[str, object]:
    started = time.time()
    output_tmp = output_path.with_suffix(output_path.suffix + ".tmp")
    stats_tmp = output_path.with_suffix(output_path.suffix + ".stats.tmp")
    output_tmp.unlink(missing_ok=True)
    stats_tmp.unlink(missing_ok=True)
    with tempfile.TemporaryDirectory(prefix="complete_gcs_mimic_") as td:
        td_path = Path(td)
        awk_path = td_path / "filter.awk"
        item_path = td_path / "itemids.txt"
        awk_path.write_text(MIMIC_AWK, encoding="utf-8")
        item_path.write_text(
            "\n".join(sorted(MIMIC_GCS_ITEMS, key=int)) + "\n",
            encoding="ascii",
        )
        command = (
            "set -euo pipefail; gzip -cd \"$1\" | "
            "LC_ALL=C awk -v keyfile=\"$2\" -v itemfile=\"$3\" "
            "-v statsfile=\"$4\" -f \"$5\" | gzip -n > \"$6\""
        )
        completed = subprocess.run(
            [
                "/bin/bash",
                "-c",
                command,
                "complete-gcs-filter",
                str(raw_path),
                str(key_path),
                str(item_path),
                str(stats_tmp),
                str(awk_path),
                str(output_tmp),
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
                f"MIMIC GCS filter failed ({completed.returncode}): "
                f"{completed.stderr.strip()}"
            )
    values = stats_tmp.read_text(encoding="ascii").strip().split(",")
    stats_tmp.unlink()
    if len(values) != 2 or any(not value.isdigit() for value in values):
        raise ValueError(f"Malformed MIMIC filter stats: {values!r}")
    scanned, kept = map(int, values)
    logical_kept = validate_cache(output_tmp, MIMIC_OUTPUT_COLUMNS)
    if logical_kept != kept:
        raise RuntimeError(
            f"MIMIC retained-row mismatch: {logical_kept} != {kept}"
        )
    raw_hash = sha256_file(raw_path)
    if raw_hash != MIMIC_EXPECTED_SHA256:
        raise RuntimeError("Official MIMIC chartevents SHA256 mismatch")
    os.replace(output_tmp, output_path)
    stat = raw_path.stat()
    return {
        "database": "mimic",
        "source_name": "chartevents",
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "raw_sha256": raw_hash,
        "expected_sha256": MIMIC_EXPECTED_SHA256,
        "official_sha256_match": "TRUE",
        "scanned_rows": scanned,
        "kept_rows": logical_kept,
        "reached_eof": "TRUE",
        "filter_spec": (
            "target stay_id AND itemid IN 220739,223900,223901"
        ),
        "output_columns": ";".join(MIMIC_OUTPUT_COLUMNS),
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def filter_eicu(
    raw_path: Path,
    output_path: Path,
    target_ids: Set[str],
) -> Dict[str, object]:
    started = time.time()
    temporary = output_path.with_suffix(output_path.suffix + ".tmp")
    temporary.unlink(missing_ok=True)
    scanned = kept = 0
    raw_handle = compressed_handle = text_handle = None
    try:
        with gzip.open(
            raw_path, "rt", encoding="utf-8", errors="strict", newline=""
        ) as source:
            reader = csv.reader(source, strict=True)
            header = next(reader)
            if len(header) != len(set(header)):
                raise ValueError("Duplicate eICU nurseCharting header")
            missing = [
                column
                for column in EICU_OUTPUT_COLUMNS
                if column not in header
            ]
            if missing:
                raise ValueError(
                    f"Missing eICU source columns: {missing!r}"
                )
            positions = {name: index for index, name in enumerate(header)}
            selected = [positions[name] for name in EICU_OUTPUT_COLUMNS]
            raw_handle, compressed_handle, text_handle = (
                canonical_gzip_writer(temporary)
            )
            writer = csv.writer(text_handle, lineterminator="\n")
            writer.writerow(EICU_OUTPUT_COLUMNS)
            for row in reader:
                scanned += 1
                if len(row) != len(header):
                    raise ValueError(
                        "eICU nurseCharting CSV width mismatch at row "
                        f"{scanned + 1}"
                    )
                stay_id = row[positions["patientunitstayid"]]
                if stay_id not in target_ids:
                    continue
                key = (
                    row[positions["nursingchartcelltypevallabel"]],
                    row[positions["nursingchartcelltypevalname"]],
                )
                if key not in EICU_GCS_LABEL_NAME:
                    continue
                writer.writerow([row[index] for index in selected])
                kept += 1
            text_handle.flush()
            text_handle.detach()
            text_handle = None
            compressed_handle.close()
            compressed_handle = None
            raw_handle.close()
            raw_handle = None
        logical_kept = validate_cache(temporary, EICU_OUTPUT_COLUMNS)
        if logical_kept != kept:
            raise RuntimeError(
                f"eICU retained-row mismatch: {logical_kept} != {kept}"
            )
        raw_hash = sha256_file(raw_path)
        if raw_hash != EICU_EXPECTED_SHA256:
            raise RuntimeError("Official eICU nurseCharting SHA256 mismatch")
        os.replace(temporary, output_path)
    except Exception:
        for handle in (text_handle, compressed_handle, raw_handle):
            if handle is not None:
                try:
                    handle.close()
                except Exception:
                    pass
        temporary.unlink(missing_ok=True)
        raise
    stat = raw_path.stat()
    return {
        "database": "eicu",
        "source_name": "nurseCharting",
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "raw_sha256": raw_hash,
        "expected_sha256": EICU_EXPECTED_SHA256,
        "official_sha256_match": "TRUE",
        "scanned_rows": scanned,
        "kept_rows": kept,
        "reached_eof": "TRUE",
        "filter_spec": "target ID AND exact locked GCS label/name pair",
        "output_columns": ";".join(EICU_OUTPUT_COLUMNS),
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def manifest_valid(
    manifest_path: Path,
    gate_path: Path,
    raw_path: Path,
    output_path: Path,
    output_columns: Sequence[str],
    target_hash: str,
    helper_hash: str,
    expected_source_hash: str,
) -> bool:
    if not manifest_path.exists() or not gate_path.exists():
        return False
    try:
        with manifest_path.open(
            "r", encoding="utf-8", newline=""
        ) as handle:
            rows = list(csv.DictReader(handle, strict=True))
        with gate_path.open(
            "r", encoding="utf-8", newline=""
        ) as handle:
            gates = list(csv.DictReader(handle, strict=True))
        if len(rows) != 1 or len(gates) != 1:
            return False
        row, gate = rows[0], gates[0]
        if row.get("status") != "PASS" or gate.get("status") != "PASS":
            return False
        if gate.get("target_keys_sha256") != target_hash:
            return False
        if gate.get("helper_sha256") != helper_hash:
            return False
        if row.get("raw_sha256") != expected_source_hash:
            return False
        if not raw_path.exists() or not output_path.exists():
            return False
        stat = raw_path.stat()
        if int(row["raw_size"]) != stat.st_size:
            return False
        if int(row["raw_mtime_ns"]) != stat.st_mtime_ns:
            return False
        if row.get("output_sha256") != sha256_file(output_path):
            return False
        if validate_cache(output_path, output_columns) != int(
            row["kept_rows"]
        ):
            return False
        return True
    except Exception:
        return False


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--database", choices=["mimic", "eicu"], required=True
    )
    parser.add_argument("--keys", type=Path, required=True)
    parser.add_argument("--raw-root", type=Path, required=True)
    parser.add_argument("--cache-dir", type=Path, required=True)
    args = parser.parse_args()

    cache_dir = args.cache_dir.resolve()
    cache_dir.mkdir(parents=True, exist_ok=True)
    helper_hash = sha256_file(Path(__file__).resolve())
    if args.database == "mimic":
        target_ids = load_mimic_keys(args.keys)
        target_hash = sha256_lines(target_ids)
        raw_path = args.raw_root.resolve() / "icu" / "chartevents.csv.gz"
        output_path = cache_dir / "mimic_gcs_candidates_v2.csv.gz"
        output_columns = MIMIC_OUTPUT_COLUMNS
        expected_source_hash = MIMIC_EXPECTED_SHA256
    else:
        target_ids = load_eicu_ids(args.keys)
        target_hash = sha256_lines(target_ids)
        raw_path = args.raw_root.resolve() / "nurseCharting.csv.gz"
        output_path = cache_dir / "eicu_gcs_candidates_v2.csv.gz"
        output_columns = EICU_OUTPUT_COLUMNS
        expected_source_hash = EICU_EXPECTED_SHA256
    if not raw_path.exists():
        raise FileNotFoundError(raw_path)

    manifest_path = cache_dir / MANIFEST_NAME
    gate_path = cache_dir / GATE_NAME
    if manifest_valid(
        manifest_path,
        gate_path,
        raw_path,
        output_path,
        output_columns,
        target_hash,
        helper_hash,
        expected_source_hash,
    ):
        print(f"CACHE_HIT {gate_path}")
        return 0

    gate_path.unlink(missing_ok=True)
    manifest_path.unlink(missing_ok=True)
    if args.database == "mimic":
        result = filter_mimic(raw_path, output_path, args.keys)
    else:
        result = filter_eicu(raw_path, output_path, target_ids)
    result.update(
        {
            "target_count": len(target_ids),
            "target_keys_sha256": target_hash,
            "helper_version": VERSION,
            "helper_sha256": helper_hash,
        }
    )
    fields = [
        "database",
        "source_name",
        "raw_path",
        "raw_size",
        "raw_mtime_ns",
        "raw_sha256",
        "expected_sha256",
        "official_sha256_match",
        "scanned_rows",
        "kept_rows",
        "reached_eof",
        "filter_spec",
        "output_columns",
        "output_path",
        "output_size",
        "output_sha256",
        "target_count",
        "target_keys_sha256",
        "helper_version",
        "helper_sha256",
        "elapsed_seconds",
        "status",
    ]
    atomic_write_csv(manifest_path, [result], fields)
    gate = {
        "status": "PASS",
        "database": args.database,
        "completed_at": time.strftime("%Y-%m-%d %H:%M:%S %z"),
        "target_count": len(target_ids),
        "target_keys_sha256": target_hash,
        "helper_version": VERSION,
        "helper_sha256": helper_hash,
        "source_sha256": result["raw_sha256"],
        "source_official_sha256_match": "TRUE",
        "scanned_rows": result["scanned_rows"],
        "kept_rows": result["kept_rows"],
        "reached_eof": "TRUE",
        "output_sha256": result["output_sha256"],
        "manifest_sha256": sha256_file(manifest_path),
        "outcome_artifacts_opened": "FALSE",
    }
    atomic_write_csv(gate_path, [gate], list(gate))
    print(
        f"CACHE_COMPLETE {gate_path} scanned={result['scanned_rows']} "
        f"kept={result['kept_rows']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
