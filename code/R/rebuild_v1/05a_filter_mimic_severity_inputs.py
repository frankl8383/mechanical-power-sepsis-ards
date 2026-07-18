#!/usr/bin/env python3
"""Fast, auditable, outcome-blind MIMIC-IV severity-input filtering.

The three source tables are multi-gigabyte gzip CSVs.  Their identifiers and
itemid occur before any free-text field, so the first structured columns can be
used for a target filter without parsing quoted free text.  The retained rows
are written byte-for-byte and are then validated to EOF with Python's strict
CSV parser.  Source gzip CRC errors, pipeline failures, malformed retained CSV,
an early stop, or an official SHA256 mismatch are fatal.

No outcome, admissions, discharge, or death table is opened here.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time
from typing import Dict, Iterable, List, Sequence, Set, Tuple


VERSION = "mimic_severity_target_filter_v2_omr"
MANIFEST_NAME = "filter_manifest_v1.csv"
GATE_NAME = "severity_input_cache_complete_v1.csv"

CHARTEVENT_ITEMIDS = {
    "220052", "220181",  # invasive and non-invasive MAP
    "220739", "223900", "223901",  # GCS eye/verbal/motor
    "226730", "226707",  # height cm/inch
}
LAB_ITEMIDS = {"51265", "50912"}  # platelet, creatinine
INPUT_ITEMIDS = {
    "221906", "221289", "222315", "221662", "221653", "221749"
}
OMR_RESULT_NAMES = {"Height (Inches)"}

EXPECTED_RAW_SHA256 = {
    "chartevents": "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098",
    "labevents": "2cd5e09b7e48c0189828854221f97e2e8568268eda82157c6933bf8e674d08d5",
    "inputevents": "09ffc40fade12d017debcb939225ab3924ac807625fdae27f09a260a8ee0ce48",
    "omr": "8280a8023203bddc6cc4419bd8cd253a015b8bc0bc53852ec92f98c7353f31e2",
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
        digest.update(value.encode("ascii"))
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


def load_keys(path: Path) -> Tuple[Set[str], Set[str], Set[Tuple[str, str]]]:
    subject_ids: Set[str] = set()
    stay_ids: Set[str] = set()
    admission_keys: Set[Tuple[str, str]] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, strict=True)
        expected = {"subject_id", "hadm_id", "stay_id"}
        if set(reader.fieldnames or []) != expected:
            raise ValueError(f"Unexpected key header: {reader.fieldnames!r}")
        for row in reader:
            subject = (row["subject_id"] or "").strip()
            hadm = (row["hadm_id"] or "").strip()
            stay = (row["stay_id"] or "").strip()
            if not subject.isdigit() or not hadm.isdigit() or not stay.isdigit():
                raise ValueError(f"Non-numeric target key: {row!r}")
            stay_ids.add(stay)
            subject_ids.add(subject)
            admission_keys.add((subject, hadm))
    if not subject_ids or not stay_ids or not admission_keys:
        raise ValueError("Target-key file is empty")
    return subject_ids, stay_ids, admission_keys


def normalized_filter_values(values: Iterable[str]) -> str:
    def sort_key(value: str) -> Tuple[int, object]:
        return (0, int(value)) if value.isdigit() else (1, value)

    return ";".join(sorted(set(values), key=sort_key))


AWK_PROGRAM = r'''
BEGIN {
  FS=",";
  while ((getline line < keyfile) > 0) {
    if (line ~ /^subject_id,hadm_id,stay_id\r?$/) continue;
    n=split(line, a, ",");
    if (n != 3 || a[1] !~ /^[0-9]+$/ || a[2] !~ /^[0-9]+$/ || a[3] !~ /^[0-9]+$/) {
      print "invalid key line: " line > "/dev/stderr"; exit 41;
    }
    stays[a[3]]=1;
    subjects[a[1]]=1;
    admissions[a[1] SUBSEP a[2]]=1;
  }
  close(keyfile);
  while ((getline item < itemfile) > 0) {
    sub(/\r$/, "", item);
    if (mode != "omr" && item !~ /^[0-9]+$/) {
      print "invalid itemid: " item > "/dev/stderr"; exit 42;
    }
    if (mode == "omr" && item == "") {
      print "empty OMR result name" > "/dev/stderr"; exit 43;
    }
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
  } else if (mode == "omr") {
    if (n != 5 || h[1] != "subject_id" || h[2] != "chartdate" ||
        h[4] != "result_name" || h[5] != "result_value") exit 54;
  } else exit 55;
  print $0;
  next;
}
{
  scanned++;
  hit=0;
  if (mode == "chartevents") hit=(($3 in stays) && ($7 in items));
  else if (mode == "labevents") hit=((($2 SUBSEP $3) in admissions) && ($5 in items));
  else if (mode == "inputevents") hit=(($3 in stays) && ($8 in items));
  else if (mode == "omr") hit=(($1 in subjects) && ($4 in items));
  if (hit) { print $0; kept++; }
}
END {
  print scanned "," kept > statsfile;
  close(statsfile);
}
'''


def validate_cache(path: Path, expected_header: Sequence[str]) -> int:
    logical_rows = 0
    with gzip.open(path, "rt", encoding="utf-8", errors="strict", newline="") as src:
        reader = csv.reader(src, strict=True)
        try:
            header = next(reader)
        except StopIteration as exc:
            raise ValueError(f"Empty cache: {path}") from exc
        if header != list(expected_header):
            raise ValueError(f"Cache header mismatch for {path}: {header!r}")
        for row in reader:
            logical_rows += 1
            if len(row) != len(header):
                raise ValueError(
                    f"CSV width mismatch in {path} at logical row {logical_rows + 1}: "
                    f"{len(row)} != {len(header)}"
                )
    return logical_rows


def filter_one(
    source_name: str,
    raw_path: Path,
    output_path: Path,
    mode: str,
    itemids: Set[str],
    key_path: Path,
    expected_header: Sequence[str],
) -> Dict[str, object]:
    started = time.time()
    output_tmp = output_path.with_suffix(output_path.suffix + ".tmp")
    stats_tmp = output_path.with_suffix(output_path.suffix + ".stats.tmp")
    output_tmp.unlink(missing_ok=True)
    stats_tmp.unlink(missing_ok=True)

    with tempfile.TemporaryDirectory(prefix="mimic_severity_filter_") as td:
        td_path = Path(td)
        awk_path = td_path / "filter.awk"
        item_path = td_path / "itemids.txt"
        awk_path.write_text(AWK_PROGRAM, encoding="utf-8")
        item_path.write_text(
            "\n".join(normalized_filter_values(itemids).split(";")) + "\n",
            encoding="ascii",
        )
        # pipefail is required: a gzip CRC error or awk early exit must not be
        # hidden by a successful downstream gzip process.
        command = (
            "set -euo pipefail; "
            "gzip -cd \"$1\" | "
            "LC_ALL=C awk -v keyfile=\"$2\" -v itemfile=\"$3\" "
            "-v mode=\"$4\" -v statsfile=\"$5\" -f \"$6\" | "
            "gzip -n > \"$7\""
        )
        completed = subprocess.run(
            [
                "/bin/bash", "-c", command, "mimic-severity-filter",
                str(raw_path), str(key_path), str(item_path), mode,
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
                f"Target filter failed ({source_name}, status={completed.returncode}): "
                f"{completed.stderr.strip()}"
            )

    if not stats_tmp.exists() or not output_tmp.exists():
        raise RuntimeError(f"Filter ended without output/stats: {source_name}")
    stats_text = stats_tmp.read_text(encoding="ascii").strip()
    parts = stats_text.split(",")
    if len(parts) != 2 or not all(part.isdigit() for part in parts):
        raise ValueError(f"Malformed filter stats for {source_name}: {stats_text!r}")
    scanned_physical_rows, kept_physical_rows = map(int, parts)
    stats_tmp.unlink()
    if scanned_physical_rows <= 0:
        raise RuntimeError(f"No source rows scanned: {source_name}")

    # Validation reads the entire retained gzip stream to EOF and uses a real
    # CSV parser for all fields containing quoted commas/free text.
    kept_logical_rows = validate_cache(output_tmp, expected_header)
    if kept_logical_rows != kept_physical_rows:
        raise RuntimeError(
            f"Physical/logical retained-row mismatch for {source_name}: "
            f"{kept_physical_rows} != {kept_logical_rows}"
        )

    raw_sha256 = sha256_file(raw_path)
    expected_raw_sha256 = EXPECTED_RAW_SHA256[source_name]
    if raw_sha256 != expected_raw_sha256:
        raise RuntimeError(
            f"Official raw SHA256 mismatch for {source_name}: "
            f"{raw_sha256} != {expected_raw_sha256}"
        )
    os.replace(output_tmp, output_path)
    stat = raw_path.stat()
    return {
        "source_name": source_name,
        "raw_path": str(raw_path.resolve()),
        "raw_size": stat.st_size,
        "raw_mtime_ns": stat.st_mtime_ns,
        "raw_sha256": raw_sha256,
        "official_sha256_match": "TRUE",
        "scanned_physical_rows": scanned_physical_rows,
        "kept_rows": kept_logical_rows,
        "reached_eof": "TRUE",
        "filter_mode": mode,
        "itemids": normalized_filter_values(itemids),
        "output_path": str(output_path.resolve()),
        "output_size": output_path.stat().st_size,
        "output_sha256": sha256_file(output_path),
        "elapsed_seconds": round(time.time() - started, 3),
        "status": "PASS",
    }


def manifest_is_valid(
    manifest_path: Path,
    gate_path: Path,
    specs: Sequence[Dict[str, object]],
    keys_sha256: str,
    helper_sha256: str,
) -> bool:
    if not manifest_path.exists() or not gate_path.exists():
        return False
    try:
        with gate_path.open("r", encoding="utf-8", newline="") as handle:
            gates = list(csv.DictReader(handle, strict=True))
        if len(gates) != 1 or gates[0].get("status") != "PASS":
            return False
        gate = gates[0]
        if gate.get("target_keys_sha256") != keys_sha256:
            return False
        if gate.get("helper_sha256") != helper_sha256:
            return False
        if int(gate.get("spec_count", "-1")) != len(specs):
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
            if row.get("reached_eof") != "TRUE" or row.get("official_sha256_match") != "TRUE":
                return False
            raw_path = Path(str(spec["raw_path"]))
            output_path = Path(str(spec["output_path"]))
            if not raw_path.exists() or not output_path.exists():
                return False
            stat = raw_path.stat()
            if int(row["raw_size"]) != stat.st_size or int(row["raw_mtime_ns"]) != stat.st_mtime_ns:
                return False
            if row.get("raw_sha256") != EXPECTED_RAW_SHA256[name]:
                return False
            if row.get("itemids") != normalized_filter_values(spec["itemids"]):
                return False
            if row.get("filter_mode") != spec["mode"]:
                return False
            if int(row["output_size"]) != output_path.stat().st_size:
                return False
            if row.get("output_sha256") != sha256_file(output_path):
                return False
            if validate_cache(output_path, spec["header"]) != int(row["kept_rows"]):
                return False
        return True
    except Exception:
        return False


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--keys", required=True, type=Path)
    parser.add_argument("--mimic-root", required=True, type=Path)
    parser.add_argument("--cache-dir", required=True, type=Path)
    args = parser.parse_args()

    cache_dir = args.cache_dir.resolve()
    cache_dir.mkdir(parents=True, exist_ok=True)
    subject_ids, stay_ids, admission_keys = load_keys(args.keys)
    key_lines = [f"{s},{h}" for s, h in admission_keys]
    key_lines.extend(f"stay:{x}" for x in stay_ids)
    keys_sha256 = sha256_lines(key_lines)
    helper_path = Path(__file__).resolve()
    helper_sha256 = sha256_file(helper_path)

    specs: List[Dict[str, object]] = [
        {
            "source_name": "omr",
            "raw_path": args.mimic_root / "hosp" / "omr.csv.gz",
            "output_path": cache_dir / "omr_height_candidates_v1.csv.gz",
            "mode": "omr",
            "itemids": OMR_RESULT_NAMES,
            "header": [
                "subject_id", "chartdate", "seq_num", "result_name",
                "result_value",
            ],
        },
        {
            "source_name": "chartevents",
            "raw_path": args.mimic_root / "icu" / "chartevents.csv.gz",
            "output_path": cache_dir / "chartevents_severity_candidates_v1.csv.gz",
            "mode": "chartevents",
            "itemids": CHARTEVENT_ITEMIDS,
            "header": [
                "subject_id", "hadm_id", "stay_id", "caregiver_id", "charttime",
                "storetime", "itemid", "value", "valuenum", "valueuom", "warning",
            ],
        },
        {
            "source_name": "labevents",
            "raw_path": args.mimic_root / "hosp" / "labevents.csv.gz",
            "output_path": cache_dir / "labevents_severity_candidates_v1.csv.gz",
            "mode": "labevents",
            "itemids": LAB_ITEMIDS,
            "header": [
                "labevent_id", "subject_id", "hadm_id", "specimen_id", "itemid",
                "order_provider_id", "charttime", "storetime", "value", "valuenum",
                "valueuom", "ref_range_lower", "ref_range_upper", "flag", "priority",
                "comments",
            ],
        },
        {
            "source_name": "inputevents",
            "raw_path": args.mimic_root / "icu" / "inputevents.csv.gz",
            "output_path": cache_dir / "inputevents_severity_candidates_v1.csv.gz",
            "mode": "inputevents",
            "itemids": INPUT_ITEMIDS,
            "header": [
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
    for spec in specs:
        if not Path(str(spec["raw_path"])).exists():
            raise FileNotFoundError(spec["raw_path"])

    manifest_path = cache_dir / MANIFEST_NAME
    gate_path = cache_dir / GATE_NAME
    if manifest_is_valid(
        manifest_path, gate_path, specs, keys_sha256, helper_sha256
    ):
        print(f"CACHE_HIT {gate_path}")
        return 0

    gate_path.unlink(missing_ok=True)
    manifest_path.unlink(missing_ok=True)
    results: List[Dict[str, object]] = []
    try:
        for spec in specs:
            result = filter_one(
                str(spec["source_name"]),
                Path(str(spec["raw_path"])),
                Path(str(spec["output_path"])),
                str(spec["mode"]),
                set(spec["itemids"]),
                args.keys.resolve(),
                list(spec["header"]),
            )
            result.update(
                {
                    "target_stay_count": len(stay_ids),
                    "target_subject_count": len(subject_ids),
                    "target_admission_count": len(admission_keys),
                    "target_keys_sha256": keys_sha256,
                    "helper_version": VERSION,
                    "helper_sha256": helper_sha256,
                }
            )
            results.append(result)
            print(
                f"FILTERED {spec['source_name']} "
                f"scanned={result['scanned_physical_rows']} kept={result['kept_rows']}",
                flush=True,
            )
    except Exception:
        gate_path.unlink(missing_ok=True)
        raise

    fields = [
        "source_name", "raw_path", "raw_size", "raw_mtime_ns", "raw_sha256",
        "official_sha256_match", "scanned_physical_rows", "kept_rows",
        "reached_eof", "filter_mode", "itemids", "output_path", "output_size",
        "output_sha256", "target_stay_count", "target_subject_count",
        "target_admission_count",
        "target_keys_sha256", "helper_version", "helper_sha256",
        "elapsed_seconds", "status",
    ]
    atomic_write_csv(manifest_path, results, fields)
    gate = {
        "status": "PASS",
        "completed_at": time.strftime("%Y-%m-%d %H:%M:%S %z"),
        "target_stay_count": len(stay_ids),
        "target_subject_count": len(subject_ids),
        "target_admission_count": len(admission_keys),
        "target_keys_sha256": keys_sha256,
        "helper_version": VERSION,
        "helper_sha256": helper_sha256,
        "spec_count": len(specs),
        "all_sources_reached_eof": "TRUE",
        "all_official_sha256_match": "TRUE",
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
