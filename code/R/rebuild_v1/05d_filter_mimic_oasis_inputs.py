#!/usr/bin/env python3
"""Outcome-blind, full-EOF target filtering for the native MIMIC OASIS build.

Only MIMIC-IV ICU chartevents/outputevents fields needed by the pinned
official OASIS dependency graph are retained.  No admissions, patient,
discharge, death, or outcome table is opened by this helper.

The fast filter is safe for these two MIMIC CSV layouts because subject_id,
stay_id, and itemid all precede the first potentially quoted free-text field.
The complete retained gzip is subsequently re-read with Python's strict CSV
parser.  Source gzip CRC/EOF, physical/logical row agreement, the official
MIMIC-IV v3.1 SHA256, cache hashes, and target-key/helper hashes are mandatory
before the atomic PASS gate is published.
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
from typing import Dict, Iterable, List, Sequence, Set


VERSION = "mimic_native_oasis_target_filter_v1"
MANIFEST_NAME = "filter_manifest_v1.csv"
GATE_NAME = "oasis_input_cache_complete_v1.csv"

# Pinned official concepts at mimic-code commit
# 5bdb9a0eb9f319c9b9dd6d533de33533ff3932e4.
CHARTEVENT_ITEMIDS = {
    # official measurement/vitalsign.sql (OASIS variables only)
    "220045",  # heart rate
    "220052", "220181", "225312",  # mean blood pressure
    "220210", "224690",  # respiratory rate
    "223761", "223762",  # temperature F/C
    # official measurement/gcs.sql
    "220739", "223900", "223901",
    # official ventilation dependency graph
    "223849", "229314",  # ventilator modes
    "223834", "227582", "227287",  # O2 flows used by oxygen_delivery.sql
    "226732",  # O2 delivery devices
}

OUTPUTEVENT_ITEMIDS = {
    # official measurement/urine_output.sql
    "226559", "226560", "226561", "226584", "226563", "226564",
    "226565", "226567", "226557", "226558", "227488", "227489",
}

EXPECTED_RAW_SHA256 = {
    "chartevents": "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098",
    "outputevents": "67734d621addb1a3abf959e96a6e047bfa073a14a512b942a9434c9dfa018df6",
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


def load_stays(path: Path) -> Set[str]:
    stay_ids: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, strict=True)
        if reader.fieldnames != ["subject_id", "hadm_id", "stay_id"]:
            raise ValueError(f"Unexpected target-key header: {reader.fieldnames!r}")
        for row in reader:
            subject = (row["subject_id"] or "").strip()
            hadm = (row["hadm_id"] or "").strip()
            stay = (row["stay_id"] or "").strip()
            if not subject.isdigit() or not hadm.isdigit() or not stay.isdigit():
                raise ValueError(f"Non-numeric target key: {row!r}")
            stay_ids.add(stay)
    if not stay_ids:
        raise ValueError("Target-key file is empty")
    return stay_ids


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
  }
  close(keyfile);
  while ((getline item < itemfile) > 0) {
    sub(/\r$/, "", item);
    if (item !~ /^[0-9]+$/) { print "invalid itemid: " item > "/dev/stderr"; exit 42; }
    items[item]=1;
  }
  close(itemfile);
}
NR == 1 {
  sub(/\r$/, "");
  n=split($0, h, ",");
  if (n < 7 || h[1] != "subject_id" || h[3] != "stay_id" || h[7] != "itemid") exit 51;
  print $0;
  next;
}
{
  scanned++;
  if (($3 in stays) && ($7 in items)) { print $0; kept++; }
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
                    f"CSV width mismatch at logical row {logical_rows + 1}: "
                    f"{len(row)} != {len(header)}"
                )
    return logical_rows


def filter_one(
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

    with tempfile.TemporaryDirectory(prefix="mimic_oasis_filter_") as td:
        td_path = Path(td)
        awk_path = td_path / "filter.awk"
        item_path = td_path / "itemids.txt"
        awk_path.write_text(AWK_PROGRAM, encoding="utf-8")
        item_path.write_text(
            "\n".join(sorted(itemids, key=int)) + "\n", encoding="ascii"
        )
        command = (
            "set -euo pipefail; "
            "gzip -cd \"$1\" | "
            "LC_ALL=C awk -v keyfile=\"$2\" -v itemfile=\"$3\" "
            "-v statsfile=\"$4\" -f \"$5\" | "
            "gzip -n > \"$6\""
        )
        completed = subprocess.run(
            [
                "/bin/bash", "-c", command, "mimic-oasis-filter",
                str(raw_path), str(key_path), str(item_path), str(stats_tmp),
                str(awk_path), str(output_tmp),
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
    parts = stats_tmp.read_text(encoding="ascii").strip().split(",")
    stats_tmp.unlink()
    if len(parts) != 2 or not all(part.isdigit() for part in parts):
        raise ValueError(f"Malformed filter stats for {source_name}: {parts!r}")
    scanned_physical_rows, kept_physical_rows = map(int, parts)
    if scanned_physical_rows <= 0:
        raise RuntimeError(f"No source rows scanned: {source_name}")

    kept_logical_rows = validate_cache(output_tmp, expected_header)
    if kept_logical_rows != kept_physical_rows:
        raise RuntimeError(
            f"Physical/logical retained-row mismatch ({source_name}): "
            f"{kept_physical_rows} != {kept_logical_rows}"
        )
    raw_sha256 = sha256_file(raw_path)
    if raw_sha256 != EXPECTED_RAW_SHA256[source_name]:
        raise RuntimeError(
            f"Official raw SHA256 mismatch ({source_name}): {raw_sha256}"
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
        "itemids": ";".join(sorted(itemids, key=int)),
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
            if row.get("reached_eof") != "TRUE":
                return False
            if row.get("official_sha256_match") != "TRUE":
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
            if row.get("raw_sha256") != EXPECTED_RAW_SHA256[name]:
                return False
            expected_items = ";".join(sorted(spec["itemids"], key=int))
            if row.get("itemids") != expected_items:
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
    stay_ids = load_stays(args.keys)
    keys_sha256 = sha256_lines(stay_ids)
    helper_path = Path(__file__).resolve()
    helper_sha256 = sha256_file(helper_path)

    specs: List[Dict[str, object]] = [
        {
            "source_name": "chartevents",
            "raw_path": args.mimic_root / "icu" / "chartevents.csv.gz",
            "output_path": cache_dir / "chartevents_oasis_candidates_v1.csv.gz",
            "itemids": CHARTEVENT_ITEMIDS,
            "header": [
                "subject_id", "hadm_id", "stay_id", "caregiver_id",
                "charttime", "storetime", "itemid", "value", "valuenum",
                "valueuom", "warning",
            ],
        },
        {
            "source_name": "outputevents",
            "raw_path": args.mimic_root / "icu" / "outputevents.csv.gz",
            "output_path": cache_dir / "outputevents_oasis_candidates_v1.csv.gz",
            "itemids": OUTPUTEVENT_ITEMIDS,
            "header": [
                "subject_id", "hadm_id", "stay_id", "caregiver_id",
                "charttime", "storetime", "itemid", "value", "valueuom",
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
                set(spec["itemids"]),
                args.keys.resolve(),
                list(spec["header"]),
            )
            result.update(
                {
                    "target_stay_count": len(stay_ids),
                    "target_keys_sha256": keys_sha256,
                    "helper_version": VERSION,
                    "helper_sha256": helper_sha256,
                }
            )
            results.append(result)
            print(
                f"FILTERED {spec['source_name']} "
                f"scanned={result['scanned_physical_rows']} "
                f"kept={result['kept_rows']}",
                flush=True,
            )
    except Exception:
        gate_path.unlink(missing_ok=True)
        raise

    fields = [
        "source_name", "raw_path", "raw_size", "raw_mtime_ns", "raw_sha256",
        "official_sha256_match", "scanned_physical_rows", "kept_rows",
        "reached_eof", "itemids", "output_path", "output_size",
        "output_sha256", "target_stay_count", "target_keys_sha256",
        "helper_version", "helper_sha256", "elapsed_seconds", "status",
    ]
    atomic_write_csv(manifest_path, results, fields)
    gate = {
        "status": "PASS",
        "completed_at": time.strftime("%Y-%m-%d %H:%M:%S %z"),
        "target_stay_count": len(stay_ids),
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
