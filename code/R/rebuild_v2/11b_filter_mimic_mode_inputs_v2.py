#!/usr/bin/env python3
"""Outcome-blind MIMIC ventilator-mode filter for rebuild_v2.

The 3.3-GB compressed ``chartevents`` table is streamed through EOF with an
early-column AWK key/item filter.  Retained complete CSV records are then
parsed strictly and compacted to the two ventilator-mode item IDs.  Source
provenance is cross-checked against the already SHA-verified no-GCS cache
manifest so the raw file is not hashed a second time after the EOF scan.
No outcome or discharge table is opened.
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


VERSION = "rebuild_v2_mimic_mode_filter_1.0.0"
MODE_ITEM_IDS = {"223849", "229314"}
EXPECTED_SOURCE_SHA256 = (
    "fd0387653084e5b142756b98b74fdddc2e5e7eb0f496aa8bf5af3d4176e71098"
)
OUTPUT_COLUMNS = (
    "subject_id",
    "hadm_id",
    "stay_id",
    "charttime",
    "storetime",
    "itemid",
    "value",
    "warning",
)


def sha256_file(path: Path, chunk_size: int = 8 * 1024 * 1024) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        while True:
            chunk = handle.read(chunk_size)
            if not chunk:
                break
            digest.update(chunk)
    return digest.hexdigest()


def sha256_values(values: Iterable[str]) -> str:
    digest = hashlib.sha256()
    for value in sorted(set(values), key=int):
        digest.update(value.encode("ascii"))
        digest.update(b"\n")
    return digest.hexdigest()


def load_target_stays(path: Path) -> Set[str]:
    stays: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.reader(handle, strict=True)
        for row_number, row in enumerate(reader, start=1):
            if len(row) != 1 or not row[0].isdigit():
                raise ValueError(
                    f"Invalid stay ID at row {row_number}: {row!r}"
                )
            stays.add(row[0])
    if not stays:
        raise ValueError("Target-stay file is empty.")
    return stays


def read_verified_source_manifest(
    path: Path, source: Path
) -> Dict[str, str]:
    with path.open("r", encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle, strict=True))
    matches = [
        row for row in rows if row.get("source_name") == "chartevents"
    ]
    if len(matches) != 1:
        raise ValueError("No unique chartevents provenance row was found.")
    row = matches[0]
    if (
        row.get("status") != "PASS"
        or row.get("reached_eof") != "TRUE"
        or row.get("official_sha256_match") != "TRUE"
        or row.get("raw_sha256") != EXPECTED_SOURCE_SHA256
        or Path(row.get("raw_path", "")).resolve() != source.resolve()
        or int(row.get("raw_size", "-1")) != source.stat().st_size
        or int(row.get("raw_mtime_ns", "-1")) != source.stat().st_mtime_ns
    ):
        raise ValueError("Verified chartevents provenance does not match source.")
    return row


def atomic_write_csv(
    path: Path,
    rows: Sequence[Dict[str, object]],
    fields: Sequence[str],
) -> None:
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


def canonical_gzip_writer(path: Path):
    raw = path.open("wb")
    gz = gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0)
    text = io.TextIOWrapper(gz, encoding="utf-8", newline="")
    return raw, gz, text


AWK_PROGRAM = r"""
BEGIN {
  FS=",";
  while ((getline line < stayfile) > 0) {
    sub(/\r$/, "", line);
    if (line !~ /^[0-9]+$/) exit 41;
    stays[line]=1;
  }
  close(stayfile);
}
NR == 1 {
  sub(/\r$/, "");
  n=split($0, h, ",");
  if (n != 11 || h[1] != "subject_id" || h[2] != "hadm_id" ||
      h[3] != "stay_id" || h[7] != "itemid" || h[8] != "value") exit 51;
  print $0;
  next;
}
{
  scanned++;
  if (($3 in stays) && ($7 == 223849 || $7 == 229314)) {
    print $0;
    kept++;
  }
}
END {
  print scanned "," kept > statsfile;
  close(statsfile);
}
"""


def filter_full_records(
    source: Path, target_stays: Path, output: Path
) -> tuple[int, int]:
    output.unlink(missing_ok=True)
    stats = output.with_suffix(output.suffix + ".stats")
    stats.unlink(missing_ok=True)
    with tempfile.TemporaryDirectory(
        prefix="rebuild_v2_mimic_mode_"
    ) as temporary_directory:
        awk_path = Path(temporary_directory) / "filter.awk"
        awk_path.write_text(AWK_PROGRAM, encoding="utf-8")
        command = (
            "set -euo pipefail; "
            "gzip -cd \"$1\" | "
            "LC_ALL=C awk -v stayfile=\"$2\" -v statsfile=\"$3\" "
            "-f \"$4\" | gzip -n > \"$5\""
        )
        completed = subprocess.run(
            [
                "/bin/bash",
                "-c",
                command,
                "mimic-mode-filter",
                str(source),
                str(target_stays),
                str(stats),
                str(awk_path),
                str(output),
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if completed.returncode != 0:
            output.unlink(missing_ok=True)
            stats.unlink(missing_ok=True)
            raise RuntimeError(
                f"MIMIC mode AWK scan failed ({completed.returncode}): "
                f"{completed.stderr.strip()}"
            )
    pieces = stats.read_text(encoding="ascii").strip().split(",")
    stats.unlink()
    if len(pieces) != 2 or not all(piece.isdigit() for piece in pieces):
        raise ValueError(f"Malformed AWK statistics: {pieces!r}")
    return int(pieces[0]), int(pieces[1])


def compact_records(
    filtered_full: Path, output: Path, target_stays: Set[str]
) -> tuple[int, Dict[str, int]]:
    temporary = output.with_suffix(output.suffix + ".tmp")
    temporary.unlink(missing_ok=True)
    retained_rows = 0
    item_counts = {item: 0 for item in sorted(MODE_ITEM_IDS, key=int)}
    raw_handle = gzip_handle = text_handle = None
    try:
        with gzip.open(
            filtered_full,
            "rt",
            encoding="utf-8",
            errors="strict",
            newline="",
        ) as source_handle:
            reader = csv.reader(source_handle, strict=True)
            header = next(reader)
            if len(header) != len(set(header)):
                raise ValueError("Filtered source contains duplicate headers.")
            missing = [
                column for column in OUTPUT_COLUMNS if column not in header
            ]
            if missing:
                raise ValueError(f"Filtered source lacks: {missing}")
            positions = {name: index for index, name in enumerate(header)}
            selected = [positions[column] for column in OUTPUT_COLUMNS]
            raw_handle, gzip_handle, text_handle = canonical_gzip_writer(
                temporary
            )
            writer = csv.writer(text_handle, lineterminator="\n")
            writer.writerow(OUTPUT_COLUMNS)
            for source_row_number, row in enumerate(reader, start=2):
                if len(row) != len(header):
                    raise ValueError(
                        f"CSV width failure at filtered row "
                        f"{source_row_number}."
                    )
                stay = row[positions["stay_id"]]
                item = row[positions["itemid"]]
                if stay not in target_stays or item not in MODE_ITEM_IDS:
                    raise ValueError("AWK filter retained an unexpected row.")
                writer.writerow([row[index] for index in selected])
                retained_rows += 1
                item_counts[item] += 1
            text_handle.flush()
            text_handle.close()
            text_handle = None
            gzip_handle = None
            raw_handle.close()
            raw_handle = None
        os.replace(temporary, output)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise
    finally:
        for handle in (text_handle, gzip_handle, raw_handle):
            if handle is not None:
                try:
                    handle.close()
                except Exception:
                    pass
    return retained_rows, item_counts


def run(args: argparse.Namespace) -> None:
    source = Path(args.source).resolve()
    target_path = Path(args.target_stays).resolve()
    verified_manifest = Path(args.verified_source_manifest).resolve()
    output = Path(args.output).resolve()
    manifest = Path(args.manifest).resolve()
    gate = Path(args.gate).resolve()
    for required in (source, target_path, verified_manifest):
        if not required.is_file():
            raise FileNotFoundError(required)
    output.parent.mkdir(parents=True, exist_ok=True)
    manifest.parent.mkdir(parents=True, exist_ok=True)
    gate.parent.mkdir(parents=True, exist_ok=True)

    provenance = read_verified_source_manifest(verified_manifest, source)
    target_stays = load_target_stays(target_path)
    started = time.time()
    filtered_full = output.with_suffix(output.suffix + ".full.tmp.gz")
    scanned_rows, awk_kept_rows = filter_full_records(
        source, target_path, filtered_full
    )
    retained_rows, item_counts = compact_records(
        filtered_full, output, target_stays
    )
    filtered_full.unlink(missing_ok=True)
    if retained_rows != awk_kept_rows:
        raise RuntimeError(
            f"AWK/strict-parser retained mismatch: "
            f"{awk_kept_rows} != {retained_rows}"
        )

    output_sha256 = sha256_file(output)
    manifest_rows: List[Dict[str, object]] = []
    for item in sorted(MODE_ITEM_IDS, key=int):
        manifest_rows.append(
            {
                "filter_version": VERSION,
                "source_path": str(source),
                "source_size": source.stat().st_size,
                "source_mtime_ns": source.stat().st_mtime_ns,
                "source_sha256": provenance["raw_sha256"],
                "source_sha_verified_upstream": "TRUE",
                "upstream_manifest_path": str(verified_manifest),
                "upstream_manifest_sha256": sha256_file(verified_manifest),
                "target_stay_path": str(target_path),
                "target_stay_count": len(target_stays),
                "target_stay_sha256": sha256_values(target_stays),
                "source_rows_scanned": scanned_rows,
                "itemid": item,
                "retained_rows": item_counts[item],
                "reached_eof": "TRUE",
                "output_path": str(output),
                "output_sha256": output_sha256,
                "elapsed_seconds": round(time.time() - started, 3),
                "status": "PASS",
            }
        )
    atomic_write_csv(
        manifest,
        manifest_rows,
        (
            "filter_version",
            "source_path",
            "source_size",
            "source_mtime_ns",
            "source_sha256",
            "source_sha_verified_upstream",
            "upstream_manifest_path",
            "upstream_manifest_sha256",
            "target_stay_path",
            "target_stay_count",
            "target_stay_sha256",
            "source_rows_scanned",
            "itemid",
            "retained_rows",
            "reached_eof",
            "output_path",
            "output_sha256",
            "elapsed_seconds",
            "status",
        ),
    )
    atomic_write_csv(
        gate,
        [
            {
                "status": "PASS",
                "filter_version": VERSION,
                "target_stay_count": len(target_stays),
                "source_rows_scanned": scanned_rows,
                "retained_rows": retained_rows,
                "reached_eof": "TRUE",
                "source_sha_verified_upstream": "TRUE",
                "helper_sha256": sha256_file(Path(__file__).resolve()),
                "manifest_sha256": sha256_file(manifest),
                "output_sha256": output_sha256,
            }
        ],
        (
            "status",
            "filter_version",
            "target_stay_count",
            "source_rows_scanned",
            "retained_rows",
            "reached_eof",
            "source_sha_verified_upstream",
            "helper_sha256",
            "manifest_sha256",
            "output_sha256",
        ),
    )
    print(
        "REBUILD_V2_MIMIC_MODE_FILTER_PASS "
        f"targets={len(target_stays)} scanned={scanned_rows} "
        f"retained={retained_rows}"
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", required=True)
    parser.add_argument("--target-stays", required=True)
    parser.add_argument("--verified-source-manifest", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--gate", required=True)
    return parser.parse_args()


if __name__ == "__main__":
    run(parse_args())
