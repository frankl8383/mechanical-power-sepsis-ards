#!/usr/bin/env python3
"""Outcome-blind eICU set/total respiratory-rate filter for rebuild_v2.

Only ``Vent Rate`` and ``Total RR`` rows belonging to the fixed-landmark
primary-tuple patients are retained.  The source CSV is parsed record by
record, read through EOF, and written atomically as a deterministic gzip CSV.
No patient outcome or discharge table is opened.
"""

from __future__ import annotations

import argparse
import csv
import gzip
import hashlib
import io
import os
from pathlib import Path
import time
from typing import Dict, Iterable, List, Sequence, Set


VERSION = "rebuild_v2_eicu_rate_filter_1.0.0"
ALLOWED_LABELS = {"Vent Rate", "Total RR"}
OUTPUT_COLUMNS = (
    "patientunitstayid",
    "respchartoffset",
    "respchartentryoffset",
    "respcharttypecat",
    "respchartvaluelabel",
    "respchartvalue",
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


def load_target_ids(path: Path) -> Set[str]:
    target_ids: Set[str] = set()
    with path.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.reader(handle, strict=True)
        for row_number, row in enumerate(reader, start=1):
            if len(row) != 1:
                raise ValueError(
                    f"Target-ID width failure at row {row_number}: {row!r}"
                )
            value = row[0].strip()
            if not value.isdigit():
                raise ValueError(
                    f"Invalid patientunitstayid at row {row_number}: {value!r}"
                )
            target_ids.add(value)
    if not target_ids:
        raise ValueError("Target-ID file is empty.")
    return target_ids


def canonical_gzip_writer(path: Path):
    raw = path.open("wb")
    gz = gzip.GzipFile(filename="", mode="wb", fileobj=raw, mtime=0)
    text = io.TextIOWrapper(gz, encoding="utf-8", newline="")
    return raw, gz, text


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


def validate_output(path: Path) -> int:
    row_count = 0
    with gzip.open(
        path, "rt", encoding="utf-8", errors="strict", newline=""
    ) as handle:
        reader = csv.reader(handle, strict=True)
        header = next(reader)
        if header != list(OUTPUT_COLUMNS):
            raise ValueError(f"Output header mismatch: {header!r}")
        for row_count, row in enumerate(reader, start=1):
            if len(row) != len(header):
                raise ValueError(
                    f"Output width failure at retained row {row_count}."
                )
    return row_count


def run(args: argparse.Namespace) -> None:
    source = Path(args.source).resolve()
    target_path = Path(args.target_ids).resolve()
    output = Path(args.output).resolve()
    manifest = Path(args.manifest).resolve()
    gate = Path(args.gate).resolve()
    for required in (source, target_path):
        if not required.is_file():
            raise FileNotFoundError(required)
    output.parent.mkdir(parents=True, exist_ok=True)
    manifest.parent.mkdir(parents=True, exist_ok=True)
    gate.parent.mkdir(parents=True, exist_ok=True)

    target_ids = load_target_ids(target_path)
    started = time.time()
    scanned_rows = 0
    target_rows = 0
    retained_rows = 0
    label_counts = {label: 0 for label in sorted(ALLOWED_LABELS)}
    temporary = output.with_suffix(output.suffix + ".tmp")
    temporary.unlink(missing_ok=True)
    raw_handle = gzip_handle = text_handle = None
    try:
        with gzip.open(
            source, "rt", encoding="utf-8", errors="strict", newline=""
        ) as source_handle:
            reader = csv.reader(source_handle, strict=True)
            header = next(reader)
            if len(header) != len(set(header)):
                raise ValueError("Source contains duplicate column names.")
            missing = [
                column for column in OUTPUT_COLUMNS if column not in header
            ]
            if missing:
                raise ValueError(f"Source lacks columns: {missing}")
            positions = {name: index for index, name in enumerate(header)}
            selected_positions = [
                positions[column] for column in OUTPUT_COLUMNS
            ]
            patient_position = positions["patientunitstayid"]
            label_position = positions["respchartvaluelabel"]

            raw_handle, gzip_handle, text_handle = canonical_gzip_writer(
                temporary
            )
            writer = csv.writer(text_handle, lineterminator="\n")
            writer.writerow(OUTPUT_COLUMNS)
            for row in reader:
                scanned_rows += 1
                if len(row) != len(header):
                    raise ValueError(
                        f"Source width failure at physical row "
                        f"{scanned_rows + 1}."
                    )
                patient = row[patient_position]
                if patient not in target_ids:
                    continue
                target_rows += 1
                label = row[label_position]
                if label not in ALLOWED_LABELS:
                    continue
                writer.writerow([row[index] for index in selected_positions])
                retained_rows += 1
                label_counts[label] += 1
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

    logical_rows = validate_output(output)
    if logical_rows != retained_rows:
        raise RuntimeError(
            f"Retained-row mismatch: {retained_rows} != {logical_rows}"
        )

    source_stat = source.stat()
    source_sha256 = sha256_file(source)
    target_sha256 = sha256_values(target_ids)
    output_sha256 = sha256_file(output)
    manifest_rows: List[Dict[str, object]] = []
    for label in sorted(ALLOWED_LABELS):
        manifest_rows.append(
            {
                "filter_version": VERSION,
                "source_path": str(source),
                "source_size": source_stat.st_size,
                "source_mtime_ns": source_stat.st_mtime_ns,
                "source_sha256": source_sha256,
                "target_id_path": str(target_path),
                "target_id_count": len(target_ids),
                "target_id_sha256": target_sha256,
                "source_rows_scanned": scanned_rows,
                "rows_for_target_ids": target_rows,
                "label": label,
                "retained_rows": label_counts[label],
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
            "target_id_path",
            "target_id_count",
            "target_id_sha256",
            "source_rows_scanned",
            "rows_for_target_ids",
            "label",
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
                "target_id_count": len(target_ids),
                "source_rows_scanned": scanned_rows,
                "retained_rows": retained_rows,
                "reached_eof": "TRUE",
                "helper_sha256": sha256_file(Path(__file__).resolve()),
                "manifest_sha256": sha256_file(manifest),
                "output_sha256": output_sha256,
            }
        ],
        (
            "status",
            "filter_version",
            "target_id_count",
            "source_rows_scanned",
            "retained_rows",
            "reached_eof",
            "helper_sha256",
            "manifest_sha256",
            "output_sha256",
        ),
    )
    print(
        "REBUILD_V2_EICU_RATE_FILTER_PASS "
        f"targets={len(target_ids)} scanned={scanned_rows} "
        f"retained={retained_rows}"
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", required=True)
    parser.add_argument("--target-ids", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--gate", required=True)
    return parser.parse_args()


if __name__ == "__main__":
    run(parse_args())
