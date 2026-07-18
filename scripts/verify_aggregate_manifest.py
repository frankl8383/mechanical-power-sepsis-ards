#!/usr/bin/env python3

"""Verify the disclosure-safe aggregate-result release."""

from __future__ import annotations

import csv
import hashlib
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
RESULTS = ROOT / "results_aggregate"
MANIFEST = RESULTS / "SHA256_MANIFEST.csv"


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def main() -> None:
    failures: list[str] = []
    checked = 0
    with MANIFEST.open(newline="", encoding="utf-8") as handle:
        for row in csv.DictReader(handle):
            relative = Path(row["path"])
            path = RESULTS / relative
            checked += 1
            if not path.is_file():
                failures.append(f"missing: {relative}")
                continue
            observed_bytes = path.stat().st_size
            expected_bytes = int(row["bytes"])
            if observed_bytes != expected_bytes:
                failures.append(
                    f"size mismatch: {relative} "
                    f"({observed_bytes} != {expected_bytes})"
                )
                continue
            observed_hash = sha256(path)
            if observed_hash != row["sha256"]:
                failures.append(f"SHA-256 mismatch: {relative}")

    if failures:
        raise SystemExit("\n".join(failures))
    print(f"PASS: verified {checked} aggregate files")


if __name__ == "__main__":
    main()
