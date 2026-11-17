"""Shared constants, paths and the date guard for Stage 1 (see SPEC.md sections 2-4)."""

import argparse
import datetime as dt
import hashlib
import subprocess
from pathlib import Path
from zoneinfo import ZoneInfo

NY = ZoneInfo("America/New_York")
UTC = dt.timezone.utc

PAIRS = ["EURUSD", "GBPUSD", "USDJPY", "USDCHF", "USDCAD", "NZDUSD"]
JPY_PAIRS = {"USDJPY"}
TIMEFRAMES = ["M15", "H1", "H4", "D1", "W1"]
DECISION_TFS = ["M15", "H1", "H4"]

ROOT = Path(__file__).resolve().parents[2]  # DXNN_Main
SOURCE_DIR = ROOT / "market_data" / "dukascopy_m1"
DERIVED_DIR = ROOT / "market_data" / "derived" / "ta_stage1"
BARS_DIR = DERIVED_DIR / "bars"
FEATURES_DIR = DERIVED_DIR / "features"

COMMISSION_BP_PER_SIDE = 0.2

DEFAULT_THROUGH = dt.date(2014, 12, 31)   # development period ends here
MAX_THROUGH = dt.date(2017, 12, 29)       # last trading day before the sealed holdout
FIRST_DAY = dt.date(2003, 1, 1)


class DateGuardError(RuntimeError):
    pass


def allowed_through(through=None, unseal_holdout=False):
    """Return the last allowed trading day (a date). Refuses anything past 2017-12-29 unless unseal_holdout."""
    if through is None:
        return DEFAULT_THROUGH
    if isinstance(through, str):
        through = dt.date.fromisoformat(through)
    if through > MAX_THROUGH and not unseal_holdout:
        raise DateGuardError(f"--through {through} is past {MAX_THROUGH}; the holdout is sealed")
    return through


def trading_day_end_utc(day):
    """UTC instant (datetime) at which trading day `day` ends: 17:00 New York on that date."""
    return dt.datetime(day.year, day.month, day.day, 17, 0, tzinfo=NY).astimezone(UTC)


def guard_limit_utc_seconds(through):
    """Rows whose minute starts at or after this UTC second are outside the allowed range."""
    return int(trading_day_end_utc(through).timestamp())


def allowed_years(through):
    """Year files that may be opened. A trading day ending on `through` can start the previous evening, and the
    first minutes of trading day `through` may sit in year `through.year`; nothing in later years is needed."""
    return list(range(FIRST_DAY.year, through.year + 1))


def add_guard_args(parser: argparse.ArgumentParser):
    parser.add_argument("--through", default=None, help="last allowed trading day, YYYY-MM-DD (default 2014-12-31)")
    parser.add_argument("--unseal-holdout", action="store_true", help="allow dates after 2017-12-29 (not used in Stage 1)")


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def git_commit():
    try:
        return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=Path(__file__).parent, text=True).strip()
    except Exception:
        return "unknown"


def git_dirty():
    try:
        out = subprocess.check_output(["git", "status", "--porcelain", "--", "."], cwd=Path(__file__).parent, text=True)
        return bool(out.strip())
    except Exception:
        return None


def source_path(pair, year):
    return SOURCE_DIR / pair / f"{pair}_{year}.csv.gz"


def read_manifest():
    """MANIFEST.csv as {relative file: {"rows": int, "sha256": str}}."""
    import csv
    out = {}
    with open(SOURCE_DIR / "MANIFEST.csv", newline="") as f:
        for r in csv.DictReader(f):
            out[r["file"]] = {"rows": int(r["rows"]), "sha256": r["sha256"]}
    return out
