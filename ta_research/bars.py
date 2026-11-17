"""Build M15, H1, H4, D1 and W1 bid/ask bars from Dukascopy minutes (SPEC.md sections 2-5).

python3 ta_research/bars.py [--pairs EURUSD,...] [--through YYYY-MM-DD]

One pair-year of minutes is in memory at a time. M15 bars are built per year; every longer timeframe is built from the
concatenated M15 bars. Output: common.BARS_DIR/<PAIR>/<TF>.npz and meta.json.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import io
import json
import os
import platform
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402

PRICE_COLS = ["bid_open", "bid_high", "bid_low", "bid_close", "ask_open", "ask_high", "ask_low", "ask_close"]
SRC_COLS = ["ts_utc"] + PRICE_COLS
PRICE_FIELDS = ["mid_open", "mid_high", "mid_low", "mid_close", "bid_open", "ask_open", "bid_close", "ask_close",
                "spread_mean"]
TF_SECONDS = {"M15": 900, "H1": 3600, "H4": 4 * 3600, "D1": 86400, "W1": 5 * 86400}
TFS_WITH_DAY = ("M15", "H1", "H4", "D1")   # SPEC: trading_day for D1 and shorter
NY_SHIFT = 7 * 3600                        # 17:00 NY + 7 h = 00:00 of the trading-day label date


# ---------------------------------------------------------------- time helpers

def ny_offset(ts):
    """UTC offset of New York (seconds, e.g. -18000) at each UTC second in `ts`, per zoneinfo."""
    ts = np.asarray(ts, dtype=np.int64)
    utc = pd.DatetimeIndex(ts.astype("datetime64[s]")).tz_localize("UTC")
    local = utc.tz_convert(common.NY).tz_localize(None)
    return local.to_numpy().astype("datetime64[s]").astype(np.int64) - ts


def ny_shifted(ts):
    """Seconds such that // 86400 is the trading-day label (days since epoch) and % 86400 is time since 17:00 NY."""
    ts = np.asarray(ts, dtype=np.int64)
    return ts + ny_offset(ts) + NY_SHIFT


def weekday_of_day(days):
    """Monday = 0 for days since 1970-01-01 (a Thursday)."""
    return (days + 3) % 7


def yyyymmdd(days):
    d = np.asarray(days, dtype=np.int64).astype("datetime64[D]")
    y = d.astype("datetime64[Y]").astype(np.int64) + 1970
    mo = d.astype("datetime64[M]").astype(np.int64) % 12 + 1
    da = (d - d.astype("datetime64[M]")).astype(np.int64) + 1
    return (y * 10000 + mo * 100 + da).astype(np.int32)


# ---------------------------------------------------------------- reading

def read_year(pair, year, through, manifest):
    """Verify the file's SHA-256 against MANIFEST.csv, then parse it. Refuses years outside the date guard."""
    if year not in common.allowed_years(through):
        raise common.DateGuardError(f"{pair} {year}: year file outside the allowed range (through {through})")
    rel = f"{pair}/{pair}_{year}.csv.gz"
    if rel not in manifest:
        raise FileNotFoundError(f"{rel} not in MANIFEST.csv")
    raw = common.source_path(pair, year).read_bytes()
    sha = hashlib.sha256(raw).hexdigest()
    if sha != manifest[rel]["sha256"]:
        raise RuntimeError(f"{rel}: SHA-256 {sha} does not match MANIFEST.csv")
    df = pd.read_csv(io.BytesIO(raw), compression="gzip", dtype={c: np.float64 for c in PRICE_COLS},
                     float_precision="round_trip")
    if list(df.columns) != SRC_COLS:
        raise RuntimeError(f"{rel}: unexpected columns {list(df.columns)}")
    ts = pd.to_datetime(df["ts_utc"], format="%Y-%m-%d %H:%M:%S").to_numpy().astype("datetime64[s]").astype(np.int64)
    m = {"ts": ts}
    for c in PRICE_COLS:
        m[c] = df[c].to_numpy(np.float64)
    del df
    if len(ts) != manifest[rel]["rows"]:
        raise RuntimeError(f"{rel}: {len(ts)} rows, manifest says {manifest[rel]['rows']}")
    if len(ts) and (np.any(np.diff(ts) <= 0) or np.any(ts % 60)):
        raise RuntimeError(f"{rel}: timestamps not strictly increasing whole minutes")
    years = ts.astype("datetime64[s]").astype("datetime64[Y]").astype(np.int64) + 1970
    if np.any(years != year):
        raise RuntimeError(f"{rel}: rows outside {year}")
    if any(np.isnan(m[c]).any() for c in PRICE_COLS):
        raise RuntimeError(f"{rel}: NaN prices")
    return m, sha


# ---------------------------------------------------------------- building

def filter_minutes(m, limit):
    """Drop minutes at/after `limit`, weekend minutes, then crossed minutes. Each row lands in exactly one counter."""
    ts = m["ts"]
    c = {"rows_read": int(len(ts))}
    in_range = ts < limit
    c["dropped_after_guard"] = int((~in_range).sum())
    sh = ny_shifted(ts)
    weekend = weekday_of_day(sh // 86400) >= 5           # trading-day label Sat/Sun = Fri 17:00 .. Sun 17:00 NY
    c["dropped_weekend"] = int((in_range & weekend).sum())
    crossed = m["ask_close"] <= m["bid_close"]  # crossed or locked (ask <= bid)
    c["dropped_crossed"] = int((in_range & ~weekend & crossed).sum())
    keep = in_range & ~weekend & ~crossed
    c["rows_kept"] = int(keep.sum())
    c["locked_kept"] = int((keep & (m["ask_close"] == m["bid_close"])).sum())  # ask == bid, kept (not crossed)
    out = {k: v[keep] for k, v in m.items()}
    out["_shifted"] = sh[keep]
    return out, c


def build_m15(m):
    """M15 bars from filtered, sorted minutes of one year. Carries `_spread_sum` for exact re-aggregation."""
    ts = m["ts"]
    n = len(ts)
    if n == 0:
        return None
    key = ts // 900
    first = np.r_[0, np.flatnonzero(np.diff(key)) + 1]
    last = np.r_[first[1:] - 1, n - 1]
    b = {"start": key[first] * 900}
    b["end"] = b["start"] + 900
    b["mid_open"] = (m["bid_open"][first] + m["ask_open"][first]) / 2
    b["mid_high"] = np.maximum.reduceat((m["bid_high"] + m["ask_high"]) / 2, first)
    b["mid_low"] = np.minimum.reduceat((m["bid_low"] + m["ask_low"]) / 2, first)
    b["mid_close"] = (m["bid_close"][last] + m["ask_close"][last]) / 2
    b["bid_open"] = m["bid_open"][first]
    b["ask_open"] = m["ask_open"][first]
    b["bid_close"] = m["bid_close"][last]
    b["ask_close"] = m["ask_close"][last]
    b["_spread_sum"] = np.add.reduceat(m["ask_close"] - m["bid_close"], first)
    b["n_minutes"] = np.diff(np.r_[first, n]).astype(np.int64)
    td = m["_shifted"] // 86400
    if np.any(td[first] != td[last]):
        raise AssertionError("M15 bar spans two trading days")
    b["trading_day"] = yyyymmdd(td[first])
    return b


def bar_starts(m15_start, tf):
    """Start (UTC s) of the `tf` interval holding each M15 bar. A trading week (Sun 17:00 .. Fri 17:00 NY) never
    contains a US clock change (those fall on Sunday 02:00 NY), so the NY offset is constant inside every interval."""
    s = m15_start
    if tf == "H1":
        return s - s % 3600
    sh = ny_shifted(s)
    if tf == "H4":
        return s - sh % 14400
    if tf == "D1":
        return s - sh % 86400
    if tf == "W1":
        return s - sh % 86400 - weekday_of_day(sh // 86400) * 86400
    raise ValueError(tf)


def aggregate(m15, tf):
    key = bar_starts(m15["start"], tf)
    n = len(key)
    first = np.r_[0, np.flatnonzero(np.diff(key)) + 1]
    last = np.r_[first[1:] - 1, n - 1]
    b = {"start": key[first]}
    b["end"] = b["start"] + TF_SECONDS[tf]
    for f in ("mid_open", "bid_open", "ask_open"):
        b[f] = m15[f][first]
    for f in ("mid_close", "bid_close", "ask_close"):
        b[f] = m15[f][last]
    b["mid_high"] = np.maximum.reduceat(m15["mid_high"], first)
    b["mid_low"] = np.minimum.reduceat(m15["mid_low"], first)
    b["_spread_sum"] = np.add.reduceat(m15["_spread_sum"], first)
    b["n_minutes"] = np.add.reduceat(m15["n_minutes"], first)
    if tf in TFS_WITH_DAY:
        if np.any(m15["trading_day"][first] != m15["trading_day"][last]):
            raise AssertionError(f"{tf} bar spans two trading days")
        b["trading_day"] = m15["trading_day"][first]
    return b


def finalize(b, tf, limit):
    """Keep only bars that end at or before `limit` (known by then); return (arrays, minutes in dropped bars)."""
    keep = b["end"] <= limit
    unfinished = int(b["n_minutes"][~keep].sum())
    out = {"start": b["start"][keep].astype(np.int64), "end": b["end"][keep].astype(np.int64)}
    for f in PRICE_FIELDS:
        if f == "spread_mean":
            out[f] = (b["_spread_sum"][keep] / b["n_minutes"][keep]).astype(np.float64)
        else:
            out[f] = b[f][keep].astype(np.float64)
    out["n_minutes"] = b["n_minutes"][keep].astype(np.int32)
    if tf in TFS_WITH_DAY:
        out["trading_day"] = b["trading_day"][keep].astype(np.int32)
    return out, unfinished


def build_from_minutes(year_minutes, limit):
    """Core builder. `year_minutes` yields (year, raw minute dict); `limit` is a UTC second (rows with ts >= limit
    are dropped, bars ending after it are not output). Returns (bars {tf: arrays}, counters)."""
    totals = {"rows_read": 0, "dropped_after_guard": 0, "dropped_weekend": 0, "dropped_crossed": 0, "rows_kept": 0,
              "locked_kept": 0}
    per_year = {}
    m15_parts = []
    for year, raw in year_minutes:
        m, c = filter_minutes(raw, limit)
        per_year[str(year)] = c
        for k in totals:
            totals[k] += c[k]
        part = build_m15(m)
        if part is not None:
            m15_parts.append(part)
        del m
    m15 = {k: np.concatenate([p[k] for p in m15_parts]) for k in m15_parts[0]}
    if np.any(np.diff(m15["start"]) <= 0):
        raise AssertionError("M15 starts not strictly increasing across years")
    built = {"M15": m15}
    for tf in ("H1", "H4", "D1", "W1"):
        built[tf] = aggregate(m15, tf)
    bars, unfinished = {}, {}
    for tf in common.TIMEFRAMES:
        bars[tf], unfinished[tf] = finalize(built[tf], tf, limit)
    counters = dict(totals, per_year=per_year, minutes_in_bars_ending_after_limit=unfinished)
    return bars, counters


# ---------------------------------------------------------------- diagnostics (structure only, no prices)

def diagnostics(bars):
    d1 = bars["D1"]
    days = (d1["start"] + ny_offset(d1["start"]) + NY_SHIFT) // 86400   # label day numbers
    full = np.arange(days[0], days[-1] + 1)
    weekdays = full[weekday_of_day(full) < 5]
    missing = np.setdiff1d(weekdays, days)
    short = d1["n_minutes"] < 1200
    return {
        "bars": {tf: int(len(bars[tf]["start"])) for tf in common.TIMEFRAMES},
        "m15_partial_bars": int((bars["M15"]["n_minutes"] < 15).sum()),
        "d1_minutes_median": float(np.median(d1["n_minutes"])),
        "d1_short_days_lt_1200_min": [[int(t), int(n)] for t, n in zip(d1["trading_day"][short], d1["n_minutes"][short])],
        "weekday_trading_days_without_bar": [int(x) for x in yyyymmdd(missing)],
    }


# ---------------------------------------------------------------- CLI

def save_npz(path, arrays):
    tmp = path.with_name(path.name + ".tmp")
    with open(tmp, "wb") as f:
        np.savez(f, **arrays)
    os.replace(tmp, path)


def build_pair(pair, through, manifest, out_root):
    t0 = time.time()
    limit = common.guard_limit_utc_seconds(through)
    sources = {}

    def gen():
        for year in common.allowed_years(through):
            if f"{pair}/{pair}_{year}.csv.gz" not in manifest:
                continue
            m, sha = read_year(pair, year, through, manifest)
            sources[f"{pair}/{pair}_{year}.csv.gz"] = sha
            yield year, m

    bars, counters = build_from_minutes(gen(), limit)
    out_dir = Path(out_root) / pair
    out_dir.mkdir(parents=True, exist_ok=True)
    out_sha = {}
    for tf in common.TIMEFRAMES:
        p = out_dir / f"{tf}.npz"
        save_npz(p, bars[tf])
        out_sha[f"{tf}.npz"] = common.sha256_file(p)
    meta = {
        "pair": pair,
        "date_range": {"first_day": common.FIRST_DAY.isoformat(), "through": through.isoformat(),
                       "guard_limit_utc": dt.datetime.fromtimestamp(limit, common.UTC).isoformat(),
                       "guard_limit_utc_seconds": limit},
        "source_files": sources,
        "counters": counters,
        "bar_files": out_sha,
        "fields": {tf: list(bars[tf]) for tf in common.TIMEFRAMES},
        "diagnostics": diagnostics(bars),
        "builder": {"script": "ta_research/bars.py", "git_commit": common.git_commit(),
                    "git_dirty_ta_research": common.git_dirty(), "python": platform.python_version(),
                    "numpy": np.__version__, "pandas": pd.__version__},
        "build_time_utc": dt.datetime.now(common.UTC).isoformat(timespec="seconds"),
        "build_seconds": round(time.time() - t0, 2),
    }
    tmp = out_dir / "meta.json.tmp"
    tmp.write_text(json.dumps(meta, indent=1) + "\n")
    os.replace(tmp, out_dir / "meta.json")
    return meta


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--pairs", default=",".join(common.PAIRS), help="comma-separated subset of " + ",".join(common.PAIRS))
    common.add_guard_args(ap)
    args = ap.parse_args(argv)
    through = common.allowed_through(args.through, args.unseal_holdout)   # raises DateGuardError
    pairs = [p.strip().upper() for p in args.pairs.split(",") if p.strip()]
    bad = [p for p in pairs if p not in common.PAIRS]
    if bad:
        ap.error(f"unknown pairs {bad}")
    manifest = common.read_manifest()
    t0 = time.time()
    for pair in pairs:
        meta = build_pair(pair, through, manifest, common.BARS_DIR)
        c = meta["counters"]
        print(f"{pair}: read {c['rows_read']:,} kept {c['rows_kept']:,} | after guard {c['dropped_after_guard']:,} "
              f"weekend {c['dropped_weekend']:,} crossed {c['dropped_crossed']:,} | "
              + " ".join(f"{tf} {n:,}" for tf, n in meta["diagnostics"]["bars"].items())
              + f" | {meta['build_seconds']:.1f}s", flush=True)
    print(f"done in {time.time() - t0:.1f}s -> {common.BARS_DIR}")


if __name__ == "__main__":
    main()
