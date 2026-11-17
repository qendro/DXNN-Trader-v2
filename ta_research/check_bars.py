"""Checks for the bars built by bars.py (truncation, independent aggregation, DST, accounting, sanity, date guard).

python3 ta_research/check_bars.py [--pairs EURUSD,...] [--seed N]

Prints PASS/FAIL per check and exits 1 on any FAIL. Reads the default build (through 2014-12-31) from common.BARS_DIR.
No price-change statistic is computed.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import subprocess
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import bars  # noqa: E402
import common  # noqa: E402

RESULTS = []
FIELDS_ALL = ["start", "end", "mid_open", "mid_high", "mid_low", "mid_close", "bid_open", "ask_open", "bid_close",
              "ask_close", "spread_mean", "n_minutes", "trading_day"]
SPREAD_RTOL = 1e-12


def report(name, ok, detail=""):
    RESULTS.append((name, bool(ok)))
    print(f"{'PASS' if ok else 'FAIL'}  {name}" + (f": {detail}" if detail else ""), flush=True)


def utc_s(y, mo, d, h=0, mi=0, s=0):
    return int(dt.datetime(y, mo, d, h, mi, s, tzinfo=common.UTC).timestamp())


def ny_s(y, mo, d, h=0, mi=0):
    return int(dt.datetime(y, mo, d, h, mi, tzinfo=common.NY).timestamp())


def load_stored(pair):
    d = common.BARS_DIR / pair
    out = {}
    for tf in common.TIMEFRAMES:
        with np.load(d / f"{tf}.npz") as z:
            out[tf] = {k: z[k] for k in z.files}
    meta = json.loads((d / "meta.json").read_text())
    return out, meta


def load_minutes(pair, through, manifest):
    cache = {}
    for year in common.allowed_years(through):
        if f"{pair}/{pair}_{year}.csv.gz" in manifest:
            cache[year], _ = bars.read_year(pair, year, through, manifest)
    return cache


# ---------------------------------------------------------------- accounting

def check_accounting(pair, stored, meta, manifest):
    c = meta["counters"]
    drops = c["dropped_after_guard"] + c["dropped_weekend"] + c["dropped_crossed"]
    ok = c["rows_read"] == c["rows_kept"] + drops
    for y, cy in c["per_year"].items():
        ok &= cy["rows_read"] == cy["rows_kept"] + cy["dropped_after_guard"] + cy["dropped_weekend"] + cy["dropped_crossed"]
        ok &= cy["rows_read"] == manifest[f"{pair}/{pair}_{y}.csv.gz"]["rows"]
    report(f"{pair} accounting rows", ok,
           f"read {c['rows_read']:,} = kept {c['rows_kept']:,} + after_guard {c['dropped_after_guard']} + "
           f"weekend {c['dropped_weekend']} + crossed {c['dropped_crossed']} (per-year rows match MANIFEST)")
    parts, ok = [], True
    for tf in common.TIMEFRAMES:
        s = int(stored[tf]["n_minutes"].astype(np.int64).sum())
        u = c["minutes_in_bars_ending_after_limit"][tf]
        ok &= (s + u == c["rows_kept"]) and (tf == "W1" or u == 0)
        parts.append(f"{tf} {s:,}" + (f"+{u:,} unfinished" if u else ""))
    report(f"{pair} accounting n_minutes", ok, "; ".join(parts))


# ---------------------------------------------------------------- sanity

def weekend_windows(first_s, last_s):
    """(start, end) UTC seconds of every Fri 17:00 -> Sun 17:00 NY window covering the range (zoneinfo, per date)."""
    d = dt.datetime.fromtimestamp(first_s, common.UTC).date() - dt.timedelta(days=14)
    stop = dt.datetime.fromtimestamp(last_s, common.UTC).date() + dt.timedelta(days=14)
    d += dt.timedelta(days=(4 - d.weekday()) % 7)
    ws, we = [], []
    while d <= stop:
        sun = d + dt.timedelta(days=2)
        ws.append(ny_s(d.year, d.month, d.day, 17))
        we.append(ny_s(sun.year, sun.month, sun.day, 17))
        d += dt.timedelta(days=7)
    return np.array(ws), np.array(we)


def check_sanity(pair, stored, limit, through):
    through_i = through.year * 10000 + through.month * 100 + through.day
    for tf in common.TIMEFRAMES:
        b = stored[tf]
        fails = []
        hi, lo, op, cl = b["mid_high"], b["mid_low"], b["mid_open"], b["mid_close"]
        n = int(((hi < op) | (hi < cl) | (lo > op) | (lo > cl) | (hi < lo)).sum())
        if n:
            fails.append(f"{n} bars with mid high/low not bracketing open/close")
        n_locked = int((b["bid_close"] == b["ask_close"]).sum())
        n_crossed = int((b["bid_close"] > b["ask_close"]).sum())
        if n_locked or n_crossed:
            bad = np.flatnonzero(b["bid_close"] >= b["ask_close"])
            when = [dt.datetime.fromtimestamp(int(s), common.UTC).strftime("%Y-%m-%d %H:%M") for s in b["start"][bad[:8]]]
            fails.append(f"bid_close < ask_close violated: {n_crossed} crossed, {n_locked} locked (ask == bid), "
                         f"bar starts UTC {when}")
        if np.any(b["end"] <= b["start"]):
            fails.append("end <= start")
        if np.any(np.diff(b["start"]) <= 0):
            fails.append("starts not strictly increasing")
        if np.any(b["end"][:-1] > b["start"][1:]):
            fails.append("overlapping bars")
        if np.any(b["end"] - b["start"] != bars.TF_SECONDS[tf]):
            fails.append("bar length differs from the timeframe")
        if np.any(b["n_minutes"] < 1) or np.any(b["n_minutes"] > bars.TF_SECONDS[tf] // 60):
            fails.append("n_minutes out of range")
        ws, we = weekend_windows(int(b["start"][0]), int(b["end"][-1]))
        i = np.searchsorted(we, b["start"], side="right")        # first weekend ending after the bar starts
        n = int((b["end"] > ws[i]).sum())
        if n:
            fails.append(f"{n} bars overlap a Fri 17:00-Sun 17:00 NY weekend")
        if np.any(b["start"] > limit) or np.any(b["end"] > limit):
            fails.append("bar time past the guard limit")
        if "trading_day" in b:
            td = b["trading_day"]
            if np.any(td > through_i):
                fails.append("trading_day past the guard")
            wd = pd.to_datetime(td.astype(str), format="%Y%m%d").dayofweek.to_numpy()
            if np.any(wd >= 5):
                fails.append("trading_day on a weekend")
        elif tf != "W1":
            fails.append("trading_day missing")
        report(f"{pair} sanity {tf}", not fails, "; ".join(fails) if fails else f"{len(b['start']):,} bars")


# ---------------------------------------------------------------- DST

# label date -> UTC hour at which its trading day starts (hard-coded, not derived from zoneinfo)
DST_SAMPLES = {
    # 2005, old US rule (1st Sun Apr .. last Sun Oct); EU Mar 27 .. Oct 30
    "2005-03-14": 22,   # would be summer under the post-2007 rule
    "2005-03-30": 22,   # EU summer, US winter
    "2005-04-01": 22,   # Friday before the change Sunday 2005-04-03
    "2005-04-04": 21,   # Monday after: starts on the change Sunday
    "2005-10-28": 21,
    "2005-10-31": 22,   # Monday after change Sunday 2005-10-30
    "2005-11-02": 22,   # would be summer under the post-2007 rule
    # 2010: US Mar 14 .. Nov 7, EU Mar 28 .. Oct 31
    "2010-01-12": 22, "2010-07-14": 21,
    "2010-03-12": 22, "2010-03-15": 21, "2010-03-16": 21, "2010-03-17": 21, "2010-03-18": 21, "2010-03-19": 21,
    "2010-03-29": 21,
    "2010-10-29": 21, "2010-11-01": 21, "2010-11-02": 21, "2010-11-03": 21, "2010-11-04": 21, "2010-11-05": 21,
    "2010-11-08": 22,
    # 2014: US Mar 9 .. Nov 2
    "2014-03-07": 22, "2014-03-10": 21, "2014-10-31": 21, "2014-11-03": 22,
}


def check_dst(pair, stored):
    d1, h4, w1 = stored["D1"], stored["H4"], stored["W1"]
    fails, n_checked = [], 0
    for label, hour in DST_SAMPLES.items():
        day = dt.date.fromisoformat(label)
        td = day.year * 10000 + day.month * 100 + day.day
        idx = np.flatnonzero(d1["trading_day"] == td)
        if len(idx) != 1:
            fails.append(f"{label}: no D1 bar")
            continue
        prev = day - dt.timedelta(days=1)   # Monday's day starts Sunday 17:00 NY
        exp_start = utc_s(prev.year, prev.month, prev.day, hour)
        if int(d1["start"][idx[0]]) != exp_start or int(d1["end"][idx[0]]) != utc_s(day.year, day.month, day.day, hour):
            fails.append(f"{label}: D1 start {dt.datetime.fromtimestamp(int(d1['start'][idx[0]]), common.UTC)}, "
                         f"expected {hour}:00 UTC")
        hidx = np.flatnonzero(h4["trading_day"] == td)
        hours = {int(s) // 3600 % 24 for s in h4["start"][hidx]}
        exp_hours = {(hour + 4 * k) % 24 for k in range(6)}
        if len(hidx) != 6 or hours != exp_hours or int(h4["start"][hidx[0]]) != exp_start:
            fails.append(f"{label}: H4 UTC start hours {sorted(hours)} ({len(hidx)} bars), expected {sorted(exp_hours)}")
        n_checked += 1
    report(f"{pair} DST samples", not fails,
           "; ".join(fails[:6]) if fails else f"{n_checked} sample trading days (US winter/summer, US-EU mismatch "
                                               f"weeks, change Sundays, pre-2007 rule): D1 and H4 starts as expected")

    # every bar, per bar with zoneinfo
    bad = []
    for s, e, td in zip(d1["start"].tolist(), d1["end"].tolist(), d1["trading_day"].tolist()):
        lab = dt.date(td // 10000, td // 100 % 100, td % 100)
        prev = lab - dt.timedelta(days=1)
        if s != ny_s(prev.year, prev.month, prev.day, 17) or e != ny_s(lab.year, lab.month, lab.day, 17):
            bad.append(td)
    hours = {}
    for s in h4["start"].tolist():
        t = dt.datetime.fromtimestamp(s, common.NY)
        hours[(t.hour, t.minute)] = hours.get((t.hour, t.minute), 0) + 1
    h4_ok = set(hours) <= {(h, 0) for h in (17, 21, 1, 5, 9, 13)}
    w1_bad = 0
    for s, e in zip(w1["start"].tolist(), w1["end"].tolist()):
        a, b = dt.datetime.fromtimestamp(s, common.NY), dt.datetime.fromtimestamp(e, common.NY)
        if not (a.weekday() == 6 and (a.hour, a.minute) == (17, 0) and b.weekday() == 4 and (b.hour, b.minute) == (17, 0)
                and (b.date() - a.date()).days == 5):
            w1_bad += 1
    d1_hours = sorted({int(s) // 3600 % 24 for s in d1["start"]})
    report(f"{pair} DST all bars", not bad and h4_ok and not w1_bad and d1_hours == [21, 22],
           f"D1 {len(d1['start'])} bars start 17:00 NY prev trading day / end 17:00 NY label (bad {len(bad)}), "
           f"UTC start hours {d1_hours}; H4 NY start (h,m) counts {dict(sorted(hours.items()))}; "
           f"W1 Sun 17:00 -> Fri 17:00 NY (bad {w1_bad})")


# ---------------------------------------------------------------- independent aggregation from minutes

def independent_d1_h4(cache, limit):
    """D1 and H4 bars straight from minutes with pandas groupby on New York wall-clock times."""
    ts = np.concatenate([m["ts"] for m in cache.values()])
    cols = {c: np.concatenate([m[c] for m in cache.values()]) for c in bars.PRICE_COLS}
    wall = pd.DatetimeIndex(pd.to_datetime(ts, unit="s", utc=True)).tz_convert(common.NY).tz_localize(None)
    dow, hr = wall.dayofweek.to_numpy(), wall.hour.to_numpy()
    weekend = (dow == 5) | ((dow == 4) & (hr >= 17)) | ((dow == 6) & (hr < 17))
    in_range = ts < limit
    crossed = cols["ask_close"] <= cols["bid_close"]  # crossed or locked
    counts = {"dropped_after_guard": int((~in_range).sum()), "dropped_weekend": int((in_range & weekend).sum()),
              "dropped_crossed": int((in_range & ~weekend & crossed).sum())}
    keep = in_range & ~weekend & ~crossed
    wall, hr = wall[keep], hr[keep]
    cols = {c: v[keep] for c, v in cols.items()}
    label = wall.normalize() + pd.to_timedelta((hr >= 17).astype(np.int64), unit="D")
    day_open = label - pd.Timedelta(hours=7)                               # 17:00 wall on the previous date
    h4_open = day_open + ((wall - day_open) // pd.Timedelta(hours=4)) * pd.Timedelta(hours=4)

    def to_utc(walls):
        u, inv = np.unique(walls.to_numpy(), return_inverse=True)
        loc = pd.DatetimeIndex(u).tz_localize(common.NY, ambiguous="raise", nonexistent="raise")
        return loc.tz_convert("UTC").tz_localize(None).to_numpy().astype("datetime64[s]").astype(np.int64)[inv]

    frame = pd.DataFrame({
        "mo": (cols["bid_open"] + cols["ask_open"]) / 2,
        "mh": (cols["bid_high"] + cols["ask_high"]) / 2,
        "ml": (cols["bid_low"] + cols["ask_low"]) / 2,
        "mc": (cols["bid_close"] + cols["ask_close"]) / 2,
        "bid_open": cols["bid_open"], "ask_open": cols["ask_open"],
        "bid_close": cols["bid_close"], "ask_close": cols["ask_close"],
        "spr": cols["ask_close"] - cols["bid_close"],
        "td": (label.year * 10000 + label.month * 100 + label.day).to_numpy().astype(np.int32),
    })
    out = {}
    for tf, open_wall, length in (("D1", day_open, pd.Timedelta(hours=24)), ("H4", h4_open, pd.Timedelta(hours=4))):
        frame["start"] = to_utc(open_wall)
        frame["end"] = to_utc(open_wall + length)
        g = frame.groupby("start", sort=True).agg(
            end=("end", "first"), end_max=("end", "max"), mid_open=("mo", "first"), mid_high=("mh", "max"),
            mid_low=("ml", "min"), mid_close=("mc", "last"), bid_open=("bid_open", "first"),
            ask_open=("ask_open", "first"), bid_close=("bid_close", "last"), ask_close=("ask_close", "last"),
            spread_mean=("spr", "mean"), n_minutes=("spr", "size"), trading_day=("td", "first"),
            td_max=("td", "max"))
        if not (g["end"].equals(g["end_max"]) and g["trading_day"].equals(g["td_max"])):
            raise AssertionError(f"independent {tf}: end/trading_day not constant within a bar")
        g = g[g["end"] <= limit]
        res = {"start": g.index.to_numpy().astype(np.int64)}
        for f in FIELDS_ALL[1:]:
            res[f] = g[f].to_numpy()
        out[tf] = res
    return out, counts


def check_independent(pair, stored, meta, cache, limit):
    ind, counts = independent_d1_h4(cache, limit)
    c = meta["counters"]
    same = all(counts[k] == c[k] for k in counts)
    report(f"{pair} independent drop counts", same, f"independent {counts} vs builder "
                                                    f"{ {k: c[k] for k in counts} }")
    for tf in ("D1", "H4"):
        a, b = stored[tf], ind[tf]
        if len(a["start"]) != len(b["start"]):
            report(f"{pair} independent {tf}", False, f"{len(a['start'])} vs {len(b['start'])} bars")
            continue
        diffs, max_spread = [], 0.0
        for f in FIELDS_ALL:
            if f == "spread_mean":
                d = np.abs(a[f] - b[f])
                max_spread = float(d.max())
                if np.any(d > SPREAD_RTOL * np.abs(a[f]).clip(1e-12)):
                    diffs.append(f)
            elif not np.array_equal(a[f], b[f].astype(a[f].dtype)):
                diffs.append(f"{f} ({int((a[f] != b[f]).sum())} bars)")
        report(f"{pair} independent {tf}", not diffs,
               (f"fields differ: {diffs}" if diffs else f"{len(a['start']):,} bars, all fields identical")
               + f" (spread_mean max |diff| {max_spread:.1e}, rtol {SPREAD_RTOL:g})")


# ---------------------------------------------------------------- truncation

DST_CUTS = [utc_s(2010, 3, 17, 13, 37, 29),   # week after the US change, EU still on winter time
            utc_s(2010, 11, 7, 22, 40, 13),   # 40 min after the open on the US fall-back Sunday
            utc_s(2005, 4, 4, 2, 7, 41)]      # first night after the old-rule spring change


def check_truncation(pair, stored, cache, limit, rng):
    first = int(stored["M15"]["start"][0])
    rand = sorted(int(x) for x in rng.integers(first + 7 * 86400, limit, size=3))
    # cuts 1-14 min after a random bar end of each timeframe: where a peek at the next bar would show
    boundary = []
    for tf in common.TIMEFRAMES:
        ends = stored[tf]["end"][stored[tf]["end"] < limit - 86400]
        boundary.append((f"after {tf} end", int(rng.choice(ends)) + int(rng.integers(60, 900))))
    cuts = ([("random", t) for t in rand] + boundary + [("DST week", t) for t in DST_CUTS]
            + [("guard limit", limit)])
    for kind, T in cuts:
        rebuilt, _ = bars.build_from_minutes(((y, m) for y, m in cache.items()), T)
        fails, compared, partial = [], 0, 0
        for tf in common.TIMEFRAMES:
            full, cut = stored[tf], rebuilt[tf]
            mask = full["end"] <= T
            compared += int(mask.sum())
            if np.any(cut["end"] > T):
                fails.append(f"{tf}: rebuilt bar ends after T")
            if set(full) != set(cut):
                fails.append(f"{tf}: field sets differ")
                continue
            for f in full:
                if not np.array_equal(full[f][mask], cut[f]) or full[f].dtype != cut[f].dtype:
                    fails.append(f"{tf}.{f}")
            # partial bars at T (end > T but holding minutes before T) in the full build
            partial += int(((full["end"] > T) & (full["start"] < T)).sum())
        when = dt.datetime.fromtimestamp(T, common.UTC).strftime("%Y-%m-%d %H:%M:%S")
        report(f"{pair} truncation {kind} T={when} UTC", not fails,
               ("differs: " + ", ".join(fails[:8])) if fails else
               f"{compared:,} bars with end <= T identical in all fields; {partial} straddling bars excluded")


# ---------------------------------------------------------------- date guard

def check_date_guard(pairs, metas, manifest):
    script = Path(__file__).resolve().parent / "bars.py"
    before = {p: (common.BARS_DIR / p / "meta.json").stat().st_mtime_ns for p in pairs}
    r = subprocess.run([sys.executable, str(script), "--through", "2018-06-01", "--pairs", pairs[0]],
                       capture_output=True, text=True)
    cli_ok = r.returncode != 0 and "DateGuardError" in r.stderr
    try:
        bars.main(["--through", "2018-06-01", "--pairs", pairs[0]])
        inproc_ok = False
    except common.DateGuardError:
        inproc_ok = True
    try:
        bars.read_year(pairs[0], 2015, common.DEFAULT_THROUGH, manifest)
        year_ok = False
    except common.DateGuardError:
        year_ok = True
    edge_ok = common.allowed_through("2017-12-29") == common.MAX_THROUGH
    try:
        common.allowed_through("2017-12-30")
        edge_ok = False
    except common.DateGuardError:
        pass
    unchanged = all((common.BARS_DIR / p / "meta.json").stat().st_mtime_ns == before[p] for p in pairs)
    report("date guard --through 2018-06-01", cli_ok and inproc_ok and year_ok and edge_ok and unchanged,
           f"CLI exit {r.returncode} with DateGuardError: {cli_ok}; main() raises: {inproc_ok}; "
           f"opening 2015 refused: {year_ok}; 2017-12-29 allowed / 2017-12-30 refused: {edge_ok}; outputs untouched: "
           f"{unchanged}")

    limit = common.guard_limit_utc_seconds(common.DEFAULT_THROUGH)
    fails, latest = [], 0
    for p in pairs:
        m = metas[p]
        if m["date_range"]["through"] != common.DEFAULT_THROUGH.isoformat() or m["date_range"]["guard_limit_utc_seconds"] != limit:
            fails.append(f"{p}: meta range {m['date_range']}")
        if any(int(f[-11:-7]) > common.DEFAULT_THROUGH.year for f in m["source_files"]):
            fails.append(f"{p}: opened a file after {common.DEFAULT_THROUGH.year}")
        for tf in common.TIMEFRAMES:
            with np.load(common.BARS_DIR / p / f"{tf}.npz") as z:
                t = int(max(z["start"].max(), z["end"].max()))
                latest = max(latest, t)
                if t > limit:
                    fails.append(f"{p} {tf}: time past the limit")
                if "trading_day" in z.files and int(z["trading_day"].max()) > 20141231:
                    fails.append(f"{p} {tf}: trading_day past 2014-12-31")
    report("date guard default build", not fails,
           "; ".join(fails) if fails else
           f"latest output time {dt.datetime.fromtimestamp(latest, common.NY)} NY <= 2014-12-31 17:00 NY; "
           f"meta ranges and source years within the guard")


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--pairs", default=",".join(common.PAIRS))
    ap.add_argument("--seed", type=int, default=20141231)
    args = ap.parse_args(argv)
    pairs = [p.strip().upper() for p in args.pairs.split(",") if p.strip()]
    manifest = common.read_manifest()
    through = common.DEFAULT_THROUGH
    limit = common.guard_limit_utc_seconds(through)
    rng = np.random.default_rng(args.seed)
    metas = {}
    t0 = time.time()
    for pair in pairs:
        stored, meta = load_stored(pair)
        metas[pair] = meta
        if meta["date_range"]["through"] != through.isoformat():
            report(f"{pair} stored build uses the default range", False, meta["date_range"]["through"])
            continue
        check_accounting(pair, stored, meta, manifest)
        check_sanity(pair, stored, limit, through)
        check_dst(pair, stored)
        cache = load_minutes(pair, through, manifest)
        check_independent(pair, stored, meta, cache, limit)
        check_truncation(pair, stored, cache, limit, rng)
        del cache
        print(f"-- {pair} done ({time.time() - t0:.0f}s)", flush=True)
    check_date_guard(pairs, metas, manifest)
    n_fail = sum(1 for _, ok in RESULTS if not ok)
    print(f"\n{len(RESULTS) - n_fail} PASS, {n_fail} FAIL in {time.time() - t0:.0f}s")
    sys.exit(1 if n_fail else 0)


if __name__ == "__main__":
    main()
