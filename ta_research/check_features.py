"""Independent checks of the Stage 1 feature tables (SPEC.md sections 4-6).

python3 ta_research/check_features.py [--pairs EURUSD,...] [--tfs M15,H1,H4] [--n-rows 200] [--seed 1]
                                      [--n-cuts 1] [--skip-truncation] [--report out.json]

Checks, per pair and decision timeframe; prints PASS/FAIL per check and exits 1 on any FAIL:

- columns     stored column set == SPEC section 6 list incl. quality_excluded (reference_indicators.spec_columns);
              meta.json lists the same
              columns; one row per decision bar; available == end of the decision bar.
- reference   sampled rows recomputed with reference_indicators.reference_row (written separately from features.py)
              and compared column by column: |diff| <= 1e-5 or |diff| <= 1e-4 |ref|; events, flags, counts, ages exact.
              Conventions: SPEC 6b. Mismatches are labelled with the alternative readings that would explain them.
              Rows: random 2006-2014, DST-change weeks, Monday opens, Friday closes, days after D1 stubs, first rows
              after D1/W1 boundaries, and warm-up rows (where each chart's indicators start).
- truncation  (all columns except quality_excluded, which build_features cannot know without the quality file)
              bars rebuilt from minutes cut at T with bars.build_from_minutes, features.build_features on them; every
              row with available <= T must equal the stored row exactly. Cuts: 1-14 min after a bar end of each
              timeframe, two DST weeks, exactly at a D1 and a W1 end, after and inside a stub day, random. Also: build_features on the stored bars reproduces the stored file.
- clock       clock columns recomputed from `available` with zoneinfo, all rows.
- quality     quality_excluded == (available in (start, end] of a quality/<PAIR>.json interval), all rows.
- date_guard  no `available` or trading_day past the guard; meta.json range and bar-file SHA-256s match the bar files.

No forward price change of any kind is computed (SPEC section 9).
"""

import argparse
import datetime as dt
import json
import math
import random
import sys
import time
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402
import reference_indicators as ri  # noqa: E402

ABS_TOL = 1e-5
REL_TOL = 1e-4
RESULTS = []


def report(check, pair, tf, ok, detail=""):
    RESULTS.append({"check": check, "pair": pair, "tf": tf, "ok": bool(ok), "detail": detail})
    print(f"{'PASS' if ok else 'FAIL'}  {check:<11} {pair} {tf or '':<4} {detail}", flush=True)


def iso(s):
    return dt.datetime.fromtimestamp(int(s), common.UTC).strftime("%Y-%m-%d %H:%M")


def utc_s(y, mo, d, h=0, mi=0):
    return int(dt.datetime(y, mo, d, h, mi, tzinfo=common.UTC).timestamp())


def ny_s(y, mo, d, h=0, mi=0):
    return int(dt.datetime(y, mo, d, h, mi, tzinfo=common.NY).timestamp())


def load_bars(pair):
    return {tf: dict(np.load(common.BARS_DIR / pair / f"{tf}.npz")) for tf in common.TIMEFRAMES}


def load_features(pair, x):
    with np.load(common.FEATURES_DIR / pair / f"{x}.npz") as z:
        return {k: z[k] for k in z.files}


def dst_sundays(year):
    """US clock changes: second Sunday of March and first Sunday of November."""
    def nth_sunday(month, n):
        d = dt.date(year, month, 1)
        d += dt.timedelta(days=(6 - d.weekday()) % 7)
        return d + dt.timedelta(days=7 * (n - 1))
    return nth_sunday(3, 2), nth_sunday(11, 1)


# ---------------------------------------------------------------- columns

def check_columns(pair, x, feats, xbars, meta):
    want = ri.spec_columns(x)
    have = list(feats)
    missing = [c for c in want if c not in feats]
    extra = [c for c in have if c not in set(want)]
    meta_cols = list(meta["tables"][x].get("columns", {})) if "tables" in meta and x in meta["tables"] else None
    problems = []
    if missing:
        problems.append(f"missing {missing}")
    if extra:
        problems.append(f"extra {extra}")
    if meta_cols is None:
        problems.append("meta.json has no column list")
    elif set(meta_cols) != set(have):
        problems.append(f"meta columns differ from file: only meta {sorted(set(meta_cols) - set(have))}, "
                        f"only file {sorted(set(have) - set(meta_cols))}")
    else:
        empty = [c for c, d in meta["tables"][x]["columns"].items() if not str(d).strip()]
        if empty:
            problems.append(f"columns without a definition string: {empty}")
    n = len(xbars["end"])
    lens = {c: len(v) for c, v in feats.items() if len(v) != n}
    if lens:
        problems.append(f"columns with {set(lens.values())} rows, expected {n}")
    elif "available" in feats and not np.array_equal(feats["available"].astype(np.int64), xbars["end"]):
        problems.append("available != end of the decision bars")
    report("columns", pair, x, not problems,
           "; ".join(problems) if problems else f"{len(want)} columns, {n} rows, available == bar end")


def check_quality(pair, x, feats, bars):
    """SPEC 5a / 6: quality_excluded = 1 exactly when available lies in (start, end] of an interval of
    quality/<PAIR>.json, all rows; the json must have been built from the current H1 bar file."""
    path = common.DERIVED_DIR / "quality" / f"{pair}.json"
    if "quality_excluded" not in feats:
        report("quality", pair, x, False, "quality_excluded missing from the features file")
        return
    if not path.exists():
        report("quality", pair, x, False, f"{path} missing")
        return
    q = json.loads(path.read_text())
    a = feats["available"].astype(np.int64)
    want = np.zeros(len(a), bool)
    for lo, hi, *_ in q["intervals"]:
        want |= (a > lo) & (a <= hi)
    got = feats["quality_excluded"].astype(np.int64)
    problems = []
    if not np.isin(got, (0, 1)).all():
        problems.append("values other than 0/1")
    bad = np.flatnonzero((got == 1) != want)
    if len(bad):
        k = int(bad[0])
        problems.append(f"{len(bad)} rows differ, e.g. {iso(a[k])} stored {int(got[k])} expected {int(want[k])}")
    h1_sha = common.sha256_file(common.BARS_DIR / pair / "H1.npz")
    if q.get("bars_sha256") != h1_sha:
        problems.append("quality json not built from the current H1 bar file")
    report("quality", pair, x, not problems, "; ".join(problems) if problems else
           f"{int(want.sum())} rows flagged = rows with available in (start, end] of {len(q['intervals'])} intervals")


# ---------------------------------------------------------------- row sample

def first_rows_after(ends_x, boundary, k=1):
    i = int(np.searchsorted(ends_x, boundary, side="left"))   # first row with available >= boundary
    return list(range(i, min(i + k, len(ends_x))))


def sample_rows(bars, x, n_random, rng):
    """{row: set of tags}."""
    xb = bars[x]
    ends = xb["end"]
    starts = xb["start"]
    tags = {}

    def add(rows, tag):
        for r in rows:
            if 0 <= r < len(ends):
                tags.setdefault(int(r), set()).add(tag)

    lo = int(np.searchsorted(ends, utc_s(2006, 1, 1)))
    hi = int(np.searchsorted(ends, utc_s(2015, 1, 1)))
    add(rng.sample(range(lo, hi), min(n_random, hi - lo)), "random")
    for year in range(2006, 2015):
        for sunday in dst_sundays(year):
            open_s = ny_s(sunday.year, sunday.month, sunday.day, 17)
            i = int(np.searchsorted(ends, open_s, side="right"))
            add([i - 1, i, i + 1], "dst")
            j = int(np.searchsorted(ends, open_s + 5 * 86400, side="right"))
            if j > i + 2:
                add([rng.randrange(i + 2, j)], "dst")
    gaps = np.flatnonzero(starts[1:] - ends[:-1] > 86400) + 1      # weekend gaps
    mondays = [int(g) for g in gaps if ends[g] >= utc_s(2006, 1, 1)]
    pick = rng.sample(mondays, min(15, len(mondays)))
    add(pick, "monday_open")
    add([g - 1 for g in rng.sample(mondays, min(15, len(mondays)))], "friday_close")
    d1 = bars["D1"]
    for i in np.flatnonzero(d1["n_minutes"] < ri.STUB_MINUTES):
        add(first_rows_after(ends, int(d1["end"][i]), 3), "after_stub")
        add(first_rows_after(ends, int(d1["end"][i]) + 1, 1), "after_stub")
    d_ends = d1["end"][(d1["end"] >= utc_s(2006, 1, 1))]
    for e in rng.sample(list(d_ends), min(15, len(d_ends))):
        add(first_rows_after(ends, int(e), 1), "d1_boundary")
        add(first_rows_after(ends, int(e) + 1, 1), "d1_boundary")
    w_ends = bars["W1"]["end"][(bars["W1"]["end"] >= utc_s(2006, 1, 1))]
    for e in rng.sample(list(w_ends), min(10, len(w_ends))):
        add(first_rows_after(ends, int(e), 1), "w1_boundary")
        add(first_rows_after(ends, int(e) + 1, 1), "w1_boundary")
    for c in ri.CHARTS[x]:
        cb = bars[c]
        c_ends = cb["end"] if c != "D1" else cb["end"][cb["n_minutes"] >= ri.STUB_MINUTES]
        for k in (0, 12, 13, 14, 15, 16, 17, 19, 25, 26, 33, 34, 49, 50, 51, 112, 113, 150, 199, 200, 201, 212, 300):
            if k < len(c_ends):
                add(first_rows_after(ends, int(c_ends[k]), 1), "warmup")
    return tags


# ---------------------------------------------------------------- reference comparison

def compare_value(ref, got, exact):
    rn, gn = math.isnan(ref), math.isnan(got)
    if rn or gn:
        return rn and gn, (0.0 if rn and gn else math.inf)
    d = abs(ref - got)
    if exact:
        return d == 0, d
    return d <= ABS_TOL or d <= REL_TOL * abs(ref), d


def check_reference(pair, x, bars, feats, n_random, rng, details):
    rows = sample_rows(bars, x, n_random, rng)
    cols = [c for c in ri.row_columns(x) if c in feats]
    exact = ri.exact_columns(x)
    mism = {c: [] for c in cols}
    t0 = time.time()
    for t in sorted(rows):
        ref = ri.reference_row(bars, x, pair, t)
        for c in cols:
            got = float(feats[c][t])
            ok, d = compare_value(ref[c], got, c in exact)
            if not ok:
                mism[c].append({"t": t, "available": iso(feats["available"][t]) if "available" in feats else "",
                                "tags": sorted(rows[t]), "ref": ref[c], "stored": got, "diff": d})
    bad = {c: v for c, v in mism.items() if v}
    explain_mismatches(bars, x, pair, feats, bad, exact)
    n = len(rows)
    tag_counts = {}
    for tg in rows.values():
        for g in tg:
            tag_counts[g] = tag_counts.get(g, 0) + 1
    summary = (f"{n} rows ({', '.join(f'{k} {v}' for k, v in sorted(tag_counts.items()))}), {len(cols)} columns, "
               f"{time.time() - t0:.1f}s")
    if not bad:
        report("reference", pair, x, True, summary)
    else:
        n_cells = sum(len(v) for v in bad.values())
        n_unexpl = sum(1 for v in bad.values() for e in v if not e["explained"])
        report("reference", pair, x, False, summary + f"; {len(bad)} columns differ in {n_cells} cells, "
               f"{n_unexpl} cells not explained by any alternative reading in reference_indicators.ALTERNATIVES")
        for c, v in bad.items():
            worst = sorted(v, key=lambda e: -e["diff"] if math.isfinite(e["diff"]) else -1e300)[:3]
            by_tag = {}
            for e in v:
                for g in e["tags"]:
                    by_tag[g] = by_tag.get(g, 0) + 1
            attr = {}
            for e in v:
                for k in e["by"]:
                    attr[k] = attr.get(k, 0) + 1
            unexpl = [e for e in v if not e["explained"]]
            print(f"        {c}: {len(v)}/{n} rows differ (by tag {by_tag}); explained by alternative readings "
                  f"{len(v) - len(unexpl)} (single convention: {attr}); unexplained {len(unexpl)}")
            if unexpl:
                worst = sorted(unexpl, key=lambda e: -e["diff"] if math.isfinite(e["diff"]) else -1e300)[:3]
            for e in worst:
                print(f"            t={e['t']} {e['available']} {','.join(e['tags'])}: ref {e['ref']!r} "
                      f"stored {e['stored']!r}")
    details.setdefault(pair, {})[x] = {"rows": n, "mismatches": {c: v for c, v in bad.items()}}


def explain_mismatches(bars, x, pair, feats, bad, exact):
    """Label each mismatch with the alternative readings (reference_indicators.ALTERNATIVES) that would reproduce the
    stored value: e["explained"] (all alternatives together) and e["by"] (single alternatives). Labels only: the
    verdict uses the SPEC 6b defaults."""
    rows = sorted({e["t"] for v in bad.values() for e in v})
    if not rows:
        return
    variants = [("ALL", dict(ri.ALTERNATIVES))] + [(k, {k: v}) for k, v in ri.ALTERNATIVES.items()]
    alt = {}
    try:
        for name, conv in variants:
            ri.set_conventions(**conv)
            alt[name] = {t: ri.reference_row(bars, x, pair, t) for t in rows}
    finally:
        ri.set_conventions()
    for c, v in bad.items():
        for e in v:
            e["explained"] = any(compare_value(alt[name][e["t"]][c], e["stored"], c in exact)[0] for name, _ in variants)
            e["by"] = [name for name, _ in variants[1:]
                       if compare_value(alt[name][e["t"]][c], e["stored"], c in exact)[0]]


# ---------------------------------------------------------------- truncation

def load_minutes(pair, through, manifest):
    import bars as bars_mod
    cache = {}
    for year in common.allowed_years(through):
        if f"{pair}/{pair}_{year}.csv.gz" in manifest:
            cache[year], _ = bars_mod.read_year(pair, year, through, manifest)
    return cache


def cut_times(bars, rng, n_random):
    cuts = []
    lo, hi = utc_s(2006, 1, 1), utc_s(2014, 12, 1)
    for tf in common.TIMEFRAMES:
        e = bars[tf]["end"]
        e = e[(e >= lo) & (e < hi)]
        cuts.append((f"{tf}+min", int(rng.choice(list(e))) + 60 * rng.randint(1, 14)))
    y1, y2 = rng.sample(range(2006, 2015), 2)
    march, _ = dst_sundays(y1)
    _, nov = dst_sundays(y2)
    for tag, d in (("dst_mar", march), ("dst_nov", nov)):
        open_s = ny_s(d.year, d.month, d.day, 17)
        # on a bar end + 1..14 min, somewhere in the first two days of the week after the change
        h4 = bars["H4"]["end"]
        cand = [int(v) for v in h4 if open_s < v <= open_s + 2 * 86400]
        cuts.append((tag, rng.choice(cand) + 60 * rng.randint(1, 14)))
    for tf in ("D1", "W1"):                                  # exactly at a bar end (the bar ending at T is kept)
        e = bars[tf]["end"]
        cuts.append((f"{tf}_end_exact", int(rng.choice(list(e[(e >= lo) & (e < hi)])))))
    d1 = bars["D1"]
    stubs = [int(v) for v in d1["end"][d1["n_minutes"] < ri.STUB_MINUTES]]
    if stubs:
        e = rng.choice(stubs)
        cuts.append(("stub_end+min", e + 60 * rng.randint(1, 14)))
        cuts.append(("inside_stub", e - 3600 * rng.randint(1, 20) + 60 * rng.randint(1, 14)))
    for _ in range(n_random):
        cuts.append(("random", rng.randrange(lo, hi)))
    return cuts


def rows_equal(a, b):
    if a.dtype.kind == "f" or b.dtype.kind == "f":
        a = a.astype(np.float64)
        b = b.astype(np.float64)
        return (a == b) | (np.isnan(a) & np.isnan(b))
    return a == b


def check_truncation(pair, tfs, bars, stored, minutes, rng, n_random):
    import bars as bars_mod
    import features
    # the function reproduces the stored file from the stored bars. quality_excluded is not compared here or below:
    # build_features is pure (SPEC 6 API: no file access) and the CLI fills that column from quality/<PAIR>.json;
    # check_quality checks it over all rows.
    for x in tfs:
        f = features.build_features(bars, x, pair)
        diff = [c for c in stored[x] if c != ri.QUALITY_COL
                and (c not in f or len(f[c]) != len(stored[x][c])
                     or not np.all(rows_equal(np.asarray(f[c]).astype(stored[x][c].dtype), stored[x][c])))]
        report("rebuild", pair, x, not diff, "build_features(stored bars) == stored file" if not diff
               else f"columns differ: {diff[:10]}")
    for tag, T in cut_times(bars, rng, n_random):
        t0 = time.time()
        rebuilt, _ = bars_mod.build_from_minutes(((y, m) for y, m in minutes.items()
                                                  if y <= dt.datetime.fromtimestamp(T, common.UTC).year), T)
        for x in tfs:
            f = features.build_features(rebuilt, x, pair)
            full = stored[x]
            n = int(np.searchsorted(full["available"], T, side="right"))     # rows with available <= T
            problems = []
            if len(f["available"]) != n:
                problems.append(f"{len(f['available'])} rows from the cut build, {n} stored rows with available <= T")
            m = min(n, len(f["available"]))
            bad_cols = []
            first_bad = None
            for c in full:
                if c == ri.QUALITY_COL:
                    continue
                if c not in f:
                    bad_cols.append(c)
                    continue
                eq = rows_equal(np.asarray(f[c][:m]).astype(full[c].dtype), full[c][:m])
                if not np.all(eq):
                    bad_cols.append(c)
                    k = int(np.flatnonzero(~eq)[0])
                    if first_bad is None or k > first_bad[1]:
                        first_bad = (c, k, float(f[c][k]), float(full[c][k]))
            if bad_cols:
                problems.append(f"{len(bad_cols)} columns differ: {bad_cols[:8]}"
                                + (f"; e.g. {first_bad[0]} row {first_bad[1]} ({iso(full['available'][first_bad[1]])})"
                                   f" cut {first_bad[2]!r} full {first_bad[3]!r}" if first_bad else ""))
            report("truncation", pair, x, not problems,
                   f"cut {tag} T={iso(T)}: " + ("; ".join(problems) if problems else
                                                f"{m} rows identical ({time.time() - t0:.0f}s)"))


# ---------------------------------------------------------------- clock

def check_clock(pair, x, feats):
    avail = feats["available"].astype(np.int64)
    exp = {c: np.empty(len(avail)) for c in ri.CLOCK_COLS}
    for i, a in enumerate(avail):
        local = dt.datetime.fromtimestamp(int(a), tz=common.NY)
        since_17 = (local - local.replace(hour=17, minute=0, second=0)).total_seconds() % 86400
        sunday17 = (local - dt.timedelta(days=(local.weekday() + 1) % 7)).replace(hour=17, minute=0, second=0)
        week = (local.replace(tzinfo=None) - sunday17.replace(tzinfo=None)).total_seconds()
        if week < 0:
            week += 7 * 86400
        fd, fw = since_17 / 86400.0, week / (5 * 86400.0)
        exp["clk_day_sin"][i] = math.sin(2 * math.pi * fd)
        exp["clk_day_cos"][i] = math.cos(2 * math.pi * fd)
        exp["clk_day2_sin"][i] = math.sin(4 * math.pi * fd)
        exp["clk_day2_cos"][i] = math.cos(4 * math.pi * fd)
        exp["clk_week_sin"][i] = math.sin(2 * math.pi * fw)
        exp["clk_week_cos"][i] = math.cos(2 * math.pi * fw)
    problems = []
    for c in ri.CLOCK_COLS:
        if c not in feats:
            problems.append(f"{c} missing")
            continue
        d = np.abs(feats[c].astype(np.float64) - exp[c])
        bad = np.flatnonzero(~(d <= ABS_TOL))
        if len(bad):
            k = int(bad[0])
            problems.append(f"{c}: {len(bad)} rows off, e.g. {iso(avail[k])} UTC stored {float(feats[c][k]):.6f} "
                            f"expected {exp[c][k]:.6f}")
    report("clock", pair, x, not problems, "; ".join(problems) if problems else f"{len(avail)} rows within {ABS_TOL}")


# ---------------------------------------------------------------- date guard

def check_date_guard(pair, tfs, stored, meta, bars_meta):
    problems = []
    dr = meta.get("date_range", {})
    through = dr.get("bars_through") or dr.get("through") or dr.get("through_requested")
    if not through:
        problems.append("meta.json records no date range")
        through = common.DEFAULT_THROUGH.isoformat()
    through_d = dt.date.fromisoformat(through)
    if through_d > common.MAX_THROUGH:
        problems.append(f"range through {through} is past {common.MAX_THROUGH}")
    if through != bars_meta["date_range"]["through"]:
        problems.append(f"features through {through} != bars through {bars_meta['date_range']['through']}")
    limit = common.guard_limit_utc_seconds(through_d)
    if dr.get("guard_limit_utc_seconds") not in (None, limit):
        problems.append(f"meta guard limit {dr.get('guard_limit_utc_seconds')} != {limit}")
    cur = {f"{tf}.npz": common.sha256_file(common.BARS_DIR / pair / f"{tf}.npz") for tf in common.TIMEFRAMES}
    if cur != bars_meta["bar_files"]:
        problems.append("bar files on disk differ from bars/meta.json")
    tday = int(through_d.strftime("%Y%m%d"))
    for x in tfs:
        f = stored[x]
        if int(f["available"].max()) > limit:
            problems.append(f"{x}: available up to {iso(f['available'].max())} past the guard {iso(limit)}")
        if int(f["trading_day"].max()) > tday:
            problems.append(f"{x}: trading_day {int(f['trading_day'].max())} past {tday}")
        tab = meta.get("tables", {}).get(x, {})
        recorded = tab.get("bar_files_sha256")
        if not recorded:
            problems.append(f"{x}: meta.json records no bar-file SHA-256s")
        else:
            need = {f"{tf}.npz" for tf in ri.CHARTS[x] + [x, "W1"]}
            if not need <= set(recorded):
                problems.append(f"{x}: meta lacks SHA-256 of {sorted(need - set(recorded))}")
            wrong = [k for k, v in recorded.items() if cur.get(k) != v]
            if wrong:
                problems.append(f"{x}: recorded SHA-256 differs from current bar file for {wrong}")
        if tab.get("sha256") and tab.get("file"):
            if common.sha256_file(common.FEATURES_DIR / pair / tab["file"]) != tab["sha256"]:
                problems.append(f"{x}: feature file SHA-256 differs from meta.json")
    report("date_guard", pair, None, not problems,
           "; ".join(problems) if problems else f"through {through}, max available <= {iso(limit)} UTC, "
                                                 f"bar SHA-256s match")


# ---------------------------------------------------------------- main

def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--pairs", default=",".join(common.PAIRS))
    ap.add_argument("--tfs", default=",".join(common.DECISION_TFS))
    ap.add_argument("--n-rows", type=int, default=200, help="random rows per pair and timeframe (plus special rows)")
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--n-cuts", type=int, default=1, help="random truncation cuts per pair (plus 11 targeted ones)")
    ap.add_argument("--skip-truncation", action="store_true")
    ap.add_argument("--report", default=None, help="write mismatch details as JSON to this path")
    args = ap.parse_args(argv)
    pairs = [p.strip().upper() for p in args.pairs.split(",") if p.strip()]
    tfs = [t.strip().upper() for t in args.tfs.split(",") if t.strip()]
    if any(p not in common.PAIRS for p in pairs) or any(t not in common.DECISION_TFS for t in tfs):
        ap.error("unknown pair or timeframe")
    rng = random.Random(args.seed)
    details = {}
    manifest = common.read_manifest()
    t_all = time.time()
    for pair in pairs:
        bars = load_bars(pair)
        bars_meta = json.loads((common.BARS_DIR / pair / "meta.json").read_text())
        meta = json.loads((common.FEATURES_DIR / pair / "meta.json").read_text())
        stored = {x: load_features(pair, x) for x in tfs}
        ri.clear_cache()
        for x in tfs:
            check_columns(pair, x, stored[x], bars[x], meta)
            check_clock(pair, x, stored[x])
            check_quality(pair, x, stored[x], bars)
            check_reference(pair, x, bars, stored[x], args.n_rows, rng, details)
        check_date_guard(pair, tfs, stored, meta, bars_meta)
        if not args.skip_truncation:
            through = dt.date.fromisoformat(bars_meta["date_range"]["through"])
            minutes = load_minutes(pair, through, manifest)
            check_truncation(pair, tfs, bars, stored, minutes, rng, args.n_cuts)
            del minutes
        ri.clear_cache()
    if args.report:
        Path(args.report).write_text(json.dumps({"results": RESULTS, "reference_mismatches": details},
                                                indent=1, default=str))
    n_fail = sum(not r["ok"] for r in RESULTS)
    print(f"\n{len(RESULTS)} checks, {n_fail} FAIL, {time.time() - t_all:.0f}s")
    return 1 if n_fail else 0


if __name__ == "__main__":
    sys.exit(main())
