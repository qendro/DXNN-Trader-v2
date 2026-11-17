"""Quick self-checks of the feature tables written by features.py (structure, NaN, ranges, truncation).

python3 ta_research/selfcheck_features.py [--pairs EURUSD,...] [--tfs M15,H1,H4] [--cuts 6] [--seed 1]

Backward-looking only: no forward return or price change is computed. Exits 1 on any FAIL.
"""

import argparse
import datetime as dt
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402
import features  # noqa: E402
import quality  # noqa: E402

FAILS = []


def check(ok, msg):
    if not ok:
        FAILS.append(msg)
        print("  FAIL", msg)
    return ok


def ts(y, m, d):
    return int(dt.datetime(y, m, d, tzinfo=common.UTC).timestamp())


def expected_columns(x):
    """Column list written out from SPEC section 6 by hand (independent of features.column_definitions)."""
    cols = ["available", "mid_close", "bid_close", "ask_close", "spread_mean", "n_minutes", "trading_day",
            "quality_excluded", "clk_day_sin", "clk_day_cos", "clk_day2_sin", "clk_day2_cos", "clk_week_sin", "clk_week_cos",
            "pdh_dist", "pdl_dist", "pdc_dist", "pwh_dist", "pwl_dist", "pwc_dist",
            "pivot_p", "pivot_r1", "pivot_s1", "pivot_r2", "pivot_s2", "round_big_dist", "round_half_dist",
            "prev_day_stub", "ev_pdh_break", "ev_pdl_break", "ev_pdh_touch", "ev_pdl_touch",
            "ev_donchian20_break", "ev_round_big_cross"]
    charts = {"M15": ["M15", "H1", "H4", "D1"], "H1": ["H1", "H4", "D1"], "H4": ["H4", "D1"]}[x]
    lags = {"M15": 32, "H1": 24, "H4": 30, "D1": 20}
    for c in charts:
        names = [f"sma{n}_dist" for n in (20, 50, 100, 200)] + [f"ema{n}_dist" for n in (20, 50, 100, 200)]
        names += ["sma50_200_dist", "cross_sma50_age", "cross_sma200_age", "rsi14", "macd", "macd_signal",
                  "macd_hist", "stoch_k", "stoch_d", "bb_pos", "bb_width", "atr_bp", "atr_regime",
                  "donchian20_hi_dist", "donchian20_lo_dist"]
        names += [f"close_lag{k}" for k in range(1, lags[c] + 1)]
        names += ["ev_sma50_cross", "ev_sma200_cross"]
        cols += [f"{c}_{n}" for n in names]
    return cols


def in_range(a, lo, hi):
    a = a[~np.isnan(a)]
    return len(a) == 0 or (a.min() >= lo and a.max() <= hi)


def loop_events(pair, x, b, d1):
    """Level, Donchian and round-number events recomputed with a plain loop on integer grid ticks (SPEC 6b)."""
    g = features.grid(pair)
    tk = lambda a: np.rint(np.asarray(a) / g).astype(np.int64).tolist()  # noqa: E731
    c, hi, lo, st = tk(b["mid_close"]), tk(b["mid_high"]), tk(b["mid_low"]), b["start"].tolist()
    dh, dl, de = tk(d1["mid_high"]), tk(d1["mid_low"]), d1["end"].tolist()
    names = ["ev_pdh_break", "ev_pdl_break", "ev_pdh_touch", "ev_pdl_touch", "ev_donchian20_break", "ev_round_big_cross"]
    out = {k: np.zeros(len(c), np.int8) for k in names}
    j = -1
    for t in range(1, len(c)):
        while j + 1 < len(de) and de[j + 1] <= st[t]:
            j += 1
        p, q = c[t - 1], c[t]
        if j >= 0:
            for name, L in (("pdh", dh[j]), ("pdl", dl[j])):
                out[f"ev_{name}_break"][t] = 1 if p < L <= q else (-1 if p >= L > q else 0)
            out["ev_pdh_touch"][t] = int(p < dh[j] <= hi[t] and q < dh[j])
            out["ev_pdl_touch"][t] = int(p >= dl[j] > lo[t] and q >= dl[j])
        if t >= 20:
            out["ev_donchian20_break"][t] = 1 if q >= max(hi[t - 20:t]) else (-1 if q < min(lo[t - 20:t]) else 0)
        out["ev_round_big_cross"][t] = int(np.sign(q // 10000 - p // 10000))
    return out


def check_table(pair, x, f, bars, intervals):
    exp = expected_columns(x)
    check(sorted(f) == sorted(exp), f"{pair} {x}: columns differ from SPEC: missing {sorted(set(exp) - set(f))}, "
                                    f"extra {sorted(set(f) - set(exp))}")
    b = bars[x]
    av = f["available"]
    check(av.dtype == np.int64 and np.all(np.diff(av) > 0), f"{pair} {x}: available not strictly increasing int64")
    check(np.array_equal(av, b["end"]), f"{pair} {x}: available != decision bar end")
    for k in features.CONTEXT:
        check(np.array_equal(f[k], b[k]), f"{pair} {x}: {k} differs from the bar file")

    after05, after06 = av >= ts(2005, 1, 1), av >= ts(2006, 1, 1)
    nan_cols = {}
    for k in exp:
        a = f[k]
        if a.dtype.kind == "f" and k not in features.CONTEXT:
            check(a.dtype == np.float32, f"{pair} {x}: {k} is {a.dtype}, expected float32")
            check(not np.isnan(a[after05]).all(), f"{pair} {x}: {k} all NaN after 2005")
            check(not np.isinf(a).any(), f"{pair} {x}: {k} has inf")
            fr = float(np.isnan(a[after06]).mean())
            if fr > 0:
                nan_cols[k] = fr
        if k.startswith("ev_") or "_ev_" in k or k in ("prev_day_stub", "quality_excluded"):
            allowed = {0, 1} if ("touch" in k or k in ("prev_day_stub", "quality_excluded")) else {-1, 0, 1}
            check(a.dtype == np.int8 and set(np.unique(a).tolist()) <= allowed, f"{pair} {x}: {k} bad dtype/values")

    # ranges
    for k in f:
        if k.endswith(("rsi14", "stoch_k", "stoch_d")) or k.startswith("clk_"):
            check(in_range(f[k], -1, 1), f"{pair} {x}: {k} outside [-1, 1]")
        if k.endswith("_age"):
            a = f[k][~np.isnan(f[k])]
            check(in_range(a, -100, 100) and np.all(a == np.round(a)) and not np.any(a == 0),
                  f"{pair} {x}: {k} not a nonzero integer in [-100, 100]")
        if k.endswith(("atr_bp", "bb_width")):
            check(in_range(f[k], 0, np.inf), f"{pair} {x}: {k} negative")
        if k.endswith("atr_regime"):
            check(in_range(f[k], -1, np.inf), f"{pair} {x}: {k} <= -1")
    check(in_range(f[f"{x}_donchian20_hi_dist"], -np.inf, 0), f"{pair} {x}: own-chart donchian hi dist > 0")
    check(in_range(f[f"{x}_donchian20_lo_dist"], 0, np.inf), f"{pair} {x}: own-chart donchian lo dist < 0")
    # strict close_lag: on the own chart lag k is bar t-k
    atr_x = f[f"{x}_atr_bp"].astype(np.float64) * f["mid_close"] / 1e4
    for k in (1, 2, features.LAGS[x]):
        lag = f[f"{x}_close_lag{k}"].astype(np.float64)
        exp = np.r_[np.full(k, np.nan), (f["mid_close"][k:] - f["mid_close"][:-k])] / atr_x
        ok = ~np.isnan(lag) & ~np.isnan(exp)
        check(np.allclose(lag[ok], exp[ok], rtol=1e-4, atol=1e-4), f"{pair} {x}: own-chart close_lag{k} != bar t-{k}")
    # pdc_dist == D1_close_lag1 except on rows where a D1 bar ends exactly at available (strict lag skips that day)
    d1_end = np.isin(av, bars["D1"]["end"][bars["D1"]["n_minutes"] >= features.STUB_MINUTES])
    d1_ok = ~np.isnan(f["pdc_dist"]) & ~np.isnan(f["D1_close_lag1"]) & ~d1_end
    check(np.allclose(f["pdc_dist"][d1_ok], f["D1_close_lag1"][d1_ok], atol=1e-5), f"{pair} {x}: pdc_dist != D1_close_lag1")
    check(np.array_equal(f["quality_excluded"], quality.excluded_mask(av, intervals).astype(np.int8)),
          f"{pair} {x}: quality_excluded differs from quality intervals")
    if x != "M15":   # plain loop is slow on M15
        d1 = {k: v[bars["D1"]["n_minutes"] >= features.STUB_MINUTES] for k, v in bars["D1"].items()}
        for k, v in loop_events(pair, x, b, d1).items():
            check(np.array_equal(v, f[k]), f"{pair} {x}: {k} differs from plain-loop grid recomputation "
                                           f"({int((v != f[k]).sum())} rows)")
    half = 0.5 / f["D1_atr_bp"].astype(np.float64) * 1e4 * (1.0 if pair in common.JPY_PAIRS else 0.01) / f["mid_close"]
    ok = ~np.isnan(half)
    check(np.all(np.abs(f["round_big_dist"][ok]) <= half[ok] * (1 + 1e-5)), f"{pair} {x}: round_big_dist beyond half a figure")

    # sma cross events: flagged exactly on the rows where a chart bar holding a cross becomes known, with the
    # direction of the latest such cross (simple loop, independent of the vectorised version)
    jumps = {}
    for c in features.CHARTS[x]:
        g = features.grid(pair)
        cb = {k: (features.snap(v, g) if k in features.MID_FIELDS else v) for k, v in bars[c].items()}
        if c == "D1":
            cb = {k: v[cb["n_minutes"] >= features.STUB_MINUTES] for k, v in cb.items()}
        s = features.chart_series(cb, g)
        idx = np.searchsorted(cb["end"], av, side="right") - 1
        jumps[c] = int((np.diff(idx) > 1).sum())
        for n in (50, 200):
            ev = s[f"ev{n}"]
            exp_ev = np.zeros(len(av), np.int8)
            prev = -1
            for t in np.flatnonzero(np.diff(np.r_[-1, idx]) > 0):
                hits = [j for j in range(prev + 1, idx[t] + 1) if ev[j] != 0]
                if hits:
                    exp_ev[t] = ev[hits[-1]]
                prev = idx[t]
            check(np.array_equal(exp_ev, f[f"{c}_ev_sma{n}_cross"]), f"{pair} {x}: {c} sma{n} cross events differ")
    return nan_cols, {c: j for c, j in jumps.items() if j}


def truncation(pair, x, f, bars, cuts, intervals):
    """Rebuild from bars cut at T (end <= T, every timeframe); rows with available <= T must be identical."""
    for T in cuts:
        cut = {tf: {k: v[b["end"] <= T] for k, v in b.items()} for tf, b in bars.items()}
        g = features.build_features(cut, x, pair, quality_intervals=intervals)
        n = int(np.searchsorted(f["available"], T, side="right"))
        same = len(g["available"]) == n and all(
            np.array_equal(g[k], f[k][:n], equal_nan=(f[k].dtype.kind == "f")) for k in f)
        bad = [] if same else [k for k in f if len(g[k]) != n or not np.array_equal(g[k], f[k][:n],
                                                                                    equal_nan=f[k].dtype.kind == "f")]
        check(same, f"{pair} {x}: truncation at {dt.datetime.fromtimestamp(T, common.UTC)} differs in {bad[:5]}")


def cut_times(bars, rng, k):
    """Random cuts plus cuts exactly at a D1 end, a Friday W1 end, a stub day's end and mid-bar instants."""
    end = bars["M15"]["end"]
    lo, hi = np.searchsorted(end, ts(2004, 1, 1)), len(end) - 1
    out = [int(end[i]) + int(rng.integers(0, 900)) for i in rng.integers(lo, hi, k)]
    d1, w1 = bars["D1"], bars["W1"]
    out.append(int(d1["end"][rng.integers(300, len(d1["end"]))]))
    out.append(int(w1["end"][rng.integers(60, len(w1["end"]))]))
    stub = np.flatnonzero(d1["n_minutes"] < features.STUB_MINUTES)
    if len(stub):
        out.append(int(d1["end"][stub[-1]]))
        out.append(int(d1["end"][stub[-1]]) + 86400 + 3600 * 3 + 420)  # mid-session the day after the stub
    out.append(int(bars["H4"]["end"][rng.integers(1000, len(bars["H4"]["end"]))]) + 7 * 60)
    return sorted(out)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--pairs", default=",".join(common.PAIRS))
    ap.add_argument("--tfs", default=",".join(common.DECISION_TFS))
    ap.add_argument("--cuts", type=int, default=4, help="random truncation cuts per pair (plus fixed ones)")
    ap.add_argument("--seed", type=int, default=1)
    common.add_guard_args(ap)
    args = ap.parse_args(argv)
    through = common.allowed_through(args.through, args.unseal_holdout)
    limit = common.guard_limit_utc_seconds(through)
    rng = np.random.default_rng(args.seed)
    for pair in [p.strip().upper() for p in args.pairs.split(",") if p.strip()]:
        bars, shas, _ = features.load_bars(pair, limit)
        intervals = features.load_quality(pair, shas["H1.npz"])[0]
        cuts = cut_times(bars, rng, args.cuts)
        for x in [t.strip().upper() for t in args.tfs.split(",") if t.strip()]:
            with np.load(common.FEATURES_DIR / pair / f"{x}.npz") as z:
                f = {k: z[k] for k in z.files}
            n_fail = len(FAILS)
            nan_cols, jumps = check_table(pair, x, f, bars, intervals)
            truncation(pair, x, f, bars, cuts, intervals)
            worst = sorted(nan_cols.items(), key=lambda kv: -kv[1])[:6]
            print(f"{pair} {x}: {len(f['available']):,} rows, {len(f)} cols, {len(cuts)} cuts, "
                  f"quality_excluded {int(f['quality_excluded'].sum()):,} rows, "
                  f"{'ok' if len(FAILS) == n_fail else 'FAIL'}; cols with NaN after 2006: {len(nan_cols)}"
                  + (" (worst " + ", ".join(f"{k} {v:.2e}" for k, v in worst) + ")" if worst else "")
                  + (f"; rows where >1 chart bar became known: {jumps}" if jumps else ""), flush=True)
    print("ALL OK" if not FAILS else f"{len(FAILS)} FAIL(s)")
    sys.exit(1 if FAILS else 0)


if __name__ == "__main__":
    main()
