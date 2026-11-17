"""Which drift removal should a score use? Compared on synthetic data only (reported, not gating).

A position u_t (held from the close of bar t to the close of bar t+1) is scored over a window of 3 trading days (the
walk-forward test window) as

    score = sum_t u_t * r_t  -  cost  -  mu * sum_t u_t          r_t = (mid[t+1] - mid[t]) / mid[t] * 1e4 (bp)

with mu, the drift per bar, taken in one of these forms:

  none        mu = 0: plain money after cost.
  span        one mu for the whole evaluated span: its mean bar return. This is the current Erlang form
              (fx.erl ep_stat, episodes:drift/1 and span_drift/2: one number per span, fixed before any agent runs).
  train_W     mu = mean bar return of the W trading days before the window (W = 5, 20, 126), or of all earlier
              days ("train_all"): what a walk-forward round could know at its decision time.
  in_window   mu = mean bar return of the scored window itself. Algebraically the same as the form fx.erl used
              until November 2026, gross - mean(u) * sum(r): sum u (r - rbar) = sum u r - ubar * sum r.

Two things are wanted: a constant long or short position should score about zero, and a rule with real timing
should keep its money. Scenarios (synthetic.py, H1 decisions):
  A  random walk, no drift
  B  random walk + constant drift
  C  trend regimes (drift that flips sign, mean regime length 20 days) + the same constant drift

`python3 drift_forms.py` prints the table and a verdict line per question.
"""

import argparse
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import rule_walkforward as rw  # noqa: E402
import signal_study as ss  # noqa: E402
import synthetic  # noqa: E402

FORMS = ["true", "none", "span", "train_5", "train_20", "train_126", "train_all", "in_window"]
STEP = 3


def window_scores(u, x, eval_period, step=STEP, true_mu=None):
    """score of position u in each 3-day window of the evaluated span, under every drift form. `true_mu` (bp per
    bar), known only on synthetic data, gives the reference form "true": the constant drift the generator added."""
    mid = x["mid_close"]
    n = len(mid)
    u = np.asarray(u, np.int8).copy()
    u[-1] = 0
    r = np.r_[np.diff(mid) / mid[:-1] * 1e4, 0.0]
    _, cost, _ = rw.bar_pnl(u, mid, x["bid_close"], x["ask_close"])
    days, gix = np.unique(ss.day_number(x["trading_day"]), return_inverse=True)
    D = len(days)
    valid = np.r_[np.ones(n - 1, bool), False]

    def daysum(v):
        return np.r_[0.0, np.cumsum(np.bincount(gix, weights=v, minlength=D))]

    M = daysum(u * r - cost)                   # money after cost
    E = daysum(u.astype(float))                # exposure (position-bars)
    R = daysum(np.where(valid, r, 0.0))
    N = daysum(valid.astype(float))
    e0 = int(np.searchsorted(days, ss.to_daynum(eval_period[0])))
    e1 = int(np.searchsorted(days, ss.to_daynum(eval_period[1]), side="right"))
    k = np.arange(e0, e1, step)
    ke = np.minimum(k + step, e1)
    money, expo = M[ke] - M[k], E[ke] - E[k]

    def mu(a, b):
        with np.errstate(invalid="ignore", divide="ignore"):
            return (R[b] - R[a]) / (N[b] - N[a])

    out = {"none": money,
           "span": money - mu(e0, e1) * expo,
           "train_all": money - mu(np.zeros_like(k), k) * expo,
           "in_window": money - mu(k, ke) * expo}
    for W in (5, 20, 126):
        out[f"train_{W}"] = money - mu(np.maximum(k - W, 0), k) * expo
    if true_mu is not None:
        out["true"] = money - true_mu * expo
    return out, days[k]


def strategies(bars, X, pair, excl=None):
    pos = rw.rule_positions(bars, X, pair, excl)
    n = len(bars[X]["mid_close"])
    keep = np.ones(n, np.int8) if excl is None else (~rw.no_trade_mask(excl)).astype(np.int8)
    return {"always_long": keep.copy(), "always_short": -keep,
            "ma_50_200 (trend)": pos["ma_50_200"], "donchian20 (trend)": pos["donchian20"],
            "rsi_30_70_rev (reversal)": pos["rsi_30_70_rev"]}


BAR_MINUTES = {"M15": 15, "H1": 60, "H4": 240}
SCENARIOS = {"A random walk": {},
             "B constant drift": {"drift_bp_day": 3.0},
             "C trend regimes + drift": {"drift_bp_day": 3.0, "trend_bp_day": 25.0, "trend_days": 20}}


def compare(seeds=4, pairs=("EURUSD", "USDJPY", "GBPUSD"), X="H1", warmup="2007-01-01", start="2008-01-01",
            end="2014-12-31", scenarios=SCENARIOS):
    rows = []
    for sc, gen in scenarios.items():
        acc = {}
        for seed in range(seeds):
            bars = synthetic.generate(pairs, warmup, end, seed=100 + seed, **gen)
            for pair in pairs:
                b = ss.guard_bars(bars[pair], end)[0]
                for sname, u in strategies(b, X, pair).items():
                    true_mu = gen.get("drift_bp_day", 0.0) * BAR_MINUTES[X] / 1440.0
                    sc_w, _ = window_scores(u, b[X], (start, end), true_mu=true_mu)
                    for f, v in sc_w.items():
                        acc.setdefault((sname, f), []).append(v)
        for (sname, f), parts in acc.items():
            v = np.concatenate(parts)
            v = v[np.isfinite(v)]
            m, se = v.mean(), v.std(ddof=1) / np.sqrt(len(v))
            rows.append({"scenario": sc, "strategy": sname, "form": f, "mean_bp_per_window": m, "t": m / se,
                         "windows": len(v)})
    return pd.DataFrame(rows)


def verdict(df):
    L = []
    piv = df.pivot_table(index=["scenario", "strategy"], columns="form", values="mean_bp_per_window")[FORMS]
    tp = df.pivot_table(index=["scenario", "strategy"], columns="form", values="t")[FORMS]
    L.append("mean score per 3-day window, bp (t in the second table; windows pooled over pairs and seeds):")
    L.append(piv.to_string(float_format=lambda v: f"{v:+8.2f}"))
    L.append("")
    L.append(tp.to_string(float_format=lambda v: f"{v:+7.1f}"))
    L.append("")
    L.append("'true' removes exactly the constant drift the generator added (only knowable on synthetic data). A t on "
             "a constant position under span or in_window is the t of a near-constant series (cost at the first and "
             "last window only): ignore it.")
    L.append("")
    B, C = "B constant drift", "C trend regimes + drift"
    for f in FORMS:
        lg = abs(tp.loc[(B, "always_long"), f])
        kept = [piv.loc[(C, s), f] / piv.loc[(C, s), "true"] for s in ("ma_50_200 (trend)", "donchian20 (trend)")]
        L.append(f"{f:10s}: constant long under drift: {piv.loc[(B, 'always_long'), f]:+6.2f} bp/window (|t| {lg:4.1f}); "
                 f"trend rules in trend regimes keep {kept[0]:6.1%} (MA) and {kept[1]:6.1%} (Donchian) of their "
                 f"true-drift score; on a random walk: reversal rule t "
                 f"{tp.loc[('A random walk', 'rsi_30_70_rev (reversal)'), f]:+5.1f}, trend rule t "
                 f"{tp.loc[('A random walk', 'ma_50_200 (trend)'), f]:+5.1f}")
    return "\n".join(L) + "\n"


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--seeds", type=int, default=4)
    ap.add_argument("--csv")
    a = ap.parse_args(argv)
    t0 = time.time()
    df = compare(a.seeds)
    print(verdict(df))
    print(f"runtime {time.time() - t0:.1f} s")
    if a.csv:
        df.to_csv(a.csv, index=False)


if __name__ == "__main__":
    main()
