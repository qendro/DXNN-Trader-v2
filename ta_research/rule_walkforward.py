"""Stage 1 side questions with hand-written rules (reported only; they do not decide the gate).

Rules, each a position u_t in {-1, 0, +1} on the decision timeframe X, held from the close of bar t to the close of
bar t+1 ("follow" version; the "fade" version is -u):

  ma_50_200      sign(SMA50 - SMA200) of X closes (always in the market once both exist)
  rsi_30_70_rev  long from RSI(14) crossing up through 30, short from crossing down through 70, flat when RSI
                 crosses 50
  pd_breakout    long after a close above the prior day's high, short after a close below its low (latest of the
                 day wins), flat at the last bar of the trading day (by the clock: bar end = 17:00 New York)
  donchian20     long after a close above the prior 20-bar high, short after a close below the prior 20-bar low
                 (stop and reverse)
  round_bounce   after a bar touches a big figure without its close crossing it, the opposite of the approach for
                 4 bars

Fills and costs (SPEC 7): enter at the ask (buy) or bid (sell) close of the bar where the position changes, exit the
same way, 0.2 bp per side; a flip is an exit and an entry. Money is in bp of the entry mid. Bar-level accounting:
the mid P&L of each held interval and the cost of each fill, both in bp of the trade's entry mid; a trade's sum is
exactly its SPEC 7 net. P&L of interval t belongs to trading_day[t].

Rounds: a training window of W trading days (W = 5, 20, 126; also "all": every earlier day), choose the variant
(5 rules x follow/fade, or flat) with the best training net after cost (ties go to flat), hold it for the next 3
trading days, roll forward by 3. The choice for the round starting on day D is made at the close of D's first bar,
and uses only P&L of intervals that end by then. Compared with each fixed variant over the same days.

Reported: per-round results; correlation between a rule's last-window result and its next-3-day result (Pearson and
rank, moving-block bootstrap CI over rounds); total net after cost by pair and year; the share of P&L in trades held
across 17:00 New York; and, beside money, a drift-adjusted score (one drift per pair over the evaluated span, the
form of episodes:span_drift/2).
"""

import argparse
import json
import math
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import common  # noqa: E402
import events  # noqa: E402
import signal_study as ss  # noqa: E402

COMM = common.COMMISSION_BP_PER_SIDE
RULES = ["ma_50_200", "rsi_30_70_rev", "pd_breakout", "donchian20", "round_bounce"]
WINDOWS = (5, 20, 126)
STEP = 3
EVAL = ("2008-01-01", "2014-12-31")


# ------------------------------------------------------------------------------------------------ rules

def _ffill_state(ev_vals, n):
    """ev_vals: array with NaN where nothing happens; returns the last value seen (0 before the first)."""
    s = pd.Series(ev_vals).ffill().fillna(0.0).to_numpy()
    return s.astype(np.int8)


def day_end_utc(trading_day):
    """17:00 New York at the end of each bar's trading day (UTC s)."""
    u, inv = np.unique(trading_day, return_inverse=True)
    ends = np.array([int(common.trading_day_end_utc(pd.Timestamp(str(d)).date()).timestamp()) for d in u], np.int64)
    return ends[inv]


def no_trade_mask(excl):
    """rows on which no position may be held (SPEC 5a): excluded rows, and the last row before each excluded
    stretch (u_t is the position held after the close of row t, so a position is closed at that row's close)."""
    excl = np.asarray(excl, bool)
    return excl | np.r_[excl[1:], False]


def rule_positions(bars, X, pair, excl=None):
    """positions of the five rules. On excluded rows (quality_excluded, SPEC 5a) events are ignored and the
    event-driven rules are reset to flat, so a position closed before a stale stretch is only reopened by a new
    signal after it; ma_50_200 has no state and resumes. No rule holds a position on a no-trade row."""
    bars = events.to_ticks(bars, pair)          # SPEC 6b: price comparisons on the grid, equal counts as above
    x = bars[X]
    c, h, l = x["mid_close"], x["mid_high"], x["mid_low"]
    n = len(c)
    excl = np.zeros(n, bool) if excl is None else np.asarray(excl, bool)
    out = {}
    s50, s200 = events.grid_sma(c, 50, events.grid(pair)), events.grid_sma(c, 200, events.grid(pair))
    with np.errstate(invalid="ignore"):
        out["ma_50_200"] = np.where(np.isfinite(s200), np.where(s50 >= s200, 1, -1), 0).astype(np.int8)
    r = events.rsi(c, 14)
    rp = np.r_[np.nan, r[:-1]]
    ev = np.full(n, np.nan)
    with np.errstate(invalid="ignore"):
        ev[(rp < 50) & (r >= 50) | (rp > 50) & (r <= 50)] = 0.0
        ev[(rp < 30) & (r >= 30)] = 1.0
        ev[(rp > 70) & (r <= 70)] = -1.0
    ev[excl] = 0.0
    out["rsi_30_70_rev"] = _ffill_state(ev, n)
    lv = events.day_levels(bars, x)
    up = events.level_events(c, h, l, lv["pdh"])[0] == 1
    dn = events.level_events(c, h, l, lv["pdl"])[0] == -1
    ev = np.full(n, np.nan)
    ev[up] = 1.0
    ev[dn] = -1.0
    ev[excl] = 0.0
    ev = pd.Series(ev).groupby(x["trading_day"]).ffill().fillna(0.0).to_numpy()
    last_bar = x["end"] >= day_end_utc(x["trading_day"])
    out["pd_breakout"] = np.where(last_bar, 0, ev).astype(np.int8)
    d = events.donchian_events(c, h, l, 20)
    out["donchian20"] = _ffill_state(np.where(excl, 0, np.where(d != 0, d, np.nan)), n)
    t = events.grid_events(c, h, l, events.FIGURE_TICKS, 0.0)[1]
    t = np.where(excl, 0, t).astype(np.int8)
    idx = np.where(t != 0, np.arange(n), -1)
    last = np.maximum.accumulate(idx)
    age = np.arange(n) - last
    out["round_bounce"] = np.where((last >= 0) & (age < 4), -t[np.maximum(last, 0)], 0).astype(np.int8)
    nt = no_trade_mask(excl)
    return {k: np.where(nt, 0, v).astype(np.int8) for k, v in out.items()}


def variants(pos):
    names, U = ["flat"], [np.zeros_like(next(iter(pos.values())))]
    for r in RULES:
        names += [f"{r}:follow", f"{r}:fade"]
        U += [pos[r], (-pos[r]).astype(np.int8)]
    return names, np.vstack(U)


# ------------------------------------------------------------------------------------------------ accounting

def bar_pnl(u, mid, bid, ask, comm=COMM):
    """Bar-level P&L of a position series. Returns gross (mid P&L of interval t), cost (fills charged to the held
    interval they belong to), and the trades (entry bar, exit bar, sign, gross, cost)."""
    u = np.asarray(u, np.int8).copy()
    n = len(u)
    if n == 0:
        z = np.zeros(0)
        return z, z, {"entry": np.zeros(0, int), "exit": np.zeros(0, int), "sign": np.zeros(0, int), "gross": z,
                      "cost": z}
    u[-1] = 0                                   # anything open is closed at the last bar
    prev = np.r_[0, u[:-1]].astype(np.int8)
    change = u != prev
    start = np.maximum.accumulate(np.where(change, np.arange(n), 0))
    emid = mid[start]
    dm = np.r_[np.diff(mid), 0.0]
    gross = u * dm / emid * 1e4
    cost = np.zeros(n)
    ent = np.flatnonzero(change & (u != 0))
    cost[ent] += np.where(u[ent] > 0, ask[ent] - mid[ent], mid[ent] - bid[ent]) / mid[ent] * 1e4 + comm
    ex = np.flatnonzero(change & (prev != 0))
    em = emid[ex - 1]
    cost[ex - 1] += np.where(prev[ex] > 0, mid[ex] - bid[ex], ask[ex] - mid[ex]) / em * 1e4 + comm
    # trades: runs of constant non-zero u; the k-th entry pairs with the k-th exit
    run = np.cumsum(change)
    g_run = np.bincount(run, weights=gross)
    c_run = np.bincount(run, weights=cost)
    trades = {"entry": ent, "exit": ex, "sign": u[ent].astype(int), "gross": g_run[run[ent]], "cost": c_run[run[ent]]}
    assert len(ent) == len(ex)
    return gross, cost, trades


# ------------------------------------------------------------------------------------------------ walk-forward

def _pnl_by_day(u, x, gix, D):
    g, c, tr = bar_pnl(u, x["mid_close"], x["bid_close"], x["ask_close"])
    return np.bincount(gix, weights=g, minlength=D), np.bincount(gix, weights=c, minlength=D), g, c, tr


def strategy_report(u, x, gix, D, td):
    """daily gross/cost and the 17:00 breakdown of one position series on one pair."""
    gd, cd, g, c, tr = _pnl_by_day(u, x, gix, D)
    cross_tr = td[tr["exit"]] != td[tr["entry"]]
    tnet = tr["gross"] - tr["cost"]
    across = np.r_[td[1:] != td[:-1], False]
    return {"gross_day": gd, "cost_day": cd, "trades": len(tr["entry"]),
            "net_trades_across_17": float(tnet[cross_tr].sum()), "net_trades_within_day": float(tnet[~cross_tr].sum()),
            "gross_trades_across_17": float(tr["gross"][cross_tr].sum()),
            "gross_trades_within_day": float(tr["gross"][~cross_tr].sum()),
            "gross_intervals_across_17": float(g[across].sum()), "gross_all_intervals": float(g.sum()),
            "exposure": float(np.abs(u).sum()), "u": u}


def walkforward(bars_by_pair, X="H1", through=None, eval_period=EVAL, windows=WINDOWS, step=STEP, boot=1000, seed=0,
                quality_intervals=None):
    guarded = {p: ss.guard_bars(b, through)[0] for p, b in bars_by_pair.items()}
    quality_intervals = quality_intervals or {}
    pairs = list(guarded)
    allday = np.unique(np.concatenate([ss.day_number(guarded[p][X]["trading_day"]) for p in pairs]))
    D = len(allday)
    e0 = int(np.searchsorted(allday, ss.to_daynum(eval_period[0])))
    e1 = int(np.searchsorted(allday, ss.to_daynum(eval_period[1]), side="right"))
    rounds = np.arange(e0, e1, step)
    if len(rounds) == 0:
        raise ValueError("no evaluation days")
    W_all = list(windows) + ["all"]
    per_pair = {}
    for pair in pairs:
        x = guarded[pair][X]
        gix = np.searchsorted(allday, ss.day_number(x["trading_day"]))
        excl = ss.excluded_rows(x, quality_intervals.get(pair))
        names, U = variants(rule_positions(guarded[pair], X, pair, excl))
        V = len(names)
        Vnet, Vgross = np.zeros((V, D)), np.zeros((V, D))
        for v in range(V):
            gd, cd, *_ = _pnl_by_day(U[v], x, gix, D)
            Vnet[v], Vgross[v] = gd - cd, gd
        C = np.concatenate([np.zeros((V, 1)), np.cumsum(Vnet, axis=1)], axis=1)
        test_end = np.minimum(rounds + step, e1)
        test = C[:, test_end] - C[:, rounds]                  # (V, R) next-3-day net of each variant
        in_eval = (gix >= e0) & (gix < e1)
        round_of_bar = np.searchsorted(rounds, gix, side="right") - 1
        strat, choice, train_scores = {}, {}, {}
        for W in W_all:
            lo = np.zeros_like(rounds) if W == "all" else np.maximum(rounds - W, 0)
            train = C[:, rounds] - C[:, lo]                     # (V, R)
            ch = np.argmax(train, axis=0)                       # flat (index 0) wins ties
            choice[W], train_scores[W] = ch, train
            u = np.where(in_eval, U[ch[np.clip(round_of_bar, 0, len(rounds) - 1)], np.arange(len(gix))], 0)
            strat[f"rolling_W{W}"] = strategy_report(u.astype(np.int8), x, gix, D, x["trading_day"])
        for v in range(1, V):
            strat[f"static:{names[v]}"] = strategy_report(np.where(in_eval, U[v], 0).astype(np.int8), x, gix, D,
                                                          x["trading_day"])
        mid = x["mid_close"]
        r = np.r_[np.diff(mid) / mid[:-1] * 1e4, 0.0]
        mu = float(r[in_eval].sum() / max(1, in_eval.sum()))   # one drift per pair over the evaluated span
        for s in strat.values():
            s["drift_adj"] = float((s["gross_day"] - s["cost_day"]).sum() - mu * s["u"][in_eval].sum())
        per_pair[pair] = {"names": names, "train": train_scores, "choice": choice, "test": test, "strat": strat,
                          "mu_bp_per_bar": mu, "Vnet": Vnet, "Vgross": Vgross}
    return _assemble(per_pair, pairs, allday, rounds, e0, e1, W_all, step, boot, seed)


def _assemble(per_pair, pairs, allday, rounds, e0, e1, W_all, step, boot, seed):
    names = per_pair[pairs[0]]["names"]
    # per-round results
    rr = []
    for pair in pairs:
        P = per_pair[pair]
        for W in W_all:
            ch = P["choice"][W]
            st = P["strat"][f"rolling_W{W}"]
            net_day = st["gross_day"] - st["cost_day"]
            Cs = np.r_[0, np.cumsum(net_day)]
            te = np.minimum(rounds + step, e1)
            for i, k in enumerate(rounds):
                rr.append({"pair": pair, "W": str(W), "round_start": int(allday[k]), "chosen": names[ch[i]],
                           "train_net": float(P["train"][W][ch[i], i]), "test_net_variant": float(P["test"][ch[i], i]),
                           "test_net_strategy": float(Cs[te[i]] - Cs[k])})
    rounds_df = pd.DataFrame(rr)
    rounds_df["round_start"] = pd.to_datetime(rounds_df["round_start"], unit="D").dt.date
    # strategies: per pair and basket (equal notional: mean of the pairs' daily net)
    srows, yrows = [], []
    days = allday[e0:e1]
    years = pd.to_datetime(days, unit="D").year.to_numpy()
    wk = ss.week_key(days)
    for sname in per_pair[pairs[0]]["strat"]:
        basket = np.zeros(e1 - e0)
        basket_g = np.zeros(e1 - e0)
        tot = {k: 0.0 for k in ("gross", "cost", "drift_adj", "net_x17", "net_in", "g_x17", "g_in", "gi_x17", "gi_all")}
        trades = 0
        for pair in pairs:
            s = per_pair[pair]["strat"][sname]
            net = (s["gross_day"] - s["cost_day"])[e0:e1]
            basket += net / len(pairs)
            basket_g += s["gross_day"][e0:e1] / len(pairs)
            tot["gross"] += s["gross_day"][e0:e1].sum()
            tot["cost"] += s["cost_day"][e0:e1].sum()
            tot["drift_adj"] += s["drift_adj"]
            tot["net_x17"] += s["net_trades_across_17"]
            tot["net_in"] += s["net_trades_within_day"]
            tot["g_x17"] += s["gross_trades_across_17"]
            tot["g_in"] += s["gross_trades_within_day"]
            tot["gi_x17"] += s["gross_intervals_across_17"]
            tot["gi_all"] += s["gross_all_intervals"]
            trades += s["trades"]
            st = ss.tstats(net, np.arange(len(net)), wk - wk.min())
            stg = ss.tstats(s["gross_day"][e0:e1], np.arange(len(net)), wk - wk.min())
            srows.append({"strategy": sname, "pair": pair, "net_bp": float(net.sum()), "t_gross": stg["t"],
                          "gross_bp": float(s["gross_day"][e0:e1].sum()), "cost_bp": float(s["cost_day"][e0:e1].sum()),
                          "drift_adj_bp": s["drift_adj"], "trades": s["trades"], "net_bp_per_day": st["mean"],
                          "t": st["t"], "net_trades_across_17": s["net_trades_across_17"],
                          "net_trades_within_day": s["net_trades_within_day"],
                          "gross_trades_across_17": s["gross_trades_across_17"],
                          "gross_intervals_across_17": s["gross_intervals_across_17"]})
        st = ss.tstats(basket, np.arange(len(basket)), wk - wk.min())
        stg = ss.tstats(basket_g, np.arange(len(basket)), wk - wk.min())
        n = len(pairs)
        srows.append({"strategy": sname, "pair": "BASKET", "net_bp": float(basket.sum()), "t_gross": stg["t"],
                      "gross_bp": tot["gross"] / n,
                      "cost_bp": tot["cost"] / n, "drift_adj_bp": tot["drift_adj"] / n, "trades": trades,
                      "net_bp_per_day": st["mean"], "t": st["t"], "net_trades_across_17": tot["net_x17"] / n,
                      "net_trades_within_day": tot["net_in"] / n, "gross_trades_across_17": tot["g_x17"] / n,
                      "gross_intervals_across_17": tot["gi_x17"] / n})
        for y in np.unique(years):
            yrows.append({"strategy": sname, "year": int(y), "basket_net_bp": float(basket[years == y].sum())})
    strat_df = pd.DataFrame(srows)
    year_df = pd.DataFrame(yrows).pivot(index="strategy", columns="year", values="basket_net_bp")
    # does the last window predict the next 3 days?
    rng = np.random.default_rng(seed)
    crow = []
    follow = [i for i, nm in enumerate(names) if nm.endswith(":follow")]
    groups = [(names[i].split(":")[0], [i]) for i in follow] + [("ALL_RULES", follow)]
    perms = [_block_perm(len(allday), step, rng) for _ in range(PERMS)]
    for W in W_all:
        L = 20 if W == "all" else max(5, int(math.ceil(W / step)))   # covers the overlap of training windows
        for measure in ("net", "gross"):
            key = "Vnet" if measure == "net" else "Vgross"
            for label, vs in groups:
                M = np.vstack([per_pair[p][key][v] for p in pairs for v in vs])
                xs, ys = _xy(M, rounds, W, step, e1)
                r, lo, hi = boot_corr(xs, ys, L, boot, rng)
                rk, _, _ = boot_corr(_ranks(xs), _ranks(ys), L, 0, rng)
                null = np.array([float(_pooled_corr(*_xy(M[:, pm], rounds, W, step, e1))) for pm in perms])
                crow.append({"W": str(W), "rule": label, "measure": measure, "rounds": len(rounds),
                             "series": xs.shape[0], "block": L, "pearson": r, "boot_lo": lo, "boot_hi": hi,
                             "null_mean": float(null.mean()), "null_lo": float(np.percentile(null, 2.5)),
                             "null_hi": float(np.percentile(null, 97.5)),
                             "excess": r - float(null.mean()),
                             "p_perm": float(np.mean(np.abs(null - null.mean()) >= abs(r - null.mean()))),
                             "rank": rk})
    corr_df = pd.DataFrame(crow)
    return {"rounds": rounds_df, "strategies": strat_df, "by_year": year_df, "corr": corr_df,
            "mu": {p: per_pair[p]["mu_bp_per_bar"] for p in pairs}, "pairs": pairs,
            "eval_days": int(e1 - e0), "n_rounds": int(len(rounds)), "per_pair": per_pair}


PERMS = 200


def _xy(M, rounds, W, step, e1):
    """training-window sum (x) and next-`step`-day sum (y) of each daily series in M at each round."""
    C = np.concatenate([np.zeros((M.shape[0], 1)), np.cumsum(M, axis=1)], axis=1)
    lo = np.zeros_like(rounds) if W == "all" else np.maximum(rounds - W, 0)
    te = np.minimum(rounds + step, e1)
    return C[:, rounds] - C[:, lo], C[:, te] - C[:, rounds]


def _block_perm(D, step, rng):
    """a random order of the days in blocks of `step` (the same order for every series). Under this order nothing
    predicts anything, but the overlap of the training windows is unchanged, so the correlation it gives is the
    statistic's own centre under 'no predictability' (it is below zero: a training sum demeaned over the sample
    contains the later test sums; Stambaugh-type bias)."""
    nb = int(math.ceil(D / step))
    order = rng.permutation(nb)
    idx = (order[:, None] * step + np.arange(step)).ravel()
    return idx[idx < D]


def _ranks(a):
    return np.vstack([pd.Series(r).rank().to_numpy() for r in a])


def _pooled_corr(x, y):
    xd = x - x.mean(axis=-1, keepdims=True)
    yd = y - y.mean(axis=-1, keepdims=True)
    num = (xd * yd).sum(axis=(-1, -2))
    den = np.sqrt((xd ** 2).sum(axis=(-1, -2)) * (yd ** 2).sum(axis=(-1, -2)))
    with np.errstate(invalid="ignore", divide="ignore"):
        return num / den


def boot_corr(x, y, L, B, rng):
    """pooled correlation (series demeaned separately) and a 95% moving-block bootstrap interval over rounds; a block
    keeps all series of the same rounds together."""
    R = x.shape[1]
    r0 = float(_pooled_corr(x, y))
    L = min(L, R)
    nb = int(math.ceil(R / L))
    out = []
    for b0 in range(0, B, 50):
        bb = min(50, B - b0)
        st = rng.integers(0, R - L + 1, size=(bb, nb))
        idx = (st[:, :, None] + np.arange(L)).reshape(bb, -1)[:, :R]
        xs = np.transpose(x[:, idx], (1, 0, 2))
        ys = np.transpose(y[:, idx], (1, 0, 2))
        out.append(_pooled_corr(xs, ys))
    if not out:
        return r0, float("nan"), float("nan")
    v = np.concatenate(out)
    v = v[np.isfinite(v)]
    return r0, float(np.percentile(v, 2.5)), float(np.percentile(v, 97.5))


# ------------------------------------------------------------------------------------------------ report

def report_text(res, X):
    L = [f"Rule walk-forward on {X}, pairs {' '.join(res['pairs'])}, {res['eval_days']} evaluated days, "
         f"{res['n_rounds']} rounds of {STEP} days"]
    s = res["strategies"]
    b = s[s["pair"] == "BASKET"].copy()
    b["x17_share_net"] = b["net_trades_across_17"] / (b["net_trades_across_17"] + b["net_trades_within_day"]).abs()
    L.append("\nBASKET (equal notional; bp of one unit, summed over the evaluated days):")
    L.append(b[["strategy", "net_bp", "gross_bp", "cost_bp", "drift_adj_bp", "net_bp_per_day", "t", "t_gross", "trades",
                "net_trades_across_17", "net_trades_within_day", "gross_intervals_across_17"]]
             .to_string(index=False, float_format=lambda v: f"{v:,.2f}"))
    L.append("\nnet bp by year (basket):")
    L.append(res["by_year"].to_string(float_format=lambda v: f"{v:,.1f}"))
    L.append("\nlast-window result vs next-3-day result (pooled over pairs; boot = 95% moving-block bootstrap; null = "
             "the same statistic on days put in random order in blocks of 3, which keeps the window overlap; "
             "excess = pearson - null_mean, p_perm two-sided):")
    L.append(res["corr"].to_string(index=False, float_format=lambda v: f"{v:+.3f}"))
    ch = res["rounds"].groupby(["W", "chosen"]).size().unstack(fill_value=0)
    L.append("\nhow often each variant was chosen:")
    L.append(ch.to_string())
    return "\n".join(L) + "\n"


def cmd_synth(a):
    import synthetic
    gen = {}
    if a.trend:
        gen.update(trend_bp_day=a.trend, trend_days=a.trend_days)
    if a.drift:
        gen["drift_bp_day"] = a.drift
    if a.effect:
        gen.update(effect=a.effect, strength_bp=a.strength)
    t0 = time.time()
    bars = synthetic.generate(a.pairs.split(","), a.warmup, a.end, seed=a.seed, **gen)
    res = walkforward(bars, a.tf, a.end, (a.start, a.end), boot=a.boot)
    print(report_text(res, a.tf))
    print(f"runtime {time.time() - t0:.1f} s")


def cmd_run(a):
    ok, why = ss.prereg_approved()
    if not ok:
        raise ss.NotApprovedError(f"refusing to compute any rule result on real data: {why} (SPEC section 9)")
    through = common.allowed_through(a.through, False)
    bars = {p: ss.load_bars(p, through)[0] for p in common.PAIRS}
    qual = {p: ss.quality.load(p) for p in common.PAIRS}           # required (SPEC 5a)
    out = Path(a.out) if a.out else common.DERIVED_DIR / "rule_walkforward" / time.strftime("%Y%m%dT%H%M%S")
    out.mkdir(parents=True, exist_ok=True)
    for X in common.DECISION_TFS:
        res = walkforward(bars, X, through, quality_intervals=qual)
        res["rounds"].to_csv(out / f"rounds_{X}.csv", index=False)
        res["strategies"].to_csv(out / f"strategies_{X}.csv", index=False)
        res["by_year"].to_csv(out / f"by_year_{X}.csv")
        res["corr"].to_csv(out / f"corr_{X}.csv", index=False)
        (out / f"summary_{X}.txt").write_text(report_text(res, X))
    (out / "meta.json").write_text(json.dumps({"through": through.isoformat(), "git_commit": common.git_commit(),
                                               "git_dirty": common.git_dirty()}, indent=1))
    print(f"written to {out}")


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run")
    common.add_guard_args(r)
    r.add_argument("--out")
    s = sub.add_parser("synth")
    s.add_argument("--pairs", default="EURUSD,GBPUSD,USDJPY,USDCHF,USDCAD,NZDUSD")
    s.add_argument("--tf", default="H1")
    s.add_argument("--warmup", default="2007-01-01")
    s.add_argument("--start", default="2008-01-01")
    s.add_argument("--end", default="2014-12-31")
    s.add_argument("--seed", type=int, default=0)
    s.add_argument("--trend", type=float, default=0.0)
    s.add_argument("--trend-days", type=float, default=20)
    s.add_argument("--drift", type=float, default=0.0)
    s.add_argument("--effect", default=None)
    s.add_argument("--strength", type=float, default=0.0)
    s.add_argument("--boot", type=int, default=1000)
    a = ap.parse_args(argv)
    {"run": cmd_run, "synth": cmd_synth}[a.cmd](a)


if __name__ == "__main__":
    main()
