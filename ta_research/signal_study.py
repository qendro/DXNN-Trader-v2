"""Stage 1 event study: do technical-analysis signal events show a reaction in the mid price, beyond placebo levels,
large enough against cost to be worth learning? (SPEC sections 1, 7, 8; PREREG_TA_SIGNALS.md.)

For every signal (events.signal_catalogue), decision timeframe X in {M15, H1, H4}, pair and horizon h in HORIZONS
(decision bars), each event at bar t is a position in the event's direction d entered at the close of bar t and closed
at the close of bar t+h:

  gross (continuation, at the mid)  d * (mid[t+h] - mid[t]) / mid[t] * 1e4                 (reversal = its negative)
  cost                              half-spread at entry + half-spread at exit, both / mid[t] * 1e4, + 2 x 0.2 bp
  net                               gross - cost   (computed for the continuation and for the reversal trade)

Overlap: events of one series are thinned so that the kept trades never overlap: the first event is kept, then the
next event at or after t + h, and so on (greedy, earliest first; it uses only the past, so it is a trade a trader
could have made). Standard errors are clustered by trading day and by week (SPEC 8); t is the smaller in absolute
value. Pooled ("ALL") rows put the six pairs' events together; clusters are calendar trading days and weeks, so
correlation between pairs on the same day is in the standard error.

Placebo: the same event logic on levels nobody watches (events.py: SMA 37/67 and 163/237, RSI 11/17, Bollinger and
Donchian 15/26, levels +-0.37 D1 ATR, a figure grid offset 0.37/0.63). The two placebos of a signal are pooled. The
difference standard - placebo has a clustered standard error from the paired cluster contributions.

Commands
  run         the registered run on real bars. Refuses unless PREREG_TA_SIGNALS.md is committed with
              "Status: APPROVED".
  crosscheck  compare this tool's event definitions with a features.py table (no returns are computed).
  synth       the whole study on synthetic data (synthetic.py).
  validate    false-positive rate and planted-effect recovery over many synthetic seeds.
"""

import argparse
import datetime as dt
import json
import math
import subprocess
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import common  # noqa: E402
import events  # noqa: E402
import quality  # noqa: E402

HORIZONS = (1, 4, 16)
COMM = common.COMMISSION_BP_PER_SIDE
PERIODS = {"discovery": ("2008-01-01", "2011-12-31"), "confirmation": ("2012-01-01", "2014-12-31")}
GATE = {"disc_t": 2.5, "min_events": 50, "min_days": 30, "alpha": 0.05, "conf_t_min": 2.0, "ratio_go": 0.5,
        "ratio_owner": 0.25, "lopo_t": 2.0}
IMPOSSIBLE_T = 8.0
IMPOSSIBLE_HIT = 0.75
CROSSCHECK_MAX_MISMATCH = 0.01
PREREG = HERE / "PREREG_TA_SIGNALS.md"


class CrossCheckError(RuntimeError):
    pass


class NotApprovedError(RuntimeError):
    pass


# ------------------------------------------------------------------------------------------------ date guard

def guard_bars(bars, through=None, unseal_holdout=False):
    """Drop every bar not known by the end of the last allowed trading day (common.allowed_through)."""
    day = common.allowed_through(through, unseal_holdout)
    limit = common.guard_limit_utc_seconds(day)
    out, dropped = {}, {}
    for tf, b in bars.items():
        keep = b["end"] <= limit
        dropped[tf] = int((~keep).sum())
        out[tf] = {k: v[keep] for k, v in b.items()}
    return out, {"through": day.isoformat(), "limit_utc": limit, "dropped_rows": dropped}


def guard_features(feat, through=None, unseal_holdout=False):
    day = common.allowed_through(through, unseal_holdout)
    limit = common.guard_limit_utc_seconds(day)
    keep = feat["available"] <= limit
    return {k: v[keep] for k, v in feat.items()}, int((~keep).sum())


# ------------------------------------------------------------------------------------------------ helpers

def day_number(yyyymmdd_arr):
    a = np.asarray(yyyymmdd_arr, dtype=np.int64)
    y, m, d = a // 10000, a // 100 % 100, a % 100
    return (pd.to_datetime(pd.DataFrame({"year": y, "month": m, "day": d})).to_numpy()
            .astype("datetime64[D]").astype(np.int64))


def to_daynum(iso):
    return (np.datetime64(iso, "D") - np.datetime64("1970-01-01", "D")).astype(np.int64)


def week_key(daynum):
    return daynum - (daynum + 3) % 7      # the Monday of the week


def thin(idx, h):
    """greedy non-overlapping subset of sorted event indices: keep one, skip until t + h."""
    if h <= 1 or len(idx) == 0:
        return idx
    nxt = np.searchsorted(idx, idx + h).tolist()
    keep, i, L = [], 0, len(idx)
    while i < L:
        keep.append(i)
        i = nxt[i]
    return idx[keep]


def norm_sf(z):
    return 0.5 * math.erfc(z / math.sqrt(2.0))


def _cl_var(x, k, m):
    S = np.bincount(k, weights=x)
    c = np.bincount(k)
    nz = c > 0
    G = int(nz.sum())
    n = len(x)
    if G < 2:
        return float("nan"), G
    return G / (G - 1) * float(np.sum((S[nz] - c[nz] * m) ** 2)) / n ** 2, G


def tstats(x, day, week):
    """mean and clustered t by day and by week; t = the smaller in absolute value."""
    n = len(x)
    if n < 3:
        return {"n": n, "mean": float("nan"), "t_day": float("nan"), "t_week": float("nan"), "t": float("nan"),
                "days": 0, "sd": float("nan")}
    m = float(x.mean())
    vd, G = _cl_var(x, day, m)
    vw, _ = _cl_var(x, week, m)
    td = m / math.sqrt(vd) if vd > 0 else float("nan")
    tw = m / math.sqrt(vw) if vw > 0 else float("nan")
    t = td if abs(td) <= abs(tw) else tw
    return {"n": n, "mean": m, "t_day": td, "t_week": tw, "t": t, "days": G, "sd": float(x.std(ddof=1))}


def _cl_diff_var(xa, ka, xb, kb):
    ma, mb = xa.mean(), xb.mean()
    L = max(ka.max(), kb.max()) + 1
    a = np.bincount(ka, weights=xa - ma, minlength=L) / len(xa)
    b = np.bincount(kb, weights=xb - mb, minlength=L) / len(xb)
    nz = (np.bincount(ka, minlength=L) > 0) | (np.bincount(kb, minlength=L) > 0)
    G = int(nz.sum())
    return G / (G - 1) * float(np.sum((a[nz] - b[nz]) ** 2)) if G > 1 else float("nan")


def diffstats(A, B):
    if len(A["g"]) < 3 or len(B["g"]) < 3:
        return {"diff": float("nan"), "t_diff_day": float("nan"), "t_diff_week": float("nan"), "t_diff": float("nan")}
    d = float(A["g"].mean() - B["g"].mean())
    vd = _cl_diff_var(A["g"], A["day"], B["g"], B["day"])
    vw = _cl_diff_var(A["g"], A["week"], B["g"], B["week"])
    td = d / math.sqrt(vd) if vd > 0 else float("nan")
    tw = d / math.sqrt(vw) if vw > 0 else float("nan")
    return {"diff": d, "t_diff_day": td, "t_diff_week": tw, "t_diff": td if abs(td) <= abs(tw) else tw}


# ------------------------------------------------------------------------------------------------ event records

def excluded_rows(x, intervals):
    """quality_excluded for the rows of decision bars x (SPEC 5a), from the stale-quote intervals of quality.py."""
    if not intervals:
        return np.zeros(len(x["end"]), bool)
    return quality.excluded_mask(x["end"], intervals)


def exit_index(n, h, excl):
    """exit row of a position entered at row t for h bars: t + h, or the last row before the next excluded stretch if
    that comes first (SPEC 5a: open positions are closed at the last row before the interval). -1 where no position
    can be opened (t excluded, or the very next row is excluded, or t + h is past the data)."""
    t = np.arange(n)
    ex = t + h
    if excl is not None and excl.any():
        nxt = np.searchsorted(np.flatnonzero(excl), t, side="right")
        first = np.r_[np.flatnonzero(excl), n][nxt]               # first excluded row after t (n if none)
        ex = np.minimum(ex, first - 1)
        ex = np.where(excl, -1, ex)
    ex = np.where((ex > t) & (ex < n), ex, -1)
    return ex


def outcomes(x, h, excl=None):
    """per entry bar t: up-move at mid (bp), cost of a long and of a short round trip (bp), and the exit row."""
    mid, bid, ask = x["mid_close"], x["bid_close"], x["ask_close"]
    n = len(mid)
    ex = exit_index(n, h, excl)
    ok = ex >= 0
    e = np.where(ok, ex, 0)
    g = np.where(ok, (mid[e] - mid) / mid * 1e4, np.nan)
    cl = np.where(ok, ((ask - mid) + (mid[e] - bid[e])) / mid * 1e4 + 2 * COMM, np.nan)
    cs = np.where(ok, ((mid - bid) + (ask[e] - mid[e])) / mid * 1e4 + 2 * COMM, np.nan)
    return g, cl, cs, ex


def series_records(series, x_out, dayn, h, periods):
    """event records of one event series at horizon h: events on rows where no position can be opened (excluded rows,
    the end of the data) are skipped first, then the rest are thinned to non-overlapping trades."""
    g_up, cl, cs, ex = x_out
    idx = thin(np.flatnonzero((series != 0) & (ex >= 0)), h)
    d = series[idx].astype(float)
    g = d * g_up[idx]
    cc = np.where(d > 0, cl[idx], cs[idx])
    cr = np.where(d > 0, cs[idx], cl[idx])
    e_day, x_day = dayn[idx], dayn[ex[idx]]
    pm = np.zeros(len(idx), np.int8)
    for bit, (p0, p1) in enumerate(periods.values()):
        pm |= (((e_day >= p0) & (x_day <= p1)).astype(np.int8) << bit)
    ok = np.isfinite(g) & np.isfinite(cc) & np.isfinite(cr) & (pm != 0)
    return {"g": g[ok], "cc": cc[ok], "cr": cr[ok], "dayn": e_day[ok], "pm": pm[ok]}


def _cat(recs):
    keys = ("g", "cc", "cr", "dayn", "pm", "pair", "k")
    if not recs:
        return {k: np.zeros(0) for k in keys}
    return {k: np.concatenate([r[k] for r in recs]) for k in keys}


def pair_records(bars, pair, pair_id, X, horizons, periods, extra_signals=None, intervals=None):
    """{(signal, h, kind): records} for one pair and decision timeframe, plus signal metadata."""
    x = bars[X]
    excl = excluded_rows(x, intervals)
    dayn = day_number(x["trading_day"])
    cat = events.signal_catalogue(bars, X, pair)
    if extra_signals:
        cat = cat + extra_signals(bars, X, pair)
    meta, recs = {}, {}
    for h in horizons:
        xo = outcomes(x, h, excl)
        for s in cat:
            meta[s["name"]] = (s["family"], s["chart"])
            for kind, lst in (("std", [s["std"]]), ("plc", s["placebo"])):
                parts = []
                for j, ser in enumerate(lst):
                    r = series_records(ser, xo, dayn, h, periods)
                    r["pair"] = np.full(len(r["g"]), pair_id, np.int8)
                    r["k"] = np.full(len(r["g"]), j, np.int8)
                    parts.append(r)
                recs[(s["name"], h, kind)] = _cat(parts)
    return meta, recs


# ------------------------------------------------------------------------------------------------ the study

def _select(R, mask, day0):
    return {"g": R["g"][mask], "cc": R["cc"][mask], "cr": R["cr"][mask], "k": R["k"][mask],
            "day": (R["dayn"][mask] - day0).astype(np.int64),
            "week": (week_key(R["dayn"][mask]) - week_key(day0) + 7).astype(np.int64)}


def test_row(A, B):
    s = tstats(A["g"], A["day"], A["week"])
    p = tstats(B["g"], B["day"], B["week"])
    row = {"n_std": s["n"], "days_std": s["days"], "mean_std": s["mean"], "sd_std": s["sd"], "t_std_day": s["t_day"],
           "t_std_week": s["t_week"], "t_std": s["t"],
           "hit_std": float((A["g"] > 0).mean()) if len(A["g"]) else float("nan"),
           "n_plc": p["n"], "mean_plc": p["mean"], "t_plc": p["t"]}
    row.update(diffstats(A, B))
    for j in (0, 1):                                  # each placebo on its own (reported; used by one gate guard)
        Bj = {key: v[B["k"] == j] for key, v in B.items()}
        row[f"mean_plc{j + 1}"] = float(Bj["g"].mean()) if len(Bj["g"]) else float("nan")
        row[f"t_diff_plc{j + 1}"] = diffstats(A, Bj)["t_diff"]
    for k, src in (("cost_cont", "cc"), ("cost_rev", "cr")):
        row[k] = float(A[src].mean()) if len(A[src]) else float("nan")
    row["net_cont"] = float((A["g"] - A["cc"]).mean()) if len(A["g"]) else float("nan")
    row["net_rev"] = float((-A["g"] - A["cr"]).mean()) if len(A["g"]) else float("nan")
    return row


def run_study(bars_by_pair, through=None, horizons=HORIZONS, periods=None, decision_tfs=common.DECISION_TFS,
              extra_signals=None, lopo=True, quality_intervals=None):
    """The whole study in memory. bars_by_pair: {pair: {TF: fields}}; quality_intervals: {pair: stale-quote intervals
    as quality.py writes them} (SPEC 5a). Returns (rows DataFrame, info)."""
    quality_intervals = quality_intervals or {}
    periods = dict(periods or PERIODS)
    names = list(periods)
    p_all = (min(v[0] for v in periods.values()), max(v[1] for v in periods.values()))
    periods["full"] = p_all
    pnum = {k: (to_daynum(a), to_daynum(b)) for k, (a, b) in periods.items()}
    guarded, info = {}, {"guard": {}}
    for pair, b in bars_by_pair.items():
        guarded[pair], info["guard"][pair] = guard_bars(b, through)
    pairs = list(guarded)
    day0 = min(pnum[k][0] for k in pnum) - 7
    rows = []
    for X in decision_tfs:
        allrec, meta = {}, {}
        for pid, pair in enumerate(pairs):
            m, r = pair_records(guarded[pair], pair, pid, X, horizons, pnum, extra_signals,
                                quality_intervals.get(pair))
            meta.update(m)
            for k, v in r.items():
                allrec.setdefault(k, []).append(v)
        allrec = {k: _cat(v) for k, v in allrec.items()}
        for (sig, h, kind), A in allrec.items():
            if kind != "std":
                continue
            B = allrec[(sig, h, "plc")]
            for bit, per in enumerate(periods):
                ma, mb = (A["pm"] >> bit) & 1 == 1, (B["pm"] >> bit) & 1 == 1
                for pid, pair in enumerate(pairs + ["ALL"]):
                    if pair == "ALL":
                        sa, sb = ma, mb
                    else:
                        sa, sb = ma & (A["pair"] == pid), mb & (B["pair"] == pid)
                    row = {"period": per, "tf": X, "signal": sig, "family": meta[sig][0], "chart": meta[sig][1],
                           "horizon": h, "pair": pair}
                    row.update(test_row(_select(A, sa, day0), _select(B, sb, day0)))
                    if pair == "ALL" and lopo and len(pairs) > 1:
                        sg = np.sign(row["mean_std"]) or 1.0
                        worst = float("inf")
                        for q in range(len(pairs)):
                            r2 = test_row(_select(A, sa & (A["pair"] != q), day0), _select(B, sb & (B["pair"] != q), day0))
                            worst = min(worst, sg * r2["t_std"], sg * r2["t_diff"])
                        row["lopo_min_t"] = worst
                    rows.append(row)
    df = pd.DataFrame(rows)
    df["flag"] = np.where((df["t_std"].abs() >= IMPOSSIBLE_T) |
                          ((df["n_std"] >= 100) & ((df["hit_std"] >= IMPOSSIBLE_HIT) | (df["hit_std"] <= 1 - IMPOSSIBLE_HIT))),
                          "IMPOSSIBLE?", "")
    info.update({"pairs": pairs, "horizons": list(horizons), "periods": {k: list(v) for k, v in periods.items()},
                 "decision_tfs": list(decision_tfs), "n_tests_pooled": int(((df["pair"] == "ALL") &
                                                                          (df["period"] == names[0])).sum())})
    return df, info


# ------------------------------------------------------------------------------------------------ the gate

def gate(df, cfg=GATE, discovery="discovery", confirmation="confirmation"):
    A = df[df["pair"] == "ALL"]
    key = ["tf", "signal", "horizon"]
    disc = A[A["period"] == discovery].set_index(key)
    conf = A[A["period"] == confirmation].set_index(key)
    c = disc[(disc["t_std"].abs() >= cfg["disc_t"]) & (disc["t_diff"].abs() >= cfg["disc_t"]) &
             (np.sign(disc["mean_std"]) == np.sign(disc["diff"])) & (disc["n_std"] >= cfg["min_events"]) &
             (disc["days_std"] >= cfg["min_days"])]
    cands = []
    for k, r in c.iterrows():
        sg = float(np.sign(r["mean_std"]))
        q = conf.loc[k]
        p = max(norm_sf(sg * q["t_std"]), norm_sf(sg * q["t_diff"])) if np.isfinite(q["t_std"]) and np.isfinite(q["t_diff"]) else 1.0
        cost = q["cost_cont"] if sg > 0 else q["cost_rev"]
        beyond = bool(sg * (q["mean_std"] - q["mean_plc1"]) > 0 and sg * (q["mean_std"] - q["mean_plc2"]) > 0)
        cands.append({"tf": k[0], "signal": k[1], "horizon": k[2], "sign": sg, "disc_mean": r["mean_std"],
                      "beyond_both_placebos": beyond, "flag": q.get("flag", ""),
                      "disc_t": r["t_std"], "disc_t_diff": r["t_diff"], "conf_mean": q["mean_std"],
                      "conf_t": q["t_std"], "conf_diff": q["diff"], "conf_t_diff": q["t_diff"], "conf_n": q["n_std"],
                      "p": p, "cost": cost, "ratio": sg * q["mean_std"] / cost if cost > 0 else float("nan"),
                      "ratio_beyond_placebo": sg * q["diff"] / cost if cost > 0 else float("nan"),
                      "lopo_min_t": q.get("lopo_min_t", float("nan"))})
    K = len(cands)
    order = sorted(range(K), key=lambda i: cands[i]["p"])
    stop = False
    for rank, i in enumerate(order):
        thr = cfg["alpha"] / (K - rank)
        cands[i]["holm_threshold"] = thr
        floor = min(cands[i]["sign"] * cands[i]["conf_t"], cands[i]["sign"] * cands[i]["conf_t_diff"])
        if not stop and cands[i]["p"] <= thr and floor >= cfg["conf_t_min"]:
            cands[i]["confirmed"] = True
        else:
            stop = True
            cands[i]["confirmed"] = False
    conf_list = [x for x in cands if x["confirmed"]]
    def clean(x):
        return not (x["lopo_min_t"] < cfg["lopo_t"]) and x["beyond_both_placebos"] and not x["flag"]
    go = [x for x in conf_list if x["ratio"] >= cfg["ratio_go"] and clean(x)]
    go_one_pair = [x for x in conf_list if x["ratio"] >= cfg["ratio_go"] and not clean(x)]
    owner = [x for x in conf_list if x["ratio"] >= cfg["ratio_owner"]]
    if go:
        decision = "GO"
    elif owner or go_one_pair:
        decision = "OWNER DECIDES"
    else:
        decision = "STOP"
    best = max((x["ratio"] for x in conf_list), default=float("nan"))
    holm_full = holm_all(A[A["period"] == "full"], cfg["alpha"])
    return {"holm_full": holm_full, "decision": decision, "n_candidates": K, "n_confirmed": len(conf_list), "best_ratio": best,
            "candidates": cands, "config": dict(cfg)}


def holm_all(full, alpha=0.05):
    """Reported only: tests surviving Holm (two-sided, family alpha) over all pooled tests of the full period, against
    zero and against placebo."""
    out = {}
    for col in ("t_std", "t_diff"):
        t = full[col].to_numpy()
        ok = np.isfinite(t) & (full["n_std"].to_numpy() >= GATE["min_events"])
        p = np.array([2 * norm_sf(abs(v)) for v in t[ok]])
        names = (full["tf"] + " " + full["signal"] + " h" + full["horizon"].astype(str)).to_numpy()[ok]
        order = np.argsort(p)
        keep = []
        for r, i in enumerate(order):
            if p[i] <= alpha / (len(p) - r):
                keep.append(names[i])
            else:
                break
        out[col] = keep
    return out


# ------------------------------------------------------------------------------------------------ controls

def lookahead_canary(bars_by_pair, through=None, periods=None, X="H1", quality_intervals=None):
    """Positive control: the prior-day-high break series moved one bar EARLIER (it knows the next bar). The tool must
    report an impossible result for it. Negative control: random events at the same rate (seeded)."""
    periods = dict(periods or PERIODS)
    rng = np.random.default_rng(12345)

    def extra(bars, X_, pair):
        s = events.spec_event_columns(bars, X_, pair)["ev_pdh_break"]
        fut = np.r_[s[1:], 0].astype(np.int8)
        rnd = (rng.random(len(s)) < max(np.mean(s != 0), 1e-3)) * rng.choice([-1, 1], len(s))
        return [{"name": "CANARY_future_pdh_break", "family": "control", "chart": X_, "std": fut, "placebo": [s]},
                {"name": "CONTROL_random_events", "family": "control", "chart": X_, "std": rnd.astype(np.int8),
                 "placebo": [s]}]

    df, _ = run_study(bars_by_pair, through, horizons=(1,), periods=periods, decision_tfs=[X], extra_signals=extra,
                      quality_intervals=quality_intervals,
                      lopo=False)
    df = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["family"] == "control")]
    return df[["signal", "n_std", "mean_std", "t_std", "hit_std", "flag"]]


def crosscheck(bars, feats, X, pair, through=None, intervals=None):
    """Compare this tool's event series with a features.py table (SPEC section 6 names). No returns are computed.
    Returns a DataFrame per column; raises CrossCheckError if any compared column disagrees on more than 1% of its
    events."""
    b, _ = guard_bars(bars, through)
    f, _ = guard_features(feats, through)
    x = b[X]
    mine = events.spec_event_columns(b, X, pair)
    if intervals is not None or "quality_excluded" in f:
        mine["quality_excluded"] = excluded_rows(x, intervals).astype(np.int8)
    pos = np.searchsorted(f["available"], x["end"])
    ok = (pos < len(f["available"]))
    ok[ok] = f["available"][pos[ok]] == x["end"][ok]
    rows, bad = [], []
    for col, m in mine.items():
        if col not in f:
            rows.append({"column": col, "status": "missing in features"})
            continue
        fv = np.zeros(len(m), np.int64)
        fv[ok] = np.asarray(f[col])[pos[ok]].astype(np.int64)
        mv = m.astype(np.int64)
        union = int(((mv != 0) | (fv != 0)).sum())
        dis = int((mv != fv).sum())
        rate = dis / max(1, union)
        rows.append({"column": col, "status": "ok" if rate <= CROSSCHECK_MAX_MISMATCH else "MISMATCH",
                     "events_tool": int((mv != 0).sum()), "events_features": int((fv != 0).sum()),
                     "disagree": dis, "rate": rate, "rows_unmatched": int((~ok).sum())})
        if rate > CROSSCHECK_MAX_MISMATCH:
            bad.append(col)
    out = pd.DataFrame(rows)
    if bad:
        raise CrossCheckError(f"{pair} {X}: event definitions disagree with the feature table on {bad}\n{out}")
    return out


def no_result_reasons(df, controls, pairs=common.PAIRS):
    """Conditions fixed in the pre-registration under which the run gives NO RESULT instead of a decision."""
    why = []
    seen = set(df["pair"]) - {"ALL"}
    if set(pairs) - seen:
        why.append(f"pairs missing: {sorted(set(pairs) - seen)}")
    can = controls[controls["signal"] == "CANARY_future_pdh_break"]
    if len(can) == 0 or not (can["t_std"].iloc[0] >= IMPOSSIBLE_T):
        why.append("the look-ahead canary was not detected: the pipeline cannot be trusted")
    full = df[(df["pair"] != "ALL") & (df["period"] == "full")]
    spr = full.groupby("pair")["cost_cont"].mean() - 2 * COMM
    if (spr < 0.01).any():
        why.append(f"quoted spread at the events below 0.01 bp for {list(spr[spr < 0.01].index)}: these are not "
                   f"bid/ask data")
    return why


# ------------------------------------------------------------------------------------------------ report

def summary_text(df, g, info, controls=None, cross=None, runtime=None):
    L = []
    L.append("Stage 1 signal study")
    L.append(f"pairs {' '.join(info['pairs'])}; decision TFs {' '.join(info['decision_tfs'])}; horizons "
             f"{info['horizons']} decision bars; periods {info['periods']}")
    L.append(f"pooled tests per period: {info['n_tests_pooled']}; rows in CSV: {len(df)}")
    if runtime is not None:
        L.append(f"runtime {runtime:.1f} s")
    L.append("")
    for w in g.get("no_result", []):
        L.append(f"NO RESULT because: {w}")
    L.append(f"GATE DECISION: {g['decision']}   (candidates {g['n_candidates']}, confirmed {g['n_confirmed']}, "
             f"best gross/cost among confirmed {g['best_ratio']:.3f})")
    cfg = g["config"]
    L.append(f"  rule: discovery |t| >= {cfg['disc_t']} against zero and against placebo, same sign; confirmation "
             f"Holm at {cfg['alpha']} one-sided on max(p_std, p_diff) and both t >= {cfg['conf_t_min']}; GO if a confirmed reaction has gross/cost >= "
             f"{cfg['ratio_go']}, survives leaving out any one pair (t >= {cfg['lopo_t']}), lies beyond each placebo "
             f"and is not flagged; OWNER DECIDES if a confirmed reaction has gross/cost >= {cfg['ratio_owner']} or "
             f"fails only a guard; else STOP")
    L.append(f"reported only: Holm at {cfg['alpha']} over all pooled tests, full period: against zero "
             f"{g['holm_full']['t_std'] or 'none'}; against placebo {g['holm_full']['t_diff'] or 'none'}")
    if g["candidates"]:
        L.append("")
        L.append("candidates (discovery) and their confirmation:")
        L.append("  tf  signal                       h  sign  disc_mean disc_t  conf_mean conf_t conf_tdiff   cost  ratio  lopo  beyond_both  confirmed")
        for c in sorted(g["candidates"], key=lambda c: c["p"]):
            L.append(f"  {c['tf']:3s} {c['signal']:28s} {c['horizon']:2d} {c['sign']:+.0f}  {c['disc_mean']:+8.3f} "
                     f"{c['disc_t']:+6.2f}  {c['conf_mean']:+8.3f} {c['conf_t']:+6.2f} {c['conf_t_diff']:+6.2f}  "
                     f"{c['cost']:6.3f} {c['ratio']:+6.3f} {c['lopo_min_t']:+5.2f}  {str(c['beyond_both_placebos']):11s}  "
                     f"{c['confirmed']}")
    full = df[(df["pair"] == "ALL") & (df["period"] == "full")]
    L.append("")
    L.append("pooled tests, full period: distribution of t (gross at mid) and t against placebo")
    for col in ("t_std", "t_diff"):
        t = full[col].dropna()
        L.append(f"  {col}: n {len(t)}, mean {t.mean():+.2f}, sd {t.std():.2f}, |t|>=2: {(t.abs() >= 2).mean():.1%}, "
                 f"|t|>=3: {(t.abs() >= 3).mean():.1%}")
    L.append(f"  mean cost per round trip at the events: {full['cost_cont'].mean():.3f} bp")
    flagged = df[df["flag"] != ""]
    L.append(f"tests flagged IMPOSSIBLE? (|t| >= {IMPOSSIBLE_T} or hit rate outside {1 - IMPOSSIBLE_HIT:.2f}.."
             f"{IMPOSSIBLE_HIT:.2f}): {len(flagged)}")
    for _, r in flagged.head(10).iterrows():
        L.append(f"  {r['period']} {r['tf']} {r['signal']} h{r['horizon']} {r['pair']}: t {r['t_std']:+.1f} "
                 f"hit {r['hit_std']:.2f}")
    L.append("")
    L.append("largest |t against placebo|, pooled, full period (tests with at least the gate's minimum events):")
    big = full[full["n_std"] >= g["config"]["min_events"]]
    top = big.reindex(big["t_diff"].abs().sort_values(ascending=False).index).head(15)
    for _, r in top.iterrows():
        L.append(f"  {r['tf']:3s} {r['signal']:28s} h{r['horizon']:<2d} n {r['n_std']:6d} mean {r['mean_std']:+7.3f} "
                 f"t {r['t_std']:+5.2f} | placebo {r['mean_plc']:+7.3f} | diff t {r['t_diff']:+5.2f} | "
                 f"cost {r['cost_cont']:.3f}")
    if controls is not None:
        L.append("")
        L.append("controls (H1, h = 1, pooled, full period):")
        for _, r in controls.iterrows():
            L.append(f"  {r['signal']:28s} n {r['n_std']:6d} mean {r['mean_std']:+8.3f} t {r['t_std']:+7.2f} "
                     f"hit {r['hit_std']:.2f} {r['flag']}")
    if cross is not None:
        L.append("")
        L.append("cross-check against the feature tables:")
        L.append(cross.to_string(index=False))
    return "\n".join(L) + "\n"


# ------------------------------------------------------------------------------------------------ real data

def prereg_approved():
    """True only if PREREG_TA_SIGNALS.md is committed, unchanged since the commit, and says Status: APPROVED."""
    if not PREREG.exists():
        return False, "PREREG_TA_SIGNALS.md does not exist"
    if not any(line.strip().startswith("Status: APPROVED") for line in PREREG.read_text().splitlines()[:15]):
        return False, "PREREG_TA_SIGNALS.md is not marked 'Status: APPROVED'"
    try:
        subprocess.check_call(["git", "ls-files", "--error-unmatch", str(PREREG)], cwd=HERE,
                              stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        if subprocess.call(["git", "diff", "--quiet", "HEAD", "--", str(PREREG)], cwd=HERE) != 0:
            return False, "PREREG_TA_SIGNALS.md has changes that are not committed"
    except Exception:
        return False, "PREREG_TA_SIGNALS.md is not committed"
    return True, "approved"


def load_bars(pair, through):
    d = common.BARS_DIR / pair
    out = {}
    for tf in common.TIMEFRAMES:
        with np.load(d / f"{tf}.npz") as z:
            out[tf] = {k: z[k] for k in z.files}
    b, ginfo = guard_bars(out, through)
    return b, ginfo


def load_features(pair, X):
    p = common.FEATURES_DIR / pair / f"{X}.npz"
    if not p.exists():
        return None
    with np.load(p) as z:
        return {k: z[k] for k in z.files}


def cmd_run(a):
    ok, why = prereg_approved()
    if not ok:
        raise NotApprovedError(f"refusing to compute any return on real data: {why} (SPEC section 9)")
    through = common.allowed_through(a.through, False)
    if through < dt.date(2014, 12, 31):
        raise SystemExit("the registered run needs data through 2014-12-31")
    t0 = time.time()
    bars, cross, qual = {}, [], {}
    for pair in common.PAIRS:
        bars[pair], _ = load_bars(pair, through)
        qual[pair] = quality.load(pair)            # required (SPEC 5a): a missing file stops the run here
        for X in common.DECISION_TFS:
            f = load_features(pair, X)
            if f is not None:
                c = crosscheck(bars[pair], f, X, pair, through, qual[pair])
                c.insert(0, "tf", X)
                c.insert(0, "pair", pair)
                cross.append(c)
    df, info = run_study(bars, through, quality_intervals=qual)
    g = gate(df)
    controls = lookahead_canary(bars, through, quality_intervals=qual)
    g["no_result"] = no_result_reasons(df, controls)
    if g["no_result"]:
        g["decision"] = "NO RESULT"
    out = Path(a.out) if a.out else common.DERIVED_DIR / "signal_study" / time.strftime("%Y%m%dT%H%M%S")
    out.mkdir(parents=True, exist_ok=True)
    df.to_csv(out / "tests.csv", index=False)
    txt = summary_text(df, g, info, controls, pd.concat(cross) if cross else None, time.time() - t0)
    (out / "summary.txt").write_text(txt)
    meta = {"through": through.isoformat(), "git_commit": common.git_commit(), "git_dirty": common.git_dirty(),
            "prereg_sha256": common.sha256_file(PREREG), "gate": {k: v for k, v in g.items() if k != "candidates"},
            "candidates": g["candidates"], **info}
    (out / "meta.json").write_text(json.dumps(meta, indent=1, default=str))
    print(txt)
    print(f"written to {out}")


def cmd_crosscheck(a):
    through = common.allowed_through(a.through, False)
    for pair in common.PAIRS:
        b, _ = load_bars(pair, through)
        for X in common.DECISION_TFS:
            f = load_features(pair, X)
            if f is None:
                print(pair, X, "no feature table")
                continue
            try:
                print(pair, X)
                print(crosscheck(b, f, X, pair, through, quality.load(pair)).to_string(index=False))
            except CrossCheckError as e:
                print(e)


# ------------------------------------------------------------------------------------------------ synthetic

def synth_periods(start, end):
    """discovery = the first 4/7 of the span, confirmation = the rest (the 2008-2011 / 2012-2014 split)."""
    a, b = to_daynum(start), to_daynum(end)
    cut = a + int((b - a) * 4 / 7)
    iso = lambda d: str(np.datetime64(int(d), "D"))
    return {"discovery": (iso(a), iso(cut)), "confirmation": (iso(cut + 1), iso(b))}


def synth_study(pairs, start, end, seed, warmup_start=None, **gen):
    import synthetic
    bars = synthetic.generate(pairs, warmup_start or start, end, seed=seed, **gen)
    per = synth_periods(start, end)
    df, info = run_study(bars, through=end if end <= "2017-12-29" else None, periods=per)
    return bars, df, info, gate(df)


def cmd_synth(a):
    t0 = time.time()
    gen = {}
    if a.effect:
        gen.update(effect=a.effect, strength_bp=a.strength)
    if a.drift:
        gen["drift_bp_day"] = a.drift
    bars, df, info, g = synth_study(a.pairs.split(","), a.start, a.end, a.seed, a.warmup, **gen)
    controls = lookahead_canary(bars, a.end, synth_periods(a.start, a.end))
    txt = summary_text(df, g, info, controls, None, time.time() - t0)
    print(txt)
    if a.csv:
        df.to_csv(a.csv, index=False)


def cmd_validate(a):
    """Null runs (random walks) and planted-effect runs; prints the false-positive rates and recovery."""
    pairs = a.pairs.split(",")
    lines = []
    tvals, dvals, decisions = [], [], []
    t0 = time.time()
    for s in range(a.seeds):
        _, df, info, g = synth_study(pairs, a.start, a.end, a.seed + 1000 + s, a.warmup)
        f = df[(df["pair"] == "ALL") & (df["period"] == "full")]
        tvals.append(f["t_std"].dropna().to_numpy())
        dvals.append(f["t_diff"].dropna().to_numpy())
        decisions.append((g["decision"], g["n_candidates"], g["n_confirmed"]))
        print(f"null seed {s}: decision {g['decision']} candidates {g['n_candidates']} confirmed {g['n_confirmed']} "
              f"({time.time() - t0:.0f} s)", flush=True)
    if a.seeds:
        t, d = np.concatenate(tvals), np.concatenate(dvals)
        lines.append(f"NULL ({a.seeds} seeds x {len(tvals[0])} pooled tests, {a.start}..{a.end}, pairs {pairs}):")
        lines.append(f"  t_std:  mean {t.mean():+.3f} sd {t.std():.3f}  |t|>=2 {np.mean(np.abs(t) >= 2):.2%} "
                     f"(normal 4.55%)  |t|>=3 {np.mean(np.abs(t) >= 3):.2%} (0.27%)")
        lines.append(f"  t_diff: mean {d.mean():+.3f} sd {d.std():.3f}  |t|>=2 {np.mean(np.abs(d) >= 2):.2%}  "
                     f"|t|>=3 {np.mean(np.abs(d) >= 3):.2%}")
        lines.append(f"  gate decisions: {pd.Series([x[0] for x in decisions]).value_counts().to_dict()}; "
                     f"mean candidates {np.mean([x[1] for x in decisions]):.2f}; runs with any confirmed "
                     f"{np.mean([x[2] > 0 for x in decisions]):.0%}")
    for strength in [float(x) for x in a.strengths.split(",")] if a.strengths else []:
        hits, confirmed = [], []
        for s in range(a.power_seeds):
            _, df, info, g = synth_study(pairs, a.start, a.end, a.seed + 2000 + s, a.warmup, effect=a.effect,
                                         strength_bp=strength)
            sig = "pdh_break" if a.effect == "pdh_break" else "round_big_touch"
            r = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["tf"] == "M15") & (df["signal"] == sig) &
                   (df["horizon"] == 4)].iloc[0]
            hits.append((r["mean_std"], r["t_std"], r["t_diff"]))
            confirmed.append(any(c["confirmed"] and c["signal"] == sig for c in g["candidates"]))
            print(f"  strength {strength} seed {s}: M15 h4 mean {r['mean_std']:+.3f} t {r['t_std']:+.2f} "
                  f"t_diff {r['t_diff']:+.2f} confirmed {confirmed[-1]} decision {g['decision']}", flush=True)
        h = np.array(hits)
        lines.append(f"PLANTED {a.effect} {strength} bp over 4 M15 bars ({a.power_seeds} seeds): M15 h4 mean "
                     f"{h[:, 0].mean():+.3f} bp (sd over seeds {h[:, 0].std():.3f}), t {h[:, 1].mean():+.2f}, t_diff "
                     f"{h[:, 2].mean():+.2f}; |t|>=3 in {np.mean(np.abs(h[:, 1]) >= 3):.0%}; confirmed by the gate in "
                     f"{np.mean(confirmed):.0%}")
        print(lines[-1], flush=True)
    lines.append(f"runtime {time.time() - t0:.0f} s")
    print("\n".join(lines))


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("run")
    common.add_guard_args(r)
    r.add_argument("--out")
    c = sub.add_parser("crosscheck")
    common.add_guard_args(c)
    for name in ("synth", "validate"):
        s = sub.add_parser(name)
        s.add_argument("--pairs", default="EURUSD,GBPUSD,USDJPY,USDCHF,USDCAD,NZDUSD")
        s.add_argument("--start", default="2008-01-01")
        s.add_argument("--end", default="2014-12-31")
        s.add_argument("--warmup", default="2007-01-01")
        s.add_argument("--seed", type=int, default=0)
        s.add_argument("--effect", default=None if name == "synth" else "pdh_break")
        s.add_argument("--strength", type=float, default=0.0)
        s.add_argument("--drift", type=float, default=0.0)
        s.add_argument("--csv")
        s.add_argument("--seeds", type=int, default=10)
        s.add_argument("--power-seeds", type=int, default=5)
        s.add_argument("--strengths", default="")
    a = ap.parse_args(argv)
    {"run": cmd_run, "crosscheck": cmd_crosscheck, "synth": cmd_synth, "validate": cmd_validate}[a.cmd](a)


if __name__ == "__main__":
    main()
