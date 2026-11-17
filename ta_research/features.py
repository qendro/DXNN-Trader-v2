"""Feature tables for Stage 1 (SPEC.md section 6).

python3 ta_research/features.py [--pairs EURUSD,...] [--tfs M15,H1,H4] [--through YYYY-MM-DD]

`build_features(bars, decision_tf, pair)` is pure: it takes the bar arrays (as loaded from the bar npz files) and returns
one array per column, one row per decision bar. Every indicator is computed once on its chart series and gathered to the
decision rows through the index of the last completed chart bar (end <= available), so row t depends only on bars with
end <= end[t]. Mid prices are snapped to the price grid first and every price comparison is made on the grid (SPEC
section 6b). `quality_excluded` comes from the stale-quote intervals in derived/ta_stage1/quality/<PAIR>.json (section
5a), which the CLI loads and passes in. Output: common.FEATURES_DIR/<PAIR>/<X>.npz and meta.json.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import platform
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
from numpy.lib.stride_tricks import sliding_window_view

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402
import quality  # noqa: E402

CHARTS = {"M15": ("M15", "H1", "H4", "D1"), "H1": ("H1", "H4", "D1"), "H4": ("H4", "D1")}
LAGS = {"M15": 32, "H1": 24, "H4": 30, "D1": 20}
MA_LENS = (20, 50, 100, 200)
STUB_MINUTES = 360           # D1 bars with fewer minutes are holiday stubs
AGE_CAP = 100
CONTEXT = ("mid_close", "bid_close", "ask_close", "spread_mean", "n_minutes", "trading_day")
NY_SHIFT = 7 * 3600          # 17:00 NY + 7 h = 00:00 of the trading-day label date


MID_FIELDS = ("mid_open", "mid_high", "mid_low", "mid_close")
FIGURE_TICKS = 10000         # a big figure is 10,000 grid steps for every pair (0.01 / 1e-6 and 1.00 / 1e-4)


def big_figure(pair):
    return 1.0 if pair in common.JPY_PAIRS else 0.01


def grid(pair):
    """Price grid of mid prices (half-sums of 5- or 3-decimal quotes), SPEC 6b rule 1."""
    return 1e-4 if pair in common.JPY_PAIRS else 1e-6


def ticks(x, g):
    """Price in grid steps (float, NaN-safe), rounded half-up (SPEC 6b rule 1; np.rint would round half-to-even and
    put an average exactly half-way between grid points on the wrong side). All price comparisons use these."""
    return np.floor(np.asarray(x, dtype=np.float64) / g + 0.5)


def snap(x, g):
    """Mid prices put exactly on the grid, so equal grid prices are equal floats."""
    return ticks(x, g) * g


# ---------------------------------------------------------------- indicator primitives
# All are causal: value i depends only on inputs[:i + 1]. float64 throughout.

def rolling(x, n, fn):
    """fn over each window x[i - n + 1 : i + 1]; NaN for i < n - 1 (and wherever the window holds a NaN)."""
    out = np.full(len(x), np.nan)
    if len(x) >= n:
        out[n - 1:] = fn(sliding_window_view(x, n), axis=1)
    return out


def smooth(x, n, alpha):
    """y_i = y_{i-1} + alpha * (x_i - y_{i-1}), seeded at the n-th valid value with the mean of the first n valid
    values. NaN before the seed. x may be NaN only on a leading block."""
    out = np.full(len(x), np.nan)
    ok = np.flatnonzero(~np.isnan(x))
    if len(ok) == 0 or len(x) - ok[0] < n:
        return out
    f = int(ok[0])
    if np.isnan(x[f:]).any():
        raise ValueError("NaN after the first valid value")
    y = float(np.mean(x[f:f + n]))
    res = [y]
    for v in x[f + n:].tolist():
        y = y + alpha * (v - y)
        res.append(y)
    out[f + n - 1:] = res
    return out


def ema(x, n):
    return smooth(x, n, 2.0 / (n + 1))


def wilder(x, n):
    return smooth(x, n, 1.0 / n)


def atr(h, l, c, n=14):
    """Wilder ATR. TR needs the previous close, so TR[0] is NaN and ATR starts at index n (mean of TR[1..n])."""
    pc = np.r_[np.nan, c[:-1]]
    tr = np.maximum(h - l, np.maximum(np.abs(h - pc), np.abs(l - pc)))
    return wilder(tr, n)


def rsi(c, n=14):
    """Wilder RSI in 0..100; starts at index n. Average loss 0 gives 100 (50 if the average gain is 0 too)."""
    d = np.r_[np.nan, np.diff(c)]
    gain = np.where(d > 0, d, 0.0)
    loss = np.where(d < 0, -d, 0.0)
    gain[0] = loss[0] = np.nan
    ag, al = wilder(gain, n), wilder(loss, n)
    with np.errstate(divide="ignore", invalid="ignore"):
        r = 100.0 - 100.0 / (1.0 + ag / al)
    r = np.where(al == 0, np.where(ag == 0, 50.0, 100.0), r)
    return r


def stochastic(h, l, c, n=14, k=3, d=3):
    """Slow stochastic: raw %K over n bars (current bar included), slow %K = SMA_k(raw), %D = SMA_d(slow %K).
    A flat window (highest high == lowest low) gives raw %K = 50."""
    hh, ll = rolling(h, n, np.max), rolling(l, n, np.min)
    rng = hh - ll                                  # h, l, c are on the grid, so a flat window gives exactly 0
    with np.errstate(divide="ignore", invalid="ignore"):
        raw = np.where(rng > 0, 100.0 * (c - ll) / rng, 50.0)
    raw[np.isnan(rng)] = np.nan
    slow_k = rolling(raw, k, np.mean)
    return slow_k, rolling(slow_k, d, np.mean)


def cross_age_and_events(c, ma, g, cap=AGE_CAP):
    """Side of the close vs the average on the grid (+1 if close >= average rounded to the grid, else -1). Age = side * number of consecutive bars on
    this side including the current one (1 on the crossing bar), capped at `cap`. Before the first observed cross the
    run length is only a lower bound, so the age is NaN unless it has already reached the cap. Cross event = new side
    on the bar where the side changed (both bars with a valid average), else 0."""
    n = len(c)
    age = np.full(n, np.nan)
    ev = np.zeros(n, np.int8)
    valid = np.flatnonzero(~np.isnan(ma))
    if len(valid) == 0:
        return age, ev
    f = int(valid[0])
    side = np.where(ticks(c[f:], g) >= ticks(ma[f:], g), 1, -1)
    change = np.r_[False, side[1:] != side[:-1]]
    pos = np.arange(len(side))
    run_start = np.maximum.accumulate(np.where(change, pos, 0))
    run = pos - run_start + 1
    a = (np.minimum(run, cap) * side).astype(np.float64)
    a[(run_start == 0) & (run < cap)] = np.nan
    age[f:] = a
    ev[f:][change] = side[change]
    return age, ev


# ---------------------------------------------------------------- per-chart features

def chart_series(b, g):
    """Indicators on one chart series (one value per chart bar). Mid prices must already be on the grid `g`."""
    h, l, c = b["mid_high"], b["mid_low"], b["mid_close"]
    s = {"close": c, "atr": atr(h, l, c)}
    for n in MA_LENS:
        s[f"sma{n}"] = rolling(c, n, np.mean)
        s[f"ema{n}"] = ema(c, n)
    s["age50"], s["ev50"] = cross_age_and_events(c, s["sma50"], g)
    s["age200"], s["ev200"] = cross_age_and_events(c, s["sma200"], g)
    s["rsi"] = rsi(c)
    macd = ema(c, 12) - ema(c, 26)
    sig = ema(macd, 9)
    s["macd"], s["macd_signal"], s["macd_hist"] = macd, sig, macd - sig
    s["stoch_k"], s["stoch_d"] = stochastic(h, l, c)
    s["sd20"] = rolling(c, 20, np.std)            # population stdev (ddof=0)
    s["flat20"] = rolling(c, 20, np.max) == rolling(c, 20, np.min)   # 20 equal closes: stdev is 0
    s["atr_mean100"] = rolling(s["atr"], 100, np.mean)
    s["don_hi"] = rolling(h, 20, np.max)
    s["don_lo"] = rolling(l, 20, np.min)
    return s


def last_completed(ends, t):
    """Index of the last bar with end <= t (-1 if none). `ends` must be increasing."""
    return np.searchsorted(ends, t, side="right") - 1


def take(arr, idx):
    """arr[idx] as float64 with NaN where idx < 0."""
    out = np.asarray(arr, dtype=np.float64)[np.maximum(idx, 0)]
    out[idx < 0] = np.nan
    return out


def newly_known(idx, ev):
    """On each decision row, the latest nonzero chart event among the chart bars that became known at that row
    (idx[t-1] < j <= idx[t]), else 0. Usually one bar becomes known per row; after a data gap a partial chart bar and
    the next one can become known together."""
    prev = np.r_[-1, idx[:-1]]
    pos = np.arange(len(ev))
    last_ev = np.maximum.accumulate(np.where(ev != 0, pos, -1))   # index of latest event at or before j
    out = np.zeros(len(idx), np.int8)
    ok = idx >= 0
    j = np.where(ok, last_ev[np.maximum(idx, 0)], -1)
    new = ok & (j > prev)
    out[new] = ev[j[new]]
    return out


def chart_columns(cb, tf, price, avail, g):
    """Columns with prefix `tf_` for chart bars `cb`, gathered at the decision rows (price, available)."""
    s = chart_series(cb, g)
    i = last_completed(cb["end"], avail)
    i_lag = np.searchsorted(cb["end"], avail, side="left") - 1    # last bar with end < available (strict)
    at = lambda k: take(s[k], i)  # noqa: E731
    a = at("atr")
    p = tf + "_"
    out = {}
    with np.errstate(divide="ignore", invalid="ignore"):
        for n in MA_LENS:
            out[f"{p}sma{n}_dist"] = (price - at(f"sma{n}")) / a
        for n in MA_LENS:
            out[f"{p}ema{n}_dist"] = (price - at(f"ema{n}")) / a
        out[p + "sma50_200_dist"] = (at("sma50") - at("sma200")) / a
        out[p + "cross_sma50_age"] = at("age50")
        out[p + "cross_sma200_age"] = at("age200")
        out[p + "rsi14"] = (at("rsi") - 50.0) / 50.0
        out[p + "macd"] = at("macd") / a
        out[p + "macd_signal"] = at("macd_signal") / a
        out[p + "macd_hist"] = at("macd_hist") / a
        out[p + "stoch_k"] = (at("stoch_k") - 50.0) / 50.0
        out[p + "stoch_d"] = (at("stoch_d") - 50.0) / 50.0
        sd = at("sd20")
        flat = take(s["flat20"], i) == 1
        out[p + "bb_pos"] = np.where(flat, np.nan, (price - at("sma20")) / (2.0 * sd))
        out[p + "bb_width"] = 4.0 * sd / a
        out[p + "atr_bp"] = a / price * 1e4
        out[p + "atr_regime"] = a / at("atr_mean100") - 1.0
        out[p + "donchian20_hi_dist"] = (price - at("don_hi")) / a
        out[p + "donchian20_lo_dist"] = (price - at("don_lo")) / a
        for k in range(1, LAGS[tf] + 1):
            out[f"{p}close_lag{k}"] = (price - take(s["close"], i_lag - (k - 1))) / a
    for k in out:
        out[k] = out[k].astype(np.float32)
    out[p + "ev_sma50_cross"] = newly_known(i, s["ev50"])
    out[p + "ev_sma200_cross"] = newly_known(i, s["ev200"])
    return out


# ---------------------------------------------------------------- levels, events, clock

def ny_shifted(ts):
    """Seconds such that // 86400 is the trading-day label (days since epoch) and % 86400 is time since 17:00 NY."""
    ts = np.asarray(ts, dtype=np.int64)
    utc = pd.DatetimeIndex(ts.astype("datetime64[s]")).tz_localize("UTC")
    local = utc.tz_convert(common.NY).tz_localize(None).to_numpy().astype("datetime64[s]").astype(np.int64)
    return local + NY_SHIFT


def clock_columns(avail):
    sh = ny_shifted(avail)
    sec = sh % 86400
    weekday = (sh // 86400 + 3) % 7                  # Monday = 0 for the trading-day label
    day = 2 * np.pi * sec / 86400.0
    week = 2 * np.pi * (weekday * 86400 + sec) / (5 * 86400.0)
    cols = {"clk_day_sin": np.sin(day), "clk_day_cos": np.cos(day),
            "clk_day2_sin": np.sin(2 * day), "clk_day2_cos": np.cos(2 * day),
            "clk_week_sin": np.sin(week), "clk_week_cos": np.cos(week)}
    return {k: v.astype(np.float32) for k, v in cols.items()}


def cross_dir(prev, cur, level):
    """+1 if prev < level <= cur, -1 if prev >= level > cur, else 0 (0 where anything is NaN). Grid ticks in."""
    up = (prev < level) & (cur >= level)
    down = (prev >= level) & (cur < level)
    return (up.astype(np.int8) - down.astype(np.int8)).astype(np.int8)


def level_columns(x, d1, d1_all, w1, pair, d1_atr_rows):
    """Previous day / week levels, pivots and round numbers (D1 ATR units), plus the level events.
    Mid prices in x, d1, w1 are on the grid; every comparison below is made in grid ticks (SPEC 6b rule 1)."""
    g = grid(pair)
    price, avail = x["mid_close"], x["end"]
    out = {}
    i = last_completed(d1["end"], avail)
    H, L, C = take(d1["mid_high"], i), take(d1["mid_low"], i), take(d1["mid_close"], i)
    iw = last_completed(w1["end"], avail)
    P = (H + L + C) / 3.0
    levels = {"pdh": H, "pdl": L, "pdc": C,
              "pwh": take(w1["mid_high"], iw), "pwl": take(w1["mid_low"], iw), "pwc": take(w1["mid_close"], iw)}
    a = d1_atr_rows
    B = big_figure(pair)
    fig = np.floor(ticks(price, g) / FIGURE_TICKS)          # big figure at or below the price
    up_half = ticks(price, g) - fig * FIGURE_TICKS >= FIGURE_TICKS // 2
    with np.errstate(divide="ignore", invalid="ignore"):
        for k, v in levels.items():
            out[f"{k}_dist"] = (price - v) / a
        for k, v in {"p": P, "r1": 2 * P - L, "s1": 2 * P - H, "r2": P + (H - L), "s2": P - (H - L)}.items():
            out[f"pivot_{k}"] = (price - v) / a
        out["round_big_dist"] = (price - (fig + up_half) * B) / a     # tie at .50 -> upper figure
        out["round_half_dist"] = (price - (fig + 0.5) * B) / a        # on a big figure -> upper half (k + 0.5)
    out = {k: v.astype(np.float32) for k, v in out.items()}

    # prev_day_stub: most recent completed trading day (stubs included) was a stub
    j = last_completed(d1_all["end"], avail)
    stub = d1_all["n_minutes"] < STUB_MINUTES
    out["prev_day_stub"] = np.where(j >= 0, stub[np.maximum(j, 0)], False).astype(np.int8)

    # Events: levels in force during bar t, i.e. the last non-stub day completed by the bar's start
    c, hi, lo = ticks(x["mid_close"], g), ticks(x["mid_high"], g), ticks(x["mid_low"], g)
    cp = np.r_[np.nan, c[:-1]]
    js = last_completed(d1["end"], x["start"])
    ph, pl = ticks(take(d1["mid_high"], js), g), ticks(take(d1["mid_low"], js), g)
    out["ev_pdh_break"] = cross_dir(cp, c, ph)
    out["ev_pdl_break"] = cross_dir(cp, c, pl)
    out["ev_pdh_touch"] = ((cp < ph) & (hi >= ph) & (c < ph)).astype(np.int8)
    out["ev_pdl_touch"] = ((cp >= pl) & (lo < pl) & (c >= pl)).astype(np.int8)
    hh_prev = np.r_[np.nan, rolling(hi, 20, np.max)[:-1]]
    ll_prev = np.r_[np.nan, rolling(lo, 20, np.min)[:-1]]
    out["ev_donchian20_break"] = ((c >= hh_prev).astype(np.int8) - (c < ll_prev).astype(np.int8)).astype(np.int8)
    lvl = np.floor(c / FIGURE_TICKS)                        # a close on a figure counts as above it
    lvl_prev = np.r_[np.nan, lvl[:-1]]
    out["ev_round_big_cross"] = ((lvl > lvl_prev).astype(np.int8) - (lvl < lvl_prev).astype(np.int8)).astype(np.int8)
    return out


# ---------------------------------------------------------------- API

def build_features(bars, decision_tf, pair, quality_intervals=None):
    """Feature columns for one pair and decision timeframe. `bars` maps a timeframe name to its field arrays
    (SPEC section 5). Pure: no file access. Returns {column: array}, one row per decision bar.

    `quality_intervals` ([[start, end, ...], ...] from derived/ta_stage1/quality/<PAIR>.json) sets
    `quality_excluded`; it is data-quality metadata found with hindsight, not a feature, so it is passed in. Without it
    the column is all 0."""
    if decision_tf not in CHARTS:
        raise ValueError(decision_tf)
    g = grid(pair)
    raw = bars[decision_tf]
    # computation copy with mid prices exactly on the grid (SPEC 6b rule 1); context columns keep the raw values
    bars = {tf: {k: (snap(v, g) if k in MID_FIELDS else np.asarray(v)) for k, v in b.items()} for tf, b in bars.items()}
    x = bars[decision_tf]
    avail = np.asarray(x["end"], dtype=np.int64)
    if len(avail) > 1 and np.any(np.diff(avail) <= 0):
        raise ValueError("decision bar ends not strictly increasing")
    price = x["mid_close"]
    d1_all = bars["D1"]
    keep = d1_all["n_minutes"] >= STUB_MINUTES
    d1 = {k: v[keep] for k, v in d1_all.items()}

    cols = {"available": avail.copy()}
    for k in CONTEXT:
        cols[k] = np.asarray(raw[k]).copy()
    cols["quality_excluded"] = quality.excluded_mask(avail, quality_intervals or []).astype(np.int8)
    cols.update(clock_columns(avail))
    d1_atr_rows = take(atr(d1["mid_high"], d1["mid_low"], d1["mid_close"]), last_completed(d1["end"], avail))
    cols.update(level_columns(x, d1, d1_all, bars["W1"], pair, d1_atr_rows))
    for tf in CHARTS[decision_tf]:
        cols.update(chart_columns(d1 if tf == "D1" else bars[tf], tf, price, avail, g))
    return cols


def column_definitions(decision_tf, pair):
    """One-line definition per column, in output order (matches build_features)."""
    B = big_figure(pair)
    d = {"available": "UTC seconds when the row is known = end of the decision bar (int64)"}
    for k in CONTEXT:
        d[k] = f"decision bar {k}, copied unchanged"
    d["quality_excluded"] = "1 if available lies in (start, end] of a stale-quote interval in quality/<PAIR>.json (SPEC 5a)"
    for k, desc in [("clk_day", "1 cycle per day"), ("clk_day2", "2 cycles per day")]:
        d[k + "_sin"] = f"sin, {desc}; NY time at available, day starts 17:00 NY"
        d[k + "_cos"] = f"cos, {desc}; NY time at available, day starts 17:00 NY"
    d["clk_week_sin"] = "sin of trading-week fraction at available (Sun 17:00 NY = 0, Fri 17:00 NY = 1)"
    d["clk_week_cos"] = "cos of trading-week fraction at available (Sun 17:00 NY = 0, Fri 17:00 NY = 1)"
    lv = "D1 ATR(14) units; previous = last completed non-stub D1 bar with end <= available"
    d["pdh_dist"] = f"(price - previous day mid high) / D1 ATR; {lv}"
    d["pdl_dist"] = f"(price - previous day mid low) / D1 ATR; {lv}"
    d["pdc_dist"] = f"(price - previous day mid close) / D1 ATR; {lv}"
    for k, w in (("pwh", "high"), ("pwl", "low"), ("pwc", "close")):
        d[f"{k}_dist"] = f"(price - last completed W1 bar mid {w}) / D1 ATR (W1 end <= available, stubs included)"
    for k, f in (("p", "(H+L+C)/3"), ("r1", "2P-L"), ("s1", "2P-H"), ("r2", "P+(H-L)"), ("s2", "P-(H-L)")):
        d[f"pivot_{k}"] = f"(price - pivot {f}) / D1 ATR, H/L/C of previous non-stub day"
    d["round_big_dist"] = f"(price - nearest multiple of {B:g}, upper one on a tie) / D1 ATR"
    d["round_half_dist"] = (f"(price - nearest half figure (k + 0.5) * {B:g}, big figures excluded, upper one on a "
                            f"tie) / D1 ATR")
    d["prev_day_stub"] = f"1 if the last completed D1 bar (end <= available) had < {STUB_MINUTES} minutes"
    ev = "level = last non-stub D1 bar completed by the decision bar's start; prices compared on the grid"
    d["ev_pdh_break"] = f"+1 close[t-1] < PDH <= close[t], -1 close[t-1] >= PDH > close[t]; {ev}"
    d["ev_pdl_break"] = f"+1 close[t-1] < PDL <= close[t], -1 close[t-1] >= PDL > close[t]; {ev}"
    d["ev_pdh_touch"] = f"1 if close[t-1] < PDH, high[t] >= PDH, close[t] < PDH; {ev}"
    d["ev_pdl_touch"] = f"1 if close[t-1] >= PDL, low[t] < PDL, close[t] >= PDL; {ev}"
    d["ev_donchian20_break"] = ("+1 close[t] >= max high of bars t-20..t-1, -1 close[t] < min low of them "
                                "(on the grid)")
    d["ev_round_big_cross"] = (f"+1/-1 if floor(close / {B:g}) rose/fell from bar t-1 to t (on the grid; a close on a "
                               f"figure counts as above)")
    for tf in CHARTS[decision_tf]:
        p = tf + "_"
        u = f"{tf} ATR(14) units; {tf} values from the last completed {tf} bar" + (" (stubs excluded)" if tf == "D1" else "")
        for n in MA_LENS:
            d[f"{p}sma{n}_dist"] = f"(price - SMA{n} of {tf} closes) / ATR; {u}"
        for n in MA_LENS:
            d[f"{p}ema{n}_dist"] = f"(price - EMA{n} of {tf} closes, alpha 2/(n+1), SMA seed) / ATR; {u}"
        d[p + "sma50_200_dist"] = f"(SMA50 - SMA200) / ATR; {u}"
        for n in (50, 200):
            d[f"{p}cross_sma{n}_age"] = (f"side (+1 close >= SMA{n} on the grid, else -1) x consecutive {tf} bars on that side incl. "
                                         f"current (1 on the crossing bar), capped at 100; NaN before first cross "
                                         f"unless already 100")
        d[p + "rsi14"] = f"(Wilder RSI14 - 50) / 50; {u}"
        d[p + "macd"] = f"(EMA12 - EMA26) / ATR; {u}"
        d[p + "macd_signal"] = f"EMA9 of MACD line (SMA seed) / ATR; {u}"
        d[p + "macd_hist"] = f"(MACD - signal) / ATR; {u}"
        d[p + "stoch_k"] = f"(slow %K - 50) / 50; raw %K(14) smoothed by SMA3; flat range -> raw 50; {u}"
        d[p + "stoch_d"] = f"(%D - 50) / 50; %D = SMA3 of slow %K; {u}"
        d[p + "bb_pos"] = f"(price - SMA20) / (2 x population stdev20 of {tf} closes); NaN if the 20 closes are equal"
        d[p + "bb_width"] = f"4 x population stdev20 / ATR; {u}"
        d[p + "atr_bp"] = f"ATR / price x 10000; {u}"
        d[p + "atr_regime"] = f"ATR / mean of ATR over the last 100 {tf} bars (incl. current) - 1"
        d[p + "donchian20_hi_dist"] = f"(price - highest {tf} high of the last 20 completed bars) / ATR"
        d[p + "donchian20_lo_dist"] = f"(price - lowest {tf} low of the last 20 completed bars) / ATR"
        for k in range(1, LAGS[tf] + 1):
            d[f"{p}close_lag{k}"] = f"(price - close of the {k}-th last {tf} bar with end < available) / ATR"
        for n in (50, 200):
            d[f"{p}ev_sma{n}_cross"] = (f"+1/-1 when a {tf} close crossed SMA{n} up/down in a {tf} bar that became "
                                        f"known at this row (first row only; latest cross if several), else 0")
    return d


CONVENTIONS = {
    "spec": "SPEC.md section 6b (settled 2026-11-16) fixes all of these",
    "price": "decision bar mid_close",
    "grid": "mid prices snapped to 1e-6 (non-JPY) / 1e-4 (USDJPY) before computing; every price comparison is on the "
            "grid (averages rounded to it); equal counts as above: cross up prev < L <= cur, down prev >= L > cur",
    "atr": "Wilder ATR(14) on mid prices; TR[0] undefined (needs previous close); seeded at bar 14 with mean TR[1..14]",
    "rsi": "Wilder RSI(14) on mid closes; seeded at bar 14 with mean of first 14 gains/losses; avg loss 0 -> 100 "
           "(both 0 -> 50)",
    "ema": "alpha 2/(n+1), seeded at the n-th value with the SMA of the first n values; MACD signal seeded at the "
           "9th valid MACD value",
    "stochastic": "raw %K over 14 bars incl. current with highs/lows; flat window -> 50; slow %K = SMA3; %D = SMA3",
    "bollinger": "population stdev (ddof=0) of 20 closes; bb_pos NaN when the 20 closes are equal (stdev 0)",
    "cross_age": "consecutive bars on the current side incl. current (1 on the crossing bar); a close equal to the "
                 "average counts as above",
    "chart_cross_events": "flag on the first decision row where the crossing chart bar is known; if a data gap makes "
                          "two chart bars known on one row, the latest cross among them is flagged",
    "close_lag": "k-th last chart bar with end < available (strict); on the decision chart lag 1 is bar t-1",
    "levels_dist": "D1/W1 bars with end <= available (at the 17:00 NY row the day just ended is 'previous')",
    "level_events": "level in force during bar t: last non-stub D1 bar with end <= start of bar t",
    "round_half": "half figures are the levels (k + 0.5) x big figure only; nearest level on a tie is the upper one",
    "breaks_touches": "break +1 prev < L <= close, -1 prev >= L > close; PDH touch prev < L, high >= L, close < L; "
                      "PDL touch prev >= L, low < L, close >= L",
    "quality_excluded": "from quality/<PAIR>.json (SPEC 5a), passed to build_features; hindsight data-quality flag, "
                        "not a feature",
    "stubs": f"D1 bars with n_minutes < {STUB_MINUTES} are dropped from the D1 chart, D1 ATR, levels and pivots",
    "dtypes": "features float32, events/prev_day_stub int8, available int64; context columns keep the bar dtypes",
}


# ---------------------------------------------------------------- CLI

def load_bars(pair, limit):
    """Bar arrays for one pair, checked against the bars meta.json SHA-256s, cut to end <= limit (date guard)."""
    d = common.BARS_DIR / pair
    meta = json.loads((d / "meta.json").read_text())
    bars, shas = {}, {}
    for tf in common.TIMEFRAMES:
        p = d / f"{tf}.npz"
        sha = common.sha256_file(p)
        if sha != meta["bar_files"][f"{tf}.npz"]:
            raise RuntimeError(f"{p}: SHA-256 differs from bars meta.json")
        shas[f"{tf}.npz"] = sha
        with np.load(p) as z:
            arrs = {k: z[k] for k in z.files}
        keep = arrs["end"] <= limit
        bars[tf] = {k: v[keep] for k, v in arrs.items()}
    return bars, shas, meta


def load_quality(pair, h1_sha):
    """Stale-quote intervals (SPEC 5a) and the quality file's SHA-256; the file must be built from these H1 bars."""
    path = quality.QUALITY_DIR / f"{pair}.json"
    q = json.loads(path.read_text())
    if q["bars_sha256"] != h1_sha:
        raise RuntimeError(f"{path}: built from different H1 bars; rerun ta_research/quality.py")
    return q["intervals"], common.sha256_file(path), q["rule"]


def save_npz(path, arrays):
    tmp = path.with_name(path.name + ".tmp")
    with open(tmp, "wb") as f:
        np.savez(f, **arrays)
    os.replace(tmp, path)


def iso(ts):
    return dt.datetime.fromtimestamp(int(ts), common.UTC).isoformat()


def build_pair(pair, tfs, through):
    limit = common.guard_limit_utc_seconds(through)
    bars, shas, bars_meta = load_bars(pair, limit)
    bars_through = dt.date.fromisoformat(bars_meta["date_range"]["through"])
    out_dir = common.FEATURES_DIR / pair
    out_dir.mkdir(parents=True, exist_ok=True)
    meta_path = out_dir / "meta.json"
    meta = json.loads(meta_path.read_text()) if meta_path.exists() else {}
    meta.setdefault("tables", {})
    intervals, q_sha, q_rule = load_quality(pair, shas["H1.npz"])
    report = []
    for tf in tfs:
        t0 = time.time()
        cols = build_features(bars, tf, pair, quality_intervals=intervals)
        defs = column_definitions(tf, pair)
        if list(defs) != list(cols):
            raise AssertionError(f"{pair} {tf}: definitions and columns differ")
        p = out_dir / f"{tf}.npz"
        save_npz(p, cols)
        secs = time.time() - t0
        meta["tables"][tf] = {
            "file": f"{tf}.npz", "sha256": common.sha256_file(p), "rows": int(len(cols["available"])),
            "first_available_utc": iso(cols["available"][0]), "last_available_utc": iso(cols["available"][-1]),
            "charts": list(CHARTS[tf]),
            "bar_files_sha256": {f"{t}.npz": shas[f"{t}.npz"] for t in sorted(set(CHARTS[tf]) | {tf, "D1", "W1"})},
            "columns": {k: {"dtype": str(cols[k].dtype), "definition": v} for k, v in defs.items()},
            "build_time_utc": dt.datetime.now(common.UTC).isoformat(timespec="seconds"),
            "build_seconds": round(secs, 2),
            "quality_excluded_rows": int(cols["quality_excluded"].sum()),
        }
        report.append((tf, len(cols["available"]), len(cols), p.stat().st_size, secs))
    meta.update({
        "pair": pair,
        "date_range": {"through_requested": through.isoformat(), "bars_through": bars_through.isoformat(),
                       "guard_limit_utc": iso(limit), "guard_limit_utc_seconds": limit},
        "bars_dir": str(common.BARS_DIR / pair),
        "bars_git_commit": bars_meta["builder"]["git_commit"],
        "quality": {"file": str(quality.QUALITY_DIR / f"{pair}.json"), "sha256": q_sha, "rule": q_rule,
                    "intervals": intervals},
        "conventions": CONVENTIONS,
        "builder": {"script": "ta_research/features.py", "git_commit": common.git_commit(),
                    "git_dirty_ta_research": common.git_dirty(), "python": platform.python_version(),
                    "numpy": np.__version__, "pandas": pd.__version__},
        "build_time_utc": dt.datetime.now(common.UTC).isoformat(timespec="seconds"),
    })
    tmp = out_dir / "meta.json.tmp"
    tmp.write_text(json.dumps(meta, indent=1) + "\n")
    os.replace(tmp, meta_path)
    return report


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--pairs", default=",".join(common.PAIRS), help="comma-separated subset of " + ",".join(common.PAIRS))
    ap.add_argument("--tfs", default=",".join(common.DECISION_TFS), help="decision timeframes, subset of M15,H1,H4")
    common.add_guard_args(ap)
    args = ap.parse_args(argv)
    through = common.allowed_through(args.through, args.unseal_holdout)   # raises DateGuardError
    pairs = [p.strip().upper() for p in args.pairs.split(",") if p.strip()]
    tfs = [t.strip().upper() for t in args.tfs.split(",") if t.strip()]
    if [p for p in pairs if p not in common.PAIRS] or [t for t in tfs if t not in CHARTS]:
        ap.error("unknown pair or timeframe")
    t0 = time.time()
    for pair in pairs:
        for tf, rows, ncol, size, secs in build_pair(pair, tfs, through):
            print(f"{pair} {tf}: {rows:,} rows x {ncol} cols, {size / 2**20:.0f} MB, {secs:.1f}s", flush=True)
    print(f"done in {time.time() - t0:.1f}s -> {common.FEATURES_DIR}")


if __name__ == "__main__":
    main()
