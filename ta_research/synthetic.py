"""Synthetic bars and feature tables in the SPEC section 5 / 6 format, for testing the Stage 1 study tools.

Nothing here reads market data. Prices are generated one M15 bar at a time from 15 one-minute steps:

- random walk in log mid: Student-t(5) minute steps, an intraday volatility profile (New York hours), daily volatility
  that wanders (log-AR(1)), a shared dollar factor across pairs (correlation `rho`), a jump at each Sunday open,
  no bars from Friday 17:00 to Sunday 17:00 New York, no bars on 25 December and 1 January;
- bid and ask around the mid: a spread in pips that is wider at 17:00 New York and in Asian hours;
- optional constant drift (`drift_bp_day`), optional trend regimes (`trend_bp_day`: the drift flips sign at random,
  mean regime length `trend_days`), and optional planted reactions to events (`effect`, `strength_bp`):
    "pdh_break"     after an M15 close crosses the prior day's high or low, the mid moves strength_bp further in the
                    direction of the cross over the next `effect_bars` M15 bars;
    "round_bounce"  after an M15 bar touches a big figure without its close crossing it, the mid moves strength_bp away
                    from the figure over the next `effect_bars` M15 bars.
  Events are detected on the generated path as it is built, with the definitions of events.py (levels known at the
  start of the bar), so the planted reaction starts on the bar after the event: no tool can see it before the close.

`generate(...)` returns {pair: {TF: {field: array}}} for TF in M15, H1, H4, D1, W1.
`make_features(bars, X, pair)` returns a SPEC section 6 feature dict: every SPEC column name is present; the columns
the study tools use (fill and context columns, `available`, the event columns) are filled, the others are NaN.
"""

import datetime as dt
import math
import sys
from pathlib import Path

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402
import events  # noqa: E402

PAIR_PARAMS = {
    # price level, pip, typical spread in pips (2008-2012-like), daily vol of log price, sign of the dollar factor
    "EURUSD": dict(price=1.35, pip=1e-4, spread_pips=1.0, vol=0.0065, usd=-1),
    "GBPUSD": dict(price=1.60, pip=1e-4, spread_pips=1.4, vol=0.0062, usd=-1),
    "USDJPY": dict(price=90.0, pip=1e-2, spread_pips=1.0, vol=0.0068, usd=+1),
    "USDCHF": dict(price=1.00, pip=1e-4, spread_pips=1.6, vol=0.0070, usd=+1),
    "USDCAD": dict(price=1.10, pip=1e-4, spread_pips=1.6, vol=0.0058, usd=+1),
    "NZDUSD": dict(price=0.70, pip=1e-4, spread_pips=2.0, vol=0.0085, usd=-1),
}

NY_SHIFT = 7 * 3600
TF_SECONDS = {"M15": 900, "H1": 3600, "H4": 14400, "D1": 86400, "W1": 5 * 86400}


# ------------------------------------------------------------------------------------------------ clock

def ny_offset(ts):
    ts = np.asarray(ts, dtype=np.int64)
    utc = pd.DatetimeIndex(ts.astype("datetime64[s]")).tz_localize("UTC")
    local = utc.tz_convert(common.NY).tz_localize(None)
    return local.to_numpy().astype("datetime64[s]").astype(np.int64) - ts


def ny_shifted(ts):
    """// 86400 = trading-day label (days since epoch); % 86400 = seconds since 17:00 New York."""
    return np.asarray(ts, dtype=np.int64) + ny_offset(ts) + NY_SHIFT


def yyyymmdd(days):
    d = np.asarray(days, dtype=np.int64).astype("datetime64[D]")
    y = d.astype("datetime64[Y]").astype(np.int64) + 1970
    m = d.astype("datetime64[M]").astype(np.int64) % 12 + 1
    dd = (d - d.astype("datetime64[M]")).astype(np.int64) + 1
    return (y * 10000 + m * 100 + dd).astype(np.int32)


def day_number(yyyymmdd_arr):
    """YYYYMMDD int -> days since 1970-01-01."""
    a = np.asarray(yyyymmdd_arr, dtype=np.int64)
    s = pd.to_datetime(pd.Series(a.astype(str)), format="%Y%m%d")
    return (s.to_numpy().astype("datetime64[D]").astype(np.int64))


def m15_calendar(start, end):
    """M15 bar starts (UTC s) for trading days start..end (dates or ISO strings), weekends and 25 Dec / 1 Jan removed.
    Returns (start, trading-day number, seconds since 17:00 NY)."""
    if isinstance(start, str):
        start = dt.date.fromisoformat(start)
    if isinstance(end, str):
        end = dt.date.fromisoformat(end)
    t0 = int(common.trading_day_end_utc(start - dt.timedelta(days=1)).timestamp())
    t1 = int(common.trading_day_end_utc(end).timestamp())
    s = np.arange(t0, t1, 900, dtype=np.int64)
    sh = ny_shifted(s)
    day = sh // 86400
    wd = (day + 3) % 7
    lab = yyyymmdd(day)
    md = lab % 10000
    keep = (wd < 5) & (md != 1225) & (md != 101)
    return s[keep], day[keep], (sh % 86400)[keep]


def intraday_profile(sec_since_17):
    """relative minute volatility by New York hour; mean square over a day = 1."""
    h = ((sec_since_17 // 3600) + 17) % 24
    p = np.select([h == 17, (h >= 18) | (h <= 1), h <= 2, h <= 7, h <= 11, h <= 16],
                  [0.5, 0.6, 0.8, 1.2, 1.4, 0.9], 1.0)
    allh = (np.arange(96) * 900 // 3600 + 17) % 24
    q = np.select([allh == 17, (allh >= 18) | (allh <= 1), allh <= 2, allh <= 7, allh <= 11, allh <= 16],
                  [0.5, 0.6, 0.8, 1.2, 1.4, 0.9], 1.0)
    return p / math.sqrt(np.mean(q ** 2))


def spread_multiplier(sec_since_17):
    h = ((sec_since_17 // 3600) + 17) % 24
    return np.select([h == 17, (h >= 18) | (h <= 1)], [3.0, 1.4], 1.0)


# ------------------------------------------------------------------------------------------------ bars

def aggregate(m15, tf):
    """M15 -> tf, per SPEC section 5 (H4 anchored on 17:00 NY, D1 = trading day, W1 = Sunday 17:00 .. Friday 17:00)."""
    s = m15["start"]
    if tf == "H1":
        key = s - s % 3600
    else:
        sh = ny_shifted(s)
        if tf == "H4":
            key = s - sh % 14400
        elif tf == "D1":
            key = s - sh % 86400
        elif tf == "W1":
            key = s - sh % 86400 - ((sh // 86400 + 3) % 7) * 86400
        else:
            raise ValueError(tf)
    n = len(key)
    first = np.r_[0, np.flatnonzero(np.diff(key)) + 1]
    last = np.r_[first[1:] - 1, n - 1]
    b = {"start": key[first].astype(np.int64)}
    b["end"] = b["start"] + TF_SECONDS[tf]
    for f in ("mid_open", "bid_open", "ask_open"):
        b[f] = m15[f][first]
    b["mid_high"] = np.maximum.reduceat(m15["mid_high"], first)
    b["mid_low"] = np.minimum.reduceat(m15["mid_low"], first)
    for f in ("mid_close", "bid_close", "ask_close"):
        b[f] = m15[f][last]
    nm = np.add.reduceat(m15["n_minutes"].astype(np.int64), first)
    b["spread_mean"] = np.add.reduceat(m15["spread_mean"] * m15["n_minutes"], first) / nm
    b["n_minutes"] = nm.astype(np.int32)
    if tf != "W1":
        b["trading_day"] = m15["trading_day"][last]
    return b


def _ar1(rng, n, phi, sd):
    e = rng.standard_normal(n) * sd * math.sqrt(1 - phi * phi)
    x = np.empty(n)
    acc = rng.standard_normal() * sd
    for i in range(n):
        acc = phi * acc + e[i]
        x[i] = acc
    return x


def _trend_path(rng, days, trend_bp_day, trend_days):
    """per-day drift (bp/day) that flips sign at random with mean regime length trend_days."""
    nd = len(days)
    flips = rng.random(nd) < 1.0 / trend_days
    sign = np.where(np.cumsum(flips) % 2 == 0, 1.0, -1.0) * (1 if rng.random() < 0.5 else -1)
    return sign * trend_bp_day


def generate(pairs=("EURUSD", "USDJPY"), start="2008-01-01", end="2008-12-31", seed=0, effect=None, strength_bp=0.0,
             effect_bars=4, drift_bp_day=0.0, trend_bp_day=0.0, trend_days=20, rho=0.4, vol_cluster=0.3,
             spread_scale=1.0, stale=None):
    """stale: list of (UTC start "YYYY-MM-DD HH:MM", hours): a planted stale-quote stretch in every pair, during which
    the mid does not move (a stuck feed, as EUR/USD 2009-06-29 to 07-01; SPEC 5a)."""
    rng = np.random.default_rng(seed)
    s, day, sec = m15_calendar(start, end)
    n = len(s)
    udays, dinv = np.unique(day, return_inverse=True)
    regime = np.exp(_ar1(rng, len(udays), 0.97, vol_cluster) - vol_cluster ** 2 / 2)[dinv]
    prof = intraday_profile(sec)
    F = rng.standard_t(5, (n, 15)) * math.sqrt(3.0 / 5.0)
    gap = np.r_[True, np.diff(s) > 3600]          # first bar of the data or of a week / after a holiday
    out = {}
    for pi, pair in enumerate(pairs):
        p = PAIR_PARAMS[pair]
        r = np.random.default_rng([seed, pi + 1])
        E = r.standard_t(5, (n, 15)) * math.sqrt(3.0 / 5.0)
        Z = math.sqrt(rho) * p["usd"] * F + math.sqrt(1 - rho) * E
        sig = p["vol"] / math.sqrt(1440.0) * prof * regime
        inc = Z * sig[:, None]
        drift_day = np.full(len(udays), drift_bp_day, float)
        if trend_bp_day:
            drift_day = drift_day + _trend_path(r, udays, trend_bp_day, trend_days)
        inc += (drift_day[dinv] * 1e-4 / 1440.0)[:, None]
        jump = np.where(gap, r.standard_normal(n) * 3.0 * sig * math.sqrt(15.0), 0.0)
        jump[0] = 0.0
        for st, hours in stale or []:
            t0 = int(pd.Timestamp(st, tz="UTC").timestamp())
            frozen = (s >= t0) & (s < t0 + int(hours * 3600))
            inc[frozen] = 0.0
            jump[frozen] = 0.0
        cum = np.cumsum(inc, axis=1)
        d_close = cum[:, -1]
        d_hi = np.maximum(0.0, cum.max(axis=1))
        d_lo = np.minimum(0.0, cum.min(axis=1))
        if effect is None or strength_bp == 0:
            lc = math.log(p["price"]) + np.cumsum(jump + d_close)
            lo_ = lc - d_close
            lhi, llo = lo_ + d_hi, lo_ + d_lo
        else:
            lo_, lhi, llo, lc = _sequential(pair, effect, strength_bp, effect_bars, math.log(p["price"]), day,
                                            jump, cum, d_close, d_hi, d_lo)
        mo, mh, ml, mc = np.exp(lo_), np.exp(lhi), np.exp(llo), np.exp(lc)
        spr = p["spread_pips"] * p["pip"] * spread_scale * spread_multiplier(sec) * (1 + 0.15 * np.abs(r.standard_normal(n)))
        m15 = {"start": s.copy(), "end": s + 900, "mid_open": mo, "mid_high": mh, "mid_low": ml, "mid_close": mc,
               "bid_open": mo - spr / 2, "ask_open": mo + spr / 2, "bid_close": mc - spr / 2, "ask_close": mc + spr / 2,
               "spread_mean": spr, "n_minutes": np.full(n, 15, np.int32), "trading_day": yyyymmdd(day)}
        bars = {"M15": m15}
        for tf in ("H1", "H4", "D1", "W1"):
            bars[tf] = aggregate(m15, tf)
        out[pair] = bars
    return out


def _sequential(pair, effect, strength_bp, k, l0, day, jump, cum, d_close, d_hi, d_lo):
    """Build the log path bar by bar, detecting events at each close and adding the planted drift afterwards."""
    n, m = cum.shape
    ar = np.arange(1, m + 1, dtype=float)
    fig = events.big_figure(pair)
    lo_, lhi, llo, lc = np.empty(n), np.empty(n), np.empty(n), np.empty(n)
    dayl = day.tolist()
    jl, dcl, dhl, dll = jump.tolist(), d_close.tolist(), d_hi.tolist(), d_lo.tolist()
    per_min = strength_bp * 1e-4 / (m * k)
    left, sgn = 0, 0.0
    prev_close = l0
    cur_day, day_hi, day_lo = None, -1e300, 1e300
    pdh = pdl = None
    pc_price = None
    for b in range(n):
        if dayl[b] != cur_day:
            if cur_day is not None:
                pdh, pdl = day_hi, day_lo
            cur_day, day_hi, day_lo = dayl[b], -1e300, 1e300
        o = prev_close + jl[b]
        if left > 0:
            path = cum[b] + sgn * per_min * ar
            c = o + path[-1]
            hi = o + max(0.0, path.max())
            lw = o + min(0.0, path.min())
            left -= 1
        else:
            c, hi, lw = o + dcl[b], o + dhl[b], o + dll[b]
        lo_[b], lhi[b], llo[b], lc[b] = o, hi, lw, c
        day_hi, day_lo = max(day_hi, hi), min(day_lo, lw)
        # events at the close of bar b (levels known at the start of bar b)
        d = 0
        if effect == "pdh_break" and pdh is not None:
            for L in (pdh, pdl):
                if prev_close < L <= c:
                    d = 1
                elif prev_close >= L > c:
                    d = -1
            if d:
                left, sgn = k, float(d)
        elif effect == "round_bounce" and pc_price is not None:
            cp, hp, lp = math.exp(c), math.exp(hi), math.exp(lw)
            cell, pcell = math.floor(cp / fig), math.floor(pc_price / fig)
            if cell == pcell:
                up = hp >= (cell + 1) * fig
                dn = lp < cell * fig
                if up and not dn:
                    left, sgn = k, -1.0
                elif dn and not up:
                    left, sgn = k, 1.0
        elif effect not in ("pdh_break", "round_bounce"):
            raise ValueError(effect)
        pc_price = math.exp(c)
        prev_close = c
    return lo_, lhi, llo, lc


# ------------------------------------------------------------------------------------------------ features

CLOSE_LAGS = {"M15": 32, "H1": 24, "H4": 30, "D1": 20}
PER_CHART = (["sma%d_dist" % n for n in (20, 50, 100, 200)] + ["ema%d_dist" % n for n in (20, 50, 100, 200)] +
             ["sma50_200_dist", "cross_sma50_age", "cross_sma200_age", "rsi14", "macd", "macd_signal", "macd_hist",
              "stoch_k", "stoch_d", "bb_pos", "bb_width", "atr_bp", "atr_regime", "donchian20_hi_dist",
              "donchian20_lo_dist"])
LEVELS = ["pdh_dist", "pdl_dist", "pdc_dist", "pwh_dist", "pwl_dist", "pwc_dist", "pivot_p", "pivot_r1", "pivot_s1",
          "pivot_r2", "pivot_s2", "round_big_dist", "round_half_dist"]
CLOCK = ["clk_day_sin", "clk_day_cos", "clk_day2_sin", "clk_day2_cos", "clk_week_sin", "clk_week_cos"]
CONTEXT = ["mid_close", "bid_close", "ask_close", "spread_mean", "n_minutes", "trading_day"]


def spec_feature_columns(X):
    cols = ["available"]
    for C in events.CHARTS[X]:
        cols += [f"{C}_{c}" for c in PER_CHART] + [f"{C}_close_lag{k}" for k in range(1, CLOSE_LAGS[C] + 1)]
    cols += LEVELS
    cols += ["ev_pdh_break", "ev_pdl_break", "ev_pdh_touch", "ev_pdl_touch", "ev_donchian20_break",
             "ev_round_big_cross"]
    cols += [f"{C}_ev_sma{n}_cross" for C in events.CHARTS[X] for n in (50, 200)]
    cols += CLOCK + CONTEXT + ["quality_excluded"]
    return cols


def make_features(bars, X, pair, intervals=None):
    """intervals: stale-quote intervals as quality.py writes them (e.g. quality.stale_intervals(bars["H1"]))."""
    import quality
    x = bars[X]
    n = len(x["end"])
    ev = events.spec_event_columns(bars, X, pair)
    ev["quality_excluded"] = quality.excluded_mask(x["end"], intervals or []).astype(np.int8)
    out = {}
    for col in spec_feature_columns(X):
        if col == "available":
            out[col] = x["end"].astype(np.int64)
        elif col in CONTEXT:
            out[col] = x[col].copy()
        elif col in ev:
            out[col] = ev[col].astype(np.int8)
        else:
            out[col] = np.full(n, np.nan, np.float32)
    return out
