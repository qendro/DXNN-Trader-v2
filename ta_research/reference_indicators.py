"""Slow, loop-based reference for the SPEC section 6 feature columns, one decision row at a time.

    reference_row(bars, decision_tf, pair, t_index) -> dict[str, float]

Written from SPEC.md independently of features.py, so that the two can disagree. Plain Python loops over the bar
lists; the recursive indicators (Wilder ATR/RSI, EMAs, MACD) run from the first bar of each series and are cached per
series. Nothing here reads files.

Conventions: SPEC section 6b (settled 2026-11-16), implemented from the SPEC text:

- Completed chart bars: C bars with end <= available (= end[t] of the decision bar). For C == X that includes bar t.
  Price = mid_close[t] of the decision bar.
- Price grid (6b.1): every comparison uses integers on the grid (1e-6 non-JPY, 1e-4 USD/JPY); mids are exact on it.
  Equal counts as ABOVE everywhere: side = (x >= L); cross up = prev < L <= cur, cross down = prev >= L > cur.
  Comparisons with an average use the average rounded to the grid (half-way rounds up); see CONVENTIONS["sma_compare"].
- ATR(14) (6b.2): TR undefined on the series' first bar; TR[i] = max(H - L, |H - C[i-1]|, |L - C[i-1]|) with the previous
  bar of the same series (gaps ignored); Wilder, seeded at i=14 with mean(TR[1..14]).
- RSI(14): gains/losses of C[i] - C[i-1], i >= 1, seeded at i=14 with the means of the first 14; average loss 0 -> 100,
  both 0 -> 50.
- EMA(n) (6b.3): alpha = 2/(n+1), seeded with the SMA of the first n values (first value at i=n-1). MACD = EMA12 - EMA26
  (from i=25); signal = EMA9 of MACD seeded with the mean of its first 9 values (from i=33); hist = MACD - signal.
- Slow stochastic (14, 3, 3): raw %K = 100 (C - LL14) / (HH14 - LL14) over mid highs/lows, 50 if HH == LL;
  slow %K = SMA3(raw %K); %D = SMA3(slow %K).
- Bollinger: SMA20 and population stdev of the last 20 C closes; bb_pos NaN when stdev is 0.
- cross_sma{n}_age (6b.4): count of consecutive C bars on the current side (close >= SMA is above) including bar j,
  so +-1 on the crossing bar; capped at 100; NaN if no side change has been seen and the count is below 100.
- {C}_ev_sma{n}_cross: +1/-1 when the newest completed C bar j is itself a crossing bar (side differs from bar j-1),
  flagged only on the row whose last completed C bar differs from the previous row's.
- atr_regime: ATR_C[j] / mean(ATR_C[j-99..j]) - 1 (needs 100 defined ATR values).
- donchian20_{hi,lo}_dist: highest mid_high / lowest mid_low over the last 20 completed C bars (bar j included).
- close_lag{k} (6b.9): close of the k-th last C bar with end < available (strictly); for C == X lag1 is bar t-1.
  ATR_C is the ATR at j (the last completed bar).
- D1 stubs (n_minutes < 360) are removed from the D1 series before anything D1-based is computed (D1 chart features,
  D1 ATR, previous-day levels, pivots). W1 bars are used as built. prev_day_stub: the last completed D1 bar of the
  full series (stubs included) has n_minutes < 360.
- Level distances (6b.5): last completed non-stub D1 / last completed W1 bar at `available`, in D1 ATR(14) units at
  that D1 bar; distance = price - level.
- Round numbers (6b.7): big figures k x 0.01 (1.00 for USD/JPY); half figures only (k + 1/2) x big figure; the nearest
  level on an exact half-way tie is the upper one.
- Decision events: previous close = mid_close[t-1] (all events 0 for t = 0).
  Levels for ev_pdh_*/ev_pdl_* (6b.5): last non-stub D1 bar with end <= start[t].
  ev_pdh_break / ev_pdl_break (6b.6): +1 if prev < L <= close, -1 if prev >= L > close.
  ev_pdh_touch: 1 if prev < PDH, mid_high[t] >= PDH and close < PDH.
  ev_pdl_touch: 1 if prev >= PDL, mid_low[t] < PDL and close >= PDL.
  ev_donchian20_break: +1 if close >= max(mid_high[t-20..t-1]) (equal counts as above, 6b.1), -1 if
  close < min(mid_low[t-20..t-1]), 0 if t < 20.
  ev_round_big_cross: +1 / -1 if some big figure F has prev < F <= close / prev >= F > close.
- Clock (6b.8): New York wall time at `available`; day fraction f = (time since 17:00) / 24 h -> sin/cos(2 pi f),
  sin/cos(4 pi f); week fraction w = (time since Sunday 17:00) / 5 days -> sin/cos(2 pi w).
- quality_excluded (6, 5a): 1 when available lies in (start, end] of an interval in quality/<PAIR>.json (checked in
  check_features.py; not part of reference_row, which reads no files).
- Division by zero (ATR 0, stdev 0) gives NaN.

CONVENTIONS holds the few choices that have an alternative reading. Its defaults are the reference (SPEC 6b).
check_features.py switches single entries to ALTERNATIVES only to label a mismatch; the verdict uses the defaults.
"""

import bisect
import datetime as dt
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402

NAN = float("nan")
CHARTS = {"M15": ["M15", "H1", "H4", "D1"], "H1": ["H1", "H4", "D1"], "H4": ["H4", "D1"]}
LAGS = {"M15": 32, "H1": 24, "H4": 30, "D1": 20}
AVG_LENS = (20, 50, 100, 200)
STUB_MINUTES = 360
ATR_N = 14
RSI_N = 14

DEFAULT_CONVENTIONS = {
    "sma_compare": "grid",          # close vs SMA with the SMA rounded to the price grid; "exact": exact rational SMA
    "atr_tr0": "skip",              # 6b.2; "high_low": TR[0] = H - L, first ATR at i=13 (my pre-6b reading)
    "donchian_ties": "equal_above",  # 6b.1 applied to the Donchian break; "strict": close > prior high
    "close_lag": "strict",          # 6b.9; "inclusive": end <= end[t] (the pre-revision rule)
}
CONVENTIONS = dict(DEFAULT_CONVENTIONS)
ALTERNATIVES = {"sma_compare": "exact", "atr_tr0": "high_low", "donchian_ties": "strict", "close_lag": "inclusive"}


def set_conventions(**kw):
    """Replace CONVENTIONS (defaults plus kw). Cached series are keyed by the one convention they depend on."""
    CONVENTIONS.clear()
    CONVENTIONS.update(DEFAULT_CONVENTIONS)
    for k, v in kw.items():
        if k not in DEFAULT_CONVENTIONS:
            raise KeyError(k)
        CONVENTIONS[k] = v


LEVEL_COLS = ["pdh_dist", "pdl_dist", "pdc_dist", "pwh_dist", "pwl_dist", "pwc_dist",
              "pivot_p", "pivot_r1", "pivot_s1", "pivot_r2", "pivot_s2", "round_big_dist", "round_half_dist"]
EVENT_COLS = ["ev_pdh_break", "ev_pdl_break", "ev_pdh_touch", "ev_pdl_touch", "ev_donchian20_break",
              "ev_round_big_cross"]
CLOCK_COLS = ["clk_day_sin", "clk_day_cos", "clk_day2_sin", "clk_day2_cos", "clk_week_sin", "clk_week_cos"]
FILL_FLOAT_COLS = ["mid_close", "bid_close", "ask_close", "spread_mean"]
FILL_INT_COLS = ["n_minutes", "trading_day"]


def chart_columns(c):
    p = c + "_"
    cols = []
    cols += [f"{p}sma{n}_dist" for n in AVG_LENS]
    cols += [f"{p}ema{n}_dist" for n in AVG_LENS]
    cols += [f"{p}sma50_200_dist", f"{p}cross_sma50_age", f"{p}cross_sma200_age", f"{p}rsi14",
             f"{p}macd", f"{p}macd_signal", f"{p}macd_hist", f"{p}stoch_k", f"{p}stoch_d",
             f"{p}bb_pos", f"{p}bb_width", f"{p}atr_bp", f"{p}atr_regime",
             f"{p}donchian20_hi_dist", f"{p}donchian20_lo_dist"]
    cols += [f"{p}close_lag{k}" for k in range(1, LAGS[c] + 1)]
    cols += [f"{p}ev_sma50_cross", f"{p}ev_sma200_cross"]
    return cols


def spec_columns(decision_tf):
    """Every SPEC section 6 column for decision timeframe X, as written in the SPEC."""
    cols = []
    for c in CHARTS[decision_tf]:
        cols += chart_columns(c)
    cols += LEVEL_COLS + ["prev_day_stub"] + EVENT_COLS + CLOCK_COLS + FILL_FLOAT_COLS + FILL_INT_COLS + ["available"]
    return cols + [QUALITY_COL]


def row_columns(decision_tf):
    """The columns reference_row computes: every SPEC column except quality_excluded (it needs the quality file)."""
    return [c for c in spec_columns(decision_tf) if c != QUALITY_COL]


QUALITY_COL = "quality_excluded"     # SPEC 5a / 6: checked over all rows in check_features.check_quality


def exact_columns(decision_tf):
    """Columns compared exactly (events, flags, counts, times, bar ages)."""
    out = set(EVENT_COLS) | set(FILL_INT_COLS) | {"prev_day_stub", "available", QUALITY_COL}
    for c in CHARTS[decision_tf]:
        out |= {f"{c}_ev_sma50_cross", f"{c}_ev_sma200_cross", f"{c}_cross_sma50_age", f"{c}_cross_sma200_age"}
    return out


# ---------------------------------------------------------------- helpers

def price_scale(pair):
    return 10 ** 4 if pair in common.JPY_PAIRS else 10 ** 6


def to_grid(x, scale):
    v = x * scale
    r = int(round(v))
    if abs(v - r) > 1e-3:
        raise ValueError(f"price {x!r} is not on the 1/{scale} grid")
    return r


def div(a, b):
    if a != a or b != b or b == 0:
        return NAN
    return a / b


def wilder_atr(high, low, close, n=ATR_N, skip_first=False):
    size = len(close)
    tr = []
    for i in range(size):
        if i == 0:
            tr.append(high[i] - low[i])
        else:
            tr.append(max(high[i] - low[i], abs(high[i] - close[i - 1]), abs(low[i] - close[i - 1])))
    out = [NAN] * size
    f = 1 if skip_first else 0
    if size >= n + f:
        a = sum(tr[f:f + n]) / n
        out[f + n - 1] = a
        for i in range(f + n, size):
            a = (a * (n - 1) + tr[i]) / n
            out[i] = a
    return out


def rsi_value(avg_gain, avg_loss):
    if avg_loss == 0:
        return 100.0 if avg_gain > 0 else 50.0
    return 100.0 - 100.0 / (1.0 + avg_gain / avg_loss)


def wilder_rsi(close, n=RSI_N):
    size = len(close)
    out = [NAN] * size
    if size <= n:
        return out
    g = l = 0.0
    for i in range(1, n + 1):
        d = close[i] - close[i - 1]
        g += max(d, 0.0)
        l += max(-d, 0.0)
    ag, al = g / n, l / n
    out[n] = rsi_value(ag, al)
    for i in range(n + 1, size):
        d = close[i] - close[i - 1]
        ag = (ag * (n - 1) + max(d, 0.0)) / n
        al = (al * (n - 1) + max(-d, 0.0)) / n
        out[i] = rsi_value(ag, al)
    return out


def ema(values, n, first=0):
    """EMA(n) of values[first:], seeded with the SMA of values[first:first+n]; NaN before first+n-1."""
    size = len(values)
    out = [NAN] * size
    if size - first < n:
        return out
    alpha = 2.0 / (n + 1)
    e = sum(values[first:first + n]) / n
    out[first + n - 1] = e
    for i in range(first + n, size):
        e = alpha * values[i] + (1 - alpha) * e
        out[i] = e
    return out


class Series:
    """One chart series as Python lists, with the recursive indicators precomputed from its first bar."""

    def __init__(self, b, scale, keep=None):
        idx = list(range(len(b["end"]))) if keep is None else keep
        self.scale = scale
        self.end = [int(b["end"][i]) for i in idx]
        self.high = [float(b["mid_high"][i]) for i in idx]
        self.low = [float(b["mid_low"][i]) for i in idx]
        self.close = [float(b["mid_close"][i]) for i in idx]
        self.close_i = [to_grid(x, scale) for x in self.close]
        self.high_i = [to_grid(x, scale) for x in self.high]
        self.low_i = [to_grid(x, scale) for x in self.low]
        self.atr = wilder_atr(self.high, self.low, self.close, skip_first=CONVENTIONS["atr_tr0"] == "skip")
        self.first_atr = ATR_N if CONVENTIONS["atr_tr0"] == "skip" else ATR_N - 1
        self.rsi = wilder_rsi(self.close)
        self.ema = {n: ema(self.close, n) for n in (12, 26) + AVG_LENS}
        size = len(self.close)
        self.macd = [self.ema[12][i] - self.ema[26][i] if i >= 25 else NAN for i in range(size)]
        self.signal = ema(self.macd, 9, first=25)

    def last_completed(self, available):
        """Index of the last bar with end <= available, -1 if none."""
        return bisect.bisect_right(self.end, available) - 1

    def last_before(self, available):
        """Index of the last bar with end < available (strictly), -1 if none."""
        return bisect.bisect_left(self.end, available) - 1

    def sma(self, j, n):
        if j < n - 1:
            return NAN
        return sum(self.close_i[j - n + 1:j + 1]) / (n * self.scale)

    def sides(self, n, lo, hi):
        """{i: True if close[i] >= SMA_n[i] (above; equal counts as above)} for lo <= i <= hi; needs lo >= n-1.
        Integer arithmetic: SMA = s/n with s the sum of grid closes; "grid" rounds s/n half-up to an integer."""
        out = {}
        s = sum(self.close_i[lo - n + 1:lo + 1])
        for i in range(lo, hi + 1):
            if i > lo:
                s += self.close_i[i] - self.close_i[i - n]
            if CONVENTIONS["sma_compare"] == "grid":
                out[i] = self.close_i[i] >= (2 * s + n) // (2 * n)
            else:
                out[i] = self.close_i[i] * n >= s
        return out


_CACHE = {}


def _series(bars, tf, scale, kind="all"):
    arr = bars[tf]["end"]
    key = (id(arr), tf, kind, scale, CONVENTIONS["atr_tr0"])
    hit = _CACHE.get(key)
    if hit is not None and hit[0] is arr:
        return hit[1]
    b = bars[tf]
    keep = None
    if kind == "nonstub":
        keep = [i for i in range(len(b["end"])) if int(b["n_minutes"][i]) >= STUB_MINUTES]
    s = Series(b, scale, keep)
    _CACHE[key] = (arr, s)
    return s


def clear_cache():
    _CACHE.clear()


# ---------------------------------------------------------------- per-row pieces

def cross_age(s, j, n):
    """Consecutive bars on the current side including j (+-1 on the crossing bar), capped at 100; NaN if no side
    change has been seen yet and the count is below 100."""
    if j < n - 1:
        return NAN
    lo = max(n - 1, j - 99)                 # the side is defined from bar n-1; 100 bars are enough
    side = s.sides(n, lo, j)
    count = 1
    i = j
    while count < 100:
        if i - 1 < lo:
            return NAN                      # reached the first bar with an average: no cross seen
        if side[i - 1] != side[j]:
            break                           # bar i is the crossing bar
        count += 1
        i -= 1
    return float(count if side[j] else -count)


def cross_event(s, j, n):
    if j < n:
        return 0
    side = s.sides(n, j - 1, j)
    if side[j] == side[j - 1]:
        return 0
    return 1 if side[j] else -1


def raw_k(s, i):
    hh = max(s.high[i - 13:i + 1])
    ll = min(s.low[i - 13:i + 1])
    if max(s.high_i[i - 13:i + 1]) == min(s.low_i[i - 13:i + 1]):
        return 50.0
    return 100.0 * (s.close[i] - ll) / (hh - ll)


def slow_k(s, i):
    return (raw_k(s, i) + raw_k(s, i - 1) + raw_k(s, i - 2)) / 3.0


def chart_features(s, j, j_prev, price, prefix, n_lags, out, j_strict=None):
    p = prefix + "_"
    atr = s.atr[j] if j >= 0 else NAN
    sma = {n: (s.sma(j, n) if j >= 0 else NAN) for n in AVG_LENS}
    for n in AVG_LENS:
        out[f"{p}sma{n}_dist"] = div(price - sma[n], atr)
        out[f"{p}ema{n}_dist"] = div(price - (s.ema[n][j] if j >= 0 else NAN), atr)
    out[f"{p}sma50_200_dist"] = div(sma[50] - sma[200], atr)
    out[f"{p}cross_sma50_age"] = cross_age(s, j, 50) if j >= 0 else NAN
    out[f"{p}cross_sma200_age"] = cross_age(s, j, 200) if j >= 0 else NAN
    r = s.rsi[j] if j >= 0 else NAN
    out[f"{p}rsi14"] = (r - 50.0) / 50.0
    m = s.macd[j] if j >= 0 else NAN
    sg = s.signal[j] if j >= 0 else NAN
    out[f"{p}macd"] = div(m, atr)
    out[f"{p}macd_signal"] = div(sg, atr)
    out[f"{p}macd_hist"] = div(m - sg, atr)
    if j >= 15:
        out[f"{p}stoch_k"] = (slow_k(s, j) - 50.0) / 50.0
    else:
        out[f"{p}stoch_k"] = NAN
    if j >= 17:
        d = (slow_k(s, j) + slow_k(s, j - 1) + slow_k(s, j - 2)) / 3.0
        out[f"{p}stoch_d"] = (d - 50.0) / 50.0
    else:
        out[f"{p}stoch_d"] = NAN
    if j >= 19:
        window = s.close[j - 19:j + 1]
        mean = sma[20]
        var = 0.0
        for x in window:
            var += (x - mean) ** 2
        sd = math.sqrt(var / 20.0)
        out[f"{p}bb_pos"] = div(price - mean, 2.0 * sd)
        out[f"{p}bb_width"] = div(4.0 * sd, atr)
        out[f"{p}donchian20_hi_dist"] = div(price - max(s.high[j - 19:j + 1]), atr)
        out[f"{p}donchian20_lo_dist"] = div(price - min(s.low[j - 19:j + 1]), atr)
    else:
        for k in ("bb_pos", "bb_width", "donchian20_hi_dist", "donchian20_lo_dist"):
            out[p + k] = NAN
    out[f"{p}atr_bp"] = div(atr, price) * 10000.0
    if j - 99 >= s.first_atr:
        mean_atr = sum(s.atr[j - 99:j + 1]) / 100.0
        out[f"{p}atr_regime"] = div(atr, mean_atr) - 1.0
    else:
        out[f"{p}atr_regime"] = NAN
    js = j if (j_strict is None or CONVENTIONS["close_lag"] == "inclusive") else j_strict
    for k in range(1, n_lags + 1):
        i = js - k + 1
        out[f"{p}close_lag{k}"] = div(price - s.close[i], atr) if i >= 0 else NAN
    new_bar = j >= 0 and j != j_prev
    out[f"{p}ev_sma50_cross"] = float(cross_event(s, j, 50)) if new_bar else 0.0
    out[f"{p}ev_sma200_cross"] = float(cross_event(s, j, 200)) if new_bar else 0.0


def nearest_figure(p, step):
    """Nearest multiple of step to integer p; an exact half-way tie goes to the upper one: floor(p/step + 1/2)."""
    return ((2 * p + step) // (2 * step)) * step


def nearest_half(p, big):
    """Nearest (k + 1/2) x big to p (half figures only, big figures excluded); ties go up."""
    return nearest_figure(p - big // 2, big) + big // 2


def crossing(prev, cur, level):
    """6b.1: +1 if prev < level <= cur, -1 if prev >= level > cur, else 0 (integers on the grid)."""
    if prev < level <= cur:
        return 1
    if prev >= level > cur:
        return -1
    return 0


def figure_crossing(prev, cur, step):
    """+1 / -1 if some multiple F of step has prev < F <= cur / prev >= F > cur."""
    if cur > prev:
        f = (cur // step) * step                     # largest multiple <= cur
        return 1 if f > prev else 0
    if cur < prev:
        f = (prev // step) * step                    # largest multiple <= prev
        return -1 if f > cur else 0
    return 0


def clock(available):
    local = dt.datetime.fromtimestamp(available, tz=common.NY)
    tod = local.hour * 3600 + local.minute * 60 + local.second
    f = ((tod - 17 * 3600) % 86400) / 86400.0
    days_since_sunday = (local.weekday() + 1) % 7      # Monday=0 .. Sunday=6 -> Sunday=0
    secs = days_since_sunday * 86400 + tod - 17 * 3600
    if secs < 0:
        secs += 7 * 86400
    w = secs / (5 * 86400.0)
    return {
        "clk_day_sin": math.sin(2 * math.pi * f), "clk_day_cos": math.cos(2 * math.pi * f),
        "clk_day2_sin": math.sin(4 * math.pi * f), "clk_day2_cos": math.cos(4 * math.pi * f),
        "clk_week_sin": math.sin(2 * math.pi * w), "clk_week_cos": math.cos(2 * math.pi * w),
    }


# ---------------------------------------------------------------- the row

def reference_row(bars, decision_tf, pair, t_index):
    """All SPEC section 6 columns for decision bar t_index of `decision_tf`, from `bars` ({tf: {field: array}})."""
    scale = price_scale(pair)
    xb = bars[decision_tf]
    t = int(t_index)
    xs = _series(bars, decision_tf, scale)
    available = int(xb["end"][t])
    prev_available = int(xb["end"][t - 1]) if t > 0 else None
    price = float(xb["mid_close"][t])
    price_i = to_grid(price, scale)
    out = {}

    # chart features
    for c in CHARTS[decision_tf]:
        s = _series(bars, c, scale, "nonstub" if c == "D1" else "all")
        j = s.last_completed(available)
        j_prev = s.last_completed(prev_available) if prev_available is not None else -1
        chart_features(s, j, j_prev, price, c, LAGS[c], out, j_strict=s.last_before(available))

    # levels
    d1 = _series(bars, "D1", scale, "nonstub")
    jd = d1.last_completed(available)
    atr_d = d1.atr[jd] if jd >= 0 else NAN
    if jd >= 0:
        h, l, c = d1.high[jd], d1.low[jd], d1.close[jd]
    else:
        h = l = c = NAN
    out["pdh_dist"] = div(price - h, atr_d)
    out["pdl_dist"] = div(price - l, atr_d)
    out["pdc_dist"] = div(price - c, atr_d)
    pp = (h + l + c) / 3.0
    out["pivot_p"] = div(price - pp, atr_d)
    out["pivot_r1"] = div(price - (2 * pp - l), atr_d)
    out["pivot_s1"] = div(price - (2 * pp - h), atr_d)
    out["pivot_r2"] = div(price - (pp + (h - l)), atr_d)
    out["pivot_s2"] = div(price - (pp - (h - l)), atr_d)
    w1 = bars["W1"]
    w_ends = [int(x) for x in w1["end"]]
    jw = bisect.bisect_right(w_ends, available) - 1
    if jw >= 0:
        wh, wl, wc = float(w1["mid_high"][jw]), float(w1["mid_low"][jw]), float(w1["mid_close"][jw])
    else:
        wh = wl = wc = NAN
    out["pwh_dist"] = div(price - wh, atr_d)
    out["pwl_dist"] = div(price - wl, atr_d)
    out["pwc_dist"] = div(price - wc, atr_d)
    big = scale // 100 if scale == 10 ** 6 else scale     # 0.01 on the 1e-6 grid, 1.00 on the 1e-4 grid
    out["round_big_dist"] = div((price_i - nearest_figure(price_i, big)) / scale, atr_d)
    out["round_half_dist"] = div((price_i - nearest_half(price_i, big)) / scale, atr_d)
    d_all_ends = [int(x) for x in bars["D1"]["end"]]
    ja = bisect.bisect_right(d_all_ends, available) - 1
    out["prev_day_stub"] = 1.0 if ja >= 0 and int(bars["D1"]["n_minutes"][ja]) < STUB_MINUTES else 0.0

    # events on the decision bar (levels in force during bar t: D1 bars that ended by start[t])
    ev = {k: 0 for k in EVENT_COLS}
    je = d1.last_completed(int(xb["start"][t]))
    if t > 0:
        c0, c1 = xs.close_i[t - 1], xs.close_i[t]
        if je >= 0:
            ph, pl = d1.high_i[je], d1.low_i[je]
            ev["ev_pdh_break"] = crossing(c0, c1, ph)
            ev["ev_pdl_break"] = crossing(c0, c1, pl)
            if c0 < ph and xs.high_i[t] >= ph and c1 < ph:
                ev["ev_pdh_touch"] = 1
            if c0 >= pl and xs.low_i[t] < pl and c1 >= pl:
                ev["ev_pdl_touch"] = 1
        ev["ev_round_big_cross"] = figure_crossing(c0, c1, big)
    if t >= 20:
        hh = max(xs.high_i[t - 20:t])
        ll = min(xs.low_i[t - 20:t])
        up = xs.close_i[t] >= hh if CONVENTIONS["donchian_ties"] == "equal_above" else xs.close_i[t] > hh
        if up:
            ev["ev_donchian20_break"] = 1
        elif xs.close_i[t] < ll:
            ev["ev_donchian20_break"] = -1
    for k, v in ev.items():
        out[k] = float(v)

    out.update(clock(available))
    for k in FILL_FLOAT_COLS:
        out[k] = float(xb[k][t])
    for k in FILL_INT_COLS:
        out[k] = float(int(xb[k][t]))
    out["available"] = float(available)
    return out
