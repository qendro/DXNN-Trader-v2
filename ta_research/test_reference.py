"""Hand-computed cases for reference_indicators.py on tiny synthetic bar series (no market data).

python3 -m unittest ta_research/test_reference.py      (or, from ta_research/: python3 -m unittest test_reference)
"""

import datetime as dt
import math
import random
import sys
import unittest
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).resolve().parent))
import common  # noqa: E402
import reference_indicators as ri  # noqa: E402

H4 = 4 * 3600


def _arr(rows, key, dtype):
    return np.array([r[key] for r in rows], dtype=dtype)


def _pack(rows, with_day=True):
    out = {"start": _arr(rows, "start", np.int64), "end": _arr(rows, "end", np.int64)}
    for f in ("mid_open", "mid_high", "mid_low", "mid_close"):
        out[f] = _arr(rows, f, np.float64)
    for f in ("bid_open", "ask_open"):
        out[f] = out["mid_open"] + (0.00005 if f == "ask_open" else -0.00005)
    out["bid_close"] = out["mid_close"] - 0.00005
    out["ask_close"] = out["mid_close"] + 0.00005
    out["spread_mean"] = np.full(len(rows), 0.0001)
    out["n_minutes"] = _arr(rows, "n_minutes", np.int32)
    if with_day:
        out["trading_day"] = _arr(rows, "trading_day", np.int32)
    return out


def _agg(group, start, end, day=None):
    return {"start": start, "end": end, "mid_open": group[0]["mid_open"],
            "mid_high": max(g["mid_high"] for g in group), "mid_low": min(g["mid_low"] for g in group),
            "mid_close": group[-1]["mid_close"], "n_minutes": sum(g["n_minutes"] for g in group),
            "trading_day": day}


def make_bars(days):
    """days: list of (date, [(o, h, l, c, n_minutes or None), ...] up to 6 H4 bars, in order from 17:00 NY).
    Dates must be weekdays inside one DST regime per week. Returns {H4, D1, W1} bar dicts."""
    h4, d1 = [], []
    weeks = {}
    for day, bars in days:
        day_end = int(common.trading_day_end_utc(day).timestamp())
        label = day.year * 10000 + day.month * 100 + day.day
        rows = []
        for k, b in enumerate(bars):
            o, h, l, c = b[:4]
            n = b[4] if len(b) > 4 and b[4] is not None else 240
            start = day_end - 86400 + k * H4
            rows.append({"start": start, "end": start + H4, "mid_open": o, "mid_high": h, "mid_low": l,
                         "mid_close": c, "n_minutes": n, "trading_day": label})
        h4 += rows
        drow = _agg(rows, day_end - 86400, day_end, label)
        d1.append(drow)
        monday = day - dt.timedelta(days=day.weekday())
        weeks.setdefault(monday, []).append(drow)
    w1 = []
    for monday, group in sorted(weeks.items()):
        friday = monday + dt.timedelta(days=4)
        wend = int(common.trading_day_end_utc(friday).timestamp())
        w1.append(_agg(group, wend - 5 * 86400, wend))
    return {"H4": _pack(h4), "D1": _pack(d1), "W1": _pack(w1, with_day=False)}


def weekdays(first, n):
    out, d = [], first
    while len(out) < n:
        if d.weekday() < 5:
            out.append(d)
        d += dt.timedelta(days=1)
    return out


def flat_day(o, h, l, c):
    """Six H4 bars: the high in bar 1, the low in bar 2, close c."""
    return [(o, o, o, o), (o, h, o, o), (o, o, l, o), (o, o, o, o), (o, o, o, o), (o, o, o, c)]


def chain(closes, spread=0.0005, start=dt.date(2010, 1, 4)):
    """H4 bars with the given closes (open = previous close, high/low = max/min(open, close) +- spread)."""
    rows = [closes[i:i + 6] for i in range(0, len(closes), 6)]
    days = []
    prev = closes[0]
    for day, cl in zip(weekdays(start, len(rows)), rows):
        bars = []
        for c in cl:
            bars.append((prev, round(max(prev, c) + spread, 6), round(min(prev, c) - spread, 6), c))
            prev = c
        days.append((day, bars))
    return make_bars(days)


class TestIndicators(unittest.TestCase):
    def setUp(self):
        ri.clear_cache()

    def test_rsi_known_sequence(self):
        steps = [+1, +1, +1, -1, +1, +1, -1, +1, +1, -1, +1, +1, -1, +1, +2, 0, 0, 0]   # 10 up, 4 down, then +2
        closes = [1.1]
        for s in steps[:17]:
            closes.append(round(closes[-1] + s * 0.001, 6))
        bars = chain(closes)
        row13 = ri.reference_row(bars, "H4", "EURUSD", 13)
        self.assertTrue(math.isnan(row13["H4_rsi14"]))
        # avg gain 10/14, avg loss 4/14 (in 0.001 units): RS = 2.5, RSI = 100 - 100/3.5
        row14 = ri.reference_row(bars, "H4", "EURUSD", 14)
        self.assertAlmostEqual(row14["H4_rsi14"], ((100 - 100 / 3.5) - 50) / 50, places=9)
        # next change +2: avg gain (10/14*13 + 2)/14, avg loss (4/14*13)/14 -> RS = 158/52 -> RSI = 100*158/210
        row15 = ri.reference_row(bars, "H4", "EURUSD", 15)
        self.assertAlmostEqual(row15["H4_rsi14"], (100 * 158 / 210 - 50) / 50, places=9)

    def test_atr_and_ema_seed(self):
        closes = [1.1] * 14 + [1.102, 1.102]
        bars = chain(closes, spread=0.001)
        # 6b.2: TR undefined on bar 0; TR[1..13] = 0.002 (flat bars, H - L = 0.002); bar 14: open 1.100, close 1.102,
        # high 1.103, low 1.099 -> TR = 0.004; ATR[14] = mean(TR[1..14]) = (13 * 0.002 + 0.004) / 14
        row13 = ri.reference_row(bars, "H4", "EURUSD", 13)
        self.assertTrue(math.isnan(row13["H4_atr_bp"]))
        atr14 = (0.002 * 13 + 0.004) / 14
        row14 = ri.reference_row(bars, "H4", "EURUSD", 14)
        self.assertAlmostEqual(row14["H4_atr_bp"], atr14 / 1.102 * 1e4, places=9)
        # bar 15: open = close = 1.102, high 1.103, low 1.101 -> TR 0.002; Wilder step
        atr15 = (atr14 * 13 + 0.002) / 14
        row15 = ri.reference_row(bars, "H4", "EURUSD", 15)
        self.assertAlmostEqual(row15["H4_atr_bp"], atr15 / 1.102 * 1e4, places=9)
        # 6b.9: close_lag counts bars ending strictly before end[t]: lag1 is bar t-1 (close 1.100), lag14 bar 0
        self.assertAlmostEqual(row14["H4_close_lag1"], 0.002 / atr14, places=9)
        self.assertAlmostEqual(row14["H4_close_lag14"], 0.002 / atr14, places=9)
        self.assertTrue(math.isnan(row14["H4_close_lag15"]))
        self.assertAlmostEqual(row15["H4_close_lag1"], 0.0, places=12)

    def test_ema_recursion(self):
        closes = [round(1.1 + 0.0001 * i, 6) for i in range(21)]
        bars = chain(closes)
        row19 = ri.reference_row(bars, "H4", "EURUSD", 19)
        row20 = ri.reference_row(bars, "H4", "EURUSD", 20)
        seed = sum(closes[:20]) / 20                     # = 1.10095
        self.assertAlmostEqual(seed, 1.10095, places=12)
        # TR[i >= 1] = 0.0001 + 2 * 0.0005 = 0.0011 -> ATR = 0.0011 throughout
        atr = 0.0011
        self.assertAlmostEqual(row19["H4_ema20_dist"], (closes[19] - seed) / atr, places=7)
        self.assertAlmostEqual(row19["H4_sma20_dist"], (closes[19] - seed) / atr, places=7)
        e20 = 2 / 21 * closes[20] + 19 / 21 * seed
        self.assertAlmostEqual(row20["H4_ema20_dist"], (closes[20] - e20) / atr, places=7)
        # Bollinger: closes 0..19 step 0.0001: population stdev = 0.0001 * sqrt((20^2 - 1)/12)
        sd = 0.0001 * math.sqrt((400 - 1) / 12)
        self.assertAlmostEqual(row19["H4_bb_pos"], (closes[19] - seed) / (2 * sd), places=6)
        self.assertAlmostEqual(row19["H4_bb_width"], 4 * sd / atr, places=6)

    def test_slow_stochastic(self):
        # highs rise 0.001 per bar; low = high - 0.002; close = high - 0.0005
        days, bars_list = [], []
        for i in range(18):
            h = round(1.1 + 0.001 * i, 6)
            bars_list.append((round(h - 0.001, 6), h, round(h - 0.002, 6), round(h - 0.0005, 6)))
        for d, k in zip(weekdays(dt.date(2010, 1, 4), 3), range(0, 18, 6)):
            days.append((d, bars_list[k:k + 6]))
        bars = make_bars(days)
        # HH14 = high[i], LL14 = high[i] - 0.013 - 0.002: raw %K = 0.0145/0.015 = 96.67 for every i >= 13
        k = 100 * 0.0145 / 0.015
        self.assertTrue(math.isnan(ri.reference_row(bars, "H4", "EURUSD", 14)["H4_stoch_k"]))
        self.assertAlmostEqual(ri.reference_row(bars, "H4", "EURUSD", 15)["H4_stoch_k"], (k - 50) / 50, places=9)
        self.assertTrue(math.isnan(ri.reference_row(bars, "H4", "EURUSD", 16)["H4_stoch_d"]))
        self.assertAlmostEqual(ri.reference_row(bars, "H4", "EURUSD", 17)["H4_stoch_d"], (k - 50) / 50, places=9)

    def test_cross_age_and_event(self):
        closes = [1.1] * 50 + [1.099] * 5 + [1.101, 1.102, 1.103, 1.104, 1.105]
        bars = chain(closes)
        # bars 0..49 flat: close == SMA50 counts as above (6b.1/6b.4) but no cross has been seen -> NaN
        r49 = ri.reference_row(bars, "H4", "EURUSD", 49)
        self.assertTrue(math.isnan(r49["H4_cross_sma50_age"]))
        self.assertEqual(r49["H4_ev_sma50_cross"], 0.0)
        # bar 50 closes 1.099 < SMA (49 x 1.1 + 1.099)/50: cross down -> -1 on the crossing bar
        r50 = ri.reference_row(bars, "H4", "EURUSD", 50)
        self.assertEqual(r50["H4_cross_sma50_age"], -1.0)
        self.assertEqual(r50["H4_ev_sma50_cross"], -1.0)
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 54)["H4_cross_sma50_age"], -5.0)
        # bar 55 closes 1.101 > SMA = (44 x 1.1 + 5 x 1.099 + 1.101)/50 = 1.09992: cross up
        r55 = ri.reference_row(bars, "H4", "EURUSD", 55)
        self.assertEqual(r55["H4_cross_sma50_age"], 1.0)
        self.assertEqual(r55["H4_ev_sma50_cross"], 1.0)
        r58 = ri.reference_row(bars, "H4", "EURUSD", 58)
        self.assertEqual(r58["H4_cross_sma50_age"], 4.0)
        self.assertEqual(r58["H4_ev_sma50_cross"], 0.0)

    def test_cross_age_cap(self):
        closes = [1.1] * 49 + [round(1.1 - 0.0001 * i, 6) for i in range(1, 160)]
        bars = chain(closes)
        # falling from bar 49 (the first bar with an SMA50): below throughout, no cross seen; count = j - 48
        self.assertTrue(math.isnan(ri.reference_row(bars, "H4", "EURUSD", 147)["H4_cross_sma50_age"]))  # 99
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 148)["H4_cross_sma50_age"], -100.0)
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 200)["H4_cross_sma50_age"], -100.0)

    def test_sma_side_on_grid(self):
        # 20 closes 1.100001, 29 closes 1.100000, then 1.100000: exact SMA50 = 1.100000 + 0.4e-6 (above the close),
        # SMA rounded to the 1e-6 grid = 1.100000 = close -> equal counts as above (6b.1)
        closes = [1.100001] * 20 + [1.1] * 30
        bars = chain(closes, spread=0.0)
        s = ri._series(bars, "H4", 10 ** 6)
        self.assertTrue(s.sides(50, 49, 49)[49])
        try:
            ri.set_conventions(sma_compare="exact")
            self.assertFalse(s.sides(50, 49, 49)[49])
        finally:
            ri.set_conventions()

    def test_donchian_event(self):
        closes = [1.1] * 21 + [1.1006, 1.0990]
        bars = chain(closes)
        # prior 20 bars: high 1.1005, low 1.0995
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["ev_donchian20_break"], 1.0)
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 22)["ev_donchian20_break"], -1.0)
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 20)["ev_donchian20_break"], 0.0)
        # ties (6b.1): a close equal to the prior high counts as above (+1); equal to the prior low is not below (0)
        ri.clear_cache()
        bars = chain([1.1] * 21 + [1.1005])
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["ev_donchian20_break"], 1.0)
        ri.clear_cache()
        bars = chain([1.1] * 21 + [1.0995])
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["ev_donchian20_break"], 0.0)


def level_days(pair_scale=1.0, base=1.1, stub=False):
    """15 identical days (H = base+0.005, L = base-0.005, C = base), then a day H 1.1100 L 1.0980 C 1.1060
    (scaled), optionally a stub day, then a day whose first bar closes at 1.1070."""
    def p(x):
        if pair_scale == 1.0:
            return round(x, 6)
        return round(100.0 + (x - 1.1) * 100.0, 3)        # JPY-like: 1.1 -> 100.000, 1.107 -> 100.700
    days = []
    dates = weekdays(dt.date(2010, 1, 4), 18)
    for d in dates[:15]:
        days.append((d, flat_day(p(base), p(base + 0.005), p(base - 0.005), p(base))))
    days.append((dates[15], flat_day(p(1.1), p(1.11), p(1.098), p(1.106))))
    k = 16
    if stub:
        days.append((dates[16], [(p(1.106), p(1.2), p(1.0), p(1.106), 150), (p(1.106), p(1.106), p(1.106), p(1.106), 150)]))
        k = 17
    days.append((dates[k], [(p(1.106), p(1.108), p(1.105), p(1.107))] + [(p(1.107),) * 4] * 5))
    return make_bars(days)


class TestLevels(unittest.TestCase):
    def setUp(self):
        ri.clear_cache()

    def expected(self, scale=1.0):
        atr = (0.01 * 13 + 0.012) / 14 * scale            # D1 ATR at day 15: seed 0.0100, then TR 0.0120
        P = (1.11 + 1.098 + 1.106) / 3 * scale
        H, L = 1.11 * scale, 1.098 * scale
        price = 1.107 * scale
        return atr, P, H, L, price

    def test_pivots_and_prev_day(self):
        bars = level_days()
        t = 16 * 6                                         # first H4 bar of the day after day 15
        row = ri.reference_row(bars, "H4", "EURUSD", t)
        atr, P, H, L, price = self.expected()
        self.assertAlmostEqual(row["pdh_dist"], (price - H) / atr, places=7)
        self.assertAlmostEqual(row["pdl_dist"], (price - L) / atr, places=7)
        self.assertAlmostEqual(row["pdc_dist"], (price - 1.106) / atr, places=7)
        self.assertAlmostEqual(row["pivot_p"], (price - P) / atr, places=7)
        self.assertAlmostEqual(row["pivot_r1"], (price - (2 * P - L)) / atr, places=7)
        self.assertAlmostEqual(row["pivot_s1"], (price - (2 * P - H)) / atr, places=7)
        self.assertAlmostEqual(row["pivot_r2"], (price - (P + H - L)) / atr, places=7)
        self.assertAlmostEqual(row["pivot_s2"], (price - (P - (H - L))) / atr, places=7)
        self.assertAlmostEqual(row["pivot_p"], (1.107 - 3.314 / 3) / atr, places=7)
        self.assertEqual(row["prev_day_stub"], 0.0)
        # round numbers: 1.1070 -> big figure 1.11 (-0.0030), half figure 1.105 (+0.0020)
        self.assertAlmostEqual(row["round_big_dist"], -0.003 / atr, places=7)
        self.assertAlmostEqual(row["round_half_dist"], 0.002 / atr, places=7)
        # weekly: the week of 2010-01-18 (days 10..14) is complete; H 1.105, L 1.095, C 1.100
        self.assertAlmostEqual(row["pwh_dist"], (price - 1.105) / atr, places=7)
        self.assertAlmostEqual(row["pwc_dist"], (price - 1.1) / atr, places=7)
        # the previous bar (last bar of day 15) sees day 15 as completed too (end == D1 end)
        prev = ri.reference_row(bars, "H4", "EURUSD", t - 1)
        self.assertAlmostEqual(prev["pdh_dist"], (1.106 - H) / atr, places=7)
        # an earlier bar of day 15 sees day 14 (H 1.105, ATR 0.01)
        early = ri.reference_row(bars, "H4", "EURUSD", t - 3)
        self.assertAlmostEqual(early["pdh_dist"], (1.1 - 1.105) / 0.01, places=7)

    def test_round_usdjpy_vs_eurusd(self):
        bars = level_days(pair_scale=100.0)                 # same shapes, prices 100x around 100.000
        t = 16 * 6
        row = ri.reference_row(bars, "H4", "USDJPY", t)
        price = float(bars["H4"]["mid_close"][t])
        self.assertAlmostEqual(price, 100.7, places=9)
        atr = (1.0 * 13 + 1.2) / 14                         # D1 ATR: seed 1.00, then TR 1.20
        # USDJPY: big figure 1.00 -> nearest 101 (-0.30); half figure 0.50 -> nearest 100.5 (+0.20)
        self.assertAlmostEqual(row["round_big_dist"], -0.3 / atr, places=7)
        self.assertAlmostEqual(row["round_half_dist"], 0.2 / atr, places=7)
        # the same prices read as a non-JPY pair use a 0.01 grid: 100.70 is on a big figure; the half figures around it
        # are 100.695 and 100.705, an exact tie -> the upper one (6b.7): -0.005
        ri.clear_cache()
        eur = ri.reference_row(bars, "H4", "EURUSD", t)
        self.assertAlmostEqual(eur["round_big_dist"], 0.0, places=9)
        self.assertAlmostEqual(eur["round_half_dist"] * atr, -0.005, places=9)
        # EURUSD-scale prices: 1.1070 -> big 1.11 (-0.0030), half 1.105 (+0.0020)  (see test_pivots_and_prev_day)

    def test_round_known_values(self):
        self.assertEqual(ri.nearest_figure(1_234_999, 10_000), 1_230_000)
        self.assertEqual(ri.nearest_figure(1_235_000, 10_000), 1_240_000)    # tie goes up
        self.assertEqual(ri.nearest_half(1_234_000, 10_000), 1_235_000)       # half figures only
        self.assertEqual(ri.nearest_half(1_229_000, 10_000), 1_225_000)
        self.assertEqual(ri.nearest_half(1_230_000, 10_000), 1_235_000)       # on a big figure: tie, upper half figure
        self.assertEqual(ri.figure_crossing(1_229_990, 1_230_000, 10_000), 1)   # reaching it counts as above
        self.assertEqual(ri.figure_crossing(1_230_000, 1_230_010, 10_000), 0)   # was already above
        self.assertEqual(ri.figure_crossing(1_230_000, 1_229_990, 10_000), -1)
        self.assertEqual(ri.figure_crossing(1_230_010, 1_230_000, 10_000), 0)   # still at/above
        self.assertEqual(ri.figure_crossing(1_215_000, 1_245_000, 10_000), 1)
        self.assertEqual(ri.crossing(1_229_990, 1_230_000, 1_230_000), 1)
        self.assertEqual(ri.crossing(1_230_000, 1_229_990, 1_230_000), -1)
        self.assertEqual(ri.crossing(1_230_000, 1_230_000, 1_230_000), 0)

    def test_stub_day_skipped(self):
        bars = level_days(stub=True)
        self.assertEqual(int(bars["D1"]["n_minutes"][16]), 300)
        t = 15 * 6 + 6 + 2                                  # first bar after the stub day
        row = ri.reference_row(bars, "H4", "EURUSD", t)
        atr, P, H, L, price = self.expected()
        self.assertEqual(row["prev_day_stub"], 1.0)
        self.assertAlmostEqual(row["pdh_dist"], (price - H) / atr, places=7)   # day 15, not the stub's 1.2
        self.assertAlmostEqual(row["pivot_p"], (price - P) / atr, places=7)
        self.assertAlmostEqual(row["D1_close_lag1"], (price - 1.106) / atr, places=7)   # day 15, stub skipped
        self.assertAlmostEqual(row["D1_close_lag2"], (price - 1.1) / atr, places=7)
        # last bar of day 15 (end == day 15's end): D1 lag1 is day 14, not day 15 itself
        last15 = ri.reference_row(bars, "H4", "EURUSD", 15 * 6 + 5)
        self.assertAlmostEqual(last15["D1_close_lag1"], (1.106 - 1.1) / atr, places=7)
        self.assertAlmostEqual(row["D1_atr_bp"], atr / price * 1e4, places=7)
        # the stub day's last bar ends at 01:00 NY; the stub D1 bar is not complete until 17:00 NY
        row_stub = ri.reference_row(bars, "H4", "EURUSD", t - 1)
        self.assertEqual(row_stub["prev_day_stub"], 0.0)
        self.assertAlmostEqual(row_stub["pdh_dist"], (1.106 - H) / atr, places=7)

    def test_pdh_events(self):
        bars = level_days()
        t = 16 * 6                                         # first H4 bar of day 16; level in force = day 15
        row = ri.reference_row(bars, "H4", "EURUSD", t)
        # previous close 1.1060, close 1.1070, high 1.108 < PDH 1.11: nothing
        self.assertEqual(row["ev_pdh_break"], 0.0)
        self.assertEqual(row["ev_pdh_touch"], 0.0)
        # 17:00 NY row (last bar of day 15, end == day 15's end): pdh_dist uses day 15 (6b.5), events use the level in
        # force during the bar, day 14 (H 1.105): prev close 1.100 < 1.105 <= close 1.106 -> break up
        last = ri.reference_row(bars, "H4", "EURUSD", t - 1)
        atr = (0.01 * 13 + 0.012) / 14
        self.assertAlmostEqual(last["pdh_dist"], (1.106 - 1.11) / atr, places=7)
        self.assertEqual(last["ev_pdh_break"], 1.0)
        # second bar of day 16 (prev close 1.107), level day 15 (PDH 1.11, PDL 1.098)
        cases = [((1.11, 1.105, 1.109), (0, 1)),       # high reaches PDH, close below: touch
                 ((1.1101, 1.105, 1.1101), (1, 0)),    # close above: break
                 ((1.11, 1.105, 1.11), (1, 0))]        # close equal to PDH counts as above: break, not touch
        for (hi, lo, cl), (brk, tch) in cases:
            b = level_days()
            b["H4"]["mid_high"][t + 1], b["H4"]["mid_low"][t + 1], b["H4"]["mid_close"][t + 1] = hi, lo, cl
            ri.clear_cache()
            r = ri.reference_row(b, "H4", "EURUSD", t + 1)
            self.assertEqual((r["ev_pdh_break"], r["ev_pdh_touch"]), (brk, tch), (hi, lo, cl))
        # PDL touch: prev >= PDL, low < PDL, close >= PDL; low == PDL is not a touch (equal counts as above)
        for lo, tch in ((1.0979, 1), (1.098, 0)):
            b = level_days()
            b["H4"]["mid_high"][t + 1], b["H4"]["mid_low"][t + 1], b["H4"]["mid_close"][t + 1] = 1.107, lo, 1.098
            ri.clear_cache()
            r = ri.reference_row(b, "H4", "EURUSD", t + 1)
            self.assertEqual((r["ev_pdl_break"], r["ev_pdl_touch"]), (0, tch), lo)


class TestConventionSwitch(unittest.TestCase):
    def tearDown(self):
        ri.set_conventions()
        ri.clear_cache()

    def test_alternatives(self):
        bars = chain([1.1] * 21 + [1.1005])
        ri.clear_cache()
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["ev_donchian20_break"], 1.0)
        ri.set_conventions(donchian_ties="strict")
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["ev_donchian20_break"], 0.0)
        ri.set_conventions(close_lag="inclusive")
        self.assertEqual(ri.reference_row(bars, "H4", "EURUSD", 21)["H4_close_lag1"], 0.0)
        with self.assertRaises(KeyError):
            ri.set_conventions(no_such_convention=1)


class TestClock(unittest.TestCase):
    def test_clock_winter_and_summer(self):
        # Monday 2010-01-04 01:00 NY (06:00 UTC): 8 h into the trading day, 8 h into the 120 h week
        a = int(dt.datetime(2010, 1, 4, 6, 0, tzinfo=dt.timezone.utc).timestamp())
        c = ri.clock(a)
        self.assertAlmostEqual(c["clk_day_sin"], math.sin(2 * math.pi / 3), places=12)
        self.assertAlmostEqual(c["clk_day2_cos"], math.cos(4 * math.pi / 3), places=12)
        self.assertAlmostEqual(c["clk_week_sin"], math.sin(2 * math.pi * 8 / 120), places=12)
        # summer: 17:15 NY on a Sunday = 21:15 UTC
        a = int(dt.datetime(2010, 7, 11, 21, 15, tzinfo=dt.timezone.utc).timestamp())
        c = ri.clock(a)
        self.assertAlmostEqual(c["clk_day_sin"], math.sin(2 * math.pi * 0.25 / 24), places=12)
        self.assertAlmostEqual(c["clk_week_cos"], math.cos(2 * math.pi * 0.25 / 120), places=12)
        # Friday 17:00 NY closes the week: fraction 1 == 0
        a = int(dt.datetime(2010, 7, 16, 21, 0, tzinfo=dt.timezone.utc).timestamp())
        c = ri.clock(a)
        self.assertAlmostEqual(c["clk_week_sin"], 0.0, places=12)
        self.assertAlmostEqual(c["clk_week_cos"], 1.0, places=12)
        self.assertAlmostEqual(c["clk_day_cos"], 1.0, places=12)


class TestNoLookahead(unittest.TestCase):
    def test_truncated_bars_give_same_rows(self):
        rng = random.Random(3)
        days, prev = [], 1.3
        for d in weekdays(dt.date(2010, 1, 4), 60):
            bl = []
            for _ in range(6):
                c = round(prev + rng.randint(-30, 30) * 1e-4, 6)
                bl.append((prev, round(max(prev, c) + rng.randint(0, 10) * 1e-4, 6),
                           round(min(prev, c) - rng.randint(0, 10) * 1e-4, 6), c))
                prev = c
            days.append((d, bl))
        full = make_bars(days)
        ends = full["H4"]["end"]
        for t in (100, 203, 255, 300, 359):
            cut = int(ends[t])
            trunc = {tf: {k: v[full[tf]["end"] <= cut] for k, v in b.items()} for tf, b in full.items()}
            ri.clear_cache()
            a = ri.reference_row(full, "H4", "EURUSD", t)
            ri.clear_cache()
            b = ri.reference_row(trunc, "H4", "EURUSD", t)
            for k in a:
                self.assertTrue(a[k] == b[k] or (math.isnan(a[k]) and math.isnan(b[k])), (t, k, a[k], b[k]))

    def test_column_list(self):
        cols = ri.spec_columns("M15")
        self.assertEqual(len(cols), len(set(cols)))
        self.assertIn("M15_close_lag32", cols)
        self.assertIn("D1_close_lag20", cols)
        self.assertNotIn("D1_close_lag21", cols)
        self.assertNotIn("H1_close_lag25", ri.spec_columns("H1"))
        self.assertNotIn("M15_rsi14", ri.spec_columns("H1"))
        bars = level_days()
        row = ri.reference_row(bars, "H4", "EURUSD", 50)
        self.assertEqual(set(row), set(ri.spec_columns("H4")) - {"quality_excluded"})
        self.assertIn("quality_excluded", ri.spec_columns("H4"))


if __name__ == "__main__":
    unittest.main()
