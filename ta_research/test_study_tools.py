"""Tests of the Stage 1 study tools on synthetic data only.  Run from ta_research/:  python3 -m unittest -v

The wider runs whose numbers are quoted in PREREG_TA_SIGNALS.md section 10 (false-positive rates over many seeds,
planted-effect recovery at several strengths) are `python3 signal_study.py validate ...` and
`python3 drift_forms.py`; these unit tests are the fast version of the same checks.
"""

import argparse
import math
import sys
import unittest
from pathlib import Path
from unittest import mock

import numpy as np

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import common  # noqa: E402
import drift_forms as dfm  # noqa: E402
import events  # noqa: E402
import rule_walkforward as rw  # noqa: E402
import signal_study as ss  # noqa: E402
import synthetic  # noqa: E402

PAIRS = ("EURUSD", "USDJPY")
WARM, START, END = "2007-07-01", "2008-01-01", "2009-06-30"
PER = ss.synth_periods(START, END)


def _corrupt_after(bars, T, seed=7):
    """replace every bar not complete at T with nonsense (the future must not matter)."""
    rng = np.random.default_rng(seed)
    out = {}
    for tf, b in bars.items():
        c = {k: v.copy() for k, v in b.items()}
        late = c["end"] > T
        k = int(late.sum())
        if k:
            base = rng.uniform(0.5, 150.0, k)
            c["mid_close"][late] = base
            c["mid_open"][late] = base * 1.01
            c["mid_high"][late] = base * 1.05
            c["mid_low"][late] = base * 0.95
            c["bid_close"][late] = base * 0.99
            c["ask_close"][late] = base * 1.01
        out[tf] = c
    return out


class TestDateGuard(unittest.TestCase):
    def test_refuses_past_holdout(self):
        bars = synthetic.generate(("EURUSD",), "2014-11-01", "2014-12-31", seed=1)
        with self.assertRaises(common.DateGuardError):
            ss.guard_bars(bars["EURUSD"], "2018-01-02")
        with self.assertRaises(common.DateGuardError):
            ss.run_study(bars, through="2018-01-02", periods={"discovery": ("2014-11-01", "2014-11-30"),
                                                              "confirmation": ("2014-12-01", "2014-12-31")})
        with self.assertRaises(common.DateGuardError):
            rw.walkforward(bars, "H1", through="2018-01-02", eval_period=("2014-12-01", "2014-12-31"))

    def test_tools_call_allowed_through(self):
        bars = synthetic.generate(("EURUSD",), "2014-09-01", "2014-12-31", seed=2)
        with mock.patch.object(common, "allowed_through", wraps=common.allowed_through) as m:
            ss.run_study(bars, periods={"discovery": ("2014-10-01", "2014-11-15"),
                                        "confirmation": ("2014-11-16", "2014-12-31")}, decision_tfs=["H4"])
            self.assertGreaterEqual(m.call_count, 1)
            n = m.call_count
            rw.walkforward(bars, "H4", eval_period=("2014-11-01", "2014-12-31"), boot=10)
            self.assertGreater(m.call_count, n)

    def test_rows_past_guard_are_dropped_and_cannot_matter(self):
        """Data running into 2015, poisoned after 2014-12-31: results equal those on data cut at the guard."""
        bars = synthetic.generate(("EURUSD",), "2014-05-01", "2015-02-27", seed=3)
        limit = common.guard_limit_utc_seconds(common.allowed_through(None))
        poisoned = {"EURUSD": _corrupt_after(bars["EURUSD"], limit)}
        cut = {"EURUSD": {tf: {k: v[b["end"] <= limit] for k, v in b.items()} for tf, b in bars["EURUSD"].items()}}
        per = {"discovery": ("2014-09-01", "2014-10-31"), "confirmation": ("2014-11-01", "2014-12-31")}
        a, info = ss.run_study(poisoned, periods=per, decision_tfs=["H1"])
        b, _ = ss.run_study(cut, periods=per, decision_tfs=["H1"])
        self.assertGreater(info["guard"]["EURUSD"]["dropped_rows"]["M15"], 0)
        self.assertEqual(info["guard"]["EURUSD"]["through"], "2014-12-31")
        np.testing.assert_array_equal(a.drop(columns="flag").to_numpy(dtype=object).astype(str),
                                      b.drop(columns="flag").to_numpy(dtype=object).astype(str))
        g, _ = ss.guard_bars(poisoned["EURUSD"])
        for tf, arr in g.items():
            self.assertTrue(np.all(arr["end"] <= limit), tf)
        w1 = rw.walkforward(poisoned, "H1", eval_period=("2014-09-01", "2014-12-31"), boot=10)
        w2 = rw.walkforward(cut, "H1", eval_period=("2014-09-01", "2014-12-31"), boot=10)
        np.testing.assert_allclose(w1["strategies"]["net_bp"], w2["strategies"]["net_bp"])

    def test_real_run_refused_until_prereg_approved(self):
        ok, why = ss.prereg_approved()
        self.assertFalse(ok, "the pre-registration must not read as approved while it is a draft")
        ns = argparse.Namespace(through=None, out=None)
        with mock.patch.object(ss, "load_bars", side_effect=AssertionError("real data must not be opened")):
            with self.assertRaises(ss.NotApprovedError):
                ss.cmd_run(ns)
            with self.assertRaises(ss.NotApprovedError):
                rw.cmd_run(ns)


class TestCosts(unittest.TestCase):
    """Costs are charged exactly once per entry and once per exit."""

    def flat_market(self, n=50, mid=1.25, spread=0.0002):
        m = np.full(n, mid)
        return m, m - spread / 2, m + spread / 2

    def test_bar_pnl_round_trips(self):
        mid, bid, ask = self.flat_market()
        rt = 0.0002 / 1.25 * 1e4 + 2 * common.COMMISSION_BP_PER_SIDE      # two half-spreads + 2 x 0.2 bp
        u = np.zeros(50, np.int8)
        u[10:30] = 1                                                        # one long held 20 bars
        g, c, tr = rw.bar_pnl(u, mid, bid, ask)
        self.assertAlmostEqual(c.sum(), rt, places=10)
        self.assertEqual(len(tr["entry"]), 1)
        u[30:40] = -1                                                       # flip then exit: two round trips
        g, c, tr = rw.bar_pnl(u, mid, bid, ask)
        self.assertAlmostEqual(c.sum(), 2 * rt, places=10)
        self.assertEqual(list(tr["entry"]), [10, 30])
        self.assertEqual(list(tr["exit"]), [30, 40])
        u[:] = 1                                                            # open at bar 0, forced exit at the end
        g, c, tr = rw.bar_pnl(u, mid, bid, ask)
        self.assertAlmostEqual(c.sum(), rt, places=10)
        self.assertAlmostEqual(g.sum(), 0.0, places=12)

    def test_bar_pnl_equals_spec_fills(self):
        rng = np.random.default_rng(0)
        n = 400
        mid = 1.3 * np.exp(np.cumsum(rng.standard_normal(n) * 1e-3))
        spr = 0.0001 * (1 + rng.random(n))
        bid, ask = mid - spr / 2, mid + spr / 2
        u = rng.choice([-1, 0, 1], n, p=[0.3, 0.3, 0.4]).astype(np.int8)
        u = np.repeat(u[::5], 5)[:n]
        g, c, tr = rw.bar_pnl(u, mid, bid, ask)
        total = 0.0
        for e, x, s in zip(tr["entry"], tr["exit"], tr["sign"]):
            entry, exit_ = (ask[e], bid[x]) if s > 0 else (bid[e], ask[x])
            total += s * (exit_ - entry) / mid[e] * 1e4 - 2 * common.COMMISSION_BP_PER_SIDE
        self.assertAlmostEqual((g - c).sum(), total, places=8)
        self.assertAlmostEqual((tr["gross"] - tr["cost"]).sum(), total, places=8)

    def test_event_outcomes_charge_one_round_trip(self):
        mid, bid, ask = self.flat_market(40)
        x = {"mid_close": mid, "bid_close": bid, "ask_close": ask}
        rt = 0.0002 / 1.25 * 1e4 + 2 * common.COMMISSION_BP_PER_SIDE
        for h in ss.HORIZONS:
            g, cl, cs, ex = ss.outcomes(x, h)
            np.testing.assert_array_equal(ex[:-h], np.arange(40 - h) + h)
            np.testing.assert_allclose(g[:-h], 0.0)
            np.testing.assert_allclose(cl[:-h], rt)
            np.testing.assert_allclose(cs[:-h], rt)

    def test_walkforward_money_equals_an_independent_fill_by_fill_count(self):
        """The rolling strategy switches variant at round boundaries; every switch must be paid, once."""
        bars = synthetic.generate(("EURUSD",), WARM, "2008-06-30", seed=4)
        res = rw.walkforward(bars, "H1", eval_period=("2008-01-01", "2008-06-30"), boot=10)
        x = bars["EURUSD"]["H1"]
        mid, bid, ask = x["mid_close"], x["bid_close"], x["ask_close"]
        s = res["strategies"].set_index(["strategy", "pair"])
        for name in ("rolling_W5", "rolling_W20", "static:pd_breakout:follow"):
            u = res["per_pair"]["EURUSD"]["strat"][name]["u"].astype(int)
            u[-1] = 0
            pos, entry_i, total, fills = 0, None, 0.0, 0
            for t in range(len(u)):
                if u[t] != pos:
                    if pos != 0:                        # exit the old position at this bar's close
                        fill = bid[t] if pos > 0 else ask[t]
                        efill = ask[entry_i] if pos > 0 else bid[entry_i]
                        total += pos * (fill - efill) / mid[entry_i] * 1e4 - 2 * common.COMMISSION_BP_PER_SIDE
                        fills += 1
                    if u[t] != 0:
                        entry_i = t
                        fills += 1
                    pos = u[t]
            self.assertAlmostEqual(s.loc[(name, "EURUSD"), "net_bp"], total, places=6)
            self.assertEqual(s.loc[(name, "EURUSD"), "trades"] * 2, fills)


class TestNull(unittest.TestCase):
    """On random walks no tool shows an edge."""

    @classmethod
    def setUpClass(cls):
        cls.t, cls.td, cls.decisions = [], [], []
        for seed in (11, 12):
            bars = synthetic.generate(PAIRS, WARM, END, seed=seed)
            df, _ = ss.run_study(bars, END, periods=PER)
            f = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["n_std"] >= 50)]
            cls.t.append(f["t_std"].to_numpy())
            cls.td.append(f["t_diff"].to_numpy())
            cls.decisions.append(ss.gate(df)["decision"])
        cls.wf = rw.walkforward(synthetic.generate(PAIRS, WARM, END, seed=13), "H1", END, (START, END), boot=100)

    def test_study_t_is_noise(self):
        for arr in (np.concatenate(self.t), np.concatenate(self.td)):
            arr = arr[np.isfinite(arr)]
            self.assertLess(abs(arr.mean()), 0.35)
            self.assertTrue(0.7 < arr.std() < 1.35, arr.std())
            self.assertLess(np.mean(np.abs(arr) >= 3), 0.03)

    def test_gate_stops(self):
        self.assertEqual(self.decisions, ["STOP", "STOP"])

    def test_walkforward_has_no_edge(self):
        s = self.wf["strategies"]
        b = s[s["pair"] == "BASKET"]
        self.assertTrue(np.all(b["t"].abs() < 4), b[["strategy", "t"]])
        rolling = b[b["strategy"].str.startswith("rolling")]
        self.assertTrue(np.all(rolling["gross_bp"] - rolling["cost_bp"] < rolling["gross_bp"]))
        c = self.wf["corr"]
        c = c[(c["rule"] == "ALL_RULES") & (c["measure"] == "gross")]
        self.assertTrue(np.all(c["p_perm"] > 0.001), c)

    def test_drift_forms_constant_position(self):
        bars = synthetic.generate(("EURUSD",), WARM, END, seed=5, drift_bp_day=5.0)
        b = ss.guard_bars(bars["EURUSD"], END)[0]
        n = len(b["H1"]["mid_close"])
        sc, _ = dfm.window_scores(np.ones(n, np.int8), b["H1"], (START, END), true_mu=5.0 / 24)
        rt = sc["none"]
        self.assertGreater(rt.mean(), 0)                                   # plain money keeps the drift
        cost = sc["span"].sum()
        self.assertLess(abs(cost), 5.0)                                    # span form: zero except entry/exit cost
        self.assertTrue(np.allclose(sc["in_window"][1:-1], 0.0, atol=1e-8))


class TestPlanted(unittest.TestCase):
    def test_pdh_break_continuation_found(self):
        bars = synthetic.generate(PAIRS, WARM, END, seed=21, effect="pdh_break", strength_bp=6.0)
        df, _ = ss.run_study(bars, END, periods=PER, decision_tfs=["M15", "H1"])
        r = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["tf"] == "M15") &
               (df["signal"] == "pdh_break") & (df["horizon"] == 4)].iloc[0]
        self.assertGreater(r["t_std"], 3)
        self.assertGreater(r["t_diff"], 3)
        self.assertTrue(1.5 < r["mean_std"] < 6.5, r["mean_std"])     # later crosses cut some planted moves short
        p = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["tf"] == "M15") &
               (df["signal"] == "round_big_cross") & (df["horizon"] == 4)].iloc[0]
        self.assertLess(abs(p["t_std"]), 3.5)

    def test_round_bounce_found(self):
        bars = synthetic.generate(PAIRS, WARM, END, seed=22, effect="round_bounce", strength_bp=3.0)
        df, _ = ss.run_study(bars, END, periods=PER, decision_tfs=["M15"])
        r = df[(df["pair"] == "ALL") & (df["period"] == "full") & (df["signal"] == "round_big_touch") &
               (df["horizon"] == 4)].iloc[0]
        self.assertLess(r["t_std"], -3)
        self.assertLess(r["t_diff"], -3)

    def test_walkforward_keeps_a_real_trend(self):
        bars = synthetic.generate(PAIRS, WARM, END, seed=23, trend_bp_day=40.0, trend_days=20)
        res = rw.walkforward(bars, "H1", END, (START, END), boot=100)
        s = res["strategies"]
        b = s[s["pair"] == "BASKET"].set_index("strategy")
        self.assertGreater(b.loc["static:ma_50_200:follow", "t"], 2)
        self.assertGreater(b.loc["rolling_W126", "net_bp"], 0)
        ch = res["rounds"]
        ch = ch[ch["W"] == "126"]["chosen"]
        self.assertGreater(ch.isin(["ma_50_200:follow", "donchian20:follow"]).mean(), 0.8)


class TestLookAhead(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.bars = synthetic.generate(PAIRS, WARM, "2008-12-31", seed=31)

    def test_events_do_not_change_when_the_future_changes(self):
        b = self.bars["EURUSD"]
        for X in common.DECISION_TFS:
            x = b[X]
            T = int(x["end"][len(x["end"]) * 2 // 3])
            fut = _corrupt_after(b, T)
            A = events.signal_catalogue(b, X, "EURUSD")
            B = events.signal_catalogue(fut, X, "EURUSD")
            known = x["end"] <= T
            for sa, sb in zip(A, B):
                for ea, eb in zip([sa["std"]] + sa["placebo"], [sb["std"]] + sb["placebo"]):
                    np.testing.assert_array_equal(ea[known], eb[known], err_msg=f"{X} {sa['name']}")

    def test_canary_is_flagged(self):
        c = ss.lookahead_canary(self.bars, "2008-12-31", {"discovery": ("2008-01-01", "2008-06-30"),
                                                          "confirmation": ("2008-07-01", "2008-12-31")})
        can = c[c["signal"] == "CANARY_future_pdh_break"].iloc[0]
        rnd = c[c["signal"] == "CONTROL_random_events"].iloc[0]
        self.assertEqual(can["flag"], "IMPOSSIBLE?")
        self.assertGreater(can["t_std"], 8)
        self.assertLess(abs(rnd["t_std"]), 4)

    def test_crosscheck_catches_a_feature_shifted_into_the_future(self):
        b = self.bars["EURUSD"]
        for X in ("M15", "H1"):
            f = synthetic.make_features(b, X, "EURUSD")
            ok = ss.crosscheck(b, f, X, "EURUSD", "2008-12-31")
            self.assertTrue((ok["status"] == "ok").all())
            bad = dict(f)
            bad["ev_pdh_break"] = np.r_[f["ev_pdh_break"][1:], 0].astype(np.int8)
            with self.assertRaises(ss.CrossCheckError):
                ss.crosscheck(b, bad, X, "EURUSD", "2008-12-31")
            bad = dict(f)
            bad["H1_ev_sma50_cross"] = np.r_[f["H1_ev_sma50_cross"][1:], 0].astype(np.int8)
            with self.assertRaises(ss.CrossCheckError):
                ss.crosscheck(b, bad, X, "EURUSD", "2008-12-31")

    def test_shifted_signal_gives_impossible_result(self):
        """a 'feature' that knows the next bar: the study reports it as impossible."""
        def extra(bars, X, pair):
            s = events.spec_event_columns(bars, X, pair)["ev_round_big_cross"]
            return [{"name": "future_round_cross", "family": "trap", "chart": X,
                     "std": np.r_[s[1:], 0].astype(np.int8), "placebo": [s]}]
        df, _ = ss.run_study(self.bars, "2008-12-31", horizons=(1,), periods={"discovery": ("2008-01-01", "2008-06-30"),
                             "confirmation": ("2008-07-01", "2008-12-31")}, decision_tfs=["M15"], extra_signals=extra)
        r = df[(df["signal"] == "future_round_cross") & (df["pair"] == "ALL") & (df["period"] == "full")].iloc[0]
        self.assertEqual(r["flag"], "IMPOSSIBLE?")

    def test_walkforward_choice_ignores_the_future(self):
        res = rw.walkforward(self.bars, "H1", "2008-12-31", ("2008-01-01", "2008-12-31"), boot=10)
        r = res["rounds"]
        starts = sorted(r["round_start"].unique())
        k = starts[len(starts) // 2]
        x = self.bars["EURUSD"]["H1"]
        dn = ss.day_number(x["trading_day"])
        first_bar = int(np.flatnonzero(dn == ss.to_daynum(str(k)))[0])
        T = int(x["end"][first_bar])                                    # the decision time of that round
        fut = {p: _corrupt_after(b, T) for p, b in self.bars.items()}
        res2 = rw.walkforward(fut, "H1", "2008-12-31", ("2008-01-01", "2008-12-31"), boot=10)
        a = r[r["round_start"] <= k].sort_values(["pair", "W", "round_start"])
        b = res2["rounds"]
        b = b[b["round_start"] <= k].sort_values(["pair", "W", "round_start"])
        self.assertEqual(list(a["chosen"]), list(b["chosen"]))
        np.testing.assert_allclose(a["train_net"].to_numpy(), b["train_net"].to_numpy())


class TestQualityExcluded(unittest.TestCase):
    """SPEC 5a: no event and no position on excluded rows; positions closed at the last row before a stretch."""

    @classmethod
    def setUpClass(cls):
        import quality
        cls.bars = synthetic.generate(PAIRS, WARM, "2008-09-30", seed=41, stale=[("2008-06-10 12:00", 30)])
        cls.iv = {p: quality.stale_intervals(b["H1"]) for p, b in cls.bars.items()}

    def test_planted_stretch_is_found(self):
        for p, iv in self.iv.items():
            self.assertEqual(len(iv), 1, iv)
            s, e, _ = iv[0]
            self.assertLessEqual(abs(s - int(np.datetime64("2008-06-10T12:00", "s").astype(np.int64))), 3600)

    def test_event_exits_stop_before_the_stretch(self):
        x = self.bars["EURUSD"]["M15"]
        excl = ss.excluded_rows(x, self.iv["EURUSD"])
        self.assertTrue(excl.any())
        first = int(np.flatnonzero(excl)[0])
        for h in ss.HORIZONS:
            g, cl, cs, ex = ss.outcomes(x, h, excl)
            self.assertTrue(np.all(ex[excl] == -1))                    # nothing opened on an excluded row
            self.assertEqual(ex[first - 1], -1)                        # the next row is excluded: cannot hold
            if h > 1:
                self.assertEqual(ex[first - 2], first - 1)             # closed at the last row before the stretch
            ok = ex >= 0
            self.assertFalse(np.any(excl[ex[ok]]))                     # no exit inside a stretch
            seg = [(t, e) for t, e in zip(np.flatnonzero(ok), ex[ok])]
            self.assertFalse(any(excl[t:e + 1].any() for t, e in seg[::50]))

    def test_study_skips_excluded_events(self):
        def every_bar(bars, X, pair):
            one = np.ones(len(bars[X]["end"]), np.int8)
            return [{"name": "every_bar", "family": "test", "chart": X, "std": one, "placebo": [one]}]
        per = {"discovery": ("2008-01-01", "2008-05-31"), "confirmation": ("2008-06-01", "2008-09-30")}
        a, _ = ss.run_study(self.bars, "2008-09-30", horizons=(1,), periods=per, decision_tfs=["H1"],
                            extra_signals=every_bar, lopo=False)
        b, _ = ss.run_study(self.bars, "2008-09-30", horizons=(1,), periods=per, decision_tfs=["H1"],
                            extra_signals=every_bar, lopo=False, quality_intervals=self.iv)
        sel = lambda d: d[(d["signal"] == "every_bar") & (d["pair"] == "EURUSD") & (d["period"] == "full")].iloc[0]
        x = self.bars["EURUSD"]["H1"]
        excl = ss.excluded_rows(x, self.iv["EURUSD"])
        dn = ss.day_number(x["trading_day"])
        inside = (dn >= ss.to_daynum("2008-01-01")) & (dn <= ss.to_daynum("2008-09-30"))
        lost = int((rw.no_trade_mask(excl) & inside).sum())
        self.assertEqual(sel(a)["n_std"] - sel(b)["n_std"], lost)

    def test_rules_hold_nothing_on_excluded_rows(self):
        res = rw.walkforward(self.bars, "H1", "2008-09-30", ("2008-02-01", "2008-09-30"), boot=10,
                             quality_intervals=self.iv)
        x = self.bars["EURUSD"]["H1"]
        nt = rw.no_trade_mask(ss.excluded_rows(x, self.iv["EURUSD"]))
        for name, st in res["per_pair"]["EURUSD"]["strat"].items():
            self.assertTrue(np.all(st["u"][nt] == 0), name)
        u = res["per_pair"]["EURUSD"]["strat"]["static:ma_50_200:follow"]["u"]
        self.assertGreater(np.mean(u[~nt] != 0), 0.5)                  # the rule trades elsewhere

    def test_feature_column_is_crosschecked(self):
        b = self.bars["EURUSD"]
        f = synthetic.make_features(b, "H1", "EURUSD", self.iv["EURUSD"])
        self.assertTrue(f["quality_excluded"].any())
        ss.crosscheck(b, f, "H1", "EURUSD", "2008-09-30", self.iv["EURUSD"])
        f2 = dict(f, quality_excluded=np.zeros_like(f["quality_excluded"]))
        with self.assertRaises(ss.CrossCheckError):
            ss.crosscheck(b, f2, "H1", "EURUSD", "2008-09-30", self.iv["EURUSD"])


class TestAgainstFeaturesPy(unittest.TestCase):
    def test_event_definitions_match_features_py(self):
        try:
            import features
        except Exception as e:                                         # features.py is another agent's file
            self.skipTest(f"features.py not importable: {e}")
        import quality
        bars = synthetic.generate(PAIRS, "2007-01-01", "2008-06-30", seed=51, stale=[("2008-03-11 12:00", 30)])
        for snap in (False, True):            # True: prices on whole pips, so closes often sit exactly on levels
            for p, b0 in bars.items():
                pip = 0.01 if p in common.JPY_PAIRS else 1e-4
                b = {tf: {k: (np.round(v / pip) * pip if snap and k in events.MID_FIELDS else v) for k, v in d.items()}
                     for tf, d in b0.items()}
                iv = quality.stale_intervals(b["H1"])
                for X in common.DECISION_TFS:
                    f = features.build_features(b, X, p, quality_intervals=iv)
                    out = ss.crosscheck(b, f, X, p, "2008-06-30", iv)
                    self.assertTrue((out["disagree"] == 0).all(), out)
                    self.assertIn("quality_excluded", set(out["column"]))


class TestStatistics(unittest.TestCase):
    def test_day_cluster_t_equals_plain_t_with_one_event_a_day(self):
        rng = np.random.default_rng(1)
        x = rng.standard_normal(300) + 0.1
        day = np.arange(300)
        s = ss.tstats(x, day, day // 5)
        self.assertAlmostEqual(s["t_day"], x.mean() / (x.std(ddof=1) / math.sqrt(300)), places=10)

    def test_no_result_conditions(self):
        bars = synthetic.generate(("EURUSD",), WARM, "2008-06-30", seed=8)
        per = {"discovery": ("2008-01-01", "2008-03-31"), "confirmation": ("2008-04-01", "2008-06-30")}
        df, _ = ss.run_study(bars, "2008-06-30", periods=per, decision_tfs=["H1"])
        c = ss.lookahead_canary(bars, "2008-06-30", per)
        self.assertEqual(ss.no_result_reasons(df, c, pairs=("EURUSD",)), [])
        self.assertTrue(ss.no_result_reasons(df, c, pairs=("EURUSD", "GBPUSD")))
        c2 = c.copy()
        c2["t_std"] = 1.0
        self.assertTrue(ss.no_result_reasons(df, c2, pairs=("EURUSD",)))
        mid = {"EURUSD": {tf: dict(b, bid_close=b["mid_close"], ask_close=b["mid_close"]) for tf, b in bars["EURUSD"].items()}}
        df3, _ = ss.run_study(mid, "2008-06-30", periods=per, decision_tfs=["H1"])
        self.assertTrue(ss.no_result_reasons(df3, c, pairs=("EURUSD",)))

    def test_thin_never_overlaps(self):
        idx = np.array([0, 1, 2, 5, 6, 9, 20, 21, 35])
        k = ss.thin(idx, 4)
        self.assertEqual(list(k), [0, 5, 9, 20, 35])
        self.assertTrue(np.all(np.diff(k) >= 4))

    def test_holm_gate_logic(self):
        import pandas as pd
        rows = []
        for per, t, td in (("discovery", 3.0, 3.0), ("confirmation", 2.5, 2.5)):
            rows.append({"period": per, "pair": "ALL", "tf": "H1", "signal": "s", "horizon": 4, "n_std": 500,
                         "days_std": 300, "mean_std": 1.0, "t_std": t, "diff": 0.5, "t_diff": td, "cost_cont": 1.5,
                         "cost_rev": 1.5, "lopo_min_t": 2.1, "mean_plc1": 0.4, "mean_plc2": 0.6, "flag": ""})
        g = ss.gate(pd.DataFrame(rows))
        self.assertEqual(g["decision"], "GO")                   # ratio 0.67 >= 0.5, one candidate, p 0.006 <= 0.05
        rows[1]["cost_cont"] = 3.0                              # ratio 0.33
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "OWNER DECIDES")
        rows[1]["cost_cont"] = 5.0                              # ratio 0.2
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "STOP")
        rows[1]["cost_cont"] = 1.5
        rows[1]["lopo_min_t"] = 1.0                             # carried by one pair
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "OWNER DECIDES")
        rows[1]["lopo_min_t"] = 2.1
        rows[1]["mean_plc2"] = 1.2                              # standard lies between its two placebos
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "OWNER DECIDES")
        rows[1]["mean_plc2"] = 0.6
        rows[1]["flag"] = "IMPOSSIBLE?"
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "OWNER DECIDES")
        rows[1]["flag"] = ""
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "GO")
        rows[1]["t_diff"] = 1.0                                 # not confirmed against placebo
        self.assertEqual(ss.gate(pd.DataFrame(rows))["decision"], "STOP")


if __name__ == "__main__":
    unittest.main()
