# Pre-registration: do technical-analysis signals show a reaction worth learning? (Stage 1 gate)

Status: APPROVED by the owner on 2026-11-16, with every proposal in section 12 accepted as written. Nothing below may
change after the run.

Written 2026-11-16. No return, price change or rule result of any pair has been computed on real data for this
work. Every number in section 10 comes from synthetic data (`synthetic.py`). The tools that run the test are
`signal_study.py` (the gate) and `rule_walkforward.py` (side questions), with `events.py` for the event definitions.
They refuse to run on real data unless this file is committed, unchanged since its commit, and carries the line
`Status: APPROVED` near the top. When approved, the commit hash of this file and the tools is the registration.

## 1. The question

Many traders act on the same standard signals: moving averages, RSI, Bollinger bands, channel breakouts, the prior
day's and week's high and low, floor pivots, round numbers. If their orders cluster, price may react predictably
when those signals fire. Stage 2 and 3 will try to combine and filter these signals with evolution.

Stage 1 asks one question, and gates on it: **is there anything to learn?** That is, do signal events show a
reaction in the mid price, before cost, that is larger than the reaction to the same event logic on meaningless
levels (the placebo), confirmed on data the search did not use, and large enough against the measured cost that a
combined or filtered strategy could plausibly pay?

The gate is deliberately **not** "does a raw rule make money after cost". Evolution is meant to combine and filter
signals. A raw signal that loses after cost but carries a real, sizeable reaction is something to learn from.

## 2. Data

- Pairs: EUR/USD, GBP/USD, USD/JPY, USD/CHF, USD/CAD, NZD/USD (SPEC section 2; AUD/USD excluded).
- Bars and features as SPEC sections 5 and 6, built from Dukascopy one-minute bid and ask.
- **Events counted:** entry trading days 2008-01-01 to 2014-12-31. Bars from 2003 to 2007 are read only as history
  for the indicators (SMA200 on D1 needs about ten months). No result for 2003-2007 is computed or reported.
- **Discovery period:** 2008-01-01 to 2011-12-31. **Confirmation period:** 2012-01-01 to 2014-12-31.
  An event belongs to a period if its entry day and its exit day are both in it.
- Date guard: `common.allowed_through`, default 2014-12-31. Nothing later is read. The holdout (2015 on, and
  2018 on) is not touched.
- Stale-quote periods (SPEC 5a, `quality.py`): no event is counted on an excluded row, no position is opened on
  one, and a position whose horizon runs into an excluded stretch is closed at the last row before it (its
  holding is shortened). The run stops if a pair's quality file is missing.

## 3. Signals

Each signal is evaluated on each decision timeframe X in {M15, H1, H4}. Definitions are in `events.py`; on synthetic
bars they agree event for event with `features.py` for every event column the feature table has (section 10).

An event on bar t has a direction d: the direction of the move that made it (a break upward is +1, a touch of a
level from below is +1). **Continuation** is a position in direction d. **Reversal** is the opposite position.

| family | signals | on which bars |
|---|---|---|
| moving averages | close crosses SMA50; close crosses SMA200; SMA50 crosses SMA200 | every chart TF of X (M15: M15 H1 H4 D1; H1: H1 H4 D1; H4: H4 D1). The event is flagged on the first X bar at whose end the chart bar is complete. |
| RSI(14), Wilder | `rsi_enter_30_70`: crosses above 70 (+1) or below 30 (-1); `rsi_exit_30_70`: crosses back below 70 (-1) or above 30 (+1) | X |
| Bollinger(20, 2 sd) | close moves outside the band (+1 above, -1 below) | X |
| Donchian(20) | close above the prior 20-bar high (+1) or below the prior 20-bar low (-1), as SPEC `ev_donchian20_break` | X |
| prior day | high break, high touch, low break, low touch | level of the last completed non-stub trading day, as known at the start of bar t |
| prior week | high break, high touch, low break, low touch | level of the last completed week, known at the start of bar t |
| pivots | cross, touch of any of P, R1, S1, R2, S2 | prior trading day |
| round numbers | big-figure cross and touch; half-figure (x.xx50 only) cross and touch | big figure 0.01, USD/JPY 1.00 |

All comparisons follow SPEC section 6b: prices on the grid (1e-6; USD/JPY 1e-4), averages, bands, pivots and
shifted placebo levels rounded to it, and a price equal to a level counts as **above** it. So:

- **Break** of a level L: +1 when close[t-1] < L <= close[t]; -1 when close[t-1] >= L > close[t]. The same rule is
  used for crosses of averages, round-number lines and every placebo level.
- **Touch from below** (+1): close[t-1] < L, high[t] >= L, close[t] < L. **Touch from above** (-1):
  close[t-1] >= L, low[t] < L, close[t] >= L. For the prior-day high and low these are SPEC's `ev_pdh_touch` and
  `ev_pdl_touch`; the study also counts the other side of each (a retest of the high from above, of the low from
  below), with the same rule.
- **Donchian**: +1 when close >= the prior 20-bar high, -1 when close < the prior 20-bar low.
- **Bollinger**: +1 when the close moves to or above the upper band, -1 when it moves below the lower band.
- **Round numbers**: a close on a line is above it; a touch from below reaches the line above, a touch from above
  goes below the line below.

That is 30 signals on M15, 27 on H1 and 24 on H4: 81.

**Levels are taken as known at the start of bar t.** With the level as known at the end of bar t, the last bar of
each trading day would be compared with the day it belongs to, and its own high would make a "touch".

**Placebos.** The same event logic on levels nobody watches. Two placebos per signal, one on each side of the
standard setting, pooled, so a smooth dependence of the mechanical reaction on the setting cancels to first order:

| signal | placebo |
|---|---|
| SMA50, SMA200, SMA50/200 | SMA 37 and 67; 163 and 237; 37/163 and 67/237 |
| RSI(14) at 30/70 | RSI(11) and RSI(17) at 30/70 |
| Bollinger(20), Donchian(20) | lengths 15 and 26 |
| prior day/week levels, pivots | the level +0.37 and -0.37 D1 ATR(14) |
| round numbers | a grid of one big figure offset 0.37 and 0.63 of a figure (0.13 of a figure from the half figure) |

## 4. Trades, horizons, costs

- Entry at the close of the event bar t. Exit at the close of bar t + h, h in **{1, 4, 16} decision bars**.
  One bar is the immediate reaction. Four bars is the same clock time as one bar of the next timeframe up
  (M15 x 4 = H1, H1 x 4 = H4). Sixteen bars is about a session (M15: 4 hours; H1: 16 hours; H4: about 2.7 trading
  days). The spacing of 4 keeps the three horizons from repeating each other. Horizons count bars, so a horizon can
  span a weekend.
- **Gross** (the gate): d x (mid[t+h] - mid[t]) / mid[t] x 10,000, bp. Reversal is its negative.
- **Net** (reported): buy at the ask, sell at the bid, at the closes of t and t+h, plus 0.2 bp commission per side,
  in bp of the entry mid (SPEC 7). Computed for the continuation and for the reversal trade.
- **Cost of a trade** = gross - net = half-spread at entry + half-spread at exit + 0.4 bp. Financing is not charged.
- **Overlap.** Events of one series are thinned so the kept trades never overlap: keep the first event, skip
  every event before t + h, keep the next, and so on. It uses only the past, so it is a trade someone could have
  made. Each placebo series is thinned on its own. (Not a block bootstrap: the clustered standard errors below
  already take care of dependence between nearby trades.)

## 5. Statistics

- Per test (signal x X x h), pooled over the six pairs: the mean gross per trade, and its t with standard errors
  clustered by trading day and by week (`session_flow/basket_test.py` convention). **t is the smaller in absolute
  value.** A day cluster holds every pair's events of that day, so correlation between pairs is in the error.
- Against placebo: the difference of means (standard - pooled placebo), with a clustered standard error built from
  the paired cluster contributions (events of both on the same day are not treated as independent). Same min rule.
- Per pair and per placebo side: reported, not gating, except the guards in section 7.
- 81 signals x 3 horizons = **243 pooled tests** per period.

## 6. Multiple testing: why discovery then confirmation

243 correlated tests. Holm at 5% over all of them would need |t| of about 3.8 on 2008-2014. I propose finding
candidates on 2008-2011 and confirming them on 2012-2014 instead, for four reasons:

1. **The size that decides the gate is measured where nothing was chosen.** A Holm survivor's own estimate is
   inflated by the selection. A confirmation-period mean is not. The gate compares that mean with cost.
2. **It asks that a reaction hold in two different market regimes.** 2008-2011 is the crisis and its aftermath,
   wider spreads. 2012-2014 is quieter, with the narrower spreads Stage 2 would trade at (REVIEW.md 8.28).
3. **Power is similar.** For a reaction whose full-period t would be 4, the two steps pass with probability about
   0.5; Holm at 3.8 with about 0.6. The split loses little and gains point 1.
4. **The confirmation family is small.** Holm runs only over the candidates (in synthetic nulls 0.6 candidates per
   run on average), so a real candidate needs a modest t on fresh data.

Holm over all 243 tests on 2008-2014 is reported alongside, not deciding.

## 7. The gate (fixed)

**Candidate** (discovery, pooled): |t| >= 2.5 against zero and |t| >= 2.5 against the placebo, the two with the
same sign, at least 50 events on at least 30 days. The sign s of the discovery mean fixes the trade direction.

**Confirmed reaction** (confirmation, pooled), for each candidate in direction s:
p = max(one-sided p of s x t against zero, one-sided p of s x t against placebo); Holm at family 0.05 over the K
candidates; and both s x t >= 2.0.

**Ratio** of a confirmed reaction = s x (confirmation mean gross per trade) / (confirmation mean cost per trade in
direction s, at the same events). Reported beside it: the same with the beyond-placebo part only.

**Guards** (a confirmed reaction that fails one cannot give GO):
- **one pair**: leaving out any single pair, s x t (against zero and against placebo) stays >= 2.0;
- **both placebos**: the standard mean lies beyond each placebo's mean in direction s, not between them;
- **plausibility**: the test is not flagged IMPOSSIBLE? (|t| >= 8, or hit rate outside 0.25-0.75 with 100 or more
  events). Such a result means look for look-ahead first.

| outcome | condition | what it means | what follows |
|---|---|---|---|
| **GO** | a confirmed reaction with ratio >= **0.5**, passing all guards | Standard signals carry a reaction beyond placebo, confirmed out of sample, at least half the cost of a trade before any filtering. | Stage 2 goes ahead with these signals as inputs. |
| **OWNER DECIDES** | no GO; a confirmed reaction with ratio >= **0.25**, or one >= 0.5 that fails a guard | There is a reaction but it is small against cost, or it rests on one pair, or it sits between its placebos. | The owner decides. The default is to stop. |
| **STOP** | no confirmed reaction, or every confirmed reaction below 0.25 | Nothing beyond placebo that could pay even after a large improvement. | No Stage 2 on these signals. |
| **NO RESULT** | a pair missing; the look-ahead canary not detected; quoted spread at the events below 0.01 bp for a pair | The data or the pipeline is not what was registered. | Fix the reading only (section 11), rerun. |

**The fractions 0.5 and 0.25: OWNER DECIDES.** My reasoning:

- Filtering can raise the edge per trade if the reaction is concentrated in some conditions (hour, volatility,
  trend). Combining two or three partly independent signals adds their edges. Trading only in liquid hours cuts cost:
  hour 17 New York costs 2.6-4.3 times the cheapest hours (8.28), and the mean cost at the events includes those
  hours. Together, a factor of 2 to 3 over the raw signal is plausible. A factor of 4 or more is not something to
  plan on.
- So a raw ratio of 0.5 has a plausible path to 1 or more, and 0.25 needs an improvement of four times.
- Calibration against what this project has seen: one-minute reversal in EUR/USD was about 0.1 pip against a cost of
  0.8 (ratio about 0.12, 8.9 and 8.28): STOP here. The overnight agents' 0.48 pip before cost against 0.8 (about 0.6,
  8.15) would have passed 0.5, and did not hold on new data; but it was an in-sample number, and here the ratio is
  measured on the confirmation period only.
- In bp: round-trip cost with commission in 2012-2014 is roughly 0.6 bp (EUR/USD) to 2 bp (NZD/USD), from the
  spreads in 8.28 and the basket amendment. A ratio of 0.5 then means about 0.3 to 1 bp of gross reaction per trade.

Only one confirmed reaction is needed. Evolution can use one strong input. How many there are is reported.

## 8. What does not count

- Any single pair; any per-pair, per-year or per-placebo-side line; net after cost of the raw signals; the Holm
  line over all tests. They describe the result. Where the owner decides, they are what the owner reads.
- Any change after the run: other signals, levels, placebos, horizons, periods, thresholds, costs or pairs. A changed
  test is a new hypothesis for data not yet read (2015 on).
- The side questions below, whatever they show.

## 9. Side questions (reported only; none of them gates)

Run by `rule_walkforward.py` on M15, H1 and H4, 2008-2014, six pairs, fills and costs as section 4, the same
stale-quote handling (no position on an excluded row or on the row before a stretch; event-driven rules reset to
flat there).

**Rules** (positions -1/0/+1; each with a "fade" version that is its negative): MA 50/200 (sign of SMA50 - SMA200);
RSI 30/70 reversal (long from crossing up through 30, short from crossing down through 70, flat at 50); prior-day
breakout (latest break of the day, flat at the day's last bar, so it never holds across 17:00); Donchian-20 stop and
reverse; round-number bounce (4 bars against the approach after a big-figure touch).

1. **Rolling choice.** Training window W = 5, 20, 126 trading days, or all history. Choose among the 10 variants and
   flat by training net after cost (ties go to flat), hold for 3 trading days, roll by 3. The choice is made at the
   close of the round's first bar using only P&L of intervals that end by then. Compared with each fixed variant
   over the same days. Reported: per round, totals by pair and year, basket t, trades, cost.
2. **Does the last window predict the next 3 days?** Correlation, pooled over pairs, of each rule's training-window
   net and its next-3-day net (Pearson and rank), with a 95% moving-block bootstrap interval over rounds (block
   = max(5, W/3) rounds; 20 for all history). Training windows overlap, so the statistic is biased downward even when
   nothing is predictable (a Stambaugh-type bias: the sample mean of the training sums contains the later test
   sums). The bias is measured, not assumed: the same statistic on the days put in random order in blocks of 3 days
   (200 orders), which keeps the overlap. Reported: estimate, bootstrap interval, null centre and interval, excess
   over the null centre, permutation p.
3. **The 17:00 share.** Net and gross P&L of trades held across 17:00 New York against trades opened and closed
   within one trading day; and the gross P&L of the hourly intervals that contain 17:00.
4. **Money and drift.** Each strategy is reported as plain money after cost, and beside it with one drift per pair
   removed over the evaluated span.
5. **Drift forms** (`drift_forms.py`, synthetic only; see 10.4). On the evidence there, one drift per span (the
   current Erlang form) or a drift from all earlier history should be used. A drift from a short training window
   should not.

## 10. Validation on synthetic data (what was checked before registering)

All on `synthetic.py`: random walks in log mid with Student-t(5) minute steps, an intraday volatility profile,
wandering daily volatility, a shared dollar factor (correlation 0.4), Sunday-open jumps, no weekend or 25 Dec / 1 Jan
bars, bid and ask from a spread of 1-2 pips that widens at 17:00 New York (mean round-trip cost at the events about
2.4 bp, wider than real 2012-2014). Six pairs, 2007 warm-up, 2008-2014 split 4/3 years like the real periods.

1. **No edge on random walks (the study).** 100 runs x 243 pooled tests: t against zero has mean +0.01, sd 0.98-1.03,
   |t| >= 2 in 3.9-5.4% (normal 4.55%), |t| >= 3 in 0.2-0.45% (0.27%). t against placebo: mean 0.0, sd 0.99, |t| >= 2
   in 4.1-4.7%, |t| >= 3 in 0.2%. **Gate: no GO in 100 runs.** In two runs a false candidate was confirmed (ratio 1.04
   and 0.87, at h = 16, where noise per trade is large against cost). The one-pair guard turned both into OWNER
   DECIDES. With the t >= 2.0 floor added afterwards, one of the two remains. So the false OWNER DECIDES rate is
   about 1-2%. Mean candidates per run: 0.6.
2. **Planted reactions are found.** Continuation after a prior-day high/low cross (`pdh_break`), and a bounce after a
   big-figure touch (`round_bounce`), planted on M15 and measured at M15, h = 4, pooled (10 runs each):

   | planted effect | measured gross per trade | t | confirmed by the gate |
   |---|---|---|---|
   | pdh_break | 0.28 bp | 2.2 | 0% |
   | pdh_break | 0.41 bp | 3.2 | 10% |
   | pdh_break | 0.54 bp | 4.1 | 40% |
   | pdh_break | 0.80 bp | 5.9 | 90% |
   | round_bounce | 0.45 bp | 4.0 | 20% |
   | round_bounce | 0.92 bp | 8.0 | 100% |
   | round_bounce | 1.91 bp | 16.6 | 100% |

   **Reliably confirmed (90% or more) from about 0.8-0.9 bp of gross reaction per trade at M15** on this design.
   That is about the GO line (ratio 0.5) for the more expensive pairs, and above it for EUR/USD. A real reaction
   much below 0.5 bp per trade will usually give STOP. That is accepted: it would be below about 0.4 of cost anyway.
   Longer bars and horizons are noisier per trade and need proportionally larger reactions.
3. **Look-ahead.** (a) Every standard and placebo event series is unchanged when every bar after a cut time is
   replaced by nonsense (all decision TFs). (b) The canary (the prior-day-high break moved one bar earlier) gives t
   of about +58 and a hit rate of 1.00 and is flagged; random events at the same rate are not. The real run computes
   both and gives NO RESULT if the canary is missed. (c) A feature column moved one bar into the future
   (`ev_pdh_break`, `H1_ev_sma50_cross`, `quality_excluded`) is caught by the cross-check against `features.py`
   tables, which stops the run before any return is computed. (d) The walk-forward choice and training score of
   every round are unchanged when all data after that round's decision time is replaced.
4. **Drift forms** (3 pairs x 4 runs x 2008-2014, H1, 3-day windows). Constant drift 3 bp/day; trend regimes
   +-25 bp/day:

   | form | constant long under drift, bp per window | trend rules keep (MA / Donchian) | on a random walk: reversal rule t / trend rule t |
   |---|---|---|---|
   | none (money) | +11.3 | 102% / 102% | -2.1 / -0.4 (cost) |
   | one drift per span (Erlang now) | 0.0 | 98% / 98% | -2.0 / -0.5 |
   | drift of the last 5 days | +0.2 | -131% / -12% | +7.5 / -39.2 |
   | drift of the last 20 days | -0.1 | -2% / 50% | +0.4 / -12.1 |
   | drift of the last 126 days | 0.0 | 82% / 90% | -1.6 / -2.5 |
   | drift of all earlier days | -0.7 | 96% / 98% | -2.0 / -0.6 |
   | drift of the scored window (old fx.erl form) | 0.0 | -39% / -170% | +43.7 / -26.8 |

   "Keep" is against removing exactly the drift the generator added. A drift estimated from a short recent window
   moves with the price, so it is correlated with any position that reads recent prices: it penalises trend rules
   and credits reversal rules even on a random walk, the same flaw as the old in-window form, through the past.
5. **Walk-forward on random walks** (20 runs, six pairs, H1, 2008-2014). 280 basket strategy lines: gross t mean
   -0.03, sd 0.90, |t| >= 2 in 2.9%, none >= 3. Correlation lines: excess over the null centre averages 0.000; the
   bootstrap interval excludes the null centre in 4.9-5.0%. Permutation p < 0.05 in 6.4-6.6% overall, 11.7% at W = 5 (a little
   liberal there; read W = 5 with that in mind). Without the permutation null, the bootstrap interval excludes zero in
   29% of lines (the overlap bias of item 2 in section 9).
6. **Costs are charged once per entry and once per exit.** Flat price: one trade costs exactly two half-spreads +
   0.4 bp however long it is held; a flip costs two round trips; the bar-level accounting equals a fill-by-fill
   recount for the rolling strategies, including switches at round boundaries.
7. **Date guard.** A request past 2017-12-29 is refused. Bars past the guard are dropped, and poisoning them changes
   no number. Both tools call `common.allowed_through`. While this file is a draft, the real-data commands refuse
   before opening any file.
8. **Agreement with `features.py`** (built to SPEC 6b). On synthetic bars, every event column of
   `features.build_features` (prior-day breaks and touches, Donchian, round-figure cross, SMA50/200 crosses on every
   chart TF, `quality_excluded`) equals the study's own, event for event, on all three decision TFs and six pairs.
   This holds also with prices put on whole pips, where closes sit exactly on levels thousands of times, so the
   tie rules are exercised.

Commands: `python3 -m unittest test_study_tools` (30 tests, about 10 s); `python3 signal_study.py validate ...`;
`python3 drift_forms.py`; `python3 rule_walkforward.py synth ...`. Runtime of the study on six synthetic pairs: about
2.5 s for one year, 11 s for 2008-2014. Walk-forward: 3-4 s per decision TF for 2008-2014.

## 11. Running it, once

```
python3 signal_study.py run          > the gate (writes tests.csv, summary.txt, meta.json)
python3 rule_walkforward.py run      > the side questions
```

Outputs go under `market_data/derived/ta_stage1/` (not in git). The person running it records the commit hash, the
bars' and quality files' SHA-256 (in their meta.json), and the printed decision, and commits the summary.

- If the run stops before printing a decision (a crash, a format problem, the cross-check), the fix may touch only
  the reading of the files or the reconciliation of an event definition with `features.py`. It must change no
  signal, placebo, horizon, period, threshold or cost above. It is committed with its reason before the rerun.
- Once a decision is printed, that is the result. Data problems found afterwards are reported next to it.

## 12. Owner decisions (2026-11-16)

1. GO at 0.5 and OWNER DECIDES at 0.25 of cost (section 7). **Decided: 0.5 and 0.25.**
2. The ratio uses the full gross reaction; the beyond-placebo part is reported beside it. **Decided.**
3. Discovery threshold 2.5, confirmation floor 2.0, Holm at 0.05, at least 50 events on 30 days. **Decided.**
4. Horizons {1, 4, 16} bars; the placebo settings in section 3. **Decided.**
5. 2003-2007 is not added as a period; it stays unread. **Decided.**

## 13. Known limits

- The placebo removes the mechanical part of a reaction to first order only. A reaction that changes non-linearly
  with distance from the level can still differ from its pooled placebo. The both-placebos guard catches the
  monotone case.
- Tests overlap: an H1-chart signal at M15 with h = 4 is the same trade as at H1 with h = 1. Holm is conservative
  under such dependence; the candidate list can contain near-duplicates.
- Fills are one vendor's minute-close quotes, not executions (8.28). Financing is not charged.
- The power figures come from synthetic data whose volatility is close to real but whose spreads are wider. Real
  event counts and noise will differ.
