# Stage 1 specification: bars, features and evidence (Python only)

Branch `ta/stage1`, worktree `DXNN_Main/DXNN-ta-stage1`. Code lives in `ta_research/`. Generated data never goes into git:
it is written under `DXNN_Main/market_data/derived/ta_stage1/` (the Dukascopy terms forbid redistribution, and this repo has
a GitHub remote).

Python 3.9 with numpy 2.0 and pandas 2.3 only. Do not install packages. Standard library `zoneinfo` for New York time.

## 1. Purpose

Stage 1 answers, before any Erlang work, whether there is anything for evolution to learn from widely used
technical-analysis signals on six currency pairs at 15-minute to 4-hour decisions, and builds the data that Stage 2 will
load. Gate: the rule in the signal-study pre-registration (`PREREG_TA_SIGNALS.md`). No return of any trading rule on real
data is computed until that file is approved by the owner and committed.

## 2. Inputs

`DXNN_Main/market_data/dukascopy_m1/<PAIR>/<PAIR>_<YYYY>.csv.gz`, columns
`ts_utc,bid_open,bid_high,bid_low,bid_close,ask_open,ask_high,ask_low,ask_close`. `ts_utc` is the minute's start in UTC.
Only minutes quoted on both sides. `MANIFEST.csv` in the same folder has each file's SHA-256: verify it before reading.

Pairs: `EURUSD GBPUSD USDJPY USDCHF USDCAD NZDUSD`. AUD/USD is excluded (failed clock check, crossed quotes 2007–2010).

## 3. Date guard (all tools)

- Every tool takes the allowed range from one function in `ta_research/common.py` and refuses to open any year file, or
  keep any row, outside it.
- Default allowed range: trading days up to and including 2014-12-31 (the development period).
- `--through YYYY-MM-DD` may extend it to at most 2017-12-29. Anything later is refused unless `--unseal-holdout` is also
  given; no Stage 1 task uses that flag.
- Every output's `meta.json` records the range used.

## 4. Time conventions

- All stored times are UTC, as int64 seconds since the epoch.
- **Trading day:** from 17:00 New York time to 17:00 New York time the next day, labelled by the New York date on which it
  ends. Daylight saving per `zoneinfo.ZoneInfo("America/New_York")`: the boundary is 21:00 UTC in US summer time, 22:00 UTC
  otherwise.
- **Week:** the trading days Monday to Friday, i.e. Sunday 17:00 NY to Friday 17:00 NY.
- Minutes between Friday 17:00 NY and Sunday 17:00 NY are dropped and counted (`dropped_weekend`).
- Minutes with `ask_close <= bid_close` (crossed or locked) are dropped and counted (`dropped_crossed`). This removes 58
  locked EUR/USD minutes on 14–15 Oct 2013 and one crossed USD/JPY minute.

## 5. Bars

Timeframes: `M15`, `H1`, `H4`, `D1`, `W1`.

- M15 and H1 bars start on UTC quarter-hours and hours (the same instants as New York ones; offsets are whole hours).
- H4 bars are anchored on 17:00 NY: they start at 17, 21, 01, 05, 09 and 13 New York time.
- D1 is the trading day, W1 the trading week.
- Build M15 bars from minutes one pair-year at a time (an M15 bar never crosses a UTC year boundary), concatenate, and build
  every longer timeframe from the M15 bars.
- An interval with no minutes produces no bar (no forward fill). Partial bars are kept and carry their minute count.

Fields per bar (one array per field):

| field | meaning |
|---|---|
| `start` | bar start, UTC seconds |
| `end` | bar end = start + the timeframe's length (D1: the trading day's 17:00 NY end; W1: Friday 17:00 NY), UTC seconds. The bar is known at `end`; a Friday bar is known at Friday 17:00 NY, not at the Sunday reopen. |
| `mid_open, mid_high, mid_low, mid_close` | mid = (bid + ask) / 2 per minute; high/low are the max/min over minutes of (bid_high + ask_high)/2 and (bid_low + ask_low)/2 |
| `bid_open, ask_open` | first minute's bid_open and ask_open |
| `bid_close, ask_close` | last minute's bid_close and ask_close (the fill prices for a decision at this bar's close) |
| `spread_mean` | mean over minutes of (ask_close - bid_close) |
| `n_minutes` | minutes in the bar |
| `trading_day` | trading-day label as an int YYYYMMDD (D1 and shorter; W1 has none) |

Bars whose `end` is past the guard limit are not written (under the default range this drops only the unfinished week
that starts 2014-12-28); their minutes are counted in meta.json.

Output: `derived/ta_stage1/bars/<PAIR>/<TF>.npz` (float64 prices, int64 times, int32 counts) and
`derived/ta_stage1/bars/<PAIR>/meta.json` (source file SHA-256s, date range, row counts, every drop counter, builder git
commit, build time).

## 5a. Stale-quote periods

`ta_research/quality.py` writes `derived/ta_stage1/quality/<PAIR>.json`: runs of 6 or more H1 bars whose mid range is
below 20% of the median of the previous 480 H1 ranges, each excluded from its start to 24 hours after its end. Through 2014
this finds 5 EUR/USD feed defects (four in 2008, and 2009-06-29 to 07-01 with the quote stuck for 48 hours) and New Year /
Christmas quiet periods in four other pairs. Feature rows inside an interval carry `quality_excluded = 1`. **This column uses later data by design** (a run is only
recognisable after 6 H1 bars, but the flag starts at the run's first bar). It is a data-cleaning mask that removes
defective prices from the sample, never an input: no sensor, signal or agent may read it. Studies skip
events and do not trade on those rows; positions are not opened there and open positions are closed at the last row
before the interval.

## 6. Features (built after the bars)

One feature table per pair and decision timeframe `X` in {M15, H1, H4}; one row per X bar; row t uses only data known at
`end[t]` of that bar. Charts read for each X: M15 → {M15, H1, H4, D1}; H1 → {H1, H4, D1}; H4 → {H4, D1}. W1 is used for
weekly levels only.

**Rule against look-ahead:** for a chart timeframe C, only C bars with `end <= end[t]` are used (completed bars). The
current price is `mid_close[t]` of the X bar. Every row stores `available = end[t]`.

**Holiday stub days:** a D1 bar with fewer than 360 minutes (Christmas, New Year and similar) is a stub. Stubs are left out
of every D1-based calculation (D1 averages, D1 ATR, D1 RSI/MACD/etc., previous-day levels, pivots): "the previous day"
means the previous non-stub trading day. W1 bars are built from all minutes as usual. Column `prev_day_stub` (int8) is 1
when the most recent completed trading day was a stub.

**Scale:** distances are in units of ATR(14) of the chart timeframe they come from (Wilder smoothing on mid prices). Levels
from D1 and W1 use D1 ATR(14). Values are stored unclipped as float32; NaN until enough history exists.

Per chart timeframe C (name prefix `C_`, e.g. `H4_`):

- `sma{20,50,100,200}_dist`, `ema{20,50,100,200}_dist`: (price − average) / ATR_C, averages of C closes
- `sma50_200_dist`: (SMA50 − SMA200) / ATR_C
- `cross_sma50_age`, `cross_sma200_age`: signed number of C bars since the C close last crossed that average; sign + when
  above; capped at ±100
- `rsi14`: Wilder RSI(14), stored as (RSI − 50) / 50
- `macd`, `macd_signal`, `macd_hist`: MACD(12, 26, 9) on C closes, divided by ATR_C
- `stoch_k`, `stoch_d`: slow stochastic (14, 3, 3), stored as (x − 50) / 50
- `bb_pos`: (price − SMA20) / (2 × stdev20 of C closes, population stdev); `bb_width`: 4 × stdev20 / ATR_C
- `atr_bp`: ATR_C / price × 10,000; `atr_regime`: ATR_C / mean of ATR_C over the last 100 C bars − 1
- `donchian20_hi_dist`, `donchian20_lo_dist`: (price − highest high) / ATR_C and (price − lowest low) / ATR_C over the last
  20 completed C bars
- `close_lag{1..N}`: (price − close of the k-th last C bar with `end < end[t]`, strictly before the decision bar's end)
  / ATR_C, with N = 32 for M15, 24 for H1, 30 for H4, 20 for D1. The strict rule keeps lag 1 from being the bar whose close
  is the current price (on the decision timeframe's own chart, lag 1 is bar t−1).

Levels (no prefix; D1 ATR units):

- `pdh_dist, pdl_dist, pdc_dist`: previous completed trading day's high, low and close
- `pwh_dist, pwl_dist, pwc_dist`: previous completed week's high, low and close
- `pivot_p, pivot_r1, pivot_s1, pivot_r2, pivot_s2`: classic floor pivots from the previous trading day
  (P = (H+L+C)/3, R1 = 2P − L, S1 = 2P − H, R2 = P + (H − L), S2 = P − (H − L)), as distances
- `round_big_dist`, `round_half_dist`: signed distance to the nearest big figure and to the nearest half figure. Big figure
  = 0.01 (1.1000, 1.1100) for non-JPY pairs and 1.00 for USD/JPY; half figure = half of that.

Events on the decision bar t (int8, 0/1, or −1/0/+1 where a direction exists):

- `ev_pdh_break`, `ev_pdl_break`: X close crossed the previous day's high (or low) between bar t−1 and bar t (+1 up, −1 down)
- `ev_pdh_touch`, `ev_pdl_touch`: bar t's high/low reached the level without the close crossing it
- `ev_donchian20_break` on the decision timeframe: close above the prior 20-bar high (+1) or below the low (−1)
- `ev_round_big_cross`: close crossed a big figure (+1 up, −1 down)
- `ev_sma50_cross`, `ev_sma200_cross` on each chart timeframe C (prefixed): the completed C close crossed that average in
  the C bar that completed at or before t, flagged only on the first X bar where it becomes known

Clock (New York time at `available`): `clk_day_sin, clk_day_cos, clk_day2_sin, clk_day2_cos` (one and two cycles per day,
day starting 17:00 NY), `clk_week_sin, clk_week_cos` (trading week fraction).

Fill and context columns copied from the decision bar: `mid_close, bid_close, ask_close, spread_mean, n_minutes,
trading_day`.

Quality column: `quality_excluded` (int8): 1 when `available` lies in (start, end] of any interval in
`derived/ta_stage1/quality/<PAIR>.json` (section 5a).

### 6b. Conventions (settled 2026-11-16 after the independent reference check)

1. **Price grid and ties.** Every comparison of a price with a level, an average or another price is made on prices
   rounded to the grid: 1e-6 for non-JPY pairs, 1e-4 for USD/JPY (mid prices are half-sums of 5- or 3-decimal quotes).
   Raw float comparisons are not allowed. Prices and computed levels (averages, pivots) are put on the grid by rounding
   half-up. A price equal to a level on the grid counts as **above** it, everywhere: a cross up is prev < L ≤ cur, a cross
   down is prev ≥ L > cur.
2. **ATR(14) and RSI(14).** True range is undefined on a series' first bar; Wilder smoothing is seeded at bar 14 with the
   mean of TR[1..14] (gains/losses likewise). RSI = 100 when the average loss is 0, 50 when both averages are 0.
3. **EMA** alpha = 2/(n+1), seeded with the SMA of the first n values. MACD signal seeded with the mean of the first 9
   MACD values. Slow stochastic: raw %K = 50 when the 14-bar high equals the low. `bb_pos` is NaN when stdev20 is 0.
4. **`cross_*_age`.** Counts consecutive C bars on the current side including the current bar, so ±1 on the crossing bar;
   sign + when above (equal counts as above, rule 1); capped at ±100; NaN before the first cross unless the count has
   reached 100.
5. **Levels for distances vs events.** Distance columns (`pd*`, `pw*`, pivots) use the D1/W1 bars that ended by
   `available` (on the 17:00 NY row, "previous day" is the day that just ended, which is the level in force for the next
   decision). Event columns (`ev_pdh_*`, `ev_pdl_*`) use the level in force **during** bar t: the last non-stub D1 bar that
   ended by the bar's start.
6. **Breaks and touches.** `ev_pdh_break` = +1 when prev close < PDH ≤ close, −1 when prev close ≥ PDH > close (same
   for PDL). Touches are 0/1: `ev_pdh_touch` = 1 when prev close < PDH, bar high ≥ PDH and close < PDH; `ev_pdl_touch`
   mirrors it (prev close ≥ PDL, bar low < PDL, close ≥ PDL).
7. **Round numbers.** Big figures are multiples of the figure (0.01, or 1.00 for USD/JPY). Half figures are only the
   levels half-way between big figures (1.1050-type), not the big figures themselves. The nearest level on an exact
   half-way tie is the upper one. `ev_round_big_cross` follows rule 1.
8. **Clock.** Week fraction is over the 5-day trading week.
9. **`close_lag`** uses bars with `end < end[t]` (strict), as stated above.

Output: `derived/ta_stage1/features/<PAIR>/<X>.npz` (one array per column) plus `meta.json` listing every column with its
definition string, the bar file SHA-256s it was built from, and the date range.

**API** (`ta_research/features.py`): `build_features(bars: dict[str, dict[str, np.ndarray]], decision_tf: str,
pair: str) -> dict[str, np.ndarray]`, where `bars` maps a timeframe to its field arrays. Pure: no file access. A
truncation check must be able to call it on bars built from minutes cut at time T and get identical rows for every
`available <= T`.

## 7. Costs (for the study tools)

A position is entered at the ask (buy) or bid (sell) close of the decision bar and closed the same way, plus a commission of
0.2 bp of notional per side (the project's Interactive Brokers assumption, REVIEW.md 8.28). Overnight financing is
deliberately not modelled. Returns are reported in basis points of the entry mid.

## 8. Statistics conventions

As in the project's earlier tests (`session_flow/basket_test.py`): standard errors from day blocks and week blocks; the t
reported is the smaller in absolute value.

## 9. What must not happen in Stage 1

- No return, profit or forward price change of any rule or signal on real data before `PREREG_TA_SIGNALS.md` is approved and
  committed. Building bars and features, and checking them, is allowed.
- Nothing dated after the allowed range is opened.
- No generated data is added to git.
