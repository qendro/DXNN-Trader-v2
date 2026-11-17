# ta_research (Stage 1)

The contract is `SPEC.md`. Paths, pairs and the date guard are in `common.py`. Generated data goes to
`DXNN_Main/market_data/derived/ta_stage1/` and never into git.

## Bars

```
python3 ta_research/bars.py                      # all six pairs, trading days through 2014-12-31
python3 ta_research/bars.py --pairs EURUSD,USDJPY
python3 ta_research/bars.py --through 2016-06-30 # up to 2017-12-29; later dates raise DateGuardError
```

- Each year file's SHA-256 is checked against `market_data/dukascopy_m1/MANIFEST.csv` before it is parsed. Only years
  allowed by the date guard are opened.
- Rows at or after 17:00 NY on the `--through` day are dropped, then weekend minutes (Fri 17:00 to Sun 17:00 NY), then
  crossed minutes (`ask_close < bid_close`). Each drop is counted in `meta.json`.
- M15 bars are built per pair-year. H1, H4 (17:00 NY anchor), D1 (17:00 to 17:00 NY, labelled by the NY end date) and W1
  (Sun 17:00 to Fri 17:00 NY) are built from the M15 bars.
- A bar is written only if its `end` is at or before the guard limit. With the default range this drops only the last,
  unfinished W1 bar (week of 2014-12-29). Its minutes are counted in `counters.minutes_in_bars_ending_after_limit`.
- Output: `bars/<PAIR>/{M15,H1,H4,D1,W1}.npz` and `meta.json`, which holds the source SHA-256s, date range, counters,
  output SHA-256s, git commit and build time. A run with `--through` overwrites the same folder; `meta.json` records
  the range used.
- Run time is about 5 s per pair (30 s for all six), and peak memory is about 0.5 GB.

## Checks

```
python3 ta_research/check_bars.py                # all pairs; exits 1 on any FAIL
python3 ta_research/check_bars.py --pairs EURUSD --seed 7
```

The checks need the default build. They cover:

- **Truncation.** Per pair: 3 random cuts, one cut 1–14 min after a bar end for each timeframe, 3 DST-week cuts
  and the guard limit. Every bar with `end <= T` must be
  identical in all fields.
- **Independent aggregation.** D1 and H4 bars are rebuilt straight from minutes with pandas, using NY wall-clock times.
- **DST.** Hard-coded sample days, plus every D1, H4 and W1 bar checked against zoneinfo.
- **Accounting, sanity and the date guard.** No price-change statistic is computed.

Run time is about 100 s for all pairs.

## Features

```
python3 ta_research/features.py                       # six pairs x {M15, H1, H4}, through 2014-12-31
python3 ta_research/features.py --pairs EURUSD --tfs H1
python3 ta_research/selfcheck_features.py             # exits 1 on any FAIL
```

- `build_features(bars, decision_tf, pair, quality_intervals=None)` is pure.
  - Each chart indicator is computed once on the chart series, then gathered to the decision rows at the last chart
    bar with `end <= available` (`searchsorted`).
  - `close_lag` uses bars with `end < available` (strict).
  - The price is the decision bar's `mid_close`.
  - D1 stub days (< 360 minutes) are removed from the D1 series first.
- Price grid (SPEC 6b rule 1): mid prices are snapped to 1e-6, or 1e-4 for USD/JPY, before anything is computed.
  - Every comparison of prices, levels and averages (averages rounded to the grid) is therefore exact.
  - A price equal to a level counts as above it: a cross up is `prev < L <= cur`.
  - Context columns keep the raw bar values.
- `quality_excluded` (SPEC 5a) is a hindsight data-quality flag, not a feature, so it is passed in.
  - The CLI loads `quality/<PAIR>.json`, checks that it was built from the same H1 bars, and passes its intervals.
  - Called without intervals, the column is all 0.
- The CLI checks each bar file against the bars `meta.json` SHA-256 and keeps only bars ending by the guard limit.
- It writes `features/<PAIR>/<X>.npz` and one `meta.json` per pair. The `meta.json` holds:
  - a definition and dtype for each column;
  - the bar SHA-256s;
  - the quality file's SHA-256 and intervals;
  - the date range;
  - the SPEC 6b conventions;
  - the git commit and build time.
- Self-checks:
  - The column list matches SPEC.
  - dtypes are correct.
  - No float column is all NaN after 2005, and none contains inf.
  - The NaN fraction after 2006 is reported.
  - Value ranges hold.
  - `available` equals the bar `end`, and context columns equal the bars.
  - Own-chart `close_lag k` equals bar t−k.
  - `quality_excluded` equals `quality.excluded_mask`.
  - Event checks:
    - SMA-cross events match a plain-loop recomputation.
    - On H1 and H4, the level, Donchian and round-number events match a plain loop on integer grid ticks.
  - Truncation: about 9 cuts per pair, including D1 and W1 ends, a stub day and mid-bar instants. Rows with
    `available <= T` must be bit-identical.
- Run time is about 7 s for everything, with peak memory about 0.7 GB. Output is about 315 MB per pair: M15 is
  259 MB with 240 columns, H1 is 49 MB with 183 columns and H4 is 9 MB with 134 columns. The self-check takes about
  40 s.
