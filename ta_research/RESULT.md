# Result of the registered Stage 1 signal study

Run once on 2026-11-16, from `ta_research/`, at commit `52db13d` (clean tree; `PREREG_TA_SIGNALS.md` approved and
committed in that commit, SHA-256 `0a4312f8…`):

```
python3 signal_study.py run
python3 rule_walkforward.py run
```

Both exited normally on their first and only run. Full outputs are under
`market_data/derived/ta_stage1/signal_study/20261116T184213/` and `…/rule_walkforward/20261116T184235/` (not in git);
the summaries are copied to `results/`.

## Decision printed by the tool: STOP

| Item | Value |
|---|---|
| Candidates on 2008–2011 | 2, both `round_big_touch` on M15 (continuation after touching a big figure) |
| Confirmed on 2012–2014 | **0**. h = 4: +0.19 bp, t +1.33; h = 1: +0.12 bp, t +2.04 but t against placebo +0.70 |
| Their ratio to cost, had they confirmed | 0.13 and 0.08 (cost about 1.5 bp per round trip at those events, 2012–2014) |
| Mean cost per round trip at all events, 2008–2014 | 2.24 bp |
| Look-ahead canary | detected (t +45, hit 1.00); random-event control t −0.21 |
| Cross-check of event definitions against the feature tables | 0 disagreements, every pair, timeframe and event column |

**Read with it (reported, not deciding):**

- Prices do react to these events at the mid: across the 243 pooled tests, t against zero has sd 2.11 and |t| ≥ 2 in
  27%. Against the placebo the spread is close to chance (sd 1.12, |t| ≥ 2 in 9.9%). The reactions are almost all
  short-term reversal at M15 that appears equally at levels nobody watches. Example: after a 20-bar breakout, −0.35 bp
  one bar later (t −9.6, flagged by the |t| ≥ 8 rule); after 15- and 26-bar breakouts, −0.34 bp. That is generic
  reversal, at about a sixth of the cost of a trade, not a reaction to a watched level.
- Holm over all 243 tests, full period: against the placebo only `round_big_touch` at M15 (h = 1 and 4) survives,
  the same pair that failed confirmation.

## Side questions (reported only)

- **Hand-written rules after cost (2008–2014, six-pair basket):** nearly every static rule and every rolling choice
  loses after cost on M15, H1 and H4. The few positive lines are within noise (largest t +0.66: H4 RSI 30/70
  reversal; H1 MA 50/200 +0.11). Prior-day breakout and round-number bounce lose most, mainly through cost.
- **Does the last window predict the next 3 days?** No. Pooled over rules, the correlation of a rule's last-window
  result with its next-3-day result, minus the overlap null, is between −0.02 and +0.05 for W = 5, 20 and 126 days on
  every timeframe, and negative (−0.01 to −0.08) for all history. The rolling choice did worse than the all-history
  choice and than most fixed rules on M15 and H1.
- **17:00 New York, drift:** per strategy in the summaries; nothing there changes the above.

## What this means

By the registered rule there is no Stage 2 on these signals. Standard technical-analysis signals, at their standard
settings on six USD pairs at 15-minute to 4-hour decisions, showed no reaction beyond what the same logic shows at
meaningless levels, confirmed out of sample. The one candidate was about a tenth of the cost of a trade. The premise
that recent results predict the next few days also found no support in hand-written rules.

Nothing dated after 2014-12-31 was read. 2015–2017 and the holdout remain unread.
