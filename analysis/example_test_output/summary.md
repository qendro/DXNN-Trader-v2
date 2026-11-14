# Result-folder analysis: results

Input `/private/tmp/claude-501/-Users-qendrim--claude-spares-spare-QU2DOK/75009484-9b25-4f81-b4d7-8ad382ceb7db/scratchpad/results`. Output `/private/tmp/claude-501/-Users-qendrim--claude-spares-spare-QU2DOK/75009484-9b25-4f81-b4d7-8ad382ceb7db/scratchpad/results/analysis_out`. Best N = 8; training-span re-simulation on up to 40 episodes tiled over each run's training span; validation re-simulation on the run's own validation tiling (its `episodes_val`); conditions: all. Money is pips per 1,000-bar episode after cost. Every number below is rounded from the CSV row named next to it (file and `row_id`).

Price file loaded by the VMs: `/private/tmp/claude-501/-Users-qendrim--claude-spares-spare-QU2DOK/75009484-9b25-4f81-b4d7-8ad382ceb7db/scratchpad/work/prices/EURUSD1.txt` (rows before 2018-01-01 only). No span after 2017-12-31 was evaluated.

## Runs

| folder | arm | fold | evo seed | run | cost (pips) | saved at target cost | re-simulation | runs.csv |
|---|---|---|---|---|---|---|---|---|
| ff_vol_e1 | ff_vol | fold_2017 | 1 | 1 | 0.00 | no | - | run-1 |
| ff_vol_e1 | ff_vol | fold_2017 | 1 | 2 | 0.25 | yes | done (kept from an earlier pass) | run-2 |
| ff_vol_e1 | ff_vol | fold_2017 | 1 | 3 | 0.25 | yes | done (kept from an earlier pass) | run-3 |
| ff_vol_e2 | ff_vol | fold_2017 | 2 | 1 | 0.00 | no | - | run-4 |
| ff_vol_e2 | ff_vol | fold_2017 | 2 | 2 | 0.25 | yes | done (kept from an earlier pass) | run-5 |
| ff_vol_e2 | ff_vol | fold_2017 | 2 | 3 | 0.25 | yes | done (kept from an earlier pass) | run-6 |
| rec_vol_e1 | rec_vol | fold_2017 | 1 | 1 | 0.00 | no | - | run-7 |
| rec_vol_e1 | rec_vol | fold_2017 | 1 | 2 | 0.25 | yes | done (kept from an earlier pass) | run-8 |
| rec_vol_e1 | rec_vol | fold_2017 | 1 | 3 | 0.25 | yes | done (kept from an earlier pass) | run-9 |
| rec_vol_e2 | rec_vol | fold_2017 | 2 | 1 | 0.00 | no | - | run-10 |
| rec_vol_e2 | rec_vol | fold_2017 | 2 | 2 | 0.25 | yes | done (kept from an earlier pass) | run-11 |
| rec_vol_e2 | rec_vol | fold_2017 | 2 | 3 | 0.25 | yes | done (kept from an earlier pass) | run-12 |

## Learning curves

Per run: the last generation, and the mean of the last 20 generations (or all, if fewer). Full curves: `learning_curve.csv`.

| folder | run | gens | median score (last / window) | best score (last) | survivors on new episodes (last / window) | net pips per episode (last / window) | positions per episode (last) | neurons (last) | s per gen (window) | curve_summary.csv |
|---|---|---|---|---|---|---|---|---|---|---|
| ff_vol_e1 | 1 | 2 | 0.79 / 0.40 | 2.04 | 0.77 / 0.77 | 13.56 / 6.20 | 21.9 | 4.0 | 17.3 | cs-1 |
| ff_vol_e1 | 2 | 2 | -0.30 / -0.38 | 2.15 | -0.06 / -0.06 | -6.19 / -7.76 | 11.8 | 4.3 | 17.8 | cs-2 |
| ff_vol_e1 | 3 | 2 | -0.07 / -0.07 | 1.21 | -0.21 / -0.21 | -3.98 / -3.69 | 17.4 | 4.5 | 18.3 | cs-3 |
| ff_vol_e2 | 1 | 2 | -0.02 / -0.07 | 1.37 | -0.17 / -0.17 | 5.23 / 1.90 | 17.1 | 4.0 | 17.1 | cs-4 |
| ff_vol_e2 | 2 | 2 | -0.30 / -0.42 | 1.25 | -0.40 / -0.40 | -5.67 / -8.42 | 9.9 | 4.0 | 17.8 | cs-5 |
| ff_vol_e2 | 3 | 2 | -0.33 / -0.01 | 0.90 | -0.27 / -0.27 | -19.03 / -2.52 | 21.0 | 4.1 | 19.0 | cs-6 |
| rec_vol_e1 | 1 | 2 | 0.03 / -0.02 | 1.69 | -0.36 / -0.36 | 1.66 / -0.17 | 26.8 | 4.0 | 15.1 | cs-7 |
| rec_vol_e1 | 2 | 2 | 0.76 / 0.19 | 1.14 | 0.58 / 0.58 | 7.03 / 2.34 | 13.2 | 4.1 | 15.3 | cs-8 |
| rec_vol_e1 | 3 | 2 | -0.32 / -0.16 | 0.94 | -0.48 / -0.48 | -18.80 / -10.20 | 17.1 | 4.2 | 16.1 | cs-9 |
| rec_vol_e2 | 1 | 2 | 0.85 / 0.37 | 2.12 | 0.53 / 0.53 | 7.83 / 4.32 | 21.2 | 4.0 | 16.2 | cs-10 |
| rec_vol_e2 | 2 | 2 | 0.26 / -0.06 | 1.40 | 0.25 / 0.25 | 1.56 / -1.13 | 14.7 | 4.2 | 16.3 | cs-11 |
| rec_vol_e2 | 3 | 2 | -0.44 / -0.06 | 0.22 | -0.54 / -0.54 | -16.90 / -6.71 | 16.6 | 4.5 | 18.5 | cs-12 |

## Validation readings (simulator's own, whole population)

Median agent (eligible on score and validation episodes) next to the baselines of the same line. rev60/rev240 are scores in the log; their money comes from the re-simulation of the same episodes (`populations.csv`).

| folder | run | gen | agents | above 0 | median money | flat | long | mom60 | clock | rev60 / rev240 money | validation.csv |
|---|---|---|---|---|---|---|---|---|---|---|---|
| ff_vol_e1 | 1 | 0 | 24 | 1 | -5.66 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-1 |
| ff_vol_e1 | 1 | 1 | 24 | 3 | -7.53 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-2 |
| ff_vol_e1 | 2 | 0 | 24 | 6 | -5.96 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-3 |
| ff_vol_e1 | 2 | 1 | 23 | 6 | -3.20 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-4 |
| ff_vol_e1 | 3 | 0 | 23 | 6 | -1.44 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-5 |
| ff_vol_e1 | 3 | 1 | 24 | 11 | -1.45 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-13) | val-6 |
| ff_vol_e2 | 1 | 0 | 24 | 1 | -7.09 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-7 |
| ff_vol_e2 | 1 | 1 | 24 | 2 | -7.61 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-8 |
| ff_vol_e2 | 2 | 0 | 24 | 4 | -5.36 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-9 |
| ff_vol_e2 | 2 | 1 | 24 | 10 | -4.24 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-10 |
| ff_vol_e2 | 3 | 0 | 24 | 10 | -2.45 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-11 |
| ff_vol_e2 | 3 | 1 | 24 | 12 | -2.44 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-61) | val-12 |
| rec_vol_e1 | 1 | 0 | 24 | 4 | -2.60 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-13 |
| rec_vol_e1 | 1 | 1 | 24 | 3 | -3.95 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-14 |
| rec_vol_e1 | 2 | 0 | 24 | 1 | -7.49 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-15 |
| rec_vol_e1 | 2 | 1 | 24 | 1 | -9.01 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-16 |
| rec_vol_e1 | 3 | 0 | 24 | 1 | -8.60 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-17 |
| rec_vol_e1 | 3 | 1 | 24 | 4 | -7.42 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-109) | val-18 |
| rec_vol_e2 | 1 | 0 | 24 | 2 | -9.11 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-19 |
| rec_vol_e2 | 1 | 1 | 24 | 3 | -9.11 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-20 |
| rec_vol_e2 | 2 | 0 | 24 | 1 | -11.72 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-21 |
| rec_vol_e2 | 2 | 1 | 23 | 3 | -11.37 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-22 |
| rec_vol_e2 | 3 | 0 | 24 | 1 | -11.83 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-23 |
| rec_vol_e2 | 3 | 1 | 24 | 2 | -11.37 | 0.00 | 12.16 | -14.65 | 7.57 | -9.90 / -10.58 (populations.csv pop-157) | val-24 |

## Re-simulation of the best agents, under the training rule

Median over the chosen agents, and the equal-weight portfolio of them with week- and day-block standard errors (an episode belongs to the week or day its first bar is in). All twelve conditions: `populations.csv`, per agent: `per_agent.csv`.

| folder | run | span | agents | median money | portfolio (se week, se day) | strongest baseline | strongest excl. long | populations.csv |
|---|---|---|---|---|---|---|---|---|
| ff_vol_e1 | 2 | train | 8 | 1.25 | -0.51 (6.50, 6.50) | long 10.59 | flat 0.00 | pop-1 |
| ff_vol_e1 | 2 | validation | 8 | -1.62 | -2.12 (5.38, 5.38) | long 12.16 | clock 7.57 | pop-13 |
| ff_vol_e1 | 3 | train | 8 | 5.89 | 0.96 (5.62, 5.62) | long 10.59 | flat 0.00 | pop-25 |
| ff_vol_e1 | 3 | validation | 8 | -2.64 | -2.73 (4.07, 4.07) | long 12.16 | clock 7.57 | pop-37 |
| ff_vol_e2 | 2 | train | 8 | -7.62 | -6.86 (3.97, 3.97) | long 10.59 | flat 0.00 | pop-49 |
| ff_vol_e2 | 2 | validation | 8 | -1.26 | 0.33 (3.96, 3.96) | long 12.16 | clock 7.57 | pop-61 |
| ff_vol_e2 | 3 | train | 8 | -19.63 | -20.25 (8.12, 8.12) | long 10.59 | flat 0.00 | pop-73 |
| ff_vol_e2 | 3 | validation | 8 | 9.04 | 3.53 (5.10, 5.10) | long 12.16 | clock 7.57 | pop-85 |
| rec_vol_e1 | 2 | train | 8 | -20.66 | -17.76 (6.28, 6.28) | long 10.59 | flat 0.00 | pop-97 |
| rec_vol_e1 | 2 | validation | 8 | -9.62 | -9.06 (6.03, 6.03) | long 12.16 | clock 7.57 | pop-109 |
| rec_vol_e1 | 3 | train | 8 | -16.15 | -14.02 (5.91, 5.91) | long 10.59 | flat 0.00 | pop-121 |
| rec_vol_e1 | 3 | validation | 8 | -11.43 | -10.52 (3.55, 3.55) | long 12.16 | clock 7.57 | pop-133 |
| rec_vol_e2 | 2 | train | 8 | -5.46 | -7.25 (6.39, 6.39) | long 10.59 | flat 0.00 | pop-145 |
| rec_vol_e2 | 2 | validation | 8 | -11.83 | -13.39 (5.55, 5.55) | long 12.16 | clock 7.57 | pop-157 |
| rec_vol_e2 | 3 | train | 8 | -3.41 | -4.43 (6.03, 6.03) | long 10.59 | flat 0.00 | pop-169 |
| rec_vol_e2 | 3 | validation | 8 | -8.29 | -8.04 (4.61, 4.61) | long 12.16 | clock 7.57 | pop-181 |

## Comparison across arms

Outcomes: (1) the final population's validation money, median agent, from the simulator's own last validation reading; (2) the same for the best N, re-simulated under the rule the run trained with; (3) the best N under the rollover rule at 0.25 pip, fill now. For runs trained with the rollover rule on and the fill now (all four arms), outcomes (2) and (3) are the same condition and the same number.

**Comparisons made: 2** (paired differences and effects with at least two seeds, over the distinct outcomes). Standard errors come from the spread across seeds, with seeds - 1 degrees of freedom; df 1: |t| > 12.71 for p < 0.05 per comparison, |t| > 25.45 after Bonferroni over 2.

### final_population_validation_median (fold_2017, episode seed 1)

| kind | term | seeds | n | mean | se | t | comparison.csv |
|---|---|---|---|---|---|---|---|
| value | ff_vol | 1 | 1 | -1.45 | n/a | n/a | cmp-1 |
| value | ff_vol | 2 | 1 | -2.44 | n/a | n/a | cmp-2 |
| value | rec_vol | 1 | 1 | -7.42 | n/a | n/a | cmp-3 |
| value | rec_vol | 2 | 1 | -11.37 | n/a | n/a | cmp-4 |
| arm_mean | ff_vol | 1 2 | 2 | -1.95 | 0.49 | n/a | cmp-5 |
| arm_mean | rec_vol | 1 2 | 2 | -9.40 | 1.97 | n/a | cmp-6 |
| paired_difference | rec_vol minus ff_vol | 1 2 | 2 | -7.45 | 1.48 | -5.04 | cmp-7 |
| effect | recurrent minus feedforward |  | 0 | n/a | n/a | n/a | cmp-8 |
| effect | vol minus level |  | 0 | n/a | n/a | n/a | cmp-9 |
| effect | interaction (rec_vol - ff_vol) - (rec_level - ff_level) |  | 0 | n/a | n/a | n/a | cmp-10 |

### bestN_validation_median_training_rule (fold_2017, episode seed 1)

| kind | term | seeds | n | mean | se | t | comparison.csv |
|---|---|---|---|---|---|---|---|
| value | ff_vol | 1 | 1 | -2.64 | n/a | n/a | cmp-11 |
| value | ff_vol | 2 | 1 | 9.04 | n/a | n/a | cmp-12 |
| value | rec_vol | 1 | 1 | -11.43 | n/a | n/a | cmp-13 |
| value | rec_vol | 2 | 1 | -8.29 | n/a | n/a | cmp-14 |
| arm_mean | ff_vol | 1 2 | 2 | 3.20 | 5.84 | n/a | cmp-15 |
| arm_mean | rec_vol | 1 2 | 2 | -9.86 | 1.57 | n/a | cmp-16 |
| paired_difference | rec_vol minus ff_vol | 1 2 | 2 | -13.06 | 4.27 | -3.06 | cmp-17 |
| effect | recurrent minus feedforward |  | 0 | n/a | n/a | n/a | cmp-18 |
| effect | vol minus level |  | 0 | n/a | n/a | n/a | cmp-19 |
| effect | interaction (rec_vol - ff_vol) - (rec_level - ff_level) |  | 0 | n/a | n/a | n/a | cmp-20 |

### bestN_validation_median_rollover_rule_0.25 (fold_2017, episode seed 1)

Same values as bestN_validation_median_training_rule (cmp-21): not compared again.

## Folds

Validation year, final population, best N as an equal-weight portfolio under the training rule. Largest drawdown: of the cumulative money over the year's episodes in order.

| fold | arm | seed | year | money (se week) | strongest baseline | strongest excl. long | largest drawdown (pips) | folds.csv |
|---|---|---|---|---|---|---|---|---|
| fold_2017 | ff_vol | 1 | 2017 | -2.73 (4.07) | long 12.16 | clock 7.57 | 201.4 | fold-1 |
| fold_2017 | ff_vol | 2 | 2017 | 3.53 (5.10) | long 12.16 | clock 7.57 | 208.8 | fold-2 |
| fold_2017 | rec_vol | 1 | 2017 | -10.52 (3.55) | long 12.16 | clock 7.57 | 461.4 | fold-3 |
| fold_2017 | rec_vol | 2 | 2017 | -8.04 (4.61) | long 12.16 | clock 7.57 | 415.3 | fold-4 |
| fold_2017 | ff_vol | 1 2 | 2017 | 0.40 (3.13) | long 12.16 | clock 7.57 | 208.8 | fold-5 |
| fold_2017 | rec_vol | 1 2 | 2017 | -9.28 (1.24) | long 12.16 | clock 7.57 | 461.4 | fold-6 |

## Problems and limits found in this pass

- none
