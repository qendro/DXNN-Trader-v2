# Result-folder analysis (four-arm experiment and folds, REVIEW.md 8.23)

Three files do the work:

- `pipeline.js` (Node 24): reads every result folder under a directory, starts one Erlang VM per saved population for
  the re-simulation, and writes CSV files and `summary.md`.
- `dxnn_results.erl`: the Erlang side. Restores a folder's `mnesia.nonode@nohost.backup`, picks the best N agents of a
  saved population, runs `resim:run/4` on the run's training and validation spans, and runs the six baselines on the
  same episodes under the same conditions. Also makes small test runs.
- `vm.js`: compiles the sources into a work directory, cuts the price file before 2018, and runs one VM at a time
  (`erl +Bd +scl false`) under a memory cap and a time cap.

`make_test_run.js` makes a small result folder for testing.

## Use

```bash
node analysis/pipeline.js /path/to/results --n 8 --train-k 900 --timeout-s 3600
```

`/path/to/results` holds one folder per run (or is one). Each folder: `mnesia.nonode@nohost.backup`, `run.out`,
`logs/Benchmarker/generation_summary.log`, `logs/exp_runner/exp_runner.log`. Output goes to
`/path/to/results/analysis_out` (`--out` to change), and the work files (compiled modules, the cut price file, one
restored Mnesia directory at a time) to `<out>/_work` (`--work`).

Options:

| option | default | meaning |
|---|---|---|
| `--n` | 8 | agents per saved population, best first by the run's own ranking |
| `--train-k` | 900 | training-span episodes, tiled over the run's `train_span` |
| `--conditions` | `all` | the twelve of `resim:conditions/0`, or a comma list of names such as `cost0.25_rollover_on_fill_now` |
| `--timeout-s` | 600 | a VM running longer is killed and its population is reported as failed |
| `--mem-gb` | 3.5 | a VM whose process tree uses more is killed |
| `--parallel` | schedulers | agents evaluated at once inside the VM |
| `--no-resim` | | logs only: learning curves and validation readings, no VM |
| `--target-pips` | 0.25 | the cost that marks a saved population as "at the target cost" |

A population whose job finished (`done.txt`) with the same parameters is not run again, so an interrupted pass can be
started again with the same command.

Time: a job is N × (train-k + validation episodes) × conditions × 1,000 bars. At the defaults that is
8 × (900 + 369) × 12 × 1,000 = 122 million bars, about 30 minutes per population at 70,000 bars a second (small
networks on this Mac; larger networks are slower). The twelve runs leave 36 populations at the target cost: about
18 hours. `--train-k 300` and `--conditions` with four names cut that to about 2 hours. The pipeline prints its estimate
before each job; a job longer than `--timeout-s` is killed.

## What is enforced

- A run whose training or validation span ends after 2017-12-31 (from its `config:` line in `exp_runner.log`) is
  refused: the folder is not analysed and the exit code is 3.
- The VM loads `<work>/prices/EURUSD1.txt`, a copy of `fx_tables/EURUSD1.txt` that stops at the first row of 2018.
  `dxnn_results:check_table/0` stops the VM if the loaded table holds any row of 2018 or later.
- In the VM, `check_span/1` refuses each run's spans again, and `check_episodes/2` refuses any episode whose last row
  is after 2017-12-31 or past the end of the table.
- `fx_tables/EURUSD_M1_MIDPOINT_UTC.txt` is never opened: the only price path in the code is
  `fx_tables/EURUSD1.txt` (`vm.js`, `PRICE_SOURCE`).

## Definitions

- *Best N by the run's own ranking*: `#agent.fitness`, the remembered score selection ranks by. Offspring carry their
  parent's value until evaluated, so one agent per value is kept, the one of the lowest generation (the parent).
- *Saved populations*: every run at the target cost with no cost schedule (runs 3, 4 and 5 of an arm).
- *Training rule*: 0.25 pip, with the run's own rollover and fill settings (for the arms: rollover rule on, fill now).
- *Portfolio*: the N agents with equal weight, episode by episode. Its block standard errors use the formula of
  `resim.erl` (sqrt(blocks) × sd(block money) / episodes), with each episode in the day or ISO week of its first bar.
- *Largest drawdown* (folds): of the portfolio's cumulative money over the validation episodes in date order.
- *Comparison outcomes*: (1) the final population's validation money, median agent, from the last `GEN_SUMMARY`
  validation reading; (2) the best N's median under the training rule; (3) the best N's median at 0.25 pip with the
  rollover rule on and the fill now. Per outcome: each arm-seed value, arm means, paired differences between arms over
  the seeds both have, the two main effects (mean of the two recurrent arms minus mean of the two feedforward arms; the
  same for vol and level) and the interaction ((rec_vol − ff_vol) − (rec_level − ff_level)), each with the standard
  error across seeds. Comparisons are grouped by fold and episode seed. An outcome whose values are the same CSV rows
  as an earlier outcome's (for the arms, (3) is the same condition as (2)) is listed and not counted again.

## Tested

`example_test_output/` is the output for four small runs made with `make_test_run.js` (rec_vol and ff_vol, evolution
seeds 1 and 2, episode seed 1; 24 agents, 8 score episodes, 40 validation episodes, runs 1, 3 and 4 of the arm with
two generations each), with `--n 8 --train-k 40`. Each of the eight re-simulation VMs took 156 to 183 s and peaked at
503 to 797 MB. The baselines' money on the validation episodes equals the simulator's own `GEN_SUMMARY` figures, and
every episode row rebuilds to the simulator's money. The main effects were checked on copies relabelled as the other
two arms; a span ending in 2018 was refused by the pipeline (exit 3) and by `check_span/1` and `check_episodes/2`.

## Not handled

- Full size: not run. The defaults need about 30 minutes per population; `--timeout-s 600` kills such a job, so pass a
  larger value or fewer episodes and conditions.
- Outcome (1) is the last `GEN_SUMMARY` validation reading: all agents of the last generation before its selection,
  median over those eligible on both sets. It is not a re-simulation of the saved population.
- Families (`{families, N}` > 1): stored fitness is relative to each family's median (8.18), so the best N across
  families is not a fair ranking. Not handled; the arms use one family.
- Several experiments or arms in one folder: kept and flagged, but the comparison takes the first run's arm.
- Seeds are paired by evolution-seed number; nothing checks that the episode draws really match beyond the episode seed.
- Portfolio block standard errors place a whole episode in its first bar's day or week. With episodes spread thinly
  (the test's 40) every episode is its own block and the day and week values are equal.
- Drawdown is at episode resolution over the year; the largest drawdown inside one episode is a separate column.
- With three seeds each standard error has 2 degrees of freedom (|t| > 4.30 for p < 0.05 before any correction).
- `make_test_run.js` repeats `exp_runner:start_internal/2` (not exported) in `dxnn_results:start_experiment/1`; if that
  function changes, test runs no longer match real ones.
