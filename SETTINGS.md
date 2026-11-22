# Run settings reference

Every setting a run can use: what it does, its default, the values allowed, and what the current run configs use.
Checked against the code on 2026-11-16. If this file and `config.erl` ever disagree, `config.erl` is right.

Contents

1. [How settings reach a run](#1-how-settings-reach-a-run)
2. [Starting an experiment](#2-starting-an-experiment)
3. [Units and costs](#3-units-and-costs)
4. [Settings](#4-settings)
   - 4.1 [Cost and account](#41-cost-and-account)
   - 4.2 [Data, spans and episodes](#42-data-spans-and-episodes)
   - 4.3 [Simulator rules](#43-simulator-rules)
   - 4.4 [Fitness](#44-fitness)
   - 4.5 [Selection and evolution](#45-selection-and-evolution)
   - 4.6 [Run length and population size](#46-run-length-and-population-size)
   - 4.7 [Validation and logging](#47-validation-and-logging)
   - 4.8 [Sensors](#48-sensors)
   - 4.9 [Network and neurons](#49-network-and-neurons)
   - 4.10 [Mutation](#410-mutation)
   - 4.11 [Weight tuning](#411-weight-tuning)
   - 4.12 [Substrate encoding](#412-substrate-encoding)
   - 4.13 [Watchdog](#413-watchdog)
   - 4.14 [System](#414-system)
   - 4.15 [Legacy mode only](#415-legacy-mode-only)
   - 4.16 [Set by the system: do not set](#416-set-by-the-system-do-not-set)
5. [What the logged statistics mean](#5-what-the-logged-statistics-mean)
6. [The decision interval](#6-the-decision-interval)
7. [Pitfalls](#7-pitfalls)
8. [Checklist for a new run](#8-checklist-for-a-new-run)

---

## 1. How settings reach a run

- `config.erl` holds the **defaults only**. Each setting is a function such as
  `account_spread() -> get_val(account_spread, 0.0).`, which reads a value from a shared table and falls back to the
  default.
- The runs of an experiment are a list returned by `exp_runner:get_run_configs/0`:
  ```erlang
  [{1, [{Key, Value}, ...]},
   {2, [{Key, Value}, ...]},
   ...]
  ```
  Runs execute in order. Each run continues from the previous run's population.
- **Before each run the table is cleared** (`config:clear()`), and then that run's list is loaded
  (`exp_runner:apply_run_configs/3`). A setting not in a run's list goes back to the `config.erl` default. It does
  **not** carry over from the run before. This is why every run in `exp_runner.erl` repeats `Base`.
- **If a key appears twice in one list, the later one wins** (values go into an ETS `set`). So
  `Base ++ [{account_spread, X}]` overrides whatever `Base` says.
- Any key that `config.erl` reads with `get_val` can be put in a run's list.

**Where the run lists live** (`exp_runner.erl`):

| Function | Used when | What it is |
|---|---|---|
| `reference_run_configs/0` | `DXNN_TEST_ARM` is unset | Four runs: free (30 generations), ramp (60), hold, hold. Trial 7 used this. |
| `reference_base/2` | Inside the above, and as the base of the arms | The settings every run shares. |
| `arm_run_configs/1` | `DXNN_TEST_ARM` is set | Named setups: `ff_level`, `rec_level`, `ff_vol`, `rec_vol`, `fold_2015`, `fold_2016`, `fold_2017`, `<arm>+<fold>`, `night`, `pilot`. |
| `get_run_configs_legacy/0` | Never, unless you point `get_run_configs/0` at it | The old 12-phase legacy-mode curriculum. Kept for reference. |

**To make a new run**, either edit `reference_run_configs/0` or `reference_base/2`, or add a clause
`arm_run_configs("my_name") -> ...` **above** the generic `arm_run_configs(Name)` clause (copy the `"night"` and
`"pilot"` clauses) and launch with `DXNN_TEST_ARM=my_name`. A new arm keeps the earlier setups reproducible.

---

## 2. Starting an experiment

**Environment variables**

| Variable | Effect |
|---|---|
| `DXNN_TEST_ARM` | Chooses the run list (above). Unset or empty: `reference_run_configs/0`. An unknown name stops the experiment at start. |
| `DXNN_EPISODE_SEED` | Integer. Makes the episode draws repeatable. Each run gets `{episode_seed, {Seed, RunIndex}}`. |
| `DXNN_EVOLUTION_SEED` | Integer. Makes the first population, selection and mutation repeatable. Each run gets `{evolution_seed, {Seed, RunIndex}}`. |

**Launch** (from `DXNN-Trader-v2-training-core/`; one VM at a time on the Mac):

```sh
DXNN_TEST_ARM=rec_vol DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=2 caffeinate -is erl +Bd +scl false -noshell \
  -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." \
  > run.out 2>&1
```

- `+scl false` matters. Without it the VM ends up on two cores (`REVIEW.md` 8.20).
- `launcher:start()` calls `exp_runner:start(new_evo)`. Under the cloud worker it resumes the saved experiment instead.

**Start modes** (`exp_runner:start/1`):

| Mode | What it does |
|---|---|
| `new_evo` | A new population and a new lineage. This is what the launcher uses. |
| `fresh` | The same as `new_evo` for population creation. |
| `{evo, SourcePopId}` | Starts from an existing population and keeps its lineage. |

**Where results go:** `logs/Benchmarker/generation_summary.log` (`GEN_SUMMARY` and `FAMILY_SUMMARY` lines),
`logs/Benchmarker/agent_trades.log` (one `FITNESS_EVAL` line per evaluation), `logs/exp_runner/exp_runner.log` (each
run's settings), and Mnesia (the populations).

---

## 3. Units and costs

- **Prices and spreads are in price units.** For EUR/USD, 1 pip = `0.0001`.
- `account_spread` and `validation_spread` are the cost of **one round trip** (open plus close), charged when a
  position is opened.

| Cost | Value to set | Where the number comes from |
|---|---|---|
| Free | `0.0` | Runs 1 and 2 of the curriculum. |
| A quarter pip | `0.000025` | The `Target` in `exp_runner.erl`. What validation charges now. |
| Half a pip | `0.00005` | The central case in older sections of `REVIEW.md`. |
| Real EUR/USD, London hours, 2013–2017, with commission | `0.000075` | `REVIEW.md` 8.28 |
| Real EUR/USD, all hours, 2013–2017, with commission | `0.000084` | `REVIEW.md` 8.28 |
| Real EUR/USD, all hours, 2012, with commission | `0.000139` | `REVIEW.md` 8.28 |

**Position size** (episode mode): units = `buy_money_fixed × account_leverage / price` = 100 × 50 / price. That is
about 4,400 EUR at 1.13, so **1 pip ≈ $0.44**. The `P/L` column is in dollars on that size. Every episode starts from a
fresh `account_initial_balance`.

---

## 4. Settings

Columns:
- **Default**: the value in `config.erl`. Many defaults are *not* what the runs use.
- **Current runs**: the value in `reference_base/2` or the arms. "–" means the runs leave the default.
- **Fixed at creation**: only read when agents or the population are first built. Set it in run 1 and keep it the same
  in every run.

### 4.1 Cost and account

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `account_spread` | `0.0` | run 1: `0.0`; run 2: ramp; runs 3+: `0.000025` | Price units per round trip, `0.0`–`0.001` | The training cost. In episode mode `cost_schedule` replaces it when not empty, and `validation_spread` replaces it on validation episodes. |
| `validation_spread` | `undefined` | `0.000025` | `undefined` (same as training) or price units | The cost on validation episodes. Set it to the real cost so validation always reads at the real cost, even while training is still free. |
| `cost_schedule` | `[]` | run 2: `[{0,0.0},{60,0.000025}]` | `[]` or `[{Generation,Spread},...]` in rising generation order | The training cost changes along straight lines between the points and keeps the last value afterwards. Applied at the start of every generation. Generations count from 0 in each run. |
| `buy_money_fixed` | `100` | – | Any positive number | Margin per position. Position size = this × leverage / price. |
| `account_leverage` | `50` | – | 1–500 | Leverage. |
| `account_initial_balance` | `300` | – | Any positive number | Starting balance, reset at the start of each episode. |
| `account_extinction_threshold` | `1` | – | ≥ 1 | Equity floor. An agent whose equity falls to it is "ruined" for that evaluation. 1 means it practically never happens. |
| `account_lot_size` | `10000` | – | 1000, 10000, 100000 | Legacy. Not used for the episode-mode position size. |
| `account_margin` | `0` | – | 0–1.0 | Legacy margin requirement. |
| `order_size_percentage` | `0.2` | – | 0.01–1.0 | Not used: the position size comes from `buy_money_fixed`. |
| `min_profit_threshold` | `0.00015` | – | 0.00001–0.01 | Legacy. |
| `pip_size` | `0.0001` | – | `0.0001`, or `0.01` for yen pairs | Only for printing results in pips and for `alpha_pips_t`. |

### 4.2 Data, spans and episodes

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `evaluation_mode` | `legacy` | `episodes` | `legacy`, `episodes` | `episodes`: every generation draws fresh episodes from `train_span`, the same ones for every agent. Everything in this file assumes `episodes` unless it says otherwise. |
| `primary_currency_pair` | `'EURUSD1'` | – | `'EURUSD1'`, `'EURUSD1_LIVE'`; `'EURUSD15'`, `'EURUSD30'`, `'EURUSD60'` are listed but **no such tables exist** in `fx_tables/` | Which price table the simulator steps through. Every run so far has used one-minute bars (section 6). |
| `train_span` | `{{2022,10,3},{2024,6,28}}` | `{{2012,1,2},{2016,12,31}}` | `{{Y,M,D},{Y,M,D}}` (inclusive), `{bars_back,StartBack,EndBack}`, or `all` | Where training episodes are drawn from. |
| `val_span` | `{{2024,7,8},{2024,12,31}}` | `{{2017,1,1},{2017,12,31}}` | Same formats as `train_span` | Where validation episodes come from. Never used for selection. Keep a gap after `train_span`. |
| `episode_bars` | `800` | `1000` | 200–5000 | Bars per episode, including warm-up. |
| `episode_warmup_bars` | `0` | `30` | 0–500 | The first bars of each episode are forced flat and not scored, so the network and sensors can settle. |
| `episodes_score` | `6` | `32` | 2–64 | Episodes per scoring evaluation. This is the fitness selection sees. More episodes: less luck, more time. |
| `episodes_val` | `12` | `400` (night: `120`) | ≥ 1 | Maximum number of validation episodes, tiled over `val_span`. More than fit means the whole span (about 369 in 2017). Use all of them: a subset is the same subset every time, so its luck repeats. |
| `episodes_tune` | `3` | `1` (not run) | 1–16 | Tuning episodes. Only used when `episode_tuning` is true. |
| `episode_anchor_step` | `15` | – | 1–240 | Episodes can start every N bars. |
| `episode_min_history` | `1000` | – | ≥ the longest sensor window | Bars kept before the first possible episode start. |
| `episode_seed` | `undefined` | from `DXNN_EPISODE_SEED` | `undefined` (random), integer, or `{Seed,RunIndex}` | Repeatable episode draws. An integer gives every chained run the same draws. `{Seed,RunIndex}` gives each run its own. |

**Data in `fx_tables/EURUSD1.txt`:** 2012 to 2024 of one-minute EUR/USD (4.78 million bars). Its clock is New York time
plus 5 hours (UTC in winter, UTC+1 in summer, changing on the US dates; `REVIEW.md` 8.29). **2018–2020 is held back
as the forward test** (`REVIEW.md` 8.16): don't put it in `train_span` or `val_span` unless you mean to spend it.
January 2025 onward is not in the loaded file.

### 4.3 Simulator rules

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `episode_isolation` | `false` | `true` | `true`, `false` | `true`: the network is fully reset at every episode boundary (recurrent state, plastic weights, `diff_product` memory), so each episode starts like a fresh network. With `false`, episode order changed results for about half of all agent-episodes (`REVIEW.md` 8.23). **Use `true`.** |
| `sense_before_trade` | `false` | `true` | `true`, `false` | `true`: a bar's trade is not acted on until every sensor has read that bar. Without it, the same agent can trade differently on the same episodes. **Use `true`.** |
| `rollover_rule` | `false` | arms: `true` | `true`, `false` | No position is opened, closed or reversed during hours 21–23 on the file's clock. The position held is kept. The spread is widest around the 17:00 New York rollover. |
| `late_fill` | `false` | arms: `false` | `true`, `false` | `true`: fill one bar late, at the next close. A stress test, not for training. |

### 4.4 Fitness

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `fitness_function` | `total_profit` | reference: `alpha_t`; arms: **`alpha_pips_t`** | Episode scores: `alpha_pips_t`, `alpha_t`, `alpha_money_t`, `alpha_lcb`, `alpha_mean`. Single-window scores (averaged over episodes): `total_profit`, `time_weighted`, `sharpe_ratio`, `profit_factor`, `total_return`, `sortino_ratio`, `calmar_ratio`, `curriculum_risk_penalty`, `phase0_close_trades`, `phase1_profit_risk`, `curriculum_trade_quality_profit`, `phase2_profit_optimization`, `phase_size_reward`. An unknown name falls back to `time_weighted` with a warning. | What selection ranks agents by. See below. |
| `min_trades_per_episode` | `2` | `2` | 0–50, fractions allowed | An agent that opens fewer positions per episode on average is ineligible: it scores −1000 plus its entries, below every eligible agent. Keeps "never trade" out. 0.5 = 16 entries in a 32-episode evaluation. |
| `alpha_pips_sd_floor` | `9.0` | – | Pips, > 0 | `alpha_pips_t` only: the smallest standard deviation it divides by. 9 pips is a no-skill agent in the market 3% of the time on the train years. |
| `alpha_money_sd_floor` | `0.15` | – | > 0 | `alpha_money_t` only: the same floor in alpha units. |
| `fitness_lcb_z` | `1.0` | – | 0–3 | `alpha_lcb` only: z in mean − z·sd/√K. |

**The episode scores** (`fitness_functions.erl`; K = number of episodes):

| Name | Formula | Notes |
|---|---|---|
| `alpha_pips_t` | mean(m) / max(sd(m), floor) × √K, where m = an episode's money after cost with the span's drift removed, in pips | **Recommended.** It tracks money (rank correlation 0.99–1.00 in `REVIEW.md` 8.27), and the agent can't inflate it by choosing when to be in the market. |
| `alpha_t` | mean(z) × √K | **Flawed** (8.25): z divides by the agent's own exposure, so "take the gain, hold the loss" scores well and loses money. Trial 7 used it. |
| `alpha_money_t` | mean(alpha) / max(sd(alpha), floor) × √K | Failed the money test in 8.26 (+0.30 correlation). Not used. |
| `alpha_lcb` | mean(alpha) − z × sd(alpha) / √K | Favours agents that barely trade while there's no skill yet. |
| `alpha_mean` | mean(alpha) | Noise grows with activity, so it favours busy agents. |

An evaluation in which the account is wiped out scores −2000. An ineligible agent scores −1000 plus its entries.

### 4.5 Selection and evolution

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `population_selection_f` | `competition` | `rank` | `rank`, `competition`, `top3` | How survivors and offspring are chosen. **Use `rank` with any score that can be negative** (all the episode scores). |
| `survival_percentage` | `0.5` | `0.5` | 0.1–0.9 | Share of each generation that survives. |
| `selection_memory` | `0` | `0.85` | `0` (off) or 0 < W < 1 | Off: rank by this generation's score alone. On: rank by a decaying average of each agent's score minus its generation's median. 0.85 ≈ the last 7 generations. Cuts the luck of a single episode draw. The logs still show raw scores. |
| `selection_memory_prior` | `1.0` | – | ≥ 0 | How many evaluations' worth of "no better than the median" a newcomer starts with. 1 = its first result counts half. |
| `population_evo_alg_f` | `generational` | – | `generational`, `steady_state` | Generational: the whole generation is evaluated, then selected. Everything here assumes generational. Fixed at creation. |
| `population_fitness_postprocessor_f` | `none` | `none` | `none`, `size_proportional`, `size_first` | Adjusts fitness by network size before selection. |
| `families` | `1` | reference: `3`; arms: `1`; night: `4` | 1 or more | N separate species of `specie_size_limit` agents that never mix, all on the same episodes. Each is an independent attempt. A generation takes N times as long. Fixed at creation. |
| `evolution_seed` | `undefined` | from `DXNN_EVOLUTION_SEED` | `undefined`, integer, or `{Seed,RunIndex}` | Repeatable first population, selection and mutation. |
| `heredity_types` | `[darwinian, lamarckian]` | – | `darwinian`, `lamarckian` | Whether tuned weights are inherited. Only matters with weight tuning on. |

### 4.6 Run length and population size

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `generation_limit` | `1` | `30`, `60`, then `100000` (capped by time) | ≥ 1 | Generations in this run. |
| `run_time_limit_s` | `undefined` | reference hold runs: `10800`; night: `14400` | `undefined` or seconds | The run also ends at the first generation boundary after this much wall time. A run ended this way skips its last-generation validation. |
| `evaluations_limit` | `100000000000` | – | ≥ 1 | Maximum evaluations per run. Effectively off. |
| `specie_size_limit` | `2` | `48` | 1–100 | Agents per family. |
| `init_specie_size` | `2` | `48` | 1–50 | Starting agents per family. Fixed at creation. |
| `tot_runs` | `1` | – | 1–100 | Legacy benchmark setting. |

**Rough timings** (32 episodes of 1,000 bars): trial 7 run 1, 144 agents with one-neuron networks on the Mac, took
about 45 s per generation, and its generation-0 validation pass took about 8 minutes. The night run (`REVIEW.md` 8.27),
192 agents on a 12-vCPU GCP VM, took 117 s per generation at the start and about 180 s at the end as networks grew. A
generation with a validation pass took 586–832 s. Time grows with agents, episodes and network size.

### 4.7 Validation and logging

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `validate_population_every` | **`1`** | `1000000` (= generation 0 only); night: `0` | `0` (off) or N | Every N generations each agent is also run on the validation episodes, for the log only. **The default validates every generation**, which is very slow. |
| `validate_last_generation` | `false` | arms: `true` | `true`, `false` | Also validate at the last generation of each run. Skipped when `run_time_limit_s` ends the run. |
| `extra_baselines` | `false` | `true` | `true`, `false` | `GEN_SUMMARY` also scores never trading, always long, 60-bar momentum and the time-of-day rule on the same episodes. |
| `generation_test` | **`true`** | `false` | `true`, `false` | `true`: every 500 evaluations, run each family's best agent alone on validation. Nothing reads the result, and it cost 8% of a night. **Set it to `false`.** |
| `validation_series` | `false` | – | `true`, `false` | Keep each validation episode's position at every bar. Costs about 15 MB per agent per 370 episodes. Only replay tools need it. |

### 4.8 Sensors

Sensors are stored in each agent when created, so changing these affects new agents and new sensors only.

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `plt_specs` | `[]` | `[{15,32},{60,24},{240,30},{1440,20}]` | `[]` or `[{TF,N},...]`, with TF in 15, 60, 240, 1440 minutes | `fx_PLT` price lists: the last N−1 **completed** TF-minute closes plus the **current one-minute close**. So they change every minute (section 6). |
| `plt_encoding` | `level` | arms: `level` or `vol` | `level`, `vol` | `level`: scaled by the price level. `vol`: scaled by trailing volatility and clipped to ±5, which makes calm and busy years comparable. |
| `indicator_sensor` | `false` | `true` | `true`, `false` | `fx_IND`, 12 numbers, about 0 ± 1, clipped to ±5: price change over 5, 15, 60, 240, 1440 and 5760 bars; distance from the 60-, 240- and 1440-bar averages; two activity ratios; position in the last 24 hours' range. |
| `clock_sensor` | `false` | `true` | `true`, `false` | `fx_Clock`: time of day and place in the week as sines and cosines. |
| `clock_harmonics` | `1` | `2` | 1 or more | 1: four values. N: 2N + 2 values. 2 lets one neuron pick out a window of a few hours. |
| `position_sensor` | `false` | arms: `true` | `true`, `false` | `false`: the account sensor is `fx_Internals` (position and open profit), added by mutation only. `true`: `fx_Position` instead (position only: −1, 0 or 1), which new agents start with. |
| `internal_sensor_dimensions` | `3` | – | 1–10 | Size of `fx_Internals`. |
| `init_sensors` | `one` | `series_clock` | `one`, `series`, `series_clock` | What a new agent starts with. `one`: one price sensor drawn at random (a whole run once ended up reading only one sensor, 8.17). `series`: every `fx_PLT` list and `fx_IND`. `series_clock`: those plus `fx_Clock`. |
| `pli_resolutions` | `[90, 270]` | `[]` | List of bar counts | `fx_PLI`: raw lists of the last N one-minute closes. Slow and no measured gain, so the runs turn it off. |
| `pci_horizontal_resolutions` | `[30]` | `[]` | List | `fx_PCI` chart-image sensor (horizontal × vertical inputs). The runs turn it off. |
| `pci_vertical_resolutions` | `[20]` | – | List | See above. |
| `pli_1m_resolutions`, `pci_1m_horizontal_resolutions`, `pci_1m_vertical_resolutions` | `[5,10,20]`, `[20,30]`, `[10,15]` | – | Lists | The same, for `morphology` `forex_trader_1m` only. |

The morphology always offers the account sensor (`fx_Internals` or `fx_Position`), so the `add_sensor` mutation can
add it to any agent.

### 4.9 Network and neurons

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `agent_encoding_types` | `[substrate]` | `[neural]` | `neural`, `substrate` | `neural`: a direct network. `substrate`: HyperNEAT. Fixed at creation. |
| `morphology` | `forex_trader` | – | `forex_trader`, `forex_trader_1m` | The sensors and actuator on offer. `forex_trader_1m` has only raw lists, charts and `fx_Internals`. Fixed at creation. |
| `connection_architecture` | `recurrent` | arms: `recurrent` or `feedforward` | `recurrent`, `feedforward` | Whether mutation may add recurrent links. Fixed at creation. |
| `init_hidden_neurons` | `0` | arms: `3` | 0 or more | 0: a new agent is one output neuron. H: H hidden neurons that each read every starting sensor, feeding one output neuron. |
| `init_recurrent` | `false` | arms: `true` for `rec_*` | `true`, `false` | With `init_hidden_neurons` > 0, each hidden neuron also reads its own previous output. |
| `neural_activation_functions` | `[tanh, sin, cos, gaussian, absolute, sgn, log, sqrt, linear]` | `[tanh, linear]` | `tanh`, `sin`, `cos`, `gaussian`, `absolute`, `sgn`, `log`, `sqrt`, `linear`. `quadratic`, `sigmoid`, `sigmoid1`, `multiquadric`, `bin` and `trinary` exist but **crash**. | Activation functions new neurons and `mutate_af` choose from. Set it in run 1. |
| `neural_aggregation_functions` | `[dot_product, mult_product, diff_product]` | `[dot_product]` | `dot_product`, `mult_product`, `diff_product` | How a neuron combines its inputs. `diff_product` has memory. |
| `neural_plasticity_functions` | 12 rules | `[none]` | `none`, `hebbian`, `hebbian_w`, `ojas`, `ojas_w`, `self_modulationV1`–`V6`, `neuromodulation` | Weight changes during an evaluation. `none` = fixed weights. Fixed at creation. |

### 4.10 Mutation

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `mutation_operators` | weighted list (`config.erl`) | see below | List of `{Operator, Weight}` | Each offspring gets mutations drawn by weight. |
| `tot_topological_mutations_functions` | `[{ncount_exponential,0.5}]` | – | `{ncount_exponential,P}`, `{ncount_linear,M}` | Mutations per offspring: a random number from 1 to round(neurons^P), or up to M per neuron. Too many scramble offspring. |

**Operators available** (`genome_mutator.erl`):

| Group | Operators |
|---|---|
| Weights and functions | `mutate_weights`, `mutate_af`, `mutate_pf`, `mutate_aggrf`, `mutate_plasticity_parameters`, `add_bias`, `remove_bias` |
| Growth | `add_neuron`, `add_inlink`, `add_outlink`, `outsplice` |
| Sensors and actuator | `add_sensor`, `add_sensorlink`, `add_actuatorlink`. `add_actuator` always fails: forex has one actuator. |
| Removal (neural only) | `remove_neuron`, `remove_inlink`, `remove_outlink`, `remove_sensorlink`, `remove_sensor` |
| Substrate only | `add_cpp`, `add_cep` |
| Search settings | `mutate_heredity_type`, `mutate_tot_topological_mutations`, `mutate_tuning_annealing`, `mutate_tuning_selection` |

**The current mix** (`reference_base/2`; weights are relative):

```erlang
{mutation_operators, [
    {mutate_weights,350}, {mutate_af,25}, {add_bias,25}, {remove_bias,10},
    {add_neuron,60}, {add_inlink,50}, {add_outlink,40}, {outsplice,15},
    {add_sensor,10}, {add_sensorlink,30}, {add_actuatorlink,20},
    {remove_neuron,20}, {remove_inlink,15}, {remove_outlink,15}, {remove_sensorlink,10}, {remove_sensor,4}]}
```

With tuning off, `mutate_weights` is the only thing that changes weights, which is why it has half the weight.
`mutate_pf`, `mutate_aggrf` and `mutate_plasticity_parameters` are left out because each of those lists has only one
choice.

### 4.11 Weight tuning

Off in every current run. Only read when `episode_tuning` is `true`, or in legacy mode.

| Setting | Default | Current runs | Options | What it does |
|---|---|---|---|---|
| `episode_tuning` | `false` | `false` | `true`, `false` | `true`: before scoring, the agent tunes its weights on the tune episodes. It took two thirds of the compute in the first trial and nothing it found carried over. |
| `tuning_duration` | `{const,10}` | `{const,4}` | `{const,N}`, `{wsize_proportional,N}`, `{nsize_proportional,N}` | Tuning attempts per evaluation: fixed, or scaled by weights or neurons. |
| `tuning_selection_functions` | all eight | – | `dynamic`, `dynamic_random`, `active`, `active_random`, `current`, `current_random`, `all`, `all_random` (`lastgen` is not implemented) | Which neurons get perturbed. |
| `annealing_parameters` | `[0.1, 0.5, 1]` | – | List of numbers | How the perturbation shrinks with a neuron's age. |
| `perturbation_ranges` | `[0.1, 0.5, 1, 2]` | – | List of numbers | Size of the weight perturbations. |

### 4.12 Substrate encoding

Only when `agent_encoding_types` includes `substrate`.

| Setting | Default | Options |
|---|---|---|
| `substrate_plasticities` | `[none, modular_none, hebbian, ojas]` | `none`, `iterative` (heavy on CPU and memory), `abcn`, `modular_none`, `hebbian`, `ojas` |
| `substrate_linkforms` | `[l2l_feedforward, jordan_recurrent, neuronself_recurrent]` | `l2l_feedforward`, `fully_interconnected`, `jordan_recurrent`, `neuronself_recurrent` |
| `substrate_density` | `5` | 1–1000 (neurodes per dimension) |
| `substrate_depth` | `2` | 1–100 (hidden layers) |

### 4.13 Watchdog

Catches networks that mutation left broken. Not applied in live trading.

| Setting | Default | Options | What it does |
|---|---|---|---|
| `stall_timeout_ms` | `120000` | Milliseconds | Fail an agent when no bar completes for this long. |
| `progress_every_bars` | `50` | 0 or N | How often the cortex reports progress. 0: an evaluation must finish within `stall_timeout_ms`. |
| `runaway_queue_limit` | `5000` | Positive integer | Fail an agent as soon as any of its processes has more messages waiting than this. |
| `evaluation_timeout_ms` | `600000` | – | **No longer used.** |

### 4.14 System

| Setting | Default | Options | What it does |
|---|---|---|---|
| `checkpoint_enabled` | `auto` | `auto`, `true`, `false` | Saving checkpoints. `auto` = on when the run detects AWS. |
| `population_id` | `test` | Atom or string | Normally generated by `exp_runner`. Leave it alone. |
| `fx_tables_directory` | `"fx_tables/"` | Path | Where price tables are read from. |
| `source_directory` | `"fx_tables/"` | Path | Same. |
| `actuator_debug_tag` | `false` | `true`, `false` | Prints every trade. Very verbose. |
| `sensor_debug_tag` | `false` | `true`, `false` | Prints sensor output. Very verbose. |

### 4.15 Legacy mode only

Ignored when `evaluation_mode` is `episodes`.

- **Training window:** `gt_start` (1000), `gt_end` (200), `bench_start` (200), `bench_end` (`last`), all counted in bars
  back from the end of the table.
- **`time_weighted` fitness:** `fitness_time_weighted_enabled`, `fitness_discount_rate`, `fitness_loss_discount_rate`,
  `fitness_realized_bonus`, `fitness_loss_penalty`, `fitness_unrealized_penalty`, `fitness_trades_bonus`.
- **Curriculum fitness functions:** `fitness_curriculum_generation`, `fitness_curriculum_unrealized_discount`,
  `fitness_curriculum_pnl_scale`, `fitness_curriculum_trades_per_1000`, `fitness_curriculum_generation_focus`,
  `fitness_curriculum_drawdown_floor`, `fitness_curriculum_drawdown_penalty`, `fitness_phase1_pscore_weight`,
  `fitness_phase1_tradescore_weight`, `fitness_target_trades_per_1000`, `fitness_overtrade_thresh_per_1000`,
  `fitness_overtrade_lambda`, `fitness_curriculum_g1`, `fitness_curriculum_g2`, `fitness_no_trade_penalty`,
  `fitness_dom_scale`, `fitness_bigwin_pct`, `fitness_bigwin_sum_scale`, `fitness_target_bigwins_per_1000`,
  `fitness_unreal_discount_no_trades`, `fitness_dd_lambda_early`, `fitness_dd_lambda_late`.

These single-window functions can still be named as `fitness_function` in episode mode. They are then computed per
episode and averaged, but none of them removes market drift.

### 4.16 Set by the system: do not set

`episodes_generation`, `episodes_generation_started_ms`, `episodes_prev_scores`, `episodes_score_set`,
`episodes_validation_extra`, `episodes_validation_params`, `episodes_validation_rules`, and `test_arm` (copied from
`DXNN_TEST_ARM`). The population monitor and `episodes.erl` write these during a run.

---

## 5. What the logged statistics mean

Per episode (`fx:ep_stat/4`). u_t is the position held from bar t to t+1 (−1, 0 or 1); r_t = close[t+1] − close[t];
mu is the average change per bar over the whole span, fixed before any agent runs.

| Statistic | Definition | Reading |
|---|---|---|
| gross | Σ u·r | Money per unit of position, before cost. |
| timing | gross − mu·Σu − spread × entries | Money after cost with the market's drift removed. Being long in a rising period earns nothing. |
| alpha | timing / √(Σ r²) | Timing in units of the episode's market movement. |
| z | timing / √(Σ u²r²) | Timing in units of its own no-skill deviation. About 0 ± 1 without skill. |
| active | Σ u²r² / Σ r² | Share of the market's movement the agent was exposed to. 1.0 = always in the market. |
| entries | Number of times a position was opened or reversed | Each one pays the spread. |
| pnl | Account equity at the end minus the starting balance | Dollars, **not** drift-adjusted. |

Per evaluation (`FITNESS_EVAL` line and the dashboard):

| Column | Meaning |
|---|---|
| `mode` | `score` = training episodes (what selection sees); `validation` = the held-out span (logged only). |
| `fitness` | The score of `fitness_function` for this evaluation. |
| `episodes` | K. |
| `mean_z`, `mean_alpha`, `std_alpha` | Averages over the K episodes. |
| `entries` | Total entries over all K episodes. Divide by K for per episode. |
| `mean_abs_exposure` | Average of |u|: how much of the time the agent holds a position. |
| `active` | Average `active`. |
| `holder_episodes` | Episodes in which the agent kept one side for more than 90% of the bars. |
| `pnl` | Sum of the episodes' dollar P/L. Divide by K for per episode; divide by about $0.44 for pips. |

**Quick checks on an agent**
- Look at **pips per trade**, not total P/L: (P/L per episode ÷ 0.44) ÷ (entries per episode). It must be above the
  real cost of about 0.8 pip.
- A `score` result chosen as the best of many agents is mostly luck. Look at the same agent's other evaluations.
- A `validation` result chosen as the best of many agents is selection on the test set.
- Compare with the baselines in `GEN_SUMMARY` (`extra_baselines`). On 2017 at 0.25 pip: always long +2.80, clock rule
  +1.40 pips per episode (`REVIEW.md` 8.27).

---

## 6. The decision interval

- **Every run so far has decided on every one-minute bar.** The simulator steps one row of the price table per network
  cycle, and the agent can change its position on any minute (`fx:ep_step/4`).
- Sensors don't set the interval. The longer-timeframe `fx_PLT` lists include the current minute's close, and
  `fx_IND` and `fx_Clock` change every minute too.
- A sensor that only changes every 15 minutes would make the *decisions* change only every 15 minutes **only if** the
  network has no recurrent links, no plasticity and no `diff_product`, and never gets the account sensor (which
  `add_sensor` can add at any time).
- Neither of these is built. A dependable 15-minute decision needs a change in the simulator:
  1. **Hold between boundaries**: in `ep_step`, keep the current position unless the bar is on a 15-minute boundary,
     the same way `rollover_rule` works. Small change. Not faster.
  2. **Step 15 rows at a time**: sensors and the network run only at the boundary. About 15 times faster, but it
     touches episode length, warm-up, drift, scoring and the baselines.
- Why it matters: a round trip costs about 170% of a typical one-minute move, but about 42% of a 15-minute move, 21% of
  an hourly one and 10% of a 4-hour one (`REVIEW.md` section 2.3).

---

## 7. Pitfalls

- **Defaults aren't the run settings.** For example, `agent_encoding_types` defaults to `[substrate]`,
  `specie_size_limit` to 2, `validate_population_every` to 1 (every generation) and `generation_test` to `true`. Start
  from `reference_base/2`, not from an empty list.
- **Settings reset between runs.** A key you put only in run 1 is back to its default in run 2.
- **Fixed-at-creation settings** (`families`, `init_specie_size`, `agent_encoding_types`, `morphology`,
  `connection_architecture`, the neuron function lists, `init_sensors`, sensor encodings) only matter in run 1. Keep
  them the same in later runs to avoid confusion.
- **`alpha_t` is flawed.** Use `alpha_pips_t` for new runs.
- **Cost:** 0.25 pip is about a third of the real cost. Set `validation_spread` to `0.000084` to see real-cost results.
- **`run_time_limit_s` skips the last-generation validation** when it ends a run.
- **Ties in a saved population:** offspring are saved carrying their parent's fitness without being evaluated. When
  choosing an agent from a saved population, take the one from the earliest generation among equal fitness values
  (`REVIEW.md` 8.27).
- **Held-back data:** 2018–2020 is the forward test. Reading it while developing uses it up.

---

## 8. Checklist for a new run

1. Copy an existing arm clause in `exp_runner.erl` and give it a new name.
2. Set `fitness_function` (`alpha_pips_t`), `train_span` and `val_span`.
3. Set the cost: `account_spread` and/or `cost_schedule` for training, and `validation_spread` at the real cost
   (`0.000084`).
4. Set the size: `families`, `specie_size_limit`, `init_specie_size`, `episodes_score`.
5. Set the length per run: `generation_limit`, optionally `run_time_limit_s`.
6. Set validation: `validate_population_every`, `validate_last_generation`, `episodes_val`.
7. Confirm `episode_isolation` and `sense_before_trade` are `true`, and `generation_test` is `false`.
8. Pick the sensors: `plt_specs`, `indicator_sensor`, `clock_sensor`, `position_sensor`, `init_sensors`.
9. Launch with `DXNN_TEST_ARM=<name>` and both seeds, so the run can be repeated.
10. If an agent is going to be judged on held-out data, write the pass rule (and how ties are broken) before the run.
