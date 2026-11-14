```bash
make all
make shell
```
``` erlang
make_all:all().
```

# Build once
```bash
docker build -t erlang-dev .

# Run your neural network system
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev

make:all([load]).
launcher:start().
```
``` erlang
# Inside container:
make:all().
make:all([load]).
qlog:delete_all().
mnesia:create_schema([node()]).
mnesia:start().
fx:init().
fx:start().
polis:create().
polis:start().
polis:sync().
exp_runner:start(new_evo).
exp_runner:start(fresh). 

process_monitor:log_process_info([{full_system, true}, {limit, 20}, {sort_by, message_queue}, {format, per_line}]).
process_monitor:log_process_info([{full_system, true}, {limit, 20}, {sort_by, memory}, {format, per_line}]).
process_monitor:log_process_info([{full_system, true}, {limit, 20}, {sort_by, reductions}, {format, per_line}]).

process_monitor:log_process_info([{full_system, true}, {sort_by, memory}, {format, per_line}]).

benchmarker:start(sliding_window_5, 
bench_configs:get_run_configs()).

benchmarker:start(sliding_window_5, 
benchmarker:get_run_configs()).

exp_runner:start(fresh).        % Uses configs from get_run_configs()
exp_runner:start(new_evo)      % Uses configs from get_run_configs()
exp_runner:start({evo, PopId})  % Uses configs from get_run_configs()

%%Continue by experiment id (recommended)
exp_runner:continue(<<"2026-02-22T03:38:47Z_p08s_run6">>).

%%Start a NEW experiment seeded from an old population (not resume)
exp_runner:start({evo, <<"2026-02-20T23:05:10Z_p08s_run7">>}).

make:all().
mnesia:start().
fx:start().
polis:start().
exp_runner:continue(<<"2026-02-20T22:28:05Z_p08s_run4">>).



benchmarker:start(sliding_window_5).

benchmarker:start(sliding_window_5, []).

benchmarker:start(sliding_window_5, benchmarker:get_run_configs()).

benchmarker:start(chart_plane_5x10).
# ... your neural network commands

```
## Episode-based evaluation
`exp_runner:get_run_configs/0` runs in episode mode (`{evaluation_mode, episodes}`) in three stages on the same
population: trades are free (run 1), then the cost per round trip rises every generation to a quarter pip (run 2), then
it stays there (runs 3 to 6, each limited by wall time, about 10 hours in all). Each generation draws fresh score episodes from `train_span` and an agent's fitness is measured once on them:
the t-statistic of the money it made per unit of position after cost, with the average drift of the span taken out. `val_span` is only logged. Weight tuning inside a generation is off (`{episode_tuning, false}`);
weights change through the `mutate_weights` mutation. Set `{episode_tuning, true}` and `{episodes_tune, N}` to tune on
separate tune episodes first.
Design and measurements: `REVIEW.md` (section 8 has the implementation status, 8.5 the four-run trial).

A watchdog in `exoself.erl` ends agents whose network is broken, without failing large networks for being slow:
- stalled: no bar completed for `stall_timeout_ms` (default 120000). The cortex reports every `progress_every_bars` (50) bars.
- runaway: a process of the network has more than `runaway_queue_limit` (5000) messages waiting (a neuron firing forever).

Both show up as `stalls=` and `runaways=` in the generation summary and as `timeout:` lines in `logs/System/qStatus.log`.

Since the second trial (`REVIEW.md` 8.6 and 8.7):
- More sensor types are offered next to the raw price lists and the chart image, answered from price arrays packed once at start-up (`fx_series.erl`): `fx_PLT` (price lists on 15-minute, hourly, 4-hour and daily bars, set by `plt_specs`), `fx_IND` (`indicator_sensor`, 12 indicators) and `fx_Clock` (`clock_sensor`). They are off by default and switched on in the run configs. Not available in live trading yet.
- Removal mutations are in the default operator list: `remove_neuron`, `remove_inlink`, `remove_outlink`, `remove_sensorlink`, `remove_sensor`. Each refuses a removal that would leave the network unable to run.
- Validation uses 60 episodes every 4 generations, and each `GEN_SUMMARY` line carries the plain reversal rules on the same episodes at the same cost (`rev60_score`, `rev240_score`, and the `_val` versions).

Since the third trial (`REVIEW.md` 8.9):
- The score was corrected. The old formula credited any rule that sells after a rise, whether or not it made money. `eval_protocol:g0()` now includes a random-walk check that fails if that returns.
- Cost: `account_spread` is the cost per round trip on train episodes (0.0001 is one pip). `{cost_schedule, [{Generation,Spread},...]}` changes it every generation along straight lines between the given points. `validation_spread` is the cost on validation episodes; set it to the cost of real trading.
- `GEN_SUMMARY` ends with money columns: `positive` (agents above zero after cost), `gross_pips_trade` (money before cost per trade; it must exceed the cost in pips), `net_pips_ep`, and `val_positive`, `val_net_pips_ep`, `val_spread` for validation.

Since the fourth trial (`REVIEW.md` 8.10 and 8.11):
- `{run_time_limit_s, Seconds}` ends a run at the first generation boundary after that much wall time, as if `generation_limit` had been reached. The night's length is then known in advance whatever size the networks grow to.
- `{selection_memory, 0.85}` makes selection rank by a decaying average of each agent's score relative to the median of its generation (`selection_memory.erl`), not by one generation's score. `0` switches it off. `GEN_SUMMARY` still reports the raw scores.
- `min_trades_per_episode` is 2 in every run. Fractions are allowed; 0.5 was tried in the overnight run and changed nothing (`REVIEW.md` 8.15).
- Validation runs on every episode of `val_span` (about 370 in 2017) at generation 0 of each run (`{validate_population_every, 1000000}` since 2026-11-13; it was every 40 generations, which took 27% of a night, `REVIEW.md` 8.20). `episodes_val` is an upper limit. Use all of them: a subset is the same subset at every check, so its luck repeats.
- `GEN_SUMMARY` ends with `vm_mb` and `processes` (both should stay level through a long run) and `memory_evals_avg`, `memory_evals_max` (how many evaluations selection remembers per agent).
- For a run of several hours on a Mac, start it under `caffeinate -is` and keep the machine on mains power, or it will sleep.
- Start the VM with `+scl false`: `caffeinate -is erl +Bd +scl false -noshell -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." > run.out 2>&1`. Without it the VM can drop to two scheduler threads after any stretch in which one agent runs alone, and stay there with the whole population running (`REVIEW.md` 8.20). An experiment started without the flag prints a `WARNING` line first.
- `{generation_test, false}` (in the run configs since 2026-11-13; default `true`): the population monitor no longer stops every 500 evaluations to run each family's best agent alone on the validation episodes. Nothing read the result, and it took 8% of the three-family night (`REVIEW.md` 8.20).
- Where a night goes (`REVIEW.md` 8.20): scoring 63%, validation passes 27%, the benchmark step 8%. A validation pass is the work of 11.5 generations.

Price data since 2026-11-11 (`REVIEW.md` 8.12 and 8.13):
- `fx_tables/EURUSD1.txt` is now 2012 to 2024 of the long history (4.78 million one-minute bars, about 1.2 GB once loaded). The overnight run of `REVIEW.md` 8.15 trained on 2012 to 2020 and validated on 2021 to 2024.
- Since 2026-11-12 the run configs are the forward test of `REVIEW.md` 8.16: train on 2012 to 2016, validate on 2017. 2018 to 2020 is the test: no run reads it, and one agent chosen on 2017 is replayed on it once. Four runs. Since the evening of 2026-11-12 the configs are three families (`Families = 3`, `Hold = 10800` in `exp_runner:get_run_configs/0`), about 8 hours; one family (`Families = 1`, `Hold = 3600`) takes about 2 h 40 min. The first such run learned nothing and no agent was tested (`REVIEW.md` 8.17). The three-family night of 2026-11-12 ended with one family that learned, one that began to in its last hour and one that never did; no agent cleared the gate and none was tested (`REVIEW.md` 8.19).
- `{init_sensors, series}` (in the run configs since 2026-11-12): a new agent starts with all four price lists and the indicators. With the default `one` it starts with one sensor drawn at random, and the whole population then reads that sensor for the rest of the experiment (`REVIEW.md` 8.17). Like the encoding, it is fixed when the agents are created.
- `{init_sensors, series_clock}` and `{clock_harmonics, 2}` (in the run configs since 2026-11-13): a new agent also starts with the clock, and the clock gives six values: the time of day as two sine and cosine pairs (once and twice a day) and the place in the week as one pair. It is computed from the bar's own time, London time in this file, and reads no table. No year, month or date: they never repeat, so they could only be memorised (`REVIEW.md` 8.22). Agents saved earlier keep their four-value clock.
- Plain neurons (in the run configs since 2026-11-13, `REVIEW.md` 8.22): `{neural_activation_functions, [tanh, linear]}`, `{neural_plasticity_functions, [none]}`, `{neural_aggregation_functions, [dot_product]}`. `{mutation_operators, ...}` gives `mutate_weights` half of all mutations and leaves out the operators that changed a neuron's aggregator or plasticity. `{pli_resolutions, []}` and `{pci_horizontal_resolutions, []}` remove the raw one-minute lists and the chart image. Delete these lines to get the old behaviour. All are fixed when the agents are created.
- `{families, N}` (`Families` in `exp_runner:get_run_configs/0`, default 1): N separate species of 48 agents that never mix, each selected within itself, all on the same episodes. Each gets a `FAMILY_SUMMARY` line per generation. A generation takes N times as long, so raise `Hold` with it (`REVIEW.md` 8.18). Fixed when the population is created.
- `{validation_series, false}` is the default: a validation pass no longer keeps every bar's position (about 12 MB per agent, never read during a run). `eval_protocol:evaluate_agent/2` switches it on for itself.
- To see early whether a run is learning, look at `survivor_rescore` in `GEN_SUMMARY` (survivors' score on the next generation's new episodes). Its 20-generation mean was above 0.3 from 27 minutes after the start in the run that learned, and never above 0.2 in the run that did not.
- `fx_tables/EURUSD_M1_MIDPOINT_UTC.txt` is the whole long history (2000 to November 2026). January 2025 onward is the final test: it is deliberately not in the loaded file, and is to be read once, for one chosen agent.
- `fx_tables/EURUSD1_IB_2022_2025.txt` is the earlier Interactive Brokers file (September 2022 to September 2025), kept for reference. To go back to it, rename it to `EURUSD1.txt` and restore the old periods in `exp_runner:get_run_configs/0`.
- The long history's clock is local market time, one hour ahead of UTC in summer, and February to July 2023 has missing hours. Neither was corrected: the aim is to show that an agent can trade in simulation, and both only matter for connecting to a live feed.

Since 2026-11-13, evening (`REVIEW.md` 8.23):
- `{episode_isolation, true}` (in every run config; default `false`): at each episode boundary the simulator holds the next bar until the exoself has reset the whole network: recurrent inputs back to zero, plastic weights back to where the evaluation started, no `diff_product` memory. Each episode then starts like a fresh network and its 30 warm-up bars are its own. The reset between evaluations does the same. Before, the 32 episodes of an evaluation ran back to back on one network, and the order of the episodes changed the money of about half of all agent-episodes (`REVIEW.md` 8.23).
- `{sense_before_trade, true}` (in every run config; default `false`): the simulator does not act on a bar's trade until every sensor of the agent has read that bar. Without it a sensor whose neurons reach the trade only through recurrent links can read the next bar, depending on scheduling, and the same agent trades differently from run to run.
- `exp_runner:get_run_configs/0` reads three environment variables. With none set it returns the reference experiment, unchanged.
  - `DXNN_TEST_ARM`: `ff_level`, `rec_level`, `ff_vol`, `rec_vol` (the four-arm experiment), `fold_2015`, `fold_2016`, `fold_2017` (chronological folds of `rec_level`), or `<arm>+<fold>` (e.g. `ff_vol+fold_2015`), or `night` (one long run: the `rec_vol` setup with four families of 48, 20 free and 40 ramp generations, then four runs of 45 at a quarter pip, each capped at 100 minutes; about 7 to 8 hours on the Mac, `REVIEW.md` 8.24), or `pilot` (the `rec_vol` arm cut to 6, 6 and 8 generations and 40 validation episodes, for testing a machine or the cloud path). Any other value stops the experiment at start.
  - `DXNN_EPISODE_SEED`: an integer; every run's episode draws are reproducible, and each chained run draws its own (`{episode_seed,{Seed,RunIndex}}`).
  - `DXNN_EVOLUTION_SEED`: an integer; the first population's genotypes and every generation's selection and mutation are reproducible (`{evolution_seed,{Seed,RunIndex}}`). Two runs with the same two seeds give identical `GEN_SUMMARY` score columns.
- The arms (`exp_runner:arm_run_configs/1`): one family of 48, 32 episodes, free 30 generations, ramp 60, then three runs of 50 at a quarter pip (a population saved every 50), no wall-time limit. New settings they use, all off by default:
  - `{init_hidden_neurons, 3}`: a new agent starts with three hidden neurons reading every starting sensor and one output neuron. `{init_recurrent, true}` (rec arms) gives each hidden neuron a link to itself; ff arms have `{connection_architecture, feedforward}`.
  - `{plt_encoding, vol}` (vol arms): the price lists are scaled by the trailing volatility (last day's mean one-minute move, floored at 1e-5 of the price, clipped to +/- 5) instead of the price level. Stored in each sensor.
  - `{position_sensor, true}`: `fx_Position` (the position only, -1/0/1) replaces `fx_Internals` and is among the sensors a new agent starts with.
  - `{rollover_rule, true}`: no position opened, closed or reversed on a bar in hours 21 to 23 of the file's clock. `{late_fill, true}` (off; for stress tests) fills every order at the next bar's close. Both live in `fx:ep_step/4`, so the scripted rules obey them too.
  - `{validate_last_generation, true}`: every agent is also validated at the last generation of each run.
  - `{extra_baselines, true}` (also in the reference run configs): `GEN_SUMMARY` ends with `flat_`, `long_`, `mom60_` and `clock_` `score` and `pips` columns (never trade, always long, 60-bar momentum, the time-of-day rule of 8.22), and their `_val` versions.
- Launch one run (one VM at a time on the Mac): `DXNN_TEST_ARM=rec_vol DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=2 caffeinate -is erl +Bd +scl false -noshell -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." > run.out 2>&1`. The twelve runs and the fold runs are listed in `REVIEW.md` 8.23.
- `resim.erl` re-simulates saved agents, unchanged, under other costs and fill rules, and writes per-episode and per-agent CSV files with standard errors from day and week blocks (see below).

Since 2026-11-14 (`REVIEW.md` 8.25 and 8.26):
- Every arm of `exp_runner:arm_run_configs/1` (`night`, `pilot`, `ff_level`, `rec_level`, `ff_vol`, `rec_vol` and the folds) now has `{fitness_function, alpha_pips_t}` (below). The reference configs (`reference_run_configs/0`, no `DXNN_TEST_ARM`) keep `alpha_t`.
- `GEN_SUMMARY` and `FAMILY_SUMMARY` end with `rho_score_money` (rank correlation, over the eligible agents of that generation or family, between the score and the money per episode after cost on the generation's own episodes) and `median_alpha_money` (median of the agents' mean alpha: drift-free money in units of the market's movement, whatever the fitness). If `rho_score_money` sits near zero or below while scores rise, the score has stopped meaning money (`REVIEW.md` 8.25). `analysis/pipeline.js` carries both into `learning_curve.csv` (last two columns).
- `eval_protocol:score_money_check(PopId, N, K)`: replays the best N agents of each family on K training episodes (fixed seed) at the validation cost and prints money, the score in force, `alpha_t`, `alpha_pips_t` and their rank correlations with money. Run it after any change to what agents can sense or do.
- `{fitness_function, alpha_money_t}` (new, not used by any config; failed on last night's agents): per evaluation, `mean(alpha) / max(sd(alpha), alpha_money_sd_floor) * sqrt(K)`, where an episode's `alpha` is its money after cost with the span's drift taken out, divided by the market's own movement in that episode. Unlike `alpha_t` the agent cannot raise it by choosing when to be in the market. The eligibility rule is the same. Tested in `REVIEW.md` 8.26: it passes the no-skill and hand-written exploit tests but ranks last night's agents with money at only +0.30 (needed +0.7), so it is not switched on; use `alpha_pips_t` instead (below).
- `{alpha_money_sd_floor, 0.15}` (new; only read by `alpha_money_t`): the smallest sd of the per-episode alpha the score divides by. 0.15 is the sd of a no-skill agent in the market 3% of the time (`eval_protocol:sd_floor_table/2`).
- `{fitness_function, alpha_pips_t}` (new; in every arm since 2026-11-14): the same t-statistic on each episode's money after cost with the span's drift taken out, in pips, with no per-episode division (`fitness_functions:episode_pips/1`). It passes the tests of `REVIEW.md` 8.26 including +0.73 rank correlation with money on last night's agents. `{alpha_pips_sd_floor, 9.0}` (new; only read by `alpha_pips_t`): the smallest sd in pips it divides by, the sd of a no-skill agent in the market 3% of the time on the train years. `eval_protocol:money_score_check(alpha_pips_t)` runs its tests.
- `eval_protocol:sd_floor_table(NEpisodes, CostPips)`: sd of per-episode alpha of no-skill policies in the market 0.5% to 25% of the time and always in. `eval_protocol:money_score_check()`: the no-skill, "take the gain, hold the loss" and "tiny and steady" tests of `REVIEW.md` 8.26. New scripted policies: `always_in`, `{in_market,P,Hold}`, `{gain_hold,XPips,Wait}`, `{peek,Steps}` (look-ahead; a test device only).
- `resim` `agents.csv` has four more columns at the end: `alpha_t`, `alpha_money_t` (computed whatever the fitness in force), `mean_alpha` and `alpha_pips_t`. Each episode's statistics (`fx:ep_stat/4`) also carry `timing`, the drift-free money in price units.
- `eval_protocol:g0()` fails its three constant-position checks when `{rollover_rule, true}` is in force (as in every arm), with or without these changes: a constant position cannot be opened in hours 21 to 23. All twelve pass with the rule off.

``` erlang
%% After mnesia:start(), fx:start() (wait for the table to load), polis:start():
config:init().
config:load_from_list([{evaluation_mode,episodes},{fitness_function,alpha_t},{episode_bars,1000},{episode_warmup_bars,30}]).

eval_protocol:g0().                       % integrity checks; must return ok
eval_protocol:baselines().                % never trade, always long/short, random, simple rules on train and validation
eval_protocol:legacy_windows(0.00015).    % constant-position P&L on the 12 old windows at 1.5 pips
eval_protocol:evaluate_population(PopId). % every agent on validation, with a shift test (p-value)
eval_protocol:trial_count().              % how many run configurations have been compared against validation
eval_protocol:isolation_check().          % episode isolation: B alone, A then B, C then B must be identical; must return ok
eval_protocol:fill_rule_check().          % rollover rule and one-bar-late fill on scripted rules; must return ok
eval_protocol:sd_floor_table(1280,0.25).  % sd of per-episode alpha without skill, by time in the market (the alpha_money_t floor)
eval_protocol:money_score_check(alpha_pips_t). % or alpha_money_t: no skill, take-the-gain-hold-the-loss, tiny-and-steady; load the arm's settings first
eval_protocol:score_money_check(PopId,4,600).  % best 4 of each family on 600 train episodes: money, score, rank correlation
eval_protocol:order_effect(Agent_Ids,32,[]).  % how much saved agents' money depends on episode order without isolation
eval_protocol:census(PopId).              % architecture, recurrent links, price-list encoding, sensors present and connected
eval_protocol:run_agents_on(Agent_Ids,Episodes,[{episode_isolation,true}]).  % saved agents on any episodes, any settings
resim:run({population,PopId},validation,"label").            % 12 conditions: 0.25/0.5/1 pip x rollover on/off x fill now/late
resim:run(Agent_Ids,{tile,{{2017,1,1},{2017,12,31}},400},"label",#{conditions=>[#{cost=>0.25,rollover=>true,late=>false}]}).
%% resim writes logs/Resim/<label>/episodes.csv, agents.csv, conditions.csv
```
Per-generation results are in `logs/Benchmarker/generation_summary.log` (one `GEN_SUMMARY` line per generation).
The previous 12-phase curriculum is kept as `exp_runner:get_run_configs_legacy/0`.

## Reseting Mnesia
```bash
find . -name "*.beam" -delete

fprof:trace([start, {file, "/tmp/profiling_trace.trace"}]).
fprof:trace(stop).
```
```erlang
mnesia:stop().
mnesia:delete_schema([node()]).
q().

make:all([load]).
launcher:start().
```

4. **Print the best genotype:**
   ```erlang
  
   rr("records.hrl").
   c(genotype_utils).
   % Print the best genotype from the default 'test' population
   genotype_utils:get_active_agents().
   genotype_utils:list_active_agents_info().

   genotype_utils:print_active_agent_genotypes().

   genotype_utils:active_agents_process_check().
   genotype_utils:active_agents_process_check({5.663284600923985e-10,agent}).

   qlog:print_genotype({5.660575282997371e-10,agent}).

   genotype_utils:terminate_agent({5.660525142447517e-10,agent}).



   % Get total agents in database
    genotype_utils:get_total_agents().

    % Get total agents for a specific population
    genotype_utils:get_total_agents_by_population(test).

    % List all agents grouped by population
    genotype_utils:list_agents_by_population().

   genotype_utils:print_best_genotype().

   genotype_utils:print_best_genotype(all).

   mnesia:dirty_all_keys(agent).
   genotype:print({5.668233514606392e-10,agent}).

[{5.668784164536763e-10,agent},
 {5.668784164526907e-10,agent}]
   
   % Or specify a population ID
   genotype_utils:print_best_genotype(your_population_id).
   
   % List all agents with their fitness scores
   genotype_utils:list_all_agents().
   
   % Print top N agents
   genotype_utils:print_top_agents(5).
   genotype_utils:print_top_agents(1, all).
   
   % Get agent statistics
   genotype_utils:get_agent_stats().

   BestAgentId = {5.699247180669372e-10,agent}.
   exoself:start(Best_Agent_Id, self(),benchmark).
   exoself:start(Best_Agent_Id, self(),live_trading).


   % 1. Load and start your best agent
   Best_Agent_Id = {5.693207755943648e-10,agent}.  % Update with your latest
   Agent_PId = exoself:start(Best_Agent_Id, self()).

   % 2. Monitor the agent
   is_process_alive(Agent_PId).
   is_process_alive(<0.567.0>).

   % 3. Simple test run
   run_best_agent() ->
      {atomic, Best_Agent_Id} = genotype_utils:find_best_agent(all),
      Agent_PId = exoself:start(Best_Agent_Id, self()),
      timer:sleep(30000),  % Run for 30 seconds
      Agent_PId ! {self(), terminate}.





    benchmarker:start(sliding_window_10).
    benchmarker:start(sliding_window_20).
    benchmarker:start(sliding_window_50).
    benchmarker:start(sliding_window_100).
    benchmarker:start(chart_plane_5x10).
    benchmarker:start(chart_plane_5x20).
    benchmarker:start(chart_plane_10x10).
    benchmarker:start(chart_plane_10x20).
    benchmarker:start(chart_plane_20x10).
    benchmarker:start(chart_plane_20x20).
    benchmarker:start(chart_plane_50x10).
    benchmarker:start(chart_plane_50x20).
    benchmarker:start(chart_plane_100x10).

    make:all([load]).
    fx:clear_log().
    mnesia:start().
    polis:start().
    polis:sync().
    {atomic, Best} = genotype_utils:find_best_agent(all).
    exoself:start(Best, self(), live_trading).
    <CX_ID> ! {<ActuatorPID>, sync, 0.5, 1}.
    <0.337.0> ! {<0.339.0>, sync, 0.5, 0}.

    {atomic, Best} = genotype_utils:find_best_agent(all).
    exoself:start(Best, self(), benchmark).

    live_scape:start_link().
    fx:sim(<0.323.0>).
   ```

## IB Python Service Calls

### Connection & Setup
```python
# Start the comprehensive IB service
python3 priv/ib_service.py

# Key service operations (handled automatically):
await ib_service.start_service()
await connection_manager.connect(host="host.docker.internal", port=7497, client_id=101)
await historical_loader.load_weeks_of_data("EUR.USD", weeks=4, bar_size="1 min")
```

### Trading Operations
```python
# Execute trades with risk management
await trade_executor.execute_trade("EUR.USD", "BUY", 1000, "MKT")
await trade_executor.execute_trade("EUR.USD", "SELL", 1000, "LMT", limit_price=1.0850)

# Risk management controls
trade_executor.activate_kill_switch("Emergency stop")
trade_executor.deactivate_kill_switch()
trade_executor.reset_daily_counters()
```

### Data & Monitoring
```python
# Get service status
connection_manager.get_connection_status()
trade_executor.get_trading_status()

# Live data streaming (automatic)
tick_aggregator.process_tick("EUR.USD", price, volume, timestamp)
bridge.send_ohlc_bar(ohlc_bar)
```

## Live_Scape Complete Setup & Trading Guide

### 1. System Initialization & Compilation
```erlang
% Compile all modules and load them
make:all([load]).

% Load record definitions (required for data structures)
rr("records.hrl").

% Initialize Mnesia database if not already done
mnesia:create_schema([node()]).
mnesia:start().

% Start the live trading system
{ok, Pid} = live_scape:start_link().

% Verify system is running
is_process_alive(Pid).
whereis(live_scape).
```

### 2. Python Service Setup & Market Data Loading
```erlang
% The Python IB service should be running in background
% Start it externally: python3 priv/ib_service.py

% Check if Python port handler is running
whereis(python_port_handler).

% Wait for initial market data to load (automatic via Python service)
% Check ETS table for data availability
ets:info(ohlc_data, size).

% If no data, wait a few moments for Python service to load historical data
timer:sleep(5000).
ets:info(ohlc_data, size).
```

### 3. Verify Market Data in ETS
```erlang
% Check if we have OHLC data
ets:info(ohlc_data).

% Get the latest data key
LastKey = ets:last(ohlc_data).

% Look at the most recent bar
case LastKey of
    '$end_of_table' -> 
        io:format("No data available yet~n");
    Key ->
        Bar = ets:lookup(ohlc_data, Key),
        io:format("Latest bar: ~p~n", [Bar])
end.

% Get last 10 bars to verify data flow
collect_recent_bars(ohlc_data, ets:last(ohlc_data), 10, []).
```

### 4. Start Live Simulation Mode
```erlang
% Create an ExoSelf process ID (neural network coordinator)
ExoSelfPid = self().

% Start live simulation - this connects the system to live data
live_scape ! {ExoSelfPid, live_sim}.

% The system is now ready to receive sensor requests and trade signals
```

### 5. Neural Network Sensor Operations (Market Data Access)
```erlang
% Test sensor functionality - get price list for neural network
% Request 100 bars of close prices (list_sensor format)
live_scape ! {self(), sense, ohlc_data, close, [100, list_sensor], undefined, undefined}.

% Wait for response
FromPid = whereis(live_scape),
receive
    {FromPid, PriceList} ->
        io:format("Received ~p price points~n", [length(PriceList)]),
        io:format("Latest prices: ~p~n", [lists:sublist(PriceList, 5)])
after 5000 ->
    io:format("Timeout waiting for sensor data~n")
end.

% Test graph sensor (for substrate neural networks)
live_scape ! {self(), sense, ohlc_data, close, [20, 10, graph_sensor], undefined, undefined}.

FromPid2 = whereis(live_scape),
receive
    {FromPid2, GraphData} ->
        io:format("Received graph data with ~p points~n", [length(GraphData)])
after 5000 ->
    io:format("Timeout waiting for graph sensor data~n")
end.

% Get internal state (position, entry price, P&L)
live_scape ! {self(), sense, internals, []}.

FromPid3 = whereis(live_scape),
receive
    {FromPid3, [Position, EntryPrice, PreviousPC]} ->
        io:format("Position: ~p, Entry: ~p, Previous P&L: ~p~n", [Position, EntryPrice, PreviousPC])
after 5000 ->
    io:format("Timeout waiting for internals~n")
end.
```

### 6. Direct ETS Data Access (Advanced)
```erlang
% Get the latest OHLC data key
LastKey = ets:last(ohlc_data).

% Look up specific OHLC bar by key {Symbol, Timestamp}
% Note: Timestamp format is ISO string like "2024-01-01T10:00:00"
case LastKey of
    '$end_of_table' -> 
        io:format("No data in table~n");
    {Symbol, Timestamp} ->
        io:format("Latest key: ~p~n", [{Symbol, Timestamp}]),
        Bar = live_scape:lookup(ohlc_data, LastKey),
        io:format("Latest bar: ~p~n", [Bar])
end.

% Navigate through data
NextKey = live_scape:next(ohlc_data, LastKey).
PrevKey = live_scape:prev(ohlc_data, LastKey, prev, 1).

% Get multiple previous bars
PrevKey10 = live_scape:prev(ohlc_data, LastKey, prev, 10).

% Check table size and info
ets:info(ohlc_data, size).
ets:info(ohlc_data, memory).

% WARNING: Only use this for small datasets
% ets:tab2list(ohlc_data).  % Shows all data - can be large!
```

### 7. Trading Operations (Live Trading Signals)
```erlang
% IMPORTANT: Ensure Python IB service is connected to Interactive Brokers
% and ALLOW_LIVE_ORDERS environment variable is set if using live trading

% Send BUY signal (go long)
live_scape ! {self(), trade, ohlc_data, 1}.

% Wait for trade confirmation
FromPid4 = whereis(live_scape),
receive
    {FromPid4, Fitness, Halt} ->
        io:format("Trade result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for trade confirmation~n")
end.

% Send SELL signal (go short)
live_scape ! {self(), trade, ohlc_data, -1}.

FromPid5 = whereis(live_scape),
receive
    {FromPid5, Fitness, Halt} ->
        io:format("Trade result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for trade confirmation~n")
end.

% Close position (exit current trade)
live_scape ! {self(), trade, ohlc_data, 0}.

FromPid6 = whereis(live_scape),
receive
    {FromPid6, Fitness, Halt} ->
        io:format("Close position result - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 10000 ->
    io:format("Timeout waiting for close confirmation~n")
end.

% Check current position and P&L
live_scape ! {self(), sense, internals, []}.

FromPid7 = whereis(live_scape),
receive
    {FromPid7, [Position, EntryPrice, PreviousPC]} ->
        io:format("Current Position: ~p~n", [Position]),
        io:format("Entry Price: ~p~n", [EntryPrice]),
        io:format("Previous P&L Change: ~p~n", [PreviousPC])
after 5000 ->
    io:format("Timeout waiting for position info~n")
end.
```

### 8. Complete Trading Session Example
```erlang
% Complete workflow from start to trade execution
make:all([load]).
rr("records.hrl").
mnesia:start().

% Start live system
{ok, Pid} = live_scape:start_link().

% Wait for data to load
timer:sleep(10000).

% Check data availability
DataSize = ets:info(ohlc_data, size),
io:format("Available bars: ~p~n", [DataSize]).

% Start live simulation
ExoSelfPid = self(),
live_scape ! {ExoSelfPid, live_sim}.

% Get market data for decision making
live_scape ! {self(), sense, ohlc_data, close, [50, list_sensor], undefined, undefined}.

FromPid8 = whereis(live_scape),
receive
    {FromPid8, PriceList} ->
        io:format("Got ~p prices, latest: ~p~n", [length(PriceList), lists:last(PriceList)]),
        
        % Simple trading logic example
        [Latest | _] = lists:reverse(PriceList),
        [Previous | _] = lists:reverse(lists:sublist(PriceList, length(PriceList)-1)),
        
        if Latest > Previous ->
            io:format("Price rising, sending BUY signal~n"),
            live_scape ! {self(), trade, ohlc_data, 1};
        Latest < Previous ->
            io:format("Price falling, sending SELL signal~n"),
            live_scape ! {self(), trade, ohlc_data, -1};
        true ->
            io:format("Price stable, no trade~n")
        end
after 5000 ->
    io:format("No market data received~n")
end.

% Wait for trade result
FromPid9 = whereis(live_scape),
receive
    {FromPid9, Fitness, Halt} ->
        io:format("Trade executed - Fitness: ~p, Halt: ~p~n", [Fitness, Halt])
after 15000 ->
    io:format("Trade timeout~n")
end.
```

### 9. Integration with Evolved Neural Networks
```erlang
% Load the best evolved agent for live trading
rr("records.hrl").

% Find the best agent from evolution
Best_Agent_Id = case genotype_utils:find_best_agent(all) of
    {atomic, AgentId} -> AgentId;
    _ -> {5.693207755943648e-10, agent}  % Fallback example ID
end.

% Start the best agent in live trading mode
% This connects the evolved neural network to live_scape
Agent_PId = exoself:start(Best_Agent_Id, self(), live_trading).

% Monitor the agent
is_process_alive(Agent_PId).

% The agent will now automatically:
% 1. Request market data via live_scape sensors
% 2. Process data through its evolved neural network
% 3. Send trading signals via live_scape actuators
% 4. Receive real-time P&L feedback

% Check agent performance
timer:sleep(60000),  % Let it trade for 1 minute
Agent_PId ! {self(), get_backup}.

receive
    {Agent_PId, backup, AgentBackup} ->
        io:format("Agent backup received: ~p~n", [AgentBackup])
after 5000 ->
    io:format("No backup received~n")
end.
```

### 10. System Monitoring & Troubleshooting
```erlang
% Check all system components
io:format("=== System Status ===~n"),
io:format("live_scape: ~p~n", [whereis(live_scape)]),
io:format("python_port_handler: ~p~n", [whereis(python_port_handler)]),
io:format("OHLC data size: ~p~n", [ets:info(ohlc_data, size)]),
io:format("Mnesia running: ~p~n", [mnesia:system_info(is_running)]).

% Check for recent market data
case ets:last(ohlc_data) of
    '$end_of_table' ->
        io:format("No market data available~n");
    LastKey ->
        [LastBar] = ets:lookup(ohlc_data, LastKey),
        io:format("Latest bar: ~p~n", [LastBar])
end.

% Restart components if needed
% live_scape ! terminate.  % Stop current instance
% {ok, NewPid} = live_scape:start_link().  % Start fresh

% Force Python service restart (if needed)
% Kill existing: pkill -f "python3 priv/ib_service.py"
% Then restart: python3 priv/ib_service.py &
```

### 11. Environment Variables for Trading
```bash
# Set these before starting Python service for live trading
export ALLOW_LIVE_ORDERS=1        # Enable live order execution
export AUTO_BACKFILL=1             # Auto-load historical data
export IB_HOST=host.docker.internal # IB TWS host
export IB_PORT=7497                # Paper trading (7496 for live)
export IB_CLIENT_ID=101            # Unique client ID

# Start Python service with environment
python3 priv/ib_service.py
```
