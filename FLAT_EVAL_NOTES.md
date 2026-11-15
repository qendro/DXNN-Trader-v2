# flat_eval version 2: shared sensor answers, refusals, core scaling, and the population loop

Branch `sf/flat-eval`. Version 1 (`356c584`) added `flat_eval.erl`, a single-process evaluator that gives the same
positions and money as the process path. This note covers version 2:
- sensor answers shared across agents;
- every refusal case run through the process path;
- speed on 1 to 10 cores, and why the cores give what they give;
- what using `run_many` inside a generation would need (not built).

Nothing here is wired into the population loop. No existing module was changed. Everything was run on a price table cut at
the last row of 2017 (2,225,779 rows, checked at boot), on a scratch copy of
`nights/2026-11-13_night_alpha_t/Mnesia.nonode@nohost`, one VM at a time, `erl +Bd +scl false`, memory watched
(peak 2.0 GB).

## 1. Shared sensor answers

`flat_eval:run_many(Agent_Ids, Episodes, Opts)` now shares by default and returns what `run/3` returns for each agent,
in the order given. `share => false` gives version 1.

The work is cut into units of one episode and one group of agents. A unit does three things:

1. It walks the episode's keys once, the way the simulator does (from the start key by `fx:next/2` until `index_end`).
2. It computes every distinct price-list, indicator and clock spec its agents use, at every bar, into a column (a tuple
   with one answer per bar).
3. It runs its agents on the episode one after the other, reading those columns.

The position sensor depends on the agent's own account and is still answered per agent per bar. The answers are the
same terms `fx_series` gives, from the same function on the same row.

At every bar the column's key is checked against the simulator's own index. A bar the walk did not foresee is answered
directly and counted. No run below had one.

Episodes are independent (fresh network, account, accumulator and `prev_PC` each), so units run in any order or in
parallel. Each agent's statistics are then put back in episode order and cut after its first extinct episode, where
`run/3` stops.

With `parallel => N`, the agents are split into as many groups as give each worker about eight units. On one core every
agent of the call shares one set of columns per episode. On ten cores with 20 episodes, 24 agents share each set.

**Memory per episode.** All 384 saved agents use the same six specs: price lists of 32, 24, 30 and 20 values, the 12
indicators, and the 6-value clock, so 124 numbers a bar. One episode's columns take **4.1 MB** (4,088,136 bytes for
1,000 bars, about 33 bytes a number). A unit holds one episode's columns at a time, so the peak is 4.1 MB times the
number of workers: 41 MB on ten cores.

## 2. Same results

Every row compares every episode's statistics map field by field with `=:=`: the positions at every scored bar, money,
cost, entries, drift, z and alpha. Each row also compares the shared path with version 1, and counts the bars answered
outside the columns.

| set | agents | episodes | bars compared | mismatches | bars outside the columns | agents differing from v1 |
|---|---|---|---|---|---|---|
| saved `_run4` + `_run6`, 100 training episodes (seed 2026) | 384 | 38,400 | 37,209,600 | 0 | 0 | 0 |
| saved, all 369 episodes of 2017 | 384 | 141,696 | 137,303,424 | 0 | 0 | 0 |
| fresh, 21 episodes (12 training, 9 of 2017) | 2,091 | 43,911 | 42,549,759 | 0 | 0 | 0 |

How the fresh agents were made (seed 5151):
- **2,100 made in all.** 1,100 were mutated 0 to 20 rounds, and 1,000 were grown by repeated mutation to a target of 21
  to 40 neurons.
- **The groups.** Five groups: recurrent and feedforward, price lists scaled by volatility and by level, the four-value
  clock, and activation functions other than tanh and linear.
- **Size.** 992 agents have 21 to 42 neurons in the genotype; 984 still have 21 or more after pruning to the neurons that
  reach the trade, which is what flat_eval computes.
- **Refused.** 9 of the 2,100, all `output_neuron_ahead` (section 3).

Every speed run in section 4 (45 timed passes) also checked its results equal to the shared path's: all equal.

## 3. The refusals

`flat_eval_check:refusal_check/2` builds one agent per refused shape. Each starts as a freshly created agent: its sensors
and actuator are kept, and its neurons are replaced by a small network written consistently at both ends of every link.
For the link cases, one end is then changed.

Each agent runs through the process path (`eval_protocol:run_agents_on/4`, stall timeout 10 s, 6 training episodes).
When the process path completes, the same network also runs through flat_eval with the output-lead check skipped, and
the two are compared bar by bar.

| case | process path | flat_eval now | why |
|---|---|---|---|
| good (control) | completes | runs, 0 mismatches | |
| circle of non-recurrent links | stalls (no bar) | refused | waits for ever |
| sensorless self-loop that feeds the trade | runaway (123,624 messages) | refused | floods the output |
| **sensorless self-loop not reaching the trade** | completes | **runs**, 0 mismatches | spins alone, takes part in resets; not computed |
| neuron with no input, feeds the trade | runaway | refused | fires for ever |
| neuron with no input, isolated | stalls | refused | never reads its mailbox, so never answers the reset |
| output neuron one bar ahead | completes, **but trades shift** | refused | episodes 2 to 6 differ (42 bars of episode 2, 248 in all) |
| output neuron two bars ahead | completes, never trades | refused | the same reason; this agent never trades, so it shows nothing |
| **sensor fanout to a neuron that does not read it** | completes | **runs**, 0 mismatches | stray message passed by, emptied at reset |
| neuron input missing from the sensor's fanout | stalls | refused | waits for ever |
| **neuron output to a neuron that does not read it** | completes | **runs**, 0 mismatches | as the stray fanout |
| neuron input missing from the sender's outputs | stalls | refused | waits for ever |
| **ro_ids entry that is not a link** | completes | **runs**, 0 mismatches | one stray zero per reset |
| bias not last | neuron crashes, then stalls | refused | |
| weight list one short / one long | neuron crashes (`dot` function_clause), then stalls | refused | |
| duplicate input | stalls | refused | |
| duplicate output | completes, network is not the one written | refused | the target gets two messages a bar, so the link slips a bar further every bar |
| all sensors unconnected | **exoself crashes** | refused | see below |

**Output neuron ahead.** One firing ahead leaves a trade message in the actuator's mailbox at the end of an episode. The
full reset empties neuron mailboxes but not the actuator's, so every later episode trades one bar late. The first
episode matches flat_eval exactly; the rest do not. The process path's result depends on that leftover message, which a
network-level evaluator should not imitate.

**All sensors unconnected.** `exoself:link_Sensors/4` calls `genome_mutator:add_sensorlink/1` outside a transaction.
When that fails, it logs with `io_lib:format/1`, a function that does not exist, so the exoself dies with `undef`
(exoself.erl line 765). When the link does succeed, the exoself has changed the genotype at random before running it.
Either way flat_eval cannot reproduce it, so the case stays refused. **This is a bug in the process path. It was not
fixed here.**

**Stray links.** A stray link adds one message per bar to the target's mailbox until the next reset. flat_eval refuses
an episode long enough for that to reach half the runaway limit (`config:runaway_queue_limit/0`, 5,000 by default):
twice the strays per bar times the longest episode at or above the limit. Near the limit the process path gives up at a
moment that depends on its watchdog.

## 4. Speed and core scaling

**Load first.** Load average 1.97 just before the VM started. The only other busy process on the machine was a Linux VM
at about 0.4 core. (The other session's Erlang night replay seen earlier, which used 4.4 cores, had ended.) The load
average printed before each run (2 to 7) includes this VM's own runs that had just finished.

**Setup.** 96 saved agents (the first 48 of each population) on 20 training episodes (seed 404): 1,920,000 bars a pass.
Three repeats, interleaved: repeat 1 of every path and core count, then repeat 2, then repeat 3.

| path | cores | min | mean | max (bars/s) | gain over 1 core | busy schedulers |
|---|---|---|---|---|---|---|
| flat v1 | 1 | 102,427 | 103,608 | 105,198 | 1.00 | 1.00 |
| flat v1 | 2 | 207,130 | 208,008 | 209,090 | 2.01 | 2.00 |
| flat v1 | 4 | 367,368 | 383,199 | 394,592 | 3.70 | 3.98 |
| flat v1 | 8 | 486,301 | 494,610 | 503,946 | 4.77 | 7.87 |
| flat v1 | 10 | 534,736 | 536,236 | 537,417 | 5.18 | 9.64 |
| flat shared | 1 | 223,132 | 225,248 | 229,150 | 1.00 | 0.99 |
| flat shared | 2 | 403,644 | 407,593 | 411,384 | 1.81 | 1.89 |
| flat shared | 4 | 593,340 | 628,539 | 649,644 | 2.79 | 3.51 |
| flat shared | 8 | 753,416 | 774,933 | 799,322 | 3.44 | 6.88 |
| flat shared | 10 | 804,338 | 824,344 | 834,916 | 3.66 | 8.47 |
| process | 1 | 23,523 | 23,632 | 23,838 | 1.00 | 1.00 |
| process | 2 | 51,623 | 52,003 | 52,380 | 2.20 | 1.99 |
| process | 4 | 63,762 | 64,135 | 64,638 | 2.71 | 3.99 |
| process | 8 | 58,704 | 58,874 | 59,197 | 2.49 | 7.98 |
| process | 10 | 63,154 | 63,316 | 63,460 | 2.68 | 9.94 |

Shared against process: **9.5×** on one core, 7.8× on two, 9.8× on four, 13.2× on eight, **13.0×** on ten. Shared
against version 1: 2.2× on one core, 1.5× on ten.

"Busy schedulers" is the sum over online schedulers of active time divided by wall time
(`erlang:statistics(scheduler_wall_time)`), so how many cores' worth of work the VM did.

**What ten cores are worth here.** `flat_eval_check:ceiling/3` runs N processes on N schedulers that share nothing (mean
of 3):

| kernel | 1 | 2 | 4 | 8 | 10 cores |
|---|---|---|---|---|---|
| floating point, almost no allocation | 1.00 | 2.07 | 3.95 | 6.39 | 7.74 |
| one saved agent's network on fixed inputs (the flat path's inner work, with its allocation) | 1.00 | 1.89 | 3.44 | 5.10 | 6.17 |

**Answer on core scaling.** Last time both paths gained only about 2×. The causes, with the evidence:

1. **Other jobs, last time.** The load average was 7 to 9 last time and about 2 now. Version 1 on the same 96 agents and
   20 episodes:
   - one core: 81,000 then, 104,000 now;
   - ten cores: 245,000 then (spread 188,000 to 309,000), 536,000 now (spread under 1%).

   Most of last time's "2×" was other work on the machine.
2. **The hardware.** This Mac has 4 performance cores and 6 efficiency cores (`hw.perflevel0/1.physicalcpu` = 4 / 6).
   Work that shares nothing gets 3.95× from 4 cores but only 7.7× from 10. The slow cores add about 0.6 of a fast one
   each, and a one-scheduler run sits on a fast core.
   - With allocation (the network kernel) the ceiling drops to 6.2×: memory traffic shares the cache and bandwidth of
     each cluster.
   - So "10 cores" are worth 6 to 7.7 single-core equivalents here, not 10, and anything beyond 4 cores runs on the slow
     ones.
3. **Version 1 is at that ceiling.** 5.2× with 9.64 schedulers busy. It allocates 6.4 G words per pass, against 2.5 G for
   the shared path.
4. **The shared path loses another 15% to idle, and a little to less sharing.**
   - **Idle.** 8.47 schedulers busy on 10, 3.5 on 4. Its units are large (24 agents × 1,000 bars), and the last units
     finish on slow cores while the others wait.
   - **Less sharing.** On ten cores the agents are split into 4 groups, so each episode's columns are built 4 times, not
     once. That costs little: the columns are 3% of the time (section 5).
   - **Net.** 3.7× from ten cores, on top of a 2.2× better single core.
5. **The process path is not limited by work.** All its schedulers are busy at every core count (9.94 on 10), yet its
   throughput stops at 4 cores (64,000) and falls at 8.
   - **Where the time goes.** An agent is a chain of about a dozen processes passing one bar at a time. With more
     schedulers, each hand-off is more often a wake-up on another core.
   - **Garbage collection.** 4.6 to 5.2 million collections per pass (the flat paths: 1,100 to 405,000).
   - **Batching.** `run_agents_on/4` starts agents in batches of N and waits for the slowest of each batch.
   - **Busy without progress.** Busy time that does not turn into bars is that hand-off and scheduling overhead.
6. **Nothing shared inside flat_eval limits it.** Results go back as one message per unit. The shared state is
   read-only:
   - the settings table and the price table (ETS with `read_concurrency`);
   - the packed series (`persistent_term`).

   Version 1 scales as far as the allocation-heavy ceiling, which it could not if any of these were contended.

## 5. Profile of the shared path (one core, it is under 10× the process path)

96 agents on 4 episodes, microseconds per agent-bar:

| part | µs per agent-bar | share |
|---|---|---|
| whole shared run | 4.49 | |
| columns (all specs, once per episode, plus compiling) | 0.13 | 3% |
| network alone (`think` on stored inputs) | 4.61 | about 75–80% of the whole (alone it pays its own setup and loses cache locality, hence above the whole) |
| simulator alone (`fx:ep_step/4` with a scripted signal) | 0.85 | about 19% |

eprof over the whole run (it inflates small calls) agrees:

| part | share |
|---|---|
| `flat_eval:dot/3` | 57.5% |
| `aggregate/5` | 10.6% |
| `think/5` | 5.2% |
| `activate`, `sat` and `setelement` together | 4.3% |
| reading the columns | 5.7% |
| `ets:lookup/2` and `ets:next/2` (the simulator's closes and steps) | 4.3% |
| `fx:ep_step/4` | 2.5% |

Each agent does about 259 multiply-adds a bar (99.6 million `dot` steps over 384,000 agent-bars).

**Where the time goes now.** Into the weighted sums: boxed floating-point multiply-adds, about 17 ns each in BEAM. Sensor
answers, which were 55–65% in version 1, are now 3%.

**What is left in pure Erlang.** Not much without changing the order of the sums. The network is already a tight loop
over lists. Storing weights and inputs as float binaries would still box every intermediate.

**A native network step.** It would do the same sums in about 1 ns each. Estimated, not measured, and not built
(native code was ruled out):
- the network would drop to about 0.3 µs per agent-bar;
- the simulator (0.85) would become the main cost;
- per core, roughly 3 to 4× the shared path, about 35× the process path.

It would need the same summation order and the same libm `tanh`, and floating-point contraction off.

## 6. Using run_many inside a generation (not built)

**Call sites.**
- `population_monitor:summon_agents/3` (generational) and the steady-state branch (lines 280 and 285) start one exoself
  per agent: `exoself:start(Agent_Id, self())`.
- `population_monitor:run_GenTest/2` starts one per benchmark (line 909). It is off in the night arm
  (`generation_test false`).
- `eval_protocol:run_agents_on/4` and `resim` start them for replays. They could call `run_many/3` with
  `mode => validation` today.

**What the population loop expects back, per agent, in episode mode** (`exoself:loop/2` gt, `save_scored_agent/1`,
`finish_episode_agent/3`):

1. **The score.** One `score` evaluation on `episodes:current_set(score)`. Its fitness is
   `fitness_functions:aggregate_episodes(EpStats)`, what the simulator computes in `fx:episode_trade/6`. It is written to
   the genotype at once, with `genotype:write(A#agent{fitness=ScoreFitness})`.
   - `exoself:backup_genotype/2` also writes the weights back. Without tuning or plasticity they are unchanged, so a flat
     path can skip it.
2. **Validation.** When `exoself:validation_due/0` is true, a `validation` evaluation on the validation set, with
   `validation_series` off. It is logged only and never reaches selection.
3. **A ledger row.** One row in the `eval_ledger` ETS table:
   `#{tune, score, score_summary, score_cycles, score_time_us, validation, validation_summary, neurons, specie, evaluations}`.
   `end_episode_generation/2` reads it for the generation summary.
   - `score_cycles` is the cortex's bar count.
   - `score_summary` is `fitness_functions:episode_summary(EpStats)`.
4. **Casts to the monitor.** `{self(), evaluations, Specie_Id, 1, Cycles, Time}` once per evaluation; it counts
   `tot_evaluations`, which can end a run (`evaluations_limit`). Then `{Agent_Id, terminated, ScoreFitness}`, which counts
   `agents_left` down and, at zero, starts selection and the next generation.
5. **Logs.** The qlog lines (`SCORE_EVAL`, `FITNESS_EVAL`, `pid_map`), if the logs are to stay comparable.

**Smallest change that would do it.**
- In `summon_agents`, split the generation's agents into those `flat_eval:compile/1` accepts (and `stray_check` passes
  on the generation's episodes) and the rest.
- Start exoselves for the rest exactly as now.
- Start one helper process that:
  1. calls `run_many(FlatIds, score, #{mode => score, parallel => N})`, and then, when validation is due,
     `run_many(FlatIds, validation, #{mode => validation, parallel => N})`;
  2. does items 1 to 5 above for each agent;
  3. casts `terminated` per agent.
- `handle_cast` and the generation end stay as they are.
- `activeAgent_IdPs` holds `{Agent_Id, Pid}` for `stop`/`terminate` handling, so the helper's pid would stand for its
  agents.
- Scheduler use changes. One helper with N workers replaces 144 agents × a dozen processes. The exoself's own
  `+scl false` concern goes away for the flat agents.
- Random numbers are not an issue. Each exoself seeds its own process from the evolution seed and the agent's
  fingerprint, and a flat agent draws none, so the other agents' draws do not move.

**What must stay on the process path.**
- `episode_tuning true`: weight perturbation, backup and restore inside the exoself, with its own random draws. It is
  off in every current arm.
- Plasticity, substrate encoding, aggregators other than `dot_product`, modulation inputs, other sensors and actuators.
- `evaluation_mode` legacy, `episode_isolation` off, `sense_before_trade` off.
- The refused shapes of section 3: an output neuron ahead, stalls, runaways, crashes, all sensors unconnected.
- Episodes long enough for stray links to approach the runaway limit.
- Live trading.
- Agents the process path would give the failure fitness (stalls, runaways) keep getting it there. flat_eval never
  receives them.
