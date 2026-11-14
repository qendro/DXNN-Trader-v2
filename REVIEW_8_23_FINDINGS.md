# Independent review of REVIEW.md 8.23

Scope: the changes described in section 8.23 (episode isolation, ordered sensing, seeds, the four arms, fill rules, the
`vol` encoding, the position sensor and the starting hidden layer). The goal was to find defects, not to review style.
No existing file was edited and nothing was committed. All Erlang ran in a scratch copy (see "What was run" at the end).

Line numbers refer to the working tree as of 2026-11-13.

## Summary

I found **no defect that breaks the 8.23 changes** when they are switched on as the run configs switch them on
(`episode_isolation`, `sense_before_trade`, `rollover_rule`, the seeds). I found no deadlock and no gap in the watchdog
inside the new reset. Sensors cannot read the wrong bar once ordered sensing is on. The `vol` lists do not look ahead.
The built-in checks reproduce the figures in REVIEW.md exactly.

The findings below are of low severity. Two are about expectations rather than code (F1, F2). The others are older
weaknesses that 8.23 leaves in place or only narrows (F3 to F5), plus notes on intended behaviour (F6, F7).

| # | Severity | Short |
|---|---|---|
| F1 | Low (documentation) | "No env var = exactly as before" holds only against the *new* `reference_run_configs/0`, which now switches isolation and ordered sensing on |
| F2 | Low (coordination) | The GCP fleet example still uses the old arm tags (`t1b`, `t2c`, ...); with them, every arm VM stops at start |
| F3 | Low (old, tuning only) | `weight_backup` and `weight_restore` can be flushed away by the reset that follows them |
| F4 | Low (old, default path) | With isolation off, the reset and `get_backup` have no stall guard |
| F5 | Info (old) | A sensor that reads the simulator twice on one bar duplicates that bar in raw list windows |
| F6 | Info | There is no forced close at an episode's end, so the rollover rule has nothing to block there |
| F7 | Info | `validate_last_generation` only fires when a run ends at `generation_limit` |

---

## Answers to the questions asked

### 1. Can any path deadlock or wait forever?

No path deadlocks with the new settings on. Case by case:

- **A boundary reached while a sensor request is in flight.** `fx:boundary_wait/6` (fx.erl:716) answers price
  senses and internals senses from the bar just finished (`S`, the state before `ep_step`). It does not count them,
  and `episode_go` sets the count back to 0 (fx.erl:720). Step 1 of `exoself:full_reset/2` (exoself.erl:571) sends
  `reset_prep` to every sensor. A sensor blocked waiting for the simulator's answer gets that answer from
  `boundary_wait`, forwards its vector, then reads `reset_prep` and answers (sensor.erl:27). With
  `sense_before_trade` on, no sense can be in flight at a boundary at all, because the trade that ends the episode is
  only taken once all N senses of that bar have been served (fx.erl:255, `sim_senses/3`).
  *Tested:* 20 concurrent copies of the `isolation_check` agent (its H4 sensor feeds the trade only through a
  recurrent link) on 8 train episodes, isolation on, ordered sensing **off**, twice. All 20 copies finished all 8
  episodes both times, with 140 boundaries per round. The copies diverged (1 and 3 distinct position series), so the
  race was present and the boundary survived it. With ordered sensing on, all 20 were identical.
- **A neuron that never answers a reset.** `await_ready/4` (exoself.erl:596) wakes up after every
  `?WATCH_INTERVAL_MS` (100 ms) without a `ready`. It fails the agent as a runaway if any phenotype process has more
  than `runaway_queue_limit` messages waiting. It fails it as `stalled_in_reset` once the deadline (`stall_timeout_ms`
  from the start of the reset) has passed. Either failure goes to `give_up/3`, which kills every process
  (`shutdown_phenotype/1`). A neuron that fires forever without reading its mailbox never answers `reset_prep`, so it
  is caught at the first boundary: within 100 ms if it floods anything, otherwise after `stall_timeout_ms`.
- **Neurons in `reset_hold/2`** (neuron.erl:170) have no timeout, but the exoself always ends the hold, with
  `reset_flush` and `reset_full`, with `reset` on the legacy path, or with a kill.
- **`reset_full` is not acknowledged** before `episode_go` (exoself.erl:620). This is safe. A neuron still waiting for
  `reset_full` uses selective receive, so a sensor vector that arrives first stays in its mailbox. Every recurrent zero
  is sent after every mailbox was emptied in step 3, and messages between one pair of processes arrive in order, so
  the zero comes before that neuron's real output.
- **An agent with zero sensors linked to a simulator.** The count `{_,sensors,N}` is only sent to simulators that at
  least one sensor uses (exoself.erl:104, `lists:usort(SensorScapes)`). The idle extra simulators get no count and stay
  in `sim_main/3`, which never holds a trade. An agent with no sensors at all cannot arise: `remove_sensor` keeps at
  least two sensors and a price sensor (genome_mutator.erl:1094). If one did arise, the older repair path in
  `link_Sensors/4` would crash the exoself (a sensor added to the genotype but never spawned). That predates 8.23 and
  cannot be reached now.
- **The last episode of an evaluation.** It never raises a boundary: the clause at fx.erl:702 ends the evaluation,
  replies with the halt flag and leaves the simulator idle. The reset between evaluations then runs in the exoself.
- **An agent that goes extinct mid-episode.** `{extinct,_}` matches the same clause (fx.erl:702) whatever is left in
  the queue. The evaluation ends at once and its fitness is aggregated over the episodes played. No boundary is
  raised, so nothing waits. Ending the remaining episodes is older behaviour, not 8.23.
- **The validation pass.** The score and weights are stored first (`save_scored_agent/1`, exoself.erl:655). Then
  `reset_network/1` runs; a failure there goes to `give_up` with the phase set to `validation`, so the score is kept.
  Then `start_pass/2` sends `eval_mode`, which with isolation on puts the simulator back to idle (`on_eval_mode/4`,
  fx.erl:300). That also covers a late sensor that restarted the simulator on the score set.
- **A crashed process** (neuron, sensor or simulator; none is linked to the exoself). Inside a reset it is caught by
  the deadline. While bars run it is caught by the stall clock.

### 2. Does the stall and runaway watchdog still end a broken network in every state?

With isolation on: yes. While bars run, the main loop watches as before. Inside every reset (each boundary, and the
reset before validation) `await_ready/4` has its own runaway check and deadline. After a boundary the stall clock
restarts (exoself.erl:620 onward), so time spent in the reset never counts as silence.

There are two places with no guard, both older than 8.23 (F4): `get_backup/2` (exoself.erl:895) and, with isolation
off, `legacy_reset/1` (`gather_acks` gives up silently after 100 s, exoself.erl:961; the substrate wait at
exoself.erl:554 has no timeout). With isolation on, `get_backup` is safe in practice: a score evaluation of 32 episodes
has passed 31 full resets, and each of those required every neuron to answer.

### 3. Can a sensor still read a bar other than the one being decided?

With `sense_before_trade` on: no. Within a bar, a trade waits in the mailbox until N senses have been served since the
last trade. The cortex only syncs the sensors for the next bar after the actuator has its answer, so no sense of the
next bar can arrive early. The count is reset at every trade (fx.erl:388), at `eval_mode` (fx.erl:302), at
`{_,sensors,N}` (fx.erl:314) and at `episode_go` (fx.erl:720). N counts every sensor the cortex syncs, and every
sensor of the forex morphology asks the simulator (price lists, indicators, clock and position all go through the
simulator). The position sensor reads the account after the previous fill. The open-profit sensor reads it marked at
the current bar's close (`update_account(U_S2,...)` in `ep_step`). Neither looks ahead.

Remaining ways, none of which applies to the arms:
- `sense_before_trade` off. This is the default, and it is the documented race.
- A sensor that does not ask the simulator (`rng`, or every sensor in live trading). Episode mode is off in live
  trading, and the forex morphology does not offer `rng`.
- A broken network whose output neuron fires from recurrent inputs alone, before any sensing. Its trade is applied to
  the bar sensed afterwards. That is stale information, not look-ahead, and such a network is a runaway anyway.

*Tested:* `isolation_check/0` passes. The 20-copy probe above gave identical position series in every copy once
ordered sensing was on.

### 4. Does a cloud checkpoint and restore keep working, with the same arm, seeds and random sequence?

By reading the code: yes. `dxnn_runtime` is not in this repository, so the worker side was not checked (see
"Not checked").

- The hooks are where they were. `handle_call(dxnn_checkpoint_pause,...)` is at population_monitor.erl:129,
  `handle_cast(dxnn_checkpoint_cancel,...)` at :303, `dxnn_runtime:finish()` at exp_runner.erl:1302 and
  `dxnn_runtime:resume_or_new()` at launcher.erl:73. The pause reply is sent from the generation-end branch after
  selection and after `begin_episode_generation(U_PopGen)`. That is a clean boundary.
- **Arm and seeds:** `get_run_configs/0` builds `{test_arm,Arm}`, `{episode_seed,{S,I}}` and
  `{evolution_seed,{S,I}}` into the run configs, which are stored in the experiment record. Both `resume/2` and
  `continue_experiment/1` reload them with `apply_run_configs/3`. The environment is not read again on resume. The GCP
  worker also stores the `--env` pairs and refuses to restore a checkpoint whose pairs differ
  (GCP_TOOLING_RESPONSE.md, item 2).
- **Random sequence:** every stage is seeded from values a checkpoint keeps, not from what ran before in the process:
  - selection and mutation: `config:seed_rand(CurrentGen+1)`, population_monitor.erl:169, run right before
    `mutate_population`;
  - episode draws: `episodes:rand_state/1`, `{Seed,Generation,7919+Run}`, a state of its own;
  - each exoself: seed, fingerprint, the agent's generation (exoself.erl:75);
  - the first population: `seed_rand(0)`, exp_runner.erl:824 and :829.

  On restore, `init/1` restores the generation (`restore_checkpoint_progress`), redraws that generation's episodes
  (population_monitor.erl:90; the draw is identical) and restores the selection memory, which the arms use at 0.85,
  from `runtime_state.term` (:93). The next selection is then seeded exactly as it would have been without the stop.
  The cost schedule is recomputed from the generation.
- What is not reproducible either way: `run_time_limit_s` (reference runs 3 and 4 only), weight tuning (off) and the
  time-based ids. As REVIEW.md says, the ids are only used to break ties, in creation order.

### 5. With every new setting at its default, is behaviour exactly as before?

At the code level, yes for agent behaviour, with these differences that do not change any result:

- fx.erl: `on_eval_mode`, `on_internals` and the trade clause now update `fx_senses`, which nothing reads while
  `fx_sensors` is unset. `ep_acc_new` reads `rollover_rule` and `late_fill` once per episode, and `ep_step` does two
  extra `maps:get` per bar. `boundary_wait` is never reached.
- exoself.erl: `reset_network/1` uses `legacy_reset/1`, the same message sequence as before. It always returns `ok`,
  so the new `{failed,_}` branches are never taken. The exoself is not seeded, and no sensor count is sent. A new
  `episode_boundary` clause in the loop is never matched.
- neuron.erl: `reset_hold/2` behaves as the old inline `reset` wait did, and also accepts `terminate`.
- sensor.erl: the `reset_prep` clause is never used.
- morphology.erl `first_sensors/1` with `init_sensors one`: the pool now leaves out `fx_Clock` and `fx_Position` as
  well as `fx_Internals`. With the clock and position sensors off (their defaults) the pool and the random draw are
  the same as before.
- genotype.erl: with `init_hidden_neurons 0`, `construct_InitialNeuroLayer/7` runs as before.
- selection_algorithm.erl:65: with no seed, the mutants are still made in parallel.
- Listed in REVIEW.md already: the generation summary sorts its rows before summing; the validation pass restarts the
  stall clock after its reset; the `run_start` log line has more fields.

At the experiment level: **not as before**. See F1.

### 6. Does the rollover rule also block the forced close at an episode's end?

There is no forced close (F6). `update_state/1` ends the episode by returning `sim_over`. The episode's money is
`balance + unrealized_PL`, the open position marked at the last close, and the score charges cost only on entries.
So the rule has nothing to block there. The only effect is that a signal given on the episode's very last bar is
ignored when that bar is in hours 21 to 23. That bar is never scored (it has no next close), and the money uses the
same close either way, so this changes nothing measurable. I consider it consistent with what is intended.

### 7. Does the vol encoding look ahead at the start of the table or across gaps?

No.
- `answer_at(...,{plt,TF,N,vol})` (fx_series.erl:162) uses `plt_closes/4`, which takes completed TF bars at indices
  below the current bar's index plus the current close; `mean_move/3` (fx_series.erl:232), which takes the running
  sum up to row P; and the close at P.
- At the start of the table, `mean_move` is 0 for P = 0 and covers fewer than 1440 moves before row 1440. The floor
  (`1e-5 × close`) and the padding (the oldest close available) only use rows up to P.
- Across gaps, a weekend or a missing stretch counts as one 1-bar move in `mean_move`. This makes the divisor a little
  larger on the first bars after the gap, but uses no later data. A TF bar counts as completed only once a row of a
  later bucket exists at or before P.

*Tested:* `fx_series:test/2` gave `{ok,630}` on 2015-03-01 to 2015-04-30 (63,413 rows) and `{ok,630}` on the first
42,592 rows of the file (2012-01-02 to 2012-02-10). Each run checked 60 random rows plus rows 0, 1 and the last row,
and all nine specs including the four `vol` lists.

---

## Findings

### F1. Low (documentation): with no env var set, the experiment is not the pre-8.23 one

- **What happens.** REVIEW.md 8.23 §2 says that with no variable set `get_run_configs/0` "returns exactly what it
  returned before (`reference_run_configs/0`; checked with `=:=`)". That compares against today's
  `reference_run_configs/0`. That function now builds on `reference_base/2`, which contains `{episode_isolation,true}`,
  `{sense_before_trade,true}` (exp_runner.erl:747) and `{extra_baselines,true}`. So a run started with no variables
  (for example the GCP "reference arm A", which sets no `DXNN_TEST_ARM`) behaves differently from any reference run
  made before 8.23. By REVIEW.md's own table, about half of agent-episodes change.
- **Reasoning.** The `=:=` check proves that `with_seeds/3` and the arm switch change nothing when unset. It does not
  prove that the reference experiment is unchanged. The short version of 8.23 does say that the run configs switch
  the settings on, so this is disclosed, but §2 and the GCP request read as if the reference were unchanged.
- **Suggested fix.** Say in §2 and in GCP_TOOLING_REQUEST.md that the reference experiment now includes isolation and
  ordered sensing. If old results must be reproduced, add a `legacy_reference` arm that sets both to `false`.

### F2. Low (coordination): the fleet example's arm tags stop every VM at start

- **What happens.** GCP_TOOLING_RESPONSE.md item 4 says `config/gcp-fleet.example.yml` uses the tags `t1b`, `t2c`,
  `t2d`, `t3e`, `t3f`, `t3g`. `parse_arm/1` (exp_runner.erl:591) raises `{unknown_test_arm,...}` (:602) for any of
  them. Under `erl -noshell -eval` on OTP 28 a failing eval writes a crash dump and halts the VM. I checked this.
  The failure is loud, which is intended, but a fleet launched from the unchanged example ends with every arm VM down.
  Whether the worker retries is not known.
- **Suggested fix.** Before launch, update the fleet file to `ff_level`, `rec_level`, `ff_vol`, `rec_vol` (and the
  fold names). GCP_TOOLING_REQUEST.md already lists them. Run `exp_runner:get_run_configs()` once per VM's env as a
  dry run.

### F3. Low (older than 8.23, tuning only): weight messages lost to the reset that follows them

- **What happens.** On the tuning path, exoself.erl:230 to 234 send `weight_backup` or `weight_restore` and then call
  `reset_network/1`. A neuron runs a priority receive for `{ExoSelf,reset_prep}` that scans the whole mailbox
  (neuron.erl:102), then `flush_buffer/0` (:103). If the neuron is busy (for example a neuron off the trade path
  finishing its last bar) when both messages arrive, it takes `reset_prep` first and flushes `weight_backup` or
  `weight_restore` unread. The best weights are then not kept, or a failed perturbation is not undone. With isolation
  on, step 1 (the sensors) runs first, which narrows the window but does not close it.
- **Reasoning only.** Not reproduced. `episode_tuning` is false in every config, so no current run is affected.
- **Suggested fix.** Send the weight messages after the reset (between `reset_full` and `reactivate`), or have the
  neurons acknowledge them before `reset_prep` is sent.

### F4. Low (older than 8.23, default path): no stall guard in the legacy reset or in `get_backup`

- **What happens.** `gather_acks/1` (exoself.erl:957) gives up after 100 s and carries on as if the reset had
  succeeded. The substrate `ready` wait (:554) and `get_backup/2` (:895) block with no timeout. A neuron that never
  reads its mailbox and has no outputs makes `save_scored_agent/1` wait forever, and its generation never ends. With
  isolation on, that neuron would already have failed a boundary reset, so the arms are protected whenever an
  evaluation has at least two episodes.
- **Reasoning only.** I did not construct such a neuron. `require_runnable` in the removal operators probably
  prevents it.
- **Suggested fix.** Give `get_backup/2` the same deadline and runaway check as `await_ready/4`, and use `full_reset`
  logic (or at least a failing timeout) in `legacy_reset/1`.

### F5. Info (older than 8.23): a second sense of the same bar duplicates it in raw list windows

- `fx:list_encoded/2` adds the current bar to the cached window on every call. This happens when two `fx_PLI` sensors
  share a resolution, or with isolation on, ordered sensing off and tuning on, when a late sense restarts the idle
  simulator before the next evaluation (no `eval_mode` is sent between tuning evaluations, exoself.erl:288 onward).
  Either way the window holds the bar twice. None of this applies to the arms: no raw lists, ordered sensing on,
  tuning off. A fix, if needed: key the cache by bar index, or send `eval_mode` before each tuning evaluation too.

### F6. Info: no forced close at an episode's end

See answer 6. A position opened on the last bar is not charged in the timing score (Entries are counted only in the
`continue` branch of `ep_step`), though `pnl` includes its spread. This is older behaviour, and small.

One edge detail of the rollover rule: it tests the hour of the bar's own key (fx.erl:632). With keys stamped at the
start of each minute, the order decided at bar 20:59 is filled at about 21:00 and is allowed. The order at bar 23:59
(filled at about 00:00) is blocked. This is a one-minute edge and harmless. The 21:00 to 23:59 window contains the
New York 17:00 rollover whether the file's clock is UTC or London time.

### F7. Info: `validate_last_generation` assumes the run ends at `generation_limit`

`validation_due/0` (exoself.erl:629) uses `Generation == generation_limit - 1`. A run ended by `run_time_limit_s` or
`evaluations_limit` gets no last-generation validation. The arms use fixed generation counts, so they are fine. Keep
this in mind if the setting is ever combined with a wall-time limit.

---

## Checked and found sound

- Isolation: the three-round flush (sensors, neurons, flush again) and the fresh state (`si_pidps_bl`,
  `erase(diff_product)`, recurrent zeros). The only process-dictionary memory an aggregator or plasticity rule keeps
  is `diff_product`. `functions:s/1` uses `put(rec,…)`, but it is an XOR demo, not an activation function.
- `on_eval_mode/4` resetting the simulator with isolation on. `start_pass/2` sends `eval_mode` before `reactivate`.
  On one node this ordering holds in practice; Erlang only guarantees order per sender pair, and the code relied on
  it before 8.23 too.
- `prep/3` sends the sensor count to the simulator the sensors are actually linked to (the last one spawned for
  `fx_sim`), not to the idle ones.
- The seeds:
  - `config:seed_rand/1` seeds per stage.
  - The seeded path in `parallel_create_mutants/2` is sequential, and `rank/4`, which the arms use, goes through it.
  - Episode draws use their own state, so they never touch the process generator.
  - The exoself seed uses a fingerprint with ids removed.
  - `env_integer/1` and `parse_arm/1` fail loudly on bad values, and a failing `-eval` halts the VM.
- The starting hidden layer (`construct_InitialHiddenLayer/7`, genotype.erl:126):
  - its self-links come out as recurrent outputs (`calculate_ROIds`);
  - the pattern `[{0,H},{0.5,O}]` is right;
  - the feedforward arm cannot gain recurrent links, because `add_inlink`, `add_outlink` and `add_neuron` filter by
    layer and `outsplice`, `add_sensorlink` and `add_actuatorlink` cannot create one.
- The position sensor (morphology.erl:169, fx.erl:363) and the rule in `remove_sensor` that keeps a price sensor.
- The fill rules in `ep_step/4` (fx.erl:580 onward): the rollover rule keeps the position held; the late fill uses the
  previous bar's signal; the warm-up is flat. Agents and scripted rules share this code.

## Not checked

- `dxnn_runtime` (the cloud worker's module) is not in this repository. I could not check what `resume_or_new/0`,
  `checkpoint/1` and `finish/0` do, beyond the calls into this code.
- No checkpoint and restore, and no chained population run, was executed. Answer 4 comes from reading the code. The
  claim that two runs with the same seeds match was not re-run.
- `order_effect/3`, `census/1`, `resim` and the timing figures were not re-run.
- Substrate encoding with isolation on: `full_reset` does not reset the CPP and CEP processes, and resets the
  substrate only after the neurons. The arms use the neural encoding only.
- Weight tuning with isolation on (`episode_tuning` is off everywhere), and live trading.
- No full-size run, and no run on 2018 or later.

## What was run

All of it in a scratch copy, compiled with `erlc` from the working tree, with
`fx_tables/EURUSD1.txt` cut at the end of 2017 (2,225,779 rows, 2012-01-02 07:00 to 2017-12-29 21:59; the driver
asserted the last key). `EURUSD_M1_MIDPOINT_UTC.txt` was not read. Run settings: run 3 of
`exp_runner:get_run_configs()` with no env vars set. One VM at a time with `erl +Bd +scl false`; before each run I
checked for other `beam.smp` processes, and once waited about 25 s for another session's VM to finish. A wrapper
killed any VM above 3.5 GB or past 9.5 min; none came close.

| check | result | peak memory | time |
|---|---|---|---|
| `eval_protocol:isolation_check/0` | PASS. Isolation off: B −58.5 pips alone, +60.25 after A (differs at 543 of 969 bars), −103.5 after C (120 of 969). Isolation on: identical in all three orders | 593 MB (one VM for all three checks) | 18 s |
| `eval_protocol:fill_rule_check/0` | PASS: 686 position changes in hours 21 to 23 without the rule, 0 with it; the late fill shifts every position by one bar | same VM | 0.1 s |
| `eval_protocol:g0/0` | 12 of 12 PASS; random walk {reversal,60} z +0.0022 and {reversal,240} −0.0810 (standard error 0.050) | same VM | 2 s |
| `fx_series:test('EURUSD1',60)`, March to April 2015 | `{ok,630}` | 108 MB | 9 s |
| `fx_series:test('EURUSD1',60)`, start of the file to 2012-02-10 | `{ok,630}` | 72 MB | 7 s |
| 20 concurrent copies of the isolation test agent, 8 train episodes, four conditions | all 20 finished in every condition. Isolation on and ordered sensing off: 1 and then 3 distinct position series. Ordered sensing on, with or without isolation: 1 | 793 MB | 34 s |
| `erl -noshell -eval` with an expression that raises | prints "Failed to eval", writes a crash dump, VM exits | — | — |

The first two checks give the same figures as REVIEW.md 8.23. For the probe, the scratch copy of `eval_protocol` was
compiled with `+export_all` to reach `build_test_agent/0`; the source was not changed.
