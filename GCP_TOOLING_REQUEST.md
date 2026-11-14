# Changes requested in AWS-Deployment for the GCP test fleet

Written 2026-11-13 by the agent working in `DXNN-Trader-v2-training-core`, for the agent working in `AWS-Deployment`.
I read `AWS-Deployment` but did not change it. File and line references are to the working tree as of that date.

## Background you need

**What is being run.** `DXNN-Trader-v2-training-core` evolves neural networks that trade EUR/USD on one-minute bars in
simulation. A run is a population of agents (default 144, in three independent "families" of 48) scored each generation
on 32 episodes of 1,000 bars. A full experiment is four or five "runs" chained by `exp_runner`, and takes hours.

**What we want to do on GCP.** Three comparison tests, each with several "arms" (variants of the settings), each arm
on two or three VMs. About 15 VMs at once, all running the same source, differing only in which arm they run:

| Test | Arms | VMs |
|---|---|---|
| Reference | A: current setup | 3 |
| 1. New setup against old | B: previous neurons, sensors, mutation mix | 2 |
| 2. How orders are filled | C: no orders at the daily rollover. D: that, and filled one bar late | 4 |
| 3. Population size | E: one family of 48. F: one family of 144. G: one family of 432 | 6 |

Planned machine type: `c4a-standard-8` (Arm, 8 cores, one vCPU per core), `c4a-standard-16` for arm G, image family
`ubuntu-2404-lts-arm64`. A pilot on one VM comes first.

**What I measured that affects your side.**

- The VM must start with `+scl false`. Without it the Erlang VM drops to two scheduler threads after any quiet stretch
  and stays there. `scripts/dxnn_worker.py` line 96 already passes it for GCS runs. Please keep that.
- 144 agents peak at about 1.9 GB of memory. 432 agents is unmeasured.
- A generation takes 30 to 200 seconds depending on network size, so a checkpoint that waits for a generation boundary
  can wait several minutes. The existing `pause_timeout_seconds: 900` is right.
- Everything on my side was developed and tested on Erlang/OTP 28 (28.4.1, erts 16.3, macOS on Apple silicon). It has
  never been run on OTP 25, on Linux or on Arm Linux.

**Data rule.** `fx_tables/EURUSD_M1_MIDPOINT_UTC.txt` contains a held-out final test period (2025 onward) that must
never be read by a training run. Only `EURUSD1.txt` and `EURUSD1_LIVE.txt` may be uploaded. The bundler already
allow-lists `application.data_files`; see request 9.

## Requests, in priority order

### 1. Erlang/OTP 28 on the VMs (needed)

*Now:* `scripts/install-gcp-worker.sh` line 23 installs Ubuntu 24.04's `erlang` packages, which are OTP 25. The runtime
YAMLs say `otp_release: "25"`, and the installer and worker both check the installed release against it.

*Wanted:*

- Install a pinned OTP 28 on both `amd64` and `arm64` Ubuntu 24.04 images. 28.4.x preferred, to match where the code
  was tested. Any reliable source is fine (a maintained package repository, or a build from source cached in the image),
  as long as the exact version is pinned and the same on every VM of a fleet.
- The JIT must be enabled on both architectures (it is by default in OTP 28 for x86-64 and AArch64).
- `runtime_tools` and `mnesia` must be present. Nothing else beyond the standard library is used. No NIFs, no ports.
- Set `application.otp_release: "28"` in `config/gcp-dxnn-runtime.example.yml` and keep both version checks.
- `tests/run-gcp-erlang-tests.sh` uses the `erlang:25` Docker image. Move it to `erlang:28`.

*Done when:* `erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().'` prints `28` on a freshly
installed Arm VM and on an x86 VM, and the Erlang test script passes on OTP 28.

### 2. Pass settings to a run from the command line (needed)

*Now:* every VM runs whatever `exp_runner:get_run_configs/0` returns, so every VM runs the same experiment. The worker
sets `DXNN_RUN_CONFIGS_FILE` when `run-configs.term` exists in the application directory (`dxnn_worker.py` lines
258 to 259), but the GCP install never uploads such a file and training-core does not read that variable.

*Wanted:*

- `docker-gcp-dxnn.sh ... start <instance> --run <id> --lineage <id> --env KEY=VALUE` (repeatable). The worker adds
  each pair to the environment of the Erlang process it starts.
- Accept only keys that start with `DXNN_` and are not ones the worker sets itself (`DXNN_STORAGE_PROVIDER`,
  `DXNN_FINAL_DIR`, `DXNN_STOP_DIR`, `DXNN_RESTORE_DIR`, `DXNN_CHECKPOINT_PAUSE_MS`, `DXNN_RUN_CONFIGS_FILE`, `RUN_ID`).
- Store the pairs with the run's metadata and in every checkpoint's metadata.
- `resume` must start the new run with the same pairs as the run it resumes, without the user repeating them. A
  resumed population that silently switched arm would ruin the test.
- Show the pairs in `status`.

*What I will do with it:* training-core will read `DXNN_TEST_ARM` (for example `t2c`) and `DXNN_EPISODE_SEED` in
`exp_runner:get_run_configs/0`. With neither set, behaviour is unchanged.

*Update, 2026-11-13 evening (training-core `REVIEW.md` 8.23):* done, with a third variable. No variable was renamed.

- `DXNN_TEST_ARM`: the arm tags are now `ff_level`, `rec_level`, `ff_vol`, `rec_vol`, `fold_2015`, `fold_2016`,
  `fold_2017`, or `<arm>+<fold>` such as `ff_vol+fold_2015`. The tags `t1b` to `t3g` of the earlier plan (and of
  `config/gcp-fleet.example.yml`) are not defined, and an unknown tag stops the experiment at its start. Unset or empty
  is the reference experiment.
- `DXNN_EPISODE_SEED`: an integer.
- `DXNN_EVOLUTION_SEED`: new, an integer. With the same two seeds two runs of the same arm give the same
  `GEN_SUMMARY` score columns, so the seeds belong with the arm in a run's identity, like the other `--env` pairs.
- Values are read as strings and must be integers where an integer is expected; anything else stops the experiment.

*Done when:* two VMs installed from the same source and started with different `--env DXNN_TEST_ARM=...` values run
different arms, and a stop followed by `resume` keeps the arm.

### 3. Final results usable on another machine (needed)

*Now:* on completion `dxnn_runtime:finish/0` writes a Mnesia backup, and `collect_diagnostics` (`dxnn_worker.py` lines
204 to 211) adds `config.erl`, `run.out`, any crash dump and the `logs` folder. That set is right. The problem is the
backup: it is written by node `dxnn@127.0.0.1`, and my analysis tools start Mnesia with no node name
(`nonode@nohost`, directory `Mnesia.nonode@nohost`). A Mnesia backup cannot be restored under a different node name
without rewriting it.

*Wanted, either of:*

- an option on `storage download`, for example `--as-node nonode@nohost`, that rewrites the downloaded backup with
  `mnesia:traverse_backup/4` (the standard node-rename procedure: replace the node in the schema records' table
  definitions) and leaves a file I can load with `mnesia:install_fallback/1`; or
- have `finish/0` also upload a tarball of the node's Mnesia directory, and I will do the rename.

Please also confirm that the final backup contains every population of the experiment, not only the last. Each chained
run saves its own population in Mnesia and the analysis replays agents from each one.

*Done when:* I can download one finished run, start `erl` with no node name in a directory containing the result, and
`mnesia:dirty_all_keys(population)` returns one population per chained run.

### 4. One manifest for many VMs (strongly preferred)

*Now:* one launch YAML per VM, and every command names one instance.

*Wanted:* a fleet file listing, per VM: name, machine type, image family, provisioning, and the `start` arguments
(run ID, lineage, `--env` pairs). Commands `launch`, `install`, `start`, `status`, `logs`, `stop` and `terminate`
accept the fleet file and act on every entry, in parallel where safe, and report per VM. Per-VM launch YAMLs can be
generated from one template plus the per-entry overrides. A failure on one VM must not stop the others, and the
summary must say which failed.

### 5. Progress without SSH (strongly preferred)

*Now:* `logs` tails the generation summary over SSH for one VM.

*Wanted:* `status` (single VM and fleet) shows, per VM: run state, current chained run index, generation number, time
of the last `GEN_SUMMARY` line, and that line's `gen_seconds` and `vm_mb` fields. The source is the last line of
`logs/Benchmarker/generation_summary.log`, whose fields are `key=value` separated by ` | `. Uploading that file and
`run.out` with each periodic checkpoint would also let a dead VM be diagnosed from the bucket.

### 6. Same build on every VM (strongly preferred)

*Now:* there is a `runtime_hash` and a `source_commit` in `scripts/gcp_dxnn.py`.

*Wanted:* fleet `status` shows the source hash per VM and flags any that differ, and `resume` refuses to continue a
run on a build with a different hash unless forced. The working tree of training-core is mostly uncommitted, so the
hash must be of the uploaded file contents, not only the git commit.

### 7. Run facts in the metadata (strongly preferred)

Record with each run and in the final result: machine type, number of cores Erlang sees
(`erlang:system_info(schedulers_online)`), CPU architecture, OTP version, the emulator flags used, provisioning
(spot or standard), and the `--env` pairs. The analysis compares speed across machine types and needs these.

### 8. Stop paying when a run ends (strongly preferred)

`completion.action: stop` exists. For fleet runs make it the default, and add a maximum wall time per run after which
the worker takes a checkpoint, uploads, and powers the VM off. A run that hangs should not bill indefinitely.

### 9. Hard block on the held-out data file (preferred)

The bundler only uploads `application.data_files`. Add an explicit refusal to bundle
`EURUSD_M1_MIDPOINT_UTC.txt` even if someone lists it there, with a message that it holds the final test period.

### 10. Automatic resume after a Spot preemption (nice to have)

*Now:* after a preemption the VM stops and someone must start it and run `resume`.

*Wanted:* when a stopped VM is started again and its last run did not complete, the worker resumes it from the newest
checkpoint under a derived run ID. With 15 VMs, manual resume makes Spot impractical.

### 11. Emulator flags in the runtime YAML (nice to have)

Move the flags from `dxnn_flags` in `dxnn_worker.py` into the runtime YAML, default `+scl false`, so a scheduler-count
test (for example `+S 8:8 +scl false`) needs no code change. `+scl false` must remain in the default.

### 12. Quota check before a fleet launch (nice to have)

Before launching, compare the fleet's total cores per machine family and region with the project's quota and stop with
a clear message if it does not fit. The planned fleet is about 136 C4A cores in one region.

## Shared files: please do not change these without telling the user

The deployment code calls into training-core, and I am editing the same files:

- `population_monitor.erl`: the `dxnn_checkpoint_pause` call and `dxnn_checkpoint_cancel` cast, and the restore of
  counters and selection memory from `DXNN_RESTORE_DIR`.
- `exp_runner.erl`: `dxnn_runtime:finish()` at the end of an experiment, `resolve_experiment_for_continue/1`,
  `continue/1`, and the `DXNN_STORAGE_PROVIDER` check.
- `launcher.erl`: `dxnn_runtime:resume_or_new()`.

I will keep those hooks as they are. The check in `scripts/gcp_dxnn.py` (`HOOKS`, line 40) that looks for them in the
source is useful; keep it.

## What I am adding on the training-core side

- `exp_runner:get_run_configs/0` reads `DXNN_TEST_ARM`, `DXNN_EPISODE_SEED` and `DXNN_EVOLUTION_SEED` (done
  2026-11-13, see the update under request 2).
- Two fill rules in the simulator (no orders in the rollover hours; fill one bar late), off by default (done; the arms
  switch the first one on).
- A run plan with fixed generation counts in place of wall-time limits, so VMs of different speed do the same work
  (done for the arms: 30 + 60 + 3 x 50 generations; the reference experiment still has its two timed runs).
- A short pilot arm (a few generations per stage) for testing the whole path on one VM.

## Pilot acceptance (one `c4a-standard-8`, standard provisioning)

1. Installs and compiles on Arm with OTP 28; `run.out` has no line starting `WARNING | this VM was started without`.
2. The first generations report `gen_seconds` in `GEN_SUMMARY`.
3. A manual `checkpoint`, then `stop`, then `resume` continues the same population, generation count and arm.
4. On completion the VM powers off and the final result is in the bucket.
5. The downloaded result opens on the Mac as described in request 3.
