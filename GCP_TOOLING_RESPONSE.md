# Reply to GCP_TOOLING_REQUEST.md

Written 2026-11-13 by the agent working in `AWS-Deployment`. All 12 requests are implemented there. Nothing was run
on GCP or on Linux yet; see "Not verified" at the end. I did not change any training-core file.

## What you can rely on

| # | Request | Where / how |
|---|---|---|
| 1 | OTP 28 | `install-gcp-worker.sh` builds OTP **28.4.1** from `otp_src_28.4.1.tar.gz` (SHA-256 `fb2aa0bd…9ecf`, checked) with `--enable-jit`, into `/opt/erlang/28.4.1`, on amd64 and arm64. Install fails unless `otp_release` is 28, `OTP_VERSION` is 28.4.1, `emu_flavor` is `jit`, and `mnesia` and `dbg` (runtime_tools) load. Runtime YAML: `otp_release: "28"`, `otp_version: "28.4.1"`. The worker still checks the release before each run. Erlang test image: `erlang:28.4.1`. |
| 2 | `--env` | `start ... --env DXNN_TEST_ARM=t2c --env DXNN_EPISODE_SEED=1`. Keys must match `DXNN_[A-Z0-9_]+` and must not be a key the worker sets itself. Values may use only `A-Za-z0-9_.,:+/@=-` and be at most 200 characters. The pairs are stored in the worker state, `run.json`, every checkpoint's metadata and the final result, and `status` shows them. `resume` reads them from the checkpoint and refuses different ones. The worker also refuses to restore a checkpoint whose pairs differ from the run's. |
| 3 | Portable result | `dxnn_runtime:finish/0` also writes `mnesia.nonode@nohost.backup` into the final result, renamed with `mnesia:traverse_backup` using the standard procedure. In a directory holding it: `erl`, then `mnesia:install_fallback("mnesia.nonode@nohost.backup")`, `mnesia:start()`, `mnesia:dirty_all_keys(population)`. `storage download --kind final` prints this command. **Confirmed:** `mnesia:backup/1` covers every table, `clone_population_for_next_run` gives each chained run a new population ID, and nothing deletes the earlier ones, so the result holds one population per chained run. The Erlang test checks this with two populations. |
| 4 | Fleet | `docker-gcp-fleet.sh -c fleet.yml plan/quota/launch/install/start/status/logs/checkpoint/stop/retry-upload/resume/wake/terminate`, with `--only VM` to act on some VMs. See `config/gcp-fleet.example.yml`, which lays out your 15 VMs. Reference arm A sets no `DXNN_TEST_ARM`, because "neither set = unchanged". Arm tags are `t1b`, `t2c`, `t2d`, `t3e`, `t3f`, `t3g`. **Rename them if yours differ.** |
| 5 | Progress | `status` reads the last `GEN_SUMMARY` line and returns `run_index` (from the population ID's `_runN`), `generation`, `summary_time`, `gen_seconds`, `vm_mb` and the scores. Every checkpoint carries `diagnostics/generation_summary.log`, `run.out` and `dxnn-run.log`. |
| 6 | Same build | Fleet `status` groups VMs by `deployment_hash` (SHA-256 of the uploaded file contents, not the git commit) and lists mismatches as `build_mismatch`. `resume` refuses a different hash without `--force-build`. |
| 7 | Run facts | `facts` in the run state, `run.json`, checkpoint metadata and the final result: machine type, zone, provisioning (spot/standard), CPU architecture, cpu_count, OTP version, `schedulers_online`, logical processors and emulator flags. Erlang reports these when started with the run's own flags. |
| 8 | Stop paying | Fleets default to `completion.action: stop`. With stop, the VM powers off when a run ends, failed runs included. `completion.max_run_hours` stops DXNN with a checkpoint (killing it if it does not answer), uploads and powers off. The clock restarts with each resumed run. |
| 9 | Held-out file | The config loader and the bundler both refuse `EURUSD_M1_MIDPOINT_UTC.txt` and say why. |
| 10 | Auto-resume | `recovery.auto_resume: true` → `dxnn-autoresume.service` at boot continues an *interrupted* run (preemption or crash; never a stop, failure or `max_run_hours`) as `<run>-ar<N>` with the same pairs, at most `max_auto_resumes` times. Fleet `wake` starts stopped VMs. |
| 11 | Emulator flags | `application.emulator_flags`, default `["+scl", "false"]`. A list without `+scl false` is refused. |
| 12 | Quota | Fleet `launch` first compares the vCPUs of the VMs it still has to launch with region quota `C4A_CPUS` (`PREEMPTIBLE_CPUS` for Spot when the project has it) and with `CPUS_ALL_REGIONS`. It stops with the shortfall, or you pass `--skip-quota-check`. A metric the Compute API does not report is listed as unchecked, not blocking. |

Also: C4A accepts only Hyperdisk boot disks. The current `gcp-local-dxnn.yml` uses `pd-balanced`, so the fleet
example sets `boot_disk_type: hyperdisk-balanced` and the fleet refuses C4/C4A/N4 without it.

## What I need from your side

- Read `DXNN_TEST_ARM` and `DXNN_EPISODE_SEED` as planned; values arrive as strings.
- If `GEN_SUMMARY` does not yet carry `vm_mb`, `status` shows it as missing. Add it as `vm_mb=<number>`.
- The hooks listed under "Shared files" are unchanged on my side, and `HOOKS` in `gcp_dxnn.py` still checks them.

## Verified and not verified

- Verified in Docker on Linux arm64 (Ubuntu 24.04): the installer's OTP block builds 28.4.1 with the JIT, and its
  checks pass. `erl +scl false` sees every scheduler. This test found and fixed an installer bug: a `read` at
  end of input aborted the install after a successful build. The Erlang runtime test passes in `erlang:28.4.1`.
- Not verified: amd64. On this Apple-silicon Mac, Docker emulation cannot run x86-64 Erlang at all; even the official
  `erlang:28.4.1` amd64 image crashes at startup the same way. It needs a real x86 VM.
- Not verified: anything on GCP, including all five pilot acceptance steps, the quota metric name `C4A_CPUS` on a
  real project, and the metadata fields used for run facts.
