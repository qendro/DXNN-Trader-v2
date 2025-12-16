# Benchmarking Orchestrator (Design Overview)

Recommended module name: `exp_runner` (purpose-driven, avoids overloading the current `benchmarker` and makes room for orchestration/snapshotting responsibilities).

## Goals
- Deterministic, reproducible runs driven by `config.erl` + per-run overrides (no hashing needed).
- Safe warm starts (clone from prior populations); compatibility checks can be added later.
- Clean provenance with concise, human-readable logs and low atom bloat (use binaries for dynamic IDs).
- Straightforward snapshot/restore of Mnesia without unintended cross-run bleed.

## High-Level Architecture
- **Entry / CLI**: `exp_runner:start(RunId, RunConfigList)` mirrors today’s API but always stamps a unique, binary `population_id` (e.g., `<<"exp_<RunId>_<ISO8601>">>`).
- **Config Loader**: Apply `config.erl` defaults plus run overrides; capture the effective config (for logging) without hashing.
- **Compatibility Checker (optional)**: Can be added later to guard mismatches (morphology, encoding_type, connection_arch, substrate_plasticity, substrate_linkform, sensor/actuator resolutions). Simple mode skips this.
- **Population Manager**:
  - `fresh`: create new population per run config.
  - `clone_from`: copy a source population into the new `population_id`, remap specie_ids/agent_ids, reset fitness/generation/trace (carry genomes), skip compatibility checks in simple mode.
- **Snapshot Manager**:
  - End of run: stop/quiesce Mnesia, copy `Mnesia.<node>` to `snapshots/<RunId>/<timestamp>/`.
  - Record path in DB and log; provide restore/clone-from-snapshot utilities.
- **Run Loop**: Coordinates PMP, run configs, progression across runs; writes per-run stats to DB and log.
- **Metrics Reporter**: Summaries (best fitness, gen reached, evals, agent count, exit status) pushed into experiment/population records and log.

## Stored DB (Mnesia) Shape
- **experiment** (new or extend existing):
  - `id`, `start_time`, `end_time`, `run_index`, `tot_runs`
  - `config_blob` (effective config), `config_schema_version`, `code_version`
  - `population_id`, `source_population_id` (if cloned), `snapshot_path`, `snapshot_checksum`
  - `status` (in_progress/completed/failed), `notes`
- **population** (reuse, add fields as needed):
  - `id` (binary, timestamped), `polis_id`, `specie_ids`
  - `config_blob` (copy), `config_schema_version`, `code_version`
  - `compat_tags` (morphology, encoding_type, connection_arch, substrate_plasticity, substrate_linkform, sensor_resolutions)
  - `source_population_id` (for lineage), `trace` (as today)
- **specie/agent/cortex/neuron/sensor/actuator/substrate**: keep as-is; on clone, remap IDs, reset `fitness`, `generation`, `trace`, `evo_hist`.
- **catalog (optional new table)**:
  - One row per population: `population_id`, `config_hash`, `compat_tags`, `best_fitness`, `generation`, `agent_count`, `snapshot_path`, `status`.

## Log Structure (human-first, concise)
- Location: `logs/exp_runner.log` (rotate as needed).
- Max ~5 lines per run, separated by blank lines for readability.
- Separate visual markers for a fresh batch vs continuing evolution.
- Capture only essentials: who/when/mode, key config knobs, and end-of-run stats/snapshot.

## Flow Per Run (conceptual)
1) Load config → normalize → log + store effective config in DB (no hashing).
2) Decide population strategy: `fresh` vs `clone_from` (simple mode skips compatibility check).
3) Launch run loop (PMP + population_monitor) using the new `population_id`.
4) On completion: write run stats to DB, emit log entry, snapshot Mnesia with checksum.
5) Optionally prune/export old snapshots based on retention policy.

### Run Modes (simple, no compatibility check)
- `exp_runner:start(fresh)`: always build a new population (new `population_id`); apply current `config.erl` + run overrides; no reuse; snapshot at end.
- `exp_runner:start(new_evo)`: run 1 is `fresh`; each subsequent run clones the previous run’s population into a new `population_id`, remaps specie_ids/agent_ids, carries genomes, and resets fitness/generation/trace (and other run-time stats). Config is re-applied each run.
- `exp_runner:start(evo, PopId)`: clone the specified population into a new `population_id`, carry genomes, reset fitness/generation/trace, then run with current configs. (Simple mode does not check compatibility.)

## Filesystem Layout (DB + Snapshots)
- **Live DB (Mnesia dir)**: `./Mnesia.<node>` (current working directory). Contains Mnesia DETS/LOG files for tables: `agent.DAT`, `population.DAT`, `specie.DAT`, `cortex.DAT`, `sensor.DAT`, `actuator.DAT`, `neuron.DAT`, `substrate.DAT`, `schema.DAT`, plus log files `L*`, `TRANSIENT*` as usual.
- **Snapshots (per run)**: `snapshots/<date>_<run_id>_<timestamp>/` containing a full copy of the Mnesia dir and a checksum file; the date prefix keeps folders sorted.
  ```
  snapshots/
    2024-02-11T15-30-45Z_exp_scaling_001/
      mnesia_checksum.sha256
      Mnesia.nonode@nohost/
        agent.DAT
        population.DAT
        specie.DAT
        cortex.DAT
        sensor.DAT
        actuator.DAT
        neuron.DAT
        substrate.DAT
        schema.DAT
        LATEST.LOG
        LOG.*
  ```
- **Logs**: `logs/exp_runner.log` (rotate as needed).

## Log Format and Examples
## Log Format and Examples (human-friendly, max ~5 lines per run)
- Use blank lines to separate runs. Use a header marker to distinguish “fresh batch” from “continuing evolution”.
- Keep it terse; list only key knobs and outcomes.
- Example (fresh start, matches current `config.erl` defaults):
  ```
  === FRESH RUN START ===
  ts: 2024-02-11T15:30:45Z | experiment: scaling | run: 1 | population: 2024-02-11T15-30-45Z_exp_scaling_001 | mode: fresh | source: none
  config: morph=forex_trader enc=substrate arch=recurrent plast=none link=l2l_feedforward sensors=pli[20,40],pci[90x20] gt=1000-200 bench=200-last evo=generational selection=competition fpost=size_proportional survival=0.5 specie_size_limit=2 init_specie_size=2 tuning_duration={const,10}
  ```
- Example end (success):
  ```
  ts: 2024-02-11T16:05:12Z | status: completed | best_fitness: 0.2347 | generation: 1 | tot_evaluations: 10000000 | agent_count: 2
  snapshot: snapshots/2024-02-11T16-05-15Z_exp_scaling_001/Mnesia.nonode@nohost
  ```
- Example start (continuing evolution):
  ```
  === CONTINUE EVO ===
  ts: 2024-02-12T10:12:01Z | experiment: scaling | run: 2 | population: 2024-02-12T10-12-01Z_exp_scaling_002 | mode: new_evo | source: 2024-02-11T15-30-45Z_exp_scaling_001
  config: morph=forex_trader enc=substrate arch=recurrent plast=none link=l2l_feedforward sensors=pli[20,40],pci[90x20] gt=1000-200 bench=200-last evo=generational selection=competition fpost=size_proportional survival=0.5 specie_size_limit=2 init_specie_size=2 tuning_duration={const,10}
  ```
- Example end (failure note):
  ```
  ts: 2024-02-12T10:45:00Z | status: failed | reason: runtime_error | err: <<"details...">>
  ```

## Implementation Plan (exp_runner)
- **New module**: `exp_runner.erl` to orchestrate runs and logging; keep `benchmarker.erl` intact.
- **Modes**: Implement `start/0`, `start(fresh)`, `start(new_evo)`, `start(evo, PopId)` with the semantics described above. Mode selection should be explicit in logs.
- **Population handling**:
  - Fresh: call existing `population_monitor:prep_PopState/2` path (via benchmarker API) with a new `population_id`.
  - Clone: add a utility to duplicate a population (specie + agents + cortex/neuron/sensor/actuator/substrate), remap IDs, reset fitness/generation/trace/evo_hist, preserve genomes. Avoid whole-dir Mnesia copies.
- **Config application**:
  - Always `config:init()` then apply run overrides (`config:load_from_list/1`).
  - Capture the effective config (maybe as a sorted proplist) for logging and storing in experiment/population records.
- **Logging**:
  - Emit the 4–5-line human-readable blocks to `logs/exp_runner.log`.
  - Add clear separators for fresh vs continue.
  - Include snapshot path at end of run.
- **Snapshots**:
  - Reuse the existing checkpoint approach or add a lighter snapshot to `snapshots/<date>_<run>_<ts>/` after each run; at least copy `Mnesia.<node>` and key logs.
  - Provide a restore helper if desired (manual use).
- **Experiment/Population records**:
  - Extend `experiment`/`population` to store `config_blob` and `source_population_id` if needed; keep compatibility optional (future).
- **Run loop**:
  - Wrap current `benchmarker:start/2` (or similar) to run a batch; after completion, snapshot and log.
  - For `new_evo`, chain runs by cloning the last population into a new one before each run.
- **Safety**:
  - Use binaries for dynamic IDs to avoid atom leaks.
  - Skip compatibility checks for now; add a flag later if needed.
  - Handle errors gracefully and log failures with reason.
