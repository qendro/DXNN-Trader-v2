# GPT-5.4 Findings

Planning only. No implementation is included in this document.

## Scope Reviewed

I reviewed the current workspace with emphasis on the actual runtime path, not only the docs:

- Docs: `.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md`, `.ReadMe_Files/AI_README.md`, `README.md`
- Launch/orchestration: `launcher.erl`, `exp_runner.erl`, `population_monitor.erl`
- Genotype/phenotype: `genotype.erl`, `morphology.erl`, `records.hrl`
- Runtime processes: `exoself.erl`, `cortex.erl`, `sensor.erl`, `neuron.erl`, `actuator.erl`, `substrate.erl`, `substrate_cpp.erl`, `substrate_cep.erl`, `scape.erl`, `fx.erl`, `live_scape.erl`
- Monitoring/logging/utilities: `qlog.erl`, `process_monitor.erl`, `genotype_utils.erl`, `config.erl`

The workspace currently compiles cleanly: `make:all([load]) -> up_to_date`.

## Executive Summary

- The clean insertion point for an evaluator/auditor is a population-scoped process launched together with `population_monitor`, not a global monitor and not a log-file parser.
- `exoself` is already the authoritative owner of the full per-agent PID graph at spawn time. That is the right source of truth for agent runtime registration.
- The existing code already contains a useful prototype idea in `qlog:log_agent_memory_usage/0`, but it is an offline/log-driven helper, not a proper runtime auditor.
- For phase 1, the evaluator should only maintain an in-memory registry of active agent PIDs, sample memory every configurable interval, and append structured audit lines to a dedicated log file.
- The auditor should log both node-level memory and per-agent memory. Per-agent sums will not equal total BEAM memory exactly, because code, atoms, ETS, shared binaries, and non-agent processes are outside that sum.
- Before phase 2 termination/respawn work, the workspace needs a cleaner lifecycle seam than it has today.

## Actual Runtime Architecture

The real launch path today is:

1. `launcher:start/0,1` bootstraps Mnesia, FX, and Polis, then starts `exp_runner:start(new_evo)` (`launcher.erl:8-67`).
2. `exp_runner` creates run/population metadata and invokes `population_monitor` for each run (`exp_runner.erl:343-532`, `exp_runner.erl:600-625`).
3. `population_monitor:init/1` loads all agent ids for the population and summons them by calling `exoself:start(...)` (`population_monitor.erl:75-106`, `population_monitor.erl:438-447`).
4. `genotype:construct_Agent/4` and `construct_Cortex/6` define the persistent topology for each agent, including substrate metadata, CPP ids, and CEP ids (`genotype.erl:8-34`, `genotype.erl:37-98`).
5. `exoself:prep/3` spawns the agent runtime processes and already assembles the complete PID list for the agent (`exoself.erl:48-135`).

The runtime process inventory for a substrate agent is:

- `exoself`
- `cortex`
- standard `sensor` processes
- `neuron` processes
- standard `actuator` processes
- private `scape` processes
- one `substrate` process
- `substrate_cpp` processes
- `substrate_cep` processes

That aligns with the feature you described.

## Key Findings

### 1. `exoself` is the correct source of truth for per-agent PID ownership

Evidence:

- `population_monitor` only stores `{Agent_Id, ExoSelfPid}` in `activeAgent_IdPs` (`population_monitor.erl:17`, `population_monitor.erl:82-92`, `population_monitor.erl:124-129`).
- `exoself` stores the full runtime PID graph in its state: `cx_pid`, `spids`, `npids`, `apids`, `scape_pids`, `substrate_pid`, `cpp_pids`, `cep_pids` (`exoself.erl:5-30`, `exoself.erl:101-121`).
- `exoself` already flattens that graph into `All_Pids` and writes it to `pid_map.log` (`exoself.erl:125-135`).

Implication:

- The auditor should register agents from `exoself` immediately after phenotype construction, using structured runtime data rather than reconstructing it later from logs or by introspection.

### 2. A population-scoped evaluator is a better fit than a global one

Evidence:

- New populations are created and started via `population_monitor:init_population/2` and `population_monitor:init/1` (`population_monitor.erl:469-487`, `population_monitor.erl:75-106`).
- New runs and cloned populations are also funneled through `population_monitor:start/1` (`exp_runner.erl:510-529`, `exp_runner.erl:613-625`).

Implication:

- If the evaluator is launched by `population_monitor`, it will naturally follow the lifecycle of exactly one active population/run.
- This also avoids cross-run leakage of old agent PID state.

### 3. The existing `qlog:log_agent_memory_usage/0` is a useful prototype, but not an elegant runtime solution

Evidence:

- It reads `pid_map.log`, reparses agent PID lines, converts PID strings back into PIDs, and then sums `process_info(Pid, memory)` (`qlog.erl:490-570`).
- It depends on `genotype_utils:get_active_agents/0`, which only knows agent ids from `population_monitor`, not child PIDs (`genotype_utils.erl:191-209`).

Why it should not be the final design:

- It is disk-driven instead of runtime-driven.
- It reparses log files repeatedly.
- It has no notion of spawn generations or spawn refs.
- It cannot distinguish agent-owned vs shared processes.
- It is fine as a manual helper, but not as the production evaluator core.

### 4. `process_monitor.erl` provides useful primitives, but it is system-wide rather than agent-aware

Evidence:

- It can gather `memory`, `message_queue_len`, `reductions`, `heap_size`, and `total_heap_size` from PIDs (`process_monitor.erl:44-64`).
- Its public functions default to walking `erlang:processes()` globally (`process_monitor.erl:128-190`).

Implication:

- The evaluator can reuse the same BEAM APIs (`erlang:process_info/2`, `erlang:memory/0`).
- It should not reuse the global traversal model.
- The evaluator should walk only tracked agent-owned PIDs.

### 5. Role-level memory logging matters here, not only agent totals

There are two concrete reasons:

- The HyperNEAT substrate is one Erlang process holding the full substrate state, including both `old_substrate` and `cur_substrate` (`substrate.erl:6-22`, `substrate.erl:52-69`, `substrate.erl:96-116`).
- Each `neuron` keeps multiple copies of its weights for baseline/current/backup and perturbation restore paths (`neuron.erl:8-24`, `neuron.erl:37-53`, `neuron.erl:125-156`).

Implication:

- A single agent total is necessary, but not sufficient.
- The log should also emit at least these role buckets per sample:
  - `exoself`
  - `cortex`
  - `sensors`
  - `neurons`
  - `actuators`
  - `scapes`
  - `substrate`
  - `cpp`
  - `cep`

This will make it obvious whether memory pressure is caused by the substrate, the NEAT side, or environment processes.

### 6. The current training setup uses private scapes, but future shared scapes need special handling

Evidence:

- Standard trading morphology uses `{private, fx_sim}` for sensors and actuators (`morphology.erl:127-164`).
- `exoself:spawn_Scapes/5` creates private scapes per agent in normal modes, but maps to shared `live_scape` in `live_trading` (`exoself.erl:252-271`).
- `scape:live_sim/1` bridges into `live_scape` (`scape.erl:18-28`).

Implication:

- For the phase-1 population evaluator, it is safe to attribute scape memory to agents in training/benchmark modes.
- In future shared-live mode, `live_scape` must not be fully charged to a single agent.

### 7. The workspace already has lifecycle gaps that matter for phase 2 termination/respawn

Evidence:

- `population_monitor` can only externally identify active `ExoSelfPid`s, not all child PIDs (`population_monitor.erl:124-129`, `genotype_utils.erl:191-209`).
- `genotype_utils:terminate_agent/1` works around this by killing processes directly, because there is no formal exoself management API for external termination (`genotype_utils.erl:214-235` and onward).
- `exoself` loops are almost entirely focused on evaluation completion and do not expose a clean external management message contract (`exoself.erl:138-240`).

Implication:

- Phase 1 logging is safe.
- Phase 2 "terminate and later respawn agents" should not be designed as a side effect of the logger. It needs a proper lifecycle contract first.

### 8. There are real documentation/runtime mismatches in the workspace

Evidence:

- `.ReadMe_Files/AI_README.md` and `.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md` still describe `benchmarker.erl`, `ib_connector.erl`, `live_trader.erl`, and `live_trading_main`, which are not present in this workspace (`.ReadMe_Files/AI_README.md:23-27`, `.ReadMe_Files/AI_README.md:64-73`, `.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md:131-170`).
- The practical runtime path in the current workspace is `launcher:start()` and `exp_runner:start(...)` (`README.md:17-39`, `launcher.erl:47-67`, `exp_runner.erl:343-532`).

Implication:

- Any implementation plan should follow the code, not the older guide language.

### 9. There are two code inconsistencies that should be accounted for before implementation

First inconsistency:

- `population_monitor:summon_agents/2` accepts `OpMode` but ignores it and calls `exoself:start(Agent_Id, self())`, which defaults to `gt` (`population_monitor.erl:438-445`, `exoself.erl:41-45`).

Second inconsistency:

- `exp_runner:do_checkpoint/0` calls `gen_server:call(population_monitor, pause, 500)`, but `population_monitor` registers itself as `monitor`, not `population_monitor`, and pause is implemented as a cast on `{op_tag,pause}` (`population_monitor.erl:75-77`, `population_monitor.erl:285-297`, `exp_runner.erl:802-805`).

Minor consistency note:

- `config:evaluation_timeout_ms/0` exists, but `exoself` still uses a hardcoded `after 600000` timeout (`config.erl:51`, `exoself.erl:208`).

Implication:

- These do not block a phase-1 memory logger.
- They do matter if you want the evaluator to become a real runtime control plane later.

## Recommended Approach

## Primary Recommendation

Implement the evaluator as a single lightweight population-scoped process with this ownership model:

1. Launch it from `population_monitor:init/1`, before agents are summoned.
2. Store its PID in `population_monitor` state.
3. Pass the evaluator PID into `exoself` when each agent is started.
4. Have `exoself` register the full runtime PID inventory once phenotype construction is complete.
5. Have `exoself` unregister itself when the phenotype is terminated.
6. Let the evaluator sample tracked PIDs every `X` seconds and append audit lines to a dedicated log.

This is the cleanest design for the current architecture.

## Why this approach is the best fit

- One evaluator per population keeps ownership clear.
- `exoself` already knows every agent-owned PID at the right time.
- The hot path does not require Mnesia.
- The hot path does not require log parsing.
- The evaluator can stay almost entirely read-only in phase 1.
- The same registration model can be extended later for selective termination and respawn.

## Suggested Evaluator Responsibilities

The evaluator should own:

- `population_id`
- `pm_pid`
- `interval_ms`
- a runtime registry keyed by `Agent_Id`
- a `SpawnRef` per active runtime instance
- the flattened PID list per agent
- the role-grouped PID list per agent
- a file handle or log target

The evaluator should not own:

- mutation
- fitness
- selection
- respawn logic
- agent killing policy in phase 1

## Suggested Registration Contract

The clean registration event is a structured runtime message from `exoself` to evaluator after `prep/3` completes:

```erlang
{register_agent_runtime,
 PopulationId,
 AgentId,
 SpawnRef,
 #{exoself => ExoSelfPid,
   cortex => CxPid,
   sensors => SPids,
   neurons => NPids,
   actuators => APids,
   scapes => ScapePids,
   substrate => SubstratePidOrUndefined,
   cpp => CPPPids,
   cep => CEPPids}}
```

The corresponding unregister event should also carry `SpawnRef`:

```erlang
{unregister_agent_runtime, PopulationId, AgentId, SpawnRef, Reason}
```

Why `SpawnRef` matters:

- The same `Agent_Id` can appear again in later generations with a fresh runtime.
- A stale termination event must not remove a newer runtime registration.

## Sampling Strategy

Use a self-scheduled timer, not `timer:send_interval/2`:

- `erlang:send_after(IntervalMs, self(), sample)`
- reschedule after the sample finishes

This avoids timer pileups if a sample ever takes longer than expected.

Each sample should do only:

1. Read BEAM totals with `erlang:memory/0`
2. For each tracked PID, read `erlang:process_info(Pid, memory)`
3. Sum by role
4. Sum by agent
5. Drop dead PIDs from the sample result
6. Append one population summary line and one line per agent

It should not:

- read Mnesia
- parse logs
- query genotype repeatedly
- collect large `process_info` lists
- sort the entire system process table

## Recommended Metrics to Log

Per population sample:

- timestamp
- population id
- active agent count
- node total memory from `erlang:memory(total)`
- BEAM process memory from `erlang:memory(processes_used)` or `erlang:memory(processes)`
- tracked agent memory total
- untracked remainder
- sample duration in microseconds

Per agent sample:

- timestamp
- population id
- agent id
- spawn ref
- process count
- total bytes
- bytes by role bucket

Suggested line shape:

```text
2026-03-16T21:15:00Z | POP=<pop> | NODE_TOTAL=12345678 | PROC_MEM=4567890 | TRACKED=3456789 | AGENTS=24 | SAMPLE_US=1800
2026-03-16T21:15:00Z | POP=<pop> | AGENT={...} | REF=#Ref<...> | PROC_COUNT=11 | TOTAL=1432200 | EXOSELF=12000 | CORTEX=9000 | SENSORS=40000 | NEURONS=210000 | ACTUATORS=18000 | SCAPES=85000 | SUBSTRATE=980000 | CPP=7000 | CEP=6200
```

## Overhead Guardrails

To keep the evaluator cheap:

- Default interval should be conservative. I would start at 30 seconds.
- Make the interval configurable, but keep the default slow.
- Query only `memory` per PID in phase 1.
- Monitor only `ExoSelfPid` if you want lifecycle hints. Do not monitor every child PID.
- Keep the registry in evaluator state or a private ETS table, not in Mnesia.
- Prefer one dedicated evaluator log file over repeated reopening/parsing of existing files.

## Alternatives Considered

## Alternative A: Use `qlog:log_agent_memory_usage/0` periodically

Pros:

- Lowest immediate implementation effort
- Reuses existing helper logic

Cons:

- Log parsing is the wrong runtime dependency
- No spawn ref protection
- No role-level accounting
- No proper lifecycle ownership
- Less elegant

Verdict:

- Good prototype.
- Not the recommended architecture.

## Alternative B: Ask `exoself` for its process list on demand

Pros:

- Simpler than log parsing
- Avoids disk I/O

Cons:

- Adds synchronous runtime message traffic at every sample
- Still requires exoself changes
- Harder to handle races if exoself dies mid-query

Verdict:

- Acceptable fallback.
- Inferior to one-time registration from `exoself`.

## Phase 1 Plan Only

For the current request, the evaluator should do only this:

1. Start with the population.
2. Learn the active runtime PIDs for each agent.
3. Sample memory every configurable interval.
4. Log node totals and per-agent totals to a dedicated file.
5. Stop with the population.

That is enough to make RAM pressure visible without materially disturbing training.

## What I Would Not Do Yet

- I would not terminate agents yet.
- I would not respawn agents yet.
- I would not write runtime PID state to Mnesia.
- I would not rely on the older `benchmarker`-based docs.
- I would not use a global evaluator shared across populations.

## Final Recommendation

The cleanest design for this workspace is:

- Launch a single evaluator from `population_monitor`
- Register agent runtime ownership from `exoself`
- Sample only tracked agent-owned PIDs
- Log both agent totals and role totals
- Keep the implementation read-only and low-frequency in phase 1

That design is elegant, cheap, and aligned with how this codebase actually creates and owns agent processes today.
