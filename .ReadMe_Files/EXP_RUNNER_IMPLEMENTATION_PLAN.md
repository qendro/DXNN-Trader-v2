# Exp Runner Implementation Plan

## Overview

This document outlines the implementation plan for the `exp_runner` module, which will orchestrate reproducible evolutionary experiments with proper logging and population cloning.

## Goals

- Deterministic, reproducible runs driven by `config.erl` + per-run overrides
- Safe warm starts (clone from prior populations) without compatibility checks in simple mode
- Clean provenance with concise, human-readable logs (max ~5 lines per run)
- Use binary population IDs to avoid atom table bloat

## Key Design Decisions

### Run Config Format
- **REMOVED**: `population_id` from run configs (now auto-generated as binary)
- **KEPT**: All other config parameters (tuning_duration, gt_start, gt_end, specie_size_limit, etc.)
- **Example**:
  ```erlang
  [{1, [{tuning_duration, {const,1}}, {gt_start, 5000}, {gt_end, 4000}, 
        {specie_size_limit, 300}, {init_specie_size, 100}, 
        {evaluations_limit, 10000000}, {generation_limit, 5}]},
   {2, [{tuning_duration, {const,1}}, {gt_start, 4000}, {gt_end, 2500}, 
        {specie_size_limit, 300}, {init_specie_size, 250}, 
        {evaluations_limit, 20000000}, {generation_limit, 5}]}]
  ```

### Population ID Generation
- **Auto-generated**: Binary like `<<"2024-02-11T15-30-45Z_a3f9_scaling_run1">>`
- **Format**: `<<"<ISO8601>_<LineageId>_<RunId>_run<RunIndex>">>`
- **LineageId**: 4 random alphanumeric characters
  - Fresh run: Generate new random 4-char ID
  - Cloned run: Extract and reuse LineageId from source population
- **Benefits**: 
  - Avoids atom table bloat
  - Ensures uniqueness
  - Chronologically sortable (timestamp prefix)
  - Self-documenting (contains lineage ID, experiment ID, run index, and timestamp)
  - Lineage tracking embedded in ID (no separate field needed)
  - Easy to find related populations by searching for same LineageId

## Phase 1: Schema Extensions

### 1.1 Extend `experiment` Record

**File**: `records.hrl`

**Changes**:
```erlang
-record(experiment,{
    id,
    backup_flag = true,
    pm_parameters,
    init_constraints,
    progress_flag=in_progress,
    trace_acc=[],
    run_index=1,
    tot_runs=10,
    run_configs=[],
    notes,
    started={date(),time()},
    completed,
    interruptions=[]
    % No new fields needed - run_configs already stores config overrides
}).
```

**Action Items**:
- [ ] Ensure backward compatibility (no changes needed)

### 1.2 Extend `population` Record

**File**: `records.hrl`

**Changes**:
```erlang
-record(population,{
    id, 
    polis_id, 
    specie_ids=[], 
    morphologies=[], 
    innovation_factor, 
    evo_alg_f, 
    fitness_postprocessor_f, 
    selection_f, 
    trace=#trace{}
    % No new fields needed - lineage is encoded in population_id
}).
```

**Note**: Lineage, `run_index`, and `experiment_id` are encoded in the `population_id` itself:
- Format: `<<"<ISO8601>_<LineageId>_<RunId>_run<RunIndex>">>`
- Example: `<<"2024-02-11T15-30-45Z_a3f9_scaling_run1">>`
- `<LineageId>`: 4 random alphanumeric characters (e.g., "a3f9", "x9k2")
  - Fresh run: New random 4-char ID generated
  - Cloned run: LineageId extracted from source population and reused
- Can be parsed to extract lineage ID, experiment ID, and run index

**Action Items**:
- [ ] Ensure backward compatibility (no changes needed to record)

## Phase 2: Config Summary Formatting

**File**: `exp_runner.erl`

**Function**: `format_config_summary/2`

**Purpose**: Format effective config into human-readable string for logs

**Implementation**:
```erlang
format_config_summary(RunIndex, RunConfigs) ->
    % Get config for this run (if specified) or use defaults
    ConfigOverrides = case lists:keyfind(RunIndex, 1, RunConfigs) of
        {RunIndex, ConfigList} -> ConfigList;
        false -> []
    end,
    
    % Extract key values from config (overrides + defaults)
    Morph = get_config_value(morphology, ConfigOverrides),
    Enc = get_config_value(agent_encoding_types, ConfigOverrides),
    Arch = get_config_value(connection_architecture, ConfigOverrides),
    GtStart = get_config_value(gt_start, ConfigOverrides),
    GtEnd = get_config_value(gt_end, ConfigOverrides),
    SpecieSizeLimit = get_config_value(specie_size_limit, ConfigOverrides),
    InitSpecieSize = get_config_value(init_specie_size, ConfigOverrides),
    EvalLimit = get_config_value(evaluations_limit, ConfigOverrides),
    GenLimit = get_config_value(generation_limit, ConfigOverrides),
    TuningDur = get_config_value(tuning_duration, ConfigOverrides),
    
    % Format compactly
    lists:flatten(io_lib:format(
        "morph=~w enc=~w arch=~w gt=~p-~p bench=~p-~p "
        "evo=~w selection=~w fpost=~w "
        "survival=~p specie_size_limit=~p init_specie_size=~p "
        "tuning_duration=~p",
        [Morph, Enc, Arch, GtStart, GtEnd, 
         get_config_value(bench_start, ConfigOverrides),
         get_config_value(bench_end, ConfigOverrides),
         get_config_value(population_evo_alg_f, ConfigOverrides),
         get_config_value(population_selection_f, ConfigOverrides),
         get_config_value(population_fitness_postprocessor_f, ConfigOverrides),
         get_config_value(survival_percentage, ConfigOverrides),
         SpecieSizeLimit, InitSpecieSize, TuningDur]
    )).

get_config_value(Key, ConfigOverrides) ->
    case lists:keyfind(Key, 1, ConfigOverrides) of
        {Key, Value} -> Value;
        false -> config:get_val(Key, undefined)
    end.
```

**Action Items**:
- [ ] Implement config summary formatter
- [ ] Keep format concise (< 200 chars)
- [ ] Include only most important knobs

## Phase 3: Population ID Generation

### 3.1 Binary Population ID Generator

**File**: `exp_runner.erl`

**Function**: `generate_population_id/2` and `generate_population_id/3`

**Purpose**: Generate timestamped binary population IDs with lineage tracking

**Implementation**:
```erlang
% Fresh run - generate new lineage ID
generate_population_id(RunId, RunIndex) ->
    LineageId = generate_lineage_id(),
    generate_population_id(RunId, RunIndex, LineageId).

% Cloned run - reuse lineage ID from source
generate_population_id(RunId, RunIndex, SourcePopId) when is_binary(SourcePopId) ->
    LineageId = extract_lineage_id(SourcePopId),
    generate_population_id(RunId, RunIndex, LineageId);

% Internal helper with lineage ID
generate_population_id(RunId, RunIndex, LineageId) ->
    % Format: <<"<ISO8601>_<LineageId>_<RunId>_run<RunIndex>">>
    % Example: <<"2024-02-11T15-30-45Z_a3f9_scaling_run1">>
    RunIdStr = case is_atom(RunId) of
        true -> atom_to_list(RunId);
        false when is_binary(RunId) -> binary_to_list(RunId);
        false -> lists:flatten(io_lib:format("~p", [RunId]))
    end,
    Timestamp = format_iso8601(erlang:timestamp()),
    RunIndexStr = integer_to_list(RunIndex),
    <<Timestamp/binary, "_", LineageId/binary, "_", (list_to_binary(RunIdStr))/binary, 
      "_run", (list_to_binary(RunIndexStr))/binary>>.

% Generate 4 random alphanumeric characters
generate_lineage_id() ->
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
    RandomChars = [lists:nth(rand:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 4)],
    list_to_binary(RandomChars).

% Extract lineage ID from source population ID
% Format: <<"<ISO8601>_<LineageId>_<RunId>_run<RunIndex>">>
extract_lineage_id(SourcePopId) ->
    case binary:split(SourcePopId, <<"_">>, [global]) of
        [_Timestamp, LineageId | _] when byte_size(LineageId) =:= 4 ->
            LineageId;
        _ ->
            % Fallback: generate new if parsing fails
            generate_lineage_id()
    end.

format_iso8601({MegaSecs, Secs, _MicroSecs}) ->
    % Convert to ISO8601: 2024-02-11T15-30-45Z
    TotalSecs = MegaSecs * 1000000 + Secs,
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    list_to_binary(lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Y,Mo,D,H,Mi,S]))).
```

**Action Items**:
- [ ] Implement binary ID generator with lineage ID support
- [ ] Implement `generate_lineage_id/0` for 4-char random IDs
- [ ] Implement `extract_lineage_id/1` to parse lineage from source
- [ ] Test with various RunId types (atom, binary, string)
- [ ] Test lineage ID extraction and reuse
- [ ] Ensure IDs are unique and chronologically sortable
- [ ] Test parsing population_id to extract LineageId, RunId, and RunIndex

## Phase 4: Population Cloning

### 4.1 Clone Population Function

**File**: `exp_runner.erl`

**Function**: `clone_population/2`

**Purpose**: Clone entire population with ID remapping and stat reset

**Implementation Strategy**:
- Use selective transaction-based cloning
- Clone each specie and all agents within specie
- Reset fitness, generation, trace, evo_hist
- Preserve genome structure

**Key Functions Needed**:
```erlang
clone_population(SourcePopId, NewPopId) ->
    % Single transaction to clone entire population
    % Note: Lineage is encoded in NewPopId (extracted from SourcePopId)
    F = fun() ->
        SourcePop = genotype:read({population, SourcePopId}),
        NewPop = SourcePop#population{
            id = NewPopId,
            specie_ids = [],
            trace = #trace{}  % Reset trace
        },
        
        % Clone each specie
        NewSpecieIds = [
            clone_specie_with_reset(SpecieId, NewPopId) 
            || SpecieId <- SourcePop#population.specie_ids
        ],
        
        % Write new population
        genotype:write(NewPop#population{specie_ids = NewSpecieIds})
    end,
    mnesia:transaction(F).

clone_specie_with_reset(SourceSpecieId, NewPopId) ->
    Specie = genotype:read({specie, SourceSpecieId}),
    NewSpecieId = genotype:generate_UniqueId(),
    
    % Clone all agents
    NewAgentIds = [
        clone_agent_with_reset(AgentId, NewSpecieId, NewPopId)
        || AgentId <- Specie#specie.agent_ids
    ],
    
    % Create new specie with reset stats
    NewSpecie = Specie#specie{
        id = NewSpecieId,
        population_id = NewPopId,
        agent_ids = NewAgentIds,
        dead_pool = [],
        champion_ids = [],
        fitness = undefined,
        innovation_factor = {0, 0},
        stats = []
    },
    genotype:write(NewSpecie),
    NewSpecieId.

clone_agent_with_reset(SourceAgentId, NewSpecieId, NewPopId) ->
    % Use existing clone_Agent but then reset runtime fields
    CloneAgentId = genotype:clone_Agent(SourceAgentId),
    Agent = genotype:read({agent, CloneAgentId}),
    
    ResetAgent = Agent#agent{
        population_id = NewPopId,
        specie_id = NewSpecieId,
        generation = 0,
        fitness = 0,
        evo_hist = [],
        innovation_factor = 0,
        pattern = []
    },
    genotype:write(ResetAgent),
    CloneAgentId.
```

**Action Items**:
- [ ] Implement `clone_population/2`
- [ ] Implement `clone_specie_with_reset/2`
- [ ] Implement `clone_agent_with_reset/3`
- [ ] Test with populations of various sizes
- [ ] Verify all IDs are properly remapped
- [ ] Verify all runtime stats are reset

## Phase 5: QLog Extensions

### 5.1 Exp Runner Logging Function

**File**: `qlog.erl`

**New Function**: `exp_runner/2`

**Purpose**: Log exp_runner events to dedicated log file with human-readable format

**Implementation**:
```erlang
%% Exp Runner logging - human-readable, max ~5 lines per run
exp_runner(Event, Data) ->
    Dir = filename:join(get_log_root_dir(), "exp_runner"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "exp_runner.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_iso8601_for_log(erlang:timestamp()),
    
    case Event of
        run_start ->
            {RunId, RunIndex, PopId, Mode, RunConfigs} = Data,
            ConfigStr = format_config_summary(RunIndex, RunConfigs),
            ModeHeader = case Mode of
                fresh -> "=== FRESH RUN START ===";
                new_evo -> "=== CONTINUE EVO ===";
                {evo, _} -> "=== CONTINUE EVO ==="
            end,
            io:format(File, "~n~s~n", [ModeHeader]),
            io:format(File, "ts: ~s | experiment: ~p | run: ~p | population: ~p | mode: ~p~n",
                [Timestamp, RunId, RunIndex, PopId, Mode]),
            io:format(File, "config: ~s~n", [ConfigStr]);
            
        run_end ->
            {RunId, RunIndex, PopId, Trace} = Data,
            % Extract stats directly from trace and population (no record needed)
            TotEvals = Trace#trace.tot_evaluations,
            % Read population and collect agent stats
            P = genotype:dirty_read({population, PopId}),
            AgentIds = lists:flatten([genotype:dirty_read({specie, SId})#specie.agent_ids 
                                     || SId <- P#population.specie_ids]),
            % Read all agents once and extract stats
            Agents = [genotype:dirty_read({agent, AId}) || AId <- AgentIds],
            Fitnesses = [A#agent.fitness || A <- Agents, A#agent.fitness =/= undefined],
            Generations = [A#agent.generation || A <- Agents],
            BestFitness = case Fitnesses of [] -> 0.0; _ -> lists:max(Fitnesses) end,
            AvgFitness = case Fitnesses of [] -> 0.0; _ -> lists:sum(Fitnesses) / length(Fitnesses) end,
            MaxGen = case Generations of [] -> 0; _ -> lists:max(Generations) end,
            AgentCount = length(AgentIds),
            
            io:format(File, "ts: ~s | status: completed | best_fitness: ~.4f | avg_fitness: ~.4f~n",
                [Timestamp, BestFitness, AvgFitness]),
            io:format(File, "generations: ~p | tot_evaluations: ~p | agent_count: ~p~n",
                [MaxGen, TotEvals, AgentCount]);
            
        run_failed ->
            {RunId, RunIndex, Reason} = Data,
            io:format(File, "ts: ~s | status: failed | reason: ~p~n",
                [Timestamp, Reason])
    end,
    file:close(File).

format_iso8601_for_log({MegaSecs, Secs, _MicroSecs}) ->
    TotalSecs = MegaSecs * 1000000 + Secs,
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Y,Mo,D,H,Mi,S])).
```

**Action Items**:
- [ ] Add `exp_runner/2` to `qlog.erl` exports
- [ ] Implement logging function with format matching Benchmarking Architecture spec
- [ ] Add helper functions for timestamp formatting
- [ ] Test log output format

### 5.2 Stats Collection (No Record Needed)

**Note**: Stats are collected directly from the population's trace and agents at log time. No separate record is needed.

**Action Items**:
- [ ] Implement stats extraction in `qlog:exp_runner/2` for `run_end` event
- [ ] Read from `population.trace` for `tot_evaluations`
- [ ] Read from agents for fitness, generation, and count

## Phase 6: Exp Runner Module

### 6.1 Core Module Structure

**File**: `exp_runner.erl` (new file)

**Exports**:
```erlang
-module(exp_runner).
-export([
    start/1,              % start(fresh) | start(new_evo) | start({evo, PopId})
    start/2,             % start(Mode, RunId)
    start/3,             % start(Mode, RunId, RunConfigs)
    continue/1           % continue(RunId)
]).
```

### 6.2 Start Functions

**Implementation Outline**:
```erlang
start(fresh) ->
    start(fresh, generate_run_id(), []);
start(new_evo) ->
    start(new_evo, generate_run_id(), []);
start({evo, PopId}) ->
    start({evo, PopId}, generate_run_id(), []).

start(Mode, RunId) ->
    start(Mode, RunId, []).

start(Mode, RunId, RunConfigs) ->
    % 1. Initialize config
    config:init(),
    
    % 2. Apply run configs if provided
    apply_run_configs(RunId, 1, RunConfigs),
    
    % 3. Generate population ID (auto-generated, includes lineage and run_index)
    PopId = case Mode of
        fresh ->
            generate_population_id(RunId, 1);  % New lineage ID
        new_evo ->
            % First run: new lineage, subsequent: extract from previous
            generate_population_id(RunId, 1);  % Will be updated for subsequent runs
        {evo, SourcePopId} ->
            generate_population_id(RunId, 1, SourcePopId)  % Reuse lineage from source
    end,
    
    % 4. Determine population strategy
    case Mode of
        fresh ->
            create_fresh_population(PopId);
        new_evo ->
            % First run: fresh, subsequent: clone
            create_or_clone_population(RunId, PopId, 1);
        {evo, SourcePopId} ->
            clone_and_run(SourcePopId, PopId)
    end,
    
    % 5. Log run start
    qlog:exp_runner(run_start, {RunId, 1, PopId, Mode, RunConfigs}),
    
    % 6. Launch population monitor
    launch_population_monitor(PopId),
    
    % 7. Enter run loop
    run_loop(Mode, RunId, PopId, 1, RunConfigs).
```

**Run Config Application**:
```erlang
apply_run_configs(RunId, RunIndex, RunConfigs) ->
    case RunConfigs of
        [] -> ok;
        _ ->
            case lists:keyfind(RunIndex, 1, RunConfigs) of
                {RunIndex, ConfigList} when is_list(ConfigList) ->
                    config:clear(),
                    config:load_from_list(ConfigList),
                    qlog:exp_runner(config_loaded, {RunId, RunIndex, length(ConfigList)});
                false -> ok
            end
    end.
```

**Action Items**:
- [ ] Implement all start function variants
- [ ] Implement `apply_run_configs/3`
- [ ] Implement `create_fresh_population/1`
- [ ] Implement `create_or_clone_population/3`
- [ ] Implement `clone_and_run/2`
- [ ] Implement `launch_population_monitor/1`
- [ ] Implement `run_loop/5`

### 6.3 Run Loop

**Purpose**: Coordinate population monitor, collect stats, handle completion

**Implementation Outline**:
```erlang
run_loop(Mode, RunId, PopId, RunIndex, RunConfigs) ->
    receive
        {PopId, completed, Trace} ->
            % Log run end (stats extracted directly in qlog:exp_runner)
            qlog:exp_runner(run_end, {RunId, RunIndex, PopId, Trace}),
            
            % Update experiment record
            update_experiment_record(RunId, RunIndex),
            
            % Handle next run or completion
            % For next run, generate new population_id with incremented run_index
            % Reuse lineage ID from current population
            NextRunIndex = RunIndex + 1,
            NextPopId = generate_population_id(RunId, NextRunIndex, PopId),
            handle_run_completion(Mode, RunId, NextRunIndex, RunConfigs, NextPopId);
            
        {PopId, failed, Reason} ->
            qlog:exp_runner(run_failed, {RunId, RunIndex, Reason}),
            % Handle failure
            ok
    end.
```

**Action Items**:
- [ ] Implement `run_loop/5`
- [ ] Implement `update_experiment_record/2`
- [ ] Implement `handle_run_completion/5`

## Phase 7: Integration with Existing System

### 7.1 Update Population Monitor

**File**: `population_monitor.erl`

**Changes**:
- Ensure `prep_PopState/2` works with binary population IDs
- Ensure trace collection works with new system
- No code changes needed (lineage tracking is in ID, not record field)

**Action Items**:
- [ ] Test binary population IDs with population_monitor
- [ ] Verify trace collection

## Phase 8: Testing

### 8.1 Unit Tests

**Test Cases**:
- [ ] Config summary formatting with various config states
- [ ] Population ID generation (uniqueness, format, parsing)
- [ ] Population cloning (small, medium, large populations)
- [ ] Log format correctness

### 8.2 Integration Tests

**Test Cases**:
- [ ] Fresh run end-to-end
- [ ] New_evo run (multiple runs, cloning)
- [ ] Evo from specific population
- [ ] Config override application

### 8.3 Performance Tests

**Test Cases**:
- [ ] Cloning performance (100, 500, 1000 agents)

## Phase 9: Documentation

### 9.1 API Documentation

**Action Items**:
- [ ] Document all exported functions
- [ ] Provide usage examples
- [ ] Document run modes

### 9.2 User Guide

**Action Items**:
- [ ] Create user guide for exp_runner
- [ ] Document log interpretation

## Implementation Order

### Priority 1 (Foundation)
1. Schema extensions (Phase 1)
2. Config summary formatting (Phase 2)
3. Population ID generation (Phase 3)
4. QLog extensions (Phase 5)

### Priority 2 (Core Functionality)
5. Population cloning (Phase 4)
6. Exp Runner module structure (Phase 6.1-6.2)
7. Run loop (Phase 6.3)

### Priority 3 (Integration & Testing)
8. Integration (Phase 7)
9. Testing (Phase 8)
10. Documentation (Phase 9)

## Success Criteria

- [ ] Can run fresh experiments with binary population IDs
- [ ] Can clone populations and continue evolution
- [ ] Logs are human-readable and concise (~5 lines per run)
- [ ] Config summary is formatted correctly for logs
- [ ] Population IDs are chronologically sortable and self-documenting
- [ ] All runtime stats are properly reset on clone
- [ ] Backward compatibility maintained

## Notes

- **No code_version**: As requested, code_version is not included in the design
- **No config_blob**: Config is reconstructed from `run_configs` + `config.erl` defaults when needed
- **No config_schema_version**: Proplists are flexible and backward compatible
- **Must-have metrics only**: Logging includes only essential metrics (run_id, population_id, config summary, best_fitness, avg_fitness, generations, tot_evaluations, agent_count, status)
- **Simple mode**: Compatibility checks are skipped in initial implementation
- **Binary IDs**: All dynamic IDs use binaries to avoid atom table bloat
- **Transaction safety**: All cloning operations use Mnesia transactions for atomicity
- **Auto-generated population_id**: `population_id` is automatically generated per run with format `<<"<ISO8601>_<LineageId>_<RunId>_run<RunIndex>">>` and should NOT be included in run configs
- **Lineage tracking**: 4-character lineage ID is embedded in population_id - fresh runs get new ID, cloned runs reuse source's lineage ID
- **Self-documenting IDs**: Population IDs contain lineage ID, experiment ID, run index, and timestamp - no need to store separately
