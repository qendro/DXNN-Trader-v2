# Benchmarker Phase 1 Implementation Plan
## Dynamic Configuration Per Run - Option A (Inline Configs Only)

---

## Overview

**Phase 1 Goal**: Enable benchmarker to run multiple sequential runs, each with different configuration parameters, without requiring code recompilation or manual intervention between runs.

**Current Behavior**: Benchmarker runs X identical runs for statistical purposes. Each run uses the same config values from `config.erl`.

**Desired Behavior**: Benchmarker runs X runs where each run can have different configuration parameters (e.g., `evaluations_limit`, `generation_limit`, `specie_size_limit`). Config values are:
- Defined **inline** when starting the experiment as a list of key-value pairs
- Applied before each run via the ETS override system
- Base configuration always comes from `config.erl` defaults
- Only overridden values need to be specified (non-overridden values use defaults)

**Approach**: **Option A - Inline Configs Only**
- No separate config files
- All config overrides defined inline in Erlang code
- Simple, self-contained approach
- Configs stored in experiment record in Mnesia

**Implementation Strategy**: **Direct modification of `benchmarker.erl`**
- Modify the existing `benchmarker.erl` file directly (no separate `benchmarker2.erl`)
- Current setup can be changed/simplified as needed to support dynamic configs
- API is simplified - `start/2` is the primary function (RunConfigs required, can be empty list)
- Internal implementation can be restructured as needed
- **No backward compatibility** - Old API calls need to be updated

**Note**: Phase 1 does NOT handle agent restoration between runs. That is Phase 2. Phase 1 still creates fresh populations each run, but with different configurations.

---

## Current State Analysis

### Current Flow

1. **`benchmarker:start(Id)`** 
   - Reads config values from `config.erl` via function calls (e.g., `config:evaluations_limit()`)
   - Creates experiment record with PM parameters using these values
   - Sets `run_index=1`, `tot_runs=config:tot_runs()`
   - Spawns `benchmarker:prep/1`

2. **`benchmarker:prep(E)`**
   - Calls `population_monitor:prep_PopState(PMP, Constraints)`
   - Enters `loop/2`

3. **`benchmarker:loop(E, P_Id)`**
   - Receives `{P_Id, completed, Trace}` from population monitor
   - Increments `run_index`
   - If more runs: calls `prep_PopState` again with **same config values**
   - If done: generates report and exits

### Current Config System

The `config.erl` module already has a dynamic override system:

```erlang
%% ETS-based config override system
-define(CONFIG_TAB, dxnn_config).

config:init()           % Initialize ETS table
config:set(Key, Value)  % Set override value
config:get_val(Key, Default)  % Get value (checks ETS first, then defaults)
config:clear()          % Clear all overrides
```

All config functions use `get_val/2`, so they automatically read from ETS if set, otherwise use hardcoded defaults.

### Problem

The config values are read **once** at experiment start time and stored in the `#pmp{}` record. When starting a new run, the same `#pmp{}` values are reused. Even if we change ETS values, the `#pmp{}` record still has the old values.

---

## Design Decisions

### Decision 1: Config Override Timing

**Option A**: Store config overrides in experiment record, apply before each run
- ✅ Clean separation of concerns
- ✅ Configs are part of experiment definition
- ✅ Easy to persist and resume

**Option B**: Apply configs externally before each run
- ❌ Less explicit
- ❌ Harder to track what config was used for which run

**Decision**: **Option A** - Store run configs in experiment record.

### Decision 2: Config Storage Format

**Option A**: Inline list in experiment record
```erlang
run_configs = [
    {1, [{evaluations_limit, 50000}, {generation_limit, 5}]},
    {2, [{evaluations_limit, 100000}, {generation_limit, 10}]}
]
```

**Option B**: File references (NOT IMPLEMENTED)
**Option C**: Mixed approach (NOT IMPLEMENTED)

**Decision**: **Option A** - Inline configs only. Simple, self-contained approach. No file management required.

### Decision 3: When to Read Config Values

**Option A**: Re-read config values when starting each run (from ETS)
- ✅ Allows dynamic changes
- ✅ Supports runtime config updates

**Option B**: Use stored values in `#pmp{}` record
- ❌ Can't change config between runs

**Decision**: **Option A** - Re-read config values from ETS when creating `#pmp{}` for each run. This means we apply overrides to ETS, then read config functions to build the `#pmp{}` record.

---

## Implementation Steps

**Note**: These steps modify the existing `benchmarker.erl` directly (no separate `benchmarker2.erl` file). The current internal setup can be changed/simplified as needed during implementation. **No backward compatibility** is required - we can fully restructure the code.

### Step 1: Update Records

**File**: `records.hrl`

Add `run_configs` field to `experiment` record:

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
    run_configs=[],  % NEW: [{RunIndex, ConfigList | {file, Filename}}, ...]
    notes,
    started={date(),time()},
    completed,
    interruptions=[]
}).
```

**Format of `run_configs`**:
- `[]` - No run configs, use defaults from `config.erl` for all runs (empty list)
- `[{1, [{evaluations_limit, 50000}]}, {2, [{evaluations_limit, 100000}]}]` - Inline config overrides per run

**Note**: The `run_configs` field is always present in new experiments. Old experiments without this field will need to be recreated.

### Step 2: Add Config Loading Helper Function to `config.erl`

**File**: `config.erl`

Add function to load configs from inline lists:

```erlang
%% Load configuration from list of key-value pairs
%% ConfigList format: [{Key1, Value1}, {Key2, Value2}, ...]
%% This applies overrides to the ETS table
load_from_list(ConfigList) ->
    init(),
    [set(Key, Value) || {Key, Value} <- ConfigList],
    ok.
```

### Step 3: Modify `benchmarker:start/2` to Accept Run Configs

**File**: `benchmarker.erl`

**Approach**: Directly modify the existing `benchmarker.erl` file. Current setup can be restructured as needed. **No backward compatibility** - replace `start/1` with `start/2`.

Update function signature and initialization:

```erlang
%% New signature: start(Id, RunConfigs)
%% RunConfigs can be [] for default configs, or [{RunIndex, ConfigList}, ...]
start(Id, RunConfigs) ->
    %% Initialize config system
    config:init(),
    
    %% Apply config for first run if specified
    apply_run_config(1, RunConfigs),
    
    %% Build PMP - values will be read from config:XXX() which checks ETS
    PMP = #pmp{
        op_mode=benchmark,
        population_id=test,
        survival_percentage=config:survival_percentage(),
        specie_size_limit=config:specie_size_limit(),
        init_specie_size=config:init_specie_size(),
        polis_id = mathema,
        generation_limit = config:generation_limit(),
        evaluations_limit = config:evaluations_limit(),
        fitness_goal = inf
    },
    
    E=#experiment{
        id = Id,
        backup_flag = true,
        pm_parameters=PMP,
        init_constraints=get_init_constraints(),
        progress_flag=in_progress,
        run_index=1,
        tot_runs=config:tot_runs(),
        run_configs=RunConfigs,  % Store run configs
        started={date(),time()},
        interruptions=[]
    },
    
    qlog:benchmarker(Id,io_lib:format("EXPERIMENT_START | op_mode=~p | population_id=~p | tot_runs=~p | run_configs=~p",[PMP#pmp.op_mode, PMP#pmp.population_id, E#experiment.tot_runs, length(RunConfigs)])),
    genotype:write(E),
    register(benchmarker,spawn(benchmarker,prep,[E])).
```

### Step 4: Add Config Application Helper Function

**File**: `benchmarker.erl`

Add helper to apply config for a specific run:

```erlang
%% Apply configuration for a specific run
%% RunConfigs format: [{RunIndex, ConfigList}, ...]
%% ConfigList format: [{Key1, Value1}, {Key2, Value2}, ...]
apply_run_config(RunIndex, RunConfigs) ->
    case RunConfigs of
        [] ->
            %% No run configs - use defaults from config.erl
            ok;
        _ ->
            case lists:keyfind(RunIndex, 1, RunConfigs) of
                {RunIndex, ConfigList} when is_list(ConfigList) ->
                    %% Load from inline list
                    config:clear(),
                    config:load_from_list(ConfigList),
                    qlog:benchmarker(self(), io_lib:format("CONFIG_LOADED | run=~p | items=~p", [RunIndex, length(ConfigList)]));
                false ->
                    %% No config for this run - use defaults from config.erl
                    ok
            end
    end.
```

### Step 5: Modify `benchmarker:loop/2` to Apply Config Per Run

**File**: `benchmarker.erl`

Update the loop to apply configs before starting each new run:

```erlang
loop(E,P_Id)->
	receive
		{P_Id,completed,Trace}->

			U_TraceAcc = [Trace|E#experiment.trace_acc],
			U_RunIndex = E#experiment.run_index+1,
			case U_RunIndex > E#experiment.tot_runs of
				true ->
					%% All runs complete - clear config overrides
					config:clear(),
					qlog:benchmarker(E#experiment.id,io_lib:format("TRAINING_COMPLETE | population_id=~p | runs=~p",[P_Id, U_RunIndex-1])),
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex,
						completed = {date(),time()},
						progress_flag = completed
					},
					genotype:write(U_E),
					qlog:benchmarker(E#experiment.id,io_lib:format("EXPERIMENT_COMPLETE | final_runs=~p", [U_E#experiment.run_index-1])),
					report(U_E#experiment.id,"report"),
					checkpoint_and_exit();
				false ->
					%% Apply config for next run
					apply_run_config(U_RunIndex, E#experiment.run_configs),
					
					%% Rebuild PMP with new config values (read from config:XXX() functions)
					Old_PMP = E#experiment.pm_parameters,
					U_PMP = Old_PMP#pmp{
						survival_percentage=config:survival_percentage(),
						specie_size_limit=config:specie_size_limit(),
						init_specie_size=config:init_specie_size(),
						generation_limit=config:generation_limit(),
						evaluations_limit=config:evaluations_limit(),
						benchmarker_pid=self()
					},
					
					U_E = E#experiment{
						trace_acc = U_TraceAcc,
						run_index = U_RunIndex,
						pm_parameters=U_PMP
					},
					genotype:write(U_E),
					
					Constraints = U_E#experiment.init_constraints,
					population_monitor:prep_PopState(U_PMP,Constraints),
					checkpoint(),
					loop(U_E,P_Id)
			end;
		terminate ->
			qlog:benchmarker(E#experiment.id,io_lib:format("BENCHMARKER_TERMINATE | population_id=~p",[P_Id])),
			ok
	end.
```

### Step 6: Update `benchmarker:continue/1` to Handle Run Configs

**File**: `benchmarker.erl`

Ensure `continue/1` applies config for the current run:

```erlang
continue(Id)->
	case genotype:dirty_read({experiment,Id}) of
		undefined ->
			io:format("Can't continue experiment:~p, it's not present in the database.~n",[Id]);
		E ->
			case E#experiment.progress_flag of
				completed ->
					io:format("Experiment:~p already completed:~p~n",[Id,E#experiment.trace_acc]);
				in_progress ->
					%% Initialize config system
					config:init(),
					
					%% Apply config for current run
					CurrentRunIndex = E#experiment.run_index,
					apply_run_config(CurrentRunIndex, E#experiment.run_configs),
					
					Interruptions = E#experiment.interruptions,
					U_Interruptions = [now()|Interruptions],
					U_E = E#experiment{
						interruptions = U_Interruptions
					},
					genotype:write(U_E),
					register(benchmarker,spawn(benchmarker,prep,[U_E]))
			end
	end.
```

### Step 7: Update `benchmarker:prep/1` to Use Current Config

**File**: `benchmarker.erl`

Ensure prep reads config values at runtime:

```erlang
prep(E)->
	%% Rebuild PMP with current config values (in case config changed)
	Old_PMP = E#experiment.pm_parameters,
	PMP = Old_PMP#pmp{
		survival_percentage=config:survival_percentage(),
		specie_size_limit=config:specie_size_limit(),
		init_specie_size=config:init_specie_size(),
		generation_limit=config:generation_limit(),
		evaluations_limit=config:evaluations_limit(),
		benchmarker_pid=self()
	},
	Constraints = E#experiment.init_constraints,
	Population_Id = PMP#pmp.population_id,
	population_monitor:prep_PopState(PMP,Constraints),
	loop(E#experiment{pm_parameters=PMP},Population_Id).
```

---

## Configuration Format (Inline Lists)

Config overrides are defined as lists of key-value tuples in Erlang code.

**Format**: `[{Key1, Value1}, {Key2, Value2}, ...]`

**Example**:
```erlang
[
    {evaluations_limit, 50000},
    {generation_limit, 5},
    {specie_size_limit, 2}
]
```

### Available Config Keys

Any function in `config.erl` can be overridden. Common keys:

**Evolution Parameters:**
- `evaluations_limit` - Max evaluations per run
- `generation_limit` - Max generations per run
- `specie_size_limit` - Max agents per species
- `init_specie_size` - Initial agents per species
- `survival_percentage` - Percentage of agents that survive
- `tot_runs` - Number of benchmark runs

**Trading Parameters:**
- `account_leverage` - Account leverage (1-500)
- `account_initial_balance` - Starting balance
- `account_spread` - Spread cost
- `account_lot_size` - Lot size

**Data Parameters:**
- `gt_start` - Training data start index
- `gt_end` - Training data end index
- `bench_start` - Benchmark data start index
- `bench_end` - Benchmark data end index
- `primary_currency_pair` - Currency pair to trade

**Neural Parameters:**
- `morphology` - Agent morphology
- `connection_architecture` - Network architecture
- `neural_activation_functions` - List of activation functions

**Note**: See `config.erl` for complete list of configurable parameters.

---

## Usage Examples

### Example 1: Inline Config List

```erlang
%% Define configs for each run
RunConfigs = [
    {1, [
        {evaluations_limit, 50000},
        {generation_limit, 5},
        {specie_size_limit, 2}
    ]},
    {2, [
        {evaluations_limit, 100000},
        {generation_limit, 10},
        {specie_size_limit, 4}
    ]},
    {3, [
        {evaluations_limit, 200000},
        {generation_limit, 20},
        {specie_size_limit, 8}
    ]}
].

%% Start experiment
benchmarker:start(my_experiment, RunConfigs).
```

### Example 2: No Run Configs (Default Configs)

```erlang
%% Uses default config values for all runs
benchmarker:start(my_experiment, []).
```

### Example 3: Progressive Configuration

```erlang
%% Gradually increase difficulty
RunConfigs = [
    {1, [{evaluations_limit, 10000}, {generation_limit, 2}]},    % Quick test
    {2, [{evaluations_limit, 50000}, {generation_limit, 5}]},    % Medium
    {3, [{evaluations_limit, 100000}, {generation_limit, 10}]},  % Full
    {4, [{evaluations_limit, 500000}, {generation_limit, 50}]}   % Extended
].

benchmarker:start(progressive_experiment, RunConfigs).
```

### Example 4: Different Data Windows Per Run

```erlang
RunConfigs = [
    {1, [{gt_start, 1000}, {gt_end, 500}]},   % Train on older data
    {2, [{gt_start, 2000}, {gt_end, 1000}]},  % Train on newer data
    {3, [{gt_start, 3000}, {gt_end, 2000}]}   % Train on most recent
].

benchmarker:start(data_window_test, RunConfigs).
```

### Example 5: Partial Overrides (Most Common Use Case)

```erlang
%% Only override specific values - everything else uses defaults from config.erl
RunConfigs = [
    {1, [{evaluations_limit, 50000}]},      % Only change evaluations_limit
    {2, [{evaluations_limit, 100000}]},     % Only change evaluations_limit
    {3, [{evaluations_limit, 200000}]}      % Only change evaluations_limit
    %% All other 47 config values use defaults from config.erl
].

benchmarker:start(my_experiment, RunConfigs).
```

---

## Codebase Impact Analysis

### Overview

This implementation modifies **3 files** and has **minimal impact** on the rest of the codebase. The changes leverage the existing ETS-based config override system, so no modules that read config values need to be modified.

### Files Modified

1. **`records.hrl`** - Add one field to experiment record
2. **`config.erl`** - Add one helper function (`load_from_list/1`)
3. **`benchmarker.erl`** - Modify 4 functions, add 1 helper function

### Files NOT Modified (Zero Impact)

All other modules that read config values via `config:XXX()` functions will automatically use the overridden values because:
- They already call `config:XXX()` functions (e.g., `config:evaluations_limit()`)
- These functions use `get_val/2` which checks ETS first, then defaults
- The ETS override system is already in place and working

**Modules with zero impact include:**
- `population_monitor.erl` - Already calls `config:evaluations_limit()`, `config:generation_limit()`, etc.
- `fx.erl` - Already calls `config:account_initial_balance()`, `config:account_leverage()`, etc.
- `sensor.erl` - Already calls config functions for sensor parameters
- `actuator.erl` - Already calls config functions for trading parameters
- All other modules using config functions

### Detailed Impact by Module

#### 1. `records.hrl` - **MINOR CHANGE**

**Change**: Add `run_configs` field to `#experiment{}` record

**Impact**:
- ✅ **Storage**: Adds ~200-1000 bytes per experiment depending on number of config overrides
- ⚠️ **Mnesia schema**: No schema migration needed (Erlang records are flexible)
- ⚠️ **Old experiments**: Existing experiments will need to be recreated with new API

**Risk Level**: **LOW** - Record additions are straightforward

---

#### 2. `config.erl` - **MINOR CHANGE**

**Change**: Add `load_from_list/1` function

**Impact**:
- ✅ **Non-breaking**: New function, doesn't modify existing functions
- ✅ **No dependencies**: Self-contained function using existing `set/2` and `init/0`
- ✅ **Size**: Adds ~10 lines of code

**Functions Modified**: None (only adding new function)

**Functions Added**:
- `load_from_list/1` - Loads config overrides from list of key-value pairs

**Risk Level**: **LOW** - Pure addition, no existing code affected

---

#### 3. `benchmarker.erl` - **SIGNIFICANT CHANGE**

**Implementation Approach**: Direct modification of existing `benchmarker.erl`. Current setup can be restructured as needed to support dynamic configuration. Focus on functionality over preserving exact internal structure.

**Functions Modified**:

1. **`start/1`** → **`start/2`**
   - Change: Replace `start/1` with `start/2` (RunConfigs is now required parameter)
   - Impact: ✅ **Cleaner API** - No overload needed, RunConfigs can be `[]` for defaults
   - Lines changed: ~15-20 lines
   - **Note**: Can restructure initialization logic as needed

2. **`loop/2`**
   - Change: Apply config overrides before each new run, rebuild `#pmp{}` record
   - Impact: ✅ **Internal change** - Same inputs/outputs, different internal behavior
   - Lines changed: ~10-15 lines
   - **Note**: Current loop structure can be simplified/modernized if helpful

3. **`prep/1`**
   - Change: Rebuild `#pmp{}` record reading config values at runtime
   - Impact: ✅ **Internal change** - Ensures config values are current
   - Lines changed: ~5-10 lines
   - **Note**: Can simplify or restructure prep logic

4. **`continue/1`**
   - Change: Apply config for current run when continuing interrupted experiment
   - Impact: ✅ **Works with run_configs** - Assumes experiment has run_configs field
   - Lines changed: ~5-10 lines

**Functions Added**:

1. **`apply_run_config/2`**
   - New helper function to apply config overrides for a specific run
   - Impact: ✅ **Internal use only** - Not exported (or exported for testing)
   - Lines: ~15 lines

**Functions Unchanged (can be modified if needed)**:
- `report/2` - Reporting functionality
- `prepare_Graphs/1` - Graph preparation
- `write_Graphs/2` - Graph writing
- Checkpoint functions - Can be updated if needed

**Total Lines Changed in `benchmarker.erl`**: ~50-60 lines (minimum), but structure can be simplified further if beneficial

**Risk Level**: **MEDIUM** - Core benchmarker logic, but we have flexibility to improve structure while adding dynamic config support

---

### System-Wide Impact Analysis

#### Config Reading Flow (No Changes Required)

```
Before Phase 1:
┌─────────────┐
│ config.erl  │
│ defaults    │
└──────┬──────┘
       │ config:XXX()
       ▼
┌─────────────┐
│ Modules     │
│ (fx, pm,    │
│  sensor...) │
└─────────────┘

After Phase 1 (Same Flow):
┌─────────────┐
│ config.erl  │
│ defaults    │
└──────┬──────┘
       │ get_val(Key, Default)
       ▼
┌─────────────┐
│ ETS Override│ ← Can override defaults
│ Table       │
└──────┬──────┘
       │ config:XXX()
       ▼
┌─────────────┐
│ Modules     │
│ (fx, pm,    │ ← NO CHANGES NEEDED
│  sensor...) │   (automatically use overrides)
└─────────────┘
```

**Key Insight**: Because the existing config system already uses `get_val/2` (which checks ETS first), all modules automatically pick up overridden values without any code changes.

---

### Dependency Analysis

#### What Reads Config Values?

**Direct Config Callers** (Automatically use overrides):
- `benchmarker.erl` - ✅ Modified to apply overrides before reading
- `population_monitor.erl` - ✅ Automatically uses overrides (reads via `config:XXX()`)
- `fx.erl` - ✅ Automatically uses overrides
- `sensor.erl` - ✅ Automatically uses overrides  
- `actuator.erl` - ✅ Automatically uses overrides
- Any module calling `config:XXX()` - ✅ Automatically uses overrides

**No changes required** because they all use `config:XXX()` functions which check ETS automatically.

#### What Gets Config Values Indirectly?

**Records that store config values** (Need rebuilding):
- `#pmp{}` record - ⚠️ **Modified** in `benchmarker.erl` to rebuild before each run
- `#constraint{}` record - ✅ **No change needed** - built via `get_init_constraints()` which calls config functions

---

### API Changes

#### New API

- **`benchmarker:start(Id, RunConfigs)`** - Primary function (replaces `start/1`)
  - `RunConfigs` can be `[]` to use default configs for all runs
  - `RunConfigs` format: `[{RunIndex, ConfigList}, ...]`

- **`benchmarker:continue(Id)`** - Works with new experiment format
  - Assumes experiment has `run_configs` field

#### Internal Implementation (Fully Flexible)

- ⚠️ **Internal structure** - Can be changed/simplified as needed
- ⚠️ **Helper functions** - Can be refactored/modernized
- ⚠️ **Code organization** - Can be improved during implementation
- ⚠️ **Old experiments** - Will need to be recreated with new API

#### Config Usage

- ✅ **All `config:XXX()` calls** - Work as before (check ETS, fallback to defaults)
- ✅ **Default values** - Still come from `config.erl` when not overridden

---

### Performance Impact

#### Memory

- **ETS Table**: ~100-500 bytes per override (key + value)
- **Experiment Record**: ~200-1000 bytes per experiment (depends on number of overrides)
- **Total**: Negligible (< 1KB per experiment even with many overrides)

#### CPU

- **Config Application**: O(N) where N = number of overrides per run (typically < 20)
- **Config Reading**: O(1) per config read (ETS lookup is constant time)
- **Total**: Negligible - config is applied once per run, reads are infrequent

#### Runtime

- **No measurable impact** - Config application happens once per run startup
- Config reads are same speed (ETS lookup is fast)

---

### Risk Assessment

| Component | Risk Level | Impact if Issue | Mitigation |
|-----------|------------|-----------------|------------|
| `records.hrl` | **LOW** | Record change might cause issues | Simple field addition |
| `config.erl` | **LOW** | New function might have bug | Simple function, easy to test |
| `benchmarker.erl` | **MEDIUM** | Config not applied correctly | Extensive logging, easy to debug |
| Config reading modules | **NONE** | N/A | No changes required |
| API changes | **LOW** | Old code using `start/1` will fail | Requires code updates |

**Overall Risk**: **LOW-MEDIUM** - Changes are localized and leverage existing, tested infrastructure

---

## Testing Strategy

### Unit Tests

1. **Config Loading**
   - Test `config:load_from_list/1` with valid list
   - Test `config:load_from_list/1` with empty list
   - Test `config:load_from_list/1` with invalid keys (should be ignored)
   - Test `config:clear/0` clears all overrides

2. **Config Application**
   - Test `apply_run_config/2` with inline configs
   - Test `apply_run_config/2` with missing run index (should use defaults)
   - Test `apply_run_config/2` with empty run_configs list
   - Test `apply_run_config/2` clears previous configs before applying new ones

3. **Benchmarker Integration**
   - Test `start/2` stores run_configs in experiment record
   - Test `loop/2` applies config before each run
   - Test config values are read correctly after override

### Integration Tests

1. **Full Experiment Flow**
   - Start experiment with 2 runs, different configs
   - Verify first run uses first config
   - Verify second run uses second config
   - Verify report generation works

2. **Config Persistence**
   - Start experiment with `start/2`
   - Stop after run 1
   - Continue experiment
   - Verify run 2 uses correct config

### Manual Testing Checklist

- [ ] Start experiment with inline configs
- [ ] Verify config values are logged correctly
- [ ] Verify config overrides are applied before each run
- [ ] Verify non-overridden values use defaults from config.erl
- [ ] Start experiment with empty run configs list `[]` (defaults)
- [ ] Continue interrupted experiment with run_configs
- [ ] Verify all runs complete successfully
- [ ] Verify report generation includes all runs

---

## Implementation Checklist

### Code Changes

- [ ] Update `records.hrl` - Add `run_configs` field to `experiment` record (1 line)
- [ ] Update `config.erl` - Add `load_from_list/1` function (~10 lines)
- [ ] Update `benchmarker.erl` - Replace `start/1` with `start/2` (~15 lines)
- [ ] Update `benchmarker.erl` - Add `apply_run_config/2` helper function (~15 lines)
- [ ] Update `benchmarker.erl` - Modify `loop/2` to apply configs per run (~15 lines)
- [ ] Update `benchmarker.erl` - Update `prep/1` to read config at runtime (~10 lines)
- [ ] Update `benchmarker.erl` - Update `continue/1` to apply config (~5 lines)

**Total**: ~71 lines of code changes across 3 files

### Documentation

- [ ] Update inline comments in modified functions
- [ ] Document inline config format (list of key-value tuples)
- [ ] Add usage examples to README

### Testing

- [ ] Test inline configs with various parameters
- [ ] Test partial overrides (only some config values)
- [ ] Test full overrides (all config values)
- [ ] Test with empty run configs list (defaults)
- [ ] Test continue functionality with run_configs

---

## Potential Issues & Solutions

### Issue 1: Invalid Config Key

**Problem**: Typo in config key name (e.g., `evaluations_limt` instead of `evaluations_limit`).

**Solution**: Config system ignores unknown keys via `set/2` and `get_val/2`. Only valid keys that match `config.erl` function names will have any effect. Invalid keys are silently ignored (no error, just no override).

**Mitigation**: Consider adding validation in `load_from_list/1` to log warnings for unknown keys (optional enhancement).

### Issue 2: Config Override Not Applied

**Problem**: Config value not changing between runs.

**Solution**: 
- Ensure `config:clear()` is called in `apply_run_config/2` before loading new config
- Verify `#pmp{}` record is rebuilt in `loop/2` before starting new run
- Check logs for `CONFIG_LOADED` messages to verify configs are being applied

**Debug**: Add logging to see what config values are actually being used:
```erlang
qlog:benchmarker(self(), io_lib:format("CONFIG_DEBUG | evaluations_limit=~p | generation_limit=~p", 
    [config:evaluations_limit(), config:generation_limit()]))
```

### Issue 3: Constraints Don't Update

**Problem**: `init_constraints` are built once at start, don't reflect config changes.

**Solution**: Constraints use `config:XXX()` function calls which check ETS, so they should update. However, if constraints are stored in experiment record, may need to rebuild them per run.

**Note**: Current implementation builds constraints in `get_init_constraints()` which calls `config:morphology()` etc., so constraints should update automatically.

---

## Future Enhancements (Post Phase 1)

These are out of scope for Phase 1 but worth noting:

1. **Config File Support**: Add ability to load configs from `.config` files (Option B)
2. **Config Validation**: Validate config keys/values before applying
3. **Config Templates**: Support config inheritance/templates  
4. **Config Diff Logging**: Log what config values changed between runs
5. **Config Rollback**: Ability to rollback to previous config if run fails
6. **Config Import/Export**: Save/load config sets for reuse across experiments

---

## Summary

Phase 1 (Option A - Inline Configs) enables dynamic configuration per run by:

1. **Storing run configs** in the experiment record as inline lists of key-value pairs
2. **Loading configs** from inline lists via `config:load_from_list/1`
3. **Applying configs** before each run via the existing ETS override system
4. **Re-reading config values** when building `#pmp{}` for each run
5. **Leveraging existing infrastructure** - No changes needed to modules that read config values

### Implementation Approach

- **Direct modification** of `benchmarker.erl` (no separate `benchmarker2.erl` file)
- **Current setup can be changed** - not constrained to preserve exact internal structure
- **API simplified** - `start/2` is the primary function (no overload needed)
- **No backward compatibility** - Old code using `start/1` will need to be updated
- **Flexibility to improve** - Can simplify/restructure code during implementation

### Key Benefits of Option A

- ✅ **Simple**: No file management, everything in code
- ✅ **Self-contained**: Configs stored with experiment in Mnesia
- ✅ **Flexible**: Override any or all config parameters
- ✅ **Clean API**: Single function `start/2` instead of overloads
- ✅ **Low impact**: Only 3 files modified, ~71 lines of code (minimum)
- ✅ **Modernizable**: Opportunity to improve code structure during implementation
- ✅ **No legacy baggage**: Can fully restructure without compatibility concerns

### Codebase Impact Summary

- **Files Modified**: 3 (`records.hrl`, `config.erl`, `benchmarker.erl`)
- **Lines Changed**: ~71 lines total (minimum), can be more if code is simplified/restructured
- **Files NOT Modified**: All other modules (automatically use overrides via existing ETS system)
- **Breaking Changes**: `start/1` replaced with `start/2` (old code needs update)
- **Performance Impact**: Negligible
- **Risk Level**: Low-Medium
- **Implementation Freedom**: High (can restructure internals as needed, no backward compatibility constraints)

**Next Phase**: Phase 2 will add agent restoration between runs, so agents from run N are reused in run N+1 instead of creating fresh populations.

