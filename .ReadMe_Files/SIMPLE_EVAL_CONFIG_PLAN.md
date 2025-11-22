# Simple Evaluation-Based Configuration Implementation Plan

## Overview
Use the existing evaluation tracking in population_monitor to switch data parameters based on total evaluations completed. Minimal code changes, maximum leverage of existing infrastructure.

## Core Concept
- **Evaluations 1-20**: Use first data range configuration
- **Evaluations 21-50**: Use second data range configuration  
- **Evaluations 51+**: Use third data range configuration
- **Use existing `tot_evaluations` counter** in population_monitor state

## Architecture Design

### Key Components
1. **`eval_config.erl`** - Simple parameter provider based on evaluation count
2. **Modified `config.erl`** - Check eval_config before returning defaults
3. **Tiny addition to `population_monitor.erl`** - Set global config when evaluations update

### How It Works
1. Population monitor already tracks `tot_evaluations` in its state
2. When evaluations are updated, check if we need to switch parameter ranges
3. Config functions automatically return the right parameters based on current evaluation count
4. Zero changes to sensors, neural networks, or core architecture

## Implementation Steps

### Step 1: Create Simple `eval_config.erl` Module

```erlang
-module(eval_config).
-compile(export_all).

%% Simple evaluation-based configuration using global process dictionary

%% Set current evaluation count (called by population monitor)
set_current_evaluation(EvalCount) ->
    put(current_evaluation_count, EvalCount).

%% Get current evaluation count
get_current_evaluation() ->
    case get(current_evaluation_count) of
        undefined -> 0;
        Count -> Count
    end.

%% Get parameters for current evaluation count
get_current_params() ->
    EvalCount = get_current_evaluation(),
    get_params_for_evaluation(EvalCount).

%% Get parameters for specific evaluation number
get_params_for_evaluation(EvalCount) when EvalCount =< 20 ->
    #{
        currency_pair => 'EURUSD1',
        gt_start => 2500,
        gt_end => 2000,
        bench_start => 2000,
        bench_end => 1800,
        description => io_lib:format("Evaluation ~p: High volatility period (1-20)", [EvalCount])
    };
get_params_for_evaluation(EvalCount) when EvalCount =< 50 ->
    #{
        currency_pair => 'EURUSD1',
        gt_start => 1000,
        gt_end => 800,
        bench_start => 800,
        bench_end => 600,
        description => io_lib:format("Evaluation ~p: Low volatility period (21-50)", [EvalCount])
    };
get_params_for_evaluation(EvalCount) ->
    #{
        currency_pair => 'EURUSD1',
        gt_start => 5000,
        gt_end => 4500,
        bench_start => 4500,
        bench_end => 4000,
        description => io_lib:format("Evaluation ~p: Trend reversal period (51+)", [EvalCount])
    }.

%% Check if we need to switch configurations and do it
update_config_if_needed(NewEvalCount) ->
    OldEvalCount = get_current_evaluation(),
    OldRange = get_range_category(OldEvalCount),
    NewRange = get_range_category(NewEvalCount),
    
    set_current_evaluation(NewEvalCount),
    
    case OldRange =/= NewRange of
        true ->
            Params = get_params_for_evaluation(NewEvalCount),
            io:format("=== EVALUATION CONFIG SWITCH ===~n"),
            io:format("~s~n", [maps:get(description, Params)]),
            io:format("Parameters: GT=~p-~p, Bench=~p-~p~n", 
                      [maps:get(gt_start, Params), maps:get(gt_end, Params),
                       maps:get(bench_start, Params), maps:get(bench_end, Params)]),
            io:format("===============================~n");
        false ->
            ok
    end.

%% Helper: Get which range category an evaluation count falls into
get_range_category(EvalCount) when EvalCount =< 20 -> range1;
get_range_category(EvalCount) when EvalCount =< 50 -> range2;
get_range_category(_EvalCount) -> range3.

%% Status report
status() ->
    EvalCount = get_current_evaluation(),
    Params = get_current_params(),
    io:format("=== Evaluation Configuration Status ===~n"),
    io:format("Current Total Evaluations: ~p~n", [EvalCount]),
    io:format("~s~n", [maps:get(description, Params)]),
    io:format("Parameters: GT=~p-~p, Bench=~p-~p~n", 
              [maps:get(gt_start, Params), maps:get(gt_end, Params),
               maps:get(bench_start, Params), maps:get(bench_end, Params)]),
    io:format("=====================================~n").
```

### Step 2: Modify `config.erl` to Check Evaluation Config

```erlang
%% Replace these functions in config.erl:

primary_currency_pair() -> 
    case catch eval_config:get_current_params() of
        #{currency_pair := Value} -> Value;
        _ -> 'EURUSD1'  % Fallback to default
    end.

gt_start() -> 
    case catch eval_config:get_current_params() of
        #{gt_start := Value} -> Value;
        _ -> 1000  % Fallback to default
    end.

gt_end() -> 
    case catch eval_config:get_current_params() of
        #{gt_end := Value} -> Value;
        _ -> 200  % Fallback to default
    end.

bench_start() -> 
    case catch eval_config:get_current_params() of
        #{bench_start := Value} -> Value;
        _ -> 200  % Fallback to default
    end.

bench_end() -> 
    case catch eval_config:get_current_params() of
        #{bench_end := Value} -> Value;
        _ -> last  % Fallback to default
    end.
```

### Step 3: Add Tiny Hook in `population_monitor.erl`

Find the `handle_cast({From,evaluations,Specie_Id,AEA,AgentCycleAcc,AgentTimeAcc},S)` function around line 265 and add ONE line:

```erlang
handle_cast({From,evaluations,Specie_Id,AEA,AgentCycleAcc,AgentTimeAcc},S)->
    AgentEvalAcc = case S#state.goal_reached of
        true -> 0;
        false -> AEA
    end,
    Eval_Acc = S#state.eval_acc,
    U_EvalAcc = S#state.eval_acc+AgentEvalAcc,
    U_CycleAcc = S#state.cycle_acc+AgentCycleAcc,
    U_TimeAcc = S#state.time_acc+AgentTimeAcc,
    U_TotEvaluations = S#state.tot_evaluations + AgentEvalAcc,
    
    %% ADD THIS ONE LINE: Update evaluation config if needed
    eval_config:update_config_if_needed(U_TotEvaluations),
    
    %% ... rest of existing function unchanged ...
```

## Usage Examples

### System Usage
```erlang
%% Start your normal benchmarking - no initialization needed!
benchmarker:start(my_experiment).

%% The system automatically:
%% - Evaluations 1-20: Uses gt_start=2500, gt_end=2000 (high volatility)
%% - Evaluation 21: Automatically switches to gt_start=1000, gt_end=800 (low volatility)  
%% - Evaluation 51: Automatically switches to gt_start=5000, gt_end=4500 (trend reversal)

%% You'll see console output like:
%% === EVALUATION CONFIG SWITCH ===
%% Evaluation 21: Low volatility period (21-50)
%% Parameters: GT=1000-800, Bench=800-600
%% ===============================
```

### Monitoring Progress
```erlang
%% Check current evaluation status anytime
eval_config:status().
% === Evaluation Configuration Status ===
% Current Total Evaluations: 35
% Evaluation 35: Low volatility period (21-50)  
% Parameters: GT=1000-800, Bench=800-600
% =====================================
```

### Testing Individual Ranges
```erlang
%% Test specific evaluation parameters
eval_config:get_params_for_evaluation(15).  % Returns high volatility params
eval_config:get_params_for_evaluation(35).  % Returns low volatility params
eval_config:get_params_for_evaluation(75).  % Returns trend reversal params
```

## Customization

### Modify Evaluation Boundaries
Simply edit the `get_params_for_evaluation/1` function:

```erlang
%% Custom boundaries example:
get_params_for_evaluation(EvalCount) when EvalCount =< 10 ->
    #{gt_start => 3000, gt_end => 2800, description => "Easy period (1-10)"};
get_params_for_evaluation(EvalCount) when EvalCount =< 30 ->
    #{gt_start => 1500, gt_end => 1200, description => "Medium period (11-30)"};
get_params_for_evaluation(EvalCount) ->
    #{gt_start => 6000, gt_end => 5500, description => "Hard period (31+)"}.
```

### Add More Parameters
```erlang
%% Add any additional parameters you want to switch:
get_params_for_evaluation(EvalCount) when EvalCount =< 20 ->
    #{
        currency_pair => 'EURUSD1',
        gt_start => 2500,
        gt_end => 2000,
        bench_start => 2000,
        bench_end => 1800,
        account_leverage => 50,        % Custom parameter
        account_balance => 1000,       % Custom parameter  
        description => "High volatility with standard leverage"
    }.

%% Then add corresponding config functions:
account_leverage() ->
    case catch eval_config:get_current_params() of
        #{account_leverage := Value} -> Value;
        _ -> 50  % Fallback to default
    end.
```

## Benefits of This Approach

1. **Leverages Existing Infrastructure**: Uses population_monitor's existing evaluation tracking
2. **Minimal Changes**: Only 1 line added to population_monitor, 5 functions modified in config
3. **Automatic**: Evaluation counting and switching happens transparently
4. **Real-time**: Configuration switches exactly when evaluation thresholds are crossed
5. **Visible**: Clear console output when configuration switches occur
6. **Backward Compatible**: Falls back to config.erl defaults if eval_config fails
7. **Simple**: Easy to understand, test, and modify
8. **No Global State**: Uses process dictionary, no ETS tables or complex state management

## File Summary

**Files to Create:**
- `eval_config.erl` (new module - ~100 lines)

**Files to Modify:**
- `config.erl` (modify 5 functions to check eval_config first)
- `population_monitor.erl` (add 1 line in handle_cast function)

**Files Unchanged:**
- All sensor, neural network, scape, and other core files

## Testing Plan

```erlang
%% Phase 1: Module testing
eval_config:get_params_for_evaluation(5).   % Test range 1
eval_config:get_params_for_evaluation(25).  % Test range 2  
eval_config:get_params_for_evaluation(75).  % Test range 3

%% Phase 2: Integration testing
eval_config:set_current_evaluation(15).
config:gt_start().  % Should return 2500 (range 1)

eval_config:set_current_evaluation(35).  
config:gt_start().  % Should return 1000 (range 2)

%% Phase 3: Full system testing
benchmarker:start(test_eval_switching).
% Watch console for automatic switches at evaluations 21 and 51
```

This approach gives you exactly what you want: evaluation-based parameter switching using the existing population monitor evaluation tracking with minimal code changes!
