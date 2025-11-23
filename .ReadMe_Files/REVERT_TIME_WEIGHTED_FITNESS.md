# Revert Time-Weighted Fitness Implementation

This document provides step-by-step instructions to completely remove the time-weighted fitness implementation and restore the system to its original state.

## Overview

This revert process will:
1. Remove all configuration functions from `config.erl`
2. Remove state record modifications from `fx.erl`
3. Restore original function signatures and logic
4. Remove helper functions
5. Restore original logging format

**Result**: System will be identical to pre-implementation state.

---

## Files Modified

The following files were modified during implementation:
1. `config.erl` - Added 4 configuration functions
2. `fx.erl` - Modified state record, functions, and added helper
3. `exoself.erl` - Modified log format (optional change)

---

## Phase 1: Remove Configuration Functions

### 1.1 Remove from config.erl

**File**: `config.erl`  
**Location**: Remove the entire "Time-Weighted Fitness Configuration" section

**Remove these functions:**
```erlang
%% ===================================================================
%% Time-Weighted Fitness Configuration
%% ===================================================================
fitness_discount_rate() -> 0.01.
fitness_realized_bonus() -> 1.25.
fitness_loss_penalty() -> 1.5.
fitness_time_weighted_enabled() -> true.
```

**Action**: Delete the entire section (usually after line 83, after neural_plasticity_functions).

---

## Phase 2: Restore fx.erl State Record

### 2.1 Remove Fields from State Record

**File**: `fx.erl`  
**Location**: ~line 40

**Current (Modified):**
```erlang
-record(state,{
    table_name,
    feature,
    index_start,
    index_end,
    index,
    price_list=[],
    cycle=0,                    % REMOVE THIS
    realized_pl_by_cycle=[]    % REMOVE THIS
}).
```

**Revert to:**
```erlang
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).
```

**Action**: Remove `cycle=0,` and `realized_pl_by_cycle=[]` from the record definition.

---

## Phase 3: Restore init_state Function

### 3.1 Remove Initialization of New Fields

**File**: `fx.erl`  
**Function**: `init_state/5`  
**Location**: ~line 379-393

**Current (Modified):**
```erlang
init_state(S,TableName,Feature,StartBL,EndBL)->
    Index_End = case EndBL of
        last ->
            ets:last(TableName);
        _ ->
            prev(TableName,ets:last(TableName),prev,EndBL)
    end,
    Index_Start = prev(TableName,ets:last(TableName),prev,StartBL),
    S#state{
        table_name = TableName,
        feature = Feature,
        index_start = Index_Start,
        index_end = Index_End,
        index = Index_Start,
        cycle = 0,                    % REMOVE THIS LINE
        realized_pl_by_cycle = []     % REMOVE THIS LINE
    }.
```

**Revert to:**
```erlang
init_state(S,TableName,Feature,StartBL,EndBL)->
    Index_End = case EndBL of
        last ->
            ets:last(TableName);
        _ ->
            prev(TableName,ets:last(TableName),prev,EndBL)
    end,
    Index_Start = prev(TableName,ets:last(TableName),prev,StartBL),
    S#state{
        table_name = TableName,
        feature = Feature,
        index_start = Index_Start,
        index_end = Index_End,
        index = Index_Start
    }.
```

**Action**: Remove the two lines initializing `cycle` and `realized_pl_by_cycle`.

---

## Phase 4: Restore update_state Function

### 4.1 Remove Cycle Tracking

**File**: `fx.erl`  
**Function**: `update_state/1`  
**Location**: ~line 398-405

**Current (Modified):**
```erlang
update_state(S)->
    NextIndex = fx:next(S#state.table_name,S#state.index),
    case NextIndex == S#state.index_end of
        true ->
            sim_over;
        false ->
            case config:fitness_time_weighted_enabled() of
                true ->
                    S#state{
                        index=NextIndex,
                        cycle=S#state.cycle+1
                    };
                false ->
                    S#state{index=NextIndex}
            end
    end.
```

**Revert to:**
```erlang
update_state(S)->
    NextIndex = fx:next(S#state.table_name,S#state.index),
    case NextIndex == S#state.index_end of
        true ->
            sim_over;
        false ->
            S#state{index=NextIndex}
    end.
```

**Action**: Remove the conditional check and cycle increment, restore simple state update.

---

## Phase 5: Restore close_order Function

### 5.1 Remove Tracking and Restore Original Signature

**File**: `fx.erl`  
**Function**: `close_order/2`  
**Location**: ~line 512-515

**Current (Modified):**
```erlang
close_order(S,A)->
    RealizedPL_Delta = A#account.unrealized_PL,
    
    U_Balance = A#account.balance + A#account.unrealized_PL,
    U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL,
    
    U_S = case config:fitness_time_weighted_enabled() of
        true ->
            Cycle = S#state.cycle,
            S#state{
                realized_pl_by_cycle = [{Cycle, RealizedPL_Delta} | S#state.realized_pl_by_cycle]
            };
        false ->
            S
    end,
    
    {U_S, A#account{
        balance=U_Balance,
        realized_PL=U_Realized_PL,
        unrealized_PL = 0,
        order=undefined
    }}.
```

**Revert to:**
```erlang
close_order(S,A)->
    U_Balance = A#account.balance + A#account.unrealized_PL,
    U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL,
    A#account{balance=U_Balance,realized_PL=U_Realized_PL,unrealized_PL = 0,order=undefined}.
```

**Action**: 
- Remove all tracking logic
- Remove state parameter (S)
- Remove return of state tuple
- Restore original function signature: `close_order(S,A)` returns `A#account{}`

---

## Phase 6: Restore make_trade Function

### 6.1 Restore Original Return Values

**File**: `fx.erl`  
**Function**: `make_trade/3`  
**Location**: ~line 468-491

**Current (Modified):**
```erlang
make_trade(S,A,Action)->
    case A#account.order of
        undefined ->
            case Action == 0 of
                true ->%Do nothing
                    {S, A};
                false ->%Open new position
                    open_order(S,A,Action)
            end;
        O ->
            case Action == 0 of
                true ->%Close Order
                    close_order(S,A);
                false ->%Modify Order
                    Current_Position = O#order.position,
                    case Current_Position == Action of
                        true ->
                            {S, A};
                        false ->
                            {U_S, U_A}=close_order(S,A),
                            open_order(U_S,U_A,Action)
                    end
            end
    end.
```

**Revert to:**
```erlang
make_trade(S,A,Action)->
    case A#account.order of
        undefined ->
            case Action == 0 of
                true ->%Do nothing
                    A;
                false ->%Open new position
                    open_order(S,A,Action)
            end;
        O ->
            case Action == 0 of
                true ->%Close Order
                    close_order(S,A);
                false ->%Modify Order
                    Current_Position = O#order.position,
                    case Current_Position == Action of
                        true ->
                            A;
                        false ->
                            U_A=close_order(S,A),
                            open_order(S,U_A,Action)
                    end
            end
    end.
```

**Action**: 
- Change all `{S, A}` returns to just `A`
- Change `{U_S, U_A}=close_order(S,A)` to `U_A=close_order(S,A)`
- Change `open_order(U_S,U_A,Action)` to `open_order(S,U_A,Action)`
- Remove state tuple handling

---

## Phase 7: Restore open_order Function

### 7.1 Restore Original Return Value

**File**: `fx.erl`  
**Function**: `open_order/3`  
**Location**: ~line 493-510

**Current (Modified):**
```erlang
open_order(S,A,Action)->
    % ... existing code ...
    U_A = A#account{unrealized_PL = Unrealized_PL,order=New_Order},
    {S, U_A}.  % REMOVE TUPLE
```

**Revert to:**
```erlang
open_order(S,A,Action)->
    % ... existing code ...
    U_A = A#account{unrealized_PL = Unrealized_PL,order=New_Order},
    U_A.  % Return just account
```

**Action**: Change `{S, U_A}` to just `U_A`.

---

## Phase 8: Restore sim/3 Function

### 8.1 Restore sim_over Case

**File**: `fx.erl`  
**Function**: `sim/3`  
**Location**: ~line 308-362, specifically the `sim_over` case

**Current (Modified):**
```erlang
{From,trade,TableName,TradeSignal}->
    {U_S, U_A} = make_trade(S,A,TradeSignal),  % REMOVE STATE TUPLE
    Total_Profit = A#account.balance + A#account.unrealized_PL,
    
    % ... debug code ...
    
    case (U_A#account.balance + U_A#account.unrealized_PL) =< 100 of
        true ->
            From ! {self(),0,1},
            fx:sim(ExoSelf,#state{},create_account());
        false ->
            case update_state(U_S) of
                sim_over ->
                    TimeWeightedFitness = calculate_time_weighted_fitness(U_S, U_A),  % REMOVE
                    From ! {self(),TimeWeightedFitness,1},  % RESTORE
                    fx:sim(ExoSelf,#state{},create_account());
                U_S2 ->
                    From ! {self(),0,0},
                    U_A2 = update_account(U_S2,U_A),
                    fx:sim(ExoSelf,U_S2,U_A2)
            end
    end;
```

**Revert to:**
```erlang
{From,trade,TableName,TradeSignal}->
    U_A = make_trade(S,A,TradeSignal),  % RESTORE: No state tuple
    Total_Profit = A#account.balance + A#account.unrealized_PL,
    
    % ... debug code ...
    
    case (U_A#account.balance + U_A#account.unrealized_PL) =< 100 of
        true ->
            From ! {self(),0,1},
            fx:sim(ExoSelf,#state{},create_account());
        false ->
            case update_state(S) of
                sim_over ->
                    Total_Profit = A#account.balance + A#account.unrealized_PL,
                    From ! {self(),Total_Profit,1},  % RESTORE: Original calculation
                    fx:sim(ExoSelf,#state{},create_account());
                U_S ->
                    From ! {self(),0,0},
                    U_A2 = update_account(U_S,U_A),
                    fx:sim(ExoSelf,U_S,U_A2)
            end
    end;
```

**Action**:
- Change `{U_S, U_A} = make_trade(S,A,TradeSignal)` to `U_A = make_trade(S,A,TradeSignal)`
- Change `update_state(U_S)` to `update_state(S)`
- Remove `TimeWeightedFitness = calculate_time_weighted_fitness(U_S, U_A)`
- Restore `Total_Profit = A#account.balance + A#account.unrealized_PL` in sim_over case
- Change `From ! {self(),TimeWeightedFitness,1}` to `From ! {self(),Total_Profit,1}`
- Change `U_S2` to `U_S` in the else case

---

## Phase 9: Remove Helper Function

### 9.1 Delete calculate_time_weighted_fitness Function

**File**: `fx.erl`  
**Function**: `calculate_time_weighted_fitness/2`  
**Location**: ~line 466 (after `determine_profit/1`)

**Action**: Delete the entire function:

```erlang
% DELETE THIS ENTIRE FUNCTION
calculate_time_weighted_fitness(S, A) ->
    case config:fitness_time_weighted_enabled() of
        false ->
            A#account.balance + A#account.unrealized_PL;
        true ->
            Discount_Rate = config:fitness_discount_rate(),
            Realized_Bonus = config:fitness_realized_bonus(),
            Loss_Penalty = config:fitness_loss_penalty(),
            
            Realized_By_Cycle = S#state.realized_pl_by_cycle,
            
            Realized_Weighted = lists:sum([
                case PL >= 0 of
                    true ->
                        PL * (1 - Discount_Rate * Cycle) * Realized_Bonus;
                    false ->
                        PL * (1 - Discount_Rate * Cycle) * Loss_Penalty
                end
                || {Cycle, PL} <- Realized_By_Cycle
            ]),
            
            Realized_Weighted
    end.
```

---

## Phase 10: Restore Logging (If Modified)

### 10.1 Restore Original Log Format

**File**: `exoself.erl`  
**Function**: `loop/2` (gt mode)  
**Location**: ~line 123-124

**Current (Modified - if changed):**
```erlang
qlog:benchmarker(S#state.agent_id,io_lib:format("TRAIN_EVAL | attempt=~p | time_weighted_fitness=~p | cycles=~p | time=~p",[S#state.attempt,Fitness,Cycles,Time])),
```

**Revert to:**
```erlang
qlog:benchmarker(S#state.agent_id,io_lib:format("TRAIN_EVAL | attempt=~p | fitness=~p | cycles=~p | time=~p",[S#state.attempt,Fitness,Cycles,Time])),
```

**Action**: Change `time_weighted_fitness` back to `fitness` in the log format string.

**Note**: This is optional - only revert if you modified the logging.

---

## Phase 11: Verification

### 11.1 Verify Revert

After completing all phases, verify the system is restored:

1. **Compile and test:**
   ```bash
   erl -make
   # Run your test suite
   ```

2. **Check for compilation errors:**
   - All files should compile without errors
   - No references to removed functions

3. **Verify behavior:**
   - Fitness calculation should be: `Balance + Unrealized_PL`
   - No time-weighted calculations should occur
   - System should behave identically to pre-implementation state

4. **Check for leftover code:**
   - Search for `fitness_time_weighted_enabled`
   - Search for `realized_pl_by_cycle`
   - Search for `calculate_time_weighted_fitness`
   - All should return no results

---

## Summary of Changes Reverted

| File | Changes Reverted |
|------|------------------|
| `config.erl` | Removed 4 configuration functions |
| `fx.erl` | Removed 2 state record fields |
| `fx.erl` | Restored `init_state/5` to original |
| `fx.erl` | Restored `update_state/1` to original |
| `fx.erl` | Restored `close_order/2` to original signature |
| `fx.erl` | Restored `make_trade/3` return values |
| `fx.erl` | Restored `open_order/3` return value |
| `fx.erl` | Restored `sim/3` sim_over case |
| `fx.erl` | Removed `calculate_time_weighted_fitness/2` |
| `exoself.erl` | Restored log format (if modified) |

---

## Quick Revert Checklist

- [ ] Remove configuration functions from `config.erl`
- [ ] Remove `cycle` and `realized_pl_by_cycle` from state record
- [ ] Remove initialization of new fields in `init_state/5`
- [ ] Restore `update_state/1` to original
- [ ] Restore `close_order/2` to original signature
- [ ] Restore `make_trade/3` return values
- [ ] Restore `open_order/3` return value
- [ ] Restore `sim/3` sim_over case
- [ ] Delete `calculate_time_weighted_fitness/2` function
- [ ] Restore log format in `exoself.erl` (if modified)
- [ ] Compile and test
- [ ] Verify no leftover references

---

## Notes

- This revert process is **complete** - it removes all traces of the time-weighted fitness implementation
- After revert, the system will be **identical** to the pre-implementation state
- No data migration is needed (the list was never persisted)
- All changes are code-only, no database schema changes

---

**Version**: 1.0  
**Created**: 2025  
**Purpose**: Complete revert of time-weighted fitness implementation

