# Time-Weighted Fitness Function - End-of-Evaluation Implementation Plan

## Overview
This plan implements a time-weighted fitness function by calculating it once at the end of evaluation. We track when profits are realized during the simulation, then calculate time-weighted fitness when `sim_over` occurs. This approach requires minimal code changes and leverages the existing end-of-evaluation fitness calculation.

## Objectives
1. **Time Value of Money**: Realized profits early are worth more than realized profits later
2. **Realized PL Bonus**: Realized profits receive a bonus multiplier
3. **Loss Penalty**: Realized losses receive a penalty multiplier
4. **Minimal Changes**: Only modify `fx.erl` to track and calculate at end
5. **No Unrealized Penalty**: Only count realized PL (simpler, cleaner)

## Configuration Parameters

### Default Values (to be added to config.erl)
- **Discount Rate**: 0.01 (1% per cycle)
- **Realized PL Bonus**: 1.25 (25% bonus multiplier)
- **Loss Penalty**: 1.5 (50% penalty for losses)
- **Enable/Disable**: Toggle for time-weighted fitness

### Formula
```
Time-Weighted Fitness = 
    Σ[Realized_PL_at_Cycle_N × (1 - Discount_Rate × Cycle_N) × 
      (if PL >= 0 then Realized_Bonus else Loss_Penalty)]
```

**Note**: Unrealized PL is not included in the calculation. Only realized profits/losses are time-weighted.

---

## Phase 1: Configuration Setup

### 1.1 Add Configuration Functions (config.erl)

Add time-weighted fitness configuration section:

```erlang
%% ===================================================================
%% Time-Weighted Fitness Configuration
%% ===================================================================
% Discount rate per cycle (0.01 = 1% per cycle/minute)
% For 30% annual discount: 0.30 / 525600 minutes = ~0.0000005714
% Current setting: 0.01 (more aggressive for short evaluations)
fitness_discount_rate() -> 0.01.

% Bonus multiplier for realized profits (1.25 = 25% bonus)
fitness_realized_bonus() -> 1.25.

% Penalty multiplier for realized losses (1.5 = 50% worse for losses)
fitness_loss_penalty() -> 1.5.

% Enable/disable time-weighted fitness calculation
fitness_time_weighted_enabled() -> true.  % Options: true, false
```

**File**: `config.erl`
**Location**: Add after line 83 (after neural_plasticity_functions)

---

## Phase 2: Modify fx.erl State

### 2.1 Extend State Record

Add cycle tracking and realized PL history to state:

```erlang
-record(state,{
    table_name,
    feature,
    index_start,
    index_end,
    index,
    price_list=[],
    cycle=0,                    % NEW: Track current cycle
    realized_pl_by_cycle=[]    % NEW: Track when profits were realized: [{Cycle, PL}, ...]
}).
```

**File**: `fx.erl`
**Line**: ~40 (modify existing state record)

---

## Phase 3: Initialize State

### 3.1 Initialize Cycle and Realized PL History

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
        cycle = 0,                    % NEW: Initialize cycle counter
        realized_pl_by_cycle = []     % NEW: Initialize empty list
    }.
```

**File**: `fx.erl`
**Function**: `init_state/5`
**Lines**: ~379-393

---

## Phase 4: Track Cycle Counter

### 4.1 Increment Cycle in update_state (Conditional)

```erlang
update_state(S)->
    NextIndex = fx:next(S#state.table_name,S#state.index),
    case NextIndex == S#state.index_end of
        true ->
            sim_over;
        false ->
            % Only increment cycle if time-weighted fitness is enabled
            case config:fitness_time_weighted_enabled() of
                true ->
                    S#state{
                        index=NextIndex,
                        cycle=S#state.cycle+1  % NEW: Increment cycle
                    };
                false ->
                    S#state{index=NextIndex}  % Skip cycle increment (zero overhead)
            end
    end.
```

**File**: `fx.erl`
**Function**: `update_state/1`
**Lines**: ~398-405

**Note**: When disabled, cycle tracking is skipped entirely, providing zero computational and memory overhead.

---

## Phase 5: Track Realized PL When Orders Close

### 5.1 Modify close_order to Track Realized PL (Conditional)

```erlang
close_order(S,A)->
    RealizedPL_Delta = A#account.unrealized_PL,  % What we're realizing right now
    
    U_Balance = A#account.balance + A#account.unrealized_PL,
    U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL,
    
    % NEW: Only track if time-weighted fitness is enabled
    U_S = case config:fitness_time_weighted_enabled() of
        true ->
            Cycle = S#state.cycle,
            % Track when this profit was realized
            S#state{
                realized_pl_by_cycle = [{Cycle, RealizedPL_Delta} | S#state.realized_pl_by_cycle]
            };
        false ->
            % Skip tracking - zero memory overhead when disabled
            S
    end,
    
    % NEW: Return updated state along with account
    {U_S, A#account{
        balance=U_Balance,
        realized_PL=U_Realized_PL,
        unrealized_PL = 0,
        order=undefined
    }}.
```

**File**: `fx.erl`
**Function**: `close_order/2`
**Lines**: ~512-515
**Note**: Function signature changes from `close_order(S,A)` to return `{U_S, U_A}`

**Backward Compatibility**: When `fitness_time_weighted_enabled() -> false`, no list tracking occurs, providing zero memory overhead and identical behavior to current system.

---

## Phase 6: Update make_trade to Handle State

### 6.1 Modify make_trade to Return State

```erlang
make_trade(S,A,Action)->
    case A#account.order of
        undefined ->
            case Action == 0 of
                true ->%Do nothing
                    {S, A};  % NEW: Return state and account
                false ->%Open new position
                    open_order(S,A,Action)  % Returns {S, U_A}
            end;
        O ->
            case Action == 0 of
                true ->%Close Order
                    close_order(S,A);  % NEW: Returns {U_S, U_A}
                false ->%Modify Order
                    Current_Position = O#order.position,
                    case Current_Position == Action of
                        true ->
                            {S, A};  % NEW: Return state and account
                        false ->
                            {U_S, U_A}=close_order(S,A),  % NEW: Capture state
                            open_order(U_S,U_A,Action)  % Pass state through
                    end
            end
    end.
```

**File**: `fx.erl`
**Function**: `make_trade/3`
**Lines**: ~468-491
**Note**: All return values change to `{State, Account}` tuples

---

## Phase 7: Update open_order to Return State

### 7.1 Modify open_order to Return State

```erlang
open_order(S,A,Action)->
    _Order_Size = config:order_size_percentage(),
    BuyMoney = config:buy_money_fixed(),
    Spread=A#account.spread,
    Leverage = A#account.leverage,
    _Balance = A#account.balance,
    TableName = S#state.table_name,
    Index = S#state.index,
    Row = fx:lookup(TableName,Index),
    Quote = Row#technical.close,
    Entry = Quote + Spread*Action,
    Units = round((BuyMoney*Leverage)/Entry),
    Change= Quote-Entry,
    PChange = (Change/Entry)*100,
    Profit=Action*Change*Units,
    Unrealized_PL = Profit,
    New_Order = #order{pair=TableName,position=Action,entry=Entry,current=Quote,units=Units,change=Change,percentage_change=PChange,profit=Profit},
    U_A = A#account{unrealized_PL = Unrealized_PL,order=New_Order},
    {S, U_A}.  % NEW: Return state and account
```

**File**: `fx.erl`
**Function**: `open_order/3`
**Lines**: ~493-510

---

## Phase 8: Calculate Time-Weighted Fitness at End

### 8.1 Add Calculation Helper Function

Add this function to `fx.erl`:

```erlang
% Calculate time-weighted fitness at end of evaluation
calculate_time_weighted_fitness(S, A) ->
    case config:fitness_time_weighted_enabled() of
        false ->
            % Fallback to raw fitness if disabled
            A#account.balance + A#account.unrealized_PL;
        true ->
            Discount_Rate = config:fitness_discount_rate(),
            Realized_Bonus = config:fitness_realized_bonus(),
            Loss_Penalty = config:fitness_loss_penalty(),
            
            Realized_By_Cycle = S#state.realized_pl_by_cycle,
            
            % Calculate time-weighted realized PL
            % Early realized PL gets higher value (less discount)
            Realized_Weighted = lists:sum([
                case PL >= 0 of
                    true ->
                        % Profit realized - apply discount and bonus
                        PL * (1 - Discount_Rate * Cycle) * Realized_Bonus;
                    false ->
                        % Loss realized - apply discount and penalty
                        PL * (1 - Discount_Rate * Cycle) * Loss_Penalty
                end
                || {Cycle, PL} <- Realized_By_Cycle
            ]),
            
            % Note: Unrealized PL not included (only realized PL counts)
            Realized_Weighted
    end.
```

**File**: `fx.erl`
**Location**: Add after `determine_profit/1` function (around line 466)

---

### 8.2 Modify sim_over Case to Use Time-Weighted Fitness

```erlang
{From,trade,TableName,TradeSignal}->
    {U_S, U_A} = make_trade(S,A,TradeSignal),  % NEW: Capture state
    Total_Profit = A#account.balance + A#account.unrealized_PL,
    
    case config:actuator_debug_tag() of
        true ->
            % ... existing debug code ...
            ok;
        false ->
            ok
    end,
    
    case (U_A#account.balance + U_A#account.unrealized_PL) =< 100 of
        true ->
            From ! {self(),0,1},
            io:format("Lost all money~n"),
            put(prev_PC,0),
            fx:sim(ExoSelf,#state{},create_account());
        false ->
            case update_state(U_S) of
                sim_over ->
                    % NEW: Calculate time-weighted fitness instead of raw Total_Profit
                    TimeWeightedFitness = calculate_time_weighted_fitness(U_S, U_A),
                    From ! {self(),TimeWeightedFitness,1},
                    %io:format("Sim Over:~p~n",[TimeWeightedFitness]),
                    put(prev_PC,0),
                    fx:sim(ExoSelf,#state{},create_account());
                U_S2 ->
                    From ! {self(),0,0},
                    U_A2 = update_account(U_S2,U_A),
                    fx:sim(ExoSelf,U_S2,U_A2)
            end
    end;
```

**File**: `fx.erl`
**Function**: `sim/3`
**Lines**: ~308-362

---

## Phase 9: Update Logging (Optional)

### 9.1 Update ExoSelf Log to Indicate Time-Weighted Fitness

Modify the log call in exoself to indicate it's time-weighted:

```erlang
{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
    % Fitness is now time-weighted
    qlog:benchmarker(S#state.agent_id,io_lib:format("TRAIN_EVAL | attempt=~p | time_weighted_fitness=~p | cycles=~p | time=~p",[S#state.attempt,Fitness,Cycles,Time])),
    ...
```

**File**: `exoself.erl`
**Function**: `loop/2` (gt mode)
**Lines**: ~123-124

**Note**: The fitness value is now time-weighted. If you want to also log raw balance, you'd need to calculate it separately or pass it through.

---

## Phase 10: Testing

### 10.1 Test Cases

1. **Early Profit Test**
   - Agent realizes $100 profit at cycle 5
   - Expected: $100 × (1 - 0.01 × 5) × 1.25 = $118.75

2. **Late Profit Test**
   - Agent realizes $100 profit at cycle 90
   - Expected: $100 × (1 - 0.01 × 90) × 1.25 = $12.50

3. **Multiple Profits Test**
   - Agent realizes $50 at cycle 10, $50 at cycle 50
   - Expected: 
     - Cycle 10: $50 × 0.90 × 1.25 = $56.25
     - Cycle 50: $50 × 0.50 × 1.25 = $31.25
     - Total: $87.50

4. **Loss Test**
   - Agent realizes -$100 loss at cycle 20
   - Expected: -$100 × (1 - 0.01 × 20) × 1.5 = -$120.00

5. **No Trades Test**
   - Agent never trades (no realized PL)
   - Expected: 0.0 (no contributions)

### 10.2 Validation

Run a small test population and verify:
- Agents with early profits rank higher than agents with late profits
- Agents with losses rank lower
- Time-weighted fitness accumulates correctly
- Raw fitness is replaced by time-weighted fitness

---

## Implementation Checklist

### Phase 1: Configuration
- [ ] Add `fitness_discount_rate()` to config.erl
- [ ] Add `fitness_realized_bonus()` to config.erl
- [ ] Add `fitness_loss_penalty()` to config.erl
- [ ] Add `fitness_time_weighted_enabled()` to config.erl

### Phase 2: State Modifications
- [ ] Add `cycle=0` to state record
- [ ] Add `realized_pl_by_cycle=[]` to state record

### Phase 3: Initialization
- [ ] Initialize `cycle = 0` in `init_state/5`
- [ ] Initialize `realized_pl_by_cycle = []` in `init_state/5`

### Phase 4: Cycle Tracking
- [ ] Add conditional check in `update_state/1`
- [ ] Increment cycle only if `fitness_time_weighted_enabled() -> true`
- [ ] Skip cycle increment when disabled (zero overhead)

### Phase 5: Realized PL Tracking
- [ ] Add conditional check in `close_order/2`
- [ ] Modify `close_order/2` to track realized PL and return state
- [ ] Append `{Cycle, RealizedPL_Delta}` to list only if enabled
- [ ] Skip list tracking when disabled (zero memory overhead)

### Phase 6: Trade Function Updates
- [ ] Modify `make_trade/3` to return `{State, Account}`
- [ ] Update all return values

### Phase 7: Order Function Updates
- [ ] Modify `open_order/3` to return `{State, Account}`

### Phase 8: Time-Weighted Calculation
- [ ] Add `calculate_time_weighted_fitness/2` helper function
- [ ] Modify `sim_over` case to use time-weighted fitness
- [ ] Update `sim/3` to handle state from `make_trade`

### Phase 9: Logging (Optional)
- [ ] Update log format in exoself.erl

### Phase 10: Testing
- [ ] Test early vs late profits
- [ ] Test realized vs no trades
- [ ] Test loss penalties
- [ ] Verify calculation correctness
- [ ] Test with `fitness_time_weighted_enabled() -> false` (should match current system)
- [ ] Verify zero overhead when disabled

### Phase 11: Create Revert Documentation
- [ ] Create `REVERT_TIME_WEIGHTED_FITNESS.md` document
- [ ] Document all changes to revert
- [ ] Provide step-by-step revert instructions
- [ ] Include code snippets for reverting each change

---

## Code Changes Summary

**Files Modified:**
1. `config.erl` - Add 4 configuration functions (~10 lines)
2. `fx.erl` - Modify state, track cycle/realized PL, calculate at end (~60-70 lines)
3. `exoself.erl` - Update log format (optional, ~1 line)

**Total Changes: ~70-80 lines across 2-3 files**

**No Changes Needed:**
- `cortex.erl` - Still accumulates (0 + 0 + ... + TimeWeightedFitness)
- `actuator.erl` - No changes needed
- `fitness_postprocessor.erl` - Uses accumulated fitness as-is
- `records.hrl` - No new records needed
- Message protocols - No changes needed

---

## How It Works

### Example Flow:

**Cycle 5:**
- Agent closes order, realizes $100 profit
- `close_order` appends `{5, 100}` to `realized_pl_by_cycle`
- Returns `{self(), 0, 0}` (no fitness yet)

**Cycle 10:**
- Agent closes another order, realizes $50 profit
- `close_order` appends `{10, 50}` to `realized_pl_by_cycle`
- Returns `{self(), 0, 0}` (no fitness yet)

**Cycle 800 (sim_over):**
- `calculate_time_weighted_fitness` is called
- Calculates:
  - Cycle 5: $100 × 0.95 × 1.25 = $118.75
  - Cycle 10: $50 × 0.90 × 1.25 = $56.25
  - Total: $175.00
- Returns `{self(), 175.00, 1}`
- Cortex sends `FitnessAcc = 175.00` to exoself

---

## Benefits of This Approach

1. **Minimal Changes**: Only modify `fx.erl` end-of-evaluation calculation
2. **Leverages Existing System**: Uses current `sim_over` mechanism
3. **Simple Tracking**: Just track cycle and realized PL deltas
4. **No Protocol Changes**: Still returns `{Fitness, HaltFlag}`
5. **Easy to Debug**: Can inspect `realized_pl_by_cycle` list before calculation
6. **Clean Separation**: Tracking during simulation, calculation at end

---

## Edge Cases

### 1. No Orders (All Cycles)
- `realized_pl_by_cycle = []`
- Calculation: `lists:sum([]) = 0`
- Works correctly

### 2. Multiple Orders Per Cycle
- If multiple orders close in same cycle, `realized_PL` changes by total delta
- Single entry `{Cycle, TotalDelta}` captures all
- Works correctly

### 3. Losses
- `RealizedPL_Delta < 0` triggers loss penalty
- Early losses penalized more (higher cycle number = less discount, but still penalized)

### 4. Cycle 0
- First cycle, discount = (1 - 0.01 × 0) = 1.0
- Full bonus/penalty applied
- Works correctly

### 5. Very Late Cycles
- Cycle 100: discount = (1 - 0.01 × 100) = 0.0
- No value for profits/losses at cycle 100+
- This is intentional (very late profits worth nothing)

---

## Configuration Examples

### Conservative (Small Discount)
```erlang
fitness_discount_rate() -> 0.0002.      % 0.02% per cycle
fitness_realized_bonus() -> 1.05.       % 5% bonus
fitness_loss_penalty() -> 1.2.          % 20% penalty
```

### Moderate (Current Default)
```erlang
fitness_discount_rate() -> 0.01.        % 1% per cycle
fitness_realized_bonus() -> 1.25.       % 25% bonus
fitness_loss_penalty() -> 1.5.          % 50% penalty
```

### Aggressive (Large Discount)
```erlang
fitness_discount_rate() -> 0.02.        % 2% per cycle
fitness_realized_bonus() -> 1.50.       % 50% bonus
fitness_loss_penalty() -> 2.0.          % 100% penalty
```

---

## Comparison with Current System

| Aspect | Current System | New System |
|--------|---------------|------------|
| **Fitness Calculation** | Once at end: `Balance + Unrealized_PL` | Once at end: Time-weighted realized PL |
| **During Cycles** | Returns `0` | Returns `0` (same) |
| **Tracking** | None | Tracks cycle and realized PL history |
| **Complexity** | Simple | Slightly more complex |
| **Selection Pressure** | Based on final balance | Based on timing of profits |

---

## Notes

- The time-weighted fitness **replaces** raw fitness in the system
- If you need raw fitness for logging/analysis, you'd need to track it separately
- The discount rate of 0.01 (1%) is more aggressive than a true 30% annual rate (~0.0000005714 per minute) to make effects noticeable in short evaluations
- Adjust parameters in `config.erl` to tune the behavior
- Monitor evolution to ensure desired selection pressure
- Unrealized PL is intentionally excluded - only realized profits/losses are time-weighted

---

---

## Phase 11: Create Revert Documentation

### 11.1 Create Revert Guide

After implementation, create a comprehensive revert document that allows complete removal of all changes and restoration to the original system.

**File**: `.ReadMe_Files/REVERT_TIME_WEIGHTED_FITNESS.md`

This document should:
1. List all files modified
2. Provide exact code to revert each change
3. Include step-by-step instructions
4. Verify system returns to original state
5. Test that revert works correctly

**See**: `REVERT_TIME_WEIGHTED_FITNESS.md` for complete revert instructions.

---

**Version**: 3.1 (End-of-Evaluation with Backward Compatibility)  
**Created**: 2025  
**Status**: Implementation Plan  
**Approach**: Track during simulation, calculate time-weighted fitness once at end  
**Backward Compatibility**: Zero overhead when disabled via `fitness_time_weighted_enabled() -> false`

