# Multiple Fitness Functions Implementation Guide

## Overview

This implementation adds support for multiple fitness calculation functions in the HyperNEAT trading system. The system can now use different fitness metrics to evaluate trading agents, selected via configuration.

## Architecture

### Components

1. **`fitness_functions.erl`** - New module containing all fitness calculation functions
2. **`config.erl`** - Added `fitness_function/0` parameter to select which function to use
3. **`fx.erl`** - Modified to use the config-selected fitness function dynamically

### Design Principles

- **Centralized**: All fitness functions in one module for easy maintenance
- **Configurable**: Select function via `config:fitness_function()` 
- **Extensible**: Easy to add new fitness functions by following the pattern
- **Backward Compatible**: Default behavior remains the same (time_weighted)

## Available Fitness Functions

### 1. `time_weighted` (Default)
**Purpose**: Time-weighted fitness with discounts for late profits and bonuses/penalties.

**Features**:
- Rewards early realized profits (time discount)
- Applies bonus multipliers to realized profits
- Applies penalty multipliers to realized losses
- Includes unrealized P/L with penalty
- Trades bonus to encourage activity

**Use Case**: Standard trading evaluation with emphasis on realizing profits early.

**Configuration**: Uses existing time-weighted fitness config parameters.

### 2. `total_profit`
**Purpose**: Simple total profit calculation.

**Formula**: `Balance + Unrealized_P/L`

**Features**:
- No time-weighting
- No bonuses/penalties
- Direct profit measurement

**Use Case**: Simple profit maximization without complexity.

### 3. `sharpe_ratio`
**Purpose**: Risk-adjusted return using Sharpe Ratio.

**Formula**: `(Mean Return - Risk Free Rate) / StdDev(Returns)`

**Features**:
- Measures return per unit of risk
- Penalizes high volatility
- Higher values = better risk-adjusted returns

**Use Case**: When you want agents that achieve consistent, stable returns rather than high-volatility profits.

### 4. `profit_factor`
**Purpose**: Measures profit efficiency.

**Formula**: `Gross Profit / Gross Loss`

**Features**:
- Values > 1.0 indicate profitable trading
- Measures efficiency of winning vs losing trades
- Encourages consistent profitability

**Use Case**: When you want agents that win more than they lose on average.

### 5. `total_return`
**Purpose**: Percentage return from initial balance.

**Formula**: `((Current Balance - Initial Balance) / Initial Balance) * 100`

**Features**:
- Measures percentage gain/loss
- Scales by return percentage
- Encourages higher percentage returns

**Use Case**: When you want to compare agents on percentage basis rather than absolute profit.

### 6. `sortino_ratio`
**Purpose**: Downside deviation-adjusted return (like Sharpe but only penalizes losses).

**Formula**: `(Mean Return - Risk Free Rate) / DownsideStdDev`

**Features**:
- Only penalizes downside volatility (losses)
- Doesn't penalize upside volatility (gains)
- More appropriate for trading than Sharpe

**Use Case**: When you want to reward high-volatility profits but penalize losses.

### 7. `calmar_ratio`
**Purpose**: Return relative to maximum drawdown.

**Formula**: `(Total Return / Starting Balance) / Maximum Drawdown`

**Features**:
- Measures return per unit of maximum drawdown
- Encourages strategies with controlled drawdowns
- Higher values = better risk control

**Use Case**: When you want agents that maintain stable equity curves with minimal drawdowns.

## Usage

### Setting the Fitness Function

#### Method 1: Configuration File (config.erl)

```erlang
fitness_function() -> get_val(fitness_function, time_weighted).
```

Change the default to any available function:
- `time_weighted`
- `total_profit`
- `sharpe_ratio`
- `profit_factor`
- `total_return`
- `sortino_ratio`
- `calmar_ratio`

#### Method 2: Runtime Configuration (Dynamic)

```erlang
% Set fitness function at runtime
config:set(fitness_function, sharpe_ratio).

% Verify it's set
config:fitness_function().  % Returns: sharpe_ratio
```

#### Method 3: Configuration Override List

```erlang
% Load multiple config values at once
config:load_from_list([
    {fitness_function, profit_factor},
    {account_leverage, 100},
    {generation_limit, 50}
]).
```

### Example: Running Experiments with Different Fitness Functions

```erlang
% Start with Sharpe ratio fitness
config:set(fitness_function, sharpe_ratio).
benchmarker:start(chart_plane_10x20).

% Later, switch to profit factor
config:set(fitness_function, profit_factor).
benchmarker:start(chart_plane_10x20).

% Compare results
genotype_utils:print_top_agents(10).
```

### Example: Per-Experiment Configuration

```erlang
% Experiment 1: Risk-adjusted returns
config:set(fitness_function, sharpe_ratio).
benchmarker:start(sliding_window_10).

% Experiment 2: Simple profit maximization
config:set(fitness_function, total_profit).
benchmarker:start(sliding_window_10).

% Experiment 3: Drawdown control
config:set(fitness_function, calmar_ratio).
benchmarker:start(sliding_window_10).
```

## Implementation Details

### Adding a New Fitness Function

To add a new fitness function:

1. **Add function to `fitness_functions.erl`**:

```erlang
my_custom_fitness(State, Account) ->
    Starting_Balance = config:account_initial_balance(),
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    
    % Your custom calculation here
    CustomFitness = calculate_my_metric(State, Account),
    
    CustomFitness.
```

2. **Add to dispatch function**:

```erlang
calculate_fitness(State, Account) ->
    FunctionName = config:fitness_function(),
    case FunctionName of
        time_weighted -> time_weighted(State, Account);
        % ... existing functions ...
        my_custom_fitness -> my_custom_fitness(State, Account);  % Add here
        _ -> 
            io:format("Warning: Unknown fitness function ~p, using time_weighted~n", [FunctionName]),
            time_weighted(State, Account)
    end.
```

3. **Update config documentation** (optional):

Update the comment in `config.erl` to list the new function.

### Data Available to Fitness Functions

All fitness functions receive:
- **State** (`#state{}` record):
  - `realized_pl_by_cycle`: List of `{Cycle, PL}` tuples
  - `cycle`: Current cycle number
  - Other state information

- **Account** (`#account{}` record):
  - `balance`: Current account balance
  - `realized_PL`: Total realized profit/loss
  - `unrealized_PL`: Current unrealized profit/loss
  - `net_asset_value`: Net asset value
  - Other account information

### Logging

The system logs which fitness function was used:

```
FITNESS_EVAL | fitness_function=sharpe_ratio | fitness=1234.56 | ...
```

This allows you to verify which function was active during evaluation.

## Testing

### Quick Test

```erlang
% Compile the new module
make:all([load]).

% Test different functions
config:set(fitness_function, total_profit).
config:fitness_function().  % Should return: total_profit

config:set(fitness_function, sharpe_ratio).
config:fitness_function().  % Should return: sharpe_ratio

% Run a small experiment
benchmarker:start(sliding_window_5).
```

### Verification

1. Check logs for fitness function name in evaluation logs
2. Compare fitness values between different functions (they use different scales)
3. Verify evolution behavior differs based on selected function

## Migration Notes

### From Previous Version

- **Old**: `calculate_time_weighted_fitness/2` function in `fx.erl`
- **New**: `fitness_functions:calculate_fitness/2` dispatches to selected function

The old function has been removed from `fx.erl`. If you have code that directly calls it, update to use the new system:

```erlang
% Old
Fitness = calculate_time_weighted_fitness(State, Account).

% New
Fitness = fitness_functions:calculate_fitness(State, Account).
```

### Backward Compatibility

- Default behavior is unchanged: `time_weighted` is the default
- Existing config parameters still work for `time_weighted` function
- All existing experiments will continue to work with default settings

## Best Practices

1. **Choose Appropriate Function**: 
   - Risk-averse: `sharpe_ratio`, `sortino_ratio`, `calmar_ratio`
   - Profit-focused: `total_profit`, `profit_factor`
   - Balanced: `time_weighted`

2. **Document Your Choice**: When running experiments, document which fitness function you used

3. **Compare Results Carefully**: Different functions use different scales, so compare agents evaluated with the same function

4. **Experiment**: Try different functions to see which produces the best trading strategies for your goals

## Troubleshooting

### "Unknown fitness function" Warning

If you see this warning, check:
1. Function name spelling (must match exactly)
2. Function is added to dispatch in `calculate_fitness/2`
3. Function is exported from `fitness_functions` module

### Unexpected Fitness Values

Different functions produce different scales:
- `time_weighted`: Typically 100-10000 range
- `total_profit`: Typically close to account balance
- `sharpe_ratio`: Can be negative or very large
- `profit_factor`: Typically 0.5-3.0 range, then scaled

Compare values only within the same function type.

### Function Not Changing

Ensure you call `config:set(fitness_function, ...)` **before** starting the experiment. The function is selected when evaluation begins.

## Future Enhancements

Potential additions:
- Composite fitness functions (weighted combination of multiple metrics)
- Fitness function parameters (e.g., risk-free rate for Sharpe ratio)
- Per-specie fitness functions (different functions for different species)
- Adaptive fitness functions (change during evolution)

## References

- Original implementation: `fx.erl:calculate_time_weighted_fitness/2`
- Config system: `config.erl`
- Fitness postprocessor: `fitness_postprocessor.erl` (note: this is different - it post-processes fitness for selection, not calculates it)





