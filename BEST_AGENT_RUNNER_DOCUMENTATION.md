# Best Agent Runner Documentation

## Overview

The Best Agent Runner is a feature that enables running the best-performing trading agent on arbitrary new forex datasets after training completion. This provides a streamlined interface to test trained agents on different market conditions and time periods without manually managing the agent lifecycle.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Installation and Setup](#installation-and-setup)
3. [Basic Usage](#basic-usage)
4. [Advanced Usage](#advanced-usage)
5. [Data File Formats](#data-file-formats)
6. [Configuration Options](#configuration-options)
7. [Result Interpretation](#result-interpretation)
8. [Common Use Cases](#common-use-cases)
9. [Troubleshooting](#troubleshooting)
10. [API Reference](#api-reference)
11. [Performance Considerations](#performance-considerations)
12. [Integration with Existing Workflows](#integration-with-existing-workflows)

## Quick Start

### Prerequisites

- DXNN system with completed training (agents in database)
- Forex data file in CSV format
- Erlang/OTP environment
- Mnesia database running

### Basic Example

```erlang
% Start the system
1> mnesia:start().
ok

% Run best agent on a data file
2> best_agent_runner:run_best_agent_on_data("my_forex_data.csv").
{ok, #agent_run_results{
    agent_id = {agent, 123},
    data_source = "my_forex_data.csv",
    total_decisions = 45,
    buy_decisions = 15,
    sell_decisions = 12,
    hold_decisions = 18,
    total_profit_loss = 2.34,
    max_profit = 1.25,
    max_loss = -0.89,
    ...
}}
```

## Installation and Setup

### System Requirements

- Erlang/OTP 21 or higher
- Mnesia database with trained agents
- Sufficient memory for data processing (varies by file size)
- Docker environment (if using containerized setup)

### Setup Steps

1. **Ensure DXNN is properly installed and configured**
   ```bash
   # Verify Erlang installation
   erl -version
   
   # Check if required modules are available
   ls *.beam | grep -E "(best_agent_runner|genotype_utils|fx)"
   ```

2. **Verify database contains trained agents**
   ```erlang
   % Check if agents exist
   genotype_utils:print_best_genotype(all).
   ```

3. **Prepare your data files**
   - Ensure CSV files follow the required format
   - Place files in accessible directory
   - Verify file permissions

## Basic Usage

### Simple Agent Execution

The most basic usage involves running the best agent on a single data file:

```erlang
% Basic execution
Result = best_agent_runner:run_best_agent_on_data("EURUSD_2023_data.csv").
```

### With Basic Options

```erlang
% Run with basic options
Options = #run_options{
    verbose = true,
    output_file = "results.txt"
},
Result = best_agent_runner:run_best_agent_on_data("EURUSD_2023_data.csv", Options).
```

### Checking Results

```erlang
% Extract key information from results
case Result of
    {ok, Results} ->
        io:format("Agent: ~p~n", [Results#agent_run_results.agent_id]),
        io:format("Total Decisions: ~p~n", [Results#agent_run_results.total_decisions]),
        io:format("Profit/Loss: ~.2f%~n", [Results#agent_run_results.total_profit_loss]);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.
```

## Advanced Usage

### Custom Data Range Processing

```erlang
% Process specific range of data
Options = #run_options{
    start_index = 100,      % Start from 100th row
    end_index = 500,        % End at 500th row
    verbose = true,
    collect_all_decisions = true
},
Result = best_agent_runner:run_best_agent_on_data("large_dataset.csv", Options).
```

### Batch Processing Multiple Files

```erlang
% Process multiple files
DataFiles = ["EURUSD_Jan.csv", "EURUSD_Feb.csv", "EURUSD_Mar.csv"],
Results = [best_agent_runner:run_best_agent_on_data(File) || File <- DataFiles].

% Aggregate results
TotalProfit = lists:sum([
    case R of
        {ok, Res} -> Res#agent_run_results.total_profit_loss;
        _ -> 0
    end || R <- Results
]).
```

### Performance Monitoring

```erlang
% Monitor execution time and memory
{Time, Result} = timer:tc(best_agent_runner, run_best_agent_on_data, ["large_file.csv"]),
io:format("Execution time: ~.2f seconds~n", [Time/1000000]).

% Memory monitoring
{MemBefore, _} = erlang:process_info(self(), memory),
Result = best_agent_runner:run_best_agent_on_data("data.csv"),
{MemAfter, _} = erlang:process_info(self(), memory),
io:format("Memory used: ~p bytes~n", [MemAfter - MemBefore]).
```

## Data File Formats

### Standard CSV Format

The system expects CSV files with the following structure:

```csv
Timestamp,Open,High,Low,Close,Volume
2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000
2023-01-01 00:01:00,1.0510,1.0525,1.0505,1.0515,1100
2023-01-01 00:02:00,1.0515,1.0530,1.0510,1.0520,1200
```

### Field Descriptions

- **Timestamp**: Date and time in YYYY-MM-DD HH:MM:SS format
- **Open**: Opening price for the time period
- **High**: Highest price during the time period
- **Low**: Lowest price during the time period
- **Close**: Closing price for the time period
- **Volume**: Trading volume (optional, can be 0)

### Supported Currency Pairs

The system supports various currency pairs:

- **Major Pairs**: EURUSD, GBPUSD, USDJPY, USDCHF
- **Minor Pairs**: EURGBP, EURJPY, GBPJPY
- **Exotic Pairs**: Any pair following the standard format

### Data Quality Requirements

- **Chronological Order**: Data must be sorted by timestamp
- **No Missing Timestamps**: Gaps in data should be minimal
- **Realistic Values**: Prices should be within reasonable ranges
- **Consistent Intervals**: Regular time intervals preferred (1min, 5min, 1hour, etc.)

## Configuration Options

### Run Options Record

```erlang
-record(run_options, {
    start_index = first,     % Where to start in the data
    end_index = last,        % Where to end in the data
    output_file = undefined, % Optional file to save results
    verbose = false,         % Detailed logging
    collect_all_decisions = true % Store all individual decisions
}).
```

### Option Details

#### start_index and end_index
- **first/last**: Use entire dataset
- **Integer**: Specific row numbers (1-based indexing)
- **Range**: Process subset of data for testing

```erlang
% Process middle section of large file
Options = #run_options{
    start_index = 1000,
    end_index = 2000
}.
```

#### output_file
- **undefined**: No file output (default)
- **String**: Path to save detailed results

```erlang
% Save results to file
Options = #run_options{
    output_file = "trading_results_2023.txt"
}.
```

#### verbose
- **false**: Minimal console output (default)
- **true**: Detailed execution information

#### collect_all_decisions
- **true**: Store all individual trading decisions (default)
- **false**: Store only summary statistics (memory efficient)

## Result Interpretation

### Result Structure

```erlang
-record(agent_run_results, {
    agent_id,              % ID of the agent that was executed
    data_source,           % Path to the data file used
    start_time,            % Execution start timestamp
    end_time,              % Execution end timestamp
    total_decisions,       % Total number of trading decisions
    buy_decisions,         % Number of buy signals
    sell_decisions,        % Number of sell signals
    hold_decisions,        % Number of hold/no-action signals
    total_profit_loss,     % Overall profit/loss percentage
    max_profit,            % Maximum single trade profit
    max_loss,              % Maximum single trade loss
    trading_decisions,     % List of individual decisions
    execution_stats        % Performance metrics
}).
```

### Key Metrics

#### Profit/Loss Analysis
- **total_profit_loss**: Overall performance as percentage
- **max_profit**: Best single trade performance
- **max_loss**: Worst single trade performance

#### Decision Distribution
- **buy_decisions**: Bullish market signals
- **sell_decisions**: Bearish market signals  
- **hold_decisions**: Neutral/wait signals

#### Performance Indicators
- **Success Rate**: (Profitable decisions / Total decisions) * 100
- **Win/Loss Ratio**: Profitable decisions / Losing decisions
- **Average Profit per Trade**: Total profit / Number of trades

### Example Result Analysis

```erlang
analyze_results({ok, Results}) ->
    TotalDecisions = Results#agent_run_results.total_decisions,
    ProfitableDecisions = count_profitable_decisions(Results#agent_run_results.trading_decisions),
    
    SuccessRate = (ProfitableDecisions / TotalDecisions) * 100,
    
    io:format("Performance Analysis:~n"),
    io:format("  Success Rate: ~.1f%~n", [SuccessRate]),
    io:format("  Total P&L: ~.2f%~n", [Results#agent_run_results.total_profit_loss]),
    io:format("  Decision Distribution: ~p buy, ~p sell, ~p hold~n", [
        Results#agent_run_results.buy_decisions,
        Results#agent_run_results.sell_decisions,
        Results#agent_run_results.hold_decisions
    ]).
```

## Common Use Cases

### 1. Backtesting on Historical Data

Test your trained agent on historical market data to evaluate performance:

```erlang
% Test on different time periods
HistoricalFiles = [
    "EURUSD_2022_Q1.csv",
    "EURUSD_2022_Q2.csv", 
    "EURUSD_2022_Q3.csv",
    "EURUSD_2022_Q4.csv"
],

QuarterlyResults = [
    best_agent_runner:run_best_agent_on_data(File) || File <- HistoricalFiles
],

% Compare quarterly performance
lists:foreach(fun({File, Result}) ->
    case Result of
        {ok, Res} ->
            io:format("~s: ~.2f% profit~n", [File, Res#agent_run_results.total_profit_loss]);
        {error, Reason} ->
            io:format("~s: Error - ~p~n", [File, Reason])
    end
end, lists:zip(HistoricalFiles, QuarterlyResults)).
```

### 2. Cross-Currency Pair Validation

Test agent performance across different currency pairs:

```erlang
% Test on multiple currency pairs
CurrencyPairs = [
    {"EURUSD", "eurusd_data.csv"},
    {"GBPUSD", "gbpusd_data.csv"},
    {"USDJPY", "usdjpy_data.csv"}
],

CrossPairResults = [
    {Pair, best_agent_runner:run_best_agent_on_data(File)} 
    || {Pair, File} <- CurrencyPairs
],

% Analyze cross-pair performance
BestPair = lists:max([
    {case Result of {ok, R} -> R#agent_run_results.total_profit_loss; _ -> -999 end, Pair}
    || {Pair, Result} <- CrossPairResults
]),

io:format("Best performing pair: ~p~n", [BestPair]).
```

### 3. Market Condition Analysis

Test agent behavior in different market conditions:

```erlang
% Test on different market conditions
MarketConditions = [
    {"Bull Market", "bull_market_2021.csv"},
    {"Bear Market", "bear_market_2022.csv"},
    {"Sideways Market", "sideways_2020.csv"},
    {"High Volatility", "volatile_2023.csv"}
],

ConditionResults = [
    {Condition, best_agent_runner:run_best_agent_on_data(File)}
    || {Condition, File} <- MarketConditions
],

% Identify best market conditions for the agent
lists:foreach(fun({Condition, Result}) ->
    case Result of
        {ok, Res} ->
            io:format("~s: ~.2f% (Buy: ~p, Sell: ~p, Hold: ~p)~n", [
                Condition,
                Res#agent_run_results.total_profit_loss,
                Res#agent_run_results.buy_decisions,
                Res#agent_run_results.sell_decisions,
                Res#agent_run_results.hold_decisions
            ]);
        _ ->
            io:format("~s: Failed~n", [Condition])
    end
end, ConditionResults).
```

### 4. Performance Benchmarking

Compare agent performance against benchmarks:

```erlang
% Run agent and calculate benchmark metrics
{ok, Results} = best_agent_runner:run_best_agent_on_data("benchmark_data.csv"),

% Calculate Sharpe ratio (simplified)
TotalReturn = Results#agent_run_results.total_profit_loss,
NumDecisions = Results#agent_run_results.total_decisions,
AvgReturn = TotalReturn / NumDecisions,

% Calculate volatility (simplified)
Decisions = Results#agent_run_results.trading_decisions,
Returns = [D#trading_decision.profit_loss || D <- Decisions, 
           D#trading_decision.profit_loss =/= undefined],
Volatility = calculate_standard_deviation(Returns),

SharpeRatio = case Volatility of
    0 -> 0;
    _ -> AvgReturn / Volatility
end,

io:format("Performance Metrics:~n"),
io:format("  Total Return: ~.2f%~n", [TotalReturn]),
io:format("  Average Return per Trade: ~.4f%~n", [AvgReturn]),
io:format("  Volatility: ~.4f~n", [Volatility]),
io:format("  Sharpe Ratio: ~.2f~n", [SharpeRatio]).
```

### 5. Real-time Data Testing

Test agent on recent/live data:

```erlang
% Process recent data with detailed monitoring
Options = #run_options{
    verbose = true,
    output_file = "live_test_results.txt",
    collect_all_decisions = true
},

{ok, Results} = best_agent_runner:run_best_agent_on_data("recent_data.csv", Options),

% Check for recent performance trends
RecentDecisions = lists:sublist(Results#agent_run_results.trading_decisions, 10),
RecentProfit = lists:sum([
    case D#trading_decision.profit_loss of
        undefined -> 0;
        PL -> PL
    end || D <- RecentDecisions
]),

io:format("Recent performance (last 10 decisions): ~.2f%~n", [RecentProfit]).
```

## Troubleshooting

### Common Issues and Solutions

#### 1. "No agents found in database"

**Problem**: The system cannot find any trained agents.

**Solutions**:
```erlang
% Check if mnesia is running
mnesia:system_info(is_running).

% Check if agents table exists
mnesia:table_info(agent, size).

% List available agents
genotype_utils:print_best_genotype(all).
```

#### 2. "Data file not found or not readable"

**Problem**: Cannot access the specified data file.

**Solutions**:
```erlang
% Check file existence
filelib:is_file("your_data_file.csv").

% Check file permissions
file:read_file_info("your_data_file.csv").

% Use absolute path
best_agent_runner:run_best_agent_on_data("/full/path/to/data.csv").
```

#### 3. "Invalid data format"

**Problem**: Data file format is not recognized.

**Solutions**:
- Verify CSV header format
- Check timestamp format (YYYY-MM-DD HH:MM:SS)
- Ensure numeric values for prices
- Remove any extra columns or formatting

#### 4. "Agent execution timeout"

**Problem**: Agent takes too long to process data.

**Solutions**:
```erlang
% Process smaller data chunks
Options = #run_options{
    start_index = 1,
    end_index = 100  % Process first 100 rows only
},
Result = best_agent_runner:run_best_agent_on_data("large_file.csv", Options).

% Use non-verbose mode for better performance
Options = #run_options{verbose = false}.
```

#### 5. "Memory issues with large files"

**Problem**: System runs out of memory processing large datasets.

**Solutions**:
```erlang
% Disable decision collection for large files
Options = #run_options{
    collect_all_decisions = false
},

% Process in chunks
process_large_file_in_chunks("huge_file.csv", 1000).  % 1000 rows per chunk
```

### Debug Mode

Enable detailed debugging:

```erlang
% Set debug mode
put(debug_mode, true),

% Run with maximum verbosity
Options = #run_options{
    verbose = true,
    output_file = "debug_output.txt"
},

Result = best_agent_runner:run_best_agent_on_data("problem_file.csv", Options).
```

### Log Analysis

Check system logs for detailed error information:

```bash
# Check Erlang crash dumps
ls -la erl_crash.dump*

# Monitor system resources
top -p $(pgrep beam)
```

## API Reference

### Main Functions

#### `run_best_agent_on_data/1`

```erlang
run_best_agent_on_data(DataFilePath) -> {ok, Results} | {error, Reason}.
```

**Parameters**:
- `DataFilePath`: String path to CSV data file

**Returns**:
- `{ok, Results}`: Success with agent_run_results record
- `{error, Reason}`: Error with reason description

#### `run_best_agent_on_data/2`

```erlang
run_best_agent_on_data(DataFilePath, Options) -> {ok, Results} | {error, Reason}.
```

**Parameters**:
- `DataFilePath`: String path to CSV data file
- `Options`: run_options record with configuration

### Helper Functions

#### `find_best_agent/0`

```erlang
find_best_agent() -> {ok, AgentId} | {error, Reason}.
```

Identifies the best performing agent from the database.

#### `format_results/1`

```erlang
format_results(Results) -> FormattedString.
```

Formats results into a human-readable report.

### Record Definitions

```erlang
% Configuration options
-record(run_options, {
    start_index = first,
    end_index = last,
    output_file = undefined,
    verbose = false,
    collect_all_decisions = true
}).

% Execution results
-record(agent_run_results, {
    agent_id,
    data_source,
    start_time,
    end_time,
    total_decisions,
    buy_decisions,
    sell_decisions,
    hold_decisions,
    total_profit_loss,
    max_profit,
    max_loss,
    trading_decisions,
    execution_stats
}).

% Individual trading decision
-record(trading_decision, {
    timestamp,
    index,
    price,
    signal,
    confidence,
    profit_loss
}).
```

## Performance Considerations

### Memory Usage

- **Small files (< 1MB)**: Minimal memory impact
- **Medium files (1-10MB)**: ~10-50MB memory usage
- **Large files (> 10MB)**: Consider chunked processing

### Execution Time

- **Small datasets (< 1000 rows)**: < 10 seconds
- **Medium datasets (1000-10000 rows)**: 10-60 seconds
- **Large datasets (> 10000 rows)**: 1-10 minutes

### Optimization Tips

1. **Use appropriate data ranges**:
   ```erlang
   % Instead of processing entire large file
   Options = #run_options{start_index = 1000, end_index = 2000}.
   ```

2. **Disable verbose mode for production**:
   ```erlang
   Options = #run_options{verbose = false}.
   ```

3. **Consider decision collection needs**:
   ```erlang
   % For summary only
   Options = #run_options{collect_all_decisions = false}.
   ```

4. **Monitor system resources**:
   ```erlang
   % Check memory before processing large files
   {Memory, _} = erlang:process_info(self(), memory),
   io:format("Current memory usage: ~p bytes~n", [Memory]).
   ```

## Integration with Existing Workflows

### With Benchmarker

The Best Agent Runner integrates seamlessly with existing benchmarker workflows:

```erlang
% After training completion
benchmarker:run_training(),

% Automatically extract and test best agent
{ok, BestAgent} = genotype_utils:find_best_agent(),
Result = best_agent_runner:run_best_agent_on_data("validation_data.csv").
```

### With Population Monitor

```erlang
% Monitor training progress
population_monitor:start(),

% When training completes, test on new data
case population_monitor:get_status() of
    training_complete ->
        best_agent_runner:run_best_agent_on_data("test_data.csv");
    _ ->
        io:format("Training still in progress~n")
end.
```

### Docker Integration

The system works within the existing Docker environment:

```bash
# Run within Docker container
docker exec -it dxnn_container erl -eval "
    best_agent_runner:run_best_agent_on_data('/data/forex_data.csv').
"
```

### Automated Testing Pipeline

```erlang
% Automated testing after training
automated_test_pipeline() ->
    % 1. Wait for training completion
    wait_for_training_completion(),
    
    % 2. Run validation tests
    ValidationFiles = get_validation_files(),
    Results = [best_agent_runner:run_best_agent_on_data(F) || F <- ValidationFiles],
    
    % 3. Generate report
    generate_validation_report(Results),
    
    % 4. Archive results
    archive_results(Results).
```

## Advanced Examples

### Custom Result Processing

```erlang
% Custom analysis function
analyze_trading_patterns({ok, Results}) ->
    Decisions = Results#agent_run_results.trading_decisions,
    
    % Group decisions by signal type
    BuyDecisions = [D || D <- Decisions, D#trading_decision.signal =:= 1],
    SellDecisions = [D || D <- Decisions, D#trading_decision.signal =:= -1],
    HoldDecisions = [D || D <- Decisions, D#trading_decision.signal =:= 0],
    
    % Calculate average profit by signal type
    AvgBuyProfit = calculate_average_profit(BuyDecisions),
    AvgSellProfit = calculate_average_profit(SellDecisions),
    
    % Identify best performing signal type
    BestSignal = case {AvgBuyProfit, AvgSellProfit} of
        {Buy, Sell} when Buy > Sell -> {buy, Buy};
        {Buy, Sell} when Sell > Buy -> {sell, Sell};
        _ -> {neutral, 0}
    end,
    
    io:format("Trading Pattern Analysis:~n"),
    io:format("  Best signal type: ~p (~.2f% avg profit)~n", BestSignal),
    io:format("  Buy signals: ~p (avg: ~.2f%)~n", [length(BuyDecisions), AvgBuyProfit]),
    io:format("  Sell signals: ~p (avg: ~.2f%)~n", [length(SellDecisions), AvgSellProfit]).
```

### Comparative Analysis

```erlang
% Compare multiple agents (if available)
compare_agents_on_data(DataFile) ->
    % Get all available agents
    AllAgents = get_all_trained_agents(),
    
    % Test each agent
    Results = [{Agent, test_specific_agent(Agent, DataFile)} || Agent <- AllAgents],
    
    % Rank by performance
    RankedResults = lists:sort(fun({_, {ok, R1}}, {_, {ok, R2}}) ->
        R1#agent_run_results.total_profit_loss > R2#agent_run_results.total_profit_loss
    end, Results),
    
    % Display rankings
    lists:foreach(fun({Agent, {ok, Result}}) ->
        io:format("Agent ~p: ~.2f% profit~n", [
            Agent, 
            Result#agent_run_results.total_profit_loss
        ])
    end, RankedResults).
```

This documentation provides comprehensive guidance for using the Best Agent Runner feature effectively. For additional support or advanced use cases, refer to the source code documentation or contact the development team.