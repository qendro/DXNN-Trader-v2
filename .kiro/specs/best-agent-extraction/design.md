# Design Document

## Overview

This design extends the existing DXXN neuroevolutionary system to provide a streamlined interface for running the best-performing agent on arbitrary forex datasets. The current system already has the core components needed:

- `genotype_utils:print_best_genotype(all)` identifies the best agent
- `exoself:start(Agent_Id, PM_PId, OpMode)` starts agents
- `fx.erl` handles forex data loading and processing
- Agents communicate via message passing for sensing and trading

The new functionality will create a high-level interface that orchestrates these existing components to run the best agent on user-specified data files.

## Architecture

### Core Components

1. **Best Agent Runner Module** (`best_agent_runner.erl`)
   - Main interface for running best agents on new data
   - Orchestrates agent identification, data loading, and execution
   - Handles result collection and reporting

2. **Data Loader Extension** (extend `fx.erl`)
   - Load arbitrary forex data files into ETS tables
   - Validate data format compatibility
   - Clean up temporary tables after execution

3. **Agent Controller** (extend existing `exoself.erl` patterns)
   - Manage agent lifecycle for standalone execution
   - Collect trading decisions and performance metrics
   - Handle clean termination and result extraction

### Data Flow

```
User Request → Best Agent Runner → Agent Identification → Data Loading → Agent Execution → Result Collection → Cleanup
```

## Components and Interfaces

### 1. Best Agent Runner Module

**Primary Interface:**
```erlang
run_best_agent_on_data(DataFilePath) -> {ok, Results} | {error, Reason}.
run_best_agent_on_data(DataFilePath, Options) -> {ok, Results} | {error, Reason}.
```

**Internal Functions:**
```erlang
find_best_agent() -> {ok, Agent_Id} | {error, no_agents}.
load_forex_data(FilePath) -> {ok, TableName} | {error, Reason}.
execute_agent_on_data(Agent_Id, TableName) -> {ok, TradingResults}.
cleanup_temporary_data(TableName) -> ok.
format_results(TradingResults) -> FormattedResults.
```

### 2. Data Loading Extension

**New Functions in fx.erl:**
```erlang
load_data_file(FilePath) -> {ok, TableName} | {error, Reason}.
validate_data_format(FilePath) -> ok | {error, Reason}.
create_temporary_table(TableName) -> {ok, TableName}.
delete_temporary_table(TableName) -> ok.
```

**Data Format Support:**
- CSV files with columns: Timestamp, Open, High, Low, Close, Volume
- Same format as existing EURUSD tables
- Automatic timestamp parsing and indexing

### 3. Agent Execution Controller

**New Functions:**
```erlang
run_agent_standalone(Agent_Id, TableName) -> {ok, Results}.
collect_trading_decisions(Agent_PId) -> [TradingDecision].
monitor_agent_execution(Agent_PId, TableName) -> ExecutionStats.
```

**Trading Decision Record:**
```erlang
-record(trading_decision, {
    timestamp,
    index,
    price,
    signal,      % -1, 0, 1 for sell, hold, buy
    confidence,  % derived from fitness or other metrics
    profit_loss  % calculated profit/loss from decision
}).
```

## Data Models

### Input Data Format
```erlang
% CSV file format expected:
% Timestamp,Open,High,Low,Close,Volume
% 2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000
```

### Results Format
```erlang
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
    trading_decisions,  % List of #trading_decision{}
    execution_stats     % Performance metrics
}).
```

### Configuration Options
```erlang
-record(run_options, {
    start_index = first,     % Where to start in the data
    end_index = last,        % Where to end in the data
    output_file = undefined, % Optional file to save results
    verbose = false,         % Detailed logging
    collect_all_decisions = true % Store all individual decisions
}).
```

## Error Handling

### Data Loading Errors
- **File not found**: Return `{error, {file_not_found, FilePath}}`
- **Invalid format**: Return `{error, {invalid_format, Details}}`
- **Parsing errors**: Return `{error, {parse_error, LineNumber, Details}}`

### Agent Execution Errors
- **No agents available**: Return `{error, no_trained_agents}`
- **Agent startup failure**: Return `{error, {agent_startup_failed, Reason}}`
- **Execution timeout**: Return `{error, execution_timeout}`

### Resource Management
- Always clean up temporary ETS tables
- Terminate agent processes properly
- Handle memory constraints for large datasets

## Testing Strategy

### Unit Tests
1. **Data Loading Tests**
   - Valid CSV file parsing
   - Invalid format handling
   - Large file processing
   - Edge cases (empty files, malformed data)

2. **Agent Execution Tests**
   - Best agent identification
   - Agent startup and termination
   - Message passing correctness
   - Result collection accuracy

3. **Integration Tests**
   - End-to-end workflow with sample data
   - Multiple currency pair formats
   - Different data sizes and time ranges
   - Error recovery scenarios

### Test Data
- Sample EURUSD data files in various formats
- Edge case datasets (single row, missing columns)
- Large datasets for performance testing
- Corrupted files for error handling tests

### Performance Tests
- Memory usage with large datasets
- Execution time benchmarks
- Concurrent agent execution (if needed)
- Resource cleanup verification

## Implementation Approach

### Phase 1: Core Infrastructure
1. Create `best_agent_runner.erl` module skeleton
2. Implement `find_best_agent/0` function using existing `genotype_utils` patterns
3. Add data loading functions to `fx.erl`
4. Create basic result collection structures

### Phase 2: Agent Execution
1. Implement standalone agent execution controller
2. Add message collection and decision tracking
3. Integrate with existing `exoself` and `fx` communication patterns
4. Test with existing trained agents

### Phase 3: Data Processing
1. Implement CSV file parsing and validation
2. Add temporary table management
3. Ensure compatibility with existing forex data formats
4. Add error handling and cleanup

### Phase 4: Results and Reporting
1. Implement result formatting and statistics
2. Add optional file output
3. Create summary reporting functions
4. Add performance metrics collection

### Integration Points

**With Existing Modules:**
- `genotype_utils.erl`: Use existing agent identification patterns
- `exoself.erl`: Leverage existing agent startup and management
- `fx.erl`: Extend data loading and table management
- `benchmarker.erl`: Ensure no conflicts with training workflows

**Backward Compatibility:**
- All existing functions remain unchanged
- New functionality is additive only
- No modifications to core training or evolution logic
- Existing Docker and Mnesia setup requirements unchanged