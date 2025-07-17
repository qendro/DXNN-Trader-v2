# Performance Profiling Analysis for benchmarker:start(sliding_window_5)

## Objective
Identify the functions that consume the most actual time and compute power during `benchmarker:start(sliding_window_5)` execution without modifying existing production code.

## Current Status
**CACHE OPTIMIZATION ISSUE:** The cache optimization implemented in fx.erl (Priority 1: Cache Price Data in Memory) is not providing the expected performance improvement. The benchmark is still taking the same time as the old ETS table approach (~1.5 hours), despite the cache being properly populated with 10,761 records.

**NEXT STEP:** Run profiling analysis to identify why the cache optimization isn't improving performance and determine the actual bottlenecks in the system.

**COLLABORATION APPROACH:** User will run the profiling steps and share results for analysis. This ensures continuity of analysis even if there are system crashes or interruptions during the profiling process.

## 1. Performance Measurement Approach

### 1.1 Profiling Strategy Options

#### Option A: Erlang Built-in Profiler (eprof)
- **Method:** Use Erlang's built-in `eprof` module
- **Advantages:** 
  - No code changes required
  - Built into Erlang/OTP
  - Function-level timing data
  - Call count statistics
- **Disadvantages:**
  - Performance overhead during profiling
  - Limited to function-level granularity

#### Option B: Erlang Trace-based Profiling (fprof)
- **Method:** Use Erlang's `fprof` module for detailed call tracing
- **Advantages:**
  - Very detailed call tree analysis
  - Precise timing measurements
  - No code modifications needed
- **Disadvantages:**
  - High overhead (can slow execution 10-100x)
  - Large trace files
  - Complex output analysis

#### Option C: Custom Timing Instrumentation
- **Method:** Add temporary timing code to key functions
- **Advantages:**
  - Low overhead
  - Targeted measurement
  - Real-time feedback
- **Disadvantages:**
  - Requires temporary code changes
  - Manual instrumentation needed

#### Option D: Process Monitoring Approach
- **Method:** Monitor system processes and memory usage
- **Advantages:**
  - System-level view
  - No code changes
  - Resource utilization tracking
- **Disadvantages:**
  - Less precise function-level data
  - May miss short-duration bottlenecks

### 1.2 Recommended Hybrid Approach

**Primary Method:** eprof (Option A) for initial broad analysis
**Secondary Method:** Custom timing (Option C) for deep-dive on identified bottlenecks
**Supporting Method:** Process monitoring (Option D) for system resource analysis

## 2. Code Change Assessment

### 2.1 eprof Implementation (Primary Method)
**Code Changes Required:** NONE
**Implementation Complexity:** MINIMAL
**Steps:**
1. Start profiler before benchmark
2. Run benchmark
3. Stop profiler and analyze results
4. No permanent code changes

### 2.2 Custom Timing Implementation (Secondary Method)
**Code Changes Required:** TEMPORARY instrumentation
**Implementation Complexity:** LOW-MEDIUM
**Estimated Lines of Code:** 20-50 lines (temporary)
**Files to Modify Temporarily:**
- `fx.erl` - Add timing to lookup functions
- `benchmarker.erl` - Add timing to benchmark phases
- `sensor.erl` - Add timing to sensor functions
- `neuron.erl` - Add timing to neural network operations (if needed)

**Code Change Pattern:**
```erlang
%% Temporary timing wrapper
time_function(FunctionName, Fun) ->
    Start = erlang:system_time(microsecond),
    Result = Fun(),
    End = erlang:system_time(microsecond),
    Duration = End - Start,
    ets:insert(performance_stats, {FunctionName, Duration}),
    Result.
```

### 2.3 Process Monitoring Implementation
**Code Changes Required:** NONE
**Implementation Complexity:** MINIMAL
**Tools:** Built-in Erlang observer and system monitoring

## 3. Re-evaluation of Approaches

### 3.1 Refined Strategy Assessment

After reviewing the options, the optimal approach is:

**Phase 1: Non-invasive Profiling (eprof)**
- Pros: Zero code changes, immediate results
- Cons: May have profiling overhead
- Best for: Initial bottleneck identification

**Phase 2: Targeted Deep-dive (Custom Timing)**
- Pros: Low overhead, precise measurements
- Cons: Requires temporary code changes
- Best for: Detailed analysis of identified bottlenecks

**Phase 3: System Resource Analysis**
- Pros: Holistic view of resource usage
- Cons: Less function-specific data
- Best for: Understanding overall system behavior

### 3.2 Alternative Lightweight Approach

**Minimal Instrumentation Strategy:**
Instead of full custom timing, use Erlang's built-in `timer:tc/1` for specific function calls:
- Lower overhead than full tracing
- More targeted than eprof
- Minimal code changes required

## 4. Detailed Implementation Steps

### Phase 1: eprof Profiling (No Code Changes)

#### Step 1.1: Setup Profiling Environment
```erlang
%% Start Erlang shell and prepare system
make:all().
mnesia:create_schema([node()]).
mnesia:start().
fx:init().
fx:start().
polis:create().
polis:start().
polis:sync().
```

#### Step 1.2: Start eprof Profiling
```erlang
%% Start profiler targeting all processes
eprof:start().
eprof:start_profiling([self()]).
```

#### Step 1.3: Run Benchmark Under Profiling
```erlang
%% Execute the benchmark
benchmarker:start(sliding_window_5).
```

#### Step 1.4: Stop Profiling and Analyze
```erlang
%% Stop profiler and generate report
eprof:stop_profiling().
eprof:analyze().
eprof:log("benchmark_profile.txt").
eprof:stop().
```

#### Step 1.5: Share Results for Analysis
- Copy the contents of `benchmark_profile.txt` 
- Share the profiling output with analysis team
- Include any console output during profiling
- Note any errors or unexpected behavior during profiling

**RESULT SHARING FORMAT:**
```
=== EPROF PROFILING RESULTS ===
[Paste benchmark_profile.txt contents here]

=== CONSOLE OUTPUT ===
[Paste any relevant console output here]

=== OBSERVATIONS ===
- Benchmark completion time: [X minutes/hours]
- Any crashes or errors: [Yes/No - details]
- System behavior notes: [Any observations]
```

### Phase 2: Targeted Custom Timing (Temporary Code Changes)

#### Step 2.1: Create Temporary Performance Module
**File:** `performance_temp.erl` (temporary file)
```erlang
-module(performance_temp).
-export([time_function/2, init_stats/0, report_stats/0, cleanup/0]).

init_stats() ->
    ets:new(performance_stats, [named_table, public, bag]).

time_function(FunctionName, Fun) ->
    Start = erlang:system_time(microsecond),
    Result = Fun(),
    End = erlang:system_time(microsecond),
    Duration = End - Start,
    ets:insert(performance_stats, {FunctionName, Duration}),
    Result.

report_stats() ->
    Stats = ets:tab2list(performance_stats),
    Grouped = group_stats(Stats),
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, Grouped),
    [io:format("~p: ~p microseconds (~p calls)~n", [Name, Total, Count]) 
     || {Name, Total, Count} <- Sorted].

group_stats(Stats) ->
    Dict = lists:foldl(fun({Name, Duration}, Acc) ->
        case dict:find(Name, Acc) of
            {ok, {Total, Count}} -> dict:store(Name, {Total + Duration, Count + 1}, Acc);
            error -> dict:store(Name, {Duration, 1}, Acc)
        end
    end, dict:new(), Stats),
    [{Name, Total, Count} || {Name, {Total, Count}} <- dict:to_list(Dict)].

cleanup() ->
    ets:delete(performance_stats).
```

#### Step 2.2: Identify Target Functions for Instrumentation
Based on eprof results, likely candidates:
- `fx:lookup/2` - Cache lookup function
- `fx:next/2` - Cache navigation function  
- `fx_GetPriceList/4` - Sequential data access
- `sense/2` - Sensor data processing
- `list_encoded/2` - List sensor encoding
- `plane_encoded/3` - Plane sensor encoding
- Neural network forward pass functions

#### Step 2.3: Add Temporary Instrumentation
**Example for fx.erl (temporary changes):**
```erlang
%% Add to beginning of critical functions
lookup(TableName, Key) ->
    performance_temp:time_function(fx_lookup, fun() ->
        %% Original lookup code here
        original_lookup_implementation(TableName, Key)
    end).
```

#### Step 2.4: Run Instrumented Benchmark
```erlang
%% Initialize performance tracking
performance_temp:init_stats().

%% Run benchmark
benchmarker:start(sliding_window_5).

%% Generate performance report
performance_temp:report_stats().

%% Cleanup
performance_temp:cleanup().
```

#### Step 2.5: Remove Temporary Code
- Delete `performance_temp.erl`
- Remove all temporary instrumentation from production files
- Restore original function implementations

### Phase 3: System Resource Analysis

#### Step 3.1: Monitor System Resources
```erlang
%% Start system monitoring
observer:start().

%% Or use command line monitoring
%% In separate terminal: top -p $(pgrep beam.smp)
```

#### Step 3.2: Memory Usage Analysis
```erlang
%% Check memory usage before benchmark
erlang:memory().

%% Run benchmark
benchmarker:start(sliding_window_5).

%% Check memory usage after
erlang:memory().
```

#### Step 3.3: Process Analysis
```erlang
%% List all processes and their memory usage
[{Pid, process_info(Pid, [memory, message_queue_len, reductions])} 
 || Pid <- processes()].
```

## 5. Expected Bottleneck Candidates

### 5.1 High-Probability Bottlenecks
1. **fx:lookup/2** - Cache lookup operations (millions of calls)
2. **fx_GetPriceList/4** - Sequential data retrieval loops
3. **sense/2** - Sensor data processing for neural networks
4. **Neural network forward pass** - Mathematical computations
5. **list_encoded/2** - Price list encoding for sensors

### 5.2 Medium-Probability Bottlenecks
1. **fx:next/2** - Cache navigation operations
2. **plane_encoded/3** - 2D plane encoding for sensors
3. **update_account/2** - Account state updates
4. **make_trade/3** - Trading simulation logic

### 5.3 Low-Probability Bottlenecks
1. **init_state/5** - State initialization (called infrequently)
2. **File I/O operations** - Should be minimal with cache
3. **ETS operations** - Should be minimal with cache optimization

## 6. Success Criteria

### 6.1 Phase 1 Success (eprof)
- [ ] Identify top 10 most time-consuming functions
- [ ] Determine total time spent in each major subsystem (fx, sensors, neural networks)
- [ ] Identify functions with unexpectedly high call counts
- [ ] Generate baseline performance profile

### 6.2 Phase 2 Success (Custom Timing)
- [ ] Precise timing data for identified bottleneck functions
- [ ] Comparison of cache vs non-cache performance
- [ ] Identification of specific optimization opportunities
- [ ] Quantified performance impact of each bottleneck

### 6.3 Phase 3 Success (System Analysis)
- [ ] Memory usage patterns during benchmark execution
- [ ] CPU utilization distribution across processes
- [ ] Identification of memory leaks or excessive allocations
- [ ] System resource constraints analysis

## 7. Deliverables

### 7.1 Performance Report
- Function-level timing analysis
- System resource utilization report
- Bottleneck identification and ranking
- Performance optimization recommendations

### 7.2 Benchmark Baseline
- Current performance metrics with cache optimization
- Comparison with previous 1.5-hour baseline
- Performance improvement quantification

### 7.3 Optimization Roadmap
- Prioritized list of optimization opportunities
- Estimated performance impact of each optimization
- Implementation complexity assessment

## 8. Implementation Timeline

### Phase 1: eprof Analysis (30 minutes)
- Setup and run profiling: 10 minutes
- Analyze results: 20 minutes

### Phase 2: Custom Timing (2-3 hours)
- Create temporary instrumentation: 1 hour
- Run instrumented benchmark: 30-90 minutes (depending on performance)
- Analyze results and cleanup: 30 minutes

### Phase 3: System Analysis (30 minutes)
- Monitor system resources: 15 minutes
- Analyze resource usage: 15 minutes

**Total Estimated Time:** 3-4 hours for complete performance analysis

## 9. Risk Mitigation

### 9.1 Profiling Overhead Risk
- **Risk:** eprof may significantly slow down execution
- **Mitigation:** Use shorter benchmark runs for profiling if needed
- **Fallback:** Use fprof with sampling if eprof overhead is too high

### 9.2 Temporary Code Risk
- **Risk:** Temporary instrumentation code left in production
- **Mitigation:** Use git branches for temporary changes
- **Checklist:** Verify all temporary code removed before commit

### 9.3 Analysis Complexity Risk
- **Risk:** Profiling data too complex to analyze effectively
- **Mitigation:** Focus on top 10 functions initially
- **Tools:** Use automated analysis scripts for large datasets

## 10. Detailed Analysis Results from benchmark_profile.txt

### Primary Bottleneck Identified: exoself:loop/2
- **12.32%** of total execution time
- **11 calls** at **9.36 microseconds per call**
- This is the main coordination loop for neural network processes

### Critical Performance Bottlenecks Within exoself:loop/2:

#### 1. ETS Table Access - The Primary Culprit
- `ets:lookup_element/3`: **10.05%** of total time (125 calls at 0.67μs each)
- `ets:lookup/2`: **5.86%** of total time (33 calls at 1.48μs each)
- **Combined ETS access: ~16% of total execution time**

#### 2. Database Operations
- `genotype:dirty_read/1`: **2.15%** (25 calls at 0.72μs each)
- `mnesia:do_dirty_rpc/5`: **2.03%** (25 calls at 0.68μs each)

#### 3. List Processing Overhead
- Multiple list comprehensions within loop consuming 4-5% combined
- Processing neural network component lists repeatedly

### Root Cause Analysis
The exoself loop is **parameter-lookup bound, not computation-bound**. It's spending ~20% of total time just fetching neural network parameters and configurations rather than performing actual neural computations.

## 11. Optimization Strategy

### 1. Parameter Caching and Batching
- **Batch ETS lookups**: Instead of 125+ individual lookups, batch related parameters
- **Cache frequently accessed parameters**: Keep hot parameters in process memory
- **Pre-fetch parameter sets**: Load entire neural network parameter blocks at once

### 2. Data Structure Optimization
- **Flatten ETS table structure**: Reduce lookup depth and frequency
- **Denormalize critical parameters**: Store computed values rather than deriving them
- **Use process dictionaries**: Cache parameters locally within exoself process

### 3. Loop Restructuring
- **Minimize database calls**: Batch genotype reads and cache results
- **Reduce list comprehension overhead**: Pre-compute component lists
- **Implement lazy loading**: Only fetch parameters when neural components are active

### 4. Memory vs. Speed Trade-offs
- **Accept higher memory usage**: Keep parameter copies in multiple locations
- **Implement parameter versioning**: Track changes to avoid unnecessary re-reads
- **Use ETS read_concurrency**: Optimize for the read-heavy access pattern

### Expected Impact
- **Target: 50-70% reduction in exoself:loop/2 time**
- **Primary gain from ETS optimization**: Reduce 16% overhead to 3-5%
- **Secondary gain from caching**: Eliminate redundant database calls
- **Overall system speedup**: 15-20% improvement in total execution time

The key insight is that this system is infrastructure-bound rather than algorithm-bound, making it highly optimizable through better data access patterns.

## 12. Next Steps After Analysis

1. **Focus on ETS Optimization:** Primary target is the 16% ETS lookup overhead
2. **Implement Parameter Caching:** Cache neural network parameters in exoself process
3. **Batch Database Operations:** Reduce Mnesia calls through batching
4. **Measure Incremental Improvements:** Test each optimization separately
5. **Verify Overall Performance Gain:** Confirm 15-20% system speedup target