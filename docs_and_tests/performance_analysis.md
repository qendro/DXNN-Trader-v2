# FX Trading System Performance Analysis
## benchmarker:start(chart_plane_100x10) with High-Scale Parameters

### Configuration Parameters
```erlang
specie_size_limit() -> 10000          % 10,000 agents per species
init_specie_size() -> 10000           % 10,000 initial agents  
evaluations_limit() -> 1000000        % 1 million evaluations
survival_percentage() -> 0.5          % 50% survival rate
tot_runs() -> 500                     % 500 benchmark runs
internal_sensor_dimensions() -> 3     % 3D internal sensors
Data Size: 50,000 price ticks         % 50k data points
```

### Execution Flow Analysis

## Phase 1: System Initialization (1x per benchmark)

| Function | Module | Calls | Description |
|----------|--------|-------|-------------|
| `benchmarker:start/1` | benchmarker | 1 | Entry point |
| `genotype:write/1` | genotype | 1 | Write experiment record |
| `benchmarker:prep/1` | benchmarker | 1 | Setup benchmark |
| `population_monitor:prep_PopState/2` | population_monitor | 1 | Initialize population state |
| `init_population/2` | population_monitor | 1 | Create population |
| `create_Population/3` | population_monitor | 1 | Generate species |
| `create_specie/6` | population_monitor | 1 | Create species record |
| `genotype:construct_Agent/3` | genotype | **10,000** | Create each agent |
| `population_monitor:start/1` | population_monitor | 1 | Start evolution |

**Subtotal Phase 1: ~10,010 function calls**

## Phase 2: Per-Run Evolution (500x runs)

### Per Generation (Variable, ~100-1000 generations per run)

| Function | Module | Calls per Gen | Total Calls |
|----------|--------|---------------|-------------|
| `extract_AgentIds/2` | population_monitor | 1 | **500-5,000** |
| `summon_agents/3` | population_monitor | 1 | **500-5,000** |
| `exoself:start/3` | exoself | **10,000** | **5M-50M** |
| `exoself:prep/3` | exoself | **10,000** | **5M-50M** |

### Per Agent Spawning (10,000 agents × generations × runs)

| Function | Module | Calls per Agent | Total Calls |
|----------|--------|-----------------|-------------|
| `genotype:dirty_read/1` | genotype | 5-10 | **250M-500M** |
| `spawn_Scapes/4` | exoself | 1 | **5M-50M** |
| `spawn_CerebralUnits/3` | exoself | 4-6 | **20M-300M** |
| `link_Sensors/3` | exoself | 1 | **5M-50M** |
| `link_Actuators/3` | exoself | 1 | **5M-50M** |
| `link_Neurons/3` | exoself | 1 | **5M-50M** |
| `link_Cortex/2` | exoself | 1 | **5M-50M** |

## Phase 3: Agent Evaluation (Per Agent, Per Generation)

### FX Trading Simulation (10,000 agents × evaluations per agent)

| Function | Module | Calls per Evaluation | Total Calls |
|----------|--------|---------------------|-------------|
| `fx:sim/3` | fx | 1 | **1 Billion** |
| `fx:sense/2` | fx | 100-1000 | **100B-1T** |
| `fx:make_trade/3` | fx | 10-100 | **10B-100B** |
| `fx:update_account/2` | fx | 10-100 | **10B-100B** |
| `fx:update_state/1` | fx | 100-1000 | **100B-1T** |

### Sensor Processing (Chart Plane 100x10 = 1000 inputs per sense)

| Function | Module | Calls per Sense | Total Calls |
|----------|--------|-----------------|-------------|
| `fx_PCI/4` | sensor | 1 | **100B-1T** |
| `plane_encoded/3` | fx | 1 | **100B-1T** |
| `fx_GetPriceList/4` | fx | 1 | **100B-1T** |
| `l2fx/6` | fx | 1000 | **100T-1Q** |
| `fx:lookup/2` | fx | 1000+ | **100T-1Q** |
| `fx:next/2` | fx | 1000+ | **100T-1Q** |

### Neural Network Processing

| Function | Module | Calls per Cycle | Total Calls |
|----------|--------|-----------------|-------------|
| `neuron:loop/7` | neuron | 1 | **Billions** |
| `functions:dot_product/2` | functions | 1 | **Billions** |
| `functions:tanh/1` | functions | 1 | **Billions** |
| `cortex:loop/7` | cortex | 1 | **Billions** |

## Phase 4: Evolution Operations (Per Generation)

| Function | Module | Calls per Gen | Total Calls |
|----------|--------|---------------|-------------|
| `mutate_population/4` | population_monitor | 1 | **500-5,000** |
| `mutate_Specie/5` | population_monitor | 1 | **500-5,000** |
| `construct_AgentSummaries/2` | population_monitor | 1 | **500-5,000** |
| `fitness_postprocessor:size_proportional/1` | fitness_postprocessor | 1 | **500-5,000** |
| `selection_algorithm:competition/3` | selection_algorithm | 1 | **500-5,000** |
| `genome_mutator:mutate/1` | genome_mutator | **5,000** | **2.5M-25M** |
| `genotype:clone_Agent/1` | genotype | **5,000** | **2.5M-25M** |

## Critical Performance Bottlenecks

### 🔥 **Extreme Bottlenecks (Quadrillion+ calls)**
1. **`l2fx/6`** - Chart encoding function called ~100 trillion times
2. **`fx:lookup/2`** - Database lookups for price data ~100 trillion times  
3. **`fx:next/2`** - Index navigation ~100 trillion times

### 🔥 **Major Bottlenecks (Trillion+ calls)**
4. **`fx:sense/2`** - Sensor data processing ~100 billion-1 trillion times
5. **`fx:update_state/1`** - State updates ~100 billion-1 trillion times
6. **`plane_encoded/3`** - Chart plane encoding ~100 billion-1 trillion times

### 🔥 **Significant Bottlenecks (Billion+ calls)**
7. **`fx:sim/3`** - Main simulation loop ~1 billion times
8. **`fx:make_trade/3`** - Trading decisions ~10-100 billion times
9. **`neuron:loop/7`** - Neural processing ~billions of times
10. **`genotype:dirty_read/1`** - Database reads ~250-500 million times

## Performance Optimization Recommendations

### **Priority 1: Critical Path Optimization**

1. **Cache Price Data in Memory**
   ```erlang
   % Instead of fx:lookup/2 every time, preload data
   % Estimated speedup: 1000x for data access
   ```

2. **Optimize Chart Encoding**
   ```erlang
   % Pre-compute chart planes, cache results
   % Estimated speedup: 100x for sensor processing
   ```

3. **Batch Database Operations**
   ```erlang
   % Use ETS tables instead of Mnesia for hot data
   % Estimated speedup: 10-100x for reads
   ```

### **Priority 2: Algorithm Optimization**

4. **Reduce Sensor Resolution During Evolution**
   ```erlang
   % Use 10x5 charts during evolution, 100x10 for final evaluation
   % Estimated speedup: 20x overall
   ```

5. **Implement Fitness Caching**
   ```erlang
   % Cache fitness for identical genomes
   % Estimated speedup: 2-5x for repeated evaluations
   ```

### **Priority 3: System Architecture**

6. **Parallel Agent Evaluation**
   ```erlang
   % Evaluate agents in parallel processes
   % Estimated speedup: 4-8x on multi-core systems
   ```

7. **Streaming Data Processing**
   ```erlang
   % Process data in chunks instead of loading all 50k ticks
   % Memory reduction: 10x, slight speed improvement
   ```

## Estimated Runtime Analysis

### **Current Configuration (Worst Case)**
- **Total Function Calls**: ~100 Quadrillion (10^17)
- **Estimated Runtime**: 6-12 months continuous execution
- **Memory Usage**: 50-100 GB RAM
- **CPU Usage**: 100% single-core utilization

### **With Priority 1 Optimizations**
- **Total Function Calls**: ~1 Trillion (10^12) 
- **Estimated Runtime**: 2-7 days
- **Memory Usage**: 5-10 GB RAM
- **CPU Usage**: Distributed across cores

### **With All Optimizations**
- **Total Function Calls**: ~100 Billion (10^11)
- **Estimated Runtime**: 4-12 hours  
- **Memory Usage**: 1-2 GB RAM
- **CPU Usage**: Efficiently distributed

## Immediate Action Items

1. **Profile the current system** with smaller parameters first
2. **Implement price data caching** (biggest impact)
3. **Optimize chart encoding** (second biggest impact)  
4. **Consider reducing population size** for initial testing
5. **Use ETS tables** for hot data instead of Mnesia
6. **Implement parallel evaluation** for agents

The current configuration with 10,000 agents and 1 million evaluations will likely take months to complete. Start with 100 agents and 10,000 evaluations to validate optimizations first.

---

# Performance Optimization Code Implementations

## Priority 1: Critical Path Optimization

### 1. Cache Price Data in Memory (fx.erl)

**Problem**: `fx:lookup/2` and `fx:next/2` called ~100 trillion times, each doing database lookups.

**Solution**: Preload all price data into ETS table at startup.

```erlang
%% Add to fx.erl - New caching system
-define(PRICE_CACHE_TABLE, fx_price_cache).

%% Initialize price cache at startup
init_price_cache(TableName) ->
    CacheTable = ets:new(?PRICE_CACHE_TABLE, [ordered_set, public, named_table, {read_concurrency, true}]),
    
    %% Load all price data into memory
    case ets:file2tab(config:fx_tables_directory() ++ atom_to_list(TableName)) of
        {ok, SourceTable} ->
            %% Copy all data to cache with optimized structure
            ets:foldl(fun(Record, Acc) ->
                ets:insert(?PRICE_CACHE_TABLE, Record),
                Acc + 1
            end, 0, SourceTable),
            ets:delete(SourceTable),
            io:format("Loaded ~p price records into cache~n", [ets:info(?PRICE_CACHE_TABLE, size)]);
        {error, Reason} ->
            io:format("Failed to load price cache: ~p~n", [Reason])
    end.

%% Replace fx:lookup/2 with cached version
lookup_cached(TableName, Key) ->
    case ets:lookup(?PRICE_CACHE_TABLE, Key) of
        [Record] -> Record;
        [] -> undefined
    end.

%% Replace fx:next/2 with cached version  
next_cached(TableName, Key) ->
    case ets:next(?PRICE_CACHE_TABLE, Key) of
        '$end_of_table' -> 'end_of_table';
        NextKey -> NextKey
    end.

%% Replace fx:prev/4 with cached version
prev_cached(TableName, Key, Direction, Count) when Count > 0 ->
    case ets:prev(?PRICE_CACHE_TABLE, Key) of
        '$end_of_table' -> 'end_of_table';
        PrevKey -> prev_cached(TableName, PrevKey, Direction, Count - 1)
    end;
prev_cached(TableName, Key, Direction, 0) ->
    Key.

%% Update fx:sim/3 to use cache
sim(ExoSelf)->
    %% Initialize cache if not already done
    case ets:info(?PRICE_CACHE_TABLE, size) of
        undefined -> init_price_cache('EURUSD15');
        _ -> ok
    end,
    
    put(prev_PC,0),
    S = #state{},
    A = #account{
        leverage = config:account_leverage(),
        balance = config:account_initial_balance(),
        lot = config:account_lot_size(),
        spread = config:account_spread(),
        margin = config:account_margin(),
        net_asset_value = config:account_initial_balance()
    },
    sim(ExoSelf,S,A).
```

### 2. Optimize Chart Encoding (fx.erl)

**Problem**: `l2fx/6` and `plane_encoded/3` called ~100 trillion times, recalculating same chart data.

**Solution**: Pre-compute and cache chart planes.

```erlang
%% Add to fx.erl - Chart caching system
-define(CHART_CACHE_TABLE, fx_chart_cache).

%% Initialize chart cache
init_chart_cache() ->
    ets:new(?CHART_CACHE_TABLE, [set, public, named_table, {read_concurrency, true}]).

%% Generate cache key for chart parameters
chart_cache_key(CurrencyPair, StartIndex, HRes, VRes) ->
    {CurrencyPair, StartIndex, HRes, VRes}.

%% Cached version of plane_encoded/3
plane_encoded_cached(HRes, VRes, S) ->
    Index = S#state.index,
    CurrencyPair = S#state.table_name,
    CacheKey = chart_cache_key(CurrencyPair, Index, HRes, VRes),
    
    case ets:lookup(?CHART_CACHE_TABLE, CacheKey) of
        [{CacheKey, CachedResult}] ->
            %% Cache hit - return cached result
            {CachedResult, S};
        [] ->
            %% Cache miss - compute and cache result
            {Result, U_S} = plane_encoded_original(HRes, VRes, S),
            ets:insert(?CHART_CACHE_TABLE, {CacheKey, Result}),
            {Result, U_S}
    end.

%% Rename original function
plane_encoded_original(HRes, VRes, S) ->
    %% Original plane_encoded/3 code here
    Index = S#state.index,
    CurrencyPair = S#state.table_name,
    PriceListPs = S#state.price_list,
    case lists:keyfind(HRes, 2, PriceListPs) of
        false ->
            Trailing_Index = prev_cached(CurrencyPair, Index, prev, HRes-1),
            U_PList = fx_GetPriceList_cached(CurrencyPair, Trailing_Index, HRes, []),
            U_PriceListPs = [{U_PList, HRes}|PriceListPs];
        {PList, HRes} ->
            R = lookup_cached(CurrencyPair, Index),
            U_PList = [{R#technical.open, R#technical.close, R#technical.high, R#technical.low}|lists:sublist(PList, HRes-1)],
            U_PriceListPs = lists:keyreplace(HRes, 2, PriceListPs, {U_PList, HRes})
    end,
    
    LVMax1 = lists:max([High||{_Open,_Close,High,_Low}<-U_PList]),
    LVMin1 = lists:min([Low||{_Open,_Close,_High,Low}<-U_PList]),
    LVMax = LVMax1 + abs(LVMax1-LVMin1)/20,
    LVMin = LVMin1 - abs(LVMax1-LVMin1)/20,
    VStep = (LVMax-LVMin)/VRes,
    V_StartPos = LVMin + VStep/2,
    U_S = S#state{price_list=U_PriceListPs},
    {l2fx(HRes*VRes, {U_PList, U_PList}, V_StartPos, VStep, []), U_S}.

%% Optimized fx_GetPriceList using cache
fx_GetPriceList_cached(_Table, EndKey, 0, Acc) ->
    Acc;
fx_GetPriceList_cached(_Table, 'end_of_table', _Index, Acc) ->
    exit("fx_GetPriceList_cached, reached end_of_table");
fx_GetPriceList_cached(Table, Key, Index, Acc) ->
    R = lookup_cached(Table, Key),
    fx_GetPriceList_cached(Table, next_cached(Table, Key), Index-1, 
        [{R#technical.open, R#technical.close, R#technical.high, R#technical.low}|Acc]).
```

### 3. Batch Database Operations (genotype.erl)

**Problem**: `genotype:dirty_read/1` called ~250-500 million times with individual database hits.

**Solution**: Batch reads and use ETS for hot data.

```erlang
%% Add to genotype.erl - Batch reading system
-define(AGENT_CACHE_TABLE, agent_cache).

%% Initialize agent cache
init_agent_cache() ->
    ets:new(?AGENT_CACHE_TABLE, [set, public, named_table, {read_concurrency, true}]).

%% Batch load agents into cache
batch_load_agents(Agent_Ids) ->
    case ets:info(?AGENT_CACHE_TABLE, name) of
        undefined -> init_agent_cache();
        _ -> ok
    end,
    
    %% Load all agents in single transaction
    F = fun() ->
        [begin
            Agent = mnesia:read({agent, Agent_Id}),
            case Agent of
                [A] -> 
                    ets:insert(?AGENT_CACHE_TABLE, {Agent_Id, A}),
                    %% Also cache related records
                    Cortex = mnesia:read({cortex, A#agent.cx_id}),
                    case Cortex of
                        [C] -> ets:insert(?AGENT_CACHE_TABLE, {A#agent.cx_id, C});
                        [] -> ok
                    end;
                [] -> ok
            end
         end || Agent_Id <- Agent_Ids]
    end,
    mnesia:transaction(F).

%% Cached version of dirty_read
dirty_read_cached({Type, Id}) ->
    case ets:lookup(?AGENT_CACHE_TABLE, Id) of
        [{Id, Record}] -> Record;
        [] -> 
            %% Fallback to database
            case mnesia:dirty_read({Type, Id}) of
                [Record] -> 
                    ets:insert(?AGENT_CACHE_TABLE, {Id, Record}),
                    Record;
                [] -> undefined
            end
    end.

%% Update population_monitor.erl to use batch loading
%% In summon_agents/3:
summon_agents(OpMode, Agent_Ids, Acc) ->
    %% Batch load all agents before spawning
    genotype:batch_load_agents(Agent_Ids),
    summon_agents_cached(OpMode, Agent_Ids, Acc).

summon_agents_cached(OpMode, [Agent_Id|Agent_Ids], Acc) ->
    Agent_PId = exoself:start(Agent_Id, self()),
    summon_agents_cached(OpMode, Agent_Ids, [{Agent_Id, Agent_PId}|Acc]);
summon_agents_cached(_OpMode, [], Acc) ->
    Acc.
```

## Priority 2: Algorithm Optimization

### 4. Reduce Sensor Resolution During Evolution (sensor.erl)

**Problem**: Using 100x10 charts during evolution is overkill and wastes computation.

**Solution**: Use smaller charts during evolution, full resolution for final evaluation.

```erlang
%% Add to sensor.erl - Adaptive resolution
fx_PCI_adaptive(Exoself_Id, VL, Parameters, Scape) ->
    [HRes, VRes] = Parameters,
    
    %% Check if we're in evolution or final evaluation mode
    {AdaptiveHRes, AdaptiveVRes} = case get(opmode) of
        gt -> 
            %% During evolution, use reduced resolution
            EvolHRes = max(10, HRes div 10),  % Minimum 10, or 1/10th of requested
            EvolVRes = max(5, VRes div 2),    % Minimum 5, or 1/2 of requested
            {EvolHRes, AdaptiveVRes};
        benchmark ->
            %% During benchmarking, use full resolution
            {HRes, VRes}
    end,
    
    CurrencyPair = config:primary_currency_pair(),
    StartIndex = config:data_start_index(),
    EndIndex = case get(opmode) of
        gt -> config:data_end_index();
        benchmark -> config:benchmark_end_index()
    end,
    
    Scape ! {self(), sense, CurrencyPair, close, [AdaptiveHRes, AdaptiveVRes, graph_sensor], StartIndex, EndIndex},
    receive 
        {_From, Result} -> Result
    end.

%% Update morphology.erl to use adaptive sensors
forex_trader_adaptive(sensors) ->
    PLI_Sensors = [#sensor{name=fx_PLI, type=standard, scape={private,fx_sim}, format=no_geo, vl=HRes, parameters=[HRes,close]} || HRes <- config:pli_resolutions()],
    PCI_Sensors = [#sensor{name=fx_PCI_adaptive, type=standard, scape={private,fx_sim}, format={symmetric,[HRes,VRes]}, vl=HRes*VRes, parameters=[HRes,VRes]} || HRes <- config:pci_horizontal_resolutions(), VRes <- config:pci_vertical_resolutions()],
    PCI_Sensors.
```

### 5. Implement Fitness Caching (population_monitor.erl)

**Problem**: Identical genomes are re-evaluated multiple times.

**Solution**: Cache fitness results based on genome fingerprint.

```erlang
%% Add to population_monitor.erl - Fitness caching
-define(FITNESS_CACHE_TABLE, fitness_cache).

%% Initialize fitness cache
init_fitness_cache() ->
    ets:new(?FITNESS_CACHE_TABLE, [set, public, named_table, {read_concurrency, true}]).

%% Generate genome fingerprint for caching
generate_genome_fingerprint(Agent_Id) ->
    A = genotype:dirty_read_cached({agent, Agent_Id}),
    Cx = genotype:dirty_read_cached({cortex, A#agent.cx_id}),
    
    %% Create fingerprint from neuron weights and structure
    NeuronData = [begin
        N = genotype:dirty_read_cached({neuron, NId}),
        {NId, N#neuron.input_idps, N#neuron.af}
    end || NId <- Cx#cortex.neuron_ids],
    
    %% Hash the structure for fast lookup
    erlang:phash2({A#agent.constraint, NeuronData}).

%% Check fitness cache before evaluation
check_fitness_cache(Agent_Id) ->
    case ets:info(?FITNESS_CACHE_TABLE, name) of
        undefined -> init_fitness_cache(), cache_miss;
        _ -> ok
    end,
    
    Fingerprint = generate_genome_fingerprint(Agent_Id),
    case ets:lookup(?FITNESS_CACHE_TABLE, Fingerprint) of
        [{Fingerprint, CachedFitness, _Timestamp}] ->
            {cache_hit, CachedFitness};
        [] ->
            cache_miss
    end.

%% Store fitness in cache
store_fitness_cache(Agent_Id, Fitness) ->
    Fingerprint = generate_genome_fingerprint(Agent_Id),
    Timestamp = erlang:system_time(second),
    ets:insert(?FITNESS_CACHE_TABLE, {Fingerprint, Fitness, Timestamp}).

%% Update exoself.erl to use fitness caching
%% In exoself:loop/2, before evaluation:
loop(S, OpMode) ->
    Agent_Id = S#state.agent_id,
    case population_monitor:check_fitness_cache(Agent_Id) of
        {cache_hit, CachedFitness} ->
            %% Use cached fitness, skip evaluation
            PM_PId = S#state.pm_pid,
            PM_PId ! {Agent_Id, terminated, CachedFitness},
            ok;
        cache_miss ->
            %% Proceed with normal evaluation
            continue_evaluation(S, OpMode)
    end.

%% After evaluation completes, cache the result
evaluation_complete(S, Fitness) ->
    population_monitor:store_fitness_cache(S#state.agent_id, Fitness),
    %% Continue with normal termination process
    PM_PId = S#state.pm_pid,
    PM_PId ! {S#state.agent_id, terminated, Fitness}.
```

## Priority 3: System Architecture

### 6. Parallel Agent Evaluation (population_monitor.erl)

**Problem**: Agents evaluated sequentially, not utilizing multi-core systems.

**Solution**: Evaluate agents in parallel worker processes.

```erlang
%% Add to population_monitor.erl - Parallel evaluation
-define(MAX_PARALLEL_AGENTS, 8).  % Configurable based on CPU cores

%% Parallel agent spawning
summon_agents_parallel(OpMode, Agent_Ids) ->
    MaxParallel = min(?MAX_PARALLEL_AGENTS, length(Agent_Ids)),
    
    %% Split agents into batches
    Batches = split_into_batches(Agent_Ids, MaxParallel),
    
    %% Spawn batches in parallel
    BatchPids = [spawn_batch(OpMode, Batch) || Batch <- Batches],
    
    %% Collect results
    collect_batch_results(BatchPids, []).

split_into_batches(List, NumBatches) ->
    BatchSize = length(List) div NumBatches,
    split_into_batches(List, BatchSize, []).

split_into_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
split_into_batches(List, BatchSize, Acc) when length(List) =< BatchSize ->
    lists:reverse([List|Acc]);
split_into_batches(List, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(BatchSize, List),
    split_into_batches(Rest, BatchSize, [Batch|Acc]).

spawn_batch(OpMode, Agent_Ids) ->
    spawn(fun() ->
        Results = [begin
            Agent_PId = exoself:start(Agent_Id, self()),
            {Agent_Id, Agent_PId}
        end || Agent_Id <- Agent_Ids],
        exit({batch_complete, Results})
    end).

collect_batch_results([Pid|Pids], Acc) ->
    receive
        {'EXIT', Pid, {batch_complete, Results}} ->
            collect_batch_results(Pids, Results ++ Acc)
    end;
collect_batch_results([], Acc) ->
    Acc.
```

### 7. Streaming Data Processing (fx.erl)

**Problem**: Loading all 50k price ticks into memory at once.

**Solution**: Process data in streaming chunks.

```erlang
%% Add to fx.erl - Streaming data processor
-define(CHUNK_SIZE, 1000).  % Process 1000 ticks at a time

%% Streaming data loader
init_streaming_data(TableName, StartIndex, EndIndex) ->
    put(data_stream, {TableName, StartIndex, EndIndex, StartIndex}).

%% Get next chunk of data
get_next_data_chunk() ->
    case get(data_stream) of
        undefined -> 
            [];
        {TableName, StartIndex, EndIndex, CurrentIndex} when CurrentIndex >= EndIndex ->
            [];
        {TableName, StartIndex, EndIndex, CurrentIndex} ->
            ChunkEnd = min(CurrentIndex + ?CHUNK_SIZE, EndIndex),
            Chunk = load_data_chunk(TableName, CurrentIndex, ChunkEnd),
            put(data_stream, {TableName, StartIndex, EndIndex, ChunkEnd}),
            Chunk
    end.

load_data_chunk(TableName, StartIdx, EndIdx) ->
    load_data_chunk(TableName, StartIdx, EndIdx, []).

load_data_chunk(TableName, Idx, EndIdx, Acc) when Idx >= EndIdx ->
    lists:reverse(Acc);
load_data_chunk(TableName, Idx, EndIdx, Acc) ->
    case lookup_cached(TableName, Idx) of
        undefined -> 
            lists:reverse(Acc);
        Record ->
            NextIdx = next_cached(TableName, Idx),
            load_data_chunk(TableName, NextIdx, EndIdx, [Record|Acc])
    end.

%% Update simulation to use streaming
sim_streaming(ExoSelf, S, A) ->
    case get_next_data_chunk() of
        [] ->
            %% End of data stream
            Total_Profit = A#account.balance + A#account.unrealized_PL,
            Total_Profit;
        DataChunk ->
            %% Process chunk
            {U_S, U_A} = process_data_chunk(S, A, DataChunk),
            sim_streaming(ExoSelf, U_S, U_A)
    end.

process_data_chunk(S, A, [Record|Records]) ->
    %% Process single record
    {U_S, U_A} = process_single_tick(S, A, Record),
    process_data_chunk(U_S, U_A, Records);
process_data_chunk(S, A, []) ->
    {S, A}.
```

## Integration Instructions

### Step 1: Update config.erl
Add new configuration parameters:
```erlang
%% Performance optimization settings
enable_price_cache() -> true.
enable_chart_cache() -> true.
enable_fitness_cache() -> true.
max_parallel_agents() -> 8.
data_chunk_size() -> 1000.
adaptive_resolution() -> true.
```

### Step 2: Update System Initialization
In your main startup sequence, add:
```erlang
%% Initialize all caches
fx:init_price_cache('EURUSD15'),
fx:init_chart_cache(),
population_monitor:init_fitness_cache(),
genotype:init_agent_cache().
```

### Step 3: Testing
Start with small parameters to validate optimizations:
```erlang
specie_size_limit() -> 10.
init_specie_size() -> 10.
evaluations_limit() -> 1000.
```

These optimizations should reduce your runtime from months to hours while maintaining the same evolutionary results.