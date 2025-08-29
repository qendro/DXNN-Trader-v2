# Live ETS Tables Implementation Plan
## Option 1: Live ETS Tables with Pull-on-Demand Strategy

This document outlines the complete implementation plan for adding live trading support to the DXNN system using ETS tables that mirror the existing historical data structure, with a pull-on-demand strategy for handling data requests.

---

## Overview

**Goal**: Implement live trading support while maintaining 100% compatibility with existing sensors and neural network code.

**Strategy**: 
- Create live ETS tables that mirror historical table structure
- Use background process to continuously populate live tables with IB data
- Implement pull-on-demand strategy for handling data requests
- Maintain identical sensor interface for both live and historical data

**Key Benefits**:
- Zero changes required to existing sensor code
- Real-time data with immediate data fetching when needed
- Proactive data collection based on actual usage patterns
- Configurable data handling strategies

---

## Phase 1: Core Infrastructure Setup

### Step 1.1: Add Live Table Definitions
**File**: `live_scape.erl` (Lines 1-20)
**Time Estimate**: 15 minutes

```erlang
%% Live Scape Module for Sensor/Actuator Interface
%% Provides scape interface compatible with existing sensor/actuator pattern
%% Handles live market data from IB connector and trade execution

-module(live_scape).
-compile(export_all).
-include("records.hrl").

%% API for supervisor integration
-export([start_link/0]).

%% ETS table for live price data buffer
-define(LIVE_PRICE_BUFFER, live_price_buffer).
-define(MAX_BUFFER_SIZE, 1000).

%% Live ETS table definitions
-define(LIVE_TABLES, [live_EURUSD1, live_EURUSD15, live_EURUSD30, live_EURUSD60]).
-define(HISTORICAL_TABLES, [EURUSD1, EURUSD15, EURUSD30, EURUSD60]).

%% Technical record definition (matching fx.erl)
-record(technical,{
    id,    %%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open,
    high,
    low,
    close,
    volume}).
```

**Deliverable**: Live table definitions added to module header

### Step 1.2: Add Live Table Management Functions
**File**: `live_scape.erl` (Add after existing ETS buffer functions)
**Time Estimate**: 30 minutes

```erlang
%% Initialize live trading tables
init_live_tables() ->
    io:format("Initializing live FX tables:~p~n", [?LIVE_TABLES]),
    [init_live_table(TableName) || TableName <- ?LIVE_TABLES],
    io:format("Live FX tables initialized~n").

init_live_table(TableName) ->
    ets:new(TableName, [ordered_set, public, named_table, {keypos, 2}]).

%% Get live table name from historical table name
get_live_table_name(TableName) ->
    list_to_atom("live_" ++ atom_to_list(TableName)).

%% Check if table is a live table
is_live_table(TableName) ->
    lists:member(TableName, ?LIVE_TABLES).

%% Check if this is a live data request
is_live_table_request(TableName) ->
    %% Check if the request is for live_data or if we're in live trading mode
    case config:live_trading_enabled() of
        true ->
            %% In live mode, check if this table has a live equivalent
            lists:member(TableName, ?HISTORICAL_TABLES);
        false ->
            false
    end.

%% Get historical table name from live table name
get_historical_table_name(LiveTableName) ->
    TableNameStr = atom_to_list(LiveTableName),
    case string:prefix(TableNameStr, "live_") of
        nomatch -> undefined;
        HistoricalName -> list_to_atom(HistoricalName)
    end.
```

**Deliverable**: Live table creation and management functions

### Step 1.3: Add Live Data Conversion Functions
**File**: `live_scape.erl` (Add after live table management functions)
**Time Estimate**: 20 minutes

```erlang
%% Convert IB OHLC data to technical record format
convert_ohlc_to_technical(OHLC) ->
    #technical{
        id = {OHLC#live_ohlc.timestamp, 60},  % 60-second sampling rate
        open = OHLC#live_ohlc.open,
        high = OHLC#live_ohlc.high,
        low = OHLC#live_ohlc.low,
        close = OHLC#live_ohlc.close,
        volume = OHLC#live_ohlc.volume
    }.

%% Convert timestamp to technical record ID format
timestamp_to_id(Timestamp) ->
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    {Year, Month, Day, Hour, Minute, Second, 60}.  % 60-second sampling rate
```

**Deliverable**: Data conversion utilities for IB to ETS format

---

## Phase 2: Enhanced Sensor Interface

### Step 2.1: Enhance Sensor Request Handling
**File**: `live_scape.erl` (Modify existing `handle_sense_request` function)
**Time Estimate**: 45 minutes

```erlang
%% Handle sensor requests for market data
handle_sense_request(TableName, Feature, Parameters, State) ->
    case is_live_table_request(TableName) of
        true ->
            %% Handle live data request
            handle_live_sense_request(TableName, Feature, Parameters, State);
        false ->
            %% Handle historical data request (existing logic)
            handle_historical_sense_request(TableName, Feature, Parameters, State)
    end.

%% Handle live data sensor requests
handle_live_sense_request(TableName, Feature, Parameters, State) ->
    LiveTableName = get_live_table_name(TableName),
    
    %% Ensure live table has data
    case ensure_live_table_with_data(LiveTableName, TableName) of
        {ok, _DataRange} ->
            %% Use live table with same logic as historical
            handle_historical_sense_request(LiveTableName, Feature, Parameters, State);
        {error, Reason} ->
            io:format("Live data not available: ~p, using historical~n", [Reason]),
            handle_historical_sense_request(TableName, Feature, Parameters, State)
    end.

%% Handle historical data sensor requests (existing logic)
handle_historical_sense_request(TableName, Feature, Parameters, State) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            handle_pci_sensor(TableName, HRes, VRes, State);
        [HRes, list_sensor] ->
            handle_pli_sensor(TableName, HRes, State);
        _ ->
            io:format("Unknown sensor parameters: ~p~n", [Parameters]),
            {[], State}
    end.
```

**Deliverable**: Live data initialization with fallback support

### Step 2.2: Add Live Table Data Pulling Functions
**File**: `live_scape.erl` (Add after existing ETS buffer functions)
**Time Estimate**: 30 minutes

```erlang
%% Ensure live table exists and has data, pulling from IB if needed
ensure_live_table_with_data(LiveTableName, HistoricalTableName) ->
    %% Create live table if it doesn't exist
    case ets:info(LiveTableName) of
        undefined ->
            init_live_table(LiveTableName);
        _ ->
            ok
    end,
    
    %% Check if we have recent data, if not pull from IB
    case has_recent_data(LiveTableName) of
        true ->
            %% Use existing data
            Index_End = ets:last(LiveTableName),
            Index_Start = max(1, Index_End - 99),
            {ok, {Index_Start, Index_End}};
        false ->
            %% Pull fresh data from IB Bridge
            pull_live_data_from_ib(LiveTableName, HistoricalTableName)
    end.

%% Check if live table has recent data (within last 5 minutes)
has_recent_data(LiveTableName) ->
    case ets:last(LiveTableName) of
        '$end_of_table' ->
            false;
        LastIndex ->
            {Year, Month, Day, Hour, Minute, Second, _} = LastIndex,
            {CurrentYear, CurrentMonth, CurrentDay} = date(),
            {CurrentHour, CurrentMinute, CurrentSecond} = time(),
            
            LastTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
            CurrentTime = calendar:datetime_to_gregorian_seconds({{CurrentYear, CurrentMonth, CurrentDay}, {CurrentHour, CurrentMinute, CurrentSecond}}),
            
            %% Data is recent if it's within 5 minutes
            (CurrentTime - LastTime) < 300
    end.

%% Pull live data from IB Bridge
pull_live_data_from_ib(LiveTableName, HistoricalTableName) ->
    %% Get currency pair from table name
    CurrencyPair = get_currency_pair_from_table_name(LiveTableName),
    
    case ib_bridge_connector:get_ohlc_data(CurrencyPair, 60) of  % 1-minute bars
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(LiveTableName, TechnicalRecord)
            end, OHLCList),
            
            %% Return data range
            Index_End = ets:last(LiveTableName),
            Index_Start = max(1, Index_End - 99),
            {ok, {Index_Start, Index_End}};
        {ok, []} ->
            %% No live data available, fallback to historical
            io:format("No live data available for ~p, using historical~n", [CurrencyPair]),
            fallback_to_historical_data(LiveTableName, HistoricalTableName);
        {error, Reason} ->
            %% IB error, fallback to historical
            io:format("IB Bridge error for ~p: ~p, using historical~n", [CurrencyPair, Reason]),
            fallback_to_historical_data(LiveTableName, HistoricalTableName)
    end.

%% Get currency pair string from live table name
get_currency_pair_from_table_name(LiveTableName) ->
    TableNameStr = atom_to_list(LiveTableName),
    case string:prefix(TableNameStr, "live_") of
        nomatch -> 
            %% Fallback to table name itself
            TableNameStr;
        CurrencyPair -> 
            CurrencyPair
    end.

%% Fallback to historical data when live data is not available
fallback_to_historical_data(LiveTableName, HistoricalTableName) ->
    case ets:info(HistoricalTableName) of
        undefined ->
            {error, no_historical_data};
        _ ->
            %% Copy last 100 data points from historical table
            LastIndex = ets:last(HistoricalTableName),
            StartIndex = max(1, LastIndex - 99),
            copy_historical_to_live(HistoricalTableName, LiveTableName, StartIndex, LastIndex),
            {ok, {StartIndex, LastIndex}}
    end.

%% Copy data from historical table to live table
copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex) ->
    copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex, StartIndex).

copy_historical_to_live(_HistoricalTable, _LiveTable, _StartIndex, EndIndex, CurrentIndex) 
    when CurrentIndex > EndIndex ->
    ok;
copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex, CurrentIndex) ->
    case ets:lookup(HistoricalTable, CurrentIndex) of
        [Record] ->
            ets:insert(LiveTable, Record),
            copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex, CurrentIndex + 1);
        [] ->
            copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex, CurrentIndex + 1)
    end.

%% Fallback state when live data is not available
init_fallback_state(S, TableName, Feature) ->
    io:format("Using fallback historical data for ~p~n", [TableName]),
    %% Use recent historical data instead
    Index_End = ets:last(TableName),
    Index_Start = max(1, Index_End - 99),
    S#state{
        table_name = TableName,
        feature = Feature,
        index_start = Index_Start,
        index_end = Index_End,
        index = Index_Start
    }.
```

**Deliverable**: Robust live table initialization with historical fallback

---

## Phase 3: Live Data Feeder System

### Step 3.1: Create Proactive Live Data Feeder Process
**File**: `live_scape.erl` (Add after existing functions)
**Time Estimate**: 45 minutes

```erlang
%% Start live data feeder process
start_live_data_feeder() ->
    case whereis(live_data_feeder) of
        undefined ->
            Pid = spawn(fun() -> live_data_feeder_loop() end),
            register(live_data_feeder, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

%% Stop live data feeder process
stop_live_data_feeder() ->
    case whereis(live_data_feeder) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            ok
    end.

%% Main live data feeder loop with proactive data collection
live_data_feeder_loop() ->
    %% Get live data from IB Bridge for all configured pairs
    CurrencyPairs = config:live_currency_pairs(),
    
    %% Proactively update all live tables
    lists:foreach(fun(Pair) ->
        update_live_table_with_ib_data(Pair)
    end, CurrencyPairs),
    
    %% Check for pending data requests and fulfill them
    fulfill_pending_data_requests(),
    
    %% Wait before next update
    receive
        stop -> ok;
        {request_data, From, TableName, Index} ->
            %% Handle immediate data request
            handle_immediate_data_request(From, TableName, Index),
            live_data_feeder_loop()
    after config:live_data_update_interval() ->
        live_data_feeder_loop()
    end.

%% Handle immediate data requests from lookup functions
handle_immediate_data_request(From, TableName, Index) ->
    case pull_missing_data(TableName, Index) of
        {ok, Row} ->
            From ! {data_ready, Row};
        {error, Reason} ->
            From ! {data_error, Reason}
    end.

%% Fulfill any pending data requests
fulfill_pending_data_requests() ->
    %% This could be enhanced to track pending requests
    %% For now, we rely on the pull-on-demand strategy in lookup functions
    ok.

%% Update live table with data from IB Bridge
update_live_table_with_ib_data(CurrencyPair) ->
    Symbol = atom_to_list(CurrencyPair),
    LiveTableName = get_live_table_name(CurrencyPair),
    
    %% Get recent data from IB Bridge
    case ib_bridge_connector:get_recent_ohlc_data(Symbol, 60, 100) of  % Last 100 bars
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(LiveTableName, TechnicalRecord)
            end, OHLCList),
            
            %% Keep only recent data (last 1000 points)
            cleanup_old_live_data(LiveTableName, config:live_data_max_records());
        {ok, []} ->
            io:format("No OHLC data available for ~s~n", [Symbol]);
        {error, Reason} ->
            io:format("Failed to get OHLC data for ~s: ~p~n", [Symbol, Reason])
    end.

%% Clean up old data from live table
cleanup_old_live_data(TableName, MaxRecords) ->
    case ets:info(TableName, size) of
        Size when Size > MaxRecords ->
            %% Remove oldest records
            RecordsToRemove = Size - MaxRecords,
            remove_oldest_records(TableName, RecordsToRemove);
        _ ->
            ok
    end.

%% Remove oldest records from table
remove_oldest_records(TableName, Count) ->
    remove_oldest_records(TableName, Count, 0).

remove_oldest_records(_TableName, Count, Removed) when Removed >= Count ->
    ok;
remove_oldest_records(TableName, Count, Removed) ->
    case ets:first(TableName) of
        '$end_of_table' ->
            ok;
        Key ->
            ets:delete(TableName, Key),
            remove_oldest_records(TableName, Count, Removed + 1)
    end.
```

**Deliverable**: Background process for continuous IB data collection

---

## Phase 4: Enhanced Data Access Functions

### Step 4.1: Add Pull-on-Demand Data Access Functions
**File**: `live_scape.erl` (Add new functions for data access)
**Time Estimate**: 60 minutes

```erlang
%% Handle data requests in live tables with pull-on-demand strategy
lookup_live_with_pull(TableName, RequestedIndex) ->
    case ets:lookup(TableName, RequestedIndex) of
        [Row] -> 
            %% Data exists, return it immediately
            Row;
        [] ->
            %% Data doesn't exist, try to pull it
            case pull_missing_data(TableName, RequestedIndex) of
                {ok, Row} ->
                    %% Successfully pulled data
                    Row;
                {error, _Reason} ->
                    %% Failed to pull data, return latest available or undefined
                    get_latest_available_data(TableName)
            end
    end.

%% Enhanced sensor data access with live table support
get_sensor_data(TableName, Feature, Parameters) ->
    case is_live_table_request(TableName) of
        true ->
            %% Use live table with pull-on-demand
            LiveTableName = get_live_table_name(TableName),
            get_live_sensor_data(LiveTableName, Feature, Parameters);
        false ->
            %% Use historical table (existing logic)
            get_historical_sensor_data(TableName, Feature, Parameters)
    end.

%% Pull missing data from IB Bridge
pull_missing_data(TableName, RequestedIndex) ->
    %% Get currency pair from table name
    CurrencyPair = get_currency_pair_from_table_name(TableName),
    
    %% Determine time range to request from IB
    {StartTime, EndTime} = calculate_request_time_range(RequestedIndex),
    
    case ib_bridge_connector:get_ohlc_data_range(CurrencyPair, 60, StartTime, EndTime) of
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(TableName, TechnicalRecord)
            end, OHLCList),
            
            %% Try to get the requested data again
            case ets:lookup(TableName, RequestedIndex) of
                [Row] -> {ok, Row};
                [] -> {error, data_not_found}
            end;
        {ok, []} ->
            {error, no_data_available};
        {error, Reason} ->
            {error, Reason}
    end.

%% Calculate time range for IB data request
calculate_request_time_range(RequestedIndex) ->
    {Year, Month, Day, Hour, Minute, Second, _} = RequestedIndex,
    RequestedTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    
    %% Request 10 minutes before and after the requested time
    StartTime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(RequestedTime) - 600
    ),
    EndTime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(RequestedTime) + 600
    ),
    
    {StartTime, EndTime}.

%% Get the latest available data from the table
get_latest_available_data(TableName) ->
    case ets:last(TableName) of
        '$end_of_table' ->
            %% No data available at all
            create_default_technical_record();
        LastIndex ->
            %% Return the most recent data point
            case ets:lookup(TableName, LastIndex) of
                [Row] -> Row;
                [] -> create_default_technical_record()
            end
    end.

%% Create a default technical record when no data is available
create_default_technical_record() ->
    #technical{
        id = {2024, 1, 1, 0, 0, 0, 60},  % {Year,Month,Day,Hour,Minute,Second,sampling_rate}
        open = 1.0,
        high = 1.0,
        low = 1.0,
        close = 1.0,
        volume = 0
    }.
```

**Deliverable**: Robust lookup function with pull-on-demand strategy

### Step 4.2: Add Live Table Navigation Functions
**File**: `live_scape.erl` (Add navigation functions for live tables)
**Time Estimate**: 15 minutes

```erlang
%% Enhanced navigation functions with live table support
next_live(TableName, CurrentIndex) ->
    case is_live_table(TableName) of
        true ->
            %% Live table navigation
            ets:next(TableName, CurrentIndex);
        false ->
            %% Historical table navigation (existing logic)
            ets:next(TableName, CurrentIndex)
    end.

%% Get first record from live table
first_live(TableName) ->
    case is_live_table(TableName) of
        true ->
            ets:first(TableName);
        false ->
            ets:first(TableName)
    end.

%% Get last record from live table
last_live(TableName) ->
    case is_live_table(TableName) of
        true ->
            ets:last(TableName);
        false ->
            ets:last(TableName)
    end.
```

**Deliverable**: Enhanced navigation functions for live tables

---

## Phase 5: Configuration and Integration

### Step 5.1: Add Configuration Functions
**File**: `config.erl` (Add new configuration functions)
**Time Estimate**: 20 minutes

```erlang
%% Live trading configuration
live_trading_enabled() -> true.  % Set to false to disable live trading
live_currency_pairs() -> [EURUSD1, EURUSD15, EURUSD30, EURUSD60].
live_data_update_interval() -> 30000.  % 30 seconds (more frequent updates)
live_data_max_records() -> 1000.  % Max records per live table

%% Pull-on-demand strategy configuration
live_data_pull_timeout() -> 10000.  % 10 seconds timeout for IB data requests
live_data_freshness_threshold() -> 300.  % 5 minutes - data considered stale after this
live_data_pull_range_minutes() -> 20.  % Pull 20 minutes of data around requested time
live_data_fallback_strategy() -> historical.  % Options: historical, latest_available, fail
```

**Deliverable**: Configuration system for live trading parameters

### Step 5.2: Update `live_scape:init_scape/0` Function
**File**: `live_scape.erl` (Modify existing `init_scape/0` function)
**Time Estimate**: 15 minutes

```erlang
%% Initialize scape process
init_scape() ->
    %% Initialize ETS table for price buffer
    init_price_buffer(),
    
    %% Initialize live tables if live trading is enabled
    case config:live_trading_enabled() of
        true ->
            init_live_tables(),
            start_live_data_feeder();
        false ->
            ok
    end,
    
    %% Wait for exoself to connect
    receive
        {ExoSelf_PId, live_sim} ->
            live_sim(ExoSelf_PId)
    end.
```

**Deliverable**: Integration of live trading into scape initialization

### Step 5.3: Update `live_scape:cleanup_price_buffer/0` Function
**File**: `live_scape.erl` (Modify existing cleanup function)
**Time Estimate**: 15 minutes

```erlang
%% Clean up price buffer and live tables
cleanup_price_buffer() ->
    io:format("Clearing scape market data~n"),
    %% Clear price buffer
    case ets:info(?LIVE_PRICE_BUFFER) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?LIVE_PRICE_BUFFER)
    end,
    
    %% Clean up live tables
    case config:live_trading_enabled() of
        true ->
            [cleanup_live_table(TableName) || TableName <- ?LIVE_TABLES];
        false ->
            ok
    end.

%% Clean up individual live table
cleanup_live_table(TableName) ->
    case ets:info(TableName) of
        undefined -> ok;
        _ -> 
            ets:delete(TableName),
            io:format("Cleaned up live table: ~p~n", [TableName])
    end.
```

**Deliverable**: Enhanced cleanup for live tables

---

## Phase 6: Testing and Validation

### Step 6.1: Add Test Functions
**File**: `live_scape.erl` (Add test functions)
**Time Estimate**: 45 minutes

```erlang
%% Test live table functionality
test_live_tables() ->
    io:format("Testing live table functionality~n"),
    
    %% Test table creation
    init_live_tables(),
    
    %% Test data insertion
TestRecord = #technical{
    id = {2024, 1, 1, 12, 0, 0, 60},  % {Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open = 1.1000,
    high = 1.1010,
    low = 1.0990,
    close = 1.1005,
    volume = 1000
},
    
    ets:insert(live_EURUSD1, TestRecord),
    
    %% Test lookup
    case lookup_live_with_pull(live_EURUSD1, {2024, 1, 1, 12, 0, 0, 60}) of
        TestRecord ->
            io:format("✓ Live table test passed~n");
        Other ->
            io:format("✗ Live table test failed: ~p~n", [Other])
    end.

%% Test pull-on-demand strategy
test_pull_on_demand() ->
    io:format("Testing pull-on-demand strategy~n"),
    
    %% Create test table with some data
    init_live_table(test_live_table),
    
    %% Insert current time data
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    CurrentIndex = {Year, Month, Day, Hour, Minute, Second, 60},
    
    TestRecord = #technical{
    id = CurrentIndex,  % {Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open = 1.1000,
    high = 1.1010,
    low = 1.0990,
    close = 1.1005,
    volume = 1000
},
    
    ets:insert(test_live_table, TestRecord),
    
    %% Test missing data request (should pull from IB)
    MissingIndex = {Year, Month, Day, Hour, Minute + 5, Second, 60},
    
    StartTime = erlang:timestamp(),
    Result = lookup_live_with_pull(test_live_table, MissingIndex),
    EndTime = erlang:timestamp(),
    
    Duration = timer:now_diff(EndTime, StartTime) / 1000,
    io:format("Missing data lookup took ~pms, result: ~p~n", [Duration, Result]),
    
    %% Cleanup
    ets:delete(test_live_table).

%% Test live data integration
test_live_data_integration() ->
    io:format("Testing live data integration~n"),
    
    %% Test live table initialization
    init_live_tables(),
    
    %% Test sensor data retrieval with live tables
    {Result, State} = handle_live_sense_request(EURUSD1, close, [10, list_sensor], #state{}),
    io:format("Live sensor result: ~p~n", [Result]),
    io:format("Updated state: ~p~n", [State]).
```

**Deliverable**: Comprehensive test suite for live trading functionality

### Step 6.2: Add Performance Monitoring
**File**: `live_scape.erl` (Add monitoring functions)
**Time Estimate**: 30 minutes

```erlang
%% Monitor live table performance
monitor_live_tables() ->
    io:format("=== Live Table Performance Report ===~n"),
    lists:foreach(fun(TableName) ->
        case ets:info(TableName) of
            undefined ->
                io:format("~p: Not initialized~n", [TableName]);
            Info ->
                Size = proplists:get_value(size, Info),
                Memory = proplists:get_value(memory, Info),
                io:format("~p: ~p records, ~p bytes~n", [TableName, Size, Memory])
        end
    end, ?LIVE_TABLES).

%% Monitor data freshness
monitor_data_freshness() ->
    io:format("=== Data Freshness Report ===~n"),
    lists:foreach(fun(TableName) ->
        case ets:last(TableName) of
            '$end_of_table' ->
                io:format("~p: No data~n", [TableName]);
            LastIndex ->
                {Year, Month, Day, Hour, Minute, Second, _} = LastIndex,
                {CurrentYear, CurrentMonth, CurrentDay} = date(),
                {CurrentHour, CurrentMinute, CurrentSecond} = time(),
                
                LastTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
                CurrentTime = calendar:datetime_to_gregorian_seconds({{CurrentYear, CurrentMonth, CurrentDay}, {CurrentHour, CurrentMinute, CurrentSecond}}),
                
                AgeSeconds = CurrentTime - LastTime,
                io:format("~p: Last update ~p seconds ago~n", [TableName, AgeSeconds])
        end
    end, ?LIVE_TABLES).
```

**Deliverable**: Performance monitoring and debugging tools

---

## Phase 7: Deployment and Usage

### Step 7.1: Usage Examples
**Time Estimate**: 15 minutes

```erlang
%% Enable live trading
config:live_trading_enabled() -> true.

%% Start the system
live_scape:init_scape().

%% Test live tables
live_scape:test_live_tables().
live_scape:test_pull_on_demand().

%% Monitor performance
live_scape:monitor_live_tables().
live_scape:monitor_data_freshness().

%% Use live data in sensors (same interface as historical)
sensor:fx_PLI(ExoSelf_Id, VL, Parameters, Scape) ->
    Scape ! {self(), sense, EURUSD1, close, [HRes, list_sensor], live_data, live_data}.

%% Use historical data (unchanged)
sensor:fx_PLI(ExoSelf_Id, VL, Parameters, Scape) ->
    Scape ! {self(), sense, EURUSD1, close, [HRes, list_sensor], 1000, 2000}.
```

**Deliverable**: Usage documentation and examples

### Step 7.2: Monitoring and Debugging Commands
**Time Estimate**: 15 minutes

```erlang
%% Check live table status
ets:info(live_EURUSD1).

%% View live data
ets:tab2list(live_EURUSD1).

%% Compare live vs historical
ets:info(EURUSD1).
ets:info(live_EURUSD1).

%% Test live scape integration
live_scape:test_live_tables().
live_scape:monitor_live_tables().

%% Check data freshness
live_scape:monitor_data_freshness().

%% Test pull-on-demand with specific timeout
config:live_data_pull_timeout() -> 15000.  % 15 seconds for testing
```

**Deliverable**: Debugging and monitoring commands

---

## Implementation Timeline

| Phase | Description | Time Estimate | Dependencies |
|-------|-------------|---------------|--------------|
| 1 | Core Infrastructure Setup | 1 hour 5 minutes | None |
| 2 | Enhanced Sensor Interface | 1 hour 15 minutes | Phase 1 |
| 3 | Live Data Feeder System | 45 minutes | Phase 1 |
| 4 | Enhanced Data Access Functions | 1 hour 15 minutes | Phase 1, 2 |
| 5 | Configuration and Integration | 50 minutes | Phase 1, 2, 3 |
| 6 | Testing and Validation | 1 hour 15 minutes | All previous phases |
| 7 | Deployment and Usage | 30 minutes | All previous phases |

**Total estimated time**: 6 hours 35 minutes

---

## Risk Mitigation

### High-Risk Areas:
1. **IB Connection Failures**: Implement robust fallback to historical data
2. **Data Pull Timeouts**: Configurable timeout with graceful degradation
3. **Memory Management**: Automatic cleanup of old data points
4. **Performance Impact**: On-demand data pulling to minimize blocking

### Mitigation Strategies:
1. **Comprehensive Error Handling**: All IB operations wrapped in try-catch
2. **Configurable Timeouts**: Adjustable pull timeouts for different scenarios
3. **Memory Monitoring**: Automatic cleanup and size limits
4. **Performance Testing**: Benchmark against historical data performance

---

## Success Criteria

1. **Zero Sensor Changes**: All existing sensor code works unchanged
2. **Real-time Data**: Live data available immediately when requested
3. **Robust Error Handling**: System continues operating during IB outages
4. **Performance Parity**: Live data performance matches historical data
5. **Configurable Behavior**: All timeouts and pull strategies are configurable

---

## Post-Implementation Validation

1. **Functional Testing**: All sensors work with live data
2. **Performance Testing**: No degradation in neural network evaluation speed
3. **Stress Testing**: System handles IB connection failures gracefully
4. **Integration Testing**: Live trading works with existing neural networks
5. **User Acceptance**: Live data provides meaningful trading signals

This implementation plan provides a complete roadmap for adding live trading support with a pull-on-demand strategy while maintaining full compatibility with your existing system architecture. The system will proactively fetch data when needed rather than waiting for data to become available. All functionality is integrated into the existing `live_scape.erl` module, leveraging the current live trading architecture without creating new files.
