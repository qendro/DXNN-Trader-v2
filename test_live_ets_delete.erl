%% Test file for Live ETS Tables Implementation
%% This file will be deleted after testing is complete

-module(test_live_ets_delete).
-compile(export_all).

%% Test live table functionality
test_live_tables() ->
    io:format("Testing live table functionality~n"),
    
    %% Test table creation
    live_scape:init_live_tables(),
    
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
    case live_scape:lookup_live_with_pull(live_EURUSD1, {2024, 1, 1, 12, 0, 0, 60}) of
        TestRecord ->
            io:format("✓ Live table test passed~n");
        Other ->
            io:format("✗ Live table test failed: ~p~n", [Other])
    end.

%% Test pull-on-demand strategy
test_pull_on_demand() ->
    io:format("Testing pull-on-demand strategy~n"),
    
    %% Create test table with some data
    live_scape:init_live_table(test_live_table),
    
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
    Result = live_scape:lookup_live_with_pull(test_live_table, MissingIndex),
    EndTime = erlang:timestamp(),
    
    Duration = timer:now_diff(EndTime, StartTime) / 1000,
    io:format("Missing data lookup took ~pms, result: ~p~n", [Duration, Result]),
    
    %% Cleanup
    ets:delete(test_live_table).

%% Test live data integration
test_live_data_integration() ->
    io:format("Testing live data integration~n"),
    
    %% Test live table initialization
    live_scape:init_live_tables(),
    
    %% Test sensor data retrieval with live tables
    {Result, State} = live_scape:handle_live_sense_request(EURUSD1, close, [10, list_sensor], #live_state{}),
    io:format("Live sensor result: ~p~n", [Result]),
    io:format("Updated state: ~p~n", [State]).

%% Test configuration
test_configuration() ->
    io:format("Testing live ETS configuration~n"),
    
    %% Test configuration functions
    Enabled = config:live_trading_enabled(),
    UpdateInterval = config:live_data_update_interval(),
    MaxRecords = config:live_data_max_records(),
    
    io:format("Live trading enabled: ~p~n", [Enabled]),
    io:format("Update interval: ~pms~n", [UpdateInterval]),
    io:format("Max records: ~p~n", [MaxRecords]),
    
    %% Test currency pairs
    CurrencyPairs = config:live_currency_pairs(),
    io:format("Currency pairs: ~p~n", [CurrencyPairs]).

%% Run all tests
run_all_tests() ->
    io:format("=== Running Live ETS Tests ===~n"),
    
    try
        test_configuration(),
        test_live_tables(),
        test_pull_on_demand(),
        test_live_data_integration(),
        io:format("=== All tests completed ===~n")
    catch
        Error:Reason ->
            io:format("Test failed: ~p:~p~n", [Error, Reason])
    end.
