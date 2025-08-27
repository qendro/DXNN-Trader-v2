%% Simple Live ETS Test for Docker
%% Run this in your Docker container to test the implementation
%% Delete this file after testing

-module(test_live_ets_simple_delete).
-compile(export_all).

%% Quick test function - run this first
quick_test() ->
    io:format("=== Quick Live ETS Test ===~n"),
    
    %% Test 1: Configuration
    io:format("1. Testing configuration...~n"),
    try
        Enabled = config:live_trading_enabled(),
        UpdateInterval = config:live_data_update_interval(),
        MaxRecords = config:live_data_max_records(),
        io:format("   ✓ Live trading enabled: ~p~n", [Enabled]),
        io:format("   ✓ Update interval: ~pms~n", [UpdateInterval]),
        io:format("   ✓ Max records: ~p~n", [MaxRecords])
    catch
        Error:Reason ->
            io:format("   ✗ Configuration test failed: ~p:~p~n", [Error, Reason]),
            return
    end,
    
    %% Test 2: Live table creation
    io:format("2. Testing live table creation...~n"),
    try
        live_scape:init_live_tables(),
        io:format("   ✓ Live tables initialized~n")
    catch
        Error:Reason ->
            io:format("   ✗ Live table creation failed: ~p:~p~n", [Error, Reason]),
            return
    end,
    
    %% Test 3: Data insertion and lookup
    io:format("3. Testing data insertion and lookup...~n"),
    try
        TestRecord = #technical{
            id = {2024, 1, 1, 12, 0, 0, 60},
            open = 1.1000,
            high = 1.1010,
            low = 1.0990,
            close = 1.1005,
            volume = 1000
        },
        
        ets:insert(live_EURUSD1, TestRecord),
        
        case ets:lookup(live_EURUSD1, {2024, 1, 1, 12, 0, 0, 60}) of
            [TestRecord] ->
                io:format("   ✓ Data insertion and lookup successful~n");
            Other ->
                io:format("   ✗ Data lookup failed: ~p~n", [Other])
        end
    catch
        Error:Reason ->
            io:format("   ✗ Data test failed: ~p:~p~n", [Error, Reason])
    end,
    
    %% Test 4: Performance monitoring
    io:format("4. Testing performance monitoring...~n"),
    try
        live_scape:monitor_live_tables(),
        io:format("   ✓ Performance monitoring working~n")
    catch
        Error:Reason ->
            io:format("   ✗ Performance monitoring failed: ~p:~p~n", [Error, Reason])
    end,
    
    io:format("=== Quick test completed ===~n").

%% Cleanup function
cleanup() ->
    io:format("Cleaning up test data...~n"),
    try
        ets:delete(live_EURUSD1),
        ets:delete(live_EURUSD15),
        ets:delete(live_EURUSD30),
        ets:delete(live_EURUSD60),
        io:format("✓ Cleanup completed~n")
    catch
        _:_ ->
            io:format("Cleanup completed (some tables may not have existed)~n")
    end.

%% Run all tests and cleanup
run_and_cleanup() ->
    quick_test(),
    cleanup().
