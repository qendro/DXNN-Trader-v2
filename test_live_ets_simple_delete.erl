%% Simple Live ETS Test for Docker
%% Run this in your Docker container to test the implementation
%% Delete this file after testing

-module(test_live_ets_simple_delete).
-include("records.hrl").
-record(technical, {id, open, high, low, close, volume}).
-export([quick_test/0, cleanup/0, run_and_cleanup/0]).

%% Quick test function - run this first
quick_test() ->
    io:format("=== Quick Live ETS Test ===~n"),

    %% Test 1: Configuration
    io:format("1. Testing configuration...~n"),
    try
        Enabled        = config:live_trading_enabled(),
        UpdateInterval = config:live_data_update_interval(),
        MaxRecords     = config:live_data_max_records(),
        io:format("   ✓ Live trading enabled: ~p~n", [Enabled]),
        io:format("   ✓ Update interval: ~pms~n", [UpdateInterval]),
        io:format("   ✓ Max records: ~p~n", [MaxRecords])
    catch
        Error1:Reason1 ->
            io:format("   ✗ Configuration test failed: ~p:~p~n", [Error1, Reason1]),
            return
    end,

    %% Test 2: Live table creation (always recreate fresh if you implemented that)
    io:format("2. Testing live table creation...~n"),
    try
        live_scape:init_live_tables(),
        io:format("   ✓ Live tables initialized~n")
    catch
        Error2:Reason2 ->
            io:format("   ✗ Live table creation failed: ~p:~p~n", [Error2, Reason2]),
            return
    end,

    %% Test 3: Data insertion and lookup (use #technical{}; key is id 7-tuple)
    io:format("3. Testing data insertion and lookup...~n"),
    try
        TestId = {2024, 1, 1, 12, 0, 0, 60},   %% <-- 7-tuple id (keypos=2)
        TestRecord = #technical{
            id     = TestId,
            open   = 1.1000,
            high   = 1.1010,
            low    = 1.0990,
            close  = 1.1005,
            volume = 1000
        },

        true = ets:insert(live_EURUSD1, TestRecord),

        %% Optional debug
        %% io:format("Size: ~p First: ~p Last: ~p~n",
        %%           [ets:info(live_EURUSD1, size), ets:first(live_EURUSD1), ets:last(live_EURUSD1)]),

        case ets:lookup(live_EURUSD1, TestId) of
            [TestRecord] ->
                io:format("   ✓ Data insertion and lookup successful~n");
            Other ->
                io:format("   ✗ Data lookup failed: ~p~n", [Other])
        end
    catch
        Error3:Reason3 ->
            io:format("   ✗ Data test failed: ~p:~p~n", [Error3, Reason3])
    end,

    %% Test 4: Performance monitoring
    io:format("4. Testing performance monitoring...~n"),
    try
        live_scape:monitor_live_tables(),
        io:format("   ✓ Performance monitoring working~n")
    catch
        Error4:Reason4 ->
            io:format("   ✗ Performance monitoring failed: ~p:~p~n", [Error4, Reason4])
    end,

    io:format("=== Quick test completed ===~n").

%% Cleanup function
cleanup() ->
    io:format("Cleaning up test data...~n"),
    try
        ets:delete(live_EURUSD1),
        io:format("✓ Cleanup completed~n")
    catch
        _:_ ->
            io:format("Cleanup completed (some tables may not have existed)~n")
    end.

%% Run all tests and cleanup
run_and_cleanup() ->
    quick_test(),
    cleanup().
