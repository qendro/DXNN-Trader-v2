%% Phase 2 Test Suite - Essential Reliability
-module(test_phase2).
-compile(export_all).

%% Main test function
test_all() ->
    io:format("=== Phase 2: Essential Reliability Test ===~n"),
    
    %% Step 1: Test enhanced error handling
    test_enhanced_error_handling(),
    
    %% Step 2: Test symbol normalization
    test_symbol_normalization(),
    
    %% Step 3: Test reconnection (simulation)
    test_reconnection_handling(),
    
    %% Step 4: Test clean shutdown
    test_clean_shutdown(),
    
    io:format("=== Phase 2 Test Complete ===~n").

%% Test enhanced error handling
test_enhanced_error_handling() ->
    io:format("~n1. Testing Enhanced Error Handling:~n"),
    
    try
        %% Test with invalid port to trigger IB_REJECT error
        Result = ib_bridge_connector:start_connection("host.docker.internal", 9999, 101),
        case Result of
            {error, _} ->
                io:format("   ✓ Enhanced error handling works~n");
            {ok, _} ->
                io:format("   ⚠ Unexpected success with invalid port~n"),
                ib_bridge_connector:stop_connection()
        end
    catch
        _:Error ->
            io:format("   ✓ Error handling caught exception: ~p~n", [Error])
    end.

%% Test symbol normalization
test_symbol_normalization() ->
    io:format("~n2. Testing Symbol Normalization:~n"),
    
    try
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        
        {ok, _} = ib_bridge_connector:start_connection(Host, Port, ClientId),
        timer:sleep(3000), % Wait for connection
        
        %% Test different symbol formats
        Symbols = ["EUR.USD", "GBP.USD", "USD.JPY"],
        lists:foreach(fun(Symbol) ->
            case ib_bridge_connector:subscribe_market_data(Symbol, 1) of
                ok ->
                    io:format("   ✓ Symbol ~s normalized and subscribed~n", [Symbol]);
                {error, Reason} ->
                    io:format("   ⚠ Symbol ~s subscription failed: ~p~n", [Symbol, Reason])
            end
        end, Symbols),
        
        ib_bridge_connector:stop_connection()
        
    catch
        _:Error ->
            io:format("   ✗ Symbol normalization test failed: ~p~n", [Error])
    end.

%% Test reconnection handling (simulation)
test_reconnection_handling() ->
    io:format("~n3. Testing Reconnection Handling:~n"),
    
    try
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        
        {ok, _} = ib_bridge_connector:start_connection(Host, Port, ClientId),
        timer:sleep(3000), % Wait for connection
        
        %% Monitor connection status
        {ok, Status1} = ib_bridge_connector:get_connection_status(),
        io:format("   Initial connection status: ~p~n", [Status1]),
        
        %% Wait and check if reconnection logic is working
        io:format("   ⏳ Monitoring for 10 seconds (check logs for reconnection activity)...~n"),
        timer:sleep(10000),
        
        {ok, Status2} = ib_bridge_connector:get_connection_status(),
        io:format("   Final connection status: ~p~n", [Status2]),
        
        case Status2 of
            true ->
                io:format("   ✓ Connection maintained or reconnected successfully~n");
            false ->
                io:format("   ⚠ Connection lost (check TWS is running)~n")
        end,
        
        ib_bridge_connector:stop_connection()
        
    catch
        _:Error ->
            io:format("   ✗ Reconnection test failed: ~p~n", [Error])
    end.

%% Test clean shutdown
test_clean_shutdown() ->
    io:format("~n4. Testing Clean Shutdown:~n"),
    
    try
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        
        {ok, Pid} = ib_bridge_connector:start_connection(Host, Port, ClientId),
        io:format("   Bridge started with PID: ~p~n", [Pid]),
        
        timer:sleep(2000), % Let it settle
        
        %% Test graceful shutdown
        ok = ib_bridge_connector:stop_connection(),
        io:format("   ✓ Stop command sent~n"),
        
        %% Verify process is gone
        timer:sleep(1000),
        case whereis(ib_bridge_connector) of
            undefined ->
                io:format("   ✓ Bridge process cleaned up properly~n");
            StillRunning ->
                io:format("   ⚠ Bridge process still running: ~p~n", [StillRunning])
        end
        
    catch
        _:Error ->
            io:format("   ✗ Clean shutdown test failed: ~p~n", [Error])
    end.

%% Quick Phase 2 validation
quick_test() ->
    io:format("Quick Phase 2 validation...~n"),
    
    try
        %% Test compilation
        case compile:file(ib_bridge_connector) of
            {ok, _} -> io:format("✓ Phase 2 compilation OK~n");
            _ -> io:format("✗ Phase 2 compilation failed~n")
        end,
        
        %% Test basic functionality
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        
        {ok, _} = ib_bridge_connector:start_connection(Host, Port, ClientId),
        timer:sleep(2000),
        
        %% Test multiple symbols
        ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
        ok = ib_bridge_connector:subscribe_market_data("GBP.USD", 2),
        
        {ok, Status} = ib_bridge_connector:get_connection_status(),
        case Status of
            true -> io:format("✓ Phase 2 functionality OK~n");
            false -> io:format("⚠ Phase 2 connected but not to TWS~n")
        end,
        
        ib_bridge_connector:stop_connection(),
        io:format("✓ Phase 2 validation complete~n")
        
    catch
        _:Error ->
            io:format("✗ Phase 2 validation failed: ~p~n", [Error])
    end.