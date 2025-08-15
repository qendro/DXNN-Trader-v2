%% Integration test for Python bridge with real TWS connection
-module(test_bridge_integration).
-compile(export_all).

%% Full integration test - requires TWS running
test_full_integration() ->
    io:format("=== Full Bridge Integration Test ===~n"),
    io:format("This test requires TWS running on host machine~n"),
    io:format("Make sure TWS API is enabled on port 7497~n~n"),
    
    %% Step 1: Start bridge
    test_bridge_startup(),
    
    %% Step 2: Wait for connection
    test_connection_establishment(),
    
    %% Step 3: Test market data
    test_market_data_flow(),
    
    %% Step 4: Test heartbeat
    test_heartbeat_monitoring(),
    
    %% Step 5: Clean shutdown
    test_clean_shutdown(),
    
    io:format("=== Integration Test Complete ===~n").

%% Test bridge startup with real connection
test_bridge_startup() ->
    io:format("1. Testing Bridge Startup with TWS Connection:~n"),
    
    Host = config:ib_host(),
    Port = config:ib_port(), 
    ClientId = config:ib_client_id(),
    
    io:format("   Connecting to ~s:~p with client ID ~p~n", [Host, Port, ClientId]),
    
    case ib_bridge_connector:start_connection(Host, Port, ClientId) of
        {ok, Pid} ->
            io:format("   ✓ Bridge started successfully (PID: ~p)~n", [Pid]);
        {error, Reason} ->
            io:format("   ✗ Bridge startup failed: ~p~n", [Reason]),
            throw({bridge_startup_failed, Reason})
    end.

%% Test connection establishment
test_connection_establishment() ->
    io:format("~n2. Testing Connection Establishment:~n"),
    
    %% Wait for connection with timeout
    MaxWait = 10000, % 10 seconds
    WaitStep = 1000,  % 1 second
    test_connection_with_timeout(MaxWait, WaitStep).

test_connection_with_timeout(0, _) ->
    io:format("   ✗ Connection timeout - TWS may not be running~n"),
    throw(connection_timeout);
test_connection_with_timeout(Remaining, Step) ->
    case ib_bridge_connector:get_connection_status() of
        {ok, true} ->
            io:format("   ✓ Connection established successfully~n");
        {ok, false} ->
            io:format("   ⏳ Waiting for connection... (~p ms remaining)~n", [Remaining]),
            timer:sleep(Step),
            test_connection_with_timeout(Remaining - Step, Step);
        {error, Reason} ->
            io:format("   ✗ Connection status error: ~p~n", [Reason]),
            throw({connection_status_error, Reason})
    end.

%% Test market data flow
test_market_data_flow() ->
    io:format("~n3. Testing Market Data Flow:~n"),
    
    Symbol = "EUR.USD",
    io:format("   Subscribing to market data for ~s~n", [Symbol]),
    
    case ib_bridge_connector:subscribe_market_data(Symbol, 1) of
        ok ->
            io:format("   ✓ Market data subscription sent~n"),
            io:format("   ⏳ Waiting for market ticks (check logs)...~n"),
            timer:sleep(5000), % Wait 5 seconds for ticks
            io:format("   ✓ Market data test completed (check bridge logs for ticks)~n");
        {error, Reason} ->
            io:format("   ✗ Market data subscription failed: ~p~n", [Reason])
    end.

%% Test heartbeat monitoring
test_heartbeat_monitoring() ->
    io:format("~n4. Testing Heartbeat Monitoring:~n"),
    
    io:format("   ⏳ Monitoring heartbeat for 10 seconds...~n"),
    timer:sleep(10000),
    
    case ib_bridge_connector:get_connection_status() of
        {ok, true} ->
            io:format("   ✓ Heartbeat working - connection still active~n");
        {ok, false} ->
            io:format("   ⚠ Connection lost during heartbeat test~n");
        {error, Reason} ->
            io:format("   ✗ Heartbeat test error: ~p~n", [Reason])
    end.

%% Test clean shutdown
test_clean_shutdown() ->
    io:format("~n5. Testing Clean Shutdown:~n"),
    
    case ib_bridge_connector:stop_connection() of
        ok ->
            io:format("   ✓ Bridge stopped cleanly~n");
        {error, Reason} ->
            io:format("   ⚠ Shutdown error: ~p~n", [Reason])
    end,
    
    %% Verify process is gone
    timer:sleep(1000),
    case whereis(ib_bridge_connector) of
        undefined ->
            io:format("   ✓ Bridge process cleaned up~n");
        Pid ->
            io:format("   ⚠ Bridge process still running: ~p~n", [Pid])
    end.

%% Quick integration test
quick_integration_test() ->
    io:format("Quick integration test with TWS...~n"),
    
    try
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        
        {ok, _} = ib_bridge_connector:start_connection(Host, Port, ClientId),
        timer:sleep(3000), % Wait for connection
        
        case ib_bridge_connector:get_connection_status() of
            {ok, true} ->
                io:format("✓ Integration test passed - bridge connected to TWS~n"),
                ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
                timer:sleep(2000), % Wait for tick
                io:format("✓ Market data subscription sent~n");
            {ok, false} ->
                io:format("⚠ Bridge started but not connected to TWS~n")
        end,
        
        ib_bridge_connector:stop_connection(),
        io:format("✓ Clean shutdown completed~n")
        
    catch
        _:Error ->
            io:format("✗ Integration test failed: ~p~n", [Error]),
            io:format("Make sure TWS is running with API enabled on port 7497~n")
    end.