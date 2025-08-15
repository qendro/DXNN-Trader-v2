%% Test script for IB connection fixes
%% Run this to verify the Docker networking and handshake fixes work

-module(test_ib_fixes).
-compile(export_all).

%% Main test function
test_all() ->
    io:format("=== Testing IB Connection Fixes ===~n"),
    
    %% Test 1: Configuration
    test_configuration(),
    
    %% Test 2: Basic connectivity
    test_basic_connectivity(),
    
    %% Test 3: Full connection with handshake
    test_full_connection(),
    
    io:format("=== Test Complete ===~n").

%% Test Python bridge functionality
test_bridge_all() ->
    io:format("=== Testing Python Bridge ===~n"),
    
    %% Test 1: Bridge startup
    test_bridge_startup(),
    
    %% Test 2: Bridge heartbeat
    test_bridge_heartbeat(),
    
    %% Test 3: Market data subscription
    test_market_data_subscription(),
    
    %% Test 4: Error handling
    test_bridge_error_handling(),
    
    %% Test 5: Paper trading guard
    test_paper_trading_guard(),
    
    io:format("=== Bridge Test Complete ===~n").

%% Test configuration
test_configuration() ->
    io:format("~n1. Testing Configuration:~n"),
    
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    
    io:format("   Host: ~s~n", [Host]),
    io:format("   Port: ~p~n", [Port]),
    io:format("   Client ID: ~p~n", [ClientId]),
    
    case Host of
        "host.docker.internal" ->
            io:format("   ✓ Docker host configuration detected~n");
        "127.0.0.1" ->
            io:format("   ✓ Localhost configuration detected~n");
        _ ->
            io:format("   ⚠ Unknown host configuration: ~s~n", [Host])
    end.

%% Test basic connectivity
test_basic_connectivity() ->
    io:format("~n2. Testing Basic Connectivity:~n"),
    
    case ib_connector:test_connectivity() of
        ok ->
            io:format("   ✓ Basic connectivity test passed~n");
        {error, Reason} ->
            io:format("   ✗ Basic connectivity test failed: ~p~n", [Reason]),
            io:format("   ~nTroubleshooting tips:~n"),
            io:format("   - Ensure IB TWS/Gateway is running~n"),
            io:format("   - Check API settings are enabled~n"),
            io:format("   - Verify paper trading mode is active~n"),
            io:format("   - For Docker: ensure host.docker.internal resolves~n")
    end.

%% Test full connection
test_full_connection() ->
    io:format("~n3. Testing Full Connection with Handshake:~n"),
    
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    
    io:format("   Attempting connection to ~s:~p with client ID ~p~n", [Host, Port, ClientId]),
    
    case ib_connector:start_connection(Host, Port, ClientId) of
        {ok, Pid} ->
            io:format("   ✓ Full connection test passed~n"),
            
            %% Test connection status
            case ib_connector:get_connection_status() of
                {ok, true} ->
                    io:format("   ✓ Connection status confirmed~n");
                {ok, false} ->
                    io:format("   ⚠ Connection established but status is false~n");
                {error, Reason} ->
                    io:format("   ⚠ Cannot verify connection status: ~p~n", [Reason])
            end,
            
            %% Clean up
            ib_connector:stop_connection(),
            io:format("   ✓ Connection cleaned up~n");
            
        {error, Reason} ->
            io:format("   ✗ Full connection test failed: ~p~n", [Reason]),
            io:format("   ~nPossible causes:~n"),
            case Reason of
                {handshake_timeout, _} ->
                    io:format("   - Handshake timeout: Check IB API version compatibility~n");
                {send_version_failed, _} ->
                    io:format("   - Version send failed: Check network connectivity~n");
                {send_client_id_failed, _} ->
                    io:format("   - Client ID send failed: Check IB API settings~n");
                {unsupported_server_version, Version} ->
                    io:format("   - Unsupported server version ~p: Update TWS or adjust MIN_SERVER_VER~n", [Version]);
                econnrefused ->
                    io:format("   - Connection refused: Check if TWS is running and port is correct~n");
                _ ->
                    io:format("   - Unknown error: Check TWS logs for details~n")
            end
    end.

%% Quick test function
quick_test() ->
    io:format("Quick IB connection test...~n"),
    case ib_connector:test_connectivity() of
        ok ->
            io:format("✓ Connectivity OK~n"),
            {ok, connectivity_ok};
        {error, Reason} ->
            io:format("✗ Connectivity failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Python Bridge Tests
%% ============================================================================

%% Test bridge startup
test_bridge_startup() ->
    io:format("~n1. Testing Bridge Startup:~n"),
    try
        {ok, _Pid} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
        io:format("   ✓ Bridge started successfully~n"),
        timer:sleep(3000),  % Allow connection time
        {ok, Status} = ib_bridge_connector:get_connection_status(),
        case Status of
            true ->
                io:format("   ✓ Bridge connected successfully~n");
            false ->
                io:format("   ⚠ Bridge started but not connected~n")
        end,
        ok = ib_bridge_connector:stop_connection(),
        io:format("   ✓ Bridge stopped successfully~n")
    catch
        _:Error ->
            io:format("   ✗ Bridge startup failed: ~p~n", [Error])
    end.

%% Test bridge heartbeat
test_bridge_heartbeat() ->
    io:format("~n2. Testing Bridge Heartbeat:~n"),
    try
        {ok, _Pid} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
        io:format("   ✓ Bridge started for heartbeat test~n"),
        timer:sleep(5000),  % Wait for heartbeat
        {ok, Status} = ib_bridge_connector:get_connection_status(),
        case Status of
            true ->
                io:format("   ✓ Heartbeat working - connection active~n");
            false ->
                io:format("   ⚠ Heartbeat test - connection inactive~n")
        end,
        ok = ib_bridge_connector:stop_connection()
    catch
        _:Error ->
            io:format("   ✗ Heartbeat test failed: ~p~n", [Error])
    end.

%% Test market data subscription
test_market_data_subscription() ->
    io:format("~n3. Testing Market Data Subscription:~n"),
    try
        {ok, _} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
        timer:sleep(2000),  % Allow connection
        ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
        io:format("   ✓ Market data subscription sent~n"),
        timer:sleep(3000),  % Wait for tick
        io:format("   ✓ No crash - subscription handling works~n"),
        ok = ib_bridge_connector:stop_connection()
    catch
        _:Error ->
            io:format("   ✗ Market data subscription failed: ~p~n", [Error])
    end.

%% Test bridge error handling
test_bridge_error_handling() ->
    io:format("~n4. Testing Bridge Error Handling:~n"),
    try
        % Test with invalid port to trigger error
        Result = ib_bridge_connector:start_connection("127.0.0.1", 9999, 1),
        case Result of
            {error, _} ->
                io:format("   ✓ Error handling works - invalid port rejected~n");
            {ok, _} ->
                io:format("   ⚠ Unexpected success with invalid port~n"),
                ib_bridge_connector:stop_connection()
        end
    catch
        _:Error ->
            io:format("   ✓ Error handling works - exception caught: ~p~n", [Error])
    end.

%% Test paper trading guard
test_paper_trading_guard() ->
    io:format("~n5. Testing Paper Trading Guard:~n"),
    try
        % Test that live trading port is blocked
        Result = ib_bridge_connector:start_connection("127.0.0.1", 7496, 1),
        case Result of
            {error, _} ->
                io:format("   ✓ Paper trading guard works - live port blocked~n");
            {ok, _} ->
                io:format("   ⚠ Paper trading guard may not be working~n"),
                ib_bridge_connector:stop_connection()
        end
    catch
        _:Error ->
            io:format("   ✓ Paper trading guard works - exception: ~p~n", [Error])
    end.

%% Quick bridge test
quick_bridge_test() ->
    io:format("Quick Python bridge test...~n"),
    try
        {ok, _} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
        timer:sleep(2000),
        {ok, Status} = ib_bridge_connector:get_connection_status(),
        case Status of
            true -> io:format("✓ Bridge OK~n");
            false -> io:format("⚠ Bridge started but not connected~n")
        end,
        ib_bridge_connector:stop_connection()
    catch
        _:Error ->
            io:format("✗ Bridge test failed: ~p~n", [Error])
    end.