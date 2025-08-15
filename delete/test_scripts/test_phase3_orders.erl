%% Phase 3 Order Testing - TEMPORARY FILE FOR DELETION
%% This file tests order placement functionality and should be deleted after testing

-module(test_phase3_orders).
-compile(export_all).

%% Test order placement functionality
test_order_placement() ->
    io:format("=== Phase 3 Order Placement Test ===~n"),
    io:format("⚠️  PAPER TRADING ONLY - No real money at risk~n~n"),
    
    try
        %% Start connection
        {ok, _} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101),
        timer:sleep(3000),
        
        %% Test small paper trade
        io:format("Testing small paper order...~n"),
        Result = ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
        
        case Result of
            ok ->
                io:format("✓ Order placement API works~n"),
                timer:sleep(2000),  % Wait for confirmation
                io:format("Check logs for order confirmation~n");
            {error, Reason} ->
                io:format("⚠ Order placement failed: ~p~n", [Reason])
        end,
        
        %% Clean up
        ib_bridge_connector:stop_connection(),
        io:format("✓ Test completed~n")
        
    catch
        _:Error ->
            io:format("✗ Test failed: ~p~n", [Error])
    end.

%% Test order validation
test_order_validation() ->
    io:format("=== Testing Order Validation ===~n"),
    
    try
        {ok, _} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101),
        timer:sleep(2000),
        
        %% Test invalid parameters
        io:format("Testing invalid order parameters...~n"),
        
        %% This should fail gracefully
        Result1 = ib_bridge_connector:place_order("", "BUY", 1000, "MKT"),
        io:format("Empty symbol result: ~p~n", [Result1]),
        
        %% Clean up
        ib_bridge_connector:stop_connection()
        
    catch
        _:Error ->
            io:format("Validation test error: ~p~n", [Error])
    end.

%% Quick order test
quick_order_test() ->
    io:format("Quick Phase 3 order test...~n"),
    
    try
        {ok, _} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101),
        timer:sleep(2000),
        
        %% Test API exists and responds
        Result = ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
        case Result of
            ok -> io:format("✓ Order API working~n");
            {error, Reason} -> io:format("⚠ Order API issue: ~p~n", [Reason])
        end,
        
        ib_bridge_connector:stop_connection()
        
    catch
        _:Error ->
            io:format("✗ Quick test failed: ~p~n", [Error])
    end.