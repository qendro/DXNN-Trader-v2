%% Phase 4 Integration Test - TEMPORARY FILE FOR DELETION
%% Tests complete API compatibility with original ib_connector.erl

-module(test_phase4_integration).
-compile(export_all).

%% Test complete API compatibility
test_api_compatibility() ->
    io:format("=== Phase 4 API Compatibility Test ===~n"),
    
    %% Test all exported functions exist and respond
    Functions = [
        {test_connectivity, 0},
        {test_handshake_detailed, 0},
        {start_connection, 3},
        {stop_connection, 0},
        {subscribe_market_data, 2},
        {unsubscribe_market_data, 1},
        {place_order, 4},
        {get_account_info, 0},
        {get_connection_status, 0},
        {get_market_data, 1},
        {get_ohlc_data, 2},
        {init_market_data_tables, 0},
        {cleanup_market_data_tables, 0},
        {get_pending_orders, 0},
        {get_order_confirmations, 0},
        {wait_for_order_confirmation, 2}
    ],
    
    lists:foreach(fun({Function, Arity}) ->
        case erlang:function_exported(ib_bridge_connector, Function, Arity) of
            true ->
                io:format("✓ ~s/~p exported~n", [Function, Arity]);
            false ->
                io:format("✗ ~s/~p missing~n", [Function, Arity])
        end
    end, Functions).

%% Test drop-in replacement functionality
test_drop_in_replacement() ->
    io:format("=== Drop-in Replacement Test ===~n"),
    
    try
        %% Test basic connectivity functions
        io:format("Testing connectivity functions...~n"),
        Result1 = ib_bridge_connector:test_connectivity(),
        io:format("test_connectivity: ~p~n", [Result1]),
        
        Result2 = ib_bridge_connector:test_handshake_detailed(),
        io:format("test_handshake_detailed: ~p~n", [Result2]),
        
        %% Test connection lifecycle
        io:format("Testing connection lifecycle...~n"),
        {ok, _} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101),
        timer:sleep(2000),
        
        %% Test all main functions
        {ok, Status} = ib_bridge_connector:get_connection_status(),
        io:format("Connection status: ~p~n", [Status]),
        
        {ok, AccountInfo} = ib_bridge_connector:get_account_info(),
        io:format("Account info: ~p~n", [AccountInfo]),
        
        ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
        io:format("Market data subscription: OK~n"),
        
        ok = ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
        io:format("Order placement: OK~n"),
        
        {ok, Orders} = ib_bridge_connector:get_pending_orders(),
        io:format("Pending orders: ~p~n", [Orders]),
        
        %% Clean up
        ok = ib_bridge_connector:stop_connection(),
        io:format("✓ Drop-in replacement test completed~n")
        
    catch
        _:Error ->
            io:format("✗ Drop-in replacement test failed: ~p~n", [Error])
    end.

%% Test compatibility with existing code patterns
test_existing_code_patterns() ->
    io:format("=== Testing Existing Code Patterns ===~n"),
    
    %% Test pattern 1: Connection with error handling
    case ib_bridge_connector:start_connection("host.docker.internal", 7497, 101) of
        {ok, Pid} ->
            io:format("✓ Connection pattern works, PID: ~p~n", [Pid]),
            
            %% Test pattern 2: Status checking
            case ib_bridge_connector:get_connection_status() of
                {ok, true} ->
                    io:format("✓ Status check pattern works~n");
                {ok, false} ->
                    io:format("⚠ Connected but status false~n");
                {error, Reason} ->
                    io:format("⚠ Status check error: ~p~n", [Reason])
            end,
            
            %% Test pattern 3: Market data subscription
            case ib_bridge_connector:subscribe_market_data("EUR.USD", 1) of
                ok ->
                    io:format("✓ Subscription pattern works~n");
                {error, Reason} ->
                    io:format("⚠ Subscription error: ~p~n", [Reason])
            end,
            
            %% Clean up
            ib_bridge_connector:stop_connection();
            
        {error, Reason} ->
            io:format("✗ Connection pattern failed: ~p~n", [Reason])
    end.

%% Quick Phase 4 validation
quick_phase4_test() ->
    io:format("Quick Phase 4 validation...~n"),
    
    %% Test API completeness
    RequiredFunctions = [start_connection, stop_connection, subscribe_market_data, 
                        place_order, get_connection_status],
    
    AllExported = lists:all(fun(F) ->
        erlang:function_exported(ib_bridge_connector, F, 
            case F of
                start_connection -> 3;
                subscribe_market_data -> 2;
                place_order -> 4;
                _ -> 0
            end)
    end, RequiredFunctions),
    
    case AllExported of
        true ->
            io:format("✓ Phase 4 API complete~n"),
            %% Test basic functionality
            try
                {ok, _} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101),
                timer:sleep(1000),
                ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
                ok = ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
                ib_bridge_connector:stop_connection(),
                io:format("✓ Phase 4 functionality working~n")
            catch
                _:Error ->
                    io:format("⚠ Phase 4 functionality issue: ~p~n", [Error])
            end;
        false ->
            io:format("✗ Phase 4 API incomplete~n")
    end.