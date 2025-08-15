%% Simple Phase 4 Test - TEMPORARY FILE
-module(simple_phase4_test).
-compile(export_all).

test_all() ->
    io:format("=== Phase 4 Complete Integration Test ===~n"),
    io:format("Testing with live TWS connection...~n~n"),
    
    %% Step 1: Test API completeness
    io:format("1. Testing API Completeness:~n"),
    Functions = [start_connection, stop_connection, subscribe_market_data, 
                place_order, get_connection_status, get_account_info],
    lists:foreach(fun(F) ->
        Arity = case F of
            start_connection -> 3;
            subscribe_market_data -> 2;
            place_order -> 4;
            _ -> 0
        end,
        case erlang:function_exported(ib_bridge_connector, F, Arity) of
            true -> io:format("   ✓ ~s/~p available~n", [F, Arity]);
            false -> io:format("   ✗ ~s/~p missing~n", [F, Arity])
        end
    end, Functions),
    
    %% Step 2: Test connection
    io:format("~n2. Testing Connection:~n"),
    case ib_bridge_connector:start_connection("host.docker.internal", 7497, 101) of
        {ok, Pid} ->
            io:format("   ✓ Connection started, PID: ~p~n", [Pid]),
            timer:sleep(3000),
            
            %% Step 3: Test status
            io:format("~n3. Testing Status Check:~n"),
            case ib_bridge_connector:get_connection_status() of
                {ok, Status} ->
                    io:format("   ✓ Status check works: ~p~n", [Status]);
                Error ->
                    io:format("   ⚠ Status check issue: ~p~n", [Error])
            end,
            
            %% Step 4: Test account info
            io:format("~n4. Testing Account Info:~n"),
            case ib_bridge_connector:get_account_info() of
                {ok, AccountInfo} ->
                    io:format("   ✓ Account info: ~p~n", [AccountInfo]);
                Error2 ->
                    io:format("   ⚠ Account info issue: ~p~n", [Error2])
            end,
            
            %% Step 5: Test market data
            io:format("~n5. Testing Market Data Subscription:~n"),
            case ib_bridge_connector:subscribe_market_data("EUR.USD", 1) of
                ok ->
                    io:format("   ✓ Market data subscription sent~n"),
                    io:format("   ⏳ Watch for tick data in logs...~n"),
                    timer:sleep(5000);
                Error3 ->
                    io:format("   ⚠ Market data issue: ~p~n", [Error3])
            end,
            
            %% Step 6: Test order placement
            io:format("~n6. Testing Order Placement (Paper Trading):~n"),
            case ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT") of
                ok ->
                    io:format("   ✓ Order placement sent~n"),
                    io:format("   📊 CHECK TWS: Look for BUY 1000 EUR.USD order~n"),
                    timer:sleep(3000);
                Error4 ->
                    io:format("   ⚠ Order placement issue: ~p~n", [Error4])
            end,
            
            %% Step 7: Test another order
            io:format("~n7. Testing Second Order (Different Size):~n"),
            case ib_bridge_connector:place_order("GBP.USD", "SELL", 500, "MKT") of
                ok ->
                    io:format("   ✓ Second order placement sent~n"),
                    io:format("   📊 CHECK TWS: Look for SELL 500 GBP.USD order~n"),
                    timer:sleep(3000);
                Error5 ->
                    io:format("   ⚠ Second order issue: ~p~n", [Error5])
            end,
            
            %% Step 8: Clean shutdown
            io:format("~n8. Testing Clean Shutdown:~n"),
            case ib_bridge_connector:stop_connection() of
                ok ->
                    io:format("   ✓ Clean shutdown completed~n");
                Error6 ->
                    io:format("   ⚠ Shutdown issue: ~p~n", [Error6])
            end;
            
        {error, Reason} ->
            io:format("   ✗ Connection failed: ~p~n", [Reason])
    end,
    
    io:format("~n=== Phase 4 Test Complete ===~n"),
    io:format("📊 Please confirm in TWS:~n"),
    io:format("   - BUY 1000 EUR.USD market order~n"),
    io:format("   - SELL 500 GBP.USD market order~n"),
    io:format("   - Both should be paper trading orders~n").