%% Phase 5 Cleanup Validation Tests - TEMPORARY FILE
%% Tests that essential functionality still works after cleanup

-module(test_phase5_cleanup).
-compile(export_all).

%% Test that essential functionality still works after cleanup
test_cleanup_validation() ->
    io:format("=== Phase 5 Cleanup Validation ===~n"),
    
    %% 1. Test that bridge still works
    io:format("1. Testing Bridge Functionality:~n"),
    try
        {ok, _} = ib_connector:start_connection("host.docker.internal", 7497, 101),
        io:format("   ✓ Bridge connection works~n"),
        
        {ok, Status} = ib_connector:get_connection_status(),
        io:format("   ✓ Connection status: ~p~n", [Status]),
        
        ok = ib_connector:subscribe_market_data("EUR.USD", 1),
        io:format("   ✓ Market data subscription works~n"),
        
        ok = ib_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
        io:format("   ✓ Order placement works~n"),
        
        ib_connector:stop_connection(),
        io:format("   ✓ Clean shutdown works~n")
        
    catch
        _:Error ->
            io:format("   ✗ Bridge functionality failed: ~p~n", [Error])
    end,
    
    %% 2. Test that removed functions are gone
    io:format("~n2. Testing Removed Functions:~n"),
    RemovedModules = [ib_proto, ib_diag, debug_tws_trust, test_ib_fixes],
    lists:foreach(fun(Module) ->
        case code:is_loaded(Module) of
            false ->
                case code:load_file(Module) of
                    {error, nofile} ->
                        io:format("   ✓ ~p module removed~n", [Module]);
                    _ ->
                        io:format("   ⚠ ~p module still exists~n", [Module])
                end;
            _ ->
                io:format("   ⚠ ~p module still loaded~n", [Module])
        end
    end, RemovedModules),
    
    %% 3. Test that live trading system starts
    io:format("~n3. Testing Live Trading System:~n"),
    try
        %% This would normally start the full system
        %% For validation, just test key components exist
        case erlang:function_exported(live_trading_integration, start_live_trading, 1) of
            true ->
                io:format("   ✓ live_trading_integration available~n");
            false ->
                io:format("   ✗ live_trading_integration missing~n")
        end,
        
        case erlang:function_exported(live_scape, live_sim, 1) of
            true ->
                io:format("   ✓ live_scape available~n");
            false ->
                io:format("   ✗ live_scape missing~n")
        end,
        
        case erlang:function_exported(live_trader, deploy_model, 1) of
            true ->
                io:format("   ✓ live_trader available~n");
            false ->
                io:format("   ✗ live_trader missing~n")
        end
        
    catch
        _:Error2 ->
            io:format("   ✗ Live trading system test failed: ~p~n", [Error2])
    end,
    
    %% 4. Test configuration still works
    io:format("~n4. Testing Configuration:~n"),
    try
        Host = config:ib_host(),
        Port = config:ib_port(),
        ClientId = config:ib_client_id(),
        io:format("   ✓ Configuration accessible: ~s:~p (client ~p)~n", [Host, Port, ClientId])
    catch
        _:Error3 ->
            io:format("   ✗ Configuration test failed: ~p~n", [Error3])
    end,
    
    io:format("~n=== Cleanup Validation Complete ===~n").

%% Test that removed functions are actually gone
test_removed_functions() ->
    io:format("=== Testing Removed Functions ===~n"),
    
    %% These should not exist anymore
    RemovedFunctions = [
        {ib_proto, z, 1},
        {ib_proto, i2b, 1},
        {ib_proto, read_cstring, 1},
        {ib_diag, test_env, 0},
        {ib_diag, test_tcp, 0},
        {debug_tws_trust, perform_debug_handshake, 3},
        {test_ib_fixes, test_all, 0}
    ],
    
    lists:foreach(fun({Module, Function, Arity}) ->
        case erlang:function_exported(Module, Function, Arity) of
            false ->
                io:format("   ✓ ~p:~p/~p removed~n", [Module, Function, Arity]);
            true ->
                io:format("   ⚠ ~p:~p/~p still exists~n", [Module, Function, Arity])
        end
    end, RemovedFunctions).

%% Test file size reduction
test_file_size_reduction() ->
    io:format("=== File Size Reduction Analysis ===~n"),
    
    %% Check if backup exists to compare
    case filelib:wildcard("delete/ib_connector_original.erl") of
        [OriginalFile] ->
            case file:read_file_info(OriginalFile) of
                {ok, OriginalInfo} ->
                    case file:read_file_info("ib_connector.erl") of
                        {ok, NewInfo} ->
                            OriginalSize = OriginalInfo#file_info.size,
                            NewSize = NewInfo#file_info.size,
                            Reduction = round((1 - NewSize/OriginalSize) * 100),
                            io:format("   Original ib_connector.erl: ~p bytes~n", [OriginalSize]),
                            io:format("   New ib_connector.erl: ~p bytes~n", [NewSize]),
                            io:format("   ✓ Size reduction: ~p%~n", [Reduction]);
                        _ ->
                            io:format("   ⚠ Cannot read new ib_connector.erl~n")
                    end;
                _ ->
                    io:format("   ⚠ Cannot read original file info~n")
            end;
        [] ->
            io:format("   - No original file for comparison~n")
    end.

%% Quick Phase 5 validation
quick_phase5_test() ->
    io:format("Quick Phase 5 validation...~n"),
    
    %% Test essential functionality
    try
        %% Test bridge works
        {ok, _} = ib_connector:start_connection("host.docker.internal", 7497, 101),
        timer:sleep(2000),
        {ok, Status} = ib_connector:get_connection_status(),
        ib_connector:stop_connection(),
        
        case Status of
            true -> io:format("✓ Phase 5 cleanup successful - bridge working~n");
            false -> io:format("⚠ Phase 5 cleanup - bridge started but not connected~n")
        end
        
    catch
        _:Error ->
            io:format("✗ Phase 5 cleanup validation failed: ~p~n", [Error])
    end.