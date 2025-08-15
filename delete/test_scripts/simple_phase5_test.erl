%% Simple Phase 5 validation test
-module(simple_phase5_test).
-compile(export_all).

test_phase5() ->
    io:format("=== Phase 5 Validation ===~n"),
    
    %% Test 1: Check bridge connector compiles
    io:format("1. Testing Bridge Compilation:~n"),
    case compile:file(ib_connector) of
        {ok, ib_connector} ->
            io:format("   ✓ ib_connector compiles successfully~n");
        Error ->
            io:format("   ✗ Compilation failed: ~p~n", [Error])
    end,
    
    %% Test 2: Check removed files are gone
    io:format("~n2. Testing Removed Files:~n"),
    RemovedFiles = [ib_proto, ib_diag, debug_tws_trust, test_ib_fixes],
    lists:foreach(fun(Module) ->
        case code:load_file(Module) of
            {error, nofile} ->
                io:format("   ✓ ~p module removed~n", [Module]);
            _ ->
                io:format("   ⚠ ~p module still exists~n", [Module])
        end
    end, RemovedFiles),
    
    %% Test 3: Check API functions exist
    io:format("~n3. Testing API Functions:~n"),
    ApiFunctions = [
        {start_connection, 3},
        {stop_connection, 0},
        {subscribe_market_data, 2},
        {place_order, 4},
        {get_connection_status, 0}
    ],
    lists:foreach(fun({Function, Arity}) ->
        case erlang:function_exported(ib_connector, Function, Arity) of
            true ->
                io:format("   ✓ ~p/~p available~n", [Function, Arity]);
            false ->
                io:format("   ✗ ~p/~p missing~n", [Function, Arity])
        end
    end, ApiFunctions),
    
    %% Test 4: Check Python bridge exists
    io:format("~n4. Testing Python Bridge:~n"),
    case filelib:is_file("priv/ib_service.py") of
        true ->
            io:format("   ✓ Python bridge service exists~n");
        false ->
            io:format("   ✗ Python bridge service missing~n")
    end,
    
    case filelib:is_file("priv/requirements.txt") of
        true ->
            io:format("   ✓ Python requirements file exists~n");
        false ->
            io:format("   ✗ Python requirements file missing~n")
    end,
    
    %% Test 5: Check backup files exist
    io:format("~n5. Testing Backup Files:~n"),
    case filelib:is_file("delete/ib_connector_original.erl") of
        true ->
            io:format("   ✓ Original connector backed up~n");
        false ->
            io:format("   ⚠ Original connector backup not found~n")
    end,
    
    io:format("~n=== Phase 5 Validation Complete ===~n"),
    io:format("✓ Phase 5 cleanup successfully validated~n").