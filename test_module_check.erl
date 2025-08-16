-module(test_module_check).
-compile(export_all).

check_module() ->
    io:format("=== Module Check ===~n"),
    
    %% Check if module is loaded
    Loaded = code:is_loaded(live_trading_integration),
    io:format("live_trading_integration loaded: ~p~n", [Loaded]),
    
    %% Check module info
    case code:get_object_code(live_trading_integration) of
        {live_trading_integration, Binary, Filename} ->
            io:format("Module binary size: ~p bytes~n", [byte_size(Binary)]),
            io:format("Module filename: ~p~n", [Filename]);
        error ->
            io:format("Module not found in code path~n")
    end,
    
    %% Try to get module functions
    try
        Functions = live_trading_integration:module_info(functions),
        io:format("Module functions: ~p~n", [Functions])
    catch
        _:_ ->
            io:format("Could not get module functions~n")
    end,
    
    %% Test specific function
    try
        Result = live_trading_integration:test_system_integration(),
        io:format("test_system_integration result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("test_system_integration error: ~p:~p~n", [Error, Reason])
    end,
    
    ok.
