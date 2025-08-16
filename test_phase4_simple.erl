-module(test_phase4_simple).
-compile(export_all).

run() ->
    io:format("~n=== PHASE 4 COMPREHENSIVE TESTING ===~n"),
    
    %% Load all modules using make:all()
    io:format("Loading all modules...~n"),
    make:all([load]),
    
    %% Compile our test modules
    io:format("Compiling test modules...~n"),
    case compile:file(test_phase4_comprehensive) of
        {ok, test_phase4_comprehensive} ->
            io:format("✓ test_phase4_comprehensive compiled~n");
        {error, Reason} ->
            io:format("✗ Failed to compile test_phase4_comprehensive: ~p~n", [Reason])
    end,
    
    case compile:file(run_phase4_tests) of
        {ok, run_phase4_tests} ->
            io:format("✓ run_phase4_tests compiled~n");
        {error, Reason} ->
            io:format("✗ Failed to compile run_phase4_tests: ~p~n", [Reason])
    end,
    
    %% Show available commands
    io:format("~n=== AVAILABLE TEST COMMANDS ===~n"),
    io:format("run_phase4_tests:quick()         - Quick test (5 min)~n"),
    io:format("run_phase4_tests:run()           - Full test (75 min)~n"),
    io:format("run_phase4_tests:test_level6a()  - Interface testing~n"),
    io:format("run_phase4_tests:test_level9()   - Risk management testing~n"),
    io:format("run_phase4_tests:test_level11()  - Performance monitoring~n"),
    io:format("run_phase4_tests:help()          - Show help~n"),
    io:format("~n"),
    
    %% Run a quick test
    io:format("Running quick test...~n"),
    try
        Result = run_phase4_tests:quick(),
        io:format("Quick test result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("Quick test failed: ~p:~p~n", [Error, Reason])
    end,
    
    io:format("~nPhase 4 test framework ready!~n").

%% Simple test function
test() ->
    io:format("~n=== SIMPLE PHASE 4 TEST ===~n"),
    io:format("Testing basic functionality...~n"),
    
    %% Test that modules can be loaded
    Modules = [live_trading_integration, live_trading_main, live_trader, live_scape, ib_bridge_connector],
    lists:foreach(fun(Module) ->
        case code:is_loaded(Module) of
            {file, _} -> io:format("✓ ~p loaded~n", [Module]);
            false -> io:format("✗ ~p not loaded~n", [Module])
        end
    end, Modules),
    
    io:format("~nBasic test completed!~n").
