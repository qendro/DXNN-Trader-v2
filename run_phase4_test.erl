-module(run_phase4_test).
-compile(export_all).

run() ->
    io:format("~n=== RUNNING PHASE 4 COMPREHENSIVE TESTS ===~n"),
    
    %% Compile the test modules
    case compile:file(test_phase4_comprehensive) of
        {ok, test_phase4_comprehensive} ->
            io:format("✓ test_phase4_comprehensive compiled successfully~n");
        {error, Reason1} ->
            io:format("✗ Failed to compile test_phase4_comprehensive: ~p~n", [Reason1]),
            halt(1)
    end,
    
    case compile:file(run_phase4_tests) of
        {ok, run_phase4_tests} ->
            io:format("✓ run_phase4_tests compiled successfully~n");
        {error, Reason2} ->
            io:format("✗ Failed to compile run_phase4_tests: ~p~n", [Reason2]),
            halt(1)
    end,
    
    %% Show help
    io:format("~n=== PHASE 4 TEST HELP ===~n"),
    io:format("Available commands:~n"),
    io:format("  run_phase4_tests:quick()         - Quick test for development (5 min)~n"),
    io:format("  run_phase4_tests:run()           - Full comprehensive test (75 min)~n"),
    io:format("  run_phase4_tests:test_level6a()  - Test Level 6A: Interface Testing~n"),
    io:format("  run_phase4_tests:test_level9()   - Test Level 9: Risk Management Testing~n"),
    io:format("  run_phase4_tests:test_level11()  - Test Level 11: Performance Monitoring~n"),
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
    
    io:format("~nPhase 4 test framework ready!~n"),
    io:format("Use the commands above to run specific tests.~n").
