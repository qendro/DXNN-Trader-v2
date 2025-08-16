-module(run_phase4_tests).
-compile(export_all).

%% Simple test runner for Phase 4 comprehensive testing
%% Usage: run_phase4_tests:run() or run_phase4_tests:quick()

%% Main test runner
run() ->
    io:format("~n=== RUNNING PHASE 4 COMPREHENSIVE TESTS ===~n"),
    io:format("This will run all comprehensive tests for the live trading system~n"),
    io:format("Expected duration: 75 minutes~n~n"),
    
    %% Load the test module
    case code:ensure_loaded(test_phase4_comprehensive) of
        {module, test_phase4_comprehensive} ->
            io:format("✓ Test module loaded successfully~n");
        {error, Reason1} ->
            io:format("✗ Failed to load test module: ~p~n", [Reason1]),
            {error, module_load_failed}
    end,
    
    %% Run the comprehensive tests
    try
        test_phase4_comprehensive:run_phase4_comprehensive()
    catch
        Error:Reason2:_Stack ->
            io:format("✗ Phase 4 tests failed: ~p:~p~n", [Error, Reason2]),
            {error, {phase4_failed, Error, Reason2}}
    end.

%% Quick test runner for development
quick() ->
    io:format("~n=== RUNNING QUICK PHASE 4 TEST ===~n"),
    io:format("This will run a quick subset of tests for development~n"),
    io:format("Expected duration: 5 minutes~n~n"),
    
    %% Load the test module
    case code:ensure_loaded(test_phase4_comprehensive) of
        {module, test_phase4_comprehensive} ->
            io:format("✓ Test module loaded successfully~n");
        {error, Reason3} ->
            io:format("✗ Failed to load test module: ~p~n", [Reason3]),
            {error, module_load_failed}
    end,
    
    %% Run the quick tests
    try
        test_phase4_comprehensive:quick_phase4_test()
    catch
        Error:Reason4:_Stack ->
            io:format("✗ Quick Phase 4 test failed: ~p:~p~n", [Error, Reason4]),
            {error, {quick_phase4_failed, Error, Reason4}}
    end.

%% Test individual levels
test_level6a() ->
    io:format("~n=== TESTING LEVEL 6A: INTERFACE TESTING ===~n"),
    test_phase4_comprehensive:run_level6a_tests().

test_level6b() ->
    io:format("~n=== TESTING LEVEL 6B: ADVANCED INTERFACE TESTING ===~n"),
    test_phase4_comprehensive:run_level6b_tests().

test_level7a() ->
    io:format("~n=== TESTING LEVEL 7A: BASIC E2E TESTING ===~n"),
    test_phase4_comprehensive:run_level7a_tests().

test_level8() ->
    io:format("~n=== TESTING LEVEL 8: STRESS & PERFORMANCE TESTING ===~n"),
    test_phase4_comprehensive:run_level8_tests().

test_level9() ->
    io:format("~n=== TESTING LEVEL 9: RISK MANAGEMENT TESTING ===~n"),
    test_phase4_comprehensive:run_level9_tests().

test_level10() ->
    io:format("~n=== TESTING LEVEL 10: DATA PROCESSING TESTING ===~n"),
    test_phase4_comprehensive:run_level10_tests().

test_level11() ->
    io:format("~n=== TESTING LEVEL 11: PERFORMANCE MONITORING TESTING ===~n"),
    test_phase4_comprehensive:run_level11_tests().

%% Help function
help() ->
    io:format("~n=== PHASE 4 TEST RUNNER HELP ===~n"),
    io:format("Available commands:~n"),
    io:format("  run()           - Run all comprehensive Phase 4 tests (75 min)~n"),
    io:format("  quick()         - Run quick Phase 4 test for development (5 min)~n"),
    io:format("  test_level6a()  - Test Level 6A: Interface Testing~n"),
    io:format("  test_level6b()  - Test Level 6B: Advanced Interface Testing~n"),
    io:format("  test_level7a()  - Test Level 7A: Basic E2E Testing~n"),
    io:format("  test_level8()   - Test Level 8: Stress & Performance Testing~n"),
    io:format("  test_level9()   - Test Level 9: Risk Management Testing~n"),
    io:format("  test_level10()  - Test Level 10: Data Processing Testing~n"),
    io:format("  test_level11()  - Test Level 11: Performance Monitoring Testing~n"),
    io:format("  help()          - Show this help message~n"),
    io:format("~nExample usage:~n"),
    io:format("  run_phase4_tests:quick().    % Quick test for development~n"),
    io:format("  run_phase4_tests:run().      % Full comprehensive test~n"),
    io:format("  run_phase4_tests:test_level9(). % Test risk management only~n"),
    io:format("~n").
