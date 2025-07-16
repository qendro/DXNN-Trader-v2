%% Test runner script for all best agent runner integration tests
%% This script runs all test suites and provides a comprehensive report

-module(run_integration_tests).
-compile(export_all).

%% ============================================================================
%% Main Test Runner
%% ============================================================================

%% Run all integration test suites
run_all() ->
    io:format("~n" ++ string:copies("=", 80) ++ "~n"),
    io:format("BEST AGENT RUNNER - COMPREHENSIVE INTEGRATION TEST SUITE~n"),
    io:format(string:copies("=", 80) ++ "~n"),
    
    StartTime = erlang:timestamp(),
    
    % Run all test suites
    TestSuites = [
        {"Unit Tests", fun() -> best_agent_runner_tests:run_all_tests() end},
        {"Integration Tests", fun() -> best_agent_runner_integration_tests:run_all_tests() end},
        {"End-to-End Tests", fun() -> best_agent_runner_e2e_tests:run_all_e2e_tests() end},
        {"Backward Compatibility Tests", fun() -> best_agent_runner_backward_compatibility_tests:run_all_compatibility_tests() end},
        {"Error Handling Tests", fun() -> best_agent_runner_error_tests:run_all_tests() end},
        {"Format Tests", fun() -> best_agent_runner_format_tests:run_all_tests() end},
        {"Clean Tests", fun() -> best_agent_runner_clean_tests:run_all_tests() end}
    ],
    
    % Execute all test suites
    Results = execute_test_suites(TestSuites),
    
    EndTime = erlang:timestamp(),
    TotalTime = timer:now_diff(EndTime, StartTime) / 1000000,
    
    % Generate comprehensive report
    generate_comprehensive_report(Results, TotalTime),
    
    % Return overall result
    OverallResult = determine_overall_result(Results),
    
    io:format("~n" ++ string:copies("=", 80) ++ "~n"),
    case OverallResult of
        pass ->
            io:format("🎉 ALL INTEGRATION TESTS PASSED! 🎉~n");
        fail ->
            io:format("❌ SOME INTEGRATION TESTS FAILED ❌~n")
    end,
    io:format(string:copies("=", 80) ++ "~n~n"),
    
    OverallResult.

%% Run specific test suite
run_suite(SuiteName) ->
    io:format("~n=== Running ~s ===~n", [SuiteName]),
    
    case SuiteName of
        "unit" -> best_agent_runner_tests:run_all_tests();
        "integration" -> best_agent_runner_integration_tests:run_all_tests();
        "e2e" -> best_agent_runner_e2e_tests:run_all_e2e_tests();
        "compatibility" -> best_agent_runner_backward_compatibility_tests:run_all_compatibility_tests();
        "error" -> best_agent_runner_error_tests:run_all_tests();
        "format" -> best_agent_runner_format_tests:run_all_tests();
        "clean" -> best_agent_runner_clean_tests:run_all_tests();
        _ ->
            io:format("Unknown test suite: ~s~n", [SuiteName]),
            io:format("Available suites: unit, integration, e2e, compatibility, error, format, clean~n"),
            fail
    end.

%% Quick smoke test - runs essential tests only
run_smoke_tests() ->
    io:format("~n=== SMOKE TESTS (Essential Tests Only) ===~n"),
    
    SmokeTests = [
        {"Basic Unit Tests", fun() -> run_basic_unit_tests() end},
        {"Core Integration Tests", fun() -> run_core_integration_tests() end},
        {"Backward Compatibility Check", fun() -> run_compatibility_check() end}
    ],
    
    Results = execute_test_suites(SmokeTests),
    
    case determine_overall_result(Results) of
        pass ->
            io:format("✅ Smoke tests passed - system appears functional~n"),
            pass;
        fail ->
            io:format("❌ Smoke tests failed - system has issues~n"),
            fail
    end.

%% ============================================================================
%% Test Suite Execution
%% ============================================================================

%% Execute all test suites and collect results
execute_test_suites(TestSuites) ->
    lists:map(fun({Name, TestFun}) ->
        io:format("~n" ++ string:copies("-", 60) ++ "~n"),
        io:format("Executing: ~s~n", [Name]),
        io:format(string:copies("-", 60) ++ "~n"),
        
        StartTime = erlang:timestamp(),
        
        Result = try
            TestFun()
        catch
            Error:Reason ->
                io:format("Test suite ~s crashed: ~p:~p~n", [Name, Error, Reason]),
                fail
        end,
        
        EndTime = erlang:timestamp(),
        Duration = timer:now_diff(EndTime, StartTime) / 1000000,
        
        {Name, Result, Duration}
    end, TestSuites).

%% ============================================================================
%% Smoke Test Implementations
%% ============================================================================

%% Run basic unit tests (subset)
run_basic_unit_tests() ->
    try
        % Test core function existence
        Functions = [
            {best_agent_runner, run_best_agent_on_data, 1},
            {best_agent_runner, run_best_agent_on_data, 2},
            {best_agent_runner, find_best_agent, 0}
        ],
        
        FunctionResults = [
            case erlang:function_exported(Module, Function, Arity) of
                true -> pass;
                false -> fail
            end || {Module, Function, Arity} <- Functions
        ],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("✅ Core functions exist~n"),
                pass;
            false ->
                io:format("❌ Some core functions missing~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("❌ Basic unit tests failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Run core integration tests (subset)
run_core_integration_tests() ->
    try
        % Test that we can create a simple data file and attempt to process it
        TestFile = "smoke_test_data.csv",
        TestData = "Timestamp,Open,High,Low,Close,Volume\n"
                  "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n",
        
        file:write_file(TestFile, TestData),
        
        % Attempt to run (expected to fail gracefully due to no agents)
        Result = case catch best_agent_runner:run_best_agent_on_data(TestFile) of
            {'EXIT', _} -> fail;  % Should not crash
            {error, _} -> pass;   % Expected error is OK
            {ok, _} -> pass;      % Unexpected success is also OK
            _ -> pass
        end,
        
        file:delete(TestFile),
        
        case Result of
            pass ->
                io:format("✅ Core integration test passed~n"),
                pass;
            fail ->
                io:format("❌ Core integration test failed~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("❌ Core integration tests failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Run compatibility check (subset)
run_compatibility_check() ->
    try
        % Check that key existing modules are still accessible
        Modules = [genotype_utils, fx, exoself],
        
        ModuleResults = [
            case code:is_loaded(Module) of
                {file, _} -> pass;  % Module is loaded
                false ->
                    case code:load_file(Module) of
                        {module, _} -> pass;  % Module can be loaded
                        _ -> fail
                    end
            end || Module <- Modules
        ],
        
        case lists:all(fun(R) -> R =:= pass end, ModuleResults) of
            true ->
                io:format("✅ Key modules accessible~n"),
                pass;
            false ->
                io:format("❌ Some key modules inaccessible~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("❌ Compatibility check failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% Reporting Functions
%% ============================================================================

%% Generate comprehensive test report
generate_comprehensive_report(Results, TotalTime) ->
    io:format("~n" ++ string:copies("=", 80) ++ "~n"),
    io:format("COMPREHENSIVE TEST REPORT~n"),
    io:format(string:copies("=", 80) ++ "~n"),
    
    TotalSuites = length(Results),
    PassedSuites = length([R || {_, Result, _} <- Results, Result =:= pass]),
    FailedSuites = TotalSuites - PassedSuites,
    
    io:format("~nOVERALL SUMMARY:~n"),
    io:format("  Total Test Suites: ~p~n", [TotalSuites]),
    io:format("  Passed Suites: ~p~n", [PassedSuites]),
    io:format("  Failed Suites: ~p~n", [FailedSuites]),
    io:format("  Success Rate: ~.1f%~n", [PassedSuites / TotalSuites * 100]),
    io:format("  Total Execution Time: ~.2f seconds~n", [TotalTime]),
    
    io:format("~nDETAILED RESULTS:~n"),
    lists:foreach(fun({Name, Result, Duration}) ->
        Status = case Result of
            pass -> "✅ PASS";
            fail -> "❌ FAIL";
            _ -> "⚠️  UNKNOWN"
        end,
        io:format("  ~s: ~s (~.2f seconds)~n", [Name, Status, Duration])
    end, Results),
    
    % Performance analysis
    io:format("~nPERFORMANCE ANALYSIS:~n"),
    SlowestSuite = lists:max([{Duration, Name} || {Name, _, Duration} <- Results]),
    FastestSuite = lists:min([{Duration, Name} || {Name, _, Duration} <- Results]),
    AvgDuration = lists:sum([Duration || {_, _, Duration} <- Results]) / TotalSuites,
    
    io:format("  Slowest Suite: ~s (~.2f seconds)~n", [element(2, SlowestSuite), element(1, SlowestSuite)]),
    io:format("  Fastest Suite: ~s (~.2f seconds)~n", [element(2, FastestSuite), element(1, FastestSuite)]),
    io:format("  Average Duration: ~.2f seconds~n", [AvgDuration]),
    
    % Recommendations
    generate_recommendations(Results),
    
    ok.

%% Generate recommendations based on test results
generate_recommendations(Results) ->
    io:format("~nRECOMMENDATIONS:~n"),
    
    FailedSuites = [Name || {Name, Result, _} <- Results, Result =:= fail],
    
    case FailedSuites of
        [] ->
            io:format("  🎉 All tests passed! The system is ready for production use.~n"),
            io:format("  💡 Consider running performance benchmarks on production data.~n");
        _ ->
            io:format("  ⚠️  The following test suites failed:~n"),
            lists:foreach(fun(Suite) ->
                io:format("    - ~s~n", [Suite])
            end, FailedSuites),
            io:format("  🔧 Please review and fix the failing tests before deployment.~n"),
            io:format("  📋 Check the detailed test output above for specific failure reasons.~n")
    end,
    
    % Performance recommendations
    SlowSuites = [{Name, Duration} || {Name, _, Duration} <- Results, Duration > 10.0],
    case SlowSuites of
        [] ->
            io:format("  ⚡ All test suites completed in reasonable time.~n");
        _ ->
            io:format("  🐌 The following test suites are slow (>10 seconds):~n"),
            lists:foreach(fun({Suite, Duration}) ->
                io:format("    - ~s (~.2f seconds)~n", [Suite, Duration])
            end, SlowSuites),
            io:format("  💡 Consider optimizing slow test suites for better CI/CD performance.~n")
    end,
    
    ok.

%% Determine overall result from all test suite results
determine_overall_result(Results) ->
    case lists:all(fun({_, Result, _}) -> Result =:= pass end, Results) of
        true -> pass;
        false -> fail
    end.

%% ============================================================================
%% Utility Functions
%% ============================================================================

%% Display help information
help() ->
    io:format("~nBest Agent Runner Integration Test Suite~n"),
    io:format("=======================================~n~n"),
    io:format("Usage:~n"),
    io:format("  run_integration_tests:run_all().           - Run all test suites~n"),
    io:format("  run_integration_tests:run_smoke_tests().   - Run essential tests only~n"),
    io:format("  run_integration_tests:run_suite(\"unit\").   - Run specific test suite~n"),
    io:format("  run_integration_tests:help().              - Show this help~n"),
    io:format("~nAvailable test suites:~n"),
    io:format("  - unit: Unit tests for individual functions~n"),
    io:format("  - integration: Integration tests for module interactions~n"),
    io:format("  - e2e: End-to-end tests with real data files~n"),
    io:format("  - compatibility: Backward compatibility tests~n"),
    io:format("  - error: Error handling and edge case tests~n"),
    io:format("  - format: Data format and parsing tests~n"),
    io:format("  - clean: Resource cleanup and memory tests~n"),
    io:format("~nExample:~n"),
    io:format("  1> run_integration_tests:run_smoke_tests().~n"),
    io:format("  2> run_integration_tests:run_all().~n"),
    io:format("~n"),
    ok.

%% Quick status check
status() ->
    io:format("~nBest Agent Runner Integration Test Status~n"),
    io:format("========================================~n"),
    
    % Check if required modules are available
    RequiredModules = [
        best_agent_runner,
        best_agent_runner_tests,
        best_agent_runner_integration_tests,
        best_agent_runner_e2e_tests,
        best_agent_runner_backward_compatibility_tests,
        best_agent_runner_error_tests,
        best_agent_runner_format_tests,
        best_agent_runner_clean_tests
    ],
    
    io:format("Module Availability:~n"),
    lists:foreach(fun(Module) ->
        Status = case code:is_loaded(Module) of
            {file, _} -> "✅ Loaded";
            false ->
                case code:load_file(Module) of
                    {module, _} -> "⚡ Available";
                    _ -> "❌ Missing"
                end
        end,
        io:format("  ~s: ~s~n", [Module, Status])
    end, RequiredModules),
    
    % Check system requirements
    io:format("~nSystem Requirements:~n"),
    io:format("  Erlang Version: ~s~n", [erlang:system_info(version)]),
    io:format("  System Architecture: ~s~n", [erlang:system_info(system_architecture)]),
    
    % Check Mnesia status
    MnesiaStatus = case catch mnesia:system_info(is_running) of
        yes -> "✅ Running";
        no -> "⚠️  Not running";
        starting -> "⚡ Starting";
        stopping -> "⚠️  Stopping";
        _ -> "❌ Error"
    end,
    io:format("  Mnesia Database: ~s~n", [MnesiaStatus]),
    
    io:format("~nReady to run tests!~n"),
    ok.