#!/usr/bin/env escript
%% Simple test script for Task 4 implementation
%% Run this inside the Docker container with: escript test_task4.erl

-module(test_task4).

main(_) ->
    io:format("=== Testing Task 4: Standalone Agent Execution Controller ===~n"),
    
    % Test compilation
    io:format("1. Testing compilation...~n"),
    case compile:file(best_agent_runner, [verbose, report_errors, report_warnings]) of
        {ok, _} ->
            io:format("   best_agent_runner.erl compiled successfully~n");
        {error, Errors, Warnings} ->
            io:format("   Compilation errors: ~p~n", [Errors]),
            io:format("   Compilation warnings: ~p~n", [Warnings]),
            halt(1)
    end,
    
    case compile:file(best_agent_runner_tests, [verbose, report_errors, report_warnings]) of
        {ok, _} ->
            io:format("   best_agent_runner_tests.erl compiled successfully~n");
        {error, Errors2, Warnings2} ->
            io:format("   Test compilation errors: ~p~n", [Errors2]),
            io:format("   Test compilation warnings: ~p~n", [Warnings2]),
            halt(1)
    end,
    
    % Test module loading
    io:format("2. Testing module loading...~n"),
    case code:load_file(best_agent_runner) of
        {module, best_agent_runner} ->
            io:format("   best_agent_runner module loaded~n");
        {error, Reason} ->
            io:format("   Failed to load best_agent_runner: ~p~n", [Reason]),
            halt(1)
    end,
    
    case code:load_file(best_agent_runner_tests) of
        {module, best_agent_runner_tests} ->
            io:format("   best_agent_runner_tests module loaded~n");
        {error, Reason2} ->
            io:format("   Failed to load best_agent_runner_tests: ~p~n", [Reason2]),
            halt(1)
    end,
    
    % Test basic functionality
    io:format("3. Testing basic functionality...~n"),
    try
        best_agent_runner:init(),
        io:format("   Module initialization: OK~n")
    catch
        Error:Reason3 ->
            io:format("   Module initialization failed: ~p:~p~n", [Error, Reason3])
    end,
    
    % Test function exports
    io:format("4. Testing function exports...~n"),
    Exports = best_agent_runner:module_info(exports),
    RequiredFunctions = [
        {run_agent_standalone, 2},
        {run_agent_standalone, 3},
        {start_agent_process, 1},
        {monitor_agent_execution, 4},
        {collect_trading_decisions, 3},
        {cleanup_agent_processes, 3}
    ],
    
    lists:foreach(fun({Func, Arity}) ->
        case lists:member({Func, Arity}, Exports) of
            true ->
                io:format("   Function ~p/~p: EXPORTED~n", [Func, Arity]);
            false ->
                io:format("   Function ~p/~p: MISSING~n", [Func, Arity])
        end
    end, RequiredFunctions),
    
    % Run quick tests
    io:format("5. Running quick tests...~n"),
    try
        best_agent_runner_tests:quick_test(),
        io:format("   Quick tests completed~n")
    catch
        Error2:Reason4 ->
            io:format("   Quick tests failed: ~p:~p~n", [Error2, Reason4])
    end,
    
    io:format("~n=== Task 4 Implementation Test Complete ===~n"),
    io:format("To run full tests, start the Docker container and run:~n"),
    io:format("  erl -noshell -eval \"c(best_agent_runner), c(best_agent_runner_tests), best_agent_runner_tests:run_all_tests(), halt().\"~n"),
    
    halt(0).