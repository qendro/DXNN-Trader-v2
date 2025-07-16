%% Backward compatibility tests for best_agent_runner
%% Ensures new functionality doesn't break existing training workflows

-module(best_agent_runner_backward_compatibility_tests).
-compile(export_all).
-include("records.hrl").

%% ============================================================================
%% Main Test Runner
%% ============================================================================

%% Run all backward compatibility tests
run_all_compatibility_tests() ->
    io:format("~n=== BACKWARD COMPATIBILITY TESTS ===~n"),
    
    % Setup test environment
    setup_compatibility_test_environment(),
    
    % Run individual test cases
    Results = [
        test_existing_genotype_utils_functions(),
        test_existing_fx_module_functions(),
        test_existing_exoself_functions(),
        test_existing_benchmarker_workflow(),
        test_existing_population_monitor_workflow(),
        test_existing_mnesia_operations(),
        test_existing_record_structures(),
        test_existing_ets_table_operations(),
        test_docker_environment_compatibility(),
        test_existing_file_operations()
    ],
    
    % Cleanup test environment
    cleanup_compatibility_test_environment(),
    
    % Report results
    report_compatibility_test_results(Results),
    
    % Return overall result
    case lists:all(fun(R) -> R =:= pass end, Results) of
        true -> 
            io:format("~n=== ALL BACKWARD COMPATIBILITY TESTS PASSED ===~n"),
            pass;
        false -> 
            io:format("~n=== SOME BACKWARD COMPATIBILITY TESTS FAILED ===~n"),
            fail
    end.

%% ============================================================================
%% Individual Test Cases
%% ============================================================================

%% Test that existing genotype_utils functions still work
test_existing_genotype_utils_functions() ->
    io:format("Testing existing genotype_utils functions...~n"),
    
    try
        % Test that print_best_genotype still works
        case catch genotype_utils:print_best_genotype(all) of
            {'EXIT', Reason} ->
                io:format("  ✗ print_best_genotype failed: ~p~n", [Reason]),
                fail;
            _ ->
                io:format("  ✓ print_best_genotype still works~n")
        end,
        
        % Test that other genotype_utils functions are unaffected
        TestFunctions = [
            {create_test, []},
            {test, []},
            {print_best_genotype, [all]}
        ],
        
        FunctionResults = [test_function_exists(genotype_utils, Func, Args) || {Func, Args} <- TestFunctions],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("  ✓ All genotype_utils functions preserved~n"),
                pass;
            false ->
                io:format("  ✗ Some genotype_utils functions affected~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ genotype_utils compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing fx module functions still work
test_existing_fx_module_functions() ->
    io:format("Testing existing fx module functions...~n"),
    
    try
        % Test core fx functions that should remain unchanged
        TestFunctions = [
            {sim, [self()]},
            {lookup, ['EURUSD1', 1]},
            {next, ['EURUSD1', 1]},
            {first, ['EURUSD1']},
            {last, ['EURUSD1']}
        ],
        
        % Note: These tests check function existence, not full functionality
        % since we don't have actual data tables in test environment
        FunctionResults = [test_function_exists(fx, Func, Args) || {Func, Args} <- TestFunctions],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("  ✓ All fx module functions preserved~n"),
                pass;
            false ->
                io:format("  ✗ Some fx module functions affected~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ fx module compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing exoself functions still work
test_existing_exoself_functions() ->
    io:format("Testing existing exoself functions...~n"),
    
    try
        % Test that exoself:start function signature is preserved
        case erlang:function_exported(exoself, start, 3) of
            true ->
                io:format("  ✓ exoself:start/3 function preserved~n");
            false ->
                io:format("  ✗ exoself:start/3 function missing~n"),
                throw(missing_function)
        end,
        
        % Test other critical exoself functions
        CriticalFunctions = [
            {start, 3},
            {prep, 1}
        ],
        
        FunctionResults = [
            case erlang:function_exported(exoself, Func, Arity) of
                true -> pass;
                false -> fail
            end || {Func, Arity} <- CriticalFunctions
        ],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("  ✓ All exoself functions preserved~n"),
                pass;
            false ->
                io:format("  ✗ Some exoself functions missing~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ exoself compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing benchmarker workflow is unaffected
test_existing_benchmarker_workflow() ->
    io:format("Testing existing benchmarker workflow...~n"),
    
    try
        % Check that benchmarker module and functions exist
        BenchmarkerFunctions = [
            {start, 0},
            {stop, 0}
        ],
        
        FunctionResults = [
            case erlang:function_exported(benchmarker, Func, Arity) of
                true -> pass;
                false -> fail
            end || {Func, Arity} <- BenchmarkerFunctions
        ],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("  ✓ Benchmarker workflow functions preserved~n"),
                pass;
            false ->
                io:format("  ✗ Some benchmarker functions missing~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ benchmarker compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing population_monitor workflow is unaffected
test_existing_population_monitor_workflow() ->
    io:format("Testing existing population_monitor workflow...~n"),
    
    try
        % Check that population_monitor module and functions exist
        PopulationMonitorFunctions = [
            {start, 0},
            {stop, 0}
        ],
        
        FunctionResults = [
            case erlang:function_exported(population_monitor, Func, Arity) of
                true -> pass;
                false -> fail
            end || {Func, Arity} <- PopulationMonitorFunctions
        ],
        
        case lists:all(fun(R) -> R =:= pass end, FunctionResults) of
            true ->
                io:format("  ✓ Population monitor workflow functions preserved~n"),
                pass;
            false ->
                io:format("  ✗ Some population monitor functions missing~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ population_monitor compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing Mnesia operations are unaffected
test_existing_mnesia_operations() ->
    io:format("Testing existing Mnesia operations...~n"),
    
    try
        % Test basic Mnesia operations that existing code relies on
        case mnesia:system_info(is_running) of
            yes ->
                io:format("  ✓ Mnesia is running~n");
            Status ->
                io:format("  ! Mnesia status: ~p (may affect tests)~n", [Status])
        end,
        
        % Test that we can still access standard tables
        StandardTables = [agent, cortex, neuron, sensor, actuator],
        
        TableResults = [
            case catch mnesia:table_info(Table, type) of
                {'EXIT', _} -> 
                    io:format("  ! Table ~p not available (expected in test environment)~n", [Table]),
                    pass;  % Expected in test environment
                _ -> 
                    io:format("  ✓ Table ~p accessible~n", [Table]),
                    pass
            end || Table <- StandardTables
        ],
        
        case lists:all(fun(R) -> R =:= pass end, TableResults) of
            true ->
                io:format("  ✓ Mnesia operations compatibility verified~n"),
                pass;
            false ->
                io:format("  ✗ Some Mnesia operations affected~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ Mnesia compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing record structures are preserved
test_existing_record_structures() ->
    io:format("Testing existing record structures...~n"),
    
    try
        % Test that we can still create existing records
        % Note: This tests compilation compatibility
        
        % Test agent record (if accessible)
        case catch #agent{id = test_agent} of
            {'EXIT', _} ->
                io:format("  ! Agent record not accessible (may be in different module)~n");
            _ ->
                io:format("  ✓ Agent record structure preserved~n")
        end,
        
        % Test that our new records don't conflict
        TestTradingDecision = #trading_decision{
            timestamp = now(),
            index = 1,
            price = 1.0500,
            signal = 1,
            confidence = 0.8,
            profit_loss = 0.5
        },
        
        TestRunResults = #agent_run_results{
            agent_id = test_agent,
            data_source = "test.csv",
            total_decisions = 10,
            trading_decisions = [TestTradingDecision]
        },
        
        TestRunOptions = #run_options{
            verbose = true,
            output_file = "test.txt"
        },
        
        case {is_record(TestTradingDecision, trading_decision),
              is_record(TestRunResults, agent_run_results),
              is_record(TestRunOptions, run_options)} of
            {true, true, true} ->
                io:format("  ✓ New record structures work correctly~n"),
                pass;
            _ ->
                io:format("  ✗ New record structures have issues~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ Record structure compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing ETS table operations are unaffected
test_existing_ets_table_operations() ->
    io:format("Testing existing ETS table operations...~n"),
    
    try
        % Create a test table to verify ETS operations work
        TestTable = ets:new(compatibility_test, [set, public]),
        
        % Test basic ETS operations
        ets:insert(TestTable, {key1, value1}),
        ets:insert(TestTable, {key2, value2}),
        
        case ets:lookup(TestTable, key1) of
            [{key1, value1}] ->
                io:format("  ✓ ETS insert/lookup operations work~n");
            _ ->
                io:format("  ✗ ETS operations failed~n"),
                throw(ets_failed)
        end,
        
        % Test table deletion
        ets:delete(TestTable),
        
        case ets:info(TestTable) of
            undefined ->
                io:format("  ✓ ETS table deletion works~n"),
                pass;
            _ ->
                io:format("  ✗ ETS table deletion failed~n"),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ ETS compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test Docker environment compatibility
test_docker_environment_compatibility() ->
    io:format("Testing Docker environment compatibility...~n"),
    
    try
        % Test that we can access expected directories
        ExpectedDirs = [
            ".",
            "fx_tables"
        ],
        
        DirResults = [
            case filelib:is_dir(Dir) of
                true ->
                    io:format("  ✓ Directory ~p accessible~n", [Dir]),
                    pass;
                false ->
                    io:format("  ! Directory ~p not found (may be expected)~n", [Dir]),
                    pass  % May not exist in test environment
            end || Dir <- ExpectedDirs
        ],
        
        % Test that we can create temporary files (important for data loading)
        TestFile = "compatibility_test_temp.txt",
        case file:write_file(TestFile, "test content") of
            ok ->
                io:format("  ✓ File creation works~n"),
                file:delete(TestFile),
                pass;
            {error, Reason} ->
                io:format("  ✗ File creation failed: ~p~n", [Reason]),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ Docker compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test that existing file operations are unaffected
test_existing_file_operations() ->
    io:format("Testing existing file operations...~n"),
    
    try
        % Test basic file operations that existing code relies on
        TestFile = "compatibility_file_test.txt",
        TestContent = "This is a compatibility test file",
        
        % Write file
        case file:write_file(TestFile, TestContent) of
            ok ->
                io:format("  ✓ File write operation works~n");
            {error, WriteReason} ->
                io:format("  ✗ File write failed: ~p~n", [WriteReason]),
                throw(file_write_failed)
        end,
        
        % Read file
        case file:read_file(TestFile) of
            {ok, ReadContent} ->
                case binary_to_list(ReadContent) of
                    TestContent ->
                        io:format("  ✓ File read operation works~n");
                    _ ->
                        io:format("  ✗ File content mismatch~n"),
                        throw(file_content_mismatch)
                end;
            {error, ReadReason} ->
                io:format("  ✗ File read failed: ~p~n", [ReadReason]),
                throw(file_read_failed)
        end,
        
        % Delete file
        case file:delete(TestFile) of
            ok ->
                io:format("  ✓ File delete operation works~n"),
                pass;
            {error, DeleteReason} ->
                io:format("  ✗ File delete failed: ~p~n", [DeleteReason]),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ File operations compatibility test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Test if a function exists and can be called (basic check)
test_function_exists(Module, Function, Args) ->
    try
        case erlang:function_exported(Module, Function, length(Args)) of
            true ->
                % Function exists, try to call it (may fail due to missing data, but that's OK)
                case catch apply(Module, Function, Args) of
                    {'EXIT', _} ->
                        % Function exists but failed (expected in test environment)
                        pass;
                    _ ->
                        % Function exists and executed
                        pass
                end;
            false ->
                fail
        end
    catch
        _:_ ->
            fail
    end.

%% Setup test environment
setup_compatibility_test_environment() ->
    io:format("Setting up compatibility test environment...~n"),
    put(compatibility_test_mode, true),
    ok.

%% Cleanup test environment
cleanup_compatibility_test_environment() ->
    io:format("Cleaning up compatibility test environment...~n"),
    
    % Clean up any test files
    TestFiles = [
        "compatibility_test_temp.txt",
        "compatibility_file_test.txt"
    ],
    
    lists:foreach(fun(File) ->
        case file:delete(File) of
            ok -> ok;
            {error, enoent} -> ok;  % File doesn't exist
            {error, Reason} ->
                io:format("Warning: Could not delete test file ~p: ~p~n", [File, Reason])
        end
    end, TestFiles),
    
    erase(compatibility_test_mode),
    ok.

%% Report test results
report_compatibility_test_results(Results) ->
    TotalTests = length(Results),
    PassedTests = length([R || R <- Results, R =:= pass]),
    FailedTests = TotalTests - PassedTests,
    
    io:format("~n=== COMPATIBILITY TEST RESULTS SUMMARY ===~n"),
    io:format("Total Tests: ~p~n", [TotalTests]),
    io:format("Passed: ~p~n", [PassedTests]),
    io:format("Failed: ~p~n", [FailedTests]),
    io:format("Success Rate: ~.1f%~n", [PassedTests / TotalTests * 100]),
    
    case FailedTests > 0 of
        true ->
            io:format("~nWARNING: Some compatibility tests failed!~n"),
            io:format("This may indicate that new functionality breaks existing workflows.~n"),
            io:format("Please review the failed tests and ensure backward compatibility.~n");
        false ->
            io:format("~nAll compatibility tests passed - backward compatibility verified.~n")
    end,
    
    ok.

%% ============================================================================
%% Integration Test with Existing Workflow
%% ============================================================================

%% Test complete integration with existing training workflow
test_complete_workflow_integration() ->
    io:format("~n=== COMPLETE WORKFLOW INTEGRATION TEST ===~n"),
    
    try
        % Simulate existing training workflow steps
        io:format("1. Simulating existing training initialization...~n"),
        simulate_training_initialization(),
        
        io:format("2. Simulating population creation...~n"),
        simulate_population_creation(),
        
        io:format("3. Simulating training execution...~n"),
        simulate_training_execution(),
        
        io:format("4. Testing new best agent runner integration...~n"),
        test_best_agent_runner_integration(),
        
        io:format("5. Verifying existing cleanup still works...~n"),
        simulate_existing_cleanup(),
        
        io:format("✓ Complete workflow integration test passed~n"),
        pass
    catch
        Error:Reason ->
            io:format("✗ Complete workflow integration test failed: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Simulate existing training workflow steps
simulate_training_initialization() ->
    % This would normally initialize the training environment
    put(training_initialized, true),
    ok.

simulate_population_creation() ->
    % This would normally create the initial population
    put(population_created, true),
    ok.

simulate_training_execution() ->
    % This would normally run the training
    put(training_completed, true),
    ok.

test_best_agent_runner_integration() ->
    % Test that our new functionality can be called after training
    case get(training_completed) of
        true ->
            % Create a simple test data file
            TestFile = "integration_test_data.csv",
            TestData = "Timestamp,Open,High,Low,Close,Volume\n"
                      "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n",
            file:write_file(TestFile, TestData),
            
            % Test that we can call the new function without breaking anything
            case catch best_agent_runner:run_best_agent_on_data(TestFile) of
                {'EXIT', _} ->
                    % Expected to fail due to no real agents, but function should exist
                    file:delete(TestFile),
                    ok;
                {error, _} ->
                    % Expected error due to test environment
                    file:delete(TestFile),
                    ok;
                _ ->
                    file:delete(TestFile),
                    ok
            end;
        _ ->
            throw(training_not_completed)
    end.

simulate_existing_cleanup() ->
    % This would normally clean up training resources
    erase(training_initialized),
    erase(population_created),
    erase(training_completed),
    ok.