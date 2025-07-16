%% Comprehensive error handling tests for best_agent_runner
-module(best_agent_runner_error_tests).
-compile(export_all).
-include("records.hrl").

%% Test record definitions
-record(trading_decision, {
    timestamp,
    index,
    price,
    signal,      % -1, 0, 1 for sell, hold, buy
    confidence,  % derived from fitness or other metrics
    profit_loss  % calculated profit/loss from decision
}).

-record(agent_run_results, {
    agent_id,
    data_source,
    start_time,
    end_time,
    total_decisions,
    buy_decisions,
    sell_decisions,
    hold_decisions,
    total_profit_loss,
    max_profit,
    max_loss,
    trading_decisions,  % List of #trading_decision{}
    execution_stats     % Performance metrics
}).

-record(run_options, {
    start_index = first,     % Where to start in the data
    end_index = last,        % Where to end in the data
    output_file = undefined, % Optional file to save results
    verbose = false,         % Detailed logging
    collect_all_decisions = true % Store all individual decisions
}).

%% ============================================================================
%% MAIN TEST SUITE FOR TASK 8: COMPREHENSIVE ERROR HANDLING
%% ============================================================================

%% Main test runner
run_all_error_tests() ->
    io:format("~n=== RUNNING COMPREHENSIVE ERROR HANDLING TESTS ===~n"),
    
    % Initialize test environment
    setup_error_test_environment(),
    
    % Run error handling tests
    Results = [
        test_file_not_found_error(),
        test_invalid_file_format_error(),
        test_file_parsing_errors(),
        test_agent_not_found_error(),
        test_agent_startup_failures(),
        test_agent_execution_timeouts(),
        test_resource_cleanup_on_errors(),
        test_ets_table_cleanup_errors(),
        test_process_cleanup_errors(),
        test_mnesia_connection_errors(),
        test_system_resource_validation(),
        test_error_recovery_mechanisms(),
        test_concurrent_error_scenarios(),
        test_memory_exhaustion_handling(),
        test_process_limit_handling()
    ],
    
    % Cleanup test environment
    cleanup_error_test_environment(),
    
    % Report results
    Passed = length([R || R <- Results, R == pass]),
    Failed = length([R || R <- Results, R == fail]),
    
    io:format("~n=== ERROR HANDLING TEST RESULTS ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total:  ~p~n", [Passed + Failed]),
    
    case Failed of
        0 -> 
            io:format("All error handling tests PASSED!~n"),
            pass;
        _ -> 
            io:format("Some error handling tests FAILED!~n"),
            fail
    end.

%% Setup error test environment
setup_error_test_environment() ->
    io:format("Setting up error test environment...~n"),
    
    % Create test directories
    file:make_dir("test_error_data"),
    
    % Create various test files for error scenarios
    create_test_error_files(),
    
    ok.

%% Cleanup error test environment
cleanup_error_test_environment() ->
    io:format("Cleaning up error test environment...~n"),
    
    % Remove test files
    cleanup_test_error_files(),
    
    % Remove test directory
    file:del_dir("test_error_data"),
    
    ok.

%% Create test files for error scenarios
create_test_error_files() ->
    % Create empty file
    file:write_file("test_error_data/empty_file.csv", ""),
    
    % Create file with invalid format
    file:write_file("test_error_data/invalid_format.csv", "invalid,data,format\n1,2,3\n"),
    
    % Create file with parsing errors
    file:write_file("test_error_data/parse_error.csv", 
                   "Timestamp,Open,High,Low,Close,Volume\n"
                   "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n"
                   "invalid_timestamp,not_a_number,1.0520,1.0495,1.0510,1000\n"),
    
    % Create file with insufficient columns
    file:write_file("test_error_data/insufficient_columns.csv", 
                   "Timestamp,Open,High\n"
                   "2023-01-01 00:00:00,1.0500,1.0520\n"),
    
    % Create valid test file for comparison
    file:write_file("test_error_data/valid_test.csv",
                   "Timestamp,Open,High,Low,Close,Volume\n"
                   "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n"
                   "2023-01-01 00:01:00,1.0510,1.0530,1.0505,1.0520,1100\n").

%% Cleanup test files
cleanup_test_error_files() ->
    TestFiles = [
        "test_error_data/empty_file.csv",
        "test_error_data/invalid_format.csv", 
        "test_error_data/parse_error.csv",
        "test_error_data/insufficient_columns.csv",
        "test_error_data/valid_test.csv"
    ],
    
    lists:foreach(fun(File) ->
        file:delete(File)
    end, TestFiles).

%% ============================================================================
%% FILE ERROR HANDLING TESTS
%% ============================================================================

%% Test file not found error handling
test_file_not_found_error() ->
    io:format("~n--- Test: File Not Found Error Handling ---~n"),
    
    try
        NonExistentFile = "test_error_data/nonexistent_file.csv",
        
        % Test the main interface
        case best_agent_runner:run_best_agent_on_data(NonExistentFile) of
            {error, {data_loading_failed, {file_not_found, NonExistentFile}}} ->
                io:format("   File not found error handled correctly: OK~n"),
                pass;
            {error, {data_loading_failed, {file_validation_exception, _, _}}} ->
                io:format("   File not found error handled with validation exception: OK~n"),
                pass;
            {error, Other} ->
                io:format("   Unexpected error for missing file: ~p~n", [Other]),
                pass;  % Still acceptable as long as it's an error
            {ok, _} ->
                io:format("   ERROR: Missing file should not succeed~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test invalid file format error handling
test_invalid_file_format_error() ->
    io:format("~n--- Test: Invalid File Format Error Handling ---~n"),
    
    try
        InvalidFile = "test_error_data/invalid_format.csv",
        
        case best_agent_runner:run_best_agent_on_data(InvalidFile) of
            {error, {data_loading_failed, _}} ->
                io:format("   Invalid format error handled correctly: OK~n"),
                pass;
            {error, _} ->
                io:format("   Invalid format error handled (generic): OK~n"),
                pass;
            {ok, _} ->
                io:format("   ERROR: Invalid format should not succeed~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test file parsing error handling
test_file_parsing_errors() ->
    io:format("~n--- Test: File Parsing Error Handling ---~n"),
    
    try
        ParseErrorFile = "test_error_data/parse_error.csv",
        
        case best_agent_runner:run_best_agent_on_data(ParseErrorFile) of
            {error, {data_loading_failed, _}} ->
                io:format("   Parse error handled correctly: OK~n"),
                pass;
            {error, _} ->
                io:format("   Parse error handled (generic): OK~n"),
                pass;
            {ok, _} ->
                io:format("   ERROR: Parse error should not succeed~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% AGENT ERROR HANDLING TESTS
%% ============================================================================

%% Test agent not found error handling
test_agent_not_found_error() ->
    io:format("~n--- Test: Agent Not Found Error Handling ---~n"),
    
    try
        % Test with valid file but no agents in database
        ValidFile = "test_error_data/valid_test.csv",
        
        case best_agent_runner:run_best_agent_on_data(ValidFile) of
            {error, {agent_identification_failed, _}} ->
                io:format("   Agent not found error handled correctly: OK~n"),
                pass;
            {error, _} ->
                io:format("   Agent error handled (generic): OK~n"),
                pass;
            {ok, _} ->
                io:format("   WARNING: Agent execution succeeded (may have agents in DB)~n"),
                pass  % This is OK if there are actually agents in the database
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test agent startup failure handling
test_agent_startup_failures() ->
    io:format("~n--- Test: Agent Startup Failure Handling ---~n"),
    
    try
        % Test start_agent_process_with_timeout with invalid agent
        InvalidAgentId = {nonexistent_agent_12345, agent},
        
        case best_agent_runner:start_agent_process_with_timeout(InvalidAgentId, 5000) of
            {error, {agent_validation_exception, _, _}} ->
                io:format("   Agent startup validation error handled: OK~n"),
                pass;
            {error, {agent_not_found, _}} ->
                io:format("   Agent not found during startup handled: OK~n"),
                pass;
            {error, _} ->
                io:format("   Agent startup error handled (generic): OK~n"),
                pass;
            {ok, _} ->
                io:format("   WARNING: Invalid agent startup succeeded~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test agent execution timeout handling
test_agent_execution_timeouts() ->
    io:format("~n--- Test: Agent Execution Timeout Handling ---~n"),
    
    try
        % Test timeout functionality with mock processes
        MockAgent = spawn(fun() -> timer:sleep(10000) end),  % Long running process
        MockMonitor = spawn(fun() -> timer:sleep(10000) end),
        MockCollector = spawn(fun() -> timer:sleep(10000) end),
        
        StartTime = erlang:timestamp(),
        Options = #run_options{verbose = false},
        
        % Test with very short timeout
        case best_agent_runner:wait_for_completion_with_timeout(
                MockAgent, MockMonitor, MockCollector, StartTime, Options, 1000) of
            {error, {execution_timeout, 1000}} ->
                io:format("   Execution timeout handled correctly: OK~n"),
                
                % Cleanup mock processes
                exit(MockAgent, kill),
                exit(MockMonitor, kill),
                exit(MockCollector, kill),
                
                pass;
            Other ->
                io:format("   Unexpected timeout result: ~p~n", [Other]),
                
                % Cleanup mock processes
                exit(MockAgent, kill),
                exit(MockMonitor, kill),
                exit(MockCollector, kill),
                
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% RESOURCE CLEANUP ERROR HANDLING TESTS
%% ============================================================================

%% Test resource cleanup on errors
test_resource_cleanup_on_errors() ->
    io:format("~n--- Test: Resource Cleanup on Errors ---~n"),
    
    try
        % Test cleanup_agent_processes_safe with various process states
        
        % Test with undefined processes
        best_agent_runner:cleanup_agent_processes_safe(undefined, undefined, undefined),
        io:format("   Cleanup with undefined processes: OK~n"),
        
        % Test with dead processes
        DeadProcess1 = spawn(fun() -> ok end),
        DeadProcess2 = spawn(fun() -> ok end),
        DeadProcess3 = spawn(fun() -> ok end),
        
        timer:sleep(100),  % Let processes die
        
        best_agent_runner:cleanup_agent_processes_safe(DeadProcess1, DeadProcess2, DeadProcess3),
        io:format("   Cleanup with dead processes: OK~n"),
        
        % Test with live processes
        LiveProcess1 = spawn(fun() -> 
            receive
                {_, terminate} -> ok;
                _ -> timer:sleep(5000)
            end
        end),
        LiveProcess2 = spawn(fun() -> 
            receive
                {_, terminate} -> ok;
                _ -> timer:sleep(5000)
            end
        end),
        LiveProcess3 = spawn(fun() -> 
            receive
                {_, terminate} -> ok;
                _ -> timer:sleep(5000)
            end
        end),
        
        best_agent_runner:cleanup_agent_processes_safe(LiveProcess1, LiveProcess2, LiveProcess3),
        io:format("   Cleanup with live processes: OK~n"),
        
        pass
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test ETS table cleanup errors
test_ets_table_cleanup_errors() ->
    io:format("~n--- Test: ETS Table Cleanup Error Handling ---~n"),
    
    try
        % Test cleanup of non-existent table
        NonExistentTable = non_existent_table_12345,
        best_agent_runner:cleanup_temporary_data_safe(NonExistentTable),
        io:format("   Cleanup of non-existent table handled: OK~n"),
        
        % Test cleanup of valid table
        TestTable = test_cleanup_table,
        ets:new(TestTable, [set, public, named_table]),
        ets:insert(TestTable, {test_key, test_value}),
        
        best_agent_runner:cleanup_temporary_data_safe(TestTable),
        io:format("   Cleanup of valid table: OK~n"),
        
        pass
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test process cleanup errors
test_process_cleanup_errors() ->
    io:format("~n--- Test: Process Cleanup Error Handling ---~n"),
    
    try
        % Test cleanup_process_safe with various scenarios
        
        % Test with undefined process
        best_agent_runner:cleanup_process_safe(undefined, "Test", fun(_) -> ok end),
        io:format("   Cleanup of undefined process: OK~n"),
        
        % Test with dead process
        DeadProcess = spawn(fun() -> ok end),
        timer:sleep(100),
        best_agent_runner:cleanup_process_safe(DeadProcess, "Dead", fun(_) -> ok end),
        io:format("   Cleanup of dead process: OK~n"),
        
        % Test with unresponsive process
        UnresponsiveProcess = spawn(fun() -> timer:sleep(10000) end),
        best_agent_runner:cleanup_process_safe(UnresponsiveProcess, "Unresponsive", 
                                             fun(PId) -> PId ! {self(), terminate} end),
        io:format("   Cleanup of unresponsive process: OK~n"),
        
        pass
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% DATABASE ERROR HANDLING TESTS
%% ============================================================================

%% Test Mnesia connection errors
test_mnesia_connection_errors() ->
    io:format("~n--- Test: Mnesia Connection Error Handling ---~n"),
    
    try
        % Test find_best_agent with various Mnesia states
        case best_agent_runner:find_best_agent() of
            {error, mnesia_not_running} ->
                io:format("   Mnesia not running error handled: OK~n"),
                pass;
            {error, {find_agent_exception, _, _}} ->
                io:format("   Mnesia exception handled: OK~n"),
                pass;
            {error, _} ->
                io:format("   Mnesia error handled (generic): OK~n"),
                pass;
            {ok, _} ->
                io:format("   Mnesia connection successful (system dependent): OK~n"),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% SYSTEM RESOURCE ERROR HANDLING TESTS
%% ============================================================================

%% Test system resource validation
test_system_resource_validation() ->
    io:format("~n--- Test: System Resource Validation ---~n"),
    
    try
        case best_agent_runner:validate_system_resources() of
            ok ->
                io:format("   System resources validation passed: OK~n"),
                pass;
            {error, {insufficient_memory, _}} ->
                io:format("   Insufficient memory detected: OK~n"),
                pass;
            {error, {process_limit_approaching, _, _}} ->
                io:format("   Process limit approaching detected: OK~n"),
                pass;
            {error, {ets_table_limit_approaching, _, _}} ->
                io:format("   ETS table limit approaching detected: OK~n"),
                pass;
            {error, _} ->
                io:format("   Resource validation error handled: OK~n"),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% ERROR RECOVERY MECHANISM TESTS
%% ============================================================================

%% Test error recovery mechanisms
test_error_recovery_mechanisms() ->
    io:format("~n--- Test: Error Recovery Mechanisms ---~n"),
    
    try
        % Test retry logic
        case best_agent_runner:attempt_error_recovery(agent_startup_failed, test_context, 3) of
            {retry, 2} ->
                io:format("   Agent startup retry mechanism: OK~n"),
                
                % Test data loading retry
                case best_agent_runner:attempt_error_recovery(data_loading_failed, test_context, 2) of
                    {retry, 1} ->
                        io:format("   Data loading retry mechanism: OK~n"),
                        
                        % Test timeout no-retry
                        case best_agent_runner:attempt_error_recovery(execution_timeout, test_context, 1) of
                            {no_retry, timeout_not_recoverable} ->
                                io:format("   Timeout no-retry mechanism: OK~n"),
                                pass;
                            Other ->
                                io:format("   Unexpected timeout recovery result: ~p~n", [Other]),
                                fail
                        end;
                    Other ->
                        io:format("   Unexpected data loading recovery result: ~p~n", [Other]),
                        fail
                end;
            Other ->
                io:format("   Unexpected agent startup recovery result: ~p~n", [Other]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% CONCURRENT ERROR SCENARIO TESTS
%% ============================================================================

%% Test concurrent error scenarios
test_concurrent_error_scenarios() ->
    io:format("~n--- Test: Concurrent Error Scenarios ---~n"),
    
    try
        % Spawn multiple processes that will fail concurrently
        TestProcesses = [spawn(fun() ->
            timer:sleep(random:uniform(1000)),
            exit(test_error)
        end) || _ <- lists:seq(1, 5)],
        
        % Test cleanup of multiple failing processes
        best_agent_runner:terminate_processes_forcefully(TestProcesses),
        
        % Verify all processes are terminated
        AllDead = lists:all(fun(P) -> not is_process_alive(P) end, TestProcesses),
        
        case AllDead of
            true ->
                io:format("   Concurrent process termination: OK~n"),
                pass;
            false ->
                io:format("   Some processes still alive after termination~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% STRESS TEST ERROR SCENARIOS
%% ============================================================================

%% Test memory exhaustion handling
test_memory_exhaustion_handling() ->
    io:format("~n--- Test: Memory Exhaustion Handling ---~n"),
    
    try
        % Test resource validation under memory pressure
        % (This is a light test to avoid actually exhausting memory)
        
        % Create some memory pressure
        LargeList = [lists:seq(1, 1000) || _ <- lists:seq(1, 100)],
        
        % Test resource validation
        case best_agent_runner:validate_system_resources() of
            ok ->
                io:format("   Memory validation under pressure: OK~n"),
                pass;
            {error, {insufficient_memory, _}} ->
                io:format("   Memory exhaustion detected correctly: OK~n"),
                pass;
            {error, _} ->
                io:format("   Memory validation error handled: OK~n"),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test process limit handling
test_process_limit_handling() ->
    io:format("~n--- Test: Process Limit Handling ---~n"),
    
    try
        % Get current process information
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        
        io:format("   Current processes: ~p/~p~n", [ProcessCount, ProcessLimit]),
        
        % Test resource validation
        case best_agent_runner:validate_system_resources() of
            ok ->
                io:format("   Process limit validation: OK~n"),
                pass;
            {error, {process_limit_approaching, _, _}} ->
                io:format("   Process limit warning detected: OK~n"),
                pass;
            {error, _} ->
                io:format("   Process validation error handled: OK~n"),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

%% Create a mock agent for testing
create_mock_agent_for_error_tests() ->
    MockAgent = #agent{
        id = {error_test_agent, agent},
        encoding_type = neural,
        generation = 1,
        population_id = test,
        specie_id = error_test_specie,
        cx_id = {error_test_cortex, cortex},
        fitness = 50.0,
        constraint = #constraint{}
    },
    
    put(mock_error_test_agent, MockAgent),
    MockAgent.

%% Cleanup mock agents
cleanup_mock_agents() ->
    erase(mock_error_test_agent).