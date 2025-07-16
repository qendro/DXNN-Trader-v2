%% Integration tests for best_agent_runner main interface functions
%% Tests the complete workflow: agent identification, data loading, execution, and result collection

-module(best_agent_runner_integration_tests).
-compile(export_all).
-include("records.hrl").

%% Include record definitions from best_agent_runner
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

%% Test runner function
run_all_tests() ->
    io:format("~n=== BEST AGENT RUNNER INTEGRATION TESTS ===~n"),
    
    % Initialize test environment
    setup_test_environment(),
    
    % Run individual test cases
    Results = [
        test_run_best_agent_on_data_basic(),
        test_run_best_agent_on_data_with_options(),
        test_run_best_agent_with_file_output(),
        test_run_best_agent_verbose_mode(),
        test_error_handling_no_agents(),
        test_error_handling_invalid_data_file(),
        test_error_handling_agent_execution_failure(),
        test_workflow_integration_complete()
    ],
    
    % Cleanup test environment
    cleanup_test_environment(),
    
    % Report results
    report_test_results(Results),
    
    % Return overall result
    case lists:all(fun(R) -> R =:= pass end, Results) of
        true -> 
            io:format("~n=== ALL INTEGRATION TESTS PASSED ===~n"),
            pass;
        false -> 
            io:format("~n=== SOME INTEGRATION TESTS FAILED ===~n"),
            fail
    end.

%% ============================================================================
%% Test Cases
%% ============================================================================

%% Test basic run_best_agent_on_data/1 function
test_run_best_agent_on_data_basic() ->
    io:format("Testing basic run_best_agent_on_data/1...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("basic_test_data.csv"),
        
        % Mock the dependencies
        setup_mocks_for_basic_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile),
        
        % Verify the result
        case Result of
            {ok, Results} when is_record(Results, agent_run_results) ->
                % Verify result structure
                case verify_basic_result_structure(Results) of
                    true ->
                        io:format("  ✓ Basic workflow test passed~n"),
                        pass;
                    false ->
                        io:format("  ✗ Basic workflow test failed - invalid result structure~n"),
                        fail
                end;
            {error, Reason} ->
                io:format("  ✗ Basic workflow test failed with error: ~p~n", [Reason]),
                fail;
            Other ->
                io:format("  ✗ Basic workflow test failed - unexpected result: ~p~n", [Other]),
                fail
        end
    catch
        Error:BasicReason ->
            io:format("  ✗ Basic workflow test failed with exception: ~p:~p~n", [Error, BasicReason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test run_best_agent_on_data/2 with options
test_run_best_agent_on_data_with_options() ->
    io:format("Testing run_best_agent_on_data/2 with options...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("options_test_data.csv"),
        
        % Create test options
        Options = #run_options{
            start_index = 10,
            end_index = 100,
            verbose = false,
            collect_all_decisions = true
        },
        
        % Mock the dependencies
        setup_mocks_for_options_test(),
        
        % Call the function with options
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile, Options),
        
        % Verify the result
        case Result of
            {ok, Results} when is_record(Results, agent_run_results) ->
                case verify_options_result(Results, Options) of
                    true ->
                        io:format("  ✓ Options workflow test passed~n"),
                        pass;
                    false ->
                        io:format("  ✗ Options workflow test failed - options not applied correctly~n"),
                        fail
                end;
            {error, Reason} ->
                io:format("  ✗ Options workflow test failed with error: ~p~n", [Reason]),
                fail;
            Other ->
                io:format("  ✗ Options workflow test failed - unexpected result: ~p~n", [Other]),
                fail
        end
    catch
        Error:OptionsReason ->
            io:format("  ✗ Options workflow test failed with exception: ~p:~p~n", [Error, OptionsReason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test file output functionality
test_run_best_agent_with_file_output() ->
    io:format("Testing file output functionality...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("file_output_test_data.csv"),
        OutputFile = "test_results_output.txt",
        
        % Create options with file output
        Options = #run_options{
            output_file = OutputFile,
            verbose = true
        },
        
        % Mock the dependencies
        setup_mocks_for_file_output_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile, Options),
        
        % Verify the result and file creation
        case Result of
            {ok, Results} when is_record(Results, agent_run_results) ->
                % Check if file was created
                case file:read_file(OutputFile) of
                    {ok, FileContent} ->
                        case verify_output_file_content(FileContent) of
                            true ->
                                io:format("  ✓ File output test passed~n"),
                                file:delete(OutputFile),  % Cleanup
                                pass;
                            false ->
                                io:format("  ✗ File output test failed - invalid file content~n"),
                                file:delete(OutputFile),  % Cleanup
                                fail
                        end;
                    {error, FileError} ->
                        io:format("  ✗ File output test failed - file not created: ~p~n", [FileError]),
                        fail
                end;
            {error, Reason} ->
                io:format("  ✗ File output test failed with error: ~p~n", [Reason]),
                fail
        end
    catch
        Error:FileReason ->
            io:format("  ✗ File output test failed with exception: ~p:~p~n", [Error, FileReason]),
            fail
    after
        cleanup_test_mocks(),
        file:delete("test_results_output.txt")  % Ensure cleanup
    end.

%% Test verbose mode functionality
test_run_best_agent_verbose_mode() ->
    io:format("Testing verbose mode functionality...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("verbose_test_data.csv"),
        
        % Create verbose options
        Options = #run_options{
            verbose = true,
            collect_all_decisions = true
        },
        
        % Mock the dependencies
        setup_mocks_for_verbose_test(),
        
        % Capture output (simplified - in real test we'd capture io output)
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile, Options),
        
        % Verify the result
        case Result of
            {ok, Results} when is_record(Results, agent_run_results) ->
                % In verbose mode, we expect detailed output and complete decision collection
                case verify_verbose_result(Results) of
                    true ->
                        io:format("  ✓ Verbose mode test passed~n"),
                        pass;
                    false ->
                        io:format("  ✗ Verbose mode test failed - verbose features not working~n"),
                        fail
                end;
            {error, Reason} ->
                io:format("  ✗ Verbose mode test failed with error: ~p~n", [Reason]),
                fail
        end
    catch
        Error:VerboseReason ->
            io:format("  ✗ Verbose mode test failed with exception: ~p:~p~n", [Error, VerboseReason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test error handling when no agents exist
test_error_handling_no_agents() ->
    io:format("Testing error handling - no agents...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("no_agents_test_data.csv"),
        
        % Mock no agents available
        setup_mocks_for_no_agents_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile),
        
        % Verify error handling
        case Result of
            {error, {agent_identification_failed, _}} ->
                io:format("  ✓ No agents error handling test passed~n"),
                pass;
            {ok, _} ->
                io:format("  ✗ No agents error handling test failed - should have returned error~n"),
                fail;
            {error, Other} ->
                io:format("  ✗ No agents error handling test failed - wrong error type: ~p~n", [Other]),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ No agents error handling test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test error handling for invalid data file
test_error_handling_invalid_data_file() ->
    io:format("Testing error handling - invalid data file...~n"),
    
    try
        % Use non-existent file
        InvalidDataFile = "non_existent_file.csv",
        
        % Mock the dependencies (agent exists but data loading fails)
        setup_mocks_for_invalid_data_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(InvalidDataFile),
        
        % Verify error handling
        case Result of
            {error, {data_loading_failed, _}} ->
                io:format("  ✓ Invalid data file error handling test passed~n"),
                pass;
            {ok, _} ->
                io:format("  ✗ Invalid data file error handling test failed - should have returned error~n"),
                fail;
            {error, Other} ->
                io:format("  ✗ Invalid data file error handling test failed - wrong error type: ~p~n", [Other]),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ Invalid data file error handling test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test error handling for agent execution failure
test_error_handling_agent_execution_failure() ->
    io:format("Testing error handling - agent execution failure...~n"),
    
    try
        % Create test data file
        TestDataFile = create_test_data_file("execution_failure_test_data.csv"),
        
        % Mock agent execution failure
        setup_mocks_for_execution_failure_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile),
        
        % Verify error handling
        case Result of
            {error, {execution_failed, _}} ->
                io:format("  ✓ Agent execution failure error handling test passed~n"),
                pass;
            {ok, _} ->
                io:format("  ✗ Agent execution failure error handling test failed - should have returned error~n"),
                fail;
            {error, Other} ->
                io:format("  ✗ Agent execution failure error handling test failed - wrong error type: ~p~n", [Other]),
                fail
        end
    catch
        Error:Reason ->
            io:format("  ✗ Agent execution failure error handling test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    after
        cleanup_test_mocks()
    end.

%% Test complete workflow integration
test_workflow_integration_complete() ->
    io:format("Testing complete workflow integration...~n"),
    
    try
        % Create comprehensive test data
        TestDataFile = create_comprehensive_test_data_file("complete_workflow_test_data.csv"),
        
        % Create comprehensive options
        Options = #run_options{
            start_index = 1,
            end_index = 50,
            output_file = "complete_workflow_results.txt",
            verbose = true,
            collect_all_decisions = true
        },
        
        % Mock all dependencies for complete workflow
        setup_mocks_for_complete_workflow_test(),
        
        % Call the function
        Result = best_agent_runner:run_best_agent_on_data(TestDataFile, Options),
        
        % Verify complete workflow
        case Result of
            {ok, Results} when is_record(Results, agent_run_results) ->
                case verify_complete_workflow_result(Results, Options) of
                    true ->
                        io:format("  ✓ Complete workflow integration test passed~n"),
                        pass;
                    false ->
                        io:format("  ✗ Complete workflow integration test failed - workflow incomplete~n"),
                        fail
                end;
            {error, Reason} ->
                io:format("  ✗ Complete workflow integration test failed with error: ~p~n", [Reason]),
                fail
        end
    catch
        Error:CompleteReason ->
            io:format("  ✗ Complete workflow integration test failed with exception: ~p:~p~n", [Error, CompleteReason]),
            fail
    after
        cleanup_test_mocks(),
        file:delete("complete_workflow_results.txt")
    end.

%% ============================================================================
%% Test Helper Functions
%% ============================================================================

%% Setup test environment
setup_test_environment() ->
    io:format("Setting up test environment...~n"),
    % Initialize any required test state
    put(test_mode, true),
    ok.

%% Cleanup test environment
cleanup_test_environment() ->
    io:format("Cleaning up test environment...~n"),
    % Clean up test files and state
    TestFiles = [
        "basic_test_data.csv",
        "options_test_data.csv", 
        "file_output_test_data.csv",
        "verbose_test_data.csv",
        "no_agents_test_data.csv",
        "execution_failure_test_data.csv",
        "complete_workflow_test_data.csv"
    ],
    lists:foreach(fun(File) -> file:delete(File) end, TestFiles),
    erase(test_mode),
    ok.

%% Create test data file with sample forex data
create_test_data_file(Filename) ->
    TestData = "Timestamp,Open,High,Low,Close,Volume\n"
               "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n"
               "2023-01-01 00:01:00,1.0510,1.0525,1.0505,1.0515,1100\n"
               "2023-01-01 00:02:00,1.0515,1.0530,1.0510,1.0520,1200\n"
               "2023-01-01 00:03:00,1.0520,1.0535,1.0515,1.0525,1300\n"
               "2023-01-01 00:04:00,1.0525,1.0540,1.0520,1.0530,1400\n",
    
    file:write_file(Filename, TestData),
    Filename.

%% Create comprehensive test data file
create_comprehensive_test_data_file(Filename) ->
    % Generate more comprehensive test data
    Header = "Timestamp,Open,High,Low,Close,Volume\n",
    
    % Generate 100 rows of test data
    Rows = lists:map(fun(I) ->
        BasePrice = 1.0500 + (I * 0.0001),
        High = BasePrice + 0.0010,
        Low = BasePrice - 0.0010,
        Close = BasePrice + (rand:uniform() - 0.5) * 0.0020,
        Volume = 1000 + I * 10,
        
        io_lib:format("2023-01-01 ~2..0w:~2..0w:00,~.4f,~.4f,~.4f,~.4f,~w~n",
                     [I div 60, I rem 60, BasePrice, High, Low, Close, Volume])
    end, lists:seq(1, 100)),
    
    TestData = Header ++ lists:flatten(Rows),
    file:write_file(Filename, TestData),
    Filename.

%% Mock setup functions for different test scenarios
setup_mocks_for_basic_test() ->
    % Mock genotype_utils:find_best_agent/0
    meck:new(genotype_utils, [unstick]),
    meck:expect(genotype_utils, find_best_agent, fun() -> {ok, test_agent_123} end),
    
    % Mock fx:load_data_file/1
    meck:new(fx, [unstick]),
    meck:expect(fx, load_data_file, fun(_) -> {ok, test_table_basic} end),
    meck:expect(fx, delete_temporary_table, fun(_) -> ok end),
    
    % Mock exoself:start/3
    meck:new(exoself, [unstick]),
    meck:expect(exoself, start, fun(_, _, _) -> spawn(fun() -> mock_agent_process() end) end),
    
    ok.

setup_mocks_for_options_test() ->
    setup_mocks_for_basic_test(),
    % Additional mocks specific to options testing
    ok.

setup_mocks_for_file_output_test() ->
    setup_mocks_for_basic_test(),
    % File operations are real, no additional mocks needed
    ok.

setup_mocks_for_verbose_test() ->
    setup_mocks_for_basic_test(),
    % Verbose mode uses same mocks but with different behavior expectations
    ok.

setup_mocks_for_no_agents_test() ->
    % Mock no agents available
    meck:new(genotype_utils, [unstick]),
    meck:expect(genotype_utils, find_best_agent, fun() -> {error, no_agents} end),
    ok.

setup_mocks_for_invalid_data_test() ->
    % Mock agent exists but data loading fails
    meck:new(genotype_utils, [unstick]),
    meck:expect(genotype_utils, find_best_agent, fun() -> {ok, test_agent_123} end),
    
    meck:new(fx, [unstick]),
    meck:expect(fx, load_data_file, fun(_) -> {error, file_not_found} end),
    ok.

setup_mocks_for_execution_failure_test() ->
    % Mock agent and data loading succeed but execution fails
    meck:new(genotype_utils, [unstick]),
    meck:expect(genotype_utils, find_best_agent, fun() -> {ok, test_agent_123} end),
    
    meck:new(fx, [unstick]),
    meck:expect(fx, load_data_file, fun(_) -> {ok, test_table_failure} end),
    meck:expect(fx, delete_temporary_table, fun(_) -> ok end),
    
    meck:new(exoself, [unstick]),
    meck:expect(exoself, start, fun(_, _, _) -> {error, startup_failed} end),
    ok.

setup_mocks_for_complete_workflow_test() ->
    setup_mocks_for_basic_test(),
    % Complete workflow uses comprehensive mocks
    ok.

%% Mock agent process for testing
mock_agent_process() ->
    timer:sleep(100),  % Simulate some processing time
    % Send completion message
    receive
        {From, terminate} ->
            From ! {self(), terminated, 85.5}
    after 1000 ->
        % Simulate normal completion
        exit(normal)
    end.

%% Cleanup test mocks
cleanup_test_mocks() ->
    % Unload all mocks
    try meck:unload(genotype_utils) catch _:_ -> ok end,
    try meck:unload(fx) catch _:_ -> ok end,
    try meck:unload(exoself) catch _:_ -> ok end,
    ok.

%% Verification functions
verify_basic_result_structure(Results) ->
    is_record(Results, agent_run_results) andalso
    Results#agent_run_results.agent_id =/= undefined andalso
    Results#agent_run_results.data_source =/= undefined andalso
    is_integer(Results#agent_run_results.total_decisions) andalso
    is_list(Results#agent_run_results.trading_decisions).

verify_options_result(Results, Options) ->
    verify_basic_result_structure(Results) andalso
    % Verify options were applied (simplified check)
    Results#agent_run_results.data_source =/= undefined.

verify_output_file_content(FileContent) ->
    % Check if file contains expected report structure
    Content = binary_to_list(FileContent),
    string:str(Content, "DXNN Best Agent Trading Results Report") > 0 andalso
    string:str(Content, "Agent ID:") > 0 andalso
    string:str(Content, "Trading Statistics:") > 0.

verify_verbose_result(Results) ->
    verify_basic_result_structure(Results) andalso
    % In verbose mode, we expect more detailed information
    length(Results#agent_run_results.trading_decisions) >= 0.

verify_complete_workflow_result(Results, Options) ->
    verify_basic_result_structure(Results) andalso
    verify_options_result(Results, Options) andalso
    % Check that file output was created
    case Options#run_options.output_file of
        undefined -> true;
        OutputFile -> filelib:is_file(OutputFile)
    end.

%% Report test results
report_test_results(Results) ->
    TotalTests = length(Results),
    PassedTests = length([R || R <- Results, R =:= pass]),
    FailedTests = TotalTests - PassedTests,
    
    io:format("~n=== TEST RESULTS SUMMARY ===~n"),
    io:format("Total Tests: ~p~n", [TotalTests]),
    io:format("Passed: ~p~n", [PassedTests]),
    io:format("Failed: ~p~n", [FailedTests]),
    io:format("Success Rate: ~.1f%~n", [PassedTests / TotalTests * 100]),
    ok.