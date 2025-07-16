%% Simple error handling test for best_agent_runner
-module(simple_error_test).
-compile(export_all).

%% Simple test to verify error handling concepts
test_basic_error_handling() ->
    io:format("=== Testing Basic Error Handling Concepts ===~n"),
    
    % Test 1: File not found handling
    test_file_not_found(),
    
    % Test 2: Process cleanup
    test_process_cleanup(),
    
    % Test 3: ETS table cleanup
    test_ets_cleanup(),
    
    % Test 4: Exception handling
    test_exception_handling(),
    
    io:format("=== Basic Error Handling Tests Complete ===~n").

%% Test file not found error handling
test_file_not_found() ->
    io:format("~n--- Test: File Not Found Handling ---~n"),
    
    NonExistentFile = "nonexistent_file.csv",
    
    case filelib:is_file(NonExistentFile) of
        false ->
            io:format("   File not found detected correctly: OK~n");
        true ->
            io:format("   ERROR: File should not exist~n")
    end.

%% Test process cleanup
test_process_cleanup() ->
    io:format("~n--- Test: Process Cleanup ---~n"),
    
    % Create a test process
    TestProcess = spawn(fun() -> 
        receive
            terminate -> ok;
            _ -> timer:sleep(5000)
        end
    end),
    
    case is_process_alive(TestProcess) of
        true ->
            io:format("   Test process created: OK~n"),
            
            % Terminate the process
            TestProcess ! terminate,
            timer:sleep(100),
            
            case is_process_alive(TestProcess) of
                false ->
                    io:format("   Process terminated correctly: OK~n");
                true ->
                    io:format("   Process still alive, force killing~n"),
                    exit(TestProcess, kill)
            end;
        false ->
            io:format("   ERROR: Test process not created~n")
    end.

%% Test ETS table cleanup
test_ets_cleanup() ->
    io:format("~n--- Test: ETS Table Cleanup ---~n"),
    
    TestTable = test_cleanup_table,
    
    % Create test table
    try
        ets:new(TestTable, [set, public, named_table]),
        ets:insert(TestTable, {test_key, test_value}),
        
        case ets:info(TestTable) of
            undefined ->
                io:format("   ERROR: Table not created~n");
            _Info ->
                io:format("   Test table created: OK~n"),
                
                % Cleanup table
                ets:delete(TestTable),
                
                case ets:info(TestTable) of
                    undefined ->
                        io:format("   Table cleaned up correctly: OK~n");
                    _StillExists ->
                        io:format("   ERROR: Table still exists~n")
                end
        end
    catch
        Error:Reason ->
            io:format("   Table test failed: ~p:~p~n", [Error, Reason])
    end.

%% Test exception handling
test_exception_handling() ->
    io:format("~n--- Test: Exception Handling ---~n"),
    
    % Test try-catch with expected error
    try
        error(test_error)
    catch
        error:test_error ->
            io:format("   Exception caught correctly: OK~n");
        Error:Reason ->
            io:format("   Unexpected exception: ~p:~p~n", [Error, Reason])
    end,
    
    % Test try-catch with function that might fail
    Result = safe_operation(divide_by_zero),
    case Result of
        {error, _} ->
            io:format("   Safe operation error handling: OK~n");
        _ ->
            io:format("   Unexpected safe operation result: ~p~n", [Result])
    end.

%% Safe operation that handles errors
safe_operation(Operation) ->
    try
        case Operation of
            divide_by_zero ->
                1 / 0;
            normal_operation ->
                1 + 1
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% Test resource validation concepts
test_resource_validation() ->
    io:format("~n--- Test: Resource Validation ---~n"),
    
    % Check memory info
    try
        MemoryInfo = erlang:memory(),
        TotalMemory = proplists:get_value(total, MemoryInfo, 0),
        
        case TotalMemory > 0 of
            true ->
                io:format("   Memory validation: ~p bytes available: OK~n", [TotalMemory]);
            false ->
                io:format("   ERROR: No memory information available~n")
        end
    catch
        MemError:MemReason ->
            io:format("   Memory validation failed: ~p:~p~n", [MemError, MemReason])
    end,
    
    % Check process info
    try
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        
        io:format("   Process validation: ~p/~p processes: OK~n", [ProcessCount, ProcessLimit])
    catch
        ProcError:ProcReason ->
            io:format("   Process validation failed: ~p:~p~n", [ProcError, ProcReason])
    end.

%% Test timeout handling
test_timeout_handling() ->
    io:format("~n--- Test: Timeout Handling ---~n"),
    
    % Create a long-running process
    LongProcess = spawn(fun() -> timer:sleep(10000) end),
    
    % Test timeout with receive
    Result = receive
        {LongProcess, response} ->
            got_response
    after 1000 ->
        timeout_occurred
    end,
    
    case Result of
        timeout_occurred ->
            io:format("   Timeout handled correctly: OK~n"),
            exit(LongProcess, kill);
        _ ->
            io:format("   Unexpected timeout result: ~p~n", [Result])
    end.

%% Run all tests
run_all_tests() ->
    io:format("=== RUNNING SIMPLE ERROR HANDLING TESTS ===~n"),
    
    test_basic_error_handling(),
    test_resource_validation(),
    test_timeout_handling(),
    
    io:format("=== ALL TESTS COMPLETE ===~n").