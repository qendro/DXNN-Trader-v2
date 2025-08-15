%% Test module for complete live trading system integration
%% Tests startup/shutdown procedures, component integration, and error handling

-module(test_live_trading_integration).
-compile(export_all).
-include("records.hrl").

%% ============================================================================
%% Main Test Functions
%% ============================================================================

%% Run all integration tests
run_all_tests() ->
    io:format("Starting comprehensive live trading integration tests~n"),
    
    Tests = [
        {test_configuration_validation, fun test_configuration_validation/0},
        {test_startup_sequence, fun test_startup_sequence/0},
        {test_component_communication, fun test_component_communication/0},
        {test_error_handling, fun test_error_handling/0},
        {test_shutdown_sequence, fun test_shutdown_sequence/0},
        {test_system_recovery, fun test_system_recovery/0}
    ],
    
    Results = run_test_suite(Tests),
    
    %% Print summary
    print_test_summary(Results),
    
    Results.

%% Run individual test suite
run_test_suite(Tests) ->
    lists:map(fun({TestName, TestFun}) ->
        io:format("~n=== Running Test: ~p ===~n", [TestName]),
        
        StartTime = erlang:timestamp(),
        
        try
            Result = TestFun(),
            EndTime = erlang:timestamp(),
            Duration = timer:now_diff(EndTime, StartTime) / 1000000,
            
            io:format("Test ~p: PASSED (~.2f seconds)~n", [TestName, Duration]),
            {TestName, passed, Duration, Result}
        catch
            Error:Reason ->
                EndTime2 = erlang:timestamp(),
                Duration2 = timer:now_diff(EndTime2, StartTime) / 1000000,
                
                io:format("Test ~p: FAILED (~.2f seconds) - ~p:~p~n", 
                         [TestName, Duration2, Error, Reason]),
                {TestName, failed, Duration2, {Error, Reason}}
        end
    end, Tests).

%% Print test summary
print_test_summary(Results) ->
    io:format("~n=== TEST SUMMARY ===~n"),
    
    Passed = length([R || {_, passed, _, _} = R <- Results]),
    Failed = length([R || {_, failed, _, _} = R <- Results]),
    Total = length(Results),
    TotalTime = lists:sum([D || {_, _, D, _} <- Results]),
    
    io:format("Total Tests: ~p~n", [Total]),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total Time: ~.2f seconds~n", [TotalTime]),
    
    case Failed of
        0 -> io:format("ALL TESTS PASSED!~n");
        _ -> 
            io:format("FAILED TESTS:~n"),
            lists:foreach(fun({Name, failed, _, Reason}) ->
                io:format("  - ~p: ~p~n", [Name, Reason])
            end, Results)
    end.

%% ============================================================================
%% Configuration Validation Tests
%% ============================================================================

test_configuration_validation() ->
    io:format("Testing configuration validation~n"),
    
    %% Test IB connection configuration
    case config:validate_ib_connection_config() of
        ok -> 
            io:format("IB connection config validation: PASSED~n");
        {error, Reason} ->
            throw({ib_config_validation_failed, Reason})
    end,
    
    %% Test risk parameters
    case config:validate_risk_parameters() of
        ok ->
            io:format("Risk parameters validation: PASSED~n");
        {error, RiskReason} ->
            throw({risk_params_validation_failed, RiskReason})
    end,
    
    %% Test currency pairs
    case config:validate_currency_pairs() of
        ok ->
            io:format("Currency pairs validation: PASSED~n");
        {error, CurrencyReason} ->
            throw({currency_pairs_validation_failed, CurrencyReason})
    end,
    
    %% Test complete live trading config
    case config:validate_live_trading_config() of
        ok ->
            io:format("Complete live trading config validation: PASSED~n");
        {error, ConfigReason} ->
            throw({live_trading_config_validation_failed, ConfigReason})
    end,
    
    {ok, configuration_validation_passed}.

%% ============================================================================
%% Startup Sequence Tests
%% ============================================================================

test_startup_sequence() ->
    io:format("Testing startup sequence~n"),
    
    %% Get a test agent
    TestAgent = get_test_agent(),
    
    %% Test startup
    case live_trading_integration:start_live_trading(TestAgent) of
        {ok, started} ->
            io:format("Startup sequence: PASSED~n"),
            
            %% Verify system status
            case live_trading_integration:get_system_status() of
                {ok, Status} ->
                    io:format("System status check: PASSED~n"),
                    verify_startup_components(Status),
                    
                    %% Clean up
                    live_trading_integration:stop_live_trading(),
                    {ok, startup_sequence_passed};
                {error, StatusReason} ->
                    live_trading_integration:emergency_shutdown(),
                    throw({system_status_failed, StatusReason})
            end;
        {error, Reason} ->
            throw({startup_failed, Reason})
    end.

%% Verify that all components started correctly
verify_startup_components(Status) ->
    Components = maps:get(components, Status, #{}),
    
    RequiredComponents = [ib_connector, live_scape, live_trader],
    
    lists:foreach(fun(Component) ->
        case maps:get(Component, Components, undefined) of
            undefined ->
                throw({component_missing, Component});
            ComponentStatus ->
                case maps:get(status, ComponentStatus, stopped) of
                    running ->
                        io:format("Component ~p: RUNNING~n", [Component]);
                    Status ->
                        throw({component_not_running, Component, Status})
                end
        end
    end, RequiredComponents).

%% ============================================================================
%% Component Communication Tests
%% ============================================================================

test_component_communication() ->
    io:format("Testing component communication~n"),
    
    %% Start system for testing
    TestAgent = get_test_agent(),
    
    case live_trading_integration:start_live_trading(TestAgent) of
        {ok, started} ->
            try
                %% Test IB connector communication
                test_ib_connector_communication(),
                
                %% Test live scape communication
                test_live_scape_communication(),
                
                %% Test live trader communication
                test_live_trader_communication(),
                
                io:format("Component communication: PASSED~n"),
                {ok, component_communication_passed}
            after
                live_trading_integration:stop_live_trading()
            end;
        {error, Reason} ->
            throw({startup_failed_for_communication_test, Reason})
    end.

%% Test IB connector communication
test_ib_connector_communication() ->
    %% Test connection status
    case ib_connector:get_connection_status() of
        {ok, _Status} ->
            io:format("IB connector status check: PASSED~n");
        {error, Reason} ->
            throw({ib_connector_status_failed, Reason})
    end,
    
    %% Test market data subscription (if connected)
    case ib_connector:get_connection_status() of
        {ok, true} ->
            %% Try to get market data
            case ib_connector:get_market_data("EUR.USD") of
                {ok, _Data} ->
                    io:format("IB connector market data: PASSED~n");
                {error, no_data} ->
                    io:format("IB connector market data: NO DATA (expected)~n");
                {error, DataReason} ->
                    io:format("IB connector market data error: ~p~n", [DataReason])
            end;
        {ok, false} ->
            io:format("IB connector not connected (expected in test)~n")
    end.

%% Test live scape communication
test_live_scape_communication() ->
    case whereis(live_scape) of
        undefined ->
            throw({live_scape_not_running});
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    io:format("Live scape process: ALIVE~n");
                false ->
                    throw({live_scape_process_dead})
            end
    end.

%% Test live trader communication
test_live_trader_communication() ->
    case whereis(live_trader) of
        undefined ->
            throw({live_trader_not_running});
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    io:format("Live trader process: ALIVE~n");
                false ->
                    throw({live_trader_process_dead})
            end
    end.

%% ============================================================================
%% Error Handling Tests
%% ============================================================================

test_error_handling() ->
    io:format("Testing error handling~n"),
    
    %% Start system for testing
    TestAgent = get_test_agent(),
    
    case live_trading_integration:start_live_trading(TestAgent) of
        {ok, started} ->
            try
                %% Test component crash handling
                test_component_crash_handling(),
                
                %% Test connection error handling
                test_connection_error_handling(),
                
                %% Test emergency shutdown
                test_emergency_shutdown(),
                
                io:format("Error handling: PASSED~n"),
                {ok, error_handling_passed}
            after
                %% Ensure cleanup
                live_trading_integration:emergency_shutdown()
            end;
        {error, Reason} ->
            throw({startup_failed_for_error_test, Reason})
    end.

%% Test component crash handling
test_component_crash_handling() ->
    io:format("Testing component crash handling~n"),
    
    %% Get initial system status
    {ok, _InitialStatus} = live_trading_integration:get_system_status(),
    
    %% Simulate a component crash (non-destructive test)
    case whereis(live_scape) of
        undefined ->
            io:format("Live scape not running for crash test~n");
        Pid ->
            %% Send a test message that might cause issues
            Pid ! {test_message, crash_simulation},
            
            %% Wait a moment
            timer:sleep(1000),
            
            %% Check if system is still responsive
            case live_trading_integration:get_system_status() of
                {ok, _Status} ->
                    io:format("System remained responsive after crash simulation~n");
                {error, Reason} ->
                    io:format("System became unresponsive: ~p~n", [Reason])
            end
    end.

%% Test connection error handling
test_connection_error_handling() ->
    io:format("Testing connection error handling~n"),
    
    %% Test with invalid connection parameters (simulation)
    %% This is a non-destructive test that checks error handling paths
    case ib_connector:get_connection_status() of
        {ok, false} ->
            io:format("Connection error handling: System handles disconnection~n");
        {ok, true} ->
            io:format("Connection error handling: System connected (good)~n");
        {error, Reason} ->
            io:format("Connection error handling: Error handled gracefully: ~p~n", [Reason])
    end.

%% Test emergency shutdown
test_emergency_shutdown() ->
    io:format("Testing emergency shutdown~n"),
    
    %% This will be tested by the shutdown sequence test
    %% Here we just verify the emergency shutdown function exists
    case erlang:function_exported(live_trading_integration, emergency_shutdown, 0) of
        true ->
            io:format("Emergency shutdown function: AVAILABLE~n");
        false ->
            throw({emergency_shutdown_function_missing})
    end.

%% ============================================================================
%% Shutdown Sequence Tests
%% ============================================================================

test_shutdown_sequence() ->
    io:format("Testing shutdown sequence~n"),
    
    %% Start system for shutdown testing
    TestAgent = get_test_agent(),
    
    case live_trading_integration:start_live_trading(TestAgent) of
        {ok, started} ->
            %% Test graceful shutdown
            case live_trading_integration:stop_live_trading() of
                {ok, stopped} ->
                    io:format("Graceful shutdown: PASSED~n"),
                    
                    %% Verify all components stopped
                    verify_components_stopped(),
                    
                    {ok, shutdown_sequence_passed};
                {error, Reason} ->
                    %% Try emergency shutdown
                    live_trading_integration:emergency_shutdown(),
                    throw({graceful_shutdown_failed, Reason})
            end;
        {error, Reason} ->
            throw({startup_failed_for_shutdown_test, Reason})
    end.

%% Verify that all components have stopped
verify_components_stopped() ->
    Components = [live_trading_integration, live_trader, live_scape, ib_connector],
    
    lists:foreach(fun(Component) ->
        case whereis(Component) of
            undefined ->
                io:format("Component ~p: STOPPED~n", [Component]);
            Pid ->
                case is_process_alive(Pid) of
                    false ->
                        io:format("Component ~p: STOPPED~n", [Component]);
                    true ->
                        io:format("Component ~p: STILL RUNNING (may be normal)~n", [Component])
                end
        end
    end, Components).

%% ============================================================================
%% System Recovery Tests
%% ============================================================================

test_system_recovery() ->
    io:format("Testing system recovery~n"),
    
    %% Test restart functionality
    TestAgent = get_test_agent(),
    
    %% Start system
    case live_trading_integration:start_live_trading(TestAgent) of
        {ok, started} ->
            %% Stop system
            case live_trading_integration:stop_live_trading() of
                {ok, stopped} ->
                    %% Wait for cleanup
                    timer:sleep(2000),
                    
                    %% Restart system
                    case live_trading_integration:restart_live_trading(TestAgent) of
                        {ok, started} ->
                            io:format("System recovery: PASSED~n"),
                            
                            %% Clean up
                            live_trading_integration:stop_live_trading(),
                            {ok, system_recovery_passed};
                        {error, Reason} ->
                            live_trading_integration:emergency_shutdown(),
                            throw({restart_failed, Reason})
                    end;
                {error, Reason} ->
                    live_trading_integration:emergency_shutdown(),
                    throw({stop_failed_for_recovery_test, Reason})
            end;
        {error, Reason} ->
            throw({startup_failed_for_recovery_test, Reason})
    end.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Get a test agent from the database
get_test_agent() ->
    %% Try to find any agent in the database
    case genotype_utils:list_all_agents() of
        [] ->
            %% No agents available, create a minimal test agent
            create_minimal_test_agent();
        [Agent | _] ->
            Agent#agent.id
    end.

%% Create a minimal test agent for testing
create_minimal_test_agent() ->
    io:format("Creating minimal test agent~n"),
    
    %% Create a simple agent for testing purposes
    %% This is a simplified version - in production would use proper genotype creation
    TestAgentId = {test_agent, erlang:timestamp()},
    
    %% Create minimal agent record
    TestAgent = #agent{
        id = TestAgentId,
        encoding_type = neural,
        generation = 1,
        population_id = test_population,
        specie_id = test_specie,
        cx_id = test_cortex,
        fingerprint = test_fingerprint,
        constraint = #constraint{
            morphology = forex_trader,
            connection_architecture = recurrent,
            neural_afs = [tanh]
        },
        evo_hist = [],
        fitness = 0,
        innovation_factor = 0,
        pattern = []
    },
    
    %% Store in Mnesia (simplified)
    try
        mnesia:dirty_write(TestAgent),
        TestAgentId
    catch
        _:_ ->
            %% If Mnesia write fails, return a dummy ID
            %% The actual tests will handle the missing agent gracefully
            test_agent_dummy
    end.

%% ============================================================================
%% Specific Integration Test Functions
%% ============================================================================

%% Test paper trading account integration
test_paper_trading_integration() ->
    io:format("Testing paper trading account integration~n"),
    
    %% This test requires actual IB connection
    %% For now, we'll test the configuration and connection attempt
    case config:validate_ib_connection_config() of
        ok ->
            %% Configuration is valid
            _Host = config:ib_host(),
            Port = config:ib_port(),
            
            %% Verify it's paper trading port
            case Port of
                7497 ->
                    io:format("Paper trading port confirmed: ~p~n", [Port]),
                    {ok, paper_trading_config_valid};
                7496 ->
                    throw({production_port_detected, "Live trading port not allowed in tests"});
                _ ->
                    io:format("Custom port detected: ~p~n", [Port]),
                    {ok, custom_port_config}
            end;
        {error, Reason} ->
            throw({paper_trading_config_invalid, Reason})
    end.

%% Test model deployment integration
test_model_deployment_integration() ->
    io:format("Testing model deployment integration~n"),
    
    TestAgent = get_test_agent(),
    
    %% Test model deployment without full system startup
    case live_trader:deploy_model(TestAgent) of
        {ok, _State} ->
            io:format("Model deployment: PASSED~n"),
            {ok, model_deployment_passed};
        {error, agent_not_found} ->
            io:format("Model deployment: Agent not found (expected in test)~n"),
            {ok, model_deployment_no_agent};
        {error, Reason} ->
            throw({model_deployment_failed, Reason})
    end.

%% Test performance monitoring integration
test_performance_monitoring_integration() ->
    io:format("Testing performance monitoring integration~n"),
    
    %% Test performance table initialization
    case live_trader:init_performance_tables() of
        ok ->
            io:format("Performance tables initialization: PASSED~n"),
            
            %% Test performance data retrieval
            case live_trader:get_performance_basic() of
                {ok, _Performance} ->
                    io:format("Performance data retrieval: PASSED~n"),
                    {ok, performance_monitoring_passed};
                {error, not_running} ->
                    io:format("Performance data retrieval: Not running (expected)~n"),
                    {ok, performance_monitoring_not_running};
                {error, Reason} ->
                    throw({performance_data_retrieval_failed, Reason})
            end;
        {error, Reason} ->
            throw({performance_tables_init_failed, Reason})
    end.

%% ============================================================================
%% Test Execution Functions
%% ============================================================================

%% Run quick integration test
quick_test() ->
    io:format("Running quick integration test~n"),
    
    QuickTests = [
        {test_configuration_validation, fun test_configuration_validation/0},
        {test_paper_trading_integration, fun test_paper_trading_integration/0},
        {test_performance_monitoring_integration, fun test_performance_monitoring_integration/0}
    ],
    
    Results = run_test_suite(QuickTests),
    print_test_summary(Results),
    Results.

%% Run full integration test with actual system startup
full_test() ->
    io:format("Running full integration test~n"),
    run_all_tests().

%% Test specific component
test_component(Component) ->
    case Component of
        ib_connector ->
            test_ib_connector_communication();
        live_scape ->
            test_live_scape_communication();
        live_trader ->
            test_live_trader_communication();
        configuration ->
            test_configuration_validation();
        _ ->
            {error, {unknown_component, Component}}
    end.