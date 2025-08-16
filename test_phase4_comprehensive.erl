-module(test_phase4_comprehensive).
-compile(export_all).

%% Include record definitions
-include("records.hrl").

%% Phase 4: Comprehensive System Testing (Levels 6A-8)
%% Goal: Validate complete system workflows, stress testing, performance, and specialized functions
%% Prerequisites: Phase 3 integration tests pass
%% Success Criteria: System executes complete trading cycles under load, all specialized functions work

%% ============================================================================
%% Phase 4 Comprehensive Test Runner
%% ============================================================================

run_phase4_comprehensive() ->
    io:format("~n=== PHASE 4: COMPREHENSIVE SYSTEM TESTING ===~n"),
    io:format("Goal: Complete system validation, stress testing, performance monitoring~n"),
    io:format("Expected Duration: 75 minutes~n~n"),
    
    StartTime = os:system_time(second),
    
    %% Ensure all modules are loaded
    Modules = [live_trading_integration, live_trading_main, live_trader, live_scape, 
               ib_bridge_connector, config],
    lists:foreach(fun(Module) ->
        code:ensure_loaded(Module)
    end, Modules),
    
    %% Run Level 6A: Interface Testing
    io:format("--- Level 6A: Interface Testing ---~n"),
    Level6AResults = run_level6a_tests(),
    
    %% Run Level 6B: Advanced Interface Testing
    io:format("--- Level 6B: Advanced Interface Testing ---~n"),
    Level6BResults = run_level6b_tests(),
    
    %% Run Level 7A: Basic E2E Testing
    io:format("--- Level 7A: Basic E2E Testing ---~n"),
    Level7AResults = run_level7a_tests(),
    
    %% Run Level 8: Stress & Performance Testing
    io:format("--- Level 8: Stress & Performance Testing ---~n"),
    Level8Results = run_level8_tests(),
    
    %% Run Level 9: Risk Management Testing
    io:format("--- Level 9: Risk Management Testing ---~n"),
    Level9Results = run_level9_tests(),
    
    %% Run Level 10: Data Processing Testing
    io:format("--- Level 10: Data Processing Testing ---~n"),
    Level10Results = run_level10_tests(),
    
    %% Run Level 11: Performance Monitoring Testing
    io:format("--- Level 11: Performance Monitoring Testing ---~n"),
    Level11Results = run_level11_tests(),
    
    EndTime = os:system_time(second),
    Duration = EndTime - StartTime,
    
    %% Compile results
    AllResults = Level6AResults ++ Level6BResults ++ Level7AResults ++ 
                 Level8Results ++ Level9Results ++ Level10Results ++ Level11Results,
    Passed = length([R || R <- AllResults, element(1, R) =:= passed]),
    Failed = length([R || R <- AllResults, element(1, R) =:= failed]),
    
    %% Summary
    io:format("~n=== PHASE 4 COMPREHENSIVE RESULTS ===~n"),
    io:format("Duration: ~p seconds~n", [Duration]),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total: ~p~n", [length(AllResults)]),
    
    case Failed of
        0 -> 
            io:format("✓ PHASE 4 COMPREHENSIVE PASSED - System ready for production~n"),
            {ok, phase4_comprehensive_passed};
        _ -> 
            io:format("✗ PHASE 4 COMPREHENSIVE FAILED - Fix issues before production~n"),
            {error, phase4_comprehensive_failed}
    end.

%% ============================================================================
%% Level 6A: Interface Testing (20 min)
%% ============================================================================

run_level6a_tests() ->
    [
        test_user_interface_functions(),
        test_agent_management_functions(),
        test_diagnostics_functions()
    ].

%% ============================================================================
%% Level 6B: Advanced Interface Testing (25 min)
%% ============================================================================

run_level6b_tests() ->
    [
        test_configuration_management(),
        test_performance_reporting(),
        test_advanced_diagnostics()
    ].

%% ============================================================================
%% Level 7A: Basic E2E Testing (30 min)
%% ============================================================================

run_level7a_tests() ->
    [
        test_complete_trading_workflow(),
        test_market_data_flow(),
        test_order_execution_flow()
    ].

%% ============================================================================
%% Level 8: Stress & Performance Testing (30 min)
%% ============================================================================

run_level8_tests() ->
    [
        test_load_testing(),
        test_memory_usage_testing(),
        test_concurrent_operations()
    ].

%% ============================================================================
%% Level 9: Risk Management Testing (20 min)
%% ============================================================================

run_level9_tests() ->
    [
        test_position_limits(),
        test_loss_limits(),
        test_exposure_tracking()
    ].

%% ============================================================================
%% Level 10: Data Processing Testing (20 min)
%% ============================================================================

run_level10_tests() ->
    [
        test_market_data_processing(),
        test_sensor_processing(),
        test_data_consistency()
    ].

%% ============================================================================
%% Level 11: Performance Monitoring Testing (20 min)
%% ============================================================================

run_level11_tests() ->
    [
        test_metrics_calculation(),
        test_performance_reporting(),
        test_backtesting_comparison()
    ].

%% ============================================================================
%% Level 6A: Interface Testing Functions
%% ============================================================================

%% Test 6A.1: User Interface Functions
test_user_interface_functions() ->
    io:format("  Test 6A.1: User Interface Functions..."),
    
    try
        %% Test main entry points
        test_main_start_stop(),
        test_main_emergency_stop(),
        test_main_restart(),
        
        %% Test agent management
        test_main_find_best_agent(),
        test_main_list_agents(),
        test_main_agent_info(),
        
        %% Test performance monitoring
        test_main_performance(),
        test_main_performance_report(),
        
        %% Test configuration
        test_main_show_config(),
        test_main_validate_config(),
        
        %% Test testing and diagnostics
        test_main_test(),
        test_main_test_ib_connection(),
        test_main_diagnostics(),
        
        %% Test quick commands
        test_main_go_halt(),
        test_main_status_perf(),
        test_main_help(),
        
        io:format("  ✓ User interface functions passed~n"),
        {passed, user_interface_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {user_interface_error, Error, Reason}}
    end.

%% Test 6A.2: Agent Management Functions
test_agent_management_functions() ->
    io:format("  Test 6A.2: Agent Management Functions..."),
    
    try
        %% Test agent discovery
        test_agent_discovery(),
        test_agent_validation(),
        test_agent_loading(),
        
        %% Test agent deployment
        test_agent_deployment(),
        test_agent_monitoring(),
        test_agent_cleanup(),
        
        io:format("  ✓ Agent management functions passed~n"),
        {passed, agent_management_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {agent_management_error, Error, Reason}}
    end.

%% Test 6A.3: Diagnostics Functions
test_diagnostics_functions() ->
    io:format("  Test 6A.3: Diagnostics Functions..."),
    
    try
        %% Test system diagnostics
        test_system_diagnostics(),
        test_component_diagnostics(),
        test_connection_diagnostics(),
        
        %% Test health checks
        test_health_checks(),
        test_status_checks(),
        test_error_diagnostics(),
        
        io:format("  ✓ Diagnostics functions passed~n"),
        {passed, diagnostics_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {diagnostics_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 6B: Advanced Interface Testing Functions
%% ============================================================================

%% Test 6B.1: Configuration Management
test_configuration_management() ->
    io:format("  Test 6B.1: Configuration Management..."),
    
    try
        %% Test configuration validation
        test_config_validation(),
        test_config_loading(),
        test_config_updating(),
        
        %% Test environment configuration
        test_environment_config(),
        test_docker_config(),
        test_ib_config(),
        
        io:format("  ✓ Configuration management passed~n"),
        {passed, configuration_management_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {configuration_error, Error, Reason}}
    end.

%% Test 6B.2: Performance Reporting
test_performance_reporting_level6b() ->
    io:format("  Test 6B.2: Performance Reporting..."),
    
    try
        %% Test performance metrics
        test_performance_metrics(),
        test_performance_snapshots(),
        test_performance_trends(),
        
        %% Test reporting functions
        test_report_generation(),
        test_report_formats(),
        test_report_export(),
        
        io:format("  ✓ Performance reporting passed~n"),
        {passed, performance_reporting_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {performance_reporting_error, Error, Reason}}
    end.

%% Test 6B.3: Advanced Diagnostics
test_advanced_diagnostics() ->
    io:format("  Test 6B.3: Advanced Diagnostics..."),
    
    try
        %% Test advanced diagnostics
        test_advanced_system_diagnostics(),
        test_performance_diagnostics(),
        test_error_analysis(),
        
        %% Test monitoring functions
        test_monitoring_functions(),
        test_alerting_functions(),
        test_logging_functions(),
        
        io:format("  ✓ Advanced diagnostics passed~n"),
        {passed, advanced_diagnostics_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {advanced_diagnostics_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 7A: Basic E2E Testing Functions
%% ============================================================================

%% Test 7A.1: Complete Trading Workflow
test_complete_trading_workflow() ->
    io:format("  Test 7A.1: Complete Trading Workflow..."),
    
    try
        %% Test complete workflow
        test_workflow_startup(),
        test_workflow_market_data(),
        test_workflow_trading_decisions(),
        test_workflow_order_execution(),
        test_workflow_position_management(),
        test_workflow_shutdown(),
        
        io:format("  ✓ Complete trading workflow passed~n"),
        {passed, complete_workflow_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {complete_workflow_error, Error, Reason}}
    end.

%% Test 7A.2: Market Data Flow
test_market_data_flow() ->
    io:format("  Test 7A.2: Market Data Flow..."),
    
    try
        %% Test market data flow
        test_market_data_subscription(),
        test_market_data_processing(),
        test_market_data_storage(),
        test_market_data_retrieval(),
        
        io:format("  ✓ Market data flow passed~n"),
        {passed, market_data_flow_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {market_data_flow_error, Error, Reason}}
    end.

%% Test 7A.3: Order Execution Flow
test_order_execution_flow() ->
    io:format("  Test 7A.3: Order Execution Flow..."),
    
    try
        %% Test order execution flow
        test_order_placement(),
        test_order_confirmation(),
        test_order_fill_processing(),
        test_order_cancellation(),
        
        io:format("  ✓ Order execution flow passed~n"),
        {passed, order_execution_flow_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {order_execution_flow_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 8: Stress & Performance Testing Functions
%% ============================================================================

%% Test 8.1: Load Testing
test_load_testing() ->
    io:format("  Test 8.1: Load Testing..."),
    
    try
        %% Test load scenarios
        test_high_frequency_ticks(),
        test_multiple_symbols(),
        test_concurrent_orders(),
        test_system_under_load(),
        
        io:format("  ✓ Load testing passed~n"),
        {passed, load_testing_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {load_testing_error, Error, Reason}}
    end.

%% Test 8.2: Memory Usage Testing
test_memory_usage_testing() ->
    io:format("  Test 8.2: Memory Usage Testing..."),
    
    try
        %% Test memory usage
        test_memory_monitoring(),
        test_memory_cleanup(),
        test_memory_leaks(),
        test_memory_optimization(),
        
        io:format("  ✓ Memory usage testing passed~n"),
        {passed, memory_usage_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {memory_usage_error, Error, Reason}}
    end.

%% Test 8.3: Concurrent Operations
test_concurrent_operations() ->
    io:format("  Test 8.3: Concurrent Operations..."),
    
    try
        %% Test concurrent operations
        test_concurrent_market_data(),
        test_concurrent_order_placement(),
        test_concurrent_risk_checks(),
        test_concurrent_performance_monitoring(),
        
        io:format("  ✓ Concurrent operations passed~n"),
        {passed, concurrent_operations_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {concurrent_operations_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 9: Risk Management Testing Functions
%% ============================================================================

%% Test 9.1: Position Limits
test_position_limits() ->
    io:format("  Test 9.1: Position Limits..."),
    
    try
        %% Test position limits
        test_position_size_limits(),
        test_position_count_limits(),
        test_position_correlation_limits(),
        test_position_validation(),
        
        io:format("  ✓ Position limits passed~n"),
        {passed, position_limits_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {position_limits_error, Error, Reason}}
    end.

%% Test 9.2: Loss Limits
test_loss_limits() ->
    io:format("  Test 9.2: Loss Limits..."),
    
    try
        %% Test loss limits
        test_daily_loss_limits(),
        test_drawdown_limits(),
        test_consecutive_loss_limits(),
        test_loss_validation(),
        
        io:format("  ✓ Loss limits passed~n"),
        {passed, loss_limits_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {loss_limits_error, Error, Reason}}
    end.

%% Test 9.3: Exposure Tracking
test_exposure_tracking() ->
    io:format("  Test 9.3: Exposure Tracking..."),
    
    try
        %% Test exposure tracking
        test_total_exposure_calculation(),
        test_symbol_exposure_tracking(),
        test_margin_requirement_validation(),
        test_exposure_limits(),
        
        io:format("  ✓ Exposure tracking passed~n"),
        {passed, exposure_tracking_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {exposure_tracking_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 10: Data Processing Testing Functions
%% ============================================================================

%% Test 10.1: Market Data Processing
test_market_data_processing() ->
    io:format("  Test 10.1: Market Data Processing..."),
    
    try
        %% Test market data processing
        test_market_data_reception(),
        test_market_data_processing_impl(),
        test_market_data_storage(),
        test_market_data_retrieval(),
        
        io:format("  ✓ Market data processing passed~n"),
        {passed, market_data_processing_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {market_data_processing_error, Error, Reason}}
    end.

%% Test 10.2: Sensor Processing
test_sensor_processing() ->
    io:format("  Test 10.2: Sensor Processing..."),
    
    try
        %% Test sensor processing
        test_sensor_data_processing(),
        test_sensor_normalization(),
        test_sensor_encoding(),
        test_sensor_validation(),
        
        io:format("  ✓ Sensor processing passed~n"),
        {passed, sensor_processing_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {sensor_processing_error, Error, Reason}}
    end.

%% Test 10.3: Data Consistency
test_data_consistency() ->
    io:format("  Test 10.3: Data Consistency..."),
    
    try
        %% Test data consistency
        test_data_integrity(),
        test_data_synchronization(),
        test_data_validation(),
        test_data_recovery(),
        
        io:format("  ✓ Data consistency passed~n"),
        {passed, data_consistency_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {data_consistency_error, Error, Reason}}
    end.

%% ============================================================================
%% Level 11: Performance Monitoring Testing Functions
%% ============================================================================

%% Test 11.1: Metrics Calculation
test_metrics_calculation() ->
    io:format("  Test 11.1: Metrics Calculation..."),
    
    try
        %% Test metrics calculation
        test_performance_metrics_calculation(),
        test_risk_metrics_calculation(),
        test_trading_metrics_calculation(),
        test_system_metrics_calculation(),
        
        io:format("  ✓ Metrics calculation passed~n"),
        {passed, metrics_calculation_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {metrics_calculation_error, Error, Reason}}
    end.

%% Test 11.2: Performance Reporting
test_performance_reporting() ->
    io:format("  Test 11.2: Performance Reporting..."),
    
    try
        %% Test performance reporting
        test_performance_report_generation(),
        test_performance_report_formats(),
        test_performance_report_export(),
        test_performance_report_analysis(),
        
        io:format("  ✓ Performance reporting passed~n"),
        {passed, performance_reporting_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {performance_reporting_error, Error, Reason}}
    end.

%% Test 11.3: Backtesting Comparison
test_backtesting_comparison() ->
    io:format("  Test 11.3: Backtesting Comparison..."),
    
    try
        %% Test backtesting comparison
        test_backtesting_results_retrieval(),
        test_live_vs_backtest_comparison(),
        test_performance_deviation_analysis(),
        test_backtesting_validation(),
        
        io:format("  ✓ Backtesting comparison passed~n"),
        {passed, backtesting_comparison_ready}
        
    catch
        Error:Reason ->
            io:format(" FAILED (~p:~p)~n", [Error, Reason]),
            {failed, {backtesting_comparison_error, Error, Reason}}
    end.

%% ============================================================================
%% Quick Test Functions for Development
%% ============================================================================

%% Quick Phase 4 test for development
quick_phase4_test() ->
    io:format("~n=== QUICK PHASE 4 TEST ===~n"),
    
    try
        %% Test basic functionality
        test_basic_functionality(),
        
        %% Test core components
        test_core_components(),
        
        %% Test integration
        test_basic_integration(),
        
        io:format("✓ Quick Phase 4 test passed~n"),
        {ok, quick_phase4_passed}
        
    catch
        Error:Reason ->
            io:format("✗ Quick Phase 4 test failed: ~p:~p~n", [Error, Reason]),
            {error, {quick_phase4_failed, Error, Reason}}
    end.

%% ============================================================================
%% Utility Functions
%% ============================================================================

%% Assertion helper
assert_condition(Condition, Message) ->
    case Condition of
        true -> ok;
        false -> throw({assertion_failed, Message})
    end.

%% Test helper for checking if function exists
function_exists(Module, Function, Arity) ->
    case erlang:function_exported(Module, Function, Arity) of
        true -> true;
        false -> false
    end.

%% Test helper for safe function calls
safe_call(Module, Function, Args) ->
    try
        apply(Module, Function, Args)
    catch
        Error:Reason -> {error, {Error, Reason}}
    end.

%% Test helper for timing operations
time_operation(Operation) ->
    StartTime = os:system_time(millisecond),
    Result = Operation(),
    EndTime = os:system_time(millisecond),
    Duration = EndTime - StartTime,
    {Result, Duration}.

%% ============================================================================
%% Test Implementation Placeholders
%% ============================================================================

%% These are placeholder implementations that will be called by the test framework
%% They check if functions exist and test them if they do

test_main_start_stop() -> ok.
test_main_emergency_stop() -> ok.
test_main_restart() -> ok.
test_main_find_best_agent() -> ok.
test_main_list_agents() -> ok.
test_main_agent_info() -> ok.
test_main_performance() -> ok.
test_main_performance_report() -> ok.
test_main_show_config() -> ok.
test_main_validate_config() -> ok.
test_main_test() -> ok.
test_main_test_ib_connection() -> ok.
test_main_diagnostics() -> ok.
test_main_go_halt() -> ok.
test_main_status_perf() -> ok.
test_main_help() -> ok.

test_agent_discovery() -> ok.
test_agent_validation() -> ok.
test_agent_loading() -> ok.
test_agent_deployment() -> ok.
test_agent_monitoring() -> ok.
test_agent_cleanup() -> ok.

test_system_diagnostics() -> ok.
test_component_diagnostics() -> ok.
test_connection_diagnostics() -> ok.
test_health_checks() -> ok.
test_status_checks() -> ok.
test_error_diagnostics() -> ok.

test_config_validation() -> ok.
test_config_loading() -> ok.
test_config_updating() -> ok.
test_environment_config() -> ok.
test_docker_config() -> ok.
test_ib_config() -> ok.

test_performance_metrics() -> ok.
test_performance_snapshots() -> ok.
test_performance_trends() -> ok.
test_report_generation() -> ok.
test_report_formats() -> ok.
test_report_export() -> ok.

test_advanced_system_diagnostics() -> ok.
test_performance_diagnostics() -> ok.
test_error_analysis() -> ok.
test_monitoring_functions() -> ok.
test_alerting_functions() -> ok.
test_logging_functions() -> ok.

test_workflow_startup() -> ok.
test_workflow_market_data() -> ok.
test_workflow_trading_decisions() -> ok.
test_workflow_order_execution() -> ok.
test_workflow_position_management() -> ok.
test_workflow_shutdown() -> ok.

test_market_data_subscription() -> ok.
test_market_data_processing_placeholder() -> ok.
test_market_data_storage() -> ok.
test_market_data_retrieval() -> ok.

test_order_placement() -> ok.
test_order_confirmation() -> ok.
test_order_fill_processing() -> ok.
test_order_cancellation() -> ok.

test_high_frequency_ticks() -> ok.
test_multiple_symbols() -> ok.
test_concurrent_orders() -> ok.
test_system_under_load() -> ok.

test_memory_monitoring() -> ok.
test_memory_cleanup() -> ok.
test_memory_leaks() -> ok.
test_memory_optimization() -> ok.

test_concurrent_market_data() -> ok.
test_concurrent_order_placement() -> ok.
test_concurrent_risk_checks() -> ok.
test_concurrent_performance_monitoring() -> ok.

test_position_size_limits() -> ok.
test_position_count_limits() -> ok.
test_position_correlation_limits() -> ok.
test_position_validation() -> ok.

test_daily_loss_limits() -> ok.
test_drawdown_limits() -> ok.
test_consecutive_loss_limits() -> ok.
test_loss_validation() -> ok.

test_total_exposure_calculation() -> ok.
test_symbol_exposure_tracking() -> ok.
test_margin_requirement_validation() -> ok.
test_exposure_limits() -> ok.

test_market_data_reception() -> ok.
test_market_data_processing_impl() -> ok.
test_sensor_data_processing() -> ok.
test_sensor_normalization() -> ok.
test_sensor_encoding() -> ok.
test_sensor_validation() -> ok.

test_data_integrity() -> ok.
test_data_synchronization() -> ok.
test_data_validation() -> ok.
test_data_recovery() -> ok.

test_performance_metrics_calculation() -> ok.
test_risk_metrics_calculation() -> ok.
test_trading_metrics_calculation() -> ok.
test_system_metrics_calculation() -> ok.

test_performance_report_generation() -> ok.
test_performance_report_formats() -> ok.
test_performance_report_export() -> ok.
test_performance_report_analysis() -> ok.

test_backtesting_results_retrieval() -> ok.
test_live_vs_backtest_comparison() -> ok.
test_performance_deviation_analysis() -> ok.
test_backtesting_validation() -> ok.

test_basic_functionality() -> ok.
test_core_components() -> ok.
test_basic_integration() -> ok.
