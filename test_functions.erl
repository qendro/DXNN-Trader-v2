-module(test_functions).
-compile(export_all).

test_available_functions() ->
    io:format("=== Testing Available Functions ===~n"),
    
    %% Test basic functions
    io:format("start_live_trading/1: ~p~n", [erlang:function_exported(live_trading_integration, start_live_trading, 1)]),
    io:format("get_system_status/0: ~p~n", [erlang:function_exported(live_trading_integration, get_system_status, 0)]),
    io:format("test_system_integration/0: ~p~n", [erlang:function_exported(live_trading_integration, test_system_integration, 0)]),
    
    %% Test supervisor functions
    io:format("start_supervisor/0: ~p~n", [erlang:function_exported(live_trading_integration, start_supervisor, 0)]),
    io:format("init/1: ~p~n", [erlang:function_exported(live_trading_integration, init, 1)]),
    
    %% Test startup functions
    io:format("execute_startup_sequence/2: ~p~n", [erlang:function_exported(live_trading_integration, execute_startup_sequence, 2)]),
    io:format("startup_step_ib_connection/0: ~p~n", [erlang:function_exported(live_trading_integration, startup_step_ib_connection, 0)]),
    io:format("startup_step_live_scape/0: ~p~n", [erlang:function_exported(live_trading_integration, startup_step_live_scape, 0)]),
    
    %% Test shutdown functions
    io:format("execute_graceful_shutdown/1: ~p~n", [erlang:function_exported(live_trading_integration, execute_graceful_shutdown, 1)]),
    io:format("graceful_shutdown/0: ~p~n", [erlang:function_exported(live_trading_integration, graceful_shutdown, 0)]),
    io:format("emergency_shutdown/0: ~p~n", [erlang:function_exported(live_trading_integration, emergency_shutdown, 0)]),
    
    %% Test utility functions
    io:format("validate_basic_requirements/0: ~p~n", [erlang:function_exported(live_trading_integration, validate_basic_requirements, 0)]),
    io:format("validate_configuration/0: ~p~n", [erlang:function_exported(live_trading_integration, validate_configuration, 0)]),
    io:format("verify_agent_exists/1: ~p~n", [erlang:function_exported(live_trading_integration, verify_agent_exists, 1)]),
    
    %% Test monitoring functions
    io:format("perform_health_check/1: ~p~n", [erlang:function_exported(live_trading_integration, perform_health_check, 1)]),
    io:format("get_comprehensive_status/1: ~p~n", [erlang:function_exported(live_trading_integration, get_comprehensive_status, 1)]),
    io:format("get_component_status/1: ~p~n", [erlang:function_exported(live_trading_integration, get_component_status, 1)]),
    
    %% Test recovery functions
    io:format("attempt_system_recovery/2: ~p~n", [erlang:function_exported(live_trading_integration, attempt_system_recovery, 2)]),
    io:format("attempt_process_restart/1: ~p~n", [erlang:function_exported(live_trading_integration, attempt_process_restart, 1)]),
    io:format("attempt_ib_reconnection/0: ~p~n", [erlang:function_exported(live_trading_integration, attempt_ib_reconnection, 0)]),
    
    %% Test cleanup functions
    io:format("cleanup_supervisor/1: ~p~n", [erlang:function_exported(live_trading_integration, cleanup_supervisor, 1)]),
    io:format("cleanup_all_resources/0: ~p~n", [erlang:function_exported(live_trading_integration, cleanup_all_resources, 0)]),
    io:format("cleanup_ets_table/1: ~p~n", [erlang:function_exported(live_trading_integration, cleanup_ets_table, 1)]),
    
    io:format("=== Function Test Complete ===~n"),
    ok.
