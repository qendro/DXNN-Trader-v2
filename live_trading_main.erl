%% Main Entry Point for Live Trading System
%% Provides simple interface for starting, stopping, and managing live trading

-module(live_trading_main).
-compile(export_all).
-include("records.hrl").

%% ============================================================================
%% Main Entry Points
%% ============================================================================

%% Start live trading with best available agent
start() ->
    io:format("Starting live trading system with best available agent~n"),
    
    case find_best_agent() of
        {ok, AgentId} ->
            start_with_agent(AgentId);
        {error, Reason} ->
            io:format("Cannot start live trading: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Start live trading with specific agent
start_with_agent(AgentId) ->
    io:format("Starting live trading system with Agent ~p~n", [AgentId]),
    
    %% Validate configuration first
    case config:validate_live_trading_config() of
        ok ->
            %% Start the system
            case live_trading_integration:start_live_trading(AgentId) of
                {ok, started} ->
                    print_startup_success(AgentId),
                    {ok, started};
                {error, Reason} ->
                    io:format("Failed to start live trading: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, ConfigError} ->
            io:format("Configuration validation failed: ~p~n", [ConfigError]),
            {error, {config_invalid, ConfigError}}
    end.

%% Stop live trading system
stop() ->
    io:format("Stopping live trading system~n"),
    
    case live_trading_integration:stop_live_trading() of
        {ok, stopped} ->
            print_shutdown_success(),
            {ok, stopped};
        {error, Reason} ->
            io:format("Error during shutdown: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Emergency stop - immediate shutdown
emergency_stop() ->
    io:format("EMERGENCY STOP - Immediate shutdown~n"),
    
    case live_trading_integration:emergency_shutdown() of
        {ok, emergency_stopped} ->
            print_emergency_stop_success(),
            {ok, emergency_stopped};
        {error, Reason} ->
            io:format("Error during emergency stop: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Restart live trading system
restart() ->
    io:format("Restarting live trading system~n"),
    
    case find_best_agent() of
        {ok, AgentId} ->
            restart_with_agent(AgentId);
        {error, Reason} ->
            io:format("Cannot restart live trading: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Restart with specific agent
restart_with_agent(AgentId) ->
    case live_trading_integration:restart_live_trading(AgentId) of
        {ok, started} ->
            print_restart_success(AgentId),
            {ok, started};
        {error, Reason} ->
            io:format("Failed to restart live trading: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Get system status
status() ->
    case live_trading_integration:get_system_status() of
        {ok, Status} ->
            print_system_status(Status),
            {ok, Status};
        {error, Reason} ->
            io:format("Cannot get system status: ~p~n", [Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Agent Management
%% ============================================================================

%% Find the best available agent for trading
find_best_agent() ->
    case genotype_utils:list_all_agents(all) of
        {atomic, []} ->
            {error, no_agents_available};
        {atomic, AgentTuples} ->
            %% AgentTuples are in format {AgentId, Fitness, Generation, SpecieId}
            %% Sort by fitness (already sorted by genotype_utils, but ensure descending order)
            SortedAgents = lists:sort(fun({_, F1, _, _}, {_, F2, _, _}) -> 
                F1 > F2 
            end, AgentTuples),
            
            case SortedAgents of
                [{BestAgentId, _, _, _} | _] ->
                    {ok, BestAgentId};
                [] ->
                    {error, no_valid_agents}
            end;
        {aborted, Reason} ->
            {error, {database_error, Reason}}
    end.

%% List available agents
list_agents() ->
    case genotype_utils:list_all_agents(all) of
        {atomic, []} ->
            io:format("No agents available~n"),
            {ok, []};
        {atomic, AgentTuples} ->
            io:format("Available agents:~n"),
            lists:foreach(fun({AgentId, Fitness, Generation, SpecieId}) ->
                io:format("  Agent ~p: Fitness ~p, Generation ~p, Specie ~p~n", 
                         [AgentId, Fitness, Generation, SpecieId])
            end, AgentTuples),
            {ok, AgentTuples};
        {aborted, Reason} ->
            io:format("Database error: ~p~n", [Reason]),
            {error, {database_error, Reason}}
    end.

%% Get agent details
agent_info(AgentId) ->
    case genotype:dirty_read({agent, AgentId}) of
        undefined ->
            io:format("Agent ~p not found~n", [AgentId]),
            {error, agent_not_found};
        Agent ->
            print_agent_info(Agent),
            {ok, Agent}
    end.

%% ============================================================================
%% Performance Monitoring
%% ============================================================================

%% Get current performance
performance() ->
    case live_trader:get_performance_basic() of
        {ok, Performance} ->
            print_performance_summary(Performance),
            {ok, Performance};
        {error, Reason} ->
            io:format("Cannot get performance data: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Get detailed performance report
performance_report() ->
    case live_trader:get_performance_report() of
        {ok, Report} ->
            print_detailed_performance(Report),
            {ok, Report};
        {error, Reason} ->
            io:format("Cannot get performance report: ~p~n", [Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Configuration Management
%% ============================================================================

%% Show current configuration
show_config() ->
    Config = config:get_live_trading_config(),
    print_configuration(Config),
    {ok, Config}.

%% Validate configuration
validate_config() ->
    case config:validate_live_trading_config() of
        ok ->
            io:format("Configuration validation: PASSED~n"),
            {ok, valid};
        {error, Reason} ->
            io:format("Configuration validation: FAILED~n"),
            io:format("Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Testing and Diagnostics
%% ============================================================================

%% Run system tests
test() ->
    io:format("Running live trading system tests~n"),
    test_live_trading_integration:quick_test().

%% Run full system tests
test_full() ->
    io:format("Running full live trading system tests~n"),
    test_live_trading_integration:full_test().

%% Test specific component
test_component(Component) ->
    test_live_trading_integration:test_component(Component).

%% Test IB connection specifically
test_ib_connection() ->
    io:format("Testing IB connection...~n"),
    
    %% Test basic connectivity
    case ib_connector:test_connectivity() of
        ok ->
            io:format("✓ Basic connectivity test passed~n"),
            
            %% Test full connection
            Host = config:ib_host(),
            Port = config:ib_port(),
            ClientId = config:ib_client_id(),
            
            io:format("Attempting full connection to ~s:~p with client ID ~p~n", [Host, Port, ClientId]),
            
            case ib_connector:start_connection(Host, Port, ClientId) of
                {ok, _Pid} ->
                    io:format("✓ Full connection test passed~n"),
                    
                    %% Clean up
                    ib_connector:stop_connection(),
                    {ok, connection_successful};
                {error, Reason} ->
                    io:format("✗ Full connection test failed: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("✗ Basic connectivity test failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Run diagnostics
diagnostics() ->
    io:format("Running system diagnostics~n"),
    
    %% Check configuration
    ConfigResult = validate_config(),
    
    %% Check database connectivity
    DatabaseResult = check_database_connectivity(),
    
    %% Check IB connectivity
    IBConnectivityResult = check_ib_connectivity(),
    
    %% Check system status
    StatusResult = status(),
    
    %% Compile results
    Results = #{
        configuration => ConfigResult,
        database => DatabaseResult,
        ib_connectivity => IBConnectivityResult,
        system_status => StatusResult
    },
    
    print_diagnostics_summary(Results),
    {ok, Results}.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Check database connectivity
check_database_connectivity() ->
    try
        %% Try to access Mnesia
        case mnesia:system_info(is_running) of
            yes ->
                io:format("Database connectivity: OK~n"),
                {ok, connected};
            no ->
                io:format("Database connectivity: Mnesia not running~n"),
                {error, mnesia_not_running};
            _ ->
                io:format("Database connectivity: Unknown status~n"),
                {error, unknown_status}
        end
    catch
        _:Reason ->
            io:format("Database connectivity: ERROR - ~p~n", [Reason]),
            {error, Reason}
    end.

%% Check IB connectivity
check_ib_connectivity() ->
    try
        io:format("Testing IB connectivity...~n"),
        case ib_connector:test_connectivity() of
            ok ->
                io:format("IB connectivity: OK~n"),
                {ok, connected};
            {error, Reason} ->
                io:format("IB connectivity: ERROR - ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        _:Error ->
            io:format("IB connectivity: ERROR - ~p~n", [Error]),
            {error, Error}
    end.

%% Print startup success message
print_startup_success(AgentId) ->
    io:format("~n=== LIVE TRADING STARTED ===~n"),
    io:format("Agent ID: ~p~n", [AgentId]),
    io:format("IB Host: ~s~n", [config:ib_host()]),
    io:format("IB Port: ~p~n", [config:ib_port()]),
    io:format("Currency Pairs: ~p~n", [config:live_currency_pairs()]),
    io:format("Position Size: ~p%~n", [config:live_position_size() * 100]),
    io:format("Max Daily Loss: ~p%~n", [config:live_max_daily_loss() * 100]),
    io:format("===========================~n").

%% Print shutdown success message
print_shutdown_success() ->
    io:format("~n=== LIVE TRADING STOPPED ===~n"),
    io:format("System shutdown completed successfully~n"),
    io:format("All positions closed~n"),
    io:format("Resources cleaned up~n"),
    io:format("============================~n").

%% Print emergency stop success message
print_emergency_stop_success() ->
    io:format("~n=== EMERGENCY STOP COMPLETED ===~n"),
    io:format("System immediately shut down~n"),
    io:format("All processes terminated~n"),
    io:format("Manual position check recommended~n"),
    io:format("=================================~n").

%% Print restart success message
print_restart_success(AgentId) ->
    io:format("~n=== LIVE TRADING RESTARTED ===~n"),
    io:format("Agent ID: ~p~n", [AgentId]),
    io:format("System restarted successfully~n"),
    io:format("==============================~n").

%% Print system status
print_system_status(Status) ->
    io:format("~n=== SYSTEM STATUS ===~n"),
    io:format("Status: ~p~n", [maps:get(status, Status, unknown)]),
    io:format("Agent ID: ~p~n", [maps:get(agent_id, Status, none)]),
    
    case maps:get(uptime_seconds, Status, 0) of
        0 -> io:format("Uptime: Not running~n");
        Uptime -> io:format("Uptime: ~.1f seconds~n", [Uptime])
    end,
    
    Components = maps:get(components, Status, #{}),
    io:format("Components:~n"),
    case Components of
        #{} when map_size(Components) > 0 ->
            maps:fold(fun(Name, ComponentStatus, _) ->
                ComponentState = maps:get(status, ComponentStatus, unknown),
                io:format("  ~p: ~p~n", [Name, ComponentState])
            end, ok, Components);
        _ ->
            io:format("  No components running~n")
    end,
    
    io:format("====================~n").

%% Print agent information
print_agent_info(Agent) ->
    io:format("~n=== AGENT INFORMATION ===~n"),
    io:format("ID: ~p~n", [Agent#agent.id]),
    io:format("Fitness: ~p~n", [Agent#agent.fitness]),
    io:format("Generation: ~p~n", [Agent#agent.generation]),
    io:format("Population ID: ~p~n", [Agent#agent.population_id]),
    io:format("Specie ID: ~p~n", [Agent#agent.specie_id]),
    io:format("Encoding Type: ~p~n", [Agent#agent.encoding_type]),
    io:format("=========================~n").

%% Print performance summary
print_performance_summary(Performance) ->
    io:format("~n=== PERFORMANCE SUMMARY ===~n"),
    %% This would print performance metrics
    %% Implementation depends on performance record structure
    io:format("Performance data: ~p~n", [Performance]),
    io:format("===========================~n").

%% Print detailed performance
print_detailed_performance(Report) ->
    io:format("~n=== DETAILED PERFORMANCE REPORT ===~n"),
    %% This would print detailed performance report
    %% Implementation depends on report structure
    io:format("Performance report: ~p~n", [Report]),
    io:format("===================================~n").

%% Print configuration
print_configuration(Config) ->
    io:format("~n=== LIVE TRADING CONFIGURATION ===~n"),
    lists:foreach(fun({Key, Value}) ->
        io:format("~p: ~p~n", [Key, Value])
    end, Config),
    io:format("==================================~n").

%% Print diagnostics summary
print_diagnostics_summary(Results) ->
    io:format("~n=== DIAGNOSTICS SUMMARY ===~n"),
    maps:fold(fun(Component, Result, _) ->
        Status = case Result of
            {ok, _} -> "OK";
            {error, _} -> "ERROR"
        end,
        io:format("~p: ~s~n", [Component, Status])
    end, ok, Results),
    io:format("============================~n").

%% ============================================================================
%% Quick Commands
%% ============================================================================

%% Quick start command
go() ->
    start().

%% Quick stop command
halt() ->
    stop().

%% Quick status command
st() ->
    status().

%% Quick performance command
perf() ->
    performance().

%% Help command
help() ->
    io:format("~n=== LIVE TRADING COMMANDS ===~n"),
    io:format("start() - Start live trading with best agent~n"),
    io:format("start_with_agent(AgentId) - Start with specific agent~n"),
    io:format("stop() - Stop live trading gracefully~n"),
    io:format("emergency_stop() - Emergency shutdown~n"),
    io:format("restart() - Restart system~n"),
    io:format("status() - Get system status~n"),
    io:format("performance() - Get performance summary~n"),
    io:format("performance_report() - Get detailed performance~n"),
    io:format("list_agents() - List available agents~n"),
    io:format("show_config() - Show configuration~n"),
    io:format("validate_config() - Validate configuration~n"),
    io:format("test() - Run quick tests~n"),
    io:format("test_full() - Run full test suite~n"),
    io:format("test_ib_connection() - Test IB connection specifically~n"),
    io:format("diagnostics() - Run system diagnostics~n"),
    io:format("help() - Show this help~n"),
    io:format("~nQuick commands:~n"),
    io:format("go() - Quick start~n"),
    io:format("halt() - Quick stop~n"),
    io:format("st() - Quick status~n"),
    io:format("perf() - Quick performance~n"),
    io:format("=============================~n").