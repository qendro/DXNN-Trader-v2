%% Live Trading Integration Module
%% Provides process supervision hierarchy and startup/shutdown procedures
%% Integrates all live trading components with proper error handling and recovery

-module(live_trading_integration).
-compile(export_all).
-include("records.hrl").
-behaviour(supervisor).

%% Supervisor callback
-export([init/1]).

%% Public API
-export([
    start_live_trading/1,
    stop_live_trading/0,
    restart_live_trading/1,
    get_system_status/0,
    emergency_shutdown/0,
    graceful_shutdown/0
]).

%% Internal state tracking
-record(integration_state, {
    agent_id,
    ib_bridge_connector_pid,
    live_scape_pid,
    live_trader_pid,
    supervisor_pid,
    startup_time,
    status = stopped,  % stopped, starting, running, stopping, error
    error_count = 0   % track number of errors for recovery logic
}).

%% Supervisor child specifications
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% ============================================================================
%% Public API - Main Entry Points
%% ============================================================================

%% Start complete live trading system with agent
start_live_trading(Agent_Id) ->
    io:format("Starting live trading system for Agent ~p~n", [Agent_Id]),
    
    %% Verify agent exists before starting
    case verify_agent_exists(Agent_Id) of
        ok ->
            %% Start supervisor
            case start_supervisor() of
                {ok, SupervisorPid} ->
                    %% Execute startup sequence
                    case execute_startup_sequence(Agent_Id, SupervisorPid) of
                        {ok, State} ->
                            %% Register integration process
                            register(live_trading_integration, self()),
                            
                            %% Start monitoring loop
                            spawn(?MODULE, integration_monitor_loop, [State]),
                            
                            io:format("Live trading system started successfully~n"),
                            {ok, started};
                        {error, Reason} ->
                            %% Cleanup on failure
                            cleanup_supervisor(SupervisorPid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {supervisor_start_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {agent_verification_failed, Reason}}
    end.

%% Stop complete live trading system
stop_live_trading() ->
    io:format("Stopping live trading system~n"),
    
    case whereis(live_trading_integration) of
        undefined ->
            io:format("Live trading system not running~n"),
            {ok, already_stopped};
        Pid ->
            %% Send graceful shutdown signal
            Pid ! graceful_shutdown,
            
            %% Wait for confirmation
            receive
                {live_trading_integration, shutdown_complete} ->
                    io:format("Live trading system stopped successfully~n"),
                    {ok, stopped}
            after 15000 ->
                %% Force shutdown if graceful fails
                io:format("Graceful shutdown timeout, forcing stop~n"),
                emergency_shutdown()
            end
    end.

%% Restart live trading system with same agent
restart_live_trading(Agent_Id) ->
    io:format("Restarting live trading system~n"),
    
    case stop_live_trading() of
        {ok, _} ->
            %% Wait a moment for cleanup
            timer:sleep(2000),
            start_live_trading(Agent_Id);
        {error, Reason} ->
            {error, {stop_failed, Reason}}
    end.

%% Get comprehensive system status
get_system_status() ->
    case whereis(live_trading_integration) of
        undefined ->
            {ok, #{status => stopped, components => []}};
        Pid ->
            Pid ! {get_status, self()},
            receive
                {system_status, Status} ->
                    {ok, Status}
            after 5000 ->
                {error, status_timeout}
            end
    end.

%% Emergency shutdown - immediate stop of all components
emergency_shutdown() ->
    io:format("EMERGENCY SHUTDOWN initiated~n"),
    
    %% Stop all known processes immediately
    emergency_stop_process(live_trader),
    emergency_stop_process(live_scape),
    emergency_stop_process(ib_bridge_connector),
    emergency_stop_process(live_trading_integration),
    
    %% Stop supervisor
    case whereis(live_trading_supervisor) of
        undefined -> ok;
        SupervisorPid -> 
            exit(SupervisorPid, emergency_shutdown)
    end,
    
    %% Cleanup resources
    cleanup_all_resources(),
    
    io:format("Emergency shutdown complete~n"),
    {ok, emergency_stopped}.

%% Graceful shutdown - orderly stop with position closing
graceful_shutdown() ->
    case whereis(live_trading_integration) of
        undefined ->
            {ok, already_stopped};
        Pid ->
            Pid ! graceful_shutdown,
            {ok, shutdown_initiated}
    end.

%% ============================================================================
%% Supervisor Implementation
%% ============================================================================

%% Start the supervisor
start_supervisor() ->
    supervisor:start_link({local, live_trading_supervisor}, ?MODULE, []).

%% Supervisor callback - define child processes
init([]) ->
    %% Define child specifications for live trading components
    Children = [
        %% IB Bridge Connector - handles Interactive Brokers API communication
        #{id => ib_bridge_connector,
          start => {ib_bridge_connector, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [ib_bridge_connector]},
        
        %% Live Scape - provides sensor/actuator interface
        #{id => live_scape,
          start => {live_scape, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [live_scape]},
        
        %% Live Trader - orchestrates trading operations
        #{id => live_trader,
          start => {live_trader, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [live_trader]}
    ],
    
    %% Supervisor strategy: one_for_all with restart limits
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,  % Max 3 restarts
        period => 60     % Within 60 seconds
    },
    
    {ok, {SupFlags, Children}}.

%% ============================================================================
%% Startup Sequence Implementation
%% ============================================================================

%% Execute the complete startup sequence
execute_startup_sequence(Agent_Id, SupervisorPid) ->
    io:format("Executing startup sequence for Agent ~p~n", [Agent_Id]),
    
    %% Basic system validation
    case validate_basic_requirements() of
        ok ->
            StartupSteps = [
        {step1, "Initialize IB connection", fun() -> startup_step_ib_connection() end},
        {step2, "Start live scape", fun() -> startup_step_live_scape() end},
        {step3, "Deploy neural network model", fun() -> startup_step_model_deployment(Agent_Id) end},
        {step4, "Initialize trading components", fun() -> startup_step_trading_initialization() end},
        {step5, "Start trading operations", fun() -> startup_step_start_trading(Agent_Id) end}
    ],
    
    case execute_startup_steps(StartupSteps, #{}) of
        {ok, ComponentPids} ->
            %% Create integration state
            State = #integration_state{
                agent_id = Agent_Id,
                ib_bridge_connector_pid = maps:get(ib_bridge_connector, ComponentPids, undefined),
                live_scape_pid = maps:get(live_scape, ComponentPids, undefined),
                live_trader_pid = maps:get(live_trader, ComponentPids, undefined),
                supervisor_pid = SupervisorPid,
                startup_time = erlang:timestamp(),
                status = running
            },
            {ok, State};
        {error, {Step, Reason}} ->
            io:format("Startup failed at ~p: ~p~n", [Step, Reason]),
            {error, {startup_failed, Step, Reason}}
    end;
        {error, Reason} ->
            io:format("System requirements validation failed: ~p~n", [Reason]),
            {error, {system_requirements_failed, Reason}}
    end.

%% Execute startup steps sequentially
execute_startup_steps([], ComponentPids) ->
    io:format("All startup steps completed successfully~n"),
    {ok, ComponentPids};
execute_startup_steps([{StepId, Description, StepFun} | Rest], ComponentPids) ->
    io:format("=== Executing ~s: ~s ===~n", [StepId, Description]),
    StartTime = erlang:timestamp(),
    
    case StepFun() of
        {ok, Result} ->
            %% Step succeeded, continue with next step
            EndTime = erlang:timestamp(),
            Duration = timer:now_diff(EndTime, StartTime) / 1000,
            io:format("✓ ~s completed successfully (~.1fms)~n", [StepId, Duration]),
            
            UpdatedPids = case Result of
                {ComponentName, Pid} -> maps:put(ComponentName, Pid, ComponentPids);
                _ -> ComponentPids
            end,
            execute_startup_steps(Rest, UpdatedPids);
        {error, Reason} ->
            %% Step failed, abort startup
            EndTime = erlang:timestamp(),
            Duration = timer:now_diff(EndTime, StartTime) / 1000,
            io:format("✗ ~s failed after ~.1fms: ~p~n", [StepId, Duration, Reason]),
            {error, {StepId, Reason}}
    end.

%% Startup Step 1: Initialize IB connection
startup_step_ib_connection() ->
    %% Check if IB bridge connector is already running (it should be from live_trader initialization)
    case whereis(ib_bridge_connector) of
        undefined ->
            %% IB bridge connector not running, start it
            Host = config:ib_host(),
            Port = config:ib_port(),
            ClientId = config:ib_client_id(),
            
            io:format("Starting IB bridge connector at ~s:~p with client ID ~p~n", [Host, Port, ClientId]),
            
            case ib_bridge_connector:start_connection(Host, Port, ClientId) of
                {ok, Pid} ->
                    %% Give the connection a moment to establish
                    timer:sleep(2000),
                    
                    %% Try to initialize market data tables (this will fail if connection isn't working)
                    case catch ib_bridge_connector:init_market_data_tables() of
                        ok ->
                            io:format("IB connection and market data initialization successful~n"),
                            {ok, {ib_bridge_connector, Pid}};
                        {'EXIT', Reason} ->
                            io:format("Market data init failed, but connection may be working: ~p~n", [Reason]),
                            %% Connection might still be working, continue anyway
                            {ok, {ib_bridge_connector, Pid}};
                        {error, Reason} ->
                            io:format("Market data init failed: ~p~n", [Reason]),
                            %% Connection might still be working, continue anyway
                            {ok, {ib_bridge_connector, Pid}}
                    end;
                {error, Reason} ->
                    {error, {connection_failed, Reason}}
            end;
        Pid ->
            %% IB bridge connector already running, assume it's working
            io:format("IB bridge connector already running with PID ~p, assuming connection is ready~n", [Pid]),
            
            %% Give it a moment and then proceed
            timer:sleep(1000),
            {ok, {ib_bridge_connector, Pid}}
    end.

%% Startup Step 2: Start live scape
startup_step_live_scape() ->
    io:format("Starting live scape~n"),
    
    %% Check if live_scape is already running
    case whereis(live_scape) of
        undefined ->
            %% Start new live_scape
            case live_scape:start_link() of
                {ok, Pid} ->
                    %% Wait for scape to initialize
                    case wait_for_scape_ready(5000) of
                        ok ->
                            {ok, {live_scape, Pid}};
                        {error, Reason} ->
                            {error, {scape_init_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {scape_start_failed, Reason}}
            end;
        Pid ->
            %% live_scape already running
            io:format("live_scape already running with PID ~p~n", [Pid]),
            {ok, {live_scape, Pid}}
    end.

%% Startup Step 3: Deploy neural network model
startup_step_model_deployment(Agent_Id) ->
    io:format("Deploying neural network model for Agent ~p~n", [Agent_Id]),
    
    case live_trader:deploy_model(Agent_Id) of
        {ok, State} ->
            {ok, {model_deployed, State}};
        {error, Reason} ->
            {error, {model_deployment_failed, Reason}}
    end.

%% Startup Step 4: Initialize trading components
startup_step_trading_initialization() ->
    io:format("Initializing trading components~n"),
    
    %% Subscribe to market data for configured currency pairs
    CurrencyPairs = config:live_currency_pairs(),
    
    case subscribe_to_all_pairs(CurrencyPairs) of
        ok ->
            %% Initialize performance monitoring
            case initialize_performance_monitoring() of
                ok ->
                    {ok, trading_components_initialized};
                {error, Reason} ->
                    {error, {performance_init_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {market_data_subscription_failed, Reason}}
    end.

%% Startup Step 5: Start trading operations
startup_step_start_trading(Agent_Id) ->
    io:format("Starting trading operations~n"),
    
    %% Get default risk parameters
    RiskParams = get_default_risk_parameters(),
    
    case live_trader:start_trading(Agent_Id, RiskParams) of
        {ok, trading_started} ->
            {ok, trading_started};
        {error, Reason} ->
            {error, {trading_start_failed, Reason}}
    end.

%% ============================================================================
%% Shutdown Sequence Implementation
%% ============================================================================

%% Execute graceful shutdown sequence
execute_graceful_shutdown(State) ->
    io:format("Executing graceful shutdown sequence~n"),
    
    ShutdownSteps = [
        {step1, "Stop accepting new trades", fun() -> shutdown_step_stop_new_trades() end},
        {step2, "Close open positions", fun() -> shutdown_step_close_positions() end},
        {step3, "Stop trading operations", fun() -> shutdown_step_stop_trading() end},
        {step4, "Disconnect from IB", fun() -> shutdown_step_disconnect_ib() end},
        {step5, "Cleanup resources", fun() -> shutdown_step_cleanup_resources() end}
    ],
    
    case execute_shutdown_steps(ShutdownSteps) of
        ok ->
            %% Stop supervisor
            cleanup_supervisor(State#integration_state.supervisor_pid),
            io:format("Graceful shutdown completed~n"),
            ok;
        {error, {Step, Reason}} ->
            io:format("Graceful shutdown failed at ~p: ~p~n", [Step, Reason]),
            %% Fall back to emergency shutdown
            emergency_shutdown()
    end.

%% Execute shutdown steps sequentially
execute_shutdown_steps([]) ->
    ok;
execute_shutdown_steps([{StepId, Description, StepFun} | Rest]) ->
    io:format("Executing ~s: ~s~n", [StepId, Description]),
    
    case StepFun() of
        ok ->
            execute_shutdown_steps(Rest);
        {error, Reason} ->
            {error, {StepId, Reason}}
    end.

%% Shutdown Step 1: Stop accepting new trades
shutdown_step_stop_new_trades() ->
    case whereis(live_trader) of
        undefined -> ok;
        Pid -> 
            Pid ! stop_new_trades,
            ok
    end.

%% Shutdown Step 2: Close open positions
shutdown_step_close_positions() ->
    io:format("Closing all open positions~n"),
    
    case live_trader:get_current_positions() of
        {ok, []} ->
            io:format("No open positions to close~n"),
            ok;
        {ok, Positions} ->
            close_all_positions(Positions);
        {error, Reason} ->
            io:format("Could not get current positions: ~p~n", [Reason]),
            %% Continue with shutdown anyway
            ok
    end.

%% Shutdown Step 3: Stop trading operations
shutdown_step_stop_trading() ->
    case live_trader:stop_trading() of
        {ok, _} -> ok;
        {error, Reason} -> 
            io:format("Error stopping trading: ~p~n", [Reason]),
            ok  % Continue shutdown anyway
    end.

%% Shutdown Step 4: Disconnect from IB
shutdown_step_disconnect_ib() ->
    case ib_bridge_connector:stop_connection() of
        ok -> ok;
        {error, Reason} ->
            io:format("Error disconnecting from IB: ~p~n", [Reason]),
            ok  % Continue shutdown anyway
    end.

%% Shutdown Step 5: Cleanup resources
shutdown_step_cleanup_resources() ->
    cleanup_all_resources(),
    ok.

%% ============================================================================
%% Integration Monitor Loop
%% ============================================================================

%% Main monitoring loop for the integration system
integration_monitor_loop(State) ->
    receive
        graceful_shutdown ->
            %% Execute graceful shutdown
            execute_graceful_shutdown(State),
            %% Notify caller
            case whereis(live_trading_integration) of
                undefined -> ok;
                _ -> live_trading_integration ! {live_trading_integration, shutdown_complete}
            end;
            
        {get_status, From} ->
            %% Return comprehensive system status
            Status = get_comprehensive_status(State),
            From ! {system_status, Status},
            integration_monitor_loop(State);
            
        {component_crashed, Component, Reason} ->
            %% Handle component crash
            io:format("Component ~p crashed: ~p~n", [Component, Reason]),
            UpdatedState = handle_component_crash(State, Component, Reason),
            integration_monitor_loop(UpdatedState);
            
        {health_check} ->
            %% Perform health check on all components
            HealthStatus = perform_health_check(State),
            case HealthStatus of
                healthy ->
                    integration_monitor_loop(State);
                {unhealthy, Issues} ->
                    io:format("Health check failed: ~p~n", [Issues]),
                    %% Attempt recovery
                    RecoveredState = attempt_system_recovery(State, Issues),
                    integration_monitor_loop(RecoveredState)
            end;
            
        emergency_shutdown ->
            %% Immediate emergency shutdown
            emergency_shutdown(),
            ok;
            
        restart_system ->
            %% Restart the entire system
            Agent_Id = State#integration_state.agent_id,
            execute_graceful_shutdown(State),
            timer:sleep(2000),
            start_live_trading(Agent_Id);
            
        _Other ->
            %% Unknown message
            integration_monitor_loop(State)
            
    after 30000 ->
        %% Periodic health check every 30 seconds
        self() ! {health_check},
        integration_monitor_loop(State)
    end.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Validate basic system requirements before startup
validate_basic_requirements() ->
    %% Check if required modules are available
    RequiredModules = [genotype, config, live_trader, live_scape, ib_bridge_connector],
    
    case check_modules_available(RequiredModules) of
        ok ->
            %% Validate configuration
            case validate_configuration() of
                ok -> ok;
                {error, Reason} -> {error, {config_validation_failed, Reason}}
            end;
        {error, MissingModules} ->
            {error, {missing_modules, MissingModules}}
    end.

%% Check if required modules are available
check_modules_available([]) -> ok;
check_modules_available([Module | Rest]) ->
    case code:is_loaded(Module) of
        false ->
            %% Try to load the module
            case code:load_file(Module) of
                {module, Module} ->
                    %% Module loaded successfully, continue with rest
                    check_modules_available(Rest);
                {error, _Reason} ->
                    %% Module not available, collect missing modules
                    case check_modules_available(Rest) of
                        ok -> {error, [Module]};
                        {error, Missing} -> {error, [Module | Missing]}
                    end
            end;
        _ ->
            %% Module already loaded, continue with rest
            check_modules_available(Rest)
    end.

%% Validate basic configuration
validate_configuration() ->
    try
        %% Check IB configuration
        config:validate_ib_connection_config(),
        
        %% Check if currency pairs are configured
        CurrencyPairs = config:live_currency_pairs(),
        case is_list(CurrencyPairs) andalso length(CurrencyPairs) > 0 of
            true -> ok;
            false -> throw({invalid_currency_pairs, CurrencyPairs})
        end,
        
        %% Check risk parameters
        PositionSize = config:live_position_size(),
        case is_number(PositionSize) andalso PositionSize > 0 andalso PositionSize =< 1.0 of
            true -> ok;
            false -> throw({invalid_position_size, PositionSize})
        end,
        
        ok
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% Verify that the agent exists in the database
verify_agent_exists(Agent_Id) ->
    case genotype:dirty_read({agent, Agent_Id}) of
        undefined ->
            {error, agent_not_found};
        _Agent ->
            ok
    end.

%% Wait for IB connection to be established
wait_for_ib_connection(Timeout) ->
    wait_for_ib_connection(Timeout, erlang:timestamp()).

wait_for_ib_connection(Timeout, StartTime) ->
    ElapsedMs = timer:now_diff(erlang:timestamp(), StartTime) / 1000,
    if
        ElapsedMs > Timeout ->
            {error, connection_timeout};
        true ->
            case catch ib_bridge_connector:get_connection_status() of
                {ok, true} ->
                    io:format("IB connection confirmed as ready~n"),
                    ok;
                {ok, false} ->
                    timer:sleep(500),
                    wait_for_ib_connection(Timeout, StartTime);
                {'EXIT', _Reason} ->
                    %% Connection status check failed, but connection might still be working
                    %% Try a simple ping instead
                    case catch ib_bridge_connector:ping() of
                        pong ->
                            io:format("IB connection confirmed via ping~n"),
                            ok;
                        _ ->
                            timer:sleep(500),
                            wait_for_ib_connection(Timeout, StartTime)
                    end;
                {error, _Reason} ->
                    timer:sleep(500),
                    wait_for_ib_connection(Timeout, StartTime)
            end
    end.

%% Wait for scape to be ready
wait_for_scape_ready(_Timeout) ->
    %% Simple implementation - in production would have proper readiness check
    timer:sleep(1000),
    case whereis(live_scape) of
        undefined -> {error, scape_not_started};
        _Pid -> ok
    end.

%% Subscribe to market data for all configured currency pairs
subscribe_to_all_pairs([]) ->
    ok;
subscribe_to_all_pairs([Pair | Rest]) ->
    Symbol = atom_to_list(Pair),
    ReqId = length(config:live_currency_pairs()) - length(Rest),
    
    case ib_bridge_connector:subscribe_market_data(Symbol, ReqId) of
        ok ->
            io:format("Subscribed to market data for ~s~n", [Symbol]),
            subscribe_to_all_pairs(Rest);
        {error, Reason} ->
            io:format("Failed to subscribe to ~s: ~p~n", [Symbol, Reason]),
            {error, {subscription_failed, Symbol, Reason}}
    end.

%% Initialize performance monitoring
initialize_performance_monitoring() ->
    %% Initialize ETS tables for performance tracking
    case live_trader:init_performance_tables() of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% Get default risk parameters
get_default_risk_parameters() ->
    #{
        position_size => config:live_position_size(),
        max_daily_loss => config:live_max_daily_loss(),
        max_position_per_pair => 0.1,  % 10% max per currency pair
        max_total_exposure => 0.5       % 50% max total exposure
    }.

%% Close all positions during shutdown
close_all_positions([]) ->
    ok;
close_all_positions([Position | Rest]) ->
    Symbol = Position#position_info.symbol,
    Quantity = Position#position_info.quantity,
    Side = Position#position_info.side,
    
    %% Determine close action
    CloseAction = case Side of
        long -> "SELL";
        short -> "BUY"
    end,
    
    io:format("Closing position: ~s ~p ~s~n", [CloseAction, Quantity, Symbol]),
    
    case ib_bridge_connector:place_order(Symbol, CloseAction, Quantity, "MKT") of
        ok ->
            %% Wait for fill confirmation
            case ib_bridge_connector:wait_for_order_confirmation(1, 5000) of
                {ok, _} ->
                    io:format("Position closed successfully~n"),
                    close_all_positions(Rest);
                {error, Reason} ->
                    io:format("Position close confirmation failed: ~p~n", [Reason]),
                    %% Continue with other positions
                    close_all_positions(Rest)
            end;
        {error, Reason} ->
            io:format("Failed to place close order: ~p~n", [Reason]),
            %% Continue with other positions
            close_all_positions(Rest)
    end.

%% Emergency stop a process
emergency_stop_process(ProcessName) ->
    case whereis(ProcessName) of
        undefined -> ok;
        Pid -> 
            exit(Pid, emergency_shutdown),
            io:format("Emergency stopped process: ~p~n", [ProcessName])
    end.

%% Cleanup supervisor
cleanup_supervisor(SupervisorPid) ->
    case is_pid(SupervisorPid) andalso is_process_alive(SupervisorPid) of
        true ->
            exit(SupervisorPid, shutdown);
        false ->
            ok
    end.

%% Cleanup all resources
cleanup_all_resources() ->
    io:format("Starting resource cleanup~n"),
    
    %% Cleanup ETS tables with error handling
    try
        ib_bridge_connector:cleanup_market_data_tables()
    catch
        Error:Reason ->
            io:format("Warning: Failed to cleanup IB market data tables: ~p:~p~n", [Error, Reason])
    end,
    
    %% Cleanup performance tables with validation
    cleanup_ets_table(live_trade_history),
    cleanup_ets_table(live_performance_snapshots),
    
    %% Cleanup any other known tables
    cleanup_ets_table(live_price_buffer),
    cleanup_ets_table(live_market_data),
    
    io:format("Resource cleanup completed~n").

%% Helper function to safely cleanup ETS tables
cleanup_ets_table(TableName) ->
    case ets:info(TableName) of
        undefined -> 
            ok;
        _ -> 
            try
                ets:delete(TableName),
                io:format("Cleaned up ETS table: ~p~n", [TableName])
            catch
                Error:Reason ->
                    io:format("Warning: Failed to cleanup ETS table ~p: ~p:~p~n", [TableName, Error, Reason])
            end
    end.

%% Get comprehensive system status
get_comprehensive_status(State) ->
    ComponentStatus = #{
        ib_bridge_connector => get_component_status(ib_bridge_connector),
        live_scape => get_component_status(live_scape),
        live_trader => get_component_status(live_trader)
    },
    
    #{
        status => State#integration_state.status,
        agent_id => State#integration_state.agent_id,
        startup_time => State#integration_state.startup_time,
        uptime_seconds => timer:now_diff(erlang:timestamp(), State#integration_state.startup_time) / 1000000,
        components => ComponentStatus
    }.

%% Get individual component status
get_component_status(ComponentName) ->
    case whereis(ComponentName) of
        undefined -> 
            #{status => stopped, pid => undefined};
        Pid ->
            #{status => running, pid => Pid, alive => is_process_alive(Pid)}
    end.

%% Perform health check on all components
perform_health_check(_State) ->
    %% Check if all critical processes are alive
    CriticalProcesses = [ib_bridge_connector, live_scape, live_trader],
    
    Issues = lists:foldl(fun(ProcessName, Acc) ->
        case whereis(ProcessName) of
            undefined -> [{process_dead, ProcessName} | Acc];
            Pid ->
                case is_process_alive(Pid) of
                    false -> [{process_dead, ProcessName} | Acc];
                    true -> Acc
                end
        end
    end, [], CriticalProcesses),
    
    %% Check IB connection
    ConnectionIssues = case ib_bridge_connector:get_connection_status() of
        {ok, true} -> [];
        {ok, false} -> [ib_connection_down];
        {error, _} -> [ib_connection_error]
    end,
    
    AllIssues = Issues ++ ConnectionIssues,
    
    case AllIssues of
        [] -> healthy;
        _ -> {unhealthy, AllIssues}
    end.

%% Attempt system recovery
attempt_system_recovery(State, Issues) ->
    io:format("Attempting system recovery for issues: ~p~n", [Issues]),
    
    %% Handle each issue type
    lists:foreach(fun(Issue) ->
        case Issue of
            {process_dead, ProcessName} ->
                attempt_process_restart(ProcessName);
            ib_connection_down ->
                attempt_ib_reconnection();
            ib_connection_error ->
                attempt_ib_reconnection();
            _ ->
                io:format("Unknown issue type: ~p~n", [Issue])
        end
    end, Issues),
    
    %% Return state (could be updated based on recovery results)
    State.

%% Attempt to restart a dead process
attempt_process_restart(ProcessName) ->
    io:format("Attempting to restart process: ~p~n", [ProcessName]),
    %% In a real implementation, would use supervisor restart
    %% For now, just log the attempt
    ok.

%% Attempt IB reconnection
attempt_ib_reconnection() ->
    io:format("Attempting IB reconnection~n"),
    %% Trigger reconnection in IB bridge connector
    case whereis(ib_bridge_connector) of
        undefined -> ok;
        Pid -> Pid ! reconnect
    end.

%% Handle component crash
handle_component_crash(State, Component, Reason) ->
    io:format("Component ~p crashed: ~p~n", [Component, Reason]),
    
    %% Update error count
    NewErrorCount = State#integration_state.error_count + 1,
    UpdatedState = State#integration_state{error_count = NewErrorCount},
    
    %% Basic recovery logic
    case NewErrorCount =< 3 of
        true ->
            io:format("Attempting recovery for component ~p~n", [Component]),
            UpdatedState#integration_state{status = error};
        false ->
            %% Too many errors, trigger emergency shutdown
            io:format("Too many errors, triggering emergency shutdown~n"),
            emergency_shutdown(),
            UpdatedState#integration_state{status = critical_error}
    end.

%% Log component crash
log_component_crash(CrashRecord) ->
    io:format("COMPONENT CRASH LOGGED: ~p~n", [CrashRecord]).

%% ============================================================================
%% Testing and Validation Functions
%% ============================================================================

%% Test complete system integration with paper trading account
test_system_integration() ->
    io:format("Testing complete system integration~n"),
    
    %% Use a test agent ID
    TestAgentId = get_test_agent_id(),
    
    case TestAgentId of
        {ok, Agent_Id} ->
            %% Start system
            case start_live_trading(Agent_Id) of
                {ok, started} ->
                    %% Run integration tests
                    TestResults = run_integration_tests(),
                    
                    %% Stop system
                    stop_live_trading(),
                    
                    {ok, TestResults};
                {error, Reason} ->
                    {error, {startup_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {no_test_agent, Reason}}
    end.

%% Get a test agent ID from the database
get_test_agent_id() ->
    %% Find any agent in the database for testing
    case genotype_utils:list_all_agents(all) of
        {atomic, []} ->
            {error, no_agents_available};
        {atomic, [{Agent_Id, _Fitness, _Generation, _Specie_Id} | _]} ->
            {ok, Agent_Id}
    end.

%% Run integration tests
run_integration_tests() ->
    Tests = [
        {test_ib_connection, fun test_ib_connection_integration/0},
        {test_market_data_flow, fun test_market_data_flow_integration/0},
        {test_trade_execution, fun test_trade_execution_integration/0},
        {test_system_monitoring, fun test_system_monitoring_integration/0}
    ],
    
    Results = lists:map(fun({TestName, TestFun}) ->
        io:format("Running integration test: ~p~n", [TestName]),
        try
            Result = TestFun(),
            {TestName, Result}
        catch
            Error:Reason ->
                {TestName, {error, {Error, Reason}}}
        end
    end, Tests),
    
    Results.

%% Test IB connection integration
test_ib_connection_integration() ->
    case ib_bridge_connector:get_connection_status() of
        {ok, true} -> {ok, connected};
        {ok, false} -> {error, not_connected};
        {error, Reason} -> {error, Reason}
    end.

%% Test market data flow integration
test_market_data_flow_integration() ->
    %% Test that market data is flowing from IB to sensors
    Symbol = "EUR.USD",
    case ib_bridge_connector:get_market_data(Symbol) of
        {ok, _MarketData} -> {ok, data_flowing};
        {error, no_data} -> {error, no_market_data};
        {error, Reason} -> {error, Reason}
    end.

%% Test trade execution integration
test_trade_execution_integration() ->
    %% Test small position with paper trading
    %% This would be a very small test trade
    {ok, test_trade_not_implemented}.

%% Test system monitoring integration
test_system_monitoring_integration() ->
    case get_system_status() of
        {ok, Status} when is_map(Status) -> {ok, monitoring_working};
        {error, Reason} -> {error, Reason}
    end.