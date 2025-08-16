%% Live Trader Orchestration Module
%% Manages model deployment and trading coordination for live paper trading
%% Integrates with existing exoself pattern and Mnesia database

-module(live_trader).
-compile(export_all).
-include("records.hrl").

%% API for supervisor integration
-export([start_link/0]).

%% State record for live trader
-record(live_trader_state, {
    agent_id,
    exoself_pid,
    ib_connector_pid,
    live_scape_pid,
    trading_active = false,
    start_time,
    performance_data = [],
    risk_parameters,
    risk_state = #risk_state{},
    current_positions = []  % [#position_info{}]
}).



%% ============================================================================
%% Public API
%% ============================================================================

%% Start link function for supervisor integration
start_link() ->
    Pid = spawn_link(?MODULE, init_trader, []),
    register(live_trader, Pid),
    {ok, Pid}.

%% Initialize trader process
init_trader() ->
    %% Initialize performance tracking tables
    init_performance_tables(),
    
    %% Wait for deployment commands
    trader_idle_loop().

%% Idle loop waiting for deployment
trader_idle_loop() ->
    receive
        {deploy_model, AgentId, From} ->
            Result = deploy_model_internal(AgentId),
            From ! {deploy_result, Result},
            case Result of
                {ok, State} ->
                    trading_loop(State);
                {error, _} ->
                    trader_idle_loop()
            end;
        stop ->
            ok;
        _ ->
            trader_idle_loop()
    end.

%% Deploy a model from Mnesia database and initialize neural network
deploy_model(Agent_Id) ->
    case whereis(live_trader) of
        undefined ->
            {error, trader_not_started};
        Pid ->
            Pid ! {deploy_model, Agent_Id, self()},
            receive
                {deploy_result, Result} ->
                    Result
            after 10000 ->
                {error, deployment_timeout}
            end
    end.

%% Internal deployment function
deploy_model_internal(Agent_Id) ->
    io:format("Deploying model with Agent_Id: ~p~n", [Agent_Id]),
    
    %% Verify agent exists in Mnesia
    case genotype:dirty_read({agent, Agent_Id}) of
        undefined ->
            io:format("Error: Agent ~p not found in database~n", [Agent_Id]),
            {error, agent_not_found};
        Agent ->
            io:format("Found agent: ~p~n", [Agent#agent.id]),
            
            %% Initialize live trading components
            case initialize_live_components() of
                {ok, IBPid, ScapePid} ->
                    %% Initialize performance tracking tables
                    init_performance_tables(),
                    
                    %% Deploy neural network using exoself pattern
                    case deploy_neural_network(Agent_Id, ScapePid) of
                        {ok, ExoselfPid} ->
                            %% Initialize performance tracking
                            Performance = #performance_metrics{
                                start_time = erlang:timestamp(),
                                last_update = erlang:timestamp()
                            },
                            
                            %% Initialize risk state
                            InitialRiskState = #risk_state{
                                daily_start_balance = config:account_initial_balance(),
                                daily_pnl = 0.0,
                                daily_trades = 0,
                                max_drawdown = 0.0,
                                position_exposures = [],
                                total_exposure = 0.0,
                                last_reset_date = date(),
                                risk_violations = []
                            },
                            
                            %% Create live trader state
                            State = #live_trader_state{
                                agent_id = Agent_Id,
                                exoself_pid = ExoselfPid,
                                ib_connector_pid = IBPid,
                                live_scape_pid = ScapePid,
                                start_time = erlang:timestamp(),
                                performance_data = Performance,
                                risk_parameters = get_risk_parameters(),
                                risk_state = InitialRiskState,
                                current_positions = []
                            },
                            
                            %% Register the live trader process
                            register(live_trader, self()),
                            
                            io:format("Model deployed successfully~n"),
                            {ok, State};
                        {error, Reason} ->
                            io:format("Failed to deploy neural network: ~p~n", [Reason]),
                            cleanup_components(IBPid, ScapePid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    io:format("Failed to initialize live components: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.

%% Start live trading with specified model and risk parameters
start_trading(Agent_Id, RiskParams) ->
    io:format("Starting live trading for Agent_Id: ~p~n", [Agent_Id]),
    
    case deploy_model(Agent_Id) of
        {ok, State} ->
            %% Update risk parameters
            UpdatedState = State#live_trader_state{
                risk_parameters = merge_risk_parameters(State#live_trader_state.risk_parameters, RiskParams),
                trading_active = true
            },
            
            %% Subscribe to market data for configured currency pairs
            case subscribe_to_market_data(UpdatedState) of
                ok ->
                    %% Start trading loop
                    spawn(?MODULE, trading_loop, [UpdatedState]),
                    io:format("Live trading started successfully~n"),
                    {ok, trading_started};
                {error, Reason} ->
                    io:format("Failed to subscribe to market data: ~p~n", [Reason]),
                    stop_trading(),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Stop live trading and clean up resources
stop_trading() ->
    io:format("Stopping live trading~n"),
    
    case whereis(live_trader) of
        undefined ->
            io:format("Live trader not running~n"),
            {ok, already_stopped};
        Pid ->
            %% Send stop signal to trading loop
            Pid ! stop_trading,
            
            %% Wait for graceful shutdown
            receive
                {live_trader, stopped} ->
                    io:format("Live trading stopped successfully~n"),
                    {ok, stopped}
            after 5000 ->
                %% Force stop if graceful shutdown fails
                exit(Pid, kill),
                io:format("Live trading force stopped~n"),
                {ok, force_stopped}
            end
    end.

%% Get current performance metrics (legacy function for compatibility)
get_performance_basic() ->
    case whereis(live_trader) of
        undefined ->
            {error, not_running};
        Pid ->
            Pid ! {get_performance, self()},
            receive
                {performance_data, Performance} ->
                    {ok, Performance}
            after 1000 ->
                {error, timeout}
            end
    end.

%% Get current positions for shutdown
get_current_positions() ->
    case whereis(live_trader) of
        undefined ->
            {error, trader_not_running};
        Pid ->
            Pid ! {get_current_positions, self()},
            receive
                {current_positions, Positions} ->
                    {ok, Positions}
            after 1000 ->
                {error, timeout}
            end
    end.

%% Initialize performance tables
init_performance_tables() ->
    %% Create ETS tables for performance tracking
    case ets:info(live_trade_history) of
        undefined ->
            ets:new(live_trade_history, [named_table, public, ordered_set]);
        _ ->
            ok
    end,
    
    case ets:info(live_performance_snapshots) of
        undefined ->
            ets:new(live_performance_snapshots, [named_table, public, ordered_set]);
        _ ->
            ok
    end,
    
    ok.

%% Get comprehensive performance report
get_performance_report() ->
    case get_performance() of
        {ok, Performance} ->
            %% Get trade history
            TradeHistory = case ets:info(live_trade_history) of
                undefined -> [];
                _ -> ets:tab2list(live_trade_history)
            end,
            
            %% Get performance snapshots
            Snapshots = case ets:info(live_performance_snapshots) of
                undefined -> [];
                _ -> ets:tab2list(live_performance_snapshots)
            end,
            
            %% Create comprehensive report
            Report = #{
                performance_metrics => Performance,
                trade_history => TradeHistory,
                performance_snapshots => Snapshots,
                report_timestamp => erlang:timestamp()
            },
            
            {ok, Report};
        Error ->
            Error
    end.

%% Get performance comparison with backtesting
get_performance_comparison(AgentId) ->
    case ets:info(backtesting_comparison) of
        undefined ->
            %% Generate new comparison
            compare_with_backtesting(AgentId);
        _ ->
            %% Try to get existing comparison
            case ets:lookup(backtesting_comparison, AgentId) of
                [{AgentId, Comparison}] ->
                    {ok, Comparison};
                [] ->
                    %% Generate new comparison
                    compare_with_backtesting(AgentId)
            end
    end.

%% ============================================================================
%% Neural Network Deployment
%% ============================================================================

%% Deploy neural network using existing exoself pattern
deploy_neural_network(Agent_Id, _ScapePid) ->
    io:format("Deploying neural network for Agent_Id: ~p~n", [Agent_Id]),
    
    try
        %% Start exoself process with live trading mode
        ExoselfPid = exoself:start(Agent_Id, self(), live_trading),
        
        %% Wait for exoself to initialize
        receive
            {ExoselfPid, initialized} ->
                io:format("Neural network initialized successfully~n"),
                {ok, ExoselfPid};
            {ExoselfPid, error, InitReason} ->
                io:format("Neural network initialization failed: ~p~n", [InitReason]),
                {error, InitReason}
        after 10000 ->
            io:format("Neural network initialization timeout~n"),
            {error, initialization_timeout}
        end
    catch
        Error:CatchReason ->
            io:format("Error deploying neural network: ~p:~p~n", [Error, CatchReason]),
            {error, {Error, CatchReason}}
    end.

%% ============================================================================
%% Live Trading Components Initialization
%% ============================================================================

%% Initialize IB connector and live scape
initialize_live_components() ->
    io:format("Initializing live trading components~n"),
    
    %% Check if IB connector is already running
    case whereis(ib_bridge_connector) of
        undefined ->
            %% Start new IB connector
            Host = config:ib_host(),
            Port = config:ib_port(),
            ClientId = config:ib_client_id(),
            
            case ib_bridge_connector:start_connection(Host, Port, ClientId) of
                {ok, IBPid} ->
                    io:format("IB connector started~n"),
                    initialize_remaining_components(IBPid);
                {error, Reason} ->
                    io:format("Failed to start IB connector: ~p~n", [Reason]),
                    {error, Reason}
            end;
        IBPid ->
            %% IB connector already running
            io:format("IB connector already running with PID ~p~n", [IBPid]),
            initialize_remaining_components(IBPid)
    end.

%% Initialize remaining components after IB connector is ready
initialize_remaining_components(IBPid) ->
    %% Initialize market data tables (safe to call multiple times)
    case catch ib_bridge_connector:init_market_data_tables() of
        ok ->
            io:format("Market data tables initialized~n");
        {'EXIT', _Reason} ->
            io:format("Market data tables already initialized~n");
        {error, _Reason} ->
            io:format("Market data tables already initialized~n")
    end,
    
    %% Check if live scape is already running
    case whereis(live_scape) of
        undefined ->
            %% Start new live scape
            ScapePid = spawn(?MODULE, start_live_scape, []),
            
            %% Wait for scape to initialize
            receive
                {ScapePid, scape_ready} ->
                    io:format("Live scape initialized~n"),
                    {ok, IBPid, ScapePid}
            after 5000 ->
                io:format("Live scape initialization timeout~n"),
                {error, scape_timeout}
            end;
        ScapePid ->
            %% Live scape already running
            io:format("Live scape already running with PID ~p~n", [ScapePid]),
            {ok, IBPid, ScapePid}
    end.

%% Start live scape process
start_live_scape() ->
    %% Initialize live scape
    live_scape:prep(self()),
    
    %% Signal that scape is ready
    receive
        {ParentPid, live_sim} ->
            ParentPid ! {self(), scape_ready},
            live_scape:live_sim(ParentPid)
    end.

%% Subscribe to market data for configured currency pairs
subscribe_to_market_data(State) ->
    CurrencyPairs = config:live_currency_pairs(),
    subscribe_to_pairs(CurrencyPairs, 1, State).

subscribe_to_pairs([], _ReqId, _State) ->
    ok;
subscribe_to_pairs([Symbol | Rest], ReqId, State) ->
    SymbolStr = atom_to_list(Symbol),
    case ib_bridge_connector:subscribe_market_data(SymbolStr, ReqId) of
        ok ->
            io:format("Subscribed to market data for ~s~n", [SymbolStr]),
            subscribe_to_pairs(Rest, ReqId + 1, State);
        {error, Reason} ->
            io:format("Failed to subscribe to ~s: ~p~n", [SymbolStr, Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Trading Loop
%% ============================================================================

%% Main trading loop - coordinates neural network and market interaction
trading_loop(State) ->
    case State#live_trader_state.trading_active of
        true ->
            %% Check risk limits before continuing
            case check_risk_limits(State) of
                {ok, UpdatedState} ->
                    %% Continue trading with updated risk state
                    receive
                        stop_trading ->
                            %% Graceful shutdown
                            cleanup_and_stop(State);
                        {get_performance, From} ->
                            EnhancedPerformance = calculate_enhanced_metrics(State#live_trader_state.performance_data),
                            From ! {performance_data, EnhancedPerformance},
                            trading_loop(State);
                        {exoself, evaluation_completed, Fitness, Cycles, Time, GoalReached} ->
                            %% Handle neural network evaluation completion
                            UpdatedState = update_performance_metrics(State, Fitness, Cycles, Time),
                            
                            case GoalReached of
                                true ->
                                    io:format("Trading goal reached, stopping~n"),
                                    cleanup_and_stop(UpdatedState);
                                false ->
                                    %% Continue trading
                                    trading_loop(UpdatedState)
                            end;
                        {market_data_update, _Symbol, _Tick} ->
                            %% Handle market data updates (for monitoring)
                            trading_loop(State);
                        {trade_executed, Timestamp, Symbol, Action, Quantity, Price} ->
                            %% Handle trade execution confirmation with risk tracking
                            UpdatedState = record_trade_execution_with_risk(State, Timestamp, Symbol, Action, Quantity, Price),
                            trading_loop(UpdatedState);
                        {emergency_stop, ErrorCode, ErrorMsg, Timestamp} ->
                            %% Handle emergency stop from IB connector
                            io:format("EMERGENCY STOP received: ~p - ~s~n", [ErrorCode, ErrorMsg]),
                            EmergencyState = handle_emergency_stop(State, ErrorCode, ErrorMsg, Timestamp),
                            cleanup_and_stop(EmergencyState);
                        {ib_connection_recovered, Timestamp} ->
                            %% Handle connection recovery notification
                            io:format("IB connection recovered at ~p~n", [Timestamp]),
                            RecoveredState = handle_connection_recovery(State, Timestamp),
                            trading_loop(RecoveredState);
                        {system_error, ErrorType, ErrorDetails} ->
                            %% Handle system-level errors
                            io:format("System error: ~p - ~p~n", [ErrorType, ErrorDetails]),
                            ErrorState = handle_system_error(State, ErrorType, ErrorDetails),
                            case should_continue_after_error(ErrorType) of
                                true -> trading_loop(ErrorState);
                                false -> cleanup_and_stop(ErrorState)
                            end
                    after 1000 ->
                        %% Periodic check - continue loop with updated state
                        trading_loop(UpdatedState)
                    end;
                {halt, Reason, HaltState} ->
                    io:format("Risk limit exceeded: ~p, halting trading~n", [Reason]),
                    cleanup_and_stop(HaltState)
            end;
        false ->
            %% Trading not active
            receive
                stop_trading ->
                    cleanup_and_stop(State)
            after 1000 ->
                trading_loop(State)
            end
    end.

%% ============================================================================
%% Emergency Stop and Critical Error Handling
%% ============================================================================

%% Handle emergency stop triggered by critical errors
handle_emergency_stop(State, ErrorCode, ErrorMsg, Timestamp) ->
    io:format("HANDLING EMERGENCY STOP: ~p - ~s at ~p~n", [ErrorCode, ErrorMsg, Timestamp]),
    
    %% Log emergency stop
    EmergencyRecord = {emergency_stop, ErrorCode, ErrorMsg, Timestamp, State#live_trader_state.agent_id},
    log_emergency_event(EmergencyRecord),
    
    %% Immediately halt trading
    UpdatedState = State#live_trader_state{trading_active = false},
    
    %% Attempt to close any open positions
    ClosedState = emergency_close_positions(UpdatedState),
    
    %% Update performance metrics with emergency stop
    Performance = ClosedState#live_trader_state.performance_data,
    UpdatedPerformance = Performance#performance_metrics{
        last_update = Timestamp
    },
    
    %% Record emergency stop in risk state
    RiskState = ClosedState#live_trader_state.risk_state,
    EmergencyViolation = {emergency_stop, ErrorCode, Timestamp, ErrorMsg},
    UpdatedRiskState = RiskState#risk_state{
        risk_violations = [EmergencyViolation | RiskState#risk_state.risk_violations]
    },
    
    FinalState = ClosedState#live_trader_state{
        performance_data = UpdatedPerformance,
        risk_state = UpdatedRiskState
    },
    
    %% Notify external systems of emergency stop
    notify_emergency_stop(ErrorCode, ErrorMsg, Timestamp),
    
    FinalState.

%% Handle connection recovery
handle_connection_recovery(State, Timestamp) ->
    io:format("Handling connection recovery at ~p~n", [Timestamp]),
    
    %% Log recovery event
    RecoveryRecord = {connection_recovery, Timestamp, State#live_trader_state.agent_id},
    log_recovery_event(RecoveryRecord),
    
    %% Check if we should resume trading
    case should_resume_trading_after_recovery(State) of
        true ->
            io:format("Resuming trading after connection recovery~n"),
            %% Resubscribe to market data
            case resubscribe_after_recovery(State) of
                ok ->
                    State#live_trader_state{trading_active = true};
                {error, Reason} ->
                    io:format("Failed to resubscribe after recovery: ~p~n", [Reason]),
                    State
            end;
        false ->
            io:format("Not resuming trading after recovery due to risk constraints~n"),
            State
    end.

%% Handle system-level errors
handle_system_error(State, ErrorType, ErrorDetails) ->
    io:format("Handling system error: ~p - ~p~n", [ErrorType, ErrorDetails]),
    
    %% Log system error
    SystemErrorRecord = {system_error, ErrorType, ErrorDetails, erlang:timestamp(), State#live_trader_state.agent_id},
    log_system_error(SystemErrorRecord),
    
    %% Update risk state with system error
    RiskState = State#live_trader_state.risk_state,
    SystemViolation = {system_error, ErrorType, erlang:timestamp(), ErrorDetails},
    UpdatedRiskState = RiskState#risk_state{
        risk_violations = [SystemViolation | RiskState#risk_state.risk_violations]
    },
    
    %% Implement error-specific handling
    case ErrorType of
        neural_network_failure ->
            handle_neural_network_failure(State, ErrorDetails);
        market_data_corruption ->
            handle_market_data_corruption(State, ErrorDetails);
        memory_exhaustion ->
            handle_memory_exhaustion(State, ErrorDetails);
        process_crash ->
            handle_process_crash(State, ErrorDetails);
        _ ->
            %% Generic system error handling
            State#live_trader_state{risk_state = UpdatedRiskState}
    end.

%% Emergency close all positions
emergency_close_positions(State) ->
    io:format("EMERGENCY: Closing all positions~n"),
    
    CurrentPositions = State#live_trader_state.current_positions,
    
    %% Attempt to close each position
    lists:foreach(fun(Position) ->
        Symbol = Position#position_info.symbol,
        Quantity = Position#position_info.quantity,
        Side = Position#position_info.side,
        
        %% Determine close action
        CloseAction = case Side of
            long -> "SELL";
            short -> "BUY"
        end,
        
        io:format("Emergency closing ~s position: ~s ~p ~s~n", [Side, CloseAction, Quantity, Symbol]),
        
        %% Attempt to place close order (may fail if connection is down)
        case ib_bridge_connector:place_order(Symbol, CloseAction, Quantity, "MKT") of
            ok ->
                io:format("Emergency close order placed for ~s~n", [Symbol]);
            {error, Reason} ->
                io:format("Failed to place emergency close order for ~s: ~p~n", [Symbol, Reason]),
                %% Log for manual intervention
                log_failed_emergency_close(Symbol, Side, Quantity, Reason)
        end
    end, CurrentPositions),
    
    %% Clear positions (they may not actually be closed if orders failed)
    State#live_trader_state{current_positions = []}.

%% Determine if trading should continue after error
should_continue_after_error(ErrorType) ->
    case ErrorType of
        neural_network_failure -> false;  % Cannot trade without neural network
        market_data_corruption -> false;  % Cannot trade with bad data
        memory_exhaustion -> false;       % System stability compromised
        process_crash -> false;           % System integrity compromised
        connection_timeout -> true;       % May recover
        minor_data_issue -> true;         % Can continue with caution
        _ -> false  % Conservative default
    end.

%% Check if trading should resume after connection recovery
should_resume_trading_after_recovery(State) ->
    RiskState = State#live_trader_state.risk_state,
    
    %% Check if we're within risk limits
    DailyLossLimit = config:live_max_daily_loss(),
    MaxDrawdownLimit = config:live_max_drawdown_limit(),
    
    %% Check recent violations
    RecentViolations = count_recent_violations(RiskState#risk_state.risk_violations),
    
    %% Resume only if risk conditions are acceptable
    (RiskState#risk_state.daily_pnl > -DailyLossLimit) andalso
    (RiskState#risk_state.max_drawdown > -MaxDrawdownLimit) andalso
    (RecentViolations < 3).  % No more than 3 recent violations

%% Resubscribe to market data after recovery
resubscribe_after_recovery(State) ->
    %% Get configured currency pairs
    CurrencyPairs = config:live_currency_pairs(),
    
    %% Attempt to resubscribe
    case subscribe_to_pairs(CurrencyPairs, 1, State) of
        ok ->
            io:format("Successfully resubscribed to market data~n"),
            ok;
        {error, Reason} ->
            io:format("Failed to resubscribe to market data: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Handle specific error types
handle_neural_network_failure(State, ErrorDetails) ->
    io:format("Handling neural network failure: ~p~n", [ErrorDetails]),
    
    %% Attempt to restart neural network
    case attempt_neural_network_restart(State) of
        {ok, NewState} ->
            io:format("Neural network restarted successfully~n"),
            NewState;
        {error, Reason} ->
            io:format("Failed to restart neural network: ~p~n", [Reason]),
            %% Mark trading as inactive
            State#live_trader_state{trading_active = false}
    end.

handle_market_data_corruption(State, ErrorDetails) ->
    io:format("Handling market data corruption: ~p~n", [ErrorDetails]),
    
    %% Clear corrupted data and request fresh data
    clear_market_data_cache(),
    
    %% Pause trading briefly to allow data refresh
    erlang:send_after(5000, self(), resume_after_data_refresh),
    
    State#live_trader_state{trading_active = false}.

handle_memory_exhaustion(State, ErrorDetails) ->
    io:format("Handling memory exhaustion: ~p~n", [ErrorDetails]),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    
    %% Clear non-essential caches
    clear_performance_caches(),
    
    %% Reduce trading frequency temporarily
    State.

handle_process_crash(State, ErrorDetails) ->
    io:format("Handling process crash: ~p~n", [ErrorDetails]),
    
    %% Attempt to restart crashed processes
    restart_crashed_processes(ErrorDetails),
    
    State.

%% ============================================================================
%% Error Recovery and Restart Functions
%% ============================================================================

%% Attempt to restart neural network
attempt_neural_network_restart(State) ->
    AgentId = State#live_trader_state.agent_id,
    
    %% Stop existing exoself if running
    case State#live_trader_state.exoself_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            exit(Pid, restart_required)
    end,
    
    %% Wait a moment for cleanup
    timer:sleep(1000),
    
    %% Attempt to restart
    case deploy_neural_network(AgentId, State#live_trader_state.live_scape_pid) of
        {ok, NewExoselfPid} ->
            NewState = State#live_trader_state{exoself_pid = NewExoselfPid},
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Clear market data cache
clear_market_data_cache() ->
    io:format("Clearing market data cache~n"),
    %% Clear ETS tables
    case ets:info(live_market_ticks) of
        undefined -> ok;
        _ -> ets:delete_all_objects(live_market_ticks)
    end,
    case ets:info(live_ohlc_data) of
        undefined -> ok;
        _ -> ets:delete_all_objects(live_ohlc_data)
    end.

%% Clear performance caches to free memory
clear_performance_caches() ->
    io:format("Clearing performance caches~n"),
    case ets:info(live_performance_snapshots) of
        undefined -> ok;
        _ -> 
            %% Keep only recent snapshots
            AllSnapshots = ets:tab2list(live_performance_snapshots),
            RecentSnapshots = lists:sublist(lists:reverse(AllSnapshots), 100),
            ets:delete_all_objects(live_performance_snapshots),
            lists:foreach(fun(Snapshot) -> ets:insert(live_performance_snapshots, Snapshot) end, RecentSnapshots)
    end.

%% Restart crashed processes
restart_crashed_processes(ErrorDetails) ->
    io:format("Attempting to restart crashed processes: ~p~n", [ErrorDetails]),
    %% Implementation would depend on specific process architecture
    %% For now, just log the attempt
    ok.

%% Count recent violations (within last hour)
count_recent_violations(Violations) ->
    OneHourAgo = erlang:timestamp(),
    RecentViolations = lists:filter(fun({_, _, Timestamp, _}) ->
        TimeDiff = timer:now_diff(erlang:timestamp(), Timestamp),
        TimeDiff < 3600000000  % 1 hour in microseconds
    end, Violations),
    length(RecentViolations).

%% ============================================================================
%% Error Logging Functions
%% ============================================================================

%% Log emergency events
log_emergency_event(EmergencyRecord) ->
    io:format("EMERGENCY EVENT LOGGED: ~p~n", [EmergencyRecord]),
    %% In production, would write to persistent emergency log
    ok.

%% Log recovery events
log_recovery_event(RecoveryRecord) ->
    io:format("RECOVERY EVENT LOGGED: ~p~n", [RecoveryRecord]),
    ok.

%% Log system errors
log_system_error(SystemErrorRecord) ->
    io:format("SYSTEM ERROR LOGGED: ~p~n", [SystemErrorRecord]),
    ok.

%% Log failed emergency closes
log_failed_emergency_close(Symbol, Side, Quantity, Reason) ->
    FailedCloseRecord = {failed_emergency_close, Symbol, Side, Quantity, Reason, erlang:timestamp()},
    io:format("FAILED EMERGENCY CLOSE LOGGED: ~p~n", [FailedCloseRecord]),
    %% This is critical - in production would trigger alerts
    ok.

%% Notify external systems of emergency stop
notify_emergency_stop(ErrorCode, ErrorMsg, Timestamp) ->
    io:format("Notifying external systems of emergency stop~n"),
    %% In production, would send alerts, emails, etc.
    %% For now, just ensure it's logged prominently
    io:format("*** EMERGENCY STOP NOTIFICATION ***~n"),
    io:format("Error Code: ~p~n", [ErrorCode]),
    io:format("Error Message: ~s~n", [ErrorMsg]),
    io:format("Timestamp: ~p~n", [Timestamp]),
    io:format("*** END EMERGENCY NOTIFICATION ***~n").

%% ============================================================================
%% Risk Management
%% ============================================================================

%% Check comprehensive risk limits and trading constraints
check_risk_limits(State) ->
    RiskState = State#live_trader_state.risk_state,
    Performance = State#live_trader_state.performance_data,
    
    %% Reset daily counters if new day
    UpdatedRiskState = reset_daily_counters_if_needed(RiskState),
    
    %% Perform all risk checks
    RiskChecks = [
        check_daily_loss_limit(UpdatedRiskState, Performance),
        check_max_drawdown_limit(UpdatedRiskState, Performance),
        check_daily_trade_limit(UpdatedRiskState),
        check_account_balance_limit(Performance),
        check_total_exposure_limit(UpdatedRiskState)
    ],
    
    %% Find first violation
    case lists:filter(fun({Result, _}) -> Result =/= ok end, RiskChecks) of
        [] ->
            %% Update state with reset counters
            UpdatedState = State#live_trader_state{risk_state = UpdatedRiskState},
            {ok, UpdatedState};
        [{halt, Reason} | _] ->
            %% Log violation and halt
            Violation = {Reason, erlang:timestamp(), get_risk_details(State)},
            NewViolations = [Violation | UpdatedRiskState#risk_state.risk_violations],
            FinalRiskState = UpdatedRiskState#risk_state{risk_violations = NewViolations},
            FinalState = State#live_trader_state{risk_state = FinalRiskState},
            {halt, Reason, FinalState}
    end.

%% Check daily loss limit
check_daily_loss_limit(RiskState, Performance) ->
    MaxDailyLoss = config:live_max_daily_loss(),
    StartBalance = case RiskState#risk_state.daily_start_balance of
        undefined -> Performance#performance_metrics.total_pnl; % Fallback
        Balance -> Balance
    end,
    DailyLossLimit = StartBalance * MaxDailyLoss,
    
    if
        RiskState#risk_state.daily_pnl < -DailyLossLimit ->
            {halt, daily_loss_limit_exceeded};
        true ->
            {ok, daily_loss_check_passed}
    end.

%% Check maximum drawdown limit
check_max_drawdown_limit(RiskState, Performance) ->
    MaxDrawdownLimit = config:live_max_drawdown_limit(),
    StartBalance = case RiskState#risk_state.daily_start_balance of
        undefined -> 10000; % Default fallback
        Balance -> Balance
    end,
    DrawdownLimit = StartBalance * MaxDrawdownLimit,
    
    if
        RiskState#risk_state.max_drawdown < -DrawdownLimit ->
            {halt, max_drawdown_exceeded};
        true ->
            {ok, drawdown_check_passed}
    end.

%% Check daily trade limit
check_daily_trade_limit(RiskState) ->
    MaxDailyTrades = config:live_daily_trade_limit(),
    
    if
        RiskState#risk_state.daily_trades >= MaxDailyTrades ->
            {halt, daily_trade_limit_exceeded};
        true ->
            {ok, trade_limit_check_passed}
    end.

%% Check minimum account balance
check_account_balance_limit(Performance) ->
    MinBalance = config:live_min_account_balance(),
    CurrentBalance = Performance#performance_metrics.total_pnl,
    
    if
        CurrentBalance =< MinBalance ->
            {halt, minimum_balance_exceeded};
        true ->
            {ok, balance_check_passed}
    end.

%% Check total exposure limit
check_total_exposure_limit(RiskState) ->
    MaxTotalExposure = config:live_max_total_exposure(),
    
    if
        RiskState#risk_state.total_exposure > MaxTotalExposure ->
            {halt, total_exposure_exceeded};
        true ->
            {ok, exposure_check_passed}
    end.

%% Check if new position would violate position limits
check_position_limits(Symbol, PositionSize, AccountBalance, CurrentPositions) ->
    %% Check per-pair position limit
    MaxPositionPerPair = config:live_max_position_per_pair(),
    MaxPositionAmount = AccountBalance * MaxPositionPerPair,
    
    %% Calculate current exposure for this symbol
    CurrentExposure = calculate_symbol_exposure(Symbol, CurrentPositions),
    NewTotalExposure = CurrentExposure + PositionSize,
    
    if
        NewTotalExposure > MaxPositionAmount ->
            {error, position_limit_per_pair_exceeded};
        true ->
            %% Check total portfolio exposure
            TotalExposure = calculate_total_exposure(CurrentPositions) + PositionSize,
            MaxTotalExposure = AccountBalance * config:live_max_total_exposure(),
            
            if
                TotalExposure > MaxTotalExposure ->
                    {error, total_exposure_limit_exceeded};
                true ->
                    ok
            end
    end.

%% Check margin requirements before placing order
check_margin_requirements(Symbol, PositionSize, AccountBalance) ->
    MarginRequirement = config:live_margin_requirement(),
    RequiredMargin = PositionSize * MarginRequirement,
    
    %% Get current account info from IB connector
    case ib_bridge_connector:get_account_info() of
        {ok, AccountInfo} ->
            AvailableMargin = extract_available_margin(AccountInfo),
            
            if
                RequiredMargin > AvailableMargin ->
                    {error, insufficient_margin};
                true ->
                    {ok, margin_sufficient}
            end;
        {error, _Reason} ->
            %% Fallback to simple balance check
            if
                RequiredMargin > AccountBalance * 0.1 -> % Conservative 10% margin
                    {error, insufficient_margin_fallback};
                true ->
                    {ok, margin_check_fallback}
            end
    end.

%% Calculate position size based on account balance and risk parameters
calculate_position_size(Symbol, AccountBalance, RiskParams) ->
    BasePositionSize = maps:get(position_size, RiskParams, config:live_position_size()),
    MaxPositionPerPair = config:live_max_position_per_pair(),
    
    %% Use the more conservative of the two limits
    EffectivePositionSize = min(BasePositionSize, MaxPositionPerPair),
    
    %% Calculate actual position size in currency units
    PositionAmount = AccountBalance * EffectivePositionSize,
    
    %% Convert to lot size (simplified - in production would use current price)
    LotSize = config:account_lot_size(),
    Lots = round(PositionAmount / LotSize),
    
    %% Ensure minimum position size
    max(Lots, 1).

%% Reset daily counters if new trading day
reset_daily_counters_if_needed(RiskState) ->
    Today = date(),
    LastResetDate = RiskState#risk_state.last_reset_date,
    
    case LastResetDate of
        Today ->
            RiskState; % Same day, no reset needed
        _ ->
            %% New day - reset daily counters
            RiskState#risk_state{
                daily_pnl = 0.0,
                daily_trades = 0,
                last_reset_date = Today
            }
    end.

%% Update risk state after trade execution
update_risk_state_after_trade(RiskState, TradeResult, TradeAmount) ->
    UpdatedRiskState = RiskState#risk_state{
        daily_trades = RiskState#risk_state.daily_trades + 1,
        daily_pnl = RiskState#risk_state.daily_pnl + TradeResult
    },
    
    %% Update max drawdown if necessary
    NewDrawdown = min(UpdatedRiskState#risk_state.max_drawdown, 
                     UpdatedRiskState#risk_state.daily_pnl),
    
    UpdatedRiskState#risk_state{max_drawdown = NewDrawdown}.

%% Calculate current exposure for a specific symbol
calculate_symbol_exposure(Symbol, Positions) ->
    SymbolPositions = [P || P <- Positions, P#position_info.symbol =:= Symbol],
    lists:sum([P#position_info.exposure_amount || P <- SymbolPositions]).

%% Calculate total portfolio exposure
calculate_total_exposure(Positions) ->
    lists:sum([P#position_info.exposure_amount || P <- Positions]).

%% Extract available margin from IB account info
extract_available_margin(AccountInfo) ->
    case lists:keyfind("AvailableFunds", 1, AccountInfo) of
        {"AvailableFunds", Value, _Currency} ->
            case string:to_float(Value) of
                {Float, _} -> Float;
                error -> 0.0
            end;
        false ->
            0.0 % Conservative fallback
    end.

%% Get risk details for logging
get_risk_details(State) ->
    RiskState = State#live_trader_state.risk_state,
    Performance = State#live_trader_state.performance_data,
    
    [
        {daily_pnl, RiskState#risk_state.daily_pnl},
        {daily_trades, RiskState#risk_state.daily_trades},
        {max_drawdown, RiskState#risk_state.max_drawdown},
        {total_exposure, RiskState#risk_state.total_exposure},
        {account_balance, Performance#performance_metrics.total_pnl},
        {positions_count, length(State#live_trader_state.current_positions)}
    ].

%% Get default risk parameters from config
get_risk_parameters() ->
    #{
        position_size => config:live_position_size(),
        max_daily_loss => config:live_max_daily_loss(),
        max_position_per_pair => config:live_max_position_per_pair(),
        max_total_exposure => config:live_max_total_exposure(),
        min_account_balance => config:live_min_account_balance(),
        margin_requirement => config:live_margin_requirement(),
        max_drawdown_limit => config:live_max_drawdown_limit(),
        daily_trade_limit => config:live_daily_trade_limit(),
        currency_pairs => config:live_currency_pairs()
    }.

%% Merge user-provided risk parameters with defaults
merge_risk_parameters(DefaultParams, UserParams) ->
    maps:merge(DefaultParams, UserParams).

%% ============================================================================
%% Performance Tracking
%% ============================================================================

%% Get comprehensive performance metrics
get_performance() ->
    case whereis(live_trader) of
        undefined ->
            {error, not_running};
        Pid ->
            Pid ! {get_performance, self()},
            receive
                {performance_data, Performance} ->
                    %% Enhance with real-time calculations
                    EnhancedPerformance = calculate_enhanced_metrics(Performance),
                    {ok, EnhancedPerformance}
            after 1000 ->
                {error, timeout}
            end
    end.

%% Calculate enhanced performance metrics with real-time data
calculate_enhanced_metrics(Performance) ->
    %% Get trade history from ETS
    TradeHistory = ets:tab2list(live_trade_history),
    
    %% Calculate win rate
    WinRate = calculate_win_rate(TradeHistory),
    
    %% Calculate average trade P&L
    AvgTradePnL = calculate_average_trade_pnl(TradeHistory),
    
    %% Calculate Sharpe ratio (simplified)
    SharpeRatio = calculate_sharpe_ratio(TradeHistory),
    
    %% Calculate maximum consecutive losses
    MaxConsecutiveLosses = calculate_max_consecutive_losses(TradeHistory),
    
    %% Get current drawdown
    CurrentDrawdown = calculate_current_drawdown(TradeHistory),
    
    %% Create enhanced performance record
    #{
        %% Basic metrics from performance_metrics record
        start_time => Performance#performance_metrics.start_time,
        total_trades => Performance#performance_metrics.total_trades,
        winning_trades => Performance#performance_metrics.winning_trades,
        total_pnl => Performance#performance_metrics.total_pnl,
        current_position => Performance#performance_metrics.current_position,
        daily_pnl => Performance#performance_metrics.daily_pnl,
        max_drawdown => Performance#performance_metrics.max_drawdown,
        last_update => Performance#performance_metrics.last_update,
        
        %% Enhanced real-time metrics
        win_rate => WinRate,
        avg_trade_pnl => AvgTradePnL,
        sharpe_ratio => SharpeRatio,
        max_consecutive_losses => MaxConsecutiveLosses,
        current_drawdown => CurrentDrawdown,
        
        %% Trading session metrics
        session_duration => calculate_session_duration(Performance#performance_metrics.start_time),
        trades_per_hour => calculate_trades_per_hour(Performance, TradeHistory),
        
        %% Risk metrics
        profit_factor => calculate_profit_factor(TradeHistory),
        recovery_factor => calculate_recovery_factor(Performance#performance_metrics.total_pnl, 
                                                   Performance#performance_metrics.max_drawdown)
    }.

%% Calculate win rate from trade history
calculate_win_rate([]) -> 0.0;
calculate_win_rate(TradeHistory) ->
    WinningTrades = length([Trade || {_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL} = Trade <- TradeHistory, PnL > 0]),
    TotalTrades = length(TradeHistory),
    case TotalTrades of
        0 -> 0.0;
        _ -> (WinningTrades / TotalTrades) * 100.0
    end.

%% Calculate average trade P&L
calculate_average_trade_pnl([]) -> 0.0;
calculate_average_trade_pnl(TradeHistory) ->
    TotalPnL = lists:sum([PnL || {_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL} <- TradeHistory]),
    TotalTrades = length(TradeHistory),
    case TotalTrades of
        0 -> +0.0;
        _ -> TotalPnL / TotalTrades
    end.

%% Calculate simplified Sharpe ratio
calculate_sharpe_ratio([]) -> 0.0;
calculate_sharpe_ratio(TradeHistory) when length(TradeHistory) < 2 -> 0.0;
calculate_sharpe_ratio(TradeHistory) ->
    Returns = [PnL || {_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL} <- TradeHistory],
    MeanReturn = lists:sum(Returns) / length(Returns),
    
    %% Calculate standard deviation
    Variance = lists:sum([math:pow(R - MeanReturn, 2) || R <- Returns]) / (length(Returns) - 1),
    StdDev = math:sqrt(Variance),
    
    case StdDev of
        +0.0 -> +0.0;
        _ -> MeanReturn / StdDev
    end.

%% Calculate maximum consecutive losses
calculate_max_consecutive_losses([]) -> 0;
calculate_max_consecutive_losses(TradeHistory) ->
    %% Sort by timestamp and extract P&L values
    SortedTrades = lists:sort(fun({_, T1, _, _, _, _, _}, {_, T2, _, _, _, _, _}) -> T1 =< T2 end, TradeHistory),
    PnLValues = [PnL || {_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL} <- SortedTrades],
    
    %% Count consecutive losses
    count_max_consecutive_losses(PnLValues, 0, 0).

count_max_consecutive_losses([], CurrentStreak, MaxStreak) ->
    max(CurrentStreak, MaxStreak);
count_max_consecutive_losses([PnL | Rest], CurrentStreak, MaxStreak) ->
    case PnL < 0 of
        true ->
            NewCurrentStreak = CurrentStreak + 1,
            count_max_consecutive_losses(Rest, NewCurrentStreak, max(NewCurrentStreak, MaxStreak));
        false ->
            count_max_consecutive_losses(Rest, 0, MaxStreak)
    end.

%% Calculate current drawdown
calculate_current_drawdown([]) -> 0.0;
calculate_current_drawdown(TradeHistory) ->
    %% Sort trades by timestamp
    SortedTrades = lists:sort(fun({_, T1, _, _, _, _, _}, {_, T2, _, _, _, _, _}) -> T1 =< T2 end, TradeHistory),
    
    %% Calculate running P&L and find peak
    {_, _, CurrentDrawdown} = lists:foldl(
        fun({_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL}, {RunningPnL, Peak, MaxDD}) ->
            NewRunningPnL = RunningPnL + PnL,
            NewPeak = max(Peak, NewRunningPnL),
            NewDrawdown = min(MaxDD, NewRunningPnL - NewPeak),
            {NewRunningPnL, NewPeak, NewDrawdown}
        end,
        {0.0, 0.0, 0.0},
        SortedTrades
    ),
    CurrentDrawdown.

%% Calculate session duration in hours
calculate_session_duration(StartTime) ->
    Now = erlang:timestamp(),
    DiffMicroseconds = timer:now_diff(Now, StartTime),
    DiffMicroseconds / (1000000 * 3600). % Convert to hours

%% Calculate trades per hour
calculate_trades_per_hour(Performance, TradeHistory) ->
    SessionHours = calculate_session_duration(Performance#performance_metrics.start_time),
    case SessionHours of
        +0.0 -> +0.0;
        _ -> length(TradeHistory) / SessionHours
    end.

%% Calculate profit factor (gross profit / gross loss)
calculate_profit_factor([]) -> 0.0;
calculate_profit_factor(TradeHistory) ->
    {GrossProfit, GrossLoss} = lists:foldl(
        fun({_Id, _Time, _Symbol, _Action, _Qty, _Price, PnL}, {Profit, Loss}) ->
            case PnL > 0 of
                true -> {Profit + PnL, Loss};
                false -> {Profit, Loss + abs(PnL)}
            end
        end,
        {0.0, 0.0},
        TradeHistory
    ),
    case GrossLoss of
        +0.0 -> 
            case GrossProfit > 0 of
                true -> 999.0; % Infinite profit factor
                false -> +0.0
            end;
        _ -> GrossProfit / GrossLoss
    end.

%% Calculate recovery factor (net profit / max drawdown)
calculate_recovery_factor(_NetProfit, +0.0) -> +0.0;
calculate_recovery_factor(NetProfit, MaxDrawdown) ->
    NetProfit / abs(MaxDrawdown).

%% Record trade in ETS table for performance tracking
record_trade_for_performance(TradeId, Timestamp, Symbol, Action, Quantity, Price, PnL) ->
    TradeRecord = {TradeId, Timestamp, Symbol, Action, Quantity, Price, PnL},
    ets:insert(live_trade_history, TradeRecord),
    
    %% Also create performance snapshot
    create_performance_snapshot(Timestamp).

%% Create performance snapshot at specific timestamp
create_performance_snapshot(Timestamp) ->
    case get_performance() of
        {ok, Performance} ->
            SnapshotRecord = {Timestamp, Performance},
            ets:insert(live_performance_snapshots, SnapshotRecord);
        {error, _} ->
            ok % Skip snapshot if performance data unavailable
    end.

%% Compare performance with backtesting results
compare_with_backtesting(AgentId) ->
    %% Get current live performance
    case get_performance() of
        {ok, LivePerformance} ->
            %% Get backtesting results from Mnesia
            case get_backtesting_results(AgentId) of
                {ok, BacktestResults} ->
                    %% Calculate comparison metrics
                    Comparison = calculate_performance_comparison(LivePerformance, BacktestResults),
                    
                    %% Store comparison in ETS
                    ets:insert(backtesting_comparison, {AgentId, Comparison}),
                    
                    {ok, Comparison};
                {error, Reason} ->
                    {error, {backtesting_data_unavailable, Reason}}
            end;
        {error, Reason} ->
            {error, {live_performance_unavailable, Reason}}
    end.

%% Get backtesting results from Mnesia database
get_backtesting_results(AgentId) ->
    try
        case genotype:dirty_read({agent, AgentId}) of
            undefined ->
                {error, agent_not_found};
            Agent ->
                %% Extract fitness and other metrics from agent record
                BacktestResults = #{
                    fitness => Agent#agent.fitness,
                    generation => Agent#agent.generation,
                    innovation_factor => Agent#agent.innovation_factor,
                    constraint => Agent#agent.constraint,
                    evo_hist => Agent#agent.evo_hist
                },
                {ok, BacktestResults}
        end
    catch
        Error:Reason ->
            {error, {mnesia_error, Error, Reason}}
    end.

%% Calculate performance comparison between live and backtesting
calculate_performance_comparison(LivePerformance, BacktestResults) ->
    LivePnL = maps:get(total_pnl, LivePerformance, 0.0),
    BacktestFitness = maps:get(fitness, BacktestResults, 0.0),
    
    %% Calculate performance ratio
    PerformanceRatio = case BacktestFitness of
        +0.0 -> +0.0;
        _ -> LivePnL / BacktestFitness
    end,
    
    %% Calculate win rate comparison
    LiveWinRate = maps:get(win_rate, LivePerformance, 0.0),
    %% Estimate backtest win rate (simplified)
    BacktestWinRate = estimate_backtest_win_rate(BacktestResults),
    WinRateDiff = LiveWinRate - BacktestWinRate,
    
    %% Calculate drawdown comparison
    LiveDrawdown = maps:get(max_drawdown, LivePerformance, 0.0),
    BacktestDrawdown = estimate_backtest_drawdown(BacktestResults),
    DrawdownDiff = LiveDrawdown - BacktestDrawdown,
    
    #{
        live_pnl => LivePnL,
        backtest_fitness => BacktestFitness,
        performance_ratio => PerformanceRatio,
        performance_difference => LivePnL - BacktestFitness,
        
        live_win_rate => LiveWinRate,
        backtest_win_rate => BacktestWinRate,
        win_rate_difference => WinRateDiff,
        
        live_drawdown => LiveDrawdown,
        backtest_drawdown => BacktestDrawdown,
        drawdown_difference => DrawdownDiff,
        
        comparison_timestamp => erlang:timestamp(),
        
        %% Performance classification
        performance_status => classify_performance_status(PerformanceRatio, WinRateDiff, DrawdownDiff)
    }.

%% Estimate backtest win rate (simplified heuristic)
estimate_backtest_win_rate(BacktestResults) ->
    Fitness = maps:get(fitness, BacktestResults, 0.0),
    %% Simple heuristic: positive fitness suggests >50% win rate
    case Fitness > 0 of
        true -> 55.0 + min(Fitness * 0.1, 20.0); % Cap at 75%
        false -> 45.0 + max(Fitness * 0.1, -20.0) % Floor at 25%
    end.

%% Estimate backtest drawdown (simplified heuristic)
estimate_backtest_drawdown(BacktestResults) ->
    Fitness = maps:get(fitness, BacktestResults, 0.0),
    %% Simple heuristic: higher fitness suggests lower drawdown
    case Fitness > 0 of
        true -> -abs(Fitness) * 0.2; % Negative drawdown
        false -> Fitness * 0.5 % Larger negative drawdown for poor performance
    end.

%% Classify performance status based on comparison metrics
classify_performance_status(PerformanceRatio, WinRateDiff, DrawdownDiff) ->
    case {PerformanceRatio >= 0.8, WinRateDiff >= -5.0, DrawdownDiff >= -10.0} of
        {true, true, true} -> excellent;
        {true, true, false} -> good_but_risky;
        {true, false, true} -> profitable_but_inconsistent;
        {false, true, true} -> underperforming;
        {false, false, false} -> poor;
        _ -> mixed_results
    end.

%% Update performance metrics after neural network evaluation
update_performance_metrics(State, Fitness, _Cycles, _Time) ->
    Performance = State#live_trader_state.performance_data,
    
    %% Determine if this was a winning trade
    IsWinningTrade = Fitness > 0,
    WinningTrades = case IsWinningTrade of
        true -> Performance#performance_metrics.winning_trades + 1;
        false -> Performance#performance_metrics.winning_trades
    end,
    
    %% Update metrics
    UpdatedPerformance = Performance#performance_metrics{
        total_trades = Performance#performance_metrics.total_trades + 1,
        winning_trades = WinningTrades,
        total_pnl = Performance#performance_metrics.total_pnl + Fitness,
        last_update = erlang:timestamp()
    },
    
    %% Update daily P&L (simplified - should track by calendar day)
    NewDailyPnL = Performance#performance_metrics.daily_pnl + Fitness,
    
    %% Update max drawdown
    NewMaxDrawdown = min(Performance#performance_metrics.max_drawdown, NewDailyPnL),
    
    FinalPerformance = UpdatedPerformance#performance_metrics{
        daily_pnl = NewDailyPnL,
        max_drawdown = NewMaxDrawdown
    },
    
    %% Record trade for performance tracking
    TradeId = generate_trade_id(),
    record_trade_for_performance(TradeId, erlang:timestamp(), "EURUSD", 
                                "EVALUATION", 1, 1.0, Fitness),
    
    State#live_trader_state{performance_data = FinalPerformance}.

%% Generate unique trade ID
generate_trade_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

%% Record trade execution with comprehensive risk tracking
record_trade_execution_with_risk(State, Timestamp, Symbol, Action, Quantity, Price) ->
    Performance = State#live_trader_state.performance_data,
    RiskState = State#live_trader_state.risk_state,
    CurrentPositions = State#live_trader_state.current_positions,
    
    %% Calculate trade value
    TradeValue = Quantity * Price,
    
    %% Update performance metrics
    UpdatedPerformance = Performance#performance_metrics{
        total_trades = Performance#performance_metrics.total_trades + 1,
        last_update = Timestamp
    },
    
    %% Update risk state
    UpdatedRiskState = RiskState#risk_state{
        daily_trades = RiskState#risk_state.daily_trades + 1
    },
    
    %% Update position tracking
    UpdatedPositions = update_position_tracking(CurrentPositions, Symbol, Action, Quantity, Price, Timestamp),
    
    %% Calculate new exposure
    TotalExposure = calculate_total_exposure_from_positions(UpdatedPositions),
    FinalRiskState = UpdatedRiskState#risk_state{
        total_exposure = TotalExposure,
        position_exposures = extract_position_exposures(UpdatedPositions)
    },
    
    %% Calculate P&L for this trade (simplified)
    TradePnL = case Action of
        "BUY" -> 0.0; % P&L calculated when position is closed
        "SELL" -> 0.0; % P&L calculated when position is closed
        _ -> 0.0
    end,
    
    %% Record trade for performance tracking
    TradeId = generate_trade_id(),
    record_trade_for_performance(TradeId, Timestamp, Symbol, Action, Quantity, Price, TradePnL),
    
    io:format("Trade recorded with risk tracking: ~s ~p shares of ~s at ~p (Total Exposure: ~p)~n", 
              [Action, Quantity, Symbol, Price, TotalExposure]),
    
    State#live_trader_state{
        performance_data = UpdatedPerformance,
        risk_state = FinalRiskState,
        current_positions = UpdatedPositions
    }.

%% Update position tracking based on trade execution
update_position_tracking(CurrentPositions, Symbol, Action, Quantity, Price, Timestamp) ->
    case Action of
        "BUY" ->
            %% Opening long or closing short
            case find_position(CurrentPositions, Symbol, short) of
                {found, Position} ->
                    %% Closing short position
                    close_position_tracking(CurrentPositions, Position, Quantity);
                not_found ->
                    %% Opening long position
                    NewPosition = #position_info{
                        symbol = Symbol,
                        side = long,
                        quantity = Quantity,
                        entry_price = Price,
                        entry_time = Timestamp,
                        current_price = Price,
                        exposure_amount = Quantity * Price
                    },
                    [NewPosition | CurrentPositions]
            end;
        "SELL" ->
            %% Opening short or closing long
            case find_position(CurrentPositions, Symbol, long) of
                {found, Position} ->
                    %% Closing long position
                    close_position_tracking(CurrentPositions, Position, Quantity);
                not_found ->
                    %% Opening short position
                    NewPosition = #position_info{
                        symbol = Symbol,
                        side = short,
                        quantity = Quantity,
                        entry_price = Price,
                        entry_time = Timestamp,
                        current_price = Price,
                        exposure_amount = Quantity * Price
                    },
                    [NewPosition | CurrentPositions]
            end
    end.

%% Find position by symbol and side
find_position(Positions, Symbol, Side) ->
    case lists:keyfind({Symbol, Side}, 1, 
                      [{P#position_info.symbol, P#position_info.side} || P <- Positions]) of
        false -> not_found;
        _ -> 
            Position = lists:keyfind(Symbol, #position_info.symbol, Positions),
            {found, Position}
    end.

%% Close position tracking
close_position_tracking(CurrentPositions, Position, Quantity) ->
    if
        Position#position_info.quantity =< Quantity ->
            %% Fully closing position
            lists:delete(Position, CurrentPositions);
        true ->
            %% Partially closing position
            UpdatedPosition = Position#position_info{
                quantity = Position#position_info.quantity - Quantity,
                exposure_amount = (Position#position_info.quantity - Quantity) * Position#position_info.entry_price
            },
            lists:keyreplace(Position#position_info.symbol, #position_info.symbol, 
                           CurrentPositions, UpdatedPosition)
    end.

%% Calculate total exposure from current positions
calculate_total_exposure_from_positions(Positions) ->
    lists:sum([P#position_info.exposure_amount || P <- Positions]).

%% Extract position exposures for risk state
extract_position_exposures(Positions) ->
    [{P#position_info.symbol, P#position_info.exposure_amount, P#position_info.entry_time} 
     || P <- Positions].

%% Legacy function for compatibility
record_trade_execution(State, OrderId, Symbol, Action, Quantity, Price) ->
    record_trade_execution_with_risk(State, erlang:timestamp(), Symbol, Action, Quantity, Price).

%% ============================================================================
%% Cleanup and Shutdown
%% ============================================================================

%% Clean up resources and stop trading
cleanup_and_stop(State) ->
    io:format("Cleaning up live trading resources~n"),
    
    %% Stop neural network
    case State#live_trader_state.exoself_pid of
        undefined -> ok;
        ExoselfPid -> 
            exit(ExoselfPid, normal)
    end,
    
    %% Clean up live components
    cleanup_components(State#live_trader_state.ib_connector_pid, 
                      State#live_trader_state.live_scape_pid),
    
    %% Clean up performance tracking ETS tables
    cleanup_performance_tables(),
    
    %% Unregister process
    case whereis(live_trader) of
        undefined -> ok;
        _ -> unregister(live_trader)
    end,
    
    %% Send confirmation
    case whereis(live_trader) of
        undefined -> ok;
        Pid -> Pid ! {live_trader, stopped}
    end,
    
    io:format("Live trading cleanup completed~n").

%% Clean up performance tracking ETS tables
cleanup_performance_tables() ->
    %% Delete ETS tables if they exist
    lists:foreach(fun(TableName) ->
        case ets:info(TableName) of
            undefined -> ok;
            _ -> ets:delete(TableName)
        end
    end, [live_trade_history, live_performance_snapshots, backtesting_comparison]).

%% Clean up IB connector and live scape
cleanup_components(IBPid, ScapePid) ->
    %% Stop IB connector
    case IBPid of
        undefined -> ok;
        _ -> 
            ib_bridge_connector:cleanup_market_data_tables(),
            ib_bridge_connector:stop_connection()
    end,
    
    %% Stop live scape
    case ScapePid of
        undefined -> ok;
        _ -> 
            ScapePid ! terminate
    end.

%% ============================================================================
%% Utility Functions
%% ============================================================================

%% Get current timestamp for logging
get_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                  [Year, Month, Day, Hour, Min, Sec]).

%% Format performance data for display
format_performance(Performance) ->
    [
        {start_time, Performance#performance_metrics.start_time},
        {total_trades, Performance#performance_metrics.total_trades},
        {winning_trades, Performance#performance_metrics.winning_trades},
        {total_pnl, Performance#performance_metrics.total_pnl},
        {current_position, Performance#performance_metrics.current_position},
        {daily_pnl, Performance#performance_metrics.daily_pnl},
        {max_drawdown, Performance#performance_metrics.max_drawdown},
        {last_update, Performance#performance_metrics.last_update}
    ].

%% Sync function for development
sync() ->
    make:all([load]).