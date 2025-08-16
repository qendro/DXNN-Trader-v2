%% Live Scape Module for Sensor/Actuator Interface
%% Provides scape interface compatible with existing sensor/actuator pattern
%% Handles live market data from IB connector and trade execution

-module(live_scape).
-compile(export_all).
-include("records.hrl").

%% API for supervisor integration
-export([start_link/0]).



%% ETS table for live price data buffer
-define(LIVE_PRICE_BUFFER, live_price_buffer).
-define(MAX_BUFFER_SIZE, 1000).

%% ============================================================================
%% Public API - Scape Interface
%% ============================================================================

%% Start link function for supervisor integration
start_link() ->
    Pid = spawn_link(?MODULE, init_scape, []),
    register(live_scape, Pid),
    {ok, Pid}.

%% Initialize scape process
init_scape() ->
    %% Initialize ETS table for price buffer
    init_price_buffer(),
    
    %% Wait for exoself to connect
    receive
        {ExoSelf_PId, live_sim} ->
            live_sim(ExoSelf_PId)
    end.

%% Entry point matching existing scape pattern
gen(ExoSelf_PId, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
    receive 
        {ExoSelf_PId, Name} ->
            live_scape:Name(ExoSelf_PId)
    end.

%% Main live trading scape function
live_sim(ExoSelf_PId) ->
    io:format("Live scape started~n"),
    
    %% Initialize ETS table for price buffer
    init_price_buffer(),
    
    %% Initialize state
    State = #live_state{
        table_name = config:primary_currency_pair(),
        feature = close,
        account_balance = config:account_initial_balance()
    },
    
    %% Start the simulation loop
    live_sim(ExoSelf_PId, State).

%% Main simulation loop - handles sensor and actuator requests
live_sim(ExoSelf_PId, State) ->
    receive
        {From, sense, TableName, Feature, Parameters, Start, Finish} ->
            %% Handle sensor requests for market data with error handling
            io:format("Live scape received sense request: ~p ~p ~p~n", 
                     [TableName, Feature, Parameters]),
            
            try
                {Result, UpdatedState} = handle_sense_request_with_error_handling(TableName, Feature, Parameters, State),
                From ! {self(), Result},
                live_sim(ExoSelf_PId, UpdatedState)
            catch
                Error:Reason ->
                    io:format("Error in sense request: ~p:~p~n", [Error, Reason]),
                    %% Return safe default data
                    SafeResult = generate_safe_sensor_data(Parameters),
                    From ! {self(), SafeResult},
                    %% Log error and continue
                    ErrorState = log_sensor_error(Error, Reason, State),
                    live_sim(ExoSelf_PId, ErrorState)
            end;
            
        {From, sense, internals, Parameters} ->
            %% Handle internal state sensor requests
            Result = handle_internals_request(State),
            From ! {self(), Result},
            live_sim(ExoSelf_PId, State);
            
        {From, trade, TableName, TradeSignal} ->
            %% Handle trade execution requests with comprehensive error handling
            io:format("Live scape received trade signal: ~p~n", [TradeSignal]),
            
            try
                {Fitness, HaltFlag, UpdatedState} = handle_trade_request_with_error_handling(TradeSignal, State),
                From ! {self(), Fitness, HaltFlag},
                live_sim(ExoSelf_PId, UpdatedState)
            catch
                Error:Reason ->
                    io:format("Error in trade execution: ~p:~p~n", [Error, Reason]),
                    %% Return safe response and halt trading
                    From ! {self(), 0, 1},  % No fitness, halt flag set
                    %% Log error and notify emergency
                    ErrorState = log_trade_error(Error, Reason, State),
                    notify_trade_execution_failure(Error, Reason),
                    live_sim(ExoSelf_PId, ErrorState)
            end;
            
        restart ->
            %% Restart with fresh state
            NewState = #live_state{
                table_name = config:primary_currency_pair(),
                feature = close,
                account_balance = config:account_initial_balance()
            },
            live_sim(ExoSelf_PId, NewState);
            
        {emergency_stop, ErrorCode, ErrorMsg} ->
            %% Handle emergency stop from IB connector
            io:format("Live scape received emergency stop: ~p - ~s~n", [ErrorCode, ErrorMsg]),
            EmergencyState = handle_emergency_stop_in_scape(State, ErrorCode, ErrorMsg),
            live_sim(ExoSelf_PId, EmergencyState);
            
        {ib_connection_recovered, Timestamp} ->
            %% Handle connection recovery
            io:format("Live scape notified of connection recovery at ~p~n", [Timestamp]),
            RecoveredState = handle_connection_recovery_in_scape(State, Timestamp),
            live_sim(ExoSelf_PId, RecoveredState);
            
        {market_data_interruption, Reason} ->
            %% Handle market data interruption
            io:format("Market data interruption detected: ~p~n", [Reason]),
            InterruptedState = handle_market_data_interruption(State, Reason),
            live_sim(ExoSelf_PId, InterruptedState);
            
        terminate ->
            %% Clean up and terminate
            cleanup_price_buffer(),
            io:format("Live scape terminated~n"),
            ok
            
    after 10000 ->
        %% Timeout - continue loop
        live_sim(ExoSelf_PId, State)
    end.

%% ============================================================================
%% Enhanced Error Handling for Live Scape
%% ============================================================================

%% Handle sensor requests with comprehensive error handling
handle_sense_request_with_error_handling(TableName, Feature, Parameters, State) ->
    %% Check for market data interruption before processing
    case detect_market_data_interruption_in_scape() of
        {ok, data_available} ->
            %% Normal processing
            handle_sense_request(TableName, Feature, Parameters, State);
        {interrupted, Reason} ->
            io:format("Market data interruption detected during sense: ~p~n", [Reason]),
            %% Attempt recovery
            case attempt_market_data_recovery(Reason) of
                {ok, recovered} ->
                    %% Retry with recovered data
                    handle_sense_request(TableName, Feature, Parameters, State);
                {error, recovery_failed} ->
                    %% Use fallback data
                    {generate_fallback_sensor_data(Parameters), State}
            end
    end.

%% Handle trade requests with comprehensive error handling
handle_trade_request_with_error_handling(TradeSignal, State) ->
    %% Pre-trade validation
    case validate_trade_conditions(TradeSignal, State) of
        {ok, validated} ->
            %% Proceed with trade execution
            execute_trade_with_retry(TradeSignal, State);
        {error, validation_failed, Reason} ->
            io:format("Trade validation failed: ~p~n", [Reason]),
            %% Return safe response
            {0, 0, State}
    end.

%% Handle emergency stop in scape
handle_emergency_stop_in_scape(State, ErrorCode, ErrorMsg) ->
    io:format("Handling emergency stop in scape: ~p - ~s~n", [ErrorCode, ErrorMsg]),
    
    %% Immediately close any open positions
    EmergencyCloseState = emergency_close_positions_in_scape(State),
    
    %% Clear market data to prevent stale data usage
    clear_scape_market_data(),
    
    %% Update state to reflect emergency
    EmergencyCloseState#live_state{
        current_position = 0,
        entry_price = 0,
        unrealized_pnl = 0
    }.

%% Handle connection recovery in scape
handle_connection_recovery_in_scape(State, Timestamp) ->
    io:format("Handling connection recovery in scape at ~p~n", [Timestamp]),
    
    %% Clear any stale market data
    clear_scape_market_data(),
    
    %% Reset price buffer
    init_price_buffer(),
    
    %% State remains unchanged - let normal operations resume
    State.

%% Handle market data interruption
handle_market_data_interruption(State, Reason) ->
    io:format("Handling market data interruption: ~p~n", [Reason]),
    
    %% Log the interruption
    log_market_data_interruption(Reason),
    
    %% Attempt to recover
    case attempt_market_data_recovery(Reason) of
        {ok, recovered} ->
            io:format("Market data recovery successful~n"),
            State;
        {error, recovery_failed} ->
            io:format("Market data recovery failed~n"),
            %% Notify live_trader of data issues
            notify_market_data_failure(Reason),
            State
    end.

%% Detect market data interruption in scape
detect_market_data_interruption_in_scape() ->
    %% Check if IB connector is providing recent data
    case ib_bridge_connector:get_connection_status() of
        {ok, true} ->
            %% Connection is up, check data freshness
            case check_data_freshness() of
                {ok, fresh} -> {ok, data_available};
                {stale, Age} when Age > 60 -> {interrupted, stale_data};
                _ -> {ok, data_available}
            end;
        {ok, false} ->
            {interrupted, connection_down};
        {error, Reason} ->
            {interrupted, {connector_error, Reason}}
    end.

%% Check data freshness
check_data_freshness() ->
    %% Check timestamp of most recent market data
    case ets:info(live_market_ticks) of
        undefined -> {stale, infinity};
        _ ->
            case ets:first(live_market_ticks) of
                '$end_of_table' -> {stale, infinity};
                FirstKey ->
                    case ets:lookup(live_market_ticks, FirstKey) of
                        [{_, Tick}] ->
                            Age = timer:now_diff(erlang:timestamp(), Tick#market_tick.timestamp) / 1000000,
                            if
                                Age < 30 -> {ok, fresh};
                                true -> {stale, Age}
                            end;
                        [] -> {stale, infinity}
                    end
            end
    end.

%% Attempt market data recovery
attempt_market_data_recovery(Reason) ->
    io:format("Attempting market data recovery for reason: ~p~n", [Reason]),
    
    case Reason of
        stale_data ->
            %% Request fresh data subscription
            case request_fresh_market_data() of
                ok -> {ok, recovered};
                {error, _} -> {error, recovery_failed}
            end;
        connection_down ->
            %% Wait for connection recovery
            timer:sleep(2000),
            case ib_bridge_connector:get_connection_status() of
                {ok, true} -> {ok, recovered};
                _ -> {error, recovery_failed}
            end;
        _ ->
            %% Generic recovery attempt
            timer:sleep(1000),
            {ok, recovered}  % Optimistic recovery
    end.

%% Request fresh market data
request_fresh_market_data() ->
    %% Get configured currency pairs and resubscribe
    CurrencyPairs = config:live_currency_pairs(),
    case CurrencyPairs of
        [] -> {error, no_pairs_configured};
        [FirstPair | _] ->
            Symbol = atom_to_list(FirstPair),
            case ib_bridge_connector:subscribe_market_data(Symbol, 1) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% Validate trade conditions before execution
validate_trade_conditions(TradeSignal, State) ->
    %% Check if we have valid market data
    case get_current_market_price(atom_to_list(State#live_state.table_name)) of
        {ok, _Price} ->
            %% Check if signal is valid
            case is_valid_trade_signal(TradeSignal) of
                true -> {ok, validated};
                false -> {error, validation_failed, invalid_signal}
            end;
        {error, Reason} ->
            {error, validation_failed, {no_market_data, Reason}}
    end.

%% Check if trade signal is valid
is_valid_trade_signal(TradeSignal) ->
    lists:member(TradeSignal, [-1, 0, 1]).

%% Execute trade with retry mechanism
execute_trade_with_retry(TradeSignal, State) ->
    execute_trade_with_retry(TradeSignal, State, 3).  % 3 retry attempts

execute_trade_with_retry(TradeSignal, State, 0) ->
    %% No more retries
    io:format("Trade execution failed after all retries~n"),
    {0, 1, State};  % No fitness, halt flag set

execute_trade_with_retry(TradeSignal, State, RetriesLeft) ->
    try
        %% Attempt normal trade execution
        handle_trade_request(TradeSignal, State)
    catch
        Error:Reason ->
            io:format("Trade execution attempt failed: ~p:~p, retries left: ~p~n", 
                     [Error, Reason, RetriesLeft]),
            
            %% Wait before retry
            timer:sleep(1000),
            
            %% Check if error is retryable
            case is_retryable_trade_error(Error, Reason) of
                true ->
                    execute_trade_with_retry(TradeSignal, State, RetriesLeft - 1);
                false ->
                    io:format("Trade error is not retryable: ~p:~p~n", [Error, Reason]),
                    {0, 1, State}  % No fitness, halt flag set
            end
    end.

%% Determine if trade error is retryable
is_retryable_trade_error(Error, Reason) ->
    case {Error, Reason} of
        {error, timeout} -> true;
        {error, connection_lost} -> true;
        {error, temporary_failure} -> true;
        {throw, insufficient_margin} -> false;  % Not retryable
        {error, invalid_symbol} -> false;       % Not retryable
        _ -> true  % Default to retryable for unknown errors
    end.

%% Emergency close positions in scape
emergency_close_positions_in_scape(State) ->
    CurrentPosition = State#live_state.current_position,
    
    case CurrentPosition of
        0 ->
            %% No position to close
            io:format("No position to close in emergency~n"),
            State;
        Position when Position =/= 0 ->
            io:format("Emergency closing position: ~p~n", [Position]),
            %% Attempt to close position
            try
                {_Fitness, _HaltFlag, ClosedState} = close_position(State),
                ClosedState
            catch
                Error:Reason ->
                    io:format("Failed to close position in emergency: ~p:~p~n", [Error, Reason]),
                    %% Force close in state even if order failed
                    State#live_state{
                        current_position = 0,
                        entry_price = 0,
                        unrealized_pnl = 0
                    }
            end
    end.

%% Clear scape market data
clear_scape_market_data() ->
    io:format("Clearing scape market data~n"),
    %% Clear price buffer
    case ets:info(?LIVE_PRICE_BUFFER) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?LIVE_PRICE_BUFFER)
    end.

%% Generate safe sensor data for error conditions
generate_safe_sensor_data(Parameters) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            %% Return neutral plane data
            lists:duplicate(HRes * VRes, 0);
        [HRes, list_sensor] ->
            %% Return neutral price list
            lists:duplicate(HRes, 0.0);
        _ ->
            %% Default safe data
            [0.0]
    end.

%% Generate fallback sensor data
generate_fallback_sensor_data(Parameters) ->
    %% Use last known good data or safe defaults
    case get_last_known_good_data(Parameters) of
        {ok, Data} -> Data;
        error -> generate_safe_sensor_data(Parameters)
    end.

%% Get last known good data
get_last_known_good_data(_Parameters) ->
    %% For now, return error to use safe defaults
    %% In production, would cache last good data
    error.

%% ============================================================================
%% Error Logging and Notification Functions
%% ============================================================================

%% Log sensor errors
log_sensor_error(Error, Reason, State) ->
    ErrorRecord = {sensor_error, Error, Reason, erlang:timestamp(), State#live_state.table_name},
    io:format("SENSOR ERROR LOGGED: ~p~n", [ErrorRecord]),
    State.

%% Log trade errors
log_trade_error(Error, Reason, State) ->
    ErrorRecord = {trade_error, Error, Reason, erlang:timestamp(), State#live_state.current_position},
    io:format("TRADE ERROR LOGGED: ~p~n", [ErrorRecord]),
    State.

%% Log market data interruption
log_market_data_interruption(Reason) ->
    InterruptionRecord = {market_data_interruption, Reason, erlang:timestamp()},
    io:format("MARKET DATA INTERRUPTION LOGGED: ~p~n", [InterruptionRecord]).

%% Notify trade execution failure
notify_trade_execution_failure(Error, Reason) ->
    io:format("TRADE EXECUTION FAILURE: ~p:~p~n", [Error, Reason]),
    %% Notify live_trader
    case whereis(live_trader) of
        undefined -> ok;
        Pid -> Pid ! {system_error, trade_execution_failure, {Error, Reason}}
    end.

%% Notify market data failure
notify_market_data_failure(Reason) ->
    io:format("MARKET DATA FAILURE: ~p~n", [Reason]),
    %% Notify live_trader
    case whereis(live_trader) of
        undefined -> ok;
        Pid -> Pid ! {system_error, market_data_failure, Reason}
    end.

%% ============================================================================
%% Sensor Request Handling
%% ============================================================================

%% Handle sensor requests for market data
handle_sense_request(TableName, Feature, Parameters, State) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            %% Handle fx_PCI (Price Chart Input) sensor
            handle_pci_sensor(TableName, HRes, VRes, State);
        [HRes, list_sensor] ->
            %% Handle fx_PLI (Price List Input) sensor  
            handle_pli_sensor(TableName, HRes, State);
        _ ->
            io:format("Unknown sensor parameters: ~p~n", [Parameters]),
            {[], State}
    end.

%% Handle fx_PLI sensor - returns list of closing prices
handle_pli_sensor(TableName, HRes, State) ->
    %% Get live price data from IB connector
    PriceList = get_live_price_list(TableName, HRes),
    
    %% Extract closing prices and normalize
    ClosePrices = [Close || {_Open, Close, _High, _Low} <- PriceList],
    NormalizedPrices = normalize_vector(ClosePrices),
    
    %% Update state with price list for caching
    UpdatedState = update_price_list_cache(State, HRes, PriceList),
    
    {NormalizedPrices, UpdatedState}.

%% Handle fx_PCI sensor - returns plane-encoded price data
handle_pci_sensor(TableName, HRes, VRes, State) ->
    %% Get live price data from IB connector
    PriceList = get_live_price_list(TableName, HRes),
    
    %% Calculate vertical range for encoding
    HighPrices = [High || {_Open, _Close, High, _Low} <- PriceList],
    LowPrices = [Low || {_Open, _Close, _High, Low} <- PriceList],
    
    case {HighPrices, LowPrices} of
        {[], []} ->
            %% No data available
            {lists:duplicate(HRes * VRes, -1), State};
        _ ->
            LVMax1 = lists:max(HighPrices),
            LVMin1 = lists:min(LowPrices),
            LVMax = LVMax1 + abs(LVMax1 - LVMin1) / 20,
            LVMin = LVMin1 - abs(LVMax1 - LVMin1) / 20,
            VStep = (LVMax - LVMin) / VRes,
            V_StartPos = LVMin + VStep / 2,
            
            %% Encode price data to plane format
            EncodedData = encode_to_plane(HRes * VRes, PriceList, V_StartPos, VStep, []),
            
            %% Update state with price list for caching
            UpdatedState = update_price_list_cache(State, HRes, PriceList),
            
            {EncodedData, UpdatedState}
    end.

%% Handle internal state sensor requests
handle_internals_request(State) ->
    %% Return current position, entry price, and previous percentage change
    Position = State#live_state.current_position,
    Entry = State#live_state.entry_price,
    PrevPC = State#live_state.previous_pc,
    
    [Position, Entry, PrevPC].

%% ============================================================================
%% Trade Request Handling  
%% ============================================================================

%% Handle trade execution requests with enhanced signal translation
handle_trade_request(TradeSignal, State) ->
    CurrentPosition = State#live_state.current_position,
    
    %% Translate neural network output (-1, 0, 1) to trading actions
    case {CurrentPosition, TradeSignal} of
        {0, 0} ->
            %% No position, no action - continue
            {0, 0, State};
        {0, 1} ->
            %% Open long position (BUY)
            open_position(1, State);
        {0, -1} ->
            %% Open short position (SELL)
            open_position(-1, State);
        {1, 0} ->
            %% Close long position (SELL to close)
            close_position(State);
        {-1, 0} ->
            %% Close short position (BUY to close)
            close_position(State);
        {1, 1} ->
            %% Already long, no change
            {0, 0, State};
        {-1, -1} ->
            %% Already short, no change
            {0, 0, State};
        {1, -1} ->
            %% Switch from long to short
            {_, _, ClosedState} = close_position(State),
            open_position(-1, ClosedState);
        {-1, 1} ->
            %% Switch from short to long
            {_, _, ClosedState} = close_position(State),
            open_position(1, ClosedState);
        _ ->
            %% Invalid signal - no action
            io:format("Invalid trade signal: ~p (current position: ~p)~n", [TradeSignal, CurrentPosition]),
            {0, 0, State}
    end.

%% Open a new trading position with comprehensive risk management
open_position(Signal, State) ->
    %% Get current market price from IB connector
    Symbol = atom_to_list(State#live_state.table_name),
    
    case get_current_market_price(Symbol) of
        {ok, Price} ->
            %% Calculate position size with risk management
            AccountBalance = State#live_state.account_balance,
            RiskParams = get_default_risk_params(),
            
            case calculate_risk_adjusted_position_size(Symbol, AccountBalance, RiskParams, Signal) of
                {ok, Quantity, PositionValue} ->
                    %% Check position limits before placing order
                    case check_position_limits_before_trade(Symbol, PositionValue, AccountBalance) of
                        ok ->
                            %% Check margin requirements
                            case check_margin_requirements_before_trade(Symbol, PositionValue, AccountBalance) of
                                ok ->
                                    %% All checks passed - proceed with order
                                    execute_position_open(Signal, Symbol, Quantity, Price, State);
                                {error, MarginReason} ->
                                    io:format("Margin check failed: ~p~n", [MarginReason]),
                                    {0, 0, State}
                            end;
                        {error, PositionReason} ->
                            io:format("Position limit check failed: ~p~n", [PositionReason]),
                            {0, 0, State}
                    end;
                {error, SizeReason} ->
                    io:format("Position size calculation failed: ~p~n", [SizeReason]),
                    {0, 0, State}
            end;
        {error, _Reason} ->
            io:format("No market data available for ~s~n", [Symbol]),
            {0, 0, State}
    end.

%% Execute the actual position opening after all risk checks
execute_position_open(Signal, Symbol, Quantity, Price, State) ->
    %% Calculate entry price with spread
    Spread = config:account_spread(),
    EntryPrice = Price + Spread * Signal,
    
    %% Translate signal to IB order action
    Action = case Signal of
        1 -> "BUY";   % Long position
        -1 -> "SELL"  % Short position
    end,
    
    io:format("Attempting to open ~s position: ~p shares of ~s at ~p~n", [Action, Quantity, Symbol, EntryPrice]),
    
    case ib_bridge_connector:place_order(Symbol, Action, Quantity, "MKT") of
        ok ->
            %% Wait for order confirmation with timeout
            case wait_for_order_fill(5000) of  % 5 second timeout
                {ok, FillPrice} ->
                    %% Order filled successfully - update state with position tracking
                    UpdatedState = State#live_state{
                        current_position = Signal,
                        entry_price = FillPrice
                    },
                    
                    %% Log successful position opening
                    io:format("Position opened: ~s ~p shares at ~p (expected: ~p)~n", 
                             [Action, Quantity, FillPrice, EntryPrice]),
                    
                    %% Notify live_trader of successful trade for risk tracking
                    notify_trade_execution(Symbol, Action, Quantity, FillPrice),
                    
                    {0, 0, UpdatedState};
                {error, timeout} ->
                    io:format("Order timeout - position may not be opened~n"),
                    {0, 0, State};
                {error, Reason} ->
                    io:format("Order failed: ~p~n", [Reason]),
                    {0, 0, State}
            end;
        {error, Reason} ->
            io:format("Failed to place order: ~p~n", [Reason]),
            {0, 0, State}
    end.

%% Calculate risk-adjusted position size
calculate_risk_adjusted_position_size(Symbol, AccountBalance, RiskParams, Signal) ->
    %% Get base position size from risk parameters
    BasePositionSize = maps:get(position_size, RiskParams, config:live_position_size()),
    MaxPositionPerPair = maps:get(max_position_per_pair, RiskParams, config:live_max_position_per_pair()),
    
    %% Use the more conservative limit
    EffectivePositionSize = min(BasePositionSize, MaxPositionPerPair),
    
    %% Calculate position value
    PositionValue = AccountBalance * EffectivePositionSize,
    
    %% Convert to quantity (simplified - in production would use current price and lot sizes)
    case get_current_market_price(Symbol) of
        {ok, CurrentPrice} ->
            LotSize = config:account_lot_size(),
            Leverage = config:account_leverage(),
            
            %% Calculate quantity with leverage
            Quantity = round((PositionValue * Leverage) / (CurrentPrice * LotSize)),
            
            %% Ensure minimum position size
            FinalQuantity = max(Quantity, 1),
            FinalPositionValue = (FinalQuantity * CurrentPrice * LotSize) / Leverage,
            
            {ok, FinalQuantity, FinalPositionValue};
        {error, Reason} ->
            {error, {price_unavailable, Reason}}
    end.

%% Check position limits before placing trade
check_position_limits_before_trade(Symbol, PositionValue, AccountBalance) ->
    %% Check per-pair position limit
    MaxPositionPerPair = config:live_max_position_per_pair(),
    MaxPositionAmount = AccountBalance * MaxPositionPerPair,
    
    if
        PositionValue > MaxPositionAmount ->
            {error, position_limit_per_pair_exceeded};
        true ->
            %% Check total exposure (simplified - would need current positions in production)
            MaxTotalExposure = config:live_max_total_exposure(),
            MaxTotalAmount = AccountBalance * MaxTotalExposure,
            
            if
                PositionValue > MaxTotalAmount ->
                    {error, total_exposure_limit_exceeded};
                true ->
                    ok
            end
    end.

%% Check margin requirements before placing trade
check_margin_requirements_before_trade(Symbol, PositionValue, AccountBalance) ->
    MarginRequirement = config:live_margin_requirement(),
    RequiredMargin = PositionValue * MarginRequirement,
    
    %% Simple balance-based margin check (in production would query IB for actual margin)
    AvailableMargin = AccountBalance * 0.8, % Conservative estimate
    
    if
        RequiredMargin > AvailableMargin ->
            {error, insufficient_margin};
        true ->
            ok
    end.

%% Get default risk parameters
get_default_risk_params() ->
    #{
        position_size => config:live_position_size(),
        max_position_per_pair => config:live_max_position_per_pair(),
        max_total_exposure => config:live_max_total_exposure(),
        margin_requirement => config:live_margin_requirement()
    }.

%% Notify live_trader of trade execution for risk tracking
notify_trade_execution(Symbol, Action, Quantity, Price) ->
    case whereis(live_trader) of
        undefined -> 
            ok; % No live_trader process running
        Pid ->
            Pid ! {trade_executed, erlang:timestamp(), Symbol, Action, Quantity, Price}
    end.

%% Close current trading position with enhanced risk management
close_position(State) ->
    Symbol = atom_to_list(State#live_state.table_name),
    Position = State#live_state.current_position,
    EntryPrice = State#live_state.entry_price,
    
    case get_current_market_price(Symbol) of
        {ok, CurrentPrice} ->
            %% Calculate P&L and risk metrics
            PriceChange = CurrentPrice - EntryPrice,
            PercentageChange = case EntryPrice of
                0 -> 0;  % Avoid division by zero
                _ -> (PriceChange / EntryPrice) * 100
            end,
            
            %% Calculate position size (should match opening size)
            AccountBalance = State#live_state.account_balance,
            RiskParams = get_default_risk_params(),
            
            case calculate_risk_adjusted_position_size(Symbol, AccountBalance, RiskParams, Position) of
                {ok, Quantity, _PositionValue} ->
                    %% Calculate expected profit
                    Profit = Position * PriceChange * Quantity,
                    
                    %% Check if closing would violate any risk limits
                    case check_close_position_risk(State, Profit) of
                        ok ->
                            execute_position_close(Position, Symbol, Quantity, CurrentPrice, EntryPrice, State);
                        {warning, Reason} ->
                            io:format("Risk warning on close: ~p, proceeding anyway~n", [Reason]),
                            execute_position_close(Position, Symbol, Quantity, CurrentPrice, EntryPrice, State);
                        {error, Reason} ->
                            io:format("Cannot close position due to risk constraint: ~p~n", [Reason]),
                            {0, 0, State}
                    end;
                {error, Reason} ->
                    io:format("Failed to calculate position size for close: ~p~n", [Reason]),
                    %% Use fallback calculation
                    FallbackQuantity = calculate_position_size(AccountBalance),
                    execute_position_close(Position, Symbol, FallbackQuantity, CurrentPrice, EntryPrice, State)
            end;
        {error, _Reason} ->
            io:format("No market data available for closing ~s~n", [Symbol]),
            {0, 0, State}
    end.

%% Execute the actual position closing
execute_position_close(Position, Symbol, Quantity, CurrentPrice, EntryPrice, State) ->
    %% Calculate expected P&L
    PriceChange = CurrentPrice - EntryPrice,
    PercentageChange = case EntryPrice of
        0 -> 0;
        _ -> (PriceChange / EntryPrice) * 100
    end,
    
    %% Translate position to closing action
    Action = case Position of
        1 -> "SELL";  % Close long position (sell to close)
        -1 -> "BUY"   % Close short position (buy to close)
    end,
    
    io:format("Attempting to close position: ~s ~p shares of ~s (P&L estimate: ~p)~n", 
             [Action, Quantity, Symbol, Position * PriceChange * Quantity]),
    
    case ib_bridge_connector:place_order(Symbol, Action, Quantity, "MKT") of
        ok ->
            %% Wait for order confirmation with timeout
            case wait_for_order_fill(5000) of  % 5 second timeout
                {ok, FillPrice} ->
                    %% Order filled successfully - calculate actual P&L
                    ActualPriceChange = case Position of
                        1 -> FillPrice - EntryPrice;  % Long: sell price - buy price
                        -1 -> EntryPrice - FillPrice   % Short: sell price - buy price (reversed)
                    end,
                    
                    ActualProfit = ActualPriceChange * Quantity,
                    NewBalance = State#live_state.account_balance + ActualProfit,
                    NewRealizedPnL = State#live_state.realized_pnl + ActualProfit,
                    
                    UpdatedState = State#live_state{
                        current_position = 0,
                        entry_price = 0,
                        previous_pc = PercentageChange,
                        account_balance = NewBalance,
                        realized_pnl = NewRealizedPnL,
                        unrealized_pnl = 0
                    },
                    
                    io:format("Position closed: ~s ~p shares at ~p, P&L: ~p, New Balance: ~p~n", 
                             [Action, Quantity, FillPrice, ActualProfit, NewBalance]),
                    
                    %% Notify live_trader of trade execution
                    notify_trade_execution(Symbol, Action, Quantity, FillPrice),
                    
                    %% Enhanced halt conditions based on risk management
                    HaltFlag = determine_halt_flag(NewBalance, ActualProfit, State),
                    
                    {ActualProfit, HaltFlag, UpdatedState};
                {error, timeout} ->
                    io:format("Close order timeout - position may still be open~n"),
                    {0, 0, State};
                {error, Reason} ->
                    io:format("Close order failed: ~p~n", [Reason]),
                    {0, 0, State}
            end;
        {error, Reason} ->
            io:format("Failed to place close order: ~p~n", [Reason]),
            {0, 0, State}
    end.

%% Check risk constraints when closing position
check_close_position_risk(State, ExpectedProfit) ->
    AccountBalance = State#live_state.account_balance,
    MinBalance = config:live_min_account_balance(),
    
    %% Check if closing would bring balance below minimum
    NewBalance = AccountBalance + ExpectedProfit,
    
    if
        NewBalance < MinBalance ->
            {warning, balance_below_minimum_after_close};
        NewBalance < MinBalance * 0.5 ->
            {error, balance_critically_low_after_close};
        true ->
            ok
    end.

%% Determine halt flag based on enhanced risk criteria
determine_halt_flag(NewBalance, Profit, State) ->
    MinBalance = config:live_min_account_balance(),
    MaxDailyLoss = config:live_max_daily_loss(),
    StartBalance = State#live_state.account_balance - State#live_state.realized_pnl,
    
    %% Calculate daily loss percentage
    DailyLoss = StartBalance - NewBalance,
    DailyLossPercent = case StartBalance of
        0 -> 0;
        _ -> DailyLoss / StartBalance
    end,
    
    %% Multiple halt conditions
    HaltConditions = [
        NewBalance =< MinBalance,                    % Balance too low
        DailyLossPercent >= MaxDailyLoss,           % Daily loss limit exceeded
        Profit < -MinBalance * 0.1                  % Single trade loss too large
    ],
    
    case lists:any(fun(Condition) -> Condition end, HaltConditions) of
        true -> 
            io:format("Halt conditions met: Balance=~p, DailyLoss%=~p, TradeLoss=~p~n", 
                     [NewBalance, DailyLossPercent * 100, Profit]),
            1;
        false -> 
            0
    end.

%% ============================================================================
%% Live Data Management
%% ============================================================================

%% Get live price data from IB connector and format for sensors
get_live_price_list(TableName, HRes) ->
    Symbol = atom_to_list(TableName),
    
    %% Try to get OHLC data from IB connector
    case ib_bridge_connector:get_ohlc_data(Symbol, 60) of  % 1-minute resolution
        {ok, OHLCList} when length(OHLCList) >= HRes ->
            %% Convert OHLC records to tuple format expected by sensors
            RecentData = lists:sublist(OHLCList, HRes),
            [{OHLC#live_ohlc.open, OHLC#live_ohlc.close, 
              OHLC#live_ohlc.high, OHLC#live_ohlc.low} || OHLC <- RecentData];
        {ok, OHLCList} ->
            %% Not enough data - pad with last known values
            case OHLCList of
                [] ->
                    %% No data at all - return dummy data
                    lists:duplicate(HRes, {1.0, 1.0, 1.0, 1.0});
                [LastOHLC | _] ->
                    %% Pad with last known values
                    LastTuple = {LastOHLC#live_ohlc.open, LastOHLC#live_ohlc.close,
                                LastOHLC#live_ohlc.high, LastOHLC#live_ohlc.low},
                    ExistingTuples = [{OHLC#live_ohlc.open, OHLC#live_ohlc.close,
                                     OHLC#live_ohlc.high, OHLC#live_ohlc.low} || OHLC <- OHLCList],
                    PaddingNeeded = HRes - length(ExistingTuples),
                    lists:duplicate(PaddingNeeded, LastTuple) ++ ExistingTuples
            end;
        {error, _Reason} ->
            %% Fallback to current market tick if OHLC not available
            case ib_bridge_connector:get_market_data(Symbol) of
                {ok, Tick} ->
                    %% Use current tick data for all points
                    Price = case Tick#market_tick.last of
                        undefined -> 
                            case Tick#market_tick.bid of
                                undefined -> 1.0;  % Fallback price
                                Bid -> Bid
                            end;
                        Last -> Last
                    end,
                    lists:duplicate(HRes, {Price, Price, Price, Price});
                {error, _} ->
                    %% No data available - return dummy data
                    lists:duplicate(HRes, {1.0, 1.0, 1.0, 1.0})
            end
    end.

%% Get current market price for trading
get_current_market_price(Symbol) ->
    case ib_bridge_connector:get_market_data(Symbol) of
        {ok, Tick} ->
            %% Use last price if available, otherwise use bid
            Price = case Tick#market_tick.last of
                undefined ->
                    case Tick#market_tick.bid of
                        undefined -> {error, no_price_data};
                        Bid -> {ok, Bid}
                    end;
                Last -> {ok, Last}
            end,
            Price;
        {error, Reason} ->
            {error, Reason}
    end.

%% ============================================================================
%% Data Processing Utilities
%% ============================================================================

%% Normalize vector (same as in sensor.erl)
normalize_vector([]) -> [];
normalize_vector(Vector) ->
    Normalizer = math:sqrt(lists:sum([Val * Val || Val <- Vector])),
    case Normalizer of
        0.0 -> Vector;  % Avoid division by zero
        _ -> [Val / Normalizer || Val <- Vector]
    end.

%% Encode price data to plane format (similar to fx.erl)
encode_to_plane(0, _, _, _, Acc) ->
    lists:reverse(Acc);
encode_to_plane(Index, [{Open, Close, High, Low} | PriceList], VPos, VStep, Acc) ->
    %% Determine body range
    {BHigh, BLow} = case Open > Close of
        true -> {Open, Close};
        false -> {Close, Open}
    end,
    
    %% Encode current position
    Value = if
        (VPos + VStep/2 > BLow) andalso (VPos - VStep/2 =< BHigh) -> 1;  % Body
        (VPos + VStep/2 > Low) andalso (VPos - VStep/2 =< High) -> 0;    % Wick
        true -> -1  % Background
    end,
    
    encode_to_plane(Index - 1, PriceList, VPos, VStep, [Value | Acc]);
encode_to_plane(Index, [], VPos, VStep, Acc) when Index > 0 ->
    %% No more price data - fill with background
    encode_to_plane(Index - 1, [], VPos + VStep, VStep, [-1 | Acc]);
encode_to_plane(0, [], _, _, Acc) ->
    lists:reverse(Acc).

%% Update price list cache in state
update_price_list_cache(State, HRes, PriceList) ->
    PriceListPs = State#live_state.price_list,
    UpdatedPriceListPs = lists:keystore(HRes, 2, PriceListPs, {PriceList, HRes}),
    State#live_state{price_list = UpdatedPriceListPs}.

%% Calculate position size based on account balance (legacy function for compatibility)
calculate_position_size(Balance) ->
    %% Use risk-managed position sizing
    RiskParams = get_default_risk_params(),
    PositionPercent = maps:get(position_size, RiskParams, config:live_position_size()),
    MaxPositionPerPair = maps:get(max_position_per_pair, RiskParams, config:live_max_position_per_pair()),
    
    %% Use the more conservative limit
    EffectivePositionSize = min(PositionPercent, MaxPositionPerPair),
    BuyMoney = Balance * EffectivePositionSize,
    Leverage = config:account_leverage(),
    
    %% Calculate with risk constraints
    BaseQuantity = round(BuyMoney * Leverage / 1000),
    
    %% Ensure minimum viable position
    max(BaseQuantity, 1).

%% ============================================================================
%% Direct Trade Interface (for compatibility)
%% ============================================================================

%% Direct trade/3 function interface for fx_Trade actuator compatibility
trade(TableName, TradeSignal, State) ->
    handle_trade_request(TradeSignal, State).

%% ============================================================================
%% ETS Buffer Management
%% ============================================================================

%% Initialize price buffer ETS table
init_price_buffer() ->
    case ets:info(?LIVE_PRICE_BUFFER) of
        undefined ->
            ets:new(?LIVE_PRICE_BUFFER, [ordered_set, public, named_table]),
            io:format("Live price buffer initialized~n");
        _ ->
            ets:delete_all_objects(?LIVE_PRICE_BUFFER),
            io:format("Live price buffer cleared~n")
    end.

%% Clean up price buffer
cleanup_price_buffer() ->
    case ets:info(?LIVE_PRICE_BUFFER) of
        undefined -> ok;
        _ -> 
            ets:delete(?LIVE_PRICE_BUFFER),
            io:format("Live price buffer cleaned up~n")
    end.

%% Add price data to buffer (for future use)
add_to_buffer(Symbol, Timestamp, OHLC) ->
    Key = {Symbol, Timestamp},
    ets:insert(?LIVE_PRICE_BUFFER, {Key, OHLC}),
    
    %% Maintain buffer size limit
    case ets:info(?LIVE_PRICE_BUFFER, size) of
        Size when Size > ?MAX_BUFFER_SIZE ->
            %% Remove oldest entries
            FirstKey = ets:first(?LIVE_PRICE_BUFFER),
            ets:delete(?LIVE_PRICE_BUFFER, FirstKey);
        _ ->
            ok
    end.

%% Wait for order fill confirmation with timeout
wait_for_order_fill(TimeoutMs) ->
    receive
        {execution_data, {OrderId, Symbol, Side, Shares, Price, Time}} ->
            io:format("Order filled: ~p shares of ~s at ~p~n", [Shares, Symbol, Price]),
            {ok, Price};
        {market_data, _Tick} ->
            %% Ignore market data updates while waiting for fill
            wait_for_order_fill(TimeoutMs)
    after TimeoutMs ->
        {error, timeout}
    end.