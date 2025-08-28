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

%% Live ETS table definitions
-define(LIVE_TABLES, [live_EURUSD1]).
-define(HISTORICAL_TABLES, ['EURUSD1']).

%% Technical record definition (matching fx.erl)
-record(technical,{
    id,    %%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open,
    high,
    low,
    close,
    volume}).

%% State record definition (matching fx.erl for compatibility)
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).

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
    
    %% Initialize live tables if live trading is enabled
    case config:live_trading_enabled() of
        true ->
            init_live_tables(),
            start_live_data_feeder();
        false ->
            ok
    end,
    
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
            %% Handle sensor requests for market data with error handling (silent processing)
            
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
            %% Enhanced trading decision output
            DecisionStr = case TradeSignal of
                1 -> "BUY";
                -1 -> "SELL"; 
                0 -> "HOLD";
                _ -> "INVALID"
            end,
            
            %% Get current market data if available
            CurrentPrice = case State#live_state.price_list of
                [LatestPrice | _] -> LatestPrice;
                [] -> 0.0
            end,
            
            PrevPC = State#live_state.previous_pc,
            Position = case State#live_state.current_position of
                1 -> "LONG";
                -1 -> "SHORT";
                0 -> "FLAT"
            end,
            
            io:format("🤖 TRADING DECISION: ~s | Signal: ~.4f | Price: ~.5f | Trend: ~.4f% | Position: ~s~n", 
                     [DecisionStr, TradeSignal, CurrentPrice, PrevPC * 100, Position]),
            
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
    case get_current_market_price(get_currency_pair_from_table_name(State#live_state.table_name)) of
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
%% Sensor Interface Functions (delegated from fx.erl)
%% ============================================================================

%% Initialize state for live data requests (called from fx.erl)
init_state(S, TableName, Feature, live_data, live_data) ->
    %% Only log initialization once per table to avoid spam
    case get({initialized, TableName}) of
        true -> ok;
        _ -> 
            io:format("Initializing live data state for ~p~n", [TableName]),
            put({initialized, TableName}, true)
    end,
    
    %% Ensure live tables are initialized (only once)
    case get(live_tables_initialized) of
        true -> ok;
        _ ->
            case config:live_trading_enabled() of
                true ->
                    init_live_tables(),
                    put(live_tables_initialized, true);
                false ->
                    ok
            end
    end,
    
    %% Get live table name
    LiveTableName = get_live_table_name(TableName),
    
    %% Ensure live table has data
    case ensure_live_table_with_data(LiveTableName, TableName) of
        {ok, {Index_Start, Index_End}} ->
            %% Return fx.erl compatible state record with live data range
            S#state{
                table_name = LiveTableName,
                feature = Feature,
                index_start = Index_Start,
                index_end = Index_End,
                index = Index_Start,
                price_list = []
            };
        {error, Reason} ->
            io:format("Live data initialization failed: ~p, using fallback~n", [Reason]),
            %% Fallback to a reasonable range using fx.erl state format
            S#state{
                table_name = TableName,
                feature = Feature,
                index_start = 1,
                index_end = 100,
                index = 1,
                price_list = []
            }
    end.

%% Sense function for live data (delegated from fx.erl)
sense(S, Parameters) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            handle_pci_sensor(S#state.table_name, HRes, VRes, S);
        [HRes, list_sensor] ->
            handle_pli_sensor(S#state.table_name, HRes, S);
        _ ->
            io:format("Unknown sensor parameters: ~p~n", [Parameters]),
            {[], S}
    end.

%% Lookup function for live data (delegated from fx.erl)
lookup(TableName, Index) ->
    case is_live_table(TableName) of
        true ->
            lookup_live_with_pull(TableName, Index);
        false ->
            %% Delegate to fx.erl for historical data
            fx:lookup(TableName, Index)
    end.

%% Next function for live data (delegated from fx.erl)
next(TableName, Index) ->
    case is_live_table(TableName) of
        true ->
            next_live(TableName, Index);
        false ->
            %% Delegate to fx.erl for historical data
            fx:next(TableName, Index)
    end.

%% Previous function for live data (delegated from fx.erl)
prev(TableName, CurrentIndex, Direction, Count) ->
    case is_live_table(TableName) of
        true ->
            %% Implement live table prev navigation
            prev_live(TableName, CurrentIndex, Direction, Count);
        false ->
            %% Delegate to fx.erl for historical data
            fx:prev(TableName, CurrentIndex, Direction, Count)
    end.

%% Previous navigation for live tables
prev_live(TableName, CurrentIndex, prev, 0) ->
    CurrentIndex;
prev_live(TableName, CurrentIndex, prev, Count) when Count > 0 ->
    case ets:prev(TableName, CurrentIndex) of
        '$end_of_table' ->
            CurrentIndex;
        PrevIndex ->
            prev_live(TableName, PrevIndex, prev, Count - 1)
    end;
prev_live(TableName, CurrentIndex, next, Count) when Count > 0 ->
    case ets:next(TableName, CurrentIndex) of
        '$end_of_table' ->
            CurrentIndex;
        NextIndex ->
            prev_live(TableName, NextIndex, next, Count - 1)
    end.

%% ============================================================================
%% Sensor Request Handling
%% ============================================================================

%% Handle sensor requests for market data
handle_sense_request(TableName, Feature, Parameters, State) ->
    case is_live_table_request(TableName) of
        true ->
            %% Handle live data request
            handle_live_sense_request(TableName, Feature, Parameters, State);
        false ->
            %% Handle historical data request (existing logic)
            handle_historical_sense_request(TableName, Feature, Parameters, State)
    end.

%% Handle live data sensor requests
handle_live_sense_request(TableName, Feature, Parameters, State) ->
    LiveTableName = get_live_table_name(TableName),
    
    %% Ensure live table has data
    case ensure_live_table_with_data(LiveTableName, TableName) of
        {ok, _DataRange} ->
            %% Use live table with same logic as historical
            handle_historical_sense_request(LiveTableName, Feature, Parameters, State);
        {error, Reason} ->
            io:format("Live data not available: ~p, waiting for data...~n", [Reason]),
            %% Wait for live data instead of falling back to historical
            timer:sleep(1000),  % Wait 1 second
            handle_live_sense_request(TableName, Feature, Parameters, State)
    end.

%% Handle historical data sensor requests (existing logic)
handle_historical_sense_request(TableName, Feature, Parameters, State) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            handle_pci_sensor(TableName, HRes, VRes, State);
        [HRes, list_sensor] ->
            handle_pli_sensor(TableName, HRes, State);
        _ ->
            io:format("Unknown sensor parameters: ~p~n", [Parameters]),
            {[], State}
    end.

%% Handle fx_PLI sensor - returns list of closing prices
handle_pli_sensor(TableName, HRes, State) ->
    %% Get live price data using fx.erl compatible approach
    Index = State#state.index,
    PriceListPs = State#state.price_list,
    
    case lists:keyfind(HRes, 2, PriceListPs) of
        false ->
            %% Get trailing index and build price list
            Trailing_Index = prev_live(TableName, Index, prev, HRes-1),
            U_PList = get_live_price_list_fx_format(TableName, Trailing_Index, HRes, []),
            U_PriceListPs = [{U_PList, HRes} | PriceListPs];
        {PList, HRes} ->
            %% Update with current data
            case lookup(TableName, Index) of
                R when is_record(R, technical) ->
                    U_PList = [{R#technical.open, R#technical.close, R#technical.high, R#technical.low} | lists:sublist(PList, HRes-1)],
                    U_PriceListPs = lists:keyreplace(HRes, 2, PriceListPs, {U_PList, HRes});
                _ ->
                    %% Use existing list if lookup fails
                    U_PList = PList,
                    U_PriceListPs = PriceListPs
            end
    end,
    
    %% Extract closing prices
    ClosePrices = [Close || {_Open, Close, _High, _Low} <- U_PList],
    
    %% Update state with price list for caching
    UpdatedState = State#state{price_list = U_PriceListPs},
    
    {ClosePrices, UpdatedState}.

%% Handle fx_PCI sensor - returns plane-encoded price data
handle_pci_sensor(TableName, HRes, VRes, State) ->
    %% Get live price data using fx.erl compatible approach
    Index = State#state.index,
    PriceListPs = State#state.price_list,
    
    case lists:keyfind(HRes, 2, PriceListPs) of
        false ->
            %% Get trailing index and build price list
            Trailing_Index = prev_live(TableName, Index, prev, HRes-1),
            U_PList = get_live_price_list_fx_format(TableName, Trailing_Index, HRes, []),
            U_PriceListPs = [{U_PList, HRes} | PriceListPs];
        {PList, HRes} ->
            %% Update with current data
            case lookup(TableName, Index) of
                R when is_record(R, technical) ->
                    U_PList = [{R#technical.open, R#technical.close, R#technical.high, R#technical.low} | lists:sublist(PList, HRes-1)],
                    U_PriceListPs = lists:keyreplace(HRes, 2, PriceListPs, {U_PList, HRes});
                _ ->
                    %% Use existing list if lookup fails
                    U_PList = PList,
                    U_PriceListPs = PriceListPs
            end
    end,
    
    %% Calculate vertical range for encoding
    case U_PList of
        [] ->
            %% No data available
            {lists:duplicate(HRes * VRes, -1), State};
        _ ->
            HighPrices = [High || {_Open, _Close, High, _Low} <- U_PList],
            LowPrices = [Low || {_Open, _Close, _High, Low} <- U_PList],
            
            LVMax1 = lists:max(HighPrices),
            LVMin1 = lists:min(LowPrices),
            LVMax = LVMax1 + abs(LVMax1 - LVMin1) / 20,
            LVMin = LVMin1 - abs(LVMax1 - LVMin1) / 20,
            VStep = (LVMax - LVMin) / VRes,
            V_StartPos = LVMin + VStep / 2,
            
            %% Encode price data to plane format (using fx.erl compatible function)
            EncodedData = encode_to_plane_fx_format(HRes * VRes, {U_PList, U_PList}, V_StartPos, VStep, []),
            
            %% Update state with price list for caching
            UpdatedState = State#state{price_list = U_PriceListPs},
            
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
    Symbol = get_currency_pair_from_table_name(State#live_state.table_name),
    
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
    case get_current_market_price(get_currency_pair_from_table_name(list_to_atom(Symbol))) of
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
    
    case get_current_market_price(get_currency_pair_from_table_name(State#live_state.table_name)) of
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
    Symbol = get_currency_pair_from_table_name(TableName),
    
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
                    %% No OHLC data - try to get current market price
                    case ib_bridge_connector:get_market_data(Symbol) of
                        {ok, Tick} ->
                            Price = case Tick#market_tick.last of
                                undefined -> 
                                    case Tick#market_tick.bid of
                                        undefined -> Tick#market_tick.ask;
                                        Bid -> Bid
                                    end;
                                Last -> Last
                            end,
                            lists:duplicate(HRes, {Price, Price, Price, Price});
                        {error, _} ->
                            io:format("ERROR: No market data available for ~p~n", [Symbol]),
                            exit({no_market_data, Symbol})
                    end;
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
                                undefined -> 
                                    case Tick#market_tick.ask of
                                        undefined -> 
                                            io:format("ERROR: No price data available from IB for ~p~n", [Symbol]),
                                            exit({no_market_data, Symbol});
                                        Ask -> Ask
                                    end;
                                Bid -> Bid
                            end;
                        Last -> Last
                    end,
                    lists:duplicate(HRes, {Price, Price, Price, Price});
                {error, no_data_available} ->
                    %% Try to subscribe and wait briefly for data
                    io:format("No market data for ~p, attempting subscription...~n", [Symbol]),
                    ib_bridge_connector:subscribe_market_data(Symbol),
                    timer:sleep(1000),  % Wait 1 second for data
                    case ib_bridge_connector:get_market_data(Symbol) of
                        {ok, Tick} ->
                            Price = case Tick#market_tick.last of
                                undefined -> 
                                    case Tick#market_tick.bid of
                                        undefined -> Tick#market_tick.ask;
                                        Bid -> Bid
                                    end;
                                Last -> Last
                            end,
                            lists:duplicate(HRes, {Price, Price, Price, Price});
                        {error, _} ->
                            io:format("Still waiting for market data for ~p, retrying...~n", [Symbol]),
                            timer:sleep(2000),  % Wait 2 seconds
                            get_live_price_list(TableName, HRes)  % Retry the whole function
                    end;
                {error, Other} ->
                    io:format("IB Bridge error for ~p: ~p, retrying...~n", [Symbol, Other]),
                    timer:sleep(2000),  % Wait 2 seconds
                    get_live_price_list(TableName, HRes)  % Retry the whole function
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
%% FX.erl Compatible Helper Functions
%% ============================================================================

%% Get live price list in fx.erl format
get_live_price_list_fx_format(_Table, EndKey, 0, Acc) ->
    Acc;
get_live_price_list_fx_format(_Table, '$end_of_table', _Index, Acc) ->
    Acc;
get_live_price_list_fx_format(Table, Key, Index, Acc) ->
    case lookup(Table, Key) of
        R when is_record(R, technical) ->
            PriceTuple = {R#technical.open, R#technical.close, R#technical.high, R#technical.low},
            get_live_price_list_fx_format(Table, next(Table, Key), Index-1, [PriceTuple | Acc]);
        _ ->
            %% If lookup fails, try to get current market data
            case ib_bridge_connector:get_market_data('EURUSD') of
                {ok, Tick} ->
                    Price = case Tick#market_tick.last of
                        undefined -> 
                            case Tick#market_tick.bid of
                                undefined -> Tick#market_tick.ask;
                                Bid -> Bid
                            end;
                        Last -> Last
                    end,
                    RealTuple = {Price, Price, Price, Price},
                    get_live_price_list_fx_format(Table, next(Table, Key), Index-1, [RealTuple | Acc]);
                {error, _} ->
                    %% Last resort - skip this data point
                    get_live_price_list_fx_format(Table, next(Table, Key), Index-1, Acc)
            end
    end.

%% Encode to plane format (fx.erl compatible)
encode_to_plane_fx_format(Index, {[{Open, Close, High, Low} | VList], MemList}, VPos, VStep, Acc) ->
    {BHigh, BLow} = case Open > Close of
        true -> {Open, Close};
        false -> {Close, Open}
    end,
    
    O = case (VPos + VStep/2 > BLow) and (VPos - VStep/2 =< BHigh) of
        true -> 1;
        false ->
            case (VPos + VStep/2 > Low) and (VPos - VStep/2 =< High) of
                true -> 0;
                false -> -1
            end
    end,
    
    encode_to_plane_fx_format(Index-1, {VList, MemList}, VPos, VStep, [O | Acc]);
encode_to_plane_fx_format(0, {[], _MemList}, _VPos, _VStep, Acc) ->
    Acc;
encode_to_plane_fx_format(Index, {[], MemList}, VPos, VStep, Acc) ->
    encode_to_plane_fx_format(Index, {MemList, MemList}, VPos + VStep, VStep, Acc).

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

%% Clean up price buffer and live tables
cleanup_price_buffer() ->
    io:format("Clearing scape market data~n"),
    %% Clear price buffer
    case ets:info(?LIVE_PRICE_BUFFER) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?LIVE_PRICE_BUFFER)
    end,
    
    %% Clean up live tables
    case config:live_trading_enabled() of
        true ->
            [cleanup_live_table(TableName) || TableName <- ?LIVE_TABLES];
        false ->
            ok
    end.

%% Clean up individual live table
cleanup_live_table(TableName) ->
    case ets:info(TableName) of
        undefined -> ok;
        _ -> 
            ets:delete(TableName),
            io:format("Cleaned up live table: ~p~n", [TableName])
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

%% ============================================================================
%% Live Table Management
%% ============================================================================

%% Initialize live trading tables
%% Always recreate live tables (delete if present, then new)
init_live_tables() ->
    io:format("Initializing live FX tables: ~p~n", [?LIVE_TABLES]),
    lists:foreach(
      fun(TableName) ->
          case ets:info(TableName) of
              undefined -> ok;
              _ -> ets:delete(TableName)
          end,
          ets:new(TableName, [ordered_set, public, named_table, {keypos, 2}])
      end,
      ?LIVE_TABLES
    ),
    io:format("Live FX tables initialized~n"),
    ok.


init_live_table(TableName) ->
    ets:new(TableName, [ordered_set, public, named_table, {keypos, 2}]).

%% Get live table name from historical table name (accepts atom or string)
get_live_table_name(Name) when is_atom(Name) ->
    list_to_atom("live_" ++ atom_to_list(Name));
get_live_table_name(Name) when is_list(Name) ->
    "live_" ++ Name.

%% Convert IB currency pair format to historical table format
convert_ib_pair_to_historical_table('EUR.USD') -> 'EURUSD1';
convert_ib_pair_to_historical_table("EUR.USD") -> 'EURUSD1';
convert_ib_pair_to_historical_table('EURUSD') -> 'EURUSD1';
convert_ib_pair_to_historical_table("EURUSD") -> 'EURUSD1';
convert_ib_pair_to_historical_table(Other) -> Other.  % Pass through if no conversion needed

%% Check if table is a live table
is_live_table(TableName) ->
    lists:member(TableName, ?LIVE_TABLES).

%% Check if this is a live data request
is_live_table_request(TableName) ->
    %% Check if the request is for live_data or if we're in live trading mode
    case config:live_trading_enabled() of
        true ->
            %% In live mode, check if this table has a live equivalent
            lists:member(TableName, ?HISTORICAL_TABLES) orelse
            lists:member(TableName, ?LIVE_TABLES);
        false ->
            false
    end.

%% Get historical table name from live table name
get_historical_table_name(LiveTableName) ->
    TableNameStr = atom_to_list(LiveTableName),
    case string:prefix(TableNameStr, "live_") of
        nomatch -> undefined;
        HistoricalName -> list_to_atom(HistoricalName)
    end.

%% ============================================================================
%% Live Data Conversion Functions
%% ============================================================================

%% Convert IB OHLC data to technical record format
convert_ohlc_to_technical(OHLC) ->
    #technical{
        id = OHLC#live_ohlc.timestamp,  % Use timestamp directly (already includes sampling rate)
        open = OHLC#live_ohlc.open,
        high = OHLC#live_ohlc.high,
        low = OHLC#live_ohlc.low,
        close = OHLC#live_ohlc.close,
        volume = OHLC#live_ohlc.volume
    }.

%% Convert timestamp to technical record ID format
timestamp_to_id(Timestamp) ->
    {Year, Month, Day} = date(),
    {Hour, Minute, Second} = time(),
    {Year, Month, Day, Hour, Minute, Second, 60}.  % 60-second sampling rate

%% ============================================================================
%% Live Table Data Pulling Functions
%% ============================================================================

%% Ensure live table exists and has data, pulling from IB if needed
ensure_live_table_with_data(LiveTableName, HistoricalTableName) ->
    %% Create live table if it doesn't exist
    case ets:info(LiveTableName) of
        undefined ->
            init_live_table(LiveTableName);
        _ ->
            ok
    end,
    
    %% Check if we have recent data, if not pull from IB
    case has_recent_data(LiveTableName) of
        true ->
            %% Use existing data
            Index_End = ets:last(LiveTableName),
            case Index_End of
                '$end_of_table' ->
                    {error, no_live_data};
                _ ->
                    Index_Start = find_start_index(LiveTableName, Index_End, 99),
                    {ok, {Index_Start, Index_End}}
            end;
        false ->
            %% Pull fresh data from IB Bridge
            pull_live_data_from_ib(LiveTableName, HistoricalTableName)
    end.

%% Check if live table has recent data (within last 5 minutes)
has_recent_data(LiveTableName) ->
    case ets:last(LiveTableName) of
        '$end_of_table' ->
            false;
        LastIndex ->
            {Year, Month, Day, Hour, Minute, Second, _} = LastIndex,
            {CurrentYear, CurrentMonth, CurrentDay} = date(),
            {CurrentHour, CurrentMinute, CurrentSecond} = time(),
            
            LastTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
            CurrentTime = calendar:datetime_to_gregorian_seconds({{CurrentYear, CurrentMonth, CurrentDay}, {CurrentHour, CurrentMinute, CurrentSecond}}),
            
            %% Data is recent if it's within 5 minutes
            (CurrentTime - LastTime) < 300
    end.

%% Pull live data from IB Bridge
pull_live_data_from_ib(LiveTableName, HistoricalTableName) ->
    %% Get currency pair from table name
    CurrencyPair = get_currency_pair_from_table_name(LiveTableName),
    
    case ib_bridge_connector:get_ohlc_data(CurrencyPair, 60) of  % 1-minute bars
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(LiveTableName, TechnicalRecord)
            end, OHLCList),
            
            %% Return data range
            Index_End = ets:last(LiveTableName),
            case Index_End of
                '$end_of_table' ->
                    {error, no_live_data};
                _ ->
                    Index_Start = find_start_index(LiveTableName, Index_End, 99),
                    {ok, {Index_Start, Index_End}}
            end;
        {ok, []} ->
            %% No live data available, wait and retry
            io:format("No live data available for ~p, waiting...~n", [CurrencyPair]),
            timer:sleep(2000),  % Wait 2 seconds
            pull_live_data_from_ib(LiveTableName, HistoricalTableName);
        {error, Reason} ->
            %% IB error, wait and retry
            io:format("IB Bridge error for ~p: ~p, retrying...~n", [CurrencyPair, Reason]),
            timer:sleep(2000),  % Wait 2 seconds
            pull_live_data_from_ib(LiveTableName, HistoricalTableName)
    end.

%% Get currency pair string from live table name
get_currency_pair_from_table_name(LiveTableName) ->
    TableNameStr = atom_to_list(LiveTableName),
    case string:prefix(TableNameStr, "live_") of
        nomatch -> 
            %% Convert historical table names to IB format
            convert_to_ib_symbol(TableNameStr);
        Remainder -> 
            %% Convert live table names to IB format
            convert_to_ib_symbol(Remainder)
    end.

%% Convert internal table names to IB symbol format
convert_to_ib_symbol("EURUSD" ++ _) -> "EUR.USD";
convert_to_ib_symbol("GBPUSD" ++ _) -> "GBP.USD";
convert_to_ib_symbol("USDJPY" ++ _) -> "USD.JPY";
convert_to_ib_symbol(Other) -> Other.

%% Fallback to historical data when live data is not available
fallback_to_historical_data(LiveTableName, HistoricalTableName) ->
    case ets:info(HistoricalTableName) of
        undefined ->
            {error, no_historical_data};
        _ ->
            %% Get the last 100 data points from historical table
            LastIndex = ets:last(HistoricalTableName),
            case LastIndex of
                '$end_of_table' ->
                    {error, no_historical_data};
                _ ->
                    %% Navigate backwards to find start index (get last 100 records)
                    StartIndex = find_start_index(HistoricalTableName, LastIndex, 99),
                    copy_historical_to_live(HistoricalTableName, LiveTableName, StartIndex, LastIndex),
                    {ok, {StartIndex, LastIndex}}
            end
    end.

%% Find start index by navigating backwards N steps
find_start_index(TableName, CurrentIndex, 0) ->
    CurrentIndex;
find_start_index(TableName, CurrentIndex, StepsRemaining) ->
    case ets:prev(TableName, CurrentIndex) of
        '$end_of_table' ->
            CurrentIndex;  % Reached beginning of table
        PrevIndex ->
            find_start_index(TableName, PrevIndex, StepsRemaining - 1)
    end.

%% Copy data from historical table to live table
copy_historical_to_live(HistoricalTable, LiveTable, StartIndex, EndIndex) ->
    copy_historical_range(HistoricalTable, LiveTable, StartIndex, EndIndex).

%% Copy records from StartIndex to EndIndex using ETS navigation
copy_historical_range(HistoricalTable, LiveTable, CurrentIndex, EndIndex) ->
    case ets:lookup(HistoricalTable, CurrentIndex) of
        [Record] ->
            ets:insert(LiveTable, Record),
            case CurrentIndex of
                EndIndex ->
                    ok;  % Reached end index
                _ ->
                    %% Get next index and continue
                    case ets:next(HistoricalTable, CurrentIndex) of
                        '$end_of_table' ->
                            ok;  % Reached end of table
                        NextIndex ->
                            copy_historical_range(HistoricalTable, LiveTable, NextIndex, EndIndex)
                    end
            end;
        [] ->
            %% Record not found, try next index
            case CurrentIndex of
                EndIndex ->
                    ok;  % Reached end index
                _ ->
                    case ets:next(HistoricalTable, CurrentIndex) of
                        '$end_of_table' ->
                            ok;  % Reached end of table
                        NextIndex ->
                            copy_historical_range(HistoricalTable, LiveTable, NextIndex, EndIndex)
                    end
            end
    end.

%% ============================================================================
%% Live Data Feeder System
%% ============================================================================

%% Start live data feeder process
start_live_data_feeder() ->
    case whereis(live_data_feeder) of
        undefined ->
            Pid = spawn(fun() -> live_data_feeder_loop() end),
            register(live_data_feeder, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

%% Stop live data feeder process
stop_live_data_feeder() ->
    case whereis(live_data_feeder) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            ok
    end.

%% Main live data feeder loop with proactive data collection
live_data_feeder_loop() ->
    %% Get live data from IB Bridge for all configured pairs
    CurrencyPairs = config:live_currency_pairs(),
    
    %% Proactively update all live tables
    lists:foreach(fun(Pair) ->
        update_live_table_with_ib_data(Pair)
    end, CurrencyPairs),
    
    %% Check for pending data requests and fulfill them
    fulfill_pending_data_requests(),
    
    %% Wait before next update
    receive
        stop -> ok;
        {request_data, From, TableName, Index} ->
            %% Handle immediate data request
            handle_immediate_data_request(From, TableName, Index),
            live_data_feeder_loop()
    after config:live_data_update_interval() ->
        live_data_feeder_loop()
    end.

%% Handle immediate data requests from lookup functions
handle_immediate_data_request(From, TableName, Index) ->
    case pull_missing_data(TableName, Index) of
        {ok, Row} ->
            From ! {data_ready, Row};
        {error, Reason} ->
            From ! {data_error, Reason}
    end.

%% Fulfill any pending data requests
fulfill_pending_data_requests() ->
    %% This could be enhanced to track pending requests
    %% For now, we rely on the pull-on-demand strategy in lookup functions
    ok.

%% Update live table with data from IB Bridge
update_live_table_with_ib_data(CurrencyPair) ->
    %% Convert IB format to historical table format
    HistoricalTableName = convert_ib_pair_to_historical_table(CurrencyPair),
    Symbol = get_currency_pair_from_table_name(HistoricalTableName),
    LiveTableName = get_live_table_name(HistoricalTableName),
    
    %% Get recent data from IB Bridge
    case ib_bridge_connector:get_ohlc_data(Symbol, 60) of  % 1-minute bars
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(LiveTableName, TechnicalRecord)
            end, OHLCList),
            
            %% Keep only recent data (last 1000 points)
            cleanup_old_live_data(LiveTableName, config:live_data_max_records());
        {ok, []} ->
            io:format("No OHLC data available for ~s~n", [Symbol]);
        {error, Reason} ->
            io:format("Failed to get OHLC data for ~s: ~p~n", [Symbol, Reason])
    end.

%% Clean up old data from live table
cleanup_old_live_data(TableName, MaxRecords) ->
    case ets:info(TableName, size) of
        Size when Size > MaxRecords ->
            %% Remove oldest records
            RecordsToRemove = Size - MaxRecords,
            remove_oldest_records(TableName, RecordsToRemove);
        _ ->
            ok
    end.

%% Remove oldest records from table
remove_oldest_records(TableName, Count) ->
    remove_oldest_records(TableName, Count, 0).

remove_oldest_records(_TableName, Count, Removed) when Removed >= Count ->
    ok;
remove_oldest_records(TableName, Count, Removed) ->
    case ets:first(TableName) of
        '$end_of_table' ->
            ok;
        Key ->
            ets:delete(TableName, Key),
            remove_oldest_records(TableName, Count, Removed + 1)
    end.

%% ============================================================================
%% Pull-on-Demand Data Access Functions
%% ============================================================================

%% Handle data requests in live tables with pull-on-demand strategy
lookup_live_with_pull(TableName, RequestedIndex) ->
    case ets:lookup(TableName, RequestedIndex) of
        [Row] -> 
            %% Data exists, return it immediately
            Row;
        [] ->
            %% Data doesn't exist, try to pull it
            case pull_missing_data(TableName, RequestedIndex) of
                {ok, Row} ->
                    %% Successfully pulled data
                    Row;
                {error, _Reason} ->
                    %% Failed to pull data, return latest available or undefined
                    get_latest_available_data(TableName)
            end
    end.

%% Enhanced sensor data access with live table support
get_sensor_data(TableName, Feature, Parameters) ->
    case is_live_table_request(TableName) of
        true ->
            %% Use live table with pull-on-demand
            LiveTableName = get_live_table_name(TableName),
            get_live_sensor_data(LiveTableName, Feature, Parameters);
        false ->
            %% Use historical table (existing logic)
            get_historical_sensor_data(TableName, Feature, Parameters)
    end.

%% Get live sensor data with pull-on-demand support
get_live_sensor_data(LiveTableName, Feature, Parameters) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            get_live_pci_sensor_data(LiveTableName, HRes, VRes);
        [HRes, list_sensor] ->
            get_live_pli_sensor_data(LiveTableName, HRes);
        _ ->
            io:format("Unknown live sensor parameters: ~p~n", [Parameters]),
            []
    end.

%% Get historical sensor data (existing logic)
get_historical_sensor_data(TableName, Feature, Parameters) ->
    case Parameters of
        [HRes, VRes, graph_sensor] ->
            get_historical_pci_sensor_data(TableName, HRes, VRes);
        [HRes, list_sensor] ->
            get_historical_pli_sensor_data(TableName, HRes);
        _ ->
            io:format("Unknown historical sensor parameters: ~p~n", [Parameters]),
            []
    end.

%% Get live PLI sensor data
get_live_pli_sensor_data(LiveTableName, HRes) ->
    %% Get last HRes records from live table
    case ets:last(LiveTableName) of
        '$end_of_table' ->
            [];
        LastIndex ->
            get_live_price_list(LiveTableName, HRes, LastIndex)
    end.

%% Get live PCI sensor data
get_live_pci_sensor_data(LiveTableName, HRes, VRes) ->
    %% Get last HRes records from live table
    case ets:last(LiveTableName) of
        '$end_of_table' ->
            lists:duplicate(HRes * VRes, -1);
        LastIndex ->
            get_live_price_list_for_encoding(LiveTableName, HRes, VRes, LastIndex)
    end.

%% Get historical PLI sensor data
get_historical_pli_sensor_data(TableName, HRes) ->
    %% Use existing historical data logic
    get_live_price_list(TableName, HRes).

%% Get historical PCI sensor data
get_historical_pci_sensor_data(TableName, HRes, VRes) ->
    %% Use existing historical data logic
    PriceList = get_live_price_list(TableName, HRes),
    encode_price_list_to_plane(PriceList, HRes, VRes).

%% Get live price list from live table
get_live_price_list(LiveTableName, HRes, LastIndex) ->
    get_live_price_list(LiveTableName, HRes, LastIndex, []).

get_live_price_list(_LiveTableName, 0, _CurrentIndex, Acc) ->
    lists:reverse(Acc);
get_live_price_list(LiveTableName, HRes, CurrentIndex, Acc) ->
    case ets:lookup(LiveTableName, CurrentIndex) of
        [#technical{open=Open, close=Close, high=High, low=Low}] ->
            PriceTuple = {Open, Close, High, Low},
            %% Get previous index using ETS navigation
            case ets:prev(LiveTableName, CurrentIndex) of
                '$end_of_table' ->
                    lists:reverse([PriceTuple | Acc]);
                PrevIndex ->
                    get_live_price_list(LiveTableName, HRes - 1, PrevIndex, [PriceTuple | Acc])
            end;
        [] ->
            %% Try to pull missing data
            case pull_missing_data(LiveTableName, CurrentIndex) of
                {ok, #technical{open=Open, close=Close, high=High, low=Low}} ->
                    PriceTuple = {Open, Close, High, Low},
                    %% Get previous index using ETS navigation
                    case ets:prev(LiveTableName, CurrentIndex) of
                        '$end_of_table' ->
                            lists:reverse([PriceTuple | Acc]);
                        PrevIndex ->
                            get_live_price_list(LiveTableName, HRes - 1, PrevIndex, [PriceTuple | Acc])
                    end;
                {error, _} ->
                    %% Try to get real market data instead of dummy data
                    case ib_bridge_connector:get_market_data('EURUSD') of
                        {ok, Tick} ->
                            Price = case Tick#market_tick.last of
                                undefined -> 
                                    case Tick#market_tick.bid of
                                        undefined -> Tick#market_tick.ask;
                                        Bid -> Bid
                                    end;
                                Last -> Last
                            end,
                            RealTuple = {Price, Price, Price, Price},
                            case ets:prev(LiveTableName, CurrentIndex) of
                                '$end_of_table' ->
                                    lists:reverse([RealTuple | Acc]);
                                PrevIndex ->
                                    get_live_price_list(LiveTableName, HRes - 1, PrevIndex, [RealTuple | Acc])
                            end;
                        {error, _} ->
                            %% Skip this data point if no real data available
                            case ets:prev(LiveTableName, CurrentIndex) of
                                '$end_of_table' ->
                                    lists:reverse(Acc);
                                PrevIndex ->
                                    get_live_price_list(LiveTableName, HRes - 1, PrevIndex, Acc)
                            end
                    end
            end
    end.

%% Get live price list for encoding
get_live_price_list_for_encoding(LiveTableName, HRes, VRes, LastIndex) ->
    PriceList = get_live_price_list(LiveTableName, HRes, LastIndex),
    encode_price_list_to_plane(PriceList, HRes, VRes).

%% Encode price list to plane format
encode_price_list_to_plane(PriceList, HRes, VRes) ->
    case PriceList of
        [] ->
            lists:duplicate(HRes * VRes, -1);
        _ ->
            %% Calculate vertical range for encoding
            HighPrices = [High || {_Open, _Close, High, _Low} <- PriceList],
            LowPrices = [Low || {_Open, _Close, _High, Low} <- PriceList],
            
            LVMax1 = lists:max(HighPrices),
            LVMin1 = lists:min(LowPrices),
            LVMax = LVMax1 + abs(LVMax1 - LVMin1) / 20,
            LVMin = LVMin1 - abs(LVMax1 - LVMin1) / 20,
            VStep = (LVMax - LVMin) / VRes,
            V_StartPos = LVMin + VStep / 2,
            
            %% Encode price data to plane format
            encode_to_plane(HRes * VRes, PriceList, V_StartPos, VStep, [])
    end.

%% Pull missing data from IB Bridge
pull_missing_data(TableName, RequestedIndex) ->
    %% Get currency pair from table name
    CurrencyPair = get_currency_pair_from_table_name(TableName),
    
    %% Determine time range to request from IB
    {StartTime, EndTime} = calculate_request_time_range(RequestedIndex),
    
    case ib_bridge_connector:get_ohlc_data_range(CurrencyPair, 60, StartTime, EndTime) of
        {ok, OHLCList} when length(OHLCList) > 0 ->
            %% Convert and insert new data
            lists:foreach(fun(OHLC) ->
                TechnicalRecord = convert_ohlc_to_technical(OHLC),
                ets:insert(TableName, TechnicalRecord)
            end, OHLCList),
            
            %% Try to get the requested data again
            case ets:lookup(TableName, RequestedIndex) of
                [Row] -> {ok, Row};
                [] -> {error, data_not_found}
            end;
        {ok, []} ->
            {error, no_data_available};
        {error, Reason} ->
            {error, Reason}
    end.

%% Calculate time range for IB data request
calculate_request_time_range(RequestedIndex) ->
    {Year, Month, Day, Hour, Minute, Second, _} = RequestedIndex,
    RequestedTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    
    %% Request 10 minutes before and after the requested time
    StartTime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(RequestedTime) - 600
    ),
    EndTime = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(RequestedTime) + 600
    ),
    
    {StartTime, EndTime}.

%% Get the latest available data from the table
get_latest_available_data(TableName) ->
    case ets:last(TableName) of
        '$end_of_table' ->
            %% No data available at all
            create_default_technical_record();
        LastIndex ->
            %% Return the most recent data point
            case ets:lookup(TableName, LastIndex) of
                [Row] -> Row;
                [] -> create_default_technical_record()
            end
    end.

%% Create a default technical record when no data is available
create_default_technical_record() ->
    #technical{
        id = {2024, 1, 1, 0, 0, 0, 60},  % {Year,Month,Day,Hour,Minute,Second,sampling_rate}
        open = 1.0,
        high = 1.0,
        low = 1.0,
        close = 1.0,
        volume = 0
    }.

%% ============================================================================
%% Live Table Navigation Functions
%% ============================================================================

%% Enhanced navigation functions with live table support
next_live(TableName, CurrentIndex) ->
    case is_live_table(TableName) of
        true ->
            %% Live table navigation
            ets:next(TableName, CurrentIndex);
        false ->
            %% Historical table navigation (existing logic)
            ets:next(TableName, CurrentIndex)
    end.

%% Get first record from live table
first_live(TableName) ->
    case is_live_table(TableName) of
        true ->
            ets:first(TableName);
        false ->
            ets:first(TableName)
    end.

%% Get last record from live table
last_live(TableName) ->
    case is_live_table(TableName) of
        true ->
            ets:last(TableName);
        false ->
            ets:last(TableName)
    end.

%% ============================================================================
%% Performance Monitoring Functions
%% ============================================================================

%% Monitor live table performance
monitor_live_tables() ->
    io:format("=== Live Table Performance Report ===~n"),
    lists:foreach(fun(TableName) ->
        case ets:info(TableName) of
            undefined ->
                io:format("~p: Not initialized~n", [TableName]);
            Info ->
                Size = proplists:get_value(size, Info),
                Memory = proplists:get_value(memory, Info),
                io:format("~p: ~p records, ~p bytes~n", [TableName, Size, Memory])
        end
    end, ?LIVE_TABLES).

%% Monitor data freshness
monitor_data_freshness() ->
    io:format("=== Data Freshness Report ===~n"),
    lists:foreach(fun(TableName) ->
        case ets:last(TableName) of
            '$end_of_table' ->
                io:format("~p: No data~n", [TableName]);
            LastIndex ->
                {Year, Month, Day, Hour, Minute, Second, _} = LastIndex,
                {CurrentYear, CurrentMonth, CurrentDay} = date(),
                {CurrentHour, CurrentMinute, CurrentSecond} = time(),
                
                LastTime = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
                CurrentTime = calendar:datetime_to_gregorian_seconds({{CurrentYear, CurrentMonth, CurrentDay}, {CurrentHour, CurrentMinute, CurrentSecond}}),
                
                AgeSeconds = CurrentTime - LastTime,
                io:format("~p: Last update ~p seconds ago~n", [TableName, AgeSeconds])
        end
    end, ?LIVE_TABLES).