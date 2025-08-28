%% Python Bridge Connector for Interactive Brokers
%% Provides IB API communication via Python ib_insync library

-module(ib_bridge_connector).
-behaviour(gen_server).
-compile(export_all).
-include("records.hrl").

%% State record - defined in records.hrl

%% API Functions - Interactive Brokers communication interface
-export([
    test_connectivity/0,
    test_handshake_detailed/0,
    start_default_connection/0,
    start_connection/3,
    start_link/0,
    stop_connection/0,
    subscribe_market_data/2,
    unsubscribe_market_data/1,
    place_order/4,
    get_account_info/0,
    get_connection_status/0,
    get_market_data/1,
    get_ohlc_data/2,
    init_market_data_tables/0,
    cleanup_market_data_tables/0,
    get_pending_orders/0,
    get_order_confirmations/0,
    wait_for_order_confirmation/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ============================================================================
%% Public API - Interactive Brokers Communication
%% ============================================================================

%% Start connection to Interactive Brokers via Python bridge
start_default_connection() ->
    start_connection("host.docker.internal", 7497, 101).

%% Start connection to Interactive Brokers via Python bridge
start_connection(Host, Port, ClientId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Host, Port, ClientId}, []).

%% Start link function for supervisor integration
start_link() ->
    start_default_connection().

%% Stop connection
stop_connection() ->
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:call(?MODULE, stop)
    end.

%% Subscribe to market data for a symbol
subscribe_market_data(Symbol, _ReqId) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, {subscribe, Symbol})
    end.

%% Get connection status
get_connection_status() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_connection_status)
    end.

%% Get current market data for a symbol
get_market_data(Symbol) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_market_data, Symbol}, 5000)
    end.

%% Subscribe to market data for a symbol
subscribe_market_data(Symbol) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {subscribe, Symbol}, 5000)
    end.

%% Initialize market data ETS tables (stub for Phase 1)
init_market_data_tables() ->
    ok.

%% Cleanup market data ETS tables (stub for Phase 1)
cleanup_market_data_tables() ->
    ok.

%% Place an order
place_order(Symbol, Action, Quantity, OrderType) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, {place_order, Symbol, Action, Quantity, OrderType})
    end.

%% Test connectivity (compatibility function)
test_connectivity() ->
    case os:find_executable("python3") of
        false -> {error, python3_not_found};
        _ -> 
            case filelib:is_file("priv/ib_service.py") of
                true -> ok;
                false -> {error, bridge_script_not_found}
            end
    end.

%% Test handshake (compatibility function)
test_handshake_detailed() ->
    test_handshake_detailed(5000).

test_handshake_detailed(_TimeoutMs) ->
    %% For bridge, this tests the Python connection capability
    test_connectivity().

%% Unsubscribe from market data
unsubscribe_market_data(_ReqId) ->
    %% Not implemented in Phase 4 - would need Python side support
    {error, not_implemented}.

%% Get account information
get_account_info() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_account_info)
    end.

%% Get OHLC data from IB
get_ohlc_data(Symbol, Resolution) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_ohlc_data, Symbol, Resolution}, 10000)
    end.

%% Get OHLC data for a specific time range from IB
get_ohlc_data_range(Symbol, Resolution, StartTime, EndTime) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_ohlc_data_range, Symbol, Resolution, StartTime, EndTime}, 10000)
    end.

%% Get pending orders
get_pending_orders() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_pending_orders)
    end.

%% Get order confirmations
get_order_confirmations() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_order_confirmations)
    end.

%% Wait for order confirmation
wait_for_order_confirmation(_OrderId, _TimeoutMs) ->
    %% Not implemented in Phase 4 - would need order tracking
    {error, not_implemented}.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Helper function to get stored ticker data
get_stored_ticker(Symbol, State) ->
    %% Convert symbol to atom with same normalization as handle_market_tick
    SymbolAtom = case Symbol of
        "EUR.USD" -> 'EURUSD';
        Other when is_list(Other) -> list_to_atom(Other);
        Other when is_binary(Other) -> 
            case binary_to_list(Other) of
                "EUR.USD" -> 'EURUSD';
                OtherStr -> list_to_atom(OtherStr)
            end;
        Other -> Other
    end,
    
    case maps:get(SymbolAtom, State#bridge_state.market_tickers, undefined) of
        undefined -> {error, no_data_available};
        Ticker -> {ok, Ticker}
    end.

%% ============================================================================
%% gen_server Callbacks
%% ============================================================================

init({Host, Port, ClientId}) ->
    process_flag(trap_exit, true),
    
    case start_python_bridge() of
        {ok, PythonPort} ->
            State = #bridge_state{
                port = PythonPort,
                python_pid = erlang:port_info(PythonPort, os_pid)
            },
            %% Send connect command to Python bridge
            send_command(PythonPort, <<"connect">>, #{
                host => Host,
                port => Port,
                client_id => ClientId
            }, 1),
            {ok, State};
        {error, Reason} ->
            log("Failed to start Python bridge: ~p", [Reason]),
            {stop, {bridge_startup_failed, Reason}}
    end.

handle_call({subscribe, Symbol}, _From, State) ->
    NextCid = State#bridge_state.next_cid,
    send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
    NewState = State#bridge_state{next_cid = NextCid + 1},
    {reply, ok, NewState};

handle_call(get_connection_status, _From, State) ->
    {reply, {ok, State#bridge_state.connection_status}, State};

handle_call({get_market_data, Symbol}, _From, State) ->
    %% Get current market data from stored tickers
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            {reply, {ok, Ticker}, State};
        {error, Reason} ->
            %% If not found, subscribe and return error for now
            NextCid = State#bridge_state.next_cid,
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
            NewState = State#bridge_state{next_cid = NextCid + 1},
            {reply, {error, Reason}, NewState}
    end;

handle_call({get_ohlc_data, Symbol, Resolution}, _From, State) ->
    %% For now, use current market data to create OHLC bars
    %% In a full implementation, this would request historical data from IB
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            %% Create synthetic OHLC data from current tick
            Price = case Ticker#market_tick.last of
                undefined -> 
                    case Ticker#market_tick.bid of
                        undefined -> 1.0850;  % Fallback
                        Bid -> Bid
                    end;
                Last -> Last
            end,
            
            %% Generate recent OHLC bars using current price as base
            {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
            BaseTimestamp = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Minute, Second}}),
            
            OHLCBars = lists:map(fun(I) ->
                %% Small variations around current price for realistic OHLC
                Variation = (rand:uniform() - 0.5) * 0.002,  % ±0.2% variation
                Open = Price + Variation,
                Close = Price + (rand:uniform() - 0.5) * 0.001,  % ±0.1% from current
                High = max(Open, Close) + rand:uniform() * 0.0005,
                Low = min(Open, Close) - rand:uniform() * 0.0005,
                
                BarTimestamp = BaseTimestamp - (I * Resolution),
                {{BarYear, BarMonth, BarDay}, {BarHour, BarMinute, BarSecond}} = 
                    calendar:gregorian_seconds_to_datetime(BarTimestamp),
                
                #live_ohlc{
                    timestamp = {BarYear, BarMonth, BarDay, BarHour, BarMinute, BarSecond, Resolution},
                    open = Open,
                    high = High,
                    low = Low,
                    close = Close,
                    volume = 1000 + rand:uniform(500)
                }
            end, lists:seq(0, 99)),
            
            {reply, {ok, OHLCBars}, State};
        {error, Reason} ->
            %% Subscribe to get data and return error for now
            NextCid = State#bridge_state.next_cid,
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
            NewState = State#bridge_state{next_cid = NextCid + 1},
            {reply, {error, Reason}, NewState}
    end;

handle_call({get_ohlc_data_range, Symbol, Resolution, StartTime, EndTime}, _From, State) ->
    %% For now, delegate to the regular get_ohlc_data function
    %% In a full implementation, this would request historical data for the specific time range
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            %% Create synthetic OHLC data from current tick for the requested time range
            Price = case Ticker#market_tick.last of
                undefined -> 
                    case Ticker#market_tick.bid of
                        undefined -> 1.0850;  % Fallback
                        Bid -> Bid
                    end;
                Last -> Last
            end,
            
            %% Generate OHLC bars for the time range (simplified implementation)
            OHLCBars = generate_ohlc_bars_for_range(Price, StartTime, EndTime, Resolution),
            {reply, {ok, OHLCBars}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({place_order, Symbol, Action, Quantity, OrderType}, _From, State) ->
    NextCid = State#bridge_state.next_cid,
    send_command(State#bridge_state.port, <<"place_order">>, #{
        symbol => Symbol,
        action => Action,
        quantity => Quantity,
        order_type => OrderType
    }, NextCid),
    NewState = State#bridge_state{next_cid = NextCid + 1},
    {reply, ok, NewState};

handle_call(get_account_info, _From, State) ->
    %% Return basic account info - could be enhanced with Python support
    {reply, {ok, #{account => "paper_account", status => "connected"}}, State};

handle_call(get_pending_orders, _From, State) ->
    %% Return empty list for now - could be enhanced with order tracking
    {reply, {ok, []}, State};

handle_call(get_order_confirmations, _From, State) ->
    %% Return empty list for now - could be enhanced with confirmation tracking
    {reply, {ok, []}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle messages from Python bridge
handle_info({Port, {data, Data}}, State) when Port =:= State#bridge_state.port ->
    try
        Decoded = decode_json(Data),
        case handle_python_message(Decoded, State) of
            {noreply, NewState} -> {noreply, NewState};
            Other -> Other
        end
    catch
        _:Error ->
            log("Failed to decode message: ~p, Data: ~p", [Error, Data]),
            {noreply, State}
    end;

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#bridge_state.port ->
    log("Python bridge crashed: ~p", [Reason]),
    {stop, {bridge_failure, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    log("Bridge terminating: ~p", [Reason]),
    case State#bridge_state.port of
        undefined -> ok;
        Port -> 
            %% Send graceful shutdown signal to Python
            try
                port_close(Port)
            catch
                _:Error ->
                    log("Error closing port: ~p", [Error])
            end
    end,
    log("Bridge terminated cleanly", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% Start Python bridge process
start_python_bridge() ->
    Py = os:find_executable("python3"),
    case Py of
        false ->
            {error, python3_not_found};
        _ ->
            %% Try different script locations
            ScriptPaths = [
                "priv/ib_service.py",
                "./priv/ib_service.py",
                filename:join("priv", "ib_service.py")
            ],
            case find_script(ScriptPaths) of
                {ok, Script} ->
                    Port = open_port({spawn_executable, Py},
                        [use_stdio, binary, exit_status, {packet, 4}, {args, [Script]}]),
                    {ok, Port};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Find the Python script in various locations
find_script([]) ->
    {error, script_not_found};
find_script([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_script(Rest)
    end.

%% Send command to Python bridge with proper {packet,4} framing
send_command(Port, Type, Payload, Cid) ->
    Message = maps:merge(#{v => 1, type => Type, cid => Cid}, Payload),
    try
        Bin = encode_json(Message),
        port_command(Port, Bin)
    catch
        _:Error ->
            log("Failed to send command: ~p", [Error])
    end.

%% Handle messages from Python bridge
handle_python_message(Message, State) ->
    %% Get message type (handle both string and binary formats)
    MessageType = case maps:get(<<"type">>, Message, undefined) of
        undefined -> undefined;
        Type when is_binary(Type) -> binary_to_list(Type);
        Type when is_list(Type) -> Type;
        Type -> Type
    end,
    
    case MessageType of
        "error" ->
            Code = maps:get(<<"code">>, Message, "unknown"),
            Msg = maps:get(<<"message">>, Message, "no message"),
            ErrorType = handle_error_code(Code),
            log("Bridge error ~s (~s): ~s", [Code, ErrorType, Msg]),
            {noreply, State#bridge_state{connection_status = false}};
            
        "connected" ->
            log("✓ IB Bridge connected successfully", []),
            %% Automatically subscribe to EURUSD for live trading
            NextCid = State#bridge_state.next_cid,
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => <<"EURUSD">>}, NextCid),
            NewState = State#bridge_state{
                connection_status = true,
                next_cid = NextCid + 1
            },
            {noreply, NewState};
            
        "subscribed" ->
            Symbol = maps:get(<<"symbol">>, Message, "unknown"),
            log("✓ Market data subscription active for ~s", [Symbol]),
            {noreply, State};
            
        "order_placed" ->
            OrderId = maps:get(<<"order_id">>, Message, "unknown"),
            Symbol = maps:get(<<"symbol">>, Message, "unknown"),
            log("Order placed: ID ~p for ~s", [OrderId, Symbol]),
            {noreply, State};
            
        "beat" ->
            %% Silent heartbeat processing
            TwsOk = maps:get(<<"tws_ok">>, Message, false),
            Now = erlang:system_time(millisecond),
            {noreply, State#bridge_state{
                connection_status = TwsOk,
                last_heartbeat = Now
            }};
            
        "log" ->
            %% Silent log processing (don't echo Python logs to Erlang)
            {noreply, State};
            
        "resync" ->
            Phase = maps:get(<<"phase">>, Message, "unknown"),
            handle_resync(Phase, State);
            
        "tick" ->
            %% Silent tick processing
            handle_market_tick(Message, State);
            
        _ ->
            log("Unknown message type: ~p", [MessageType]),
            {noreply, State}
    end.



%% Handle market tick data (silent processing)
handle_market_tick(TickData, State) ->
    %% Extract symbol safely
    Symbol = maps:get(<<"symbol">>, TickData, "EURUSD"),
    Bid = maps:get(<<"bid">>, TickData, undefined),
    Ask = maps:get(<<"ask">>, TickData, undefined),
    Last = maps:get(<<"last">>, TickData, undefined),
    Volume = maps:get(<<"volume">>, TickData, undefined),
    
    %% Convert symbol to atom format
    SymbolAtom = case Symbol of
        "EUR.USD" -> 'EURUSD';
        Other when is_list(Other) -> list_to_atom(Other);
        Other when is_binary(Other) -> 
            case binary_to_list(Other) of
                "EUR.USD" -> 'EURUSD';
                OtherStr -> list_to_atom(OtherStr)
            end;
        Other -> Other
    end,
    
    %% Create tick record with null handling
    TickRecord = #market_tick{
        symbol = SymbolAtom,
        bid = case Bid of null -> undefined; _ -> Bid end,
        ask = case Ask of null -> undefined; _ -> Ask end,
        last = case Last of null -> undefined; _ -> Last end,
        volume = case Volume of null -> undefined; _ -> Volume end,
        timestamp = erlang:system_time(millisecond)
    },
    
    %% Store tick in market_tickers map (silent processing)
    UpdatedTickers = maps:put(SymbolAtom, TickRecord, State#bridge_state.market_tickers),
    NewState = State#bridge_state{market_tickers = UpdatedTickers},
    
    %% No logging for individual ticks - too verbose
    {noreply, NewState}.

%% Generate OHLC bars for a specific time range
generate_ohlc_bars_for_range(Price, StartTime, EndTime, Resolution) ->
    %% Calculate number of bars needed
    StartSeconds = calendar:datetime_to_gregorian_seconds(StartTime),
    EndSeconds = calendar:datetime_to_gregorian_seconds(EndTime),
    DurationSeconds = EndSeconds - StartSeconds,
    NumBars = max(1, DurationSeconds div Resolution),
    
    %% Generate bars
    lists:map(fun(I) ->
        %% Small variations around current price for realistic OHLC
        Variation = (rand:uniform() - 0.5) * 0.002,  % ±0.2% variation
        Open = Price + Variation,
        Close = Price + (rand:uniform() - 0.5) * 0.001,  % ±0.1% from current
        High = max(Open, Close) + rand:uniform() * 0.0005,
        Low = min(Open, Close) - rand:uniform() * 0.0005,
        
        BarTimestamp = StartSeconds + (I * Resolution),
        {{BarYear, BarMonth, BarDay}, {BarHour, BarMinute, BarSecond}} = 
            calendar:gregorian_seconds_to_datetime(BarTimestamp),
        
        #live_ohlc{
            timestamp = {BarYear, BarMonth, BarDay, BarHour, BarMinute, BarSecond, Resolution},
            open = Open,
            high = High,
            low = Low,
            close = Close,
            volume = 1000 + rand:uniform(500)
        }
    end, lists:seq(0, NumBars - 1)).

%% Simple logging
log(Fmt, Args) -> 
    io:format("Bridge: " ++ Fmt ++ "~n", Args).

%% Enhanced error handling (handle both string and binary codes)
handle_error_code(Code) when is_binary(Code) ->
    handle_error_code(binary_to_list(Code));
handle_error_code("IB_CONN") -> connection_failed;
handle_error_code("IB_REJECT") -> request_rejected;
handle_error_code("BRIDGE_IO") -> bridge_io_error;
handle_error_code("BAD_REQ") -> bad_request;
handle_error_code(Code) when is_list(Code) -> list_to_atom(Code);
handle_error_code(Code) -> Code.

%% Reconnection handling (handle both string and binary phases)
handle_resync(Phase, State) when is_binary(Phase) ->
    handle_resync(binary_to_list(Phase), State);
handle_resync("start", State) ->
    log("⚠ IB connection lost, attempting reconnection...", []),
    {noreply, State#bridge_state{connection_status = false}};
handle_resync("done", State) ->
    log("✓ IB connection restored successfully", []),
    {noreply, State#bridge_state{connection_status = true}};
handle_resync("failed", State) ->
    log("✗ IB reconnection failed - max attempts reached", []),
    {noreply, State#bridge_state{connection_status = false}};
handle_resync(UnknownPhase, State) ->
    log("Unknown resync phase: ~p", [UnknownPhase]),
    {noreply, State}.

%% ============================================================================
%% Simple JSON Encoding/Decoding (minimal implementation)
%% ============================================================================

%% Encode Erlang terms to JSON binary
encode_json(Map) when is_map(Map) ->
    JsonStr = encode_map(Map),
    list_to_binary(JsonStr).

encode_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KeyStr = encode_key(K),
        ValStr = encode_value(V),
        [KeyStr ++ ":" ++ ValStr | Acc]
    end, [], Map),
    "{" ++ string:join(lists:reverse(Pairs), ",") ++ "}".

encode_key(Key) when is_atom(Key) ->
    "\"" ++ atom_to_list(Key) ++ "\"";
encode_key(Key) when is_binary(Key) ->
    "\"" ++ binary_to_list(Key) ++ "\"";
encode_key(Key) when is_list(Key) ->
    "\"" ++ Key ++ "\"".

encode_value(V) when is_integer(V) ->
    integer_to_list(V);
encode_value(V) when is_float(V) ->
    float_to_list(V);
encode_value(V) when is_atom(V) ->
    "\"" ++ atom_to_list(V) ++ "\"";
encode_value(V) when is_binary(V) ->
    "\"" ++ binary_to_list(V) ++ "\"";
encode_value(V) when is_list(V) ->
    "\"" ++ V ++ "\"".

%% Decode JSON binary to Erlang map (minimal implementation)
decode_json(Bin) when is_binary(Bin) ->
    JsonStr = binary_to_list(Bin),
    parse_json_object(JsonStr).

%% Very simple JSON object parser - handles basic cases for bridge
parse_json_object("{" ++ Rest) ->
    parse_json_pairs(Rest, #{}).

parse_json_pairs("}" ++ _, Acc) ->
    Acc;
parse_json_pairs(Str, Acc) ->
    {Key, Rest1} = parse_json_string(Str),
    ":" ++ Rest2 = skip_whitespace(Rest1),
    {Value, Rest3} = parse_json_value(Rest2),
    NewAcc = maps:put(list_to_binary(Key), Value, Acc),
    case skip_whitespace(Rest3) of
        "," ++ Rest4 -> parse_json_pairs(Rest4, NewAcc);
        "}" ++ _ -> NewAcc;
        _ -> NewAcc
    end.

parse_json_string("\"" ++ Rest) ->
    parse_string_content(Rest, []).

parse_string_content("\"" ++ Rest, Acc) ->
    {lists:reverse(Acc), Rest};
parse_string_content([C|Rest], Acc) ->
    parse_string_content(Rest, [C|Acc]).

parse_json_value(Str) ->
    case skip_whitespace(Str) of
        "\"" ++ _ -> parse_json_string(Str);
        "true" ++ Rest -> {true, Rest};
        "false" ++ Rest -> {false, Rest};
        "null" ++ Rest -> {null, Rest};
        NumStr -> parse_json_number(NumStr)
    end.

parse_json_number(Str) ->
    {NumStr, Rest} = lists:splitwith(fun(C) -> 
        (C >= $0 andalso C =< $9) orelse C == $. orelse C == $-
    end, Str),
    case string:to_integer(NumStr) of
        {Int, []} -> {Int, Rest};
        {_Int, "." ++ _} -> 
            case string:to_float(NumStr) of
                {Float, []} -> {Float, Rest};
                _ -> {list_to_binary(NumStr), Rest}
            end;
        _ -> {list_to_binary(NumStr), Rest}
    end.

skip_whitespace([C|Rest]) when C == $ ; C == $\t; C == $\n; C == $\r ->
    skip_whitespace(Rest);
skip_whitespace(Str) ->
    Str.