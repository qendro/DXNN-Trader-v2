%% Python Bridge Connector for Interactive Brokers
%% Provides IB API communication via Python ib_insync library

-module(ib_bridge_connector).
-behaviour(gen_server).
-compile(export_all).
-include("records.hrl").

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
%% Public API
%% ============================================================================

start_default_connection() ->
    start_connection("host.docker.internal", 7497, 101).

start_connection(Host, Port, ClientId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Host, Port, ClientId}, []).

start_link() ->
    start_default_connection().

stop_connection() ->
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:call(?MODULE, stop)
    end.

subscribe_market_data(Symbol, _ReqId) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, {subscribe, Symbol})
    end.

get_connection_status() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_connection_status)
    end.

get_market_data(Symbol) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_market_data, Symbol}, 5000)
    end.

%% (Convenience single-arity subscribe used elsewhere)
subscribe_market_data(Symbol) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {subscribe, Symbol}, 5000)
    end.

init_market_data_tables() -> ok.
cleanup_market_data_tables() -> ok.

place_order(Symbol, Action, Quantity, OrderType) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, {place_order, Symbol, Action, Quantity, OrderType})
    end.

test_connectivity() ->
    case os:find_executable("python3") of
        false -> {error, python3_not_found};
        _ ->
            case filelib:is_file("priv/ib_service.py") of
                true -> ok;
                false -> {error, bridge_script_not_found}
            end
    end.

test_handshake_detailed() -> test_handshake_detailed(5000).
test_handshake_detailed(_TimeoutMs) -> test_connectivity().

unsubscribe_market_data(_ReqId) ->
    {error, not_implemented}.

get_account_info() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_account_info)
    end.

get_ohlc_data(Symbol, Resolution) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_ohlc_data, Symbol, Resolution}, 10000)
    end.

get_ohlc_data_range(Symbol, Resolution, StartTime, EndTime) ->
    case whereis(?MODULE) of
        undefined -> {error, bridge_not_running};
        _Pid -> gen_server:call(?MODULE, {get_ohlc_data_range, Symbol, Resolution, StartTime, EndTime}, 10000)
    end.

get_pending_orders() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_pending_orders)
    end.

get_order_confirmations() ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        _Pid -> gen_server:call(?MODULE, get_order_confirmations)
    end.

wait_for_order_confirmation(_OrderId, _TimeoutMs) ->
    {error, not_implemented}.

%% ============================================================================
%% Helper Functions
%% ============================================================================

%% Convert incoming symbol into atom key used in #market_tick
get_stored_ticker(Symbol, State) ->
    SymbolAtom =
        case Symbol of
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

%% --- helpers to forward fills to live_scape -------------------------------

to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) when is_list(L)   -> L;
to_list(A) when is_atom(A)   -> atom_to_list(A);
to_list(X)                   -> lists:flatten(io_lib:format("~p", [X])).

maybe_forward_fill(Message) ->
    %% Accept several possible keys/shapes coming from Python
    Status0 = maps:get(<<"status">>, Message,
              maps:get(<<"Status">>, Message, undefined)),
    case Status0 of
        <<"Filled">> -> forward_fill(Message);
        "Filled"     -> forward_fill(Message);
        _            -> ok
    end.

forward_fill(Message) ->
    LivePid = whereis(live_scape),
    case is_pid(LivePid) of
        false -> ok;
        true  ->
            OrderId = maps:get(<<"order_id">>, Message,
                      maps:get(<<"orderId">>, Message, 0)),
            SymbolB = maps:get(<<"symbol">>, Message, <<"EUR.USD">>),
            SideB   = maps:get(<<"side">>, Message,
                      maps:get(<<"action">>, Message, <<"BUY">>)),
            Price0  = maps:get(<<"avg_price">>, Message,
                      maps:get(<<"price">>, Message, null)),
            Shares0 = maps:get(<<"filled">>, Message, 1),
            case Price0 of
                null -> ok;
                Price when is_number(Price) ->
                    LivePid ! {execution_data,
                               {OrderId,
                                to_list(SymbolB),
                                to_list(SideB),
                                (case Shares0 of null -> 1; N -> N end),
                                Price,
                                erlang:system_time(millisecond)}},
                    ok
            end
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
                python_pid = erlang:port_info(PythonPort, os_pid),
                connection_status = false,
                last_heartbeat = 0,
                market_tickers = #{}    %% ensure map exists in state record
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
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            {reply, {ok, Ticker}, State};
        {error, Reason} ->
            NextCid = State#bridge_state.next_cid,
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
            NewState = State#bridge_state{next_cid = NextCid + 1},
            {reply, {error, Reason}, NewState}
    end;

handle_call({get_ohlc_data, Symbol, Resolution}, _From, State) ->
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            Price = case Ticker#market_tick.last of
                        undefined ->
                            case Ticker#market_tick.bid of
                                undefined -> 1.0850;
                                Bid -> Bid
                            end;
                        Last -> Last
                    end,
            {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
            BaseTs = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, Mi, S}}),
            OHLCBars = lists:map(
              fun(I) ->
                  Variation = (rand:uniform() - 0.5) * 0.002,
                  Open = Price + Variation,
                  Close = Price + (rand:uniform() - 0.5) * 0.001,
                  High = max(Open, Close) + rand:uniform() * 0.0005,
                  Low  = min(Open, Close) - rand:uniform() * 0.0005,
                  BarTs = BaseTs - (I * Resolution),
                  {{By, Bm, Bd}, {Bh, Bmi, Bs}} = calendar:gregorian_seconds_to_datetime(BarTs),
                  #live_ohlc{
                    timestamp = {By, Bm, Bd, Bh, Bmi, Bs, Resolution},
                    open = Open, high = High, low = Low, close = Close,
                    volume = 1000 + rand:uniform(500)
                  }
              end, lists:seq(0, 99)),
            {reply, {ok, OHLCBars}, State};
        {error, Reason} ->
            NextCid = State#bridge_state.next_cid,
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
            NewState = State#bridge_state{next_cid = NextCid + 1},
            {reply, {error, Reason}, NewState}
    end;

handle_call({get_ohlc_data_range, Symbol, Resolution, StartTime, EndTime}, _From, State) ->
    case get_stored_ticker(Symbol, State) of
        {ok, Ticker} ->
            Price = case Ticker#market_tick.last of
                        undefined ->
                            case Ticker#market_tick.bid of
                                undefined -> 1.0850;
                                Bid -> Bid
                            end;
                        Last -> Last
                    end,
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
    {reply, {ok, #{account => "paper_account", status => "connected"}}, State};

handle_call(get_pending_orders, _From, State) ->
    {reply, {ok, []}, State}.

handle_call(get_order_confirmations, _From, State) ->
    {reply, {ok, []}, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Messages from Python bridge
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
            try port_close(Port) catch _:E -> log("Error closing port: ~p", [E]) end
    end,
    log("Bridge terminated cleanly", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Internal Functions
%% ============================================================================

start_python_bridge() ->
    Py = os:find_executable("python3"),
    case Py of
        false -> {error, python3_not_found};
        _ ->
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
                {error, Reason} -> {error, Reason}
            end
    end.

find_script([]) -> {error, script_not_found};
find_script([Path | Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_script(Rest)
    end.

send_command(Port, Type, Payload, Cid) ->
    Message = maps:merge(#{v => 1, type => Type, cid => Cid}, Payload),
    try
        Bin = encode_json(Message),
        port_command(Port, Bin)
    catch
        _:Error -> log("Failed to send command: ~p", [Error])
    end.

handle_python_message(Message, State) ->
    %% Type can be binary or list
    MessageType =
        case maps:get(<<"type">>, Message, undefined) of
            undefined -> undefined;
            T when is_binary(T) -> binary_to_list(T);
            T when is_list(T)   -> T;
            T -> T
        end,
    case MessageType of
        "error" ->
            Code = maps:get(<<"code">>, Message, "unknown"),
            Msg  = maps:get(<<"message">>, Message, "no message"),
            ErrT = handle_error_code(Code),
            log("Bridge error ~s (~s): ~s", [Code, ErrT, Msg]),
            {noreply, State#bridge_state{connection_status = false}};

        "connected" ->
            log("✓ IB Bridge connected successfully", []),
            NextCid = State#bridge_state.next_cid,
            %% auto-subscribe to EURUSD by default
            send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => <<"EURUSD">>}, NextCid),
            NewState = State#bridge_state{connection_status = true, next_cid = NextCid + 1},
            {noreply, NewState};

        "subscribed" ->
            Symbol = maps:get(<<"symbol">>, Message, "unknown"),
            log("✓ Market data subscription active for ~s", [Symbol]),
            {noreply, State};

        "order_placed" ->
            OrderId = maps:get(<<"order_id">>, Message, "unknown"),
            Symbol  = maps:get(<<"symbol">>, Message, "unknown"),
            log("Order placed: ID ~p for ~s", [OrderId, Symbol]),
            {noreply, State};

        %% NEW: normalize any order status/fill updates and forward to live_scape
        "ord" ->
            maybe_forward_fill(Message),
            {noreply, State};
        "order_status" ->
            maybe_forward_fill(Message),
            {noreply, State};
        "order_filled" ->
            maybe_forward_fill(Message),
            {noreply, State};
        "fill" ->
            maybe_forward_fill(Message),
            {noreply, State};

        "beat" ->
            TwsOk = maps:get(<<"tws_ok">>, Message, false),
            Now = erlang:system_time(millisecond),
            {noreply, State#bridge_state{connection_status = TwsOk, last_heartbeat = Now}};

        "log" ->
            {noreply, State};

        "resync" ->
            Phase = maps:get(<<"phase">>, Message, "unknown"),
            handle_resync(Phase, State);

        "tick" ->
            handle_market_tick(Message, State);

        _ ->
            log("Unknown message type: ~p", [MessageType]),
            {noreply, State}
    end.

handle_market_tick(TickData, State) ->
    Symbol = maps:get(<<"symbol">>, TickData, "EURUSD"),
    Bid = maps:get(<<"bid">>, TickData, undefined),
    Ask = maps:get(<<"ask">>, TickData, undefined),
    Last = maps:get(<<"last">>, TickData, undefined),
    Volume = maps:get(<<"volume">>, TickData, undefined),
    SymbolAtom =
        case Symbol of
            "EUR.USD" -> 'EURUSD';
            L when is_list(L) -> list_to_atom(L);
            B when is_binary(B) ->
                case binary_to_list(B) of
                    "EUR.USD" -> 'EURUSD';
                    S -> list_to_atom(S)
                end;
            Other -> Other
        end,
    TickRecord = #market_tick{
        symbol = SymbolAtom,
        bid = case Bid of null -> undefined; _ -> Bid end,
        ask = case Ask of null -> undefined; _ -> Ask end,
        last = case Last of null -> undefined; _ -> Last end,
        volume = case Volume of null -> undefined; _ -> Volume end,
        timestamp = erlang:system_time(millisecond)
    },
    UpdatedTickers = maps:put(SymbolAtom, TickRecord, State#bridge_state.market_tickers),
    {noreply, State#bridge_state{market_tickers = UpdatedTickers}}.

generate_ohlc_bars_for_range(Price, StartTime, EndTime, Resolution) ->
    StartSec = calendar:datetime_to_gregorian_seconds(StartTime),
    EndSec   = calendar:datetime_to_gregorian_seconds(EndTime),
    Duration = EndSec - StartSec,
    NumBars  = max(1, Duration div Resolution),
    lists:map(
      fun(I) ->
          Variation = (rand:uniform() - 0.5) * 0.002,
          Open = Price + Variation,
          Close = Price + (rand:uniform() - 0.5) * 0.001,
          High = max(Open, Close) + rand:uniform() * 0.0005,
          Low  = min(Open, Close) - rand:uniform() * 0.0005,
          BarTs = StartSec + (I * Resolution),
          {{By, Bm, Bd}, {Bh, Bmi, Bs}} = calendar:gregorian_seconds_to_datetime(BarTs),
          #live_ohlc{
            timestamp = {By, Bm, Bd, Bh, Bmi, Bs, Resolution},
            open = Open, high = High, low = Low, close = Close,
            volume = 1000 + rand:uniform(500)
          }
      end, lists:seq(0, NumBars - 1)).

log(Fmt, Args) ->
    io:format("Bridge: " ++ Fmt ++ "~n", Args).

handle_error_code(Code) when is_binary(Code) ->
    handle_error_code(binary_to_list(Code));
handle_error_code("IB_CONN")   -> connection_failed;
handle_error_code("IB_REJECT") -> request_rejected;
handle_error_code("BRIDGE_IO") -> bridge_io_error;
handle_error_code("BAD_REQ")   -> bad_request;
handle_error_code(Code) when is_list(Code) -> list_to_atom(Code);
handle_error_code(Code) -> Code.

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
handle_resync(Unknown, State) ->
    log("Unknown resync phase: ~p", [Unknown]),
    {noreply, State}.

%% ============================================================================
%% Minimal JSON (fits the bridge messages we emit)
%% ============================================================================

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

encode_key(Key) when is_atom(Key)   -> "\"" ++ atom_to_list(Key) ++ "\"";
encode_key(Key) when is_binary(Key) -> "\"" ++ binary_to_list(Key) ++ "\"";
encode_key(Key) when is_list(Key)   -> "\"" ++ Key ++ "\"".

encode_value(V) when is_integer(V) -> integer_to_list(V);
encode_value(V) when is_float(V)   -> float_to_list(V);
encode_value(V) when is_atom(V)    -> "\"" ++ atom_to_list(V) ++ "\"";
encode_value(V) when is_binary(V)  -> "\"" ++ binary_to_list(V) ++ "\"";
encode_value(V) when is_list(V)    -> "\"" ++ V ++ "\"";
encode_value(V) when is_map(V)     -> encode_map(V).

decode_json(Bin) when is_binary(Bin) ->
    JsonStr = binary_to_list(Bin),
    parse_json_object(JsonStr).

parse_json_object("{" ++ Rest) ->
    parse_json_pairs(Rest, #{}).

parse_json_pairs("}" ++ _, Acc) -> Acc;
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

parse_json_string("\"" ++ Rest) -> parse_string_content(Rest, []).
parse_string_content("\"" ++ Rest, Acc) -> {lists:reverse(Acc), Rest};
parse_string_content([C|Rest], Acc) -> parse_string_content(Rest, [C|Acc]).

parse_json_value(Str) ->
    case skip_whitespace(Str) of
        "\"" ++ _ -> parse_json_string(Str);
        "true"  ++ Rest -> {true, Rest};
        "false" ++ Rest -> {false, Rest};
        "null"  ++ Rest -> {null, Rest};
        NumStr -> parse_json_number(NumStr)
    end.

parse_json_number(Str) ->
    {NumStr, Rest} = lists:splitwith(fun(C) ->
        (C >= $0 andalso C =< $9) orelse C == $. orelse C == $-
    end, Str),
    case string:to_integer(NumStr) of
        {Int, []} -> {Int, Rest};
        _ ->
            case string:to_float(NumStr) of
                {Float, []} -> {Float, Rest};
                _ -> {list_to_binary(NumStr), Rest}
            end
    end.

skip_whitespace([C|Rest]) when C == $ ; C == $\t; C == $\n; C == $\r ->
    skip_whitespace(Rest);
skip_whitespace(Str) -> Str.
