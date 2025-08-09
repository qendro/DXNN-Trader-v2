%% Interactive Brokers API Connector Module
%% Implements TWS API communication using native Erlang gen_tcp sockets
%% Handles connection management, market data, and order execution

-module(ib_connector).
-compile(export_all).
-include("records.hrl").
-behaviour(gen_server).

%% Records are defined in records.hrl

%% API Functions
-export([
    start_connection/3,
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

%% Internal state
-define(SERVER, ?MODULE).
-define(TIMEOUT, 5000).
-define(HEARTBEAT_INTERVAL, 30000).
-define(MAX_RECONNECT_ATTEMPTS, 10).
-define(INITIAL_BACKOFF, 1000).

%% Market data ETS tables
-define(MARKET_TICKS_TABLE, live_market_ticks).
-define(OHLC_DATA_TABLE, live_ohlc_data).
-define(PRICE_BUFFER_TABLE, live_price_buffer).

%% Market data aggregation settings
-define(OHLC_WINDOW_SIZE, 60). % 60 seconds for 1-minute OHLC
-define(MAX_PRICE_BUFFER_SIZE, 1000). % Maximum number of price points to buffer

%% IB API Message Types
-define(CLIENT_VERSION, 76).
-define(MIN_SERVER_VER, 38).

%% Message IDs
-define(REQ_MKT_DATA, 1).
-define(CANCEL_MKT_DATA, 2).
-define(PLACE_ORDER, 3).
-define(CANCEL_ORDER, 4).
-define(REQ_OPEN_ORDERS, 5).
-define(REQ_ACCOUNT_DATA, 6).
-define(REQ_EXECUTIONS, 7).
-define(REQ_IDS, 8).
-define(REQ_CONTRACT_DATA, 9).
-define(REQ_MKT_DEPTH, 10).

%% Incoming message types
-define(TICK_PRICE, 1).
-define(TICK_SIZE, 2).
-define(ORDER_STATUS, 3).
-define(ERR_MSG, 4).
-define(OPEN_ORDER, 5).
-define(ACCT_VALUE, 6).
-define(PORTFOLIO_VALUE, 7).
-define(ACCT_UPDATE_TIME, 8).
-define(NEXT_VALID_ID, 9).
-define(CONTRACT_DATA, 10).
-define(EXECUTION_DATA, 11).
-define(MKT_DEPTH, 12).

%% State record
-record(state, {
    connection = #ib_connection{},
    reconnect_attempts = 0,
    reconnect_timer,
    heartbeat_timer,
    message_buffer = <<>>,
    pending_orders = [],  % List of {OrderId, Symbol, Action, Quantity, Timestamp}
    order_confirmations = []  % List of {OrderId, Status, FillPrice, FillQuantity}
}).

%% ============================================================================
%% Public API
%% ============================================================================

%% Start connection to Interactive Brokers TWS/Gateway
start_connection(Host, Port, ClientId) ->
    io:format("Starting IB connector with Host: ~p, Port: ~p, ClientId: ~p~n", 
              [Host, Port, ClientId]),
    case gen_server:start_link({local, ?SERVER}, ?MODULE, 
                              {Host, Port, ClientId}, []) of
        {ok, Pid} ->
            io:format("IB connector started successfully~n"),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start IB connector: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Stop connection
stop_connection() ->
    gen_server:call(?SERVER, stop).

%% Subscribe to market data for a symbol
subscribe_market_data(Symbol, ReqId) ->
    gen_server:call(?SERVER, {subscribe_market_data, Symbol, ReqId}).

%% Unsubscribe from market data
unsubscribe_market_data(ReqId) ->
    gen_server:call(?SERVER, {unsubscribe_market_data, ReqId}).

%% Get current market data for a symbol
get_market_data(Symbol) ->
    gen_server:call(?SERVER, {get_market_data, Symbol}).

%% Get OHLC data for a symbol with specified resolution
get_ohlc_data(Symbol, Resolution) ->
    gen_server:call(?SERVER, {get_ohlc_data, Symbol, Resolution}).

%% Initialize market data ETS tables
init_market_data_tables() ->
    gen_server:call(?SERVER, init_market_data_tables).

%% Cleanup market data ETS tables
cleanup_market_data_tables() ->
    gen_server:call(?SERVER, cleanup_market_data_tables).

%% Place an order
place_order(Symbol, Action, Quantity, OrderType) ->
    gen_server:call(?SERVER, {place_order, Symbol, Action, Quantity, OrderType}).

%% Get account information
get_account_info() ->
    gen_server:call(?SERVER, get_account_info).

%% Get connection status
get_connection_status() ->
    gen_server:call(?SERVER, get_connection_status).

%% Get pending orders
get_pending_orders() ->
    gen_server:call(?SERVER, get_pending_orders).

%% Get order confirmations
get_order_confirmations() ->
    gen_server:call(?SERVER, get_order_confirmations).

%% Wait for order confirmation with timeout
wait_for_order_confirmation(OrderId, TimeoutMs) ->
    gen_server:call(?SERVER, {wait_for_order_confirmation, OrderId, TimeoutMs}, TimeoutMs + 1000).

%% ============================================================================
%% gen_server Callbacks
%% ============================================================================

init({Host, Port, ClientId}) ->
    process_flag(trap_exit, true),
    
    %% Initialize ETS tables for market data
    init_ets_tables(),
    
    State = #state{
        connection = #ib_connection{client_id = ClientId}
    },
    case connect_to_ib(Host, Port, State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            io:format("Initial connection failed: ~p~n", [Reason]),
            %% Start with disconnected state and attempt reconnection
            Timer = erlang:send_after(?INITIAL_BACKOFF, self(), reconnect),
            {ok, State#state{reconnect_timer = Timer}}
    end.

handle_call({subscribe_market_data, Symbol, ReqId}, _From, State) ->
    case State#state.connection#ib_connection.connected of
        true ->
            case send_market_data_request(Symbol, ReqId, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        false ->
            {reply, {error, not_connected}, State}
    end;

handle_call({place_order, Symbol, Action, Quantity, OrderType}, _From, State) ->
    case State#state.connection#ib_connection.connected of
        true ->
            case send_order_request(Symbol, Action, Quantity, OrderType, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        false ->
            {reply, {error, not_connected}, State}
    end;

handle_call(get_account_info, _From, State) ->
    AccountInfo = State#state.connection#ib_connection.account_info,
    {reply, {ok, AccountInfo}, State};

handle_call({unsubscribe_market_data, ReqId}, _From, State) ->
    case State#state.connection#ib_connection.connected of
        true ->
            case send_cancel_market_data_request(ReqId, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        false ->
            {reply, {error, not_connected}, State}
    end;

handle_call({get_market_data, Symbol}, _From, State) ->
    case ets:lookup(?MARKET_TICKS_TABLE, Symbol) of
        [] ->
            {reply, {error, no_data}, State};
        [{Symbol, Tick}] ->
            {reply, {ok, Tick}, State}
    end;

handle_call({get_ohlc_data, Symbol, Resolution}, _From, State) ->
    case get_ohlc_from_buffer(Symbol, Resolution) of
        {ok, OHLCData} ->
            {reply, {ok, OHLCData}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(init_market_data_tables, _From, State) ->
    Result = init_ets_tables(),
    {reply, Result, State};

handle_call(cleanup_market_data_tables, _From, State) ->
    Result = cleanup_ets_tables(),
    {reply, Result, State};

handle_call(get_connection_status, _From, State) ->
    Connected = State#state.connection#ib_connection.connected,
    {reply, {ok, Connected}, State};

handle_call(get_pending_orders, _From, State) ->
    {reply, {ok, State#state.pending_orders}, State};

handle_call(get_order_confirmations, _From, State) ->
    {reply, {ok, State#state.order_confirmations}, State};

handle_call({wait_for_order_confirmation, OrderId, TimeoutMs}, From, State) ->
    %% Check if we already have confirmation for this order
    case lists:keyfind(OrderId, 1, State#state.order_confirmations) of
        {OrderId, Status, FillPrice, FillQuantity} ->
            {reply, {ok, {Status, FillPrice, FillQuantity}}, State};
        false ->
            %% Set up timeout and wait for confirmation
            Timer = erlang:send_after(TimeoutMs, self(), {order_timeout, OrderId, From}),
            %% Store the waiting request (simplified - in production would use more sophisticated tracking)
            {noreply, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    NewBuffer = <<(State#state.message_buffer)/binary, Data/binary>>,
    case process_messages(NewBuffer, State) of
        {ok, RemainingBuffer, NewState} ->
            {noreply, NewState#state{message_buffer = RemainingBuffer}};
        {error, Reason} ->
            io:format("Error processing messages: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    io:format("IB connection closed~n"),
    NewState = State#state{
        connection = State#state.connection#ib_connection{connected = false}
    },
    schedule_reconnect(NewState);

handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("IB connection error: ~p~n", [Reason]),
    NewState = State#state{
        connection = State#state.connection#ib_connection{connected = false}
    },
    schedule_reconnect(NewState);

handle_info(reconnect, State) ->
    case reconnect_to_ib(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _Reason} ->
            schedule_reconnect(State)
    end;

handle_info(heartbeat, State) ->
    case State#state.connection#ib_connection.connected of
        true ->
            %% Send heartbeat message
            send_heartbeat(State),
            Timer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
            {noreply, State#state{heartbeat_timer = Timer}};
        false ->
            {noreply, State}
    end;

handle_info({order_timeout, OrderId, From}, State) ->
    %% Handle order confirmation timeout
    io:format("Order ~p confirmation timeout~n", [OrderId]),
    gen_server:reply(From, {error, timeout}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.connection#ib_connection.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end,
    cancel_timer(State#state.reconnect_timer),
    cancel_timer(State#state.heartbeat_timer),
    
    %% Cleanup ETS tables
    cleanup_ets_tables(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% Connection Management
%% ============================================================================

connect_to_ib(Host, Port, State) ->
    io:format("Attempting to connect to IB at ~s:~p~n", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
        {ok, Socket} ->
            io:format("TCP connection established~n"),
            Connection = State#state.connection#ib_connection{
                socket = Socket,
                connected = true,
                connection_time = erlang:timestamp()
            },
            NewState = State#state{
                connection = Connection,
                reconnect_attempts = 0
            },
            case perform_handshake(NewState) of
                {ok, HandshakeState} ->
                    %% Start heartbeat timer
                    Timer = erlang:send_after(?HEARTBEAT_INTERVAL, self(), heartbeat),
                    FinalState = HandshakeState#state{heartbeat_timer = Timer},
                    io:format("IB connection established successfully~n"),
                    {ok, FinalState};
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to connect to IB: ~p~n", [Reason]),
            {error, Reason}
    end.

reconnect_to_ib(State) ->
    Attempts = State#state.reconnect_attempts + 1,
    if
        Attempts > ?MAX_RECONNECT_ATTEMPTS ->
            io:format("Max reconnection attempts reached~n"),
            {error, max_attempts_reached};
        true ->
            io:format("Reconnection attempt ~p/~p~n", [Attempts, ?MAX_RECONNECT_ATTEMPTS]),
            %% Get connection parameters from config
            Host = config:ib_host(),
            Port = config:ib_port(),
            NewState = State#state{reconnect_attempts = Attempts},
            case connect_to_ib(Host, Port, NewState) of
                {ok, ConnectedState} ->
                    {ok, ConnectedState};
                {error, _Reason} ->
                    {error, reconnect_failed}
            end
    end.

schedule_reconnect(State) ->
    cancel_timer(State#state.reconnect_timer),
    cancel_timer(State#state.heartbeat_timer),
    
    Attempts = State#state.reconnect_attempts,
    %% Exponential backoff: 1s, 2s, 4s, 8s, ..., max 60s
    Backoff = min(?INITIAL_BACKOFF * round(math:pow(2, Attempts)), 60000),
    
    io:format("Scheduling reconnect in ~p ms~n", [Backoff]),
    Timer = erlang:send_after(Backoff, self(), reconnect),
    
    NewState = State#state{
        reconnect_timer = Timer,
        heartbeat_timer = undefined,
        connection = State#state.connection#ib_connection{connected = false}
    },
    {noreply, NewState}.

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

%% ============================================================================
%% IB API Protocol Implementation
%% ============================================================================

perform_handshake(State) ->
    Socket = State#state.connection#ib_connection.socket,
    ClientId = State#state.connection#ib_connection.client_id,
    
    %% Send client version
    ClientVersionMsg = encode_int(?CLIENT_VERSION),
    case gen_tcp:send(Socket, ClientVersionMsg) of
        ok ->
            %% Wait for server version response
            receive
                {tcp, Socket, Data} ->
                    case decode_server_version(Data) of
                        {ok, ServerVersion} when ServerVersion >= ?MIN_SERVER_VER ->
                            %% Send connection string with client ID
                            ConnectMsg = encode_connect_message(ClientId),
                            case gen_tcp:send(Socket, ConnectMsg) of
                                ok ->
                                    Connection = State#state.connection#ib_connection{
                                        server_version = ServerVersion
                                    },
                                    {ok, State#state{connection = Connection}};
                                {error, Reason} ->
                                    {error, {send_connect_failed, Reason}}
                            end;
                        {ok, ServerVersion} ->
                            {error, {unsupported_server_version, ServerVersion}};
                        {error, Reason} ->
                            {error, {handshake_failed, Reason}}
                    end
            after ?TIMEOUT ->
                {error, handshake_timeout}
            end;
        {error, Reason} ->
            {error, {send_version_failed, Reason}}
    end.

decode_server_version(Data) ->
    try
        {ServerVersion, _Rest} = decode_int(Data),
        {ok, ServerVersion}
    catch
        _:_ -> {error, invalid_server_version}
    end.

encode_connect_message(ClientId) ->
    %% Simple connection message with client ID
    ClientIdStr = integer_to_list(ClientId),
    encode_string(ClientIdStr).

%% ============================================================================
%% Market Data Functions
%% ============================================================================

send_market_data_request(Symbol, ReqId, State) ->
    Socket = State#state.connection#ib_connection.socket,
    
    %% Build market data request message
    Message = [
        encode_int(?REQ_MKT_DATA),
        encode_int(1), %% Version
        encode_int(ReqId),
        encode_string(Symbol),
        encode_string("CASH"), %% Security type
        encode_string(""), %% Expiry
        encode_double(0.0), %% Strike
        encode_string(""), %% Right
        encode_string(""), %% Multiplier
        encode_string("IDEALPRO"), %% Exchange
        encode_string("USD"), %% Currency
        encode_string(""), %% Local symbol
        encode_string("") %% Generic tick list
    ],
    
    case gen_tcp:send(Socket, Message) of
        ok ->
            %% Add to subscriptions
            Subscriptions = State#state.connection#ib_connection.subscriptions,
            NewSubscriptions = [{ReqId, Symbol} | Subscriptions],
            Connection = State#state.connection#ib_connection{
                subscriptions = NewSubscriptions
            },
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {error, Reason}
    end.

send_cancel_market_data_request(ReqId, State) ->
    Socket = State#state.connection#ib_connection.socket,
    
    %% Build cancel market data request message
    Message = [
        encode_int(?CANCEL_MKT_DATA),
        encode_int(1), %% Version
        encode_int(ReqId)
    ],
    
    case gen_tcp:send(Socket, Message) of
        ok ->
            %% Remove from subscriptions
            Subscriptions = State#state.connection#ib_connection.subscriptions,
            NewSubscriptions = lists:keydelete(ReqId, 1, Subscriptions),
            Connection = State#state.connection#ib_connection{
                subscriptions = NewSubscriptions
            },
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {error, Reason}
    end.

%% ============================================================================
%% Order Management Functions
%% ============================================================================

send_order_request(Symbol, Action, Quantity, OrderType, State) ->
    Socket = State#state.connection#ib_connection.socket,
    OrderId = State#state.connection#ib_connection.next_order_id,
    
    %% Build enhanced order message for market orders
    Message = [
        encode_int(?PLACE_ORDER),
        encode_int(45), %% Version - using higher version for more fields
        encode_int(OrderId),
        encode_string(Symbol),
        encode_string("CASH"), %% Security type
        encode_string(""), %% Expiry
        encode_double(0.0), %% Strike
        encode_string(""), %% Right
        encode_string(""), %% Multiplier
        encode_string("IDEALPRO"), %% Exchange
        encode_string("USD"), %% Currency
        encode_string(""), %% Local symbol
        encode_string(Action), %% BUY/SELL
        encode_int(Quantity),
        encode_string(OrderType), %% MKT/LMT
        encode_double(0.0), %% Limit price
        encode_double(0.0), %% Aux price
        encode_int(0), %% Time in force (0=DAY)
        encode_string(""), %% OCA group
        encode_string(""), %% Account
        encode_string(""), %% Open/close
        encode_int(0), %% Origin
        encode_string(""), %% Order ref
        encode_int(1), %% Transmit
        encode_int(0), %% Parent id
        encode_int(0), %% Block order
        encode_int(0), %% Sweep to fill
        encode_int(0), %% Display size
        encode_int(0), %% Trigger method
        encode_int(0), %% Outside RTH
        encode_int(0) %% Hidden
    ],
    
    case gen_tcp:send(Socket, Message) of
        ok ->
            %% Track the pending order
            PendingOrder = {OrderId, Symbol, Action, Quantity, erlang:timestamp()},
            NewPendingOrders = [PendingOrder | State#state.pending_orders],
            
            Connection = State#state.connection#ib_connection{
                next_order_id = OrderId + 1
            },
            
            io:format("Placed order ~p: ~s ~p ~s~n", [OrderId, Action, Quantity, Symbol]),
            {ok, State#state{
                connection = Connection,
                pending_orders = NewPendingOrders
            }};
        {error, Reason} ->
            io:format("Failed to send order: ~p~n", [Reason]),
            {error, Reason}
    end.

%% ============================================================================
%% Message Processing
%% ============================================================================

process_messages(Buffer, State) ->
    case extract_message(Buffer) of
        {ok, Message, RemainingBuffer} ->
            case handle_ib_message(Message, State) of
                {ok, NewState} ->
                    process_messages(RemainingBuffer, NewState);
                {error, Reason} ->
                    {error, Reason}
            end;
        {incomplete, _} ->
            {ok, Buffer, State};
        {error, Reason} ->
            {error, Reason}
    end.

extract_message(<<Len:32, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Message:Len/binary, Remaining/binary>> = Rest,
    {ok, Message, Remaining};
extract_message(_Buffer) ->
    {incomplete, need_more_data}.

handle_ib_message(Message, State) ->
    try
        {MsgType, Rest} = decode_int(Message),
        case MsgType of
            ?TICK_PRICE ->
                handle_tick_price(Rest, State);
            ?TICK_SIZE ->
                handle_tick_size(Rest, State);
            ?ORDER_STATUS ->
                handle_order_status(Rest, State);
            ?EXECUTION_DATA ->
                handle_execution_data(Rest, State);
            ?ERR_MSG ->
                handle_error_message(Rest, State);
            ?NEXT_VALID_ID ->
                handle_next_valid_id(Rest, State);
            ?ACCT_VALUE ->
                handle_account_value(Rest, State);
            _ ->
                io:format("Unhandled message type: ~p~n", [MsgType]),
                {ok, State}
        end
    catch
        _:Error ->
            io:format("Error handling IB message: ~p~n", [Error]),
            {ok, State}
    end.

handle_tick_price(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {ReqId, Rest2} = decode_int(Rest1),
        {TickType, Rest3} = decode_int(Rest2),
        {Price, _Rest4} = decode_double(Rest3),
        
        %% Find symbol for this request ID
        Subscriptions = State#state.connection#ib_connection.subscriptions,
        case lists:keyfind(ReqId, 1, Subscriptions) of
            {ReqId, Symbol} ->
                %% Get current tick data or create new one
                CurrentTick = case ets:lookup(?MARKET_TICKS_TABLE, Symbol) of
                    [] ->
                        #market_tick{
                            symbol = Symbol,
                            timestamp = erlang:timestamp(),
                            bid = undefined,
                            ask = undefined,
                            last = undefined,
                            volume = 0
                        };
                    [{Symbol, ExistingTick}] ->
                        ExistingTick#market_tick{timestamp = erlang:timestamp()}
                end,
                
                %% Update tick with new price data
                UpdatedTick = case TickType of
                    1 -> CurrentTick#market_tick{bid = Price}; %% Bid price
                    2 -> CurrentTick#market_tick{ask = Price}; %% Ask price
                    4 -> CurrentTick#market_tick{last = Price}; %% Last price
                    _ -> CurrentTick
                end,
                
                %% Store updated tick in ETS
                ets:insert(?MARKET_TICKS_TABLE, {Symbol, UpdatedTick}),
                
                %% Update OHLC data if we have a last price
                case TickType of
                    4 -> %% Last price - use for OHLC aggregation
                        update_ohlc_data(Symbol, Price, erlang:timestamp());
                    _ ->
                        ok
                end,
                
                %% Add to price buffer for sensor compatibility
                add_to_price_buffer(Symbol, UpdatedTick),
                
                %% Notify any listening processes
                notify_market_data(UpdatedTick),
                {ok, State};
            false ->
                io:format("Received tick for unknown request ID: ~p~n", [ReqId]),
                {ok, State}
        end
    catch
        _:Error ->
            io:format("Error processing tick price: ~p~n", [Error]),
            {ok, State}
    end.

handle_tick_size(Data, State) ->
    %% Handle volume data
    try
        {_Version, Rest1} = decode_int(Data),
        {ReqId, Rest2} = decode_int(Rest1),
        {TickType, Rest3} = decode_int(Rest2),
        {Size, _Rest4} = decode_int(Rest3),
        
        %% Find symbol for this request ID
        Subscriptions = State#state.connection#ib_connection.subscriptions,
        case lists:keyfind(ReqId, 1, Subscriptions) of
            {ReqId, Symbol} ->
                case TickType of
                    5 -> %% Last size (volume)
                        %% Update volume in current tick
                        case ets:lookup(?MARKET_TICKS_TABLE, Symbol) of
                            [{Symbol, CurrentTick}] ->
                                UpdatedTick = CurrentTick#market_tick{
                                    volume = Size,
                                    timestamp = erlang:timestamp()
                                },
                                ets:insert(?MARKET_TICKS_TABLE, {Symbol, UpdatedTick});
                            [] ->
                                %% Create new tick with volume
                                NewTick = #market_tick{
                                    symbol = Symbol,
                                    timestamp = erlang:timestamp(),
                                    volume = Size
                                },
                                ets:insert(?MARKET_TICKS_TABLE, {Symbol, NewTick})
                        end;
                    _ ->
                        ok %% Ignore other size types for now
                end,
                {ok, State};
            false ->
                {ok, State}
        end
    catch
        _:Error ->
            io:format("Error processing tick size: ~p~n", [Error]),
            {ok, State}
    end.

handle_error_message(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {ErrorCode, Rest2} = decode_int(Rest1),
        {ErrorMsg, _Rest3} = decode_string(Rest2),
        
        io:format("IB Error ~p: ~s~n", [ErrorCode, ErrorMsg]),
        
        %% Handle specific error codes
        case ErrorCode of
            502 -> %% Couldn't connect to TWS
                {error, connection_failed};
            504 -> %% Not connected
                {error, not_connected};
            _ ->
                {ok, State}
        end
    catch
        _:Error ->
            io:format("Error processing error message: ~p~n", [Error]),
            {ok, State}
    end.

handle_next_valid_id(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {NextId, _Rest2} = decode_int(Rest1),
        
        Connection = State#state.connection#ib_connection{
            next_order_id = NextId
        },
        {ok, State#state{connection = Connection}}
    catch
        _:Error ->
            io:format("Error processing next valid ID: ~p~n", [Error]),
            {ok, State}
    end.

handle_account_value(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {Key, Rest2} = decode_string(Rest1),
        {Value, Rest3} = decode_string(Rest2),
        {Currency, _Rest4} = decode_string(Rest3),
        
        %% Store account information
        AccountInfo = case State#state.connection#ib_connection.account_info of
            undefined -> [];
            Info -> Info
        end,
        
        NewAccountInfo = [{Key, Value, Currency} | AccountInfo],
        Connection = State#state.connection#ib_connection{
            account_info = NewAccountInfo
        },
        {ok, State#state{connection = Connection}}
    catch
        _:Error ->
            io:format("Error processing account value: ~p~n", [Error]),
            {ok, State}
    end.

handle_order_status(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {OrderId, Rest2} = decode_int(Rest1),
        {Status, Rest3} = decode_string(Rest2),
        {Filled, Rest4} = decode_int(Rest3),
        {Remaining, Rest5} = decode_int(Rest4),
        {AvgFillPrice, Rest6} = decode_double(Rest5),
        {PermId, Rest7} = decode_int(Rest6),
        {ParentId, Rest8} = decode_int(Rest7),
        {LastFillPrice, Rest9} = decode_double(Rest8),
        {ClientId, Rest10} = decode_int(Rest9),
        {WhyHeld, _Rest11} = decode_string(Rest10),
        
        io:format("Order Status - ID: ~p, Status: ~s, Filled: ~p, Remaining: ~p, AvgPrice: ~p~n", 
                 [OrderId, Status, Filled, Remaining, AvgFillPrice]),
        
        %% Update order confirmation tracking
        OrderConfirmation = {OrderId, Status, AvgFillPrice, Filled},
        NewConfirmations = [OrderConfirmation | State#state.order_confirmations],
        
        %% Remove from pending orders if filled or cancelled
        NewPendingOrders = case Status of
            "Filled" ->
                lists:keydelete(OrderId, 1, State#state.pending_orders);
            "Cancelled" ->
                lists:keydelete(OrderId, 1, State#state.pending_orders);
            _ ->
                State#state.pending_orders
        end,
        
        {ok, State#state{
            pending_orders = NewPendingOrders,
            order_confirmations = NewConfirmations
        }}
    catch
        _:Error ->
            io:format("Error processing order status: ~p~n", [Error]),
            {ok, State}
    end.

handle_execution_data(Data, State) ->
    try
        {_Version, Rest1} = decode_int(Data),
        {ReqId, Rest2} = decode_int(Rest1),
        {OrderId, Rest3} = decode_int(Rest2),
        {Symbol, Rest4} = decode_string(Rest3),
        {SecType, Rest5} = decode_string(Rest4),
        {Expiry, Rest6} = decode_string(Rest5),
        {Strike, Rest7} = decode_double(Rest6),
        {Right, Rest8} = decode_string(Rest7),
        {Multiplier, Rest9} = decode_string(Rest8),
        {Exchange, Rest10} = decode_string(Rest9),
        {Currency, Rest11} = decode_string(Rest10),
        {LocalSymbol, Rest12} = decode_string(Rest11),
        {ExecId, Rest13} = decode_string(Rest12),
        {Time, Rest14} = decode_string(Rest13),
        {Account, Rest15} = decode_string(Rest14),
        {ExecExchange, Rest16} = decode_string(Rest15),
        {Side, Rest17} = decode_string(Rest16),
        {Shares, Rest18} = decode_int(Rest17),
        {Price, Rest19} = decode_double(Rest18),
        {PermId, Rest20} = decode_int(Rest19),
        {ClientId, Rest21} = decode_int(Rest20),
        {Liquidation, Rest22} = decode_int(Rest21),
        {CumQty, Rest23} = decode_int(Rest22),
        {AvgPrice, _Rest24} = decode_double(Rest23),
        
        io:format("Execution Report - OrderID: ~p, Symbol: ~s, Side: ~s, Shares: ~p, Price: ~p~n", 
                 [OrderId, Symbol, Side, Shares, Price]),
        
        %% Store execution details
        ExecutionData = {OrderId, Symbol, Side, Shares, Price, Time},
        
        %% Notify any listening processes about the execution
        notify_execution(ExecutionData),
        
        {ok, State}
    catch
        _:Error ->
            io:format("Error processing execution data: ~p~n", [Error]),
            {ok, State}
    end.

%% ============================================================================
%% Message Encoding/Decoding Functions
%% ============================================================================

encode_int(Int) ->
    <<Int:32>>.

encode_double(Double) ->
    <<Double:64/float>>.

encode_string(String) ->
    Bin = list_to_binary(String),
    Len = byte_size(Bin),
    <<Len:32, Bin/binary>>.

decode_int(<<Int:32, Rest/binary>>) ->
    {Int, Rest}.

decode_double(<<Double:64/float, Rest/binary>>) ->
    {Double, Rest}.

decode_string(<<Len:32, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<String:Len/binary, Remaining/binary>> = Rest,
    {binary_to_list(String), Remaining}.

%% ============================================================================
%% ETS Table Management
%% ============================================================================

init_ets_tables() ->
    try
        %% Create market ticks table
        case ets:info(?MARKET_TICKS_TABLE) of
            undefined ->
                ets:new(?MARKET_TICKS_TABLE, [set, public, named_table]);
            _ ->
                ets:delete_all_objects(?MARKET_TICKS_TABLE)
        end,
        
        %% Create OHLC data table
        case ets:info(?OHLC_DATA_TABLE) of
            undefined ->
                ets:new(?OHLC_DATA_TABLE, [ordered_set, public, named_table]);
            _ ->
                ets:delete_all_objects(?OHLC_DATA_TABLE)
        end,
        
        %% Create price buffer table
        case ets:info(?PRICE_BUFFER_TABLE) of
            undefined ->
                ets:new(?PRICE_BUFFER_TABLE, [ordered_set, public, named_table]);
            _ ->
                ets:delete_all_objects(?PRICE_BUFFER_TABLE)
        end,
        
        io:format("Market data ETS tables initialized~n"),
        ok
    catch
        _:Error ->
            io:format("Error initializing ETS tables: ~p~n", [Error]),
            {error, Error}
    end.

cleanup_ets_tables() ->
    try
        Tables = [?MARKET_TICKS_TABLE, ?OHLC_DATA_TABLE, ?PRICE_BUFFER_TABLE],
        [begin
            case ets:info(Table) of
                undefined -> ok;
                _ -> ets:delete(Table)
            end
         end || Table <- Tables],
        io:format("Market data ETS tables cleaned up~n"),
        ok
    catch
        _:Error ->
            io:format("Error cleaning up ETS tables: ~p~n", [Error]),
            {error, Error}
    end.

%% ============================================================================
%% OHLC Data Processing
%% ============================================================================

update_ohlc_data(Symbol, Price, Timestamp) ->
    try
        %% Create time window key (minute-based)
        {{Year, Month, Day}, {Hour, Minute, _Second}} = calendar:now_to_datetime(Timestamp),
        WindowKey = {Symbol, {Year, Month, Day, Hour, Minute}},
        
        case ets:lookup(?OHLC_DATA_TABLE, WindowKey) of
            [] ->
                %% Create new OHLC record
                NewOHLC = #live_ohlc{
                    symbol = Symbol,
                    timestamp = Timestamp,
                    open = Price,
                    high = Price,
                    low = Price,
                    close = Price,
                    volume = 0,
                    tick_count = 1
                },
                ets:insert(?OHLC_DATA_TABLE, {WindowKey, NewOHLC});
            [{WindowKey, CurrentOHLC}] ->
                %% Update existing OHLC record
                UpdatedOHLC = CurrentOHLC#live_ohlc{
                    high = max(CurrentOHLC#live_ohlc.high, Price),
                    low = min(CurrentOHLC#live_ohlc.low, Price),
                    close = Price,
                    timestamp = Timestamp,
                    tick_count = CurrentOHLC#live_ohlc.tick_count + 1
                },
                ets:insert(?OHLC_DATA_TABLE, {WindowKey, UpdatedOHLC})
        end,
        ok
    catch
        _:Error ->
            io:format("Error updating OHLC data: ~p~n", [Error]),
            error
    end.

get_ohlc_from_buffer(Symbol, Resolution) ->
    try
        %% Get OHLC data for the specified resolution
        Pattern = {{Symbol, '_'}, '_'},
        OHLCList = ets:match_object(?OHLC_DATA_TABLE, Pattern),
        
        %% Sort by timestamp and take the most recent Resolution entries
        SortedOHLC = lists:sort(fun({_, A}, {_, B}) -> 
            A#live_ohlc.timestamp >= B#live_ohlc.timestamp 
        end, OHLCList),
        
        RecentOHLC = lists:sublist(SortedOHLC, Resolution),
        
        %% Convert to format compatible with existing sensors
        TechnicalRecords = [convert_ohlc_to_technical(OHLC) || {_, OHLC} <- RecentOHLC],
        
        {ok, lists:reverse(TechnicalRecords)}
    catch
        _:Error ->
            io:format("Error getting OHLC from buffer: ~p~n", [Error]),
            {error, Error}
    end.

convert_ohlc_to_technical(LiveOHLC) ->
    %% Convert live_ohlc record to technical record format expected by sensors
    %% Using the same format as defined in fx.erl
    {LiveOHLC#live_ohlc.open, LiveOHLC#live_ohlc.close, 
     LiveOHLC#live_ohlc.high, LiveOHLC#live_ohlc.low}.

add_to_price_buffer(Symbol, Tick) ->
    try
        %% Add tick to price buffer for sensor compatibility
        BufferKey = {Symbol, erlang:timestamp()},
        ets:insert(?PRICE_BUFFER_TABLE, {BufferKey, Tick}),
        
        %% Cleanup old entries to prevent memory bloat
        cleanup_old_buffer_entries(Symbol),
        ok
    catch
        _:Error ->
            io:format("Error adding to price buffer: ~p~n", [Error]),
            error
    end.

cleanup_old_buffer_entries(Symbol) ->
    try
        %% Get all entries for this symbol
        Pattern = {{Symbol, '_'}, '_'},
        AllEntries = ets:match_object(?PRICE_BUFFER_TABLE, Pattern),
        
        %% If we have too many entries, remove the oldest ones
        case length(AllEntries) > ?MAX_PRICE_BUFFER_SIZE of
            true ->
                %% Sort by timestamp (oldest first)
                SortedEntries = lists:sort(fun({{_, T1}, _}, {{_, T2}, _}) -> 
                    T1 =< T2 
                end, AllEntries),
                
                %% Remove oldest entries
                EntriesToRemove = length(AllEntries) - ?MAX_PRICE_BUFFER_SIZE,
                OldestEntries = lists:sublist(SortedEntries, EntriesToRemove),
                
                [ets:delete(?PRICE_BUFFER_TABLE, Key) || {{Key, _}, _} <- OldestEntries];
            false ->
                ok
        end
    catch
        _:Error ->
            io:format("Error cleaning up buffer entries: ~p~n", [Error])
    end.

%% ============================================================================
%% Data Translation Functions
%% ============================================================================

%% Convert IB tick data to internal format for sensors
translate_tick_to_sensor_format(Tick, SensorType) ->
    case SensorType of
        fx_PLI ->
            %% For PLI sensors, return close price
            case Tick#market_tick.last of
                undefined ->
                    %% Use mid price if last is not available
                    case {Tick#market_tick.bid, Tick#market_tick.ask} of
                        {undefined, undefined} -> 0.0;
                        {Bid, undefined} -> Bid;
                        {undefined, Ask} -> Ask;
                        {Bid, Ask} -> (Bid + Ask) / 2.0
                    end;
                Last -> Last
            end;
        fx_PCI ->
            %% For PCI sensors, return OHLC data structure
            Price = case Tick#market_tick.last of
                undefined ->
                    case {Tick#market_tick.bid, Tick#market_tick.ask} of
                        {undefined, undefined} -> 0.0;
                        {Bid, undefined} -> Bid;
                        {undefined, Ask} -> Ask;
                        {Bid, Ask} -> (Bid + Ask) / 2.0
                    end;
                Last -> Last
            end,
            {Price, Price, Price, Price}; %% {Open, Close, High, Low}
        _ ->
            Tick
    end.

%% Get price list for sensor compatibility (similar to fx:fx_GetPriceList)
get_price_list_for_sensor(Symbol, Resolution) ->
    try
        %% Get recent price data from buffer
        Pattern = {{Symbol, '_'}, '_'},
        AllTicks = ets:match_object(?PRICE_BUFFER_TABLE, Pattern),
        
        %% Sort by timestamp (most recent first)
        SortedTicks = lists:sort(fun({{_, T1}, _}, {{_, T2}, _}) -> 
            T1 >= T2 
        end, AllTicks),
        
        %% Take the most recent Resolution entries
        RecentTicks = lists:sublist(SortedTicks, Resolution),
        
        %% Convert to price list format expected by sensors
        PriceList = [begin
            Tick = TickData,
            Price = case Tick#market_tick.last of
                undefined ->
                    case {Tick#market_tick.bid, Tick#market_tick.ask} of
                        {undefined, undefined} -> 0.0;
                        {Bid, undefined} -> Bid;
                        {undefined, Ask} -> Ask;
                        {Bid, Ask} -> (Bid + Ask) / 2.0
                    end;
                Last -> Last
            end,
            {Price, Price, Price, Price} %% {Open, Close, High, Low}
        end || {_, TickData} <- RecentTicks],
        
        {ok, lists:reverse(PriceList)}
    catch
        _:Error ->
            io:format("Error getting price list for sensor: ~p~n", [Error]),
            {error, Error}
    end.

%% ============================================================================
%% Utility Functions
%% ============================================================================

send_heartbeat(State) ->
    %% Send a simple request to keep connection alive
    Socket = State#state.connection#ib_connection.socket,
    HeartbeatMsg = encode_int(?REQ_IDS),
    gen_tcp:send(Socket, HeartbeatMsg).

notify_market_data(Tick) ->
    %% Notify any registered processes about new market data
    %% This could be enhanced to use a proper pub/sub mechanism
    case whereis(live_scape) of
        undefined -> ok;
        Pid -> Pid ! {market_data, Tick}
    end.

notify_execution(ExecutionData) ->
    %% Notify any registered processes about order execution
    case whereis(live_scape) of
        undefined -> ok;
        Pid -> Pid ! {execution_data, ExecutionData}
    end.

%% Sync function for development
sync() ->
    make:all([load]).