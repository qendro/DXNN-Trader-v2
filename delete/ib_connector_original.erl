%% Interactive Brokers API Connector Module
%% Implements TWS API communication using native Erlang gen_tcp sockets
%% Handles connection management, market data, and order execution

-module(ib_connector).
-compile(export_all).
-include("records.hrl").
-include("ib_config.hrl").
-behaviour(gen_server).

%% Records are defined in records.hrl

%% API Functions
-export([
    test_connectivity/0,
    test_handshake_detailed/0,
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
-define(TIMEOUT, 15000).  % Increased timeout for Docker networking
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

%% IB API Message Types (now using ib_config.hrl)
%% Legacy defines for backward compatibility
-define(CLIENT_VERSION, ?IB_CLIENT_VERSION).
-define(MIN_SERVER_VER, ?IB_MIN_SERVER_VER).

%% IB API Handshake Constants
-define(IB_API_VERSION, "9.76.1").
-define(IB_API_DATE, "20170803").

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

%% Test basic connectivity to IB TWS/Gateway
test_connectivity() ->
    Host = config:ib_host(),
    Port = config:ib_port(),
    
    io:format("Testing connectivity to ~s:~p~n", [Host, Port]),
    
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("✓ Basic connectivity test passed~n"),
            ok;
        {error, Reason} ->
            io:format("✗ Connectivity test failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Test IB handshake with detailed logging using real handshake protocol
test_handshake_detailed() ->
    test_handshake_detailed(15000). % Default 15 second timeout

test_handshake_detailed(TimeoutMs) ->
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    
    io:format("Testing detailed handshake with ~s:~p (Client ID: ~p)~n", [Host, Port, ClientId]),
    
    %% Use the diagnostic version for detailed hex logging
    ib_diag:test_handshake([
        {host, Host},
        {port, Port}, 
        {client_id, ClientId},
        {timeout, TimeoutMs}
    ]).

%% Start link function for supervisor integration
start_link() ->
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    start_connection(Host, Port, ClientId).

%% Start connection to Interactive Brokers TWS/Gateway
start_connection(Host, Port, ClientId) ->
    io:format("Starting IB connector with Host: ~p, Port: ~p, ClientId: ~p~n", 
              [Host, Port, ClientId]),
    case gen_server:start_link({local, ?SERVER}, ?MODULE, 
                              {Host, Port, ClientId}, []) of
        {ok, Pid} ->
            io:format("✓ IB connector started successfully~n"),
            {ok, Pid};
        {error, {connection_failed, Reason}} ->
            io:format("✗ IB connector failed to start due to connection failure: ~p~n", [Reason]),
            {error, {connection_failed, Reason}};
        {error, Reason} ->
            io:format("✗ Failed to start IB connector: ~p~n", [Reason]),
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
            %% Cleanup ETS tables since we're failing to start
            cleanup_ets_tables(),
            %% Return stop to fail fast instead of silent failure
            {stop, {connection_failed, Reason}}
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
            _Timer = erlang:send_after(TimeoutMs, self(), {order_timeout, OrderId, From}),
            %% Store the waiting request (simplified - in production would use more sophisticated tracking)
            {noreply, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
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

handle_info({critical_reconnect}, State) ->
    io:format("Attempting critical reconnection~n"),
    case reconnect_to_ib(State) of
        {ok, NewState} ->
            io:format("Critical reconnection successful~n"),
            %% Notify recovery
            notify_connection_recovery(),
            {noreply, NewState};
        {error, Reason} ->
            io:format("Critical reconnection failed: ~p~n", [Reason]),
            %% Use more aggressive retry for critical situations
            schedule_critical_reconnect(State)
    end;

handle_info(refresh_market_data, State) ->
    io:format("Refreshing market data subscriptions~n"),
    case State#state.connection#ib_connection.connected of
        true ->
            %% Resubscribe to all market data
            NewState = resubscribe_market_data(State),
            {noreply, NewState};
        false ->
            io:format("Cannot refresh market data - not connected~n"),
            {noreply, State}
    end;

handle_info(resubscribe_all_market_data, State) ->
    io:format("Resubscribing to all market data~n"),
    case State#state.connection#ib_connection.connected of
        true ->
            NewState = resubscribe_market_data(State),
            {noreply, NewState};
        false ->
            %% Schedule retry when connected
            erlang:send_after(5000, self(), resubscribe_all_market_data),
            {noreply, State}
    end;

handle_info({retry_order, OrderId}, State) ->
    io:format("Retrying order ~p~n", [OrderId]),
    %% Find original order details and retry
    case find_pending_order(OrderId, State) of
        {ok, {OrderId, Symbol, Action, Quantity, _Timestamp}} ->
            case send_order_request(Symbol, Action, Quantity, "MKT", State) of
                {ok, NewState} ->
                    io:format("Order ~p retry successful~n", [OrderId]),
                    {noreply, NewState};
                {error, Reason} ->
                    io:format("Order ~p retry failed: ~p~n", [OrderId, Reason]),
                    %% Mark as permanently failed after retry
                    FinalState = mark_order_permanently_failed(OrderId, Reason, State),
                    {noreply, FinalState}
            end;
        error ->
            io:format("Cannot find order ~p for retry~n", [OrderId]),
            {noreply, State}
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
    ConnectTimeout = application:get_env(dxnn, ib_connect_timeout, ?IB_CONNECT_TIMEOUT),
    case gen_tcp:connect(Host, Port, ?IB_TCP_OPTS, ConnectTimeout) of
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
%% Enhanced Recovery Utility Functions
%% ============================================================================

%% Notify other processes of connection recovery
notify_connection_recovery() ->
    io:format("Notifying processes of connection recovery~n"),
    
    %% Notify live_trader
    case whereis(live_trader) of
        undefined -> ok;
        Pid -> Pid ! {ib_connection_recovered, erlang:timestamp()}
    end,
    
    %% Notify live_scape
    case whereis(live_scape) of
        undefined -> ok;
        ScapePid -> ScapePid ! {ib_connection_recovered, erlang:timestamp()}
    end.

%% Find pending order by ID
find_pending_order(OrderId, State) ->
    PendingOrders = State#state.pending_orders,
    case lists:keyfind(OrderId, 1, PendingOrders) of
        false -> error;
        Order -> {ok, Order}
    end.

%% Enhanced connection health monitoring
monitor_connection_health(State) ->
    case State#state.connection#ib_connection.connected of
        true ->
            %% Check if we're receiving data
            LastHeartbeat = erlang:timestamp(),
            %% Store heartbeat timestamp for monitoring
            Connection = State#state.connection#ib_connection{
                connection_time = LastHeartbeat
            },
            State#state{connection = Connection};
        false ->
            %% Connection is down, ensure recovery is scheduled
            case State#state.reconnect_timer of
                undefined ->
                    schedule_reconnect(State);
                _ ->
                    State
            end
    end.

%% Detect market data interruption
detect_market_data_interruption() ->
    %% Check if we have recent market data
    case ets:info(?MARKET_TICKS_TABLE) of
        undefined -> 
            {interrupted, no_table};
        _ ->
            %% Check timestamp of most recent data
            case ets:first(?MARKET_TICKS_TABLE) of
                '$end_of_table' ->
                    {interrupted, no_data};
                FirstKey ->
                    case ets:lookup(?MARKET_TICKS_TABLE, FirstKey) of
                        [{_, Tick}] ->
                            TimeDiff = timer:now_diff(erlang:timestamp(), Tick#market_tick.timestamp),
                            if
                                TimeDiff > 60000000 -> % 60 seconds
                                    {interrupted, stale_data};
                                true ->
                                    {ok, recent_data}
                            end;
                        [] ->
                            {interrupted, no_data}
                    end
            end
    end.

%% Circuit breaker pattern for error handling (commented out for now)
%% -record(circuit_breaker, {
%%     failure_count = 0,
%%     last_failure_time,
%%     state = closed,  % closed, open, half_open
%%     failure_threshold = 5,
%%     timeout = 30000  % 30 seconds
%% }).

%% Check circuit breaker state
check_circuit_breaker(Operation, State) ->
    %% Simple circuit breaker implementation
    %% In production, would be more sophisticated
    case get_circuit_breaker_state(Operation) of
        closed ->
            {allow, State};
        open ->
            case should_attempt_reset(Operation) of
                true ->
                    {allow_test, State};
                false ->
                    {deny, State}
            end;
        half_open ->
            {allow_test, State}
    end.

%% Get circuit breaker state (simplified)
get_circuit_breaker_state(_Operation) ->
    %% For now, always allow - in production would track failures
    closed.

%% Check if circuit breaker should attempt reset
should_attempt_reset(_Operation) ->
    %% For now, always attempt - in production would check timeout
    true.

%% ============================================================================
%% IB API Protocol Implementation
%% ============================================================================

perform_handshake(State) ->
    Socket = State#state.connection#ib_connection.socket,
    ClientId = State#state.connection#ib_connection.client_id,
    
    case perform_ib_handshake(Socket, ClientId) of
        {ok, ServerVersion, ConnTime, Remainder} ->
            %% Create capability map based on server version
            Capabilities = create_capability_map(ServerVersion),
            
            Connection = State#state.connection#ib_connection{
                server_version = ServerVersion
            },
            NewState = State#state{
                connection = Connection,
                message_buffer = Remainder
            },
            io:format("✓ IB handshake completed successfully~n"),
            io:format("  Server version: ~p~n", [ServerVersion]),
            io:format("  Connection time: ~s~n", [ConnTime]),
            io:format("  Capabilities: ~p~n", [maps:keys(Capabilities)]),
            {ok, NewState};
        {error, Reason} ->
            io:format("✗ IB handshake failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Create capability map based on server version for feature gating
create_capability_map(ServerVersion) ->
    #{
        pnl => ServerVersion >= ?IB_SERVER_VER_PNL,
        tick_by_tick => ServerVersion >= ?IB_SERVER_VER_TICK_BY_TICK,
        market_depth => ServerVersion >= ?IB_SERVER_VER_MARKET_DEPTH
    }.

%% Correct IB API handshake implementation following TWS protocol
perform_ib_handshake(Socket, ClientId) ->
    ClientVersion = get_client_version(),
    ClientDate = get_client_date(),
    perform_ib_handshake(Socket, ClientId, ClientVersion, ClientDate, ?IB_HANDSHAKE_TIMEOUT).

perform_ib_handshake(Socket, ClientId, ClientVersion, ClientDate, TimeoutMs) ->
    %% Step 1: Send "API\0" prefix
    io:format("→ Sending API prefix~n"),
    case gen_tcp:send(Socket, <<"API", 0>>) of
        ok ->
            %% Step 2: Send "v<clientVersion>..<date>\0"
            VMsg = <<"v", (ib_proto:i2b(ClientVersion))/binary, "..", ClientDate/binary, 0>>,
            io:format("→ Sending version message: v~p..~s~n", [ClientVersion, ClientDate]),
            
            case gen_tcp:send(Socket, VMsg) of
                ok ->
                    %% Step 3: Read server greeting (serverVersion\0connectionTime\0)
                    inet:setopts(Socket, [{active, once}]),
                    receive
                        {tcp, Socket, Data} ->
                            io:format("← Received server greeting: ~p~n", [Data]),
                            case parse_server_greeting(Data) of
                                {ok, ServerVersion, ConnTime, Remainder} ->
                                    io:format("✓ Server version: ~p, Connection time: ~s~n", 
                                             [ServerVersion, ConnTime]),
                                    
                                    %% Validate server version
                                    if ServerVersion >= ?MIN_SERVER_VER ->
                                        %% Step 4: Send ClientId as C-string
                                        ClientIdMsg = ib_proto:z(ib_proto:i2b(ClientId)),
                                        io:format("→ Sending client ID: ~p~n", [ClientId]),
                                        
                                        case gen_tcp:send(Socket, ClientIdMsg) of
                                            ok ->
                                                io:format("✓ Handshake completed successfully~n"),
                                                {ok, ServerVersion, ConnTime, Remainder};
                                            {error, Reason} ->
                                                {error, {send_client_id_failed, Reason}}
                                        end;
                                    true ->
                                        {error, {unsupported_server_version, ServerVersion, ?MIN_SERVER_VER}}
                                    end;
                                {error, Reason} ->
                                    {error, {parse_server_greeting_failed, Reason}}
                            end;
                        {tcp_closed, Socket} ->
                            {error, connection_closed_during_handshake};
                        {tcp_error, Socket, Reason} ->
                            {error, {tcp_error_during_handshake, Reason}}
                    after TimeoutMs ->
                        {error, handshake_timeout}
                    end;
                {error, Reason} ->
                    {error, {send_version_message_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {send_api_prefix_failed, Reason}}
    end.

%% Parse server greeting: "<serverVersion>\0<connectionTime>\0"
parse_server_greeting(Bin) ->
    case ib_proto:read_cstring(Bin) of
        {ok, ServerVerBin, Rest1} ->
            case ib_proto:read_cstring(Rest1) of
                {ok, ConnTimeBin, Rest2} ->
                    try
                        ServerVersion = binary_to_integer(ServerVerBin),
                        {ok, ServerVersion, ConnTimeBin, Rest2}
                    catch
                        _:_ -> {error, {invalid_server_version, ServerVerBin}}
                    end;
                {error, Reason} -> 
                    {error, {bad_connection_time, Reason}}
            end;
        {error, Reason} -> 
            {error, {bad_server_version, Reason}}
    end.

%% Get runtime configurable client version
get_client_version() ->
    application:get_env(dxnn, ib_client_version, ?IB_CLIENT_VERSION).

%% Get runtime configurable client date
get_client_date() ->
    application:get_env(dxnn, ib_client_date, ?IB_CLIENT_DATE).

%% Test version of handshake for diagnostics
perform_handshake_test(Socket, ClientId, TimeoutMs) ->
    ClientVersion = get_client_version(),
    ClientDate = get_client_date(),
    perform_ib_handshake(Socket, ClientId, ClientVersion, ClientDate, TimeoutMs).

%% Legacy functions removed - now using ib_proto module for protocol handling

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
%% Enhanced Error Handling and Recovery
%% ============================================================================

%% Categorize IB API errors for appropriate handling
categorize_error(ErrorCode) ->
    case ErrorCode of
        %% Critical connection errors requiring immediate action
        502 -> {critical, connection_lost};     % Couldn't connect to TWS
        504 -> {critical, connection_lost};     % Not connected
        1100 -> {critical, connection_lost};    % Connectivity between IB and TWS lost
        1101 -> {critical, connection_lost};    % Connectivity between IB and TWS restored (but was lost)
        1102 -> {critical, connection_lost};    % Connectivity between IB and TWS restored (data lost)
        
        %% Market data related errors
        162 -> {recoverable, market_data_issue}; % Historical market data service error
        200 -> {recoverable, market_data_issue}; % No security definition found
        354 -> {recoverable, market_data_issue}; % Requested market data is not subscribed
        10167 -> {recoverable, market_data_issue}; % Requested market data is not subscribed
        
        %% Order execution errors
        103 -> {recoverable, order_issue};      % Duplicate order id
        104 -> {recoverable, order_issue};      % Cannot modify a filled order
        105 -> {recoverable, order_issue};      % Order being modified does not exist
        106 -> {recoverable, order_issue};      % Cannot transmit order ID
        107 -> {recoverable, order_issue};      % Cannot transmit incomplete order
        201 -> {recoverable, order_issue};      % Order rejected - reason in message
        202 -> {recoverable, order_issue};      % Order cancelled
        
        %% Warning level errors
        2104 -> {warning, minor_issue};         % Market data farm connection is OK
        2106 -> {warning, minor_issue};         % HMDS data farm connection is OK
        2108 -> {warning, minor_issue};         % Market data farm connection is inactive
        
        %% Unknown errors
        _ -> {unknown, unclassified}
    end.

%% Handle critical connection errors with emergency procedures
handle_critical_connection_error(ErrorCode, ErrorMsg, State) ->
    io:format("CRITICAL ERROR ~p: ~s - Initiating emergency procedures~n", [ErrorCode, ErrorMsg]),
    
    %% Log critical error with timestamp
    ErrorRecord = {critical_error, ErrorCode, ErrorMsg, erlang:timestamp()},
    log_critical_error(ErrorRecord),
    
    %% Mark connection as failed
    Connection = State#state.connection#ib_connection{connected = false},
    UpdatedState = State#state{connection = Connection},
    
    %% Trigger emergency stop if live trading is active
    trigger_emergency_stop(ErrorCode, ErrorMsg),
    
    %% Cancel all pending orders to prevent orphaned trades
    cancel_all_pending_orders(UpdatedState),
    
    %% Schedule aggressive reconnection
    schedule_critical_reconnect(UpdatedState).

%% Handle market data errors with recovery logic
handle_market_data_error(ErrorCode, ErrorMsg, State) ->
    io:format("Market data error ~p: ~s - Implementing recovery~n", [ErrorCode, ErrorMsg]),
    
    %% Log market data error
    ErrorRecord = {market_data_error, ErrorCode, ErrorMsg, erlang:timestamp()},
    log_market_data_error(ErrorRecord),
    
    %% Implement specific recovery based on error type
    case ErrorCode of
        162 -> %% Historical data service error
            %% Clear historical data cache and request fresh data
            clear_historical_data_cache(),
            schedule_data_refresh(State);
        200 -> %% No security definition found
            %% Remove invalid subscriptions
            remove_invalid_subscriptions(ErrorMsg, State);
        354 -> %% Market data not subscribed
            %% Attempt to resubscribe to market data
            resubscribe_market_data(State);
        10167 -> %% Market data not subscribed (alternative code)
            %% Attempt to resubscribe to market data
            resubscribe_market_data(State);
        _ ->
            %% Generic market data recovery
            generic_market_data_recovery(State)
    end.

%% Handle order execution errors with retry mechanisms
handle_order_error(ErrorCode, ErrorMsg, State) ->
    io:format("Order error ~p: ~s - Implementing retry logic~n", [ErrorCode, ErrorMsg]),
    
    %% Log order error
    ErrorRecord = {order_error, ErrorCode, ErrorMsg, erlang:timestamp()},
    log_order_error(ErrorRecord),
    
    %% Extract order ID from error message if possible
    OrderId = extract_order_id_from_error(ErrorMsg),
    
    %% Implement specific recovery based on error type
    case ErrorCode of
        103 -> %% Duplicate order ID
            %% Generate new order ID and retry
            handle_duplicate_order_id(OrderId, State);
        104 -> %% Cannot modify filled order
            %% Update order status and notify
            handle_filled_order_modification(OrderId, State);
        105 -> %% Order being modified does not exist
            %% Remove from pending orders
            handle_nonexistent_order_modification(OrderId, State);
        201 -> %% Order rejected
            %% Analyze rejection reason and decide on retry
            handle_order_rejection(OrderId, ErrorMsg, State);
        202 -> %% Order cancelled
            %% Update order status
            handle_order_cancellation(OrderId, State);
        _ ->
            %% Generic order error recovery
            generic_order_error_recovery(OrderId, ErrorCode, State)
    end.

%% Handle warning level errors
log_warning_error(ErrorCode, ErrorMsg, State) ->
    WarningRecord = {warning_error, ErrorCode, ErrorMsg, erlang:timestamp()},
    log_warning(WarningRecord),
    State.

%% Handle unknown errors with conservative approach
handle_unknown_error(ErrorCode, ErrorMsg, State) ->
    io:format("Unknown error ~p: ~s - Using conservative handling~n", [ErrorCode, ErrorMsg]),
    
    %% Log unknown error for analysis
    ErrorRecord = {unknown_error, ErrorCode, ErrorMsg, erlang:timestamp()},
    log_unknown_error(ErrorRecord),
    
    %% Conservative approach - treat as potentially serious
    %% but don't trigger emergency stop unless connection is affected
    case is_connection_related_error(ErrorCode) of
        true ->
            %% Treat as connection issue
            handle_critical_connection_error(ErrorCode, ErrorMsg, State);
        false ->
            %% Log and monitor
            State
    end.

%% Handle failures in error processing itself
handle_error_processing_failure(ProcessingError, State) ->
    io:format("CRITICAL: Error processing failed: ~p~n", [ProcessingError]),
    
    %% Log the meta-error
    MetaErrorRecord = {error_processing_failure, ProcessingError, erlang:timestamp()},
    log_critical_error(MetaErrorRecord),
    
    %% Conservative approach - assume connection issues
    Connection = State#state.connection#ib_connection{connected = false},
    UpdatedState = State#state{connection = Connection},
    
    %% Trigger emergency procedures
    trigger_emergency_stop(error_processing_failure, ProcessingError),
    
    UpdatedState.

%% ============================================================================
%% Error Recovery Implementation Functions
%% ============================================================================

%% Trigger emergency stop for critical errors
trigger_emergency_stop(ErrorCode, ErrorMsg) ->
    io:format("EMERGENCY STOP TRIGGERED: ~p - ~s~n", [ErrorCode, ErrorMsg]),
    
    %% Notify live_trader of emergency stop
    case whereis(live_trader) of
        undefined -> 
            io:format("No live_trader process to notify~n");
        Pid ->
            Pid ! {emergency_stop, ErrorCode, ErrorMsg, erlang:timestamp()}
    end,
    
    %% Notify live_scape of emergency stop
    case whereis(live_scape) of
        undefined ->
            io:format("No live_scape process to notify~n");
        ScapePid ->
            ScapePid ! {emergency_stop, ErrorCode, ErrorMsg}
    end.

%% Cancel all pending orders during emergency
cancel_all_pending_orders(State) ->
    PendingOrders = State#state.pending_orders,
    io:format("Cancelling ~p pending orders due to emergency~n", [length(PendingOrders)]),
    
    lists:foreach(fun({OrderId, Symbol, Action, Quantity, Timestamp}) ->
        io:format("Emergency cancelling order ~p: ~s ~p ~s~n", [OrderId, Action, Quantity, Symbol]),
        %% Note: Cannot send cancel message if connection is down
        %% Log for manual intervention
        log_emergency_order_cancellation(OrderId, Symbol, Action, Quantity, Timestamp)
    end, PendingOrders).

%% Schedule critical reconnection with aggressive retry
schedule_critical_reconnect(State) ->
    cancel_timer(State#state.reconnect_timer),
    
    %% Use shorter backoff for critical errors
    CriticalBackoff = min(?INITIAL_BACKOFF, 500), % 500ms for critical errors
    
    io:format("Scheduling critical reconnect in ~p ms~n", [CriticalBackoff]),
    Timer = erlang:send_after(CriticalBackoff, self(), {critical_reconnect}),
    
    State#state{
        reconnect_timer = Timer,
        reconnect_attempts = 0  % Reset attempts for critical reconnect
    }.

%% Clear historical data cache
clear_historical_data_cache() ->
    io:format("Clearing historical data cache~n"),
    case ets:info(?OHLC_DATA_TABLE) of
        undefined -> ok;
        _ -> ets:delete_all_objects(?OHLC_DATA_TABLE)
    end.

%% Schedule data refresh
schedule_data_refresh(State) ->
    io:format("Scheduling market data refresh~n"),
    erlang:send_after(1000, self(), refresh_market_data),
    State.

%% Remove invalid subscriptions
remove_invalid_subscriptions(ErrorMsg, State) ->
    io:format("Removing invalid subscriptions based on error: ~s~n", [ErrorMsg]),
    %% Extract symbol from error message if possible
    case extract_symbol_from_error(ErrorMsg) of
        {ok, Symbol} ->
            %% Remove subscription for this symbol
            Subscriptions = State#state.connection#ib_connection.subscriptions,
            NewSubscriptions = lists:filter(fun({_ReqId, Sub}) -> Sub =/= Symbol end, Subscriptions),
            Connection = State#state.connection#ib_connection{subscriptions = NewSubscriptions},
            State#state{connection = Connection};
        error ->
            %% Cannot extract symbol - log for manual review
            io:format("Cannot extract symbol from error message: ~s~n", [ErrorMsg]),
            State
    end.

%% Resubscribe to market data
resubscribe_market_data(State) ->
    io:format("Attempting to resubscribe to market data~n"),
    Subscriptions = State#state.connection#ib_connection.subscriptions,
    
    %% Clear current subscriptions and resubscribe
    Connection = State#state.connection#ib_connection{subscriptions = []},
    TempState = State#state{connection = Connection},
    
    %% Resubscribe to each symbol
    lists:foldl(fun({ReqId, Symbol}, AccState) ->
        case send_market_data_request(Symbol, ReqId, AccState) of
            {ok, NewState} ->
                io:format("Resubscribed to ~s~n", [Symbol]),
                NewState;
            {error, Reason} ->
                io:format("Failed to resubscribe to ~s: ~p~n", [Symbol, Reason]),
                AccState
        end
    end, TempState, Subscriptions).

%% Generic market data recovery
generic_market_data_recovery(State) ->
    io:format("Implementing generic market data recovery~n"),
    %% Wait a moment then try to resubscribe
    erlang:send_after(2000, self(), resubscribe_all_market_data),
    State.

%% Handle duplicate order ID error
handle_duplicate_order_id(OrderId, State) ->
    io:format("Handling duplicate order ID: ~p~n", [OrderId]),
    %% Increment next order ID to avoid future duplicates
    Connection = State#state.connection,
    NewConnection = Connection#ib_connection{
        next_order_id = Connection#ib_connection.next_order_id + 10  % Skip ahead
    },
    State#state{connection = NewConnection}.

%% Handle filled order modification attempt
handle_filled_order_modification(OrderId, State) ->
    io:format("Order ~p is already filled, updating status~n", [OrderId]),
    %% Mark order as filled in confirmations
    Confirmation = {OrderId, "Filled", 0.0, 0},
    NewConfirmations = [Confirmation | State#state.order_confirmations],
    State#state{order_confirmations = NewConfirmations}.

%% Handle nonexistent order modification
handle_nonexistent_order_modification(OrderId, State) ->
    io:format("Order ~p does not exist, removing from pending~n", [OrderId]),
    %% Remove from pending orders
    NewPendingOrders = lists:filter(fun({Id, _, _, _, _}) -> Id =/= OrderId end, 
                                   State#state.pending_orders),
    State#state{pending_orders = NewPendingOrders}.

%% Handle order rejection with retry logic
handle_order_rejection(OrderId, ErrorMsg, State) ->
    io:format("Order ~p rejected: ~s~n", [OrderId, ErrorMsg]),
    
    %% Analyze rejection reason
    case analyze_rejection_reason(ErrorMsg) of
        {retry_possible, Reason} ->
            io:format("Rejection is retryable: ~s~n", [Reason]),
            schedule_order_retry(OrderId, State);
        {permanent_failure, Reason} ->
            io:format("Rejection is permanent: ~s~n", [Reason]),
            mark_order_permanently_failed(OrderId, Reason, State);
        {unknown_reason} ->
            io:format("Unknown rejection reason, treating as permanent~n"),
            mark_order_permanently_failed(OrderId, ErrorMsg, State)
    end.

%% Handle order cancellation
handle_order_cancellation(OrderId, State) ->
    io:format("Order ~p was cancelled~n", [OrderId]),
    %% Update order status
    Confirmation = {OrderId, "Cancelled", 0.0, 0},
    NewConfirmations = [Confirmation | State#state.order_confirmations],
    %% Remove from pending
    NewPendingOrders = lists:filter(fun({Id, _, _, _, _}) -> Id =/= OrderId end, 
                                   State#state.pending_orders),
    State#state{
        order_confirmations = NewConfirmations,
        pending_orders = NewPendingOrders
    }.

%% Generic order error recovery
generic_order_error_recovery(OrderId, ErrorCode, State) ->
    io:format("Generic order error recovery for order ~p, error ~p~n", [OrderId, ErrorCode]),
    %% Conservative approach - mark as failed and notify
    mark_order_failed(OrderId, ErrorCode, State).

%% ============================================================================
%% Error Analysis and Utility Functions
%% ============================================================================

%% Extract order ID from error message
extract_order_id_from_error(ErrorMsg) ->
    %% Simple pattern matching for order ID
    case re:run(ErrorMsg, "order\\s+(\\d+)", [caseless, {capture, [1], list}]) of
        {match, [OrderIdStr]} ->
            try
                list_to_integer(OrderIdStr)
            catch
                _:_ -> undefined
            end;
        nomatch ->
            undefined
    end.

%% Extract symbol from error message
extract_symbol_from_error(ErrorMsg) ->
    %% Simple pattern matching for currency pairs
    case re:run(ErrorMsg, "([A-Z]{3}\\.[A-Z]{3})", [caseless, {capture, [1], list}]) of
        {match, [Symbol]} ->
            {ok, Symbol};
        nomatch ->
            error
    end.

%% Check if error code is connection related
is_connection_related_error(ErrorCode) ->
    ConnectionErrors = [502, 504, 1100, 1101, 1102, 2103, 2105, 2107],
    lists:member(ErrorCode, ConnectionErrors).

%% Analyze order rejection reason
analyze_rejection_reason(ErrorMsg) ->
    LowerMsg = string:to_lower(ErrorMsg),
    
    %% Check for insufficient funds/margin
    case (string:str(LowerMsg, "insufficient") > 0) orelse 
         (string:str(LowerMsg, "margin") > 0) orelse 
         (string:str(LowerMsg, "buying power") > 0) of
        true ->
            {permanent_failure, "Insufficient funds or margin"};
        false ->
            %% Check for market closed
            case (string:str(LowerMsg, "market closed") > 0) orelse 
                 (string:str(LowerMsg, "outside") > 0) orelse 
                 (string:str(LowerMsg, "trading hours") > 0) of
                true ->
                    {retry_possible, "Market closed - retry during trading hours"};
                false ->
                    %% Check for price issues
                    case (string:str(LowerMsg, "price") > 0) orelse 
                         (string:str(LowerMsg, "limit") > 0) of
                        true ->
                            {retry_possible, "Price limit issue - retry with market order"};
                        false ->
                            %% Check for size issues
                            case (string:str(LowerMsg, "size") > 0) orelse 
                                 (string:str(LowerMsg, "quantity") > 0) of
                                true ->
                                    {permanent_failure, "Invalid order size"};
                                false ->
                                    {unknown_reason}
                            end
                    end
            end
    end.

%% Schedule order retry with exponential backoff
schedule_order_retry(OrderId, State) ->
    RetryDelay = 5000, % 5 seconds
    io:format("Scheduling retry for order ~p in ~p ms~n", [OrderId, RetryDelay]),
    erlang:send_after(RetryDelay, self(), {retry_order, OrderId}),
    State.

%% Mark order as permanently failed
mark_order_permanently_failed(OrderId, Reason, State) ->
    io:format("Marking order ~p as permanently failed: ~s~n", [OrderId, Reason]),
    Confirmation = {OrderId, "PermanentlyFailed", 0.0, 0},
    NewConfirmations = [Confirmation | State#state.order_confirmations],
    %% Remove from pending
    NewPendingOrders = lists:filter(fun({Id, _, _, _, _}) -> Id =/= OrderId end, 
                                   State#state.pending_orders),
    State#state{
        order_confirmations = NewConfirmations,
        pending_orders = NewPendingOrders
    }.

%% Mark order as failed
mark_order_failed(OrderId, ErrorCode, State) ->
    io:format("Marking order ~p as failed due to error ~p~n", [OrderId, ErrorCode]),
    Confirmation = {OrderId, "Failed", 0.0, 0},
    NewConfirmations = [Confirmation | State#state.order_confirmations],
    State#state{order_confirmations = NewConfirmations}.

%% ============================================================================
%% Error Logging Functions
%% ============================================================================

%% Log critical errors
log_critical_error(ErrorRecord) ->
    io:format("CRITICAL ERROR LOGGED: ~p~n", [ErrorRecord]),
    %% In production, would write to persistent log file
    %% For now, just ensure it's visible in console
    ok.

%% Log market data errors
log_market_data_error(ErrorRecord) ->
    io:format("MARKET DATA ERROR LOGGED: ~p~n", [ErrorRecord]),
    ok.

%% Log order errors
log_order_error(ErrorRecord) ->
    io:format("ORDER ERROR LOGGED: ~p~n", [ErrorRecord]),
    ok.

%% Log warnings
log_warning(WarningRecord) ->
    io:format("WARNING LOGGED: ~p~n", [WarningRecord]),
    ok.

%% Log unknown errors
log_unknown_error(ErrorRecord) ->
    io:format("UNKNOWN ERROR LOGGED: ~p~n", [ErrorRecord]),
    ok.

%% Log emergency order cancellations
log_emergency_order_cancellation(OrderId, Symbol, Action, Quantity, Timestamp) ->
    CancellationRecord = {emergency_cancellation, OrderId, Symbol, Action, Quantity, Timestamp, erlang:timestamp()},
    io:format("EMERGENCY CANCELLATION LOGGED: ~p~n", [CancellationRecord]),
    ok.

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
        
        %% Enhanced error handling with categorization and recovery strategies
        case categorize_error(ErrorCode) of
            {critical, connection_lost} ->
                io:format("Critical connection error detected, initiating emergency recovery~n"),
                NewState = handle_critical_connection_error(ErrorCode, ErrorMsg, State),
                {error, {critical_error, ErrorCode, ErrorMsg}, NewState};
            {recoverable, market_data_issue} ->
                io:format("Market data error detected, attempting recovery~n"),
                NewState = handle_market_data_error(ErrorCode, ErrorMsg, State),
                {ok, NewState};
            {recoverable, order_issue} ->
                io:format("Order execution error detected, implementing retry logic~n"),
                NewState = handle_order_error(ErrorCode, ErrorMsg, State),
                {ok, NewState};
            {warning, minor_issue} ->
                io:format("Minor issue detected: ~p - ~s~n", [ErrorCode, ErrorMsg]),
                NewState = log_warning_error(ErrorCode, ErrorMsg, State),
                {ok, NewState};
            {unknown, _} ->
                io:format("Unknown error type: ~p - ~s~n", [ErrorCode, ErrorMsg]),
                NewState = handle_unknown_error(ErrorCode, ErrorMsg, State),
                {ok, NewState}
        end
    catch
        _:Error ->
            io:format("Error processing error message: ~p~n", [Error]),
            %% Even error handling can fail - implement fallback
            FallbackState = handle_error_processing_failure(Error, State),
            {ok, FallbackState}
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
        {_PermId, Rest7} = decode_int(Rest6),
        {_ParentId, Rest8} = decode_int(Rest7),
        {_LastFillPrice, Rest9} = decode_double(Rest8),
        {_ClientId, Rest10} = decode_int(Rest9),
        {_WhyHeld, _Rest11} = decode_string(Rest10),
        
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
        {_ReqId, Rest2} = decode_int(Rest1),
        {OrderId, Rest3} = decode_int(Rest2),
        {Symbol, Rest4} = decode_string(Rest3),
        {_SecType, Rest5} = decode_string(Rest4),
        {_Expiry, Rest6} = decode_string(Rest5),
        {_Strike, Rest7} = decode_double(Rest6),
        {_Right, Rest8} = decode_string(Rest7),
        {_Multiplier, Rest9} = decode_string(Rest8),
        {_Exchange, Rest10} = decode_string(Rest9),
        {_Currency, Rest11} = decode_string(Rest10),
        {_LocalSymbol, Rest12} = decode_string(Rest11),
        {_ExecId, Rest13} = decode_string(Rest12),
        {Time, Rest14} = decode_string(Rest13),
        {_Account, Rest15} = decode_string(Rest14),
        {_ExecExchange, Rest16} = decode_string(Rest15),
        {Side, Rest17} = decode_string(Rest16),
        {Shares, Rest18} = decode_int(Rest17),
        {Price, Rest19} = decode_double(Rest18),
        {_PermId, Rest20} = decode_int(Rest19),
        {_ClientId, Rest21} = decode_int(Rest20),
        {_Liquidation, Rest22} = decode_int(Rest21),
        {_CumQty, Rest23} = decode_int(Rest22),
        {_AvgPrice, _Rest24} = decode_double(Rest23),
        
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