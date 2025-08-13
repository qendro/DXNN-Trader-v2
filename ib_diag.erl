%% IB Diagnostic Tools
%% Operator-friendly debugging utilities for IB TWS API

-module(ib_diag).
-include("ib_config.hrl").
-export([test_handshake/0, test_handshake/1, hex_dump/2]).

%% Test handshake with detailed hex logging
test_handshake() ->
    test_handshake([]).

test_handshake(Opts) ->
    Host = proplists:get_value(host, Opts, config:ib_host()),
    Port = proplists:get_value(port, Opts, config:ib_port()),
    ClientId = proplists:get_value(client_id, Opts, config:ib_client_id()),
    Timeout = proplists:get_value(timeout, Opts, ?IB_HANDSHAKE_TIMEOUT),
    
    io:format("=== IB Handshake Diagnostic Test ===~n"),
    io:format("Host: ~s, Port: ~p, Client ID: ~p, Timeout: ~pms~n", 
              [Host, Port, ClientId, Timeout]),
    
    case gen_tcp:connect(Host, Port, ?IB_TCP_OPTS, ?IB_CONNECT_TIMEOUT) of
        {ok, Socket} ->
            io:format("✓ TCP connection established~n"),
            
            Result = perform_diagnostic_handshake(Socket, ClientId, Timeout),
            gen_tcp:close(Socket),
            
            case Result of
                {ok, ServerVersion, ConnTime} ->
                    io:format("~n✓ HANDSHAKE SUCCESSFUL~n"),
                    io:format("  Server Version: ~p~n", [ServerVersion]),
                    io:format("  Connection Time: ~s~n", [ConnTime]),
                    {ok, ServerVersion, ConnTime};
                {error, Reason} ->
                    io:format("~n✗ HANDSHAKE FAILED: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("✗ TCP connection failed: ~p~n", [Reason]),
            {error, {tcp_connect_failed, Reason}}
    end.

%% Perform handshake with detailed logging of each byte sent/received
perform_diagnostic_handshake(Socket, ClientId, Timeout) ->
    ClientVersion = ib_connector:get_client_version(),
    ClientDate = ib_connector:get_client_date(),
    
    %% Step 1: Send "API\0" prefix
    ApiPrefix = <<"API", 0>>,
    io:format("~nSTEP 1: Sending API prefix~n"),
    hex_dump("SEND", ApiPrefix),
    
    case gen_tcp:send(Socket, ApiPrefix) of
        ok ->
            %% Step 2: Send version message
            VMsg = <<"v", (ib_proto:i2b(ClientVersion))/binary, "..", ClientDate/binary, 0>>,
            io:format("~nSTEP 2: Sending version message~n"),
            hex_dump("SEND", VMsg),
            
            case gen_tcp:send(Socket, VMsg) of
                ok ->
                    %% Step 3: Receive server greeting
                    io:format("~nSTEP 3: Waiting for server greeting...~n"),
                    inet:setopts(Socket, [{active, false}]),
                    
                    case gen_tcp:recv(Socket, 0, Timeout) of
                        {ok, Data} ->
                            hex_dump("RECV", Data),
                            
                            case parse_and_log_greeting(Data) of
                                {ok, ServerVersion, ConnTime} ->
                                    %% Step 4: Send client ID
                                    ClientIdMsg = ib_proto:z(ib_proto:i2b(ClientId)),
                                    io:format("~nSTEP 4: Sending client ID~n"),
                                    hex_dump("SEND", ClientIdMsg),
                                    
                                    case gen_tcp:send(Socket, ClientIdMsg) of
                                        ok ->
                                            {ok, ServerVersion, ConnTime};
                                        {error, Reason} ->
                                            {error, {send_client_id_failed, Reason}}
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, {recv_server_greeting_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {send_version_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {send_api_prefix_failed, Reason}}
    end.

%% Parse server greeting with detailed logging
parse_and_log_greeting(Data) ->
    io:format("Parsing server greeting...~n"),
    case ib_proto:read_cstring(Data) of
        {ok, ServerVerBin, Rest1} ->
            io:format("  Server version string: ~s~n", [ServerVerBin]),
            case ib_proto:read_cstring(Rest1) of
                {ok, ConnTimeBin, _Rest2} ->
                    io:format("  Connection time string: ~s~n", [ConnTimeBin]),
                    try
                        ServerVersion = binary_to_integer(ServerVerBin),
                        io:format("  Parsed server version: ~p~n", [ServerVersion]),
                        {ok, ServerVersion, ConnTimeBin}
                    catch
                        _:_ -> 
                            {error, {invalid_server_version, ServerVerBin}}
                    end;
                {error, Reason} ->
                    io:format("  Failed to parse connection time: ~p~n", [Reason]),
                    {error, {bad_connection_time, Reason}}
            end;
        {error, Reason} ->
            io:format("  Failed to parse server version: ~p~n", [Reason]),
            {error, {bad_server_version, Reason}}
    end.

%% Hex dump utility for debugging
hex_dump(Label, Data) when is_binary(Data) ->
    Hex = binary:encode_hex(Data),
    Ascii = ascii_representation(Data),
    io:format("~s: ~s (~s)~n", [Label, Hex, Ascii]).

%% Create ASCII representation of binary data
ascii_representation(Data) ->
    binary_to_list(
        << <<(if Byte >= 32, Byte =< 126 -> Byte; true -> $. end)>> || <<Byte>> <= Data >>
    ).