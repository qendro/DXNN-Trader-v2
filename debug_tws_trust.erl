%% DEBUG FILE - DELETE AFTER USE
%% Debug TWS trust settings and try different approaches

-module(debug_tws_trust).
-export([test_different_approaches/0, test_with_longer_timeout/0]).

%% Test with longer timeout and different client versions
test_with_longer_timeout() ->
    io:format("=== Testing with 15 second timeout ===~n"),
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    
    SockOpts = [binary, {active, false}, {packet, 0}, {nodelay, true},
                {keepalive, true}, {send_timeout, 10000}, {send_timeout_close, true}],
    
    case gen_tcp:connect(Host, Port, SockOpts, 5000) of
        {ok, Socket} ->
            io:format("Connected, trying handshake with 15s timeout...~n"),
            Result = perform_debug_handshake(Socket, ClientId, 15000),
            gen_tcp:close(Socket),
            Result;
        {error, Reason} ->
            io:format("Connection failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Test different approaches
test_different_approaches() ->
    io:format("=== Testing Different Approaches ===~n"),
    
    % Show what IP ranges to add to TWS
    io:format("Container IP: 172.17.0.4~n"),
    io:format("Suggested TWS Trusted IP ranges to try:~n"),
    io:format("1. 172.17.0.0/16 (Docker bridge network)~n"),
    io:format("2. 172.17.0.4/32 (exact container IP)~n"),
    io:format("3. 0.0.0.0/0 (allow all - for testing only)~n"),
    io:format("4. Disable 'Allow connections from localhost only'~n~n"),
    
    % Test with longer timeout
    test_with_longer_timeout().

%% Debug handshake with more logging
perform_debug_handshake(Socket, ClientId, TimeoutMs) ->
    io:format("Starting debug handshake...~n"),
    
    % Step 1: Send API prefix
    ApiPrefix = <<"API", 0>>,
    io:format("1. Sending API prefix: ~w~n", [ApiPrefix]),
    
    case gen_tcp:send(Socket, ApiPrefix) of
        ok ->
            io:format("   API prefix sent successfully~n"),
            
            % Step 2: Send version - try without optional capabilities
            ClientVersion = 38,
            VMsg = <<"v", (integer_to_binary(ClientVersion))/binary, "..", 0>>,
            io:format("2. Sending version: ~w~n", [VMsg]),
            
            case gen_tcp:send(Socket, VMsg) of
                ok ->
                    io:format("   Version sent successfully~n"),
                    
                    % Step 3: Wait for server greeting with longer timeout
                    io:format("3. Waiting for server greeting (~p ms timeout)...~n", [TimeoutMs]),
                    
                    case gen_tcp:recv(Socket, 0, TimeoutMs) of
                        {ok, Data} ->
                            io:format("   SUCCESS! Received data: ~w~n", [Data]),
                            parse_greeting_debug(Data);
                        {error, timeout} ->
                            io:format("   TIMEOUT after ~p ms~n", [TimeoutMs]),
                            io:format("   This means TWS is not sending greeting - IP trust issue~n"),
                            {error, timeout};
                        {error, Reason} ->
                            io:format("   ERROR: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    io:format("   Failed to send version: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("   Failed to send API prefix: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Parse greeting with debug info
parse_greeting_debug(Data) ->
    io:format("Parsing greeting data...~n"),
    case binary:split(Data, <<0>>) of
        [ServerVerBin, Rest] ->
            io:format("Server version: ~s~n", [ServerVerBin]),
            case binary:split(Rest, <<0>>) of
                [ConnTimeBin, _] ->
                    io:format("Connection time: ~s~n", [ConnTimeBin]),
                    {ok, binary_to_integer(ServerVerBin), ConnTimeBin};
                _ ->
                    io:format("Could not parse connection time~n"),
                    {error, bad_format}
            end;
        _ ->
            io:format("Could not parse server version~n"),
            {error, bad_format}
    end.