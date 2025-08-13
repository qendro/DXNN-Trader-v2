# IB Connection Fix Implementation Plan

## Overview
**Goal**: Eliminate `{recv_server_greeting_failed,timeout}` by fixing Docker host addressing and TWS Trusted IPs on macOS.

**Root Cause**: Docker container IP not trusted by TWS, causing silent handshake rejection.

## Phase 0: Environment & Topology (macOS + Docker)

### Assumption
- TWS/IB Gateway runs on Mac host
- Erlang app runs in Docker container
- Inside Docker on macOS, `127.0.0.1` is wrong for reaching host services

### Solution
Use `host.docker.internal` (Docker Desktop's built-in hostname on macOS)

**Docker Run Command**:
```bash
docker run --rm -it \
  -v ${PWD}:/app -w /app \
  -e IB_HOST=host.docker.internal \
  -e IB_PORT=7497 \
  -e IB_CLIENT_ID=101 \
  erlang-dev
```

## Phase 1: TWS/Gateway Configuration (MUST DO)

### In TWS/IB Gateway → API → Settings:

1. ✅ **Enable ActiveX and Socket Clients**
2. ✅ **Confirm Socket Port** (7497 for paper trading)
3. 🔒 **Trusted IPs**: 
   - If "Allow connections from localhost only" is ON, either:
     - Turn it OFF, OR
     - Add Trusted IP: `192.168.65.0/24` (Docker Desktop on macOS)
4. 🔁 **Restart TWS/Gateway** after saving

### Why This Matters
If container's source IP isn't trusted, TWS accepts TCP but never sends server greeting → exact timeout you're seeing.

## Phase 2: Config Surface (Environment-Driven)

### Update `config.erl`
```erlang
% Replace current ib_host/0, ib_port/0, ib_client_id/0 functions:
ib_host() -> 
    os:getenv("IB_HOST", "host.docker.internal").

ib_port() -> 
    list_to_integer(os:getenv("IB_PORT", "7497")).

ib_client_id() -> 
    list_to_integer(os:getenv("IB_CLIENT_ID", "101")).

% Add logging at startup
log_ib_config() ->
    Host = ib_host(),
    Port = ib_port(),
    ClientId = ib_client_id(),
    io:format("IB Config: host=~s port=~p client_id=~p~n", [Host, Port, ClientId]).
```

## Phase 3: Handshake Corrections (Minimal but Exact)

### 3.1 Socket Options
```erlang
% Use these robust socket options:
SockOpts = [
    binary, 
    {active, false}, 
    {packet, 0}, 
    {nodelay, true},
    {keepalive, true}, 
    {send_timeout, 5000},
    {send_timeout_close, true}
].
```

### 3.2 Fix Version/Capabilities Message
**CRITICAL**: Do not send a date in optional-capabilities field.

```erlang
% In ib_config.hrl, update:
-define(IB_CLIENT_VERSION, 38).           % Conservative version
-define(IB_CLIENT_DATE, <<"">>).          % Empty string, NOT date
-define(IB_MIN_SERVER_VER, 38).

% In handshake code, ensure optional-capabilities is empty:
OptionalCaps = <<>>.  % empty string
ClientVersion = <<"38">>.
Payload = <<"API", 0, ClientVersion/binary, OptionalCaps/binary>>.
```

### 3.3 Distinguish Timeout vs Closed
```erlang
% Update handshake read to differentiate errors:
case gen_tcp:recv(Sock, 0, 5000) of
    {ok, Bin} ->
        log_hex("server_greeting_recv", Bin),
        parse_server_greeting(Bin);
    {error, timeout} ->
        {error, server_greeting_timeout};  % likely untrusted IP
    {error, closed} ->
        {error, server_closed_socket};     % wrong port or rejected
    {error, Reason} ->
        {error, {server_greeting_error, Reason}}
end.

% Add hex logging utility:
log_hex(Tag, Bin) ->
    Hex = lists:flatten([io_lib:format("~2.16.0B", [B]) || <<B>> <= Bin]),
    io:format("~s (~p bytes): ~s~n", [Tag, byte_size(Bin), Hex]).
```

## Phase 4: Diagnostics Helpers

### Create/Extend `ib_diag.erl`
```erlang
% Add these four test functions:

test_env() ->
    io:format("=== Environment Check ===~n"),
    io:format("IB_HOST: ~s~n", [config:ib_host()]),
    io:format("IB_PORT: ~p~n", [config:ib_port()]),
    io:format("IB_CLIENT_ID: ~p~n", [config:ib_client_id()]),
    % Show container IPs
    case inet:getifaddrs() of
        {ok, IfAddrs} ->
            io:format("Container IPs:~n"),
            lists:foreach(fun({Name, Props}) ->
                case proplists:get_value(addr, Props) of
                    {A,B,C,D} ->
                        io:format("  ~s: ~p.~p.~p.~p~n", [Name, A, B, C, D]);
                    _ -> ok
                end
            end, IfAddrs);
        _ -> ok
    end.

test_tcp() ->
    Host = config:ib_host(),
    Port = config:ib_port(),
    io:format("Testing TCP connection to ~s:~p~n", [Host, Port]),
    SockOpts = [binary, {active, false}, {packet, 0}, {nodelay, true},
                {keepalive, true}, {send_timeout, 5000}, {send_timeout_close, true}],
    case gen_tcp:connect(Host, Port, SockOpts, 3000) of
        {ok, Socket} ->
            % Log peer info
            case inet:peername(Socket) of
                {ok, {PeerAddr, PeerPort}} ->
                    io:format("✓ Connected to ~p:~p~n", [PeerAddr, PeerPort]);
                _ -> ok
            end,
            gen_tcp:close(Socket),
            io:format("✓ TCP connection successful~n"),
            {ok, connected};
        {error, Reason} ->
            io:format("✗ TCP connection failed: ~p~n", [Reason]),
            {error, Reason}
    end.

test_handshake() ->
    Host = config:ib_host(),
    Port = config:ib_port(),
    ClientId = config:ib_client_id(),
    
    io:format("Testing handshake with ~s:~p (Client ID: ~p)~n", [Host, Port, ClientId]),
    
    SockOpts = [binary, {active, false}, {packet, 0}, {nodelay, true},
                {keepalive, true}, {send_timeout, 5000}, {send_timeout_close, true}],
    
    case gen_tcp:connect(Host, Port, SockOpts, 3000) of
        {ok, Socket} ->
            Result = perform_minimal_handshake(Socket, ClientId),
            gen_tcp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

perform_minimal_handshake(Socket, ClientId) ->
    % Step 1: Send API prefix
    ApiPrefix = <<"API", 0>>,
    io:format("Sending API prefix~n"),
    log_hex("send_api_prefix", ApiPrefix),
    
    case gen_tcp:send(Socket, ApiPrefix) of
        ok ->
            % Step 2: Send version with empty optional-capabilities
            ClientVersion = 38,
            OptionalCaps = <<>>,  % Empty string
            VMsg = <<"v", (integer_to_binary(ClientVersion))/binary, "..", OptionalCaps/binary, 0>>,
            io:format("Sending version message~n"),
            log_hex("send_version", VMsg),
            
            case gen_tcp:send(Socket, VMsg) of
                ok ->
                    % Step 3: Receive server greeting
                    io:format("Waiting for server greeting...~n"),
                    case gen_tcp:recv(Socket, 0, 5000) of
                        {ok, Data} ->
                            log_hex("recv_greeting", Data),
                            case parse_server_greeting(Data) of
                                {ok, ServerVersion, ConnTime} ->
                                    % Step 4: Send client ID
                                    ClientIdMsg = <<(integer_to_binary(ClientId))/binary, 0>>,
                                    io:format("Sending client ID~n"),
                                    log_hex("send_client_id", ClientIdMsg),
                                    
                                    case gen_tcp:send(Socket, ClientIdMsg) of
                                        ok ->
                                            io:format("✓ Handshake successful~n"),
                                            io:format("  Server version: ~p~n", [ServerVersion]),
                                            io:format("  Connection time: ~s~n", [ConnTime]),
                                            {ok, ServerVersion, ConnTime};
                                        {error, Reason} ->
                                            {error, {send_client_id_failed, Reason}}
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, timeout} ->
                            {error, server_greeting_timeout};
                        {error, closed} ->
                            {error, server_closed_socket};
                        {error, Reason} ->
                            {error, {server_greeting_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, {send_version_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {send_api_prefix_failed, Reason}}
    end.

parse_server_greeting(Data) ->
    case ib_proto:read_cstring(Data) of
        {ok, ServerVerBin, Rest1} ->
            case ib_proto:read_cstring(Rest1) of
                {ok, ConnTimeBin, _Rest2} ->
                    try
                        ServerVersion = binary_to_integer(ServerVerBin),
                        {ok, ServerVersion, ConnTimeBin}
                    catch
                        _:_ -> 
                            {error, {invalid_server_version, ServerVerBin}}
                    end;
                {error, Reason} -> 
                    {error, {bad_connection_time, Reason}}
            end;
        {error, Reason} -> 
            {error, {bad_server_version, Reason}}
    end.

test_comprehensive() ->
    io:format("=== Comprehensive IB Connection Test ===~n"),
    test_env(),
    test_tcp(),
    test_handshake().
```

## Phase 5: Error Messages (Make Causes Obvious)

### Map Errors to Actionable Hints
```erlang
% Add to error handling:
handle_handshake_error(server_greeting_timeout) ->
    io:format("ERROR: Connected but no greeting within 5 seconds.~n"),
    io:format("Likely causes:~n"),
    io:format("1. Docker IP not trusted by TWS~n"),
    io:format("2. 'localhost only' enabled in TWS~n"),
    io:format("3. Wrong optional-capabilities format~n"),
    io:format("~nFix: Add 192.168.65.0/24 to TWS Trusted IPs or disable 'localhost only'~n");

handle_handshake_error(server_closed_socket) ->
    io:format("ERROR: Server closed immediately.~n"),
    io:format("Likely causes:~n"),
    io:format("1. Wrong port (7497 vs 7496 vs 4002/4001)~n"),
    io:format("2. API connections disabled~n"),
    io:format("3. TWS not running~n");
```

## Phase 6: Tests to Run

### 6.1 Host & Reachability
**On Mac host**:
```bash
lsof -nP -iTCP:7497 -sTCP:LISTEN
```

**From inside container**:
```bash
getent hosts host.docker.internal
nc -vz host.docker.internal 7497
```

### 6.2 App Diagnostics
```erlang
% In Erlang shell:
1> make:all([load]).
2> ib_diag:test_comprehensive().
3> ib_diag:test_handshake().
```

### 6.3 Acceptance Criteria
- ✅ No more `{recv_server_greeting_failed,timeout}`
- ✅ First server packet received and parsed
- ✅ Server version logged
- ✅ Subsequent API messages can proceed

## Phase 7: Implementation Checklist

### Code Changes Required
- [ ] Update `config.erl` with environment variable reading
- [ ] Fix `ib_config.hrl` with empty optional-capabilities
- [ ] Update socket options in handshake code
- [ ] Add timeout vs closed error distinction
- [ ] Create/update `ib_diag.erl` with 4 test functions
- [ ] Add hex logging utility
- [ ] Add peer address logging

### TWS Configuration
- [ ] Enable ActiveX and Socket Clients
- [ ] Set Socket port to 7497
- [ ] Add Trusted IP: `192.168.65.0/24`
- [ ] Restart TWS/Gateway

### Docker Command
- [ ] Use `host.docker.internal` instead of `127.0.0.1`
- [ ] Set environment variables for host, port, client ID

## Expected Output

### Successful Test
```
=== Comprehensive IB Connection Test ===
=== Environment Check ===
IB_HOST: host.docker.internal
IB_PORT: 7497
IB_CLIENT_ID: 101
Container IPs:
  lo0: 127.0.0.1
  eth0: 172.17.0.2
Testing TCP connection to host.docker.internal:7497
✓ Connected to {192,168,65,1}:7497
✓ TCP connection successful
Testing handshake with host.docker.internal:7497 (Client ID: 101)
Sending API prefix
send_api_prefix (4 bytes): 41504900
Sending version message
send_version (6 bytes): 7633382E2E00
Waiting for server greeting...
recv_greeting (15 bytes): 3135310A32303235303130310A
✓ Handshake successful
  Server version: 151
  Connection time: 20250101
```

### Failure Examples
```
✗ TCP connection failed: econnrefused
# → TWS not running or wrong port

✗ Connected but no greeting within 5 seconds
# → Docker IP not trusted by TWS
```

## Notes (What NOT to Change Yet)
- Don't escalate client version until greeting is stable
- Don't add protocol complexity (capabilities negotiation) in this pass
- Keep optional-capabilities empty for now
- Focus on getting basic handshake working first

## Summary
This plan addresses the core issue: **Docker networking + TWS trust settings**. The changes are minimal and focused, targeting the exact cause of the timeout error.

