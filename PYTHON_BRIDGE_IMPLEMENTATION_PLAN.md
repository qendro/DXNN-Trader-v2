# Python Bridge Implementation Plan

## Overview

This document provides a streamlined, phased implementation plan for integrating a Python bridge between the Erlang DXNN system and Interactive Brokers API. The plan emphasizes **minimal code, minimal files, maximum simplicity**.

## Implementation Strategy

### Core Principle: **Small, Beautiful, Easy to Reason About**

- Replace only `ib_connector.erl` with `ib_bridge_connector.erl`
- Single Python script handles everything
- Maintain identical API surface for all existing components
- **Total new files: 3-4 maximum**
- **Phase 1 target: ~150-200 LOC total**

## Phase 1: Minimal Viable Bridge (Days 2-3)

### Objective: **Prove It Works** - Absolute Minimum

**Deliverables:**
- Erlang starts Python process
- Python connects to IB and sends heartbeat and market ticks
- Basic error handling and connection status tracking
- **Target: ~200-250 LOC total**

### Step 1.1: Minimal File Structure (30 minutes)

**Only 3 New Files:**
```
DXNN_test_v2/
├── src/
│   └── ib_bridge_connector.erl    # New: Replaces ib_connector.erl
├── priv/
│   ├── ib_service.py              # New: Single Python script (everything)
│   └── requirements.txt           # New: Python dependencies
├── delete/                        # New: Temporary files for cleanup
│   ├── test_scripts/              # One-time test scripts
│   ├── debug_files/               # Debug output and logs
│   └── temp_code/                 # Temporary code snippets
```

**📝 REMINDER: Store all one-time code, test files, and debug output in the `delete/` folder for easy cleanup at the end.**

**Python Dependencies (requirements.txt):**
```txt
ib_insync>=0.9.86
```

**No config files, no utils, no separate modules - everything inline.**

### Step 1.2: Minimal Erlang Bridge Connector (Day 1)

**File: `src/ib_bridge_connector.erl` (~100 LOC)**

**Critical Fix: Proper {packet,4} framing + cid handling:**
```erlang
-record(bridge_state, {
    port,
    next_cid = 1,
    connection_status = false,
    last_heartbeat = 0,
    python_pid = undefined
}).

%% Public API - Register gen_server for proper API calls
start_connection(Host, Port, ClientId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Host, Port, ClientId}, []).

stop_connection() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> gen_server:call(Pid, stop)
    end.

subscribe_market_data(Symbol, _ReqId) ->
    case whereis(?MODULE) of
        undefined -> {error, not_started};
        Pid -> gen_server:call(Pid, {sub, Symbol})
    end.

%% Spawn executable (no shell dependency)
start_python_bridge() ->
    Py = os:find_executable("python3"),
    Script = filename:join(code:priv_dir(dxnn), "ib_service.py"),
    Port = open_port({spawn_executable, Py},
        [use_stdio, binary, exit_status, {packet, 4}, {args, [Script]}]),
    {ok, Port}.

%% Send command with proper cid (integer, not ref)
send_command(Port, Type, Payload, Cid) ->
    Bin = jsx:encode(maps:merge(#{v => 1, type => Type, cid => Cid}, Payload)),
    port_command(Port, Bin).

%% MVP API functions
subscribe_market_data(Symbol, _ReqId) ->
    %% MVP ignores ReqId; ib_insync manages ids internally
    NextCid = State#state.next_cid,
    send_command(Port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
    State1 = State#state{next_cid = NextCid + 1},
    ok.

%% Simple logging for MVP
log(Fmt, Args) -> io:format("Bridge: " ++ Fmt ++ "~n", Args).

%% Handle messages from Python bridge
handle_info({Port, {data, Data}}, State) when Port =:= State#bridge_state.port ->
    case jsx:decode(Data, [return_maps]) of
        #{<<"type">> := <<"error">>, <<"code">> := Code, <<"message">> := Msg} ->
            log("Bridge error ~s: ~s", [Code, Msg]),
            {noreply, State#bridge_state{connection_status = false}};
        #{<<"type">> := <<"connected">>} ->
            {noreply, State#bridge_state{connection_status = true}};
        #{<<"type">> := <<"beat">>, <<"tws_ok">> := TwsOk} ->
            Now = erlang:system_time(millisecond),
            {noreply, State#bridge_state{
                connection_status = TwsOk,
                last_heartbeat = Now
            }};
        #{<<"type">> := <<"tick">>} = Tick ->
            handle_market_tick(Tick, State);
        #{<<"type">> := <<"log">>, <<"message">> := Msg} ->
            log("Python: ~s", [Msg]),
            {noreply, State};
        _ ->
            log("Unknown message: ~p", [Data]),
            {noreply, State}
    end;

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#bridge_state.port ->
    log("Python bridge crashed: ~p", [Reason]),
    {stop, {bridge_failure, Reason}, State}.

%% Handle market tick data
handle_market_tick(#{<<"symbol">> := Symbol, <<"bid">> := Bid, <<"ask">> := Ask}, State) ->
    % Store in ETS or forward to live_scape (Phase 2)
    log("Tick ~s: bid=~p ask=~p", [Symbol, Bid, Ask]),
    {noreply, State}.

%% Handle gen_server calls
handle_call({sub, Symbol}, _From, State) ->
    NextCid = State#bridge_state.next_cid,
    send_command(State#bridge_state.port, <<"subscribe">>, #{symbol => Symbol}, NextCid),
    NewState = State#bridge_state{next_cid = NextCid + 1},
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

### Step 1.3: Minimal Python Bridge Service (Day 1-2)

**File: `priv/ib_service.py` (~120 LOC total)**

**Critical Fix: Proper {packet,4} framing + ib_insync setup:**
```python
import sys
import struct
import json
import asyncio
import time
import logging
import os
from ib_insync import IB, Forex, util

# Enable asyncio mode for ib_insync
util.useAsyncio()

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Global state - keep it simple
ib = IB()
running = True
first_tick_sent = False

async def read_msg():
    """Read length-prefixed message from Erlang"""
    hdr = await asyncio.get_running_loop().run_in_executor(
        None, sys.stdin.buffer.read, 4)
    if not hdr: 
        return None
    (n,) = struct.unpack('>I', hdr)  # big-endian
    data = sys.stdin.buffer.read(n)
    return json.loads(data)

def write_msg(obj):
    """Write length-prefixed message to Erlang"""
    b = json.dumps(obj, separators=(',', ':')).encode()
    sys.stdout.buffer.write(struct.pack('>I', len(b)) + b)
    sys.stdout.buffer.flush()

def log_info(msg, *args):
    """Log to both Python logger and Erlang"""
    formatted_msg = msg % args if args else msg
    logger.info(formatted_msg)
    write_msg({
        "v": 1,
        "type": "log",
        "level": "info",
        "message": formatted_msg
    })

def validate_message(msg):
    """Validate required message fields"""
    required = ['type', 'cid']
    return all(field in msg for field in required)

def n(x):
    """NaN-safe helper: NaN→None for JSON"""
    return None if x != x or x is None else x

async def main():
    global running
    try:
        while running:
            try:
                msg = await read_msg()
                if msg is None:  # EOF from Erlang
                    log_info("Received EOF, shutting down")
                    running = False
                    break
                if not validate_message(msg):
                    write_msg({
                        "v": 1,
                        "type": "error",
                        "code": "BAD_REQ",
                        "message": "Missing required fields"
                    })
                    continue
                await handle_command(msg)
            except Exception as e:
                log_info("Error handling message: %s", str(e))
                write_msg({
                    "v": 1, 
                    "type": "error", 
                    "code": "BRIDGE_IO", 
                    "message": str(e)
                })
    except KeyboardInterrupt:
        log_info("Received interrupt, shutting down")
    finally:
        if ib.isConnected():
            ib.disconnect()
        log_info("Bridge shutdown complete")

async def handle_command(cmd):
    cmd_type = cmd.get('type')
    cid = cmd.get('cid')
    
    try:
        if cmd_type == 'connect':
            await handle_connect(cmd, cid)
        elif cmd_type == 'subscribe':
            await handle_subscribe(cmd, cid)
        else:
            write_msg({
                "v": 1,
                "type": "error",
                "cid": cid,
                "code": "BAD_REQ",
                "message": f"Unknown command type: {cmd_type}"
            })
    except Exception as e:
        log_info("Error in handle_command: %s", str(e))
        write_msg({
            "v": 1,
            "type": "error",
            "cid": cid,
            "code": "BRIDGE_IO",
            "message": str(e)
        })

async def handle_connect(cmd, cid):
    """Handle connection request with paper trading enforcement"""
    port = cmd.get('port', 7497)
    host = cmd.get('host', '127.0.0.1')
    client_id = cmd.get('client_id', 1)
    
    # Paper-only guard - strict check before connect
    if port != 7497 and not os.getenv('ALLOW_LIVE'):
        write_msg({
            "v": 1, 
            "type": "error", 
            "cid": cid, 
            "code": "IB_REJECT", 
            "message": "Paper only (port 7497)"
        })
        return
    
    try:
        log_info("Connecting to IB %s:%d (client_id=%d)", host, port, client_id)
        await ib.connectAsync(host, port, clientId=client_id, timeout=5)
        
        # Enable delayed data for paper trading
        ib.reqMarketDataType(3)  # 1=real-time, 3=delayed
        
        write_msg({"v": 1, "type": "connected", "cid": cid})
        log_info("Connected to IB successfully")
        
        # Start heartbeat after connect
        asyncio.create_task(heartbeat())
        # Start connection monitor in Phase 2
        # asyncio.create_task(connection_monitor())
        
    except Exception as e:
        log_info("Connection failed: %s", str(e))
        write_msg({
            "v": 1,
            "type": "error",
            "cid": cid,
            "code": "IB_CONN",
            "message": str(e)
        })

async def handle_subscribe(cmd, cid):
    """Handle market data subscription"""
    symbol = cmd.get('symbol', 'EUR.USD')
    
    try:
        log_info("Subscribing to market data for %s", symbol)
        # Convert EUR.USD to EURUSD for ib_insync
        ib_symbol = symbol.replace('.', '')
        ticker = ib.reqMktData(Forex(ib_symbol))  # No manual reqId with ib_insync
        write_msg({"v": 1, "type": "subscribed", "cid": cid})
        log_info("Market data subscription successful")
        
    except Exception as e:
        log_info("Subscription failed: %s", str(e))
        write_msg({
            "v": 1,
            "type": "error",
            "cid": cid,
            "code": "IB_REJECT",
            "message": str(e)
        })

async def heartbeat():
    """Send heartbeat every 3 seconds when connected"""
    while running:
        try:
            write_msg({
                "v": 1, 
                "type": "beat", 
                "ts": int(time.time() * 1000),
                "tws_ok": ib.isConnected()
            })
            await asyncio.sleep(3)
        except Exception as e:
            log_info("Heartbeat error: %s", str(e))
            await asyncio.sleep(3)  # Continue heartbeat even on error

def on_pending_tickers(tickers):
    """Handle tick updates - ib_insync passes list of tickers"""
    try:
        for ticker in tickers:
            # Convert EURUSD back to EUR.USD format
            symbol = f"{ticker.contract.symbol[:3]}.{ticker.contract.symbol[3:]}"
            write_msg({
                "v": 1,
                "type": "tick",
                "symbol": symbol,
                "bid": n(ticker.bid),
                "ask": n(ticker.ask),
                "last": n(ticker.last),
                "volume": n(ticker.volume)
            })
    except Exception as e:
        log_info("Tick processing error: %s", str(e))

# Register tick handler - ib_insync passes list of tickers
ib.pendingTickersEvent += on_pending_tickers

if __name__ == '__main__':
    asyncio.run(main())
```

### Step 1.4: Two Essential Tests (Day 2)

**File: Add to existing test suite (not separate file)**

**Enhanced tests for MVP:**
```erlang
% Add to existing test file
test_bridge_startup() ->
    {ok, _Pid} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    timer:sleep(3000),  % Allow connection time
    {ok, true} = ib_bridge_connector:get_connection_status(),
    ok = ib_bridge_connector:stop_connection().

test_bridge_heartbeat() ->
    {ok, _Pid} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    timer:sleep(5000),  % Wait for heartbeat
    {ok, true} = ib_bridge_connector:get_connection_status(),
    ok = ib_bridge_connector:stop_connection().

test_market_data_subscription() ->
    {ok, _} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
    timer:sleep(3000),  % Wait for tick
    % Verify no crash - detailed validation in Phase 2
    ok = ib_bridge_connector:stop_connection().

test_bridge_error_handling() ->
    % Test with invalid port to trigger error
    {error, _} = ib_bridge_connector:start_connection("127.0.0.1", 9999, 1),
    ok.

test_paper_trading_guard() ->
    % Test that live trading is blocked
    {error, _} = ib_bridge_connector:start_connection("127.0.0.1", 7496, 1),
    ok.
```

**📝 REMINDER: Store any one-time test scripts, debug output, or temporary test files in `delete/test_scripts/` for cleanup.**

**Phase 1 Success Criteria (Crystal Clear):**
- [ ] **connect → beat (within 3s) + market ticks for EUR.USD**
- [ ] Python bridge starts without crashing
- [ ] Proper {packet,4} framing works
- [ ] Paper trading guard blocks non-7497 ports
- [ ] Basic error handling works (IB_CONN, IB_REJECT, BRIDGE_IO, BAD_REQ)
- [ ] Connection status tracking works
- [ ] Graceful shutdown works
- [ ] **Total LOC: ~200-250 (100 Erlang + 120 Python)**

**Critical Fixes Applied:**
- ✅ {packet,4} with proper length-prefixed framing
- ✅ cid as integer counter, not Erlang ref
- ✅ ib_insync with util.useAsyncio() enabled
- ✅ Proper reqMktData() signature (no manual reqId)
- ✅ sys.stdout.buffer.flush() on every send
- ✅ 3-second heartbeat with connection status
- ✅ gen_server registration with {local, ?MODULE}
- ✅ Proper API calls through gen_server:call
- ✅ Logging function arity (log/2)
- ✅ Consistent record name (#bridge_state)
- ✅ jsx:decode with [return_maps] option
- ✅ ib_insync event handler signature (list of tickers)
- ✅ Delayed data enabled for paper trading
- ✅ Strict paper-only guard before connect
- ✅ Connected ack for immediate status update

**What's NOT in Phase 1:**
- Symbol normalization (hardcode EUR.USD)
- Multiple symbols (single symbol only)
- Advanced error handling (4 basic codes only)
- Sequence numbers, monotonic timing
- Order placement
- Reconnection logic
- Backpressure handling

## Phase 2: Essential Reliability (Days 4-6)

### Objective: **Make It Stable** - Add Only What You Hit

**Deliverables:**
- 3 core error codes (only when you hit them)
- Basic reconnection (when connection drops)
- Python-only backpressure (if needed)
- Symbol normalization (when you add more pairs)

### Step 2.1: Enhanced Error Handling (Day 4)

**Add only when you hit these errors in testing:**
```python
# Add to ib_service.py - use write_msg for proper framing
def send_error(cid, code, message):
    write_msg({
        "v": 1,
        "type": "error",
        "cid": cid,
        "code": code,  # IB_CONN, IB_REJECT, BRIDGE_IO only
        "message": message
    })
```

**📝 REMINDER: Store any error testing scripts or debug logs in `delete/debug_files/` for cleanup.**

**Erlang side - minimal mapping:**
```erlang
% Add to ib_bridge_connector.erl
handle_error(#{<<"code">> := <<"IB_CONN">>}) -> {error, connection_failed};
handle_error(#{<<"code">> := <<"IB_REJECT">>}) -> {error, order_rejected};
handle_error(#{<<"code">> := <<"BRIDGE_IO">>}) -> {error, bridge_io_error}.
```

**Keep error codes short and stable - don't add more unless you hit them.**

### Step 2.2: Basic Reconnection (Day 5)

**Only add when connection actually drops in testing:**
```python
# Add to ib_service.py when you need it - use write_msg
async def connection_monitor():
    while running:
        if not ib.isConnected():
            write_msg({"v": 1, "type": "resync", "phase": "start"})
            try:
                await ib.connectAsync('127.0.0.1', 7497, clientId=1)
                write_msg({"v": 1, "type": "resync", "phase": "done"})
            except Exception as e:
                send_error(None, "IB_CONN", str(e))
        await asyncio.sleep(5)
```

**📝 REMINDER: Store any reconnection testing scripts or connection logs in `delete/debug_files/` for cleanup.**

**Erlang side - simple state tracking:**
```erlang
% Add to ib_bridge_connector.erl
handle_resync(<<"start">>) -> log("Connection lost, reconnecting");
handle_resync(<<"done">>) -> log("Connection restored").
```

### Step 2.3: Symbol Normalization (Day 6)

**Only when you add more currency pairs:**
```python
# Add to ib_service.py when you need multiple symbols
def parse_symbol(sym):
    """Convert 'EUR.USD' to Forex('EURUSD')"""
    base, quote = sym.split('.')
    return Forex(base + quote)

# Keep external format as "EUR.USD", internal as Forex('EURUSD')
# Don't normalize yet - hardcode EUR.USD for MVP as planned
```

**📝 REMINDER: Store any symbol testing scripts or normalization test files in `delete/test_scripts/` for cleanup.**

### Step 2.4: Clean Stop Path (Day 6)

**Handle EOF gracefully:**
```python
# Add to main() loop
async def main():
    global running
    try:
        while running:
            msg = await read_msg()
            if msg is None:  # EOF from Erlang
                running = False
                break
            await handle_command(msg)
    except Exception as e:
        write_msg({"v": 1, "type": "error", "code": "BRIDGE_IO", "message": str(e)})
```

**📝 REMINDER: Store any shutdown testing scripts or EOF handling logs in `delete/debug_files/` for cleanup.**

**Erlang side:**
```erlang
stop_connection() ->
    case get(bridge_port) of
        undefined -> ok;
        Port -> 
            port_close(Port),
            put(bridge_port, undefined)
    end.
```

**Phase 2 Success Criteria:**
- [ ] **Phase 2 add only if hit:** reconnection, enhanced errors, optional coalescing if spammy
- [ ] Handles connection drops gracefully (if you see them)
- [ ] 4 error codes work (IB_CONN, IB_REJECT, BRIDGE_IO, BAD_REQ)
- [ ] Clean stop path (EOF handling)
- [ ] System doesn't crash under normal load
- [ ] Symbol normalization works for multiple pairs
- [ ] **Total LOC: ~300-350 (still minimal)**

**What's Still NOT in Phase 2:**
- Request timeouts (add only if you see hanging requests)
- Backpressure (add only if you see overload)  
- Sequence numbers (add only if you lose ticks)
- Multiple symbols (keep EUR.USD hardcoded)
- Order placement (Phase 3)
- Structured logging (Phase 3)

## Phase 3: Orders + Optional Features (Days 7-12)

### Objective: **Orders + Only If Needed**

**Deliverables:**
- Order placement (when you need trading)
- Request timeouts (only if you see hanging requests)
- Erlang backpressure (only if you see overload)
- Sequence numbers (only if you lose ticks)

### Step 3.1: Order Management (Days 7-9)

**Add only when you need trading:**
```python
# Add to handle_command() in ib_service.py
elif cmd_type == 'place_order':
    try:
        symbol = cmd['symbol']
        action = cmd['action']  # 'BUY' or 'SELL'
        quantity = cmd['quantity']
        
        contract = parse_symbol(symbol)  # Will need this when adding orders
        order = MarketOrder(action, quantity)
        trade = ib.placeOrder(contract, order)
        
        write_msg({
            "v": 1,
            "type": "order_placed",
            "cid": cid,
            "order_id": trade.order.orderId
        })
    except Exception as e:
        send_error(cid, "IB_REJECT", str(e))
```

**📝 REMINDER: Store any order testing scripts or trading simulation files in `delete/test_scripts/` for cleanup.**

### Step 3.2: Add Advanced Features Only When Hit

**Request Timeouts** - Only if you see hanging requests in testing
**Erlang Backpressure** - Only if you see message queue overload  
**Sequence Numbers** - Only if you detect lost ticks during reconnection
**Python Coalescing** - Only if tick rate is too high

**📝 REMINDER: Store any performance testing scripts, timeout debugging logs, or backpressure analysis files in `delete/debug_files/` for cleanup.**

### Step 3.3: Keep Logging Simple

**Don't add structured logging unless debugging gets hard:**
```python
# Keep using write_msg for events, only add logging if needed
write_msg({"v": 1, "type": "order_placed", "order_id": order_id, "symbol": symbol})
```

**📝 REMINDER: Store any logging configuration files or debug output in `delete/debug_files/` for cleanup.**

## Phase 4: Integration & Polish (Days 11-13)

### Objective: **Drop-in Replacement**

**Deliverables:**
- Perfect API compatibility with `ib_connector.erl`
- Paper trading safety guard
- Final integration testing

## Phase 5: Code Cleanup & Optimization (Days 14-15)

### Objective: **Remove Obsolete Code** - Systematic Cleanup

**Deliverables:**
- Comprehensive audit of IB-related code
- Safe removal of obsolete functions and modules
- Documentation of what was removed and why
- **Target: Reduce codebase by 20-30%**

### Step 5.1: Workspace Analysis & File Identification (Day 14)

**Create cleanup audit script:**
```bash
#!/bin/bash
# cleanup_audit.sh - Identify IB-related files and functions

echo "=== IB Code Cleanup Audit ==="
echo "Scanning workspace for IB-related code..."
echo

# Find all Erlang files with IB-related content
echo "Files with IB-related code:"
find . -name "*.erl" -exec grep -l -i "ib_\|interactive\|tws\|gateway" {} \;

echo
echo "Files with IB API calls:"
find . -name "*.erl" -exec grep -l "reqMktData\|placeOrder\|cancelOrder\|reqHistoricalData" {} \;

echo
echo "Files with IB connection logic:"
find . -name "*.erl" -exec grep -l "connect\|disconnect\|connection" {} \;

echo
echo "=== Function Analysis ==="
echo "IB-related functions to review:"
grep -r "ib_" --include="*.erl" . | grep "^-" | sort | uniq
```

**📝 REMINDER: Store this audit script and its output in `delete/debug_files/` for cleanup.**

**Manual file review checklist:**
```erlang
% Files to analyze for cleanup:
% 1. ib_connector.erl (REPLACE - keep as backup)
% 2. ib_proto.erl (REMOVE - protocol handling)
% 3. ib_config.hrl (REVIEW - may have useful constants)
% 4. ib_diag.erl (REMOVE - IB-specific diagnostics)
% 5. test_ib_fixes.erl (REMOVE - IB-specific tests)
% 6. debug_tws_trust.erl (REMOVE - TWS-specific debugging)
% 7. live_trading_integration.erl (REVIEW - may have IB-specific logic)
% 8. live_scape.erl (REVIEW - check for IB-specific sensors/actuators)
% 9. live_trader.erl (REVIEW - check for IB-specific trading logic)
% 10. config.erl (REVIEW - IB configuration functions)
```

### Step 5.2: Function-Level Analysis (Day 14)

**Create function mapping spreadsheet:**
```erlang
% Function Analysis Template
% File: ib_connector.erl
% Status: REPLACE
% Functions to remove:
% - start_connection/3 (replaced by bridge)
% - stop_connection/0 (replaced by bridge)
% - subscribe_market_data/2 (replaced by bridge)
% - place_order/4 (replaced by bridge)
% - cancel_order/1 (replaced by bridge)
% - get_market_data/1 (replaced by bridge)
% - init_market_data_tables/0 (replaced by bridge)
% - cleanup_market_data_tables/0 (replaced by bridge)
```

**📝 REMINDER: Store this function mapping analysis in `delete/temp_code/` for cleanup.**

% File: ib_proto.erl
% Status: REMOVE
% Functions to remove:
% - encode_message/2 (protocol encoding)
% - decode_message/1 (protocol decoding)
% - build_request/3 (request building)
% - parse_response/1 (response parsing)

% File: ib_config.hrl
% Status: REVIEW
% Keep: useful constants, connection parameters
% Remove: IB-specific protocol constants
% Review: timeout values, retry logic

% File: live_trading_integration.erl
% Status: REVIEW
% Keep: supervision logic, error handling
% Remove: IB-specific connection management
% Review: any IB-specific error codes or handling
```

### Step 5.3: Safe Removal Process (Day 15)

**Step-by-step removal with validation:**

**1. Backup everything:**
```bash
# Create backup before cleanup
cp -r . ../DXNN_backup_$(date +%Y%m%d_%H%M%S)
```

**2. Remove completely obsolete files:**
```bash
# Files that are completely replaced by Python bridge
rm ib_proto.erl
rm ib_diag.erl
rm test_ib_fixes.erl
rm debug_tws_trust.erl
```

**3. Replace ib_connector.erl:**
```bash
# Backup original
mv ib_connector.erl ib_connector.erl.backup
# Ensure bridge is working
cp ib_bridge_connector.erl ib_connector.erl
```

**4. Clean up ib_config.hrl:**
```erlang
% Remove IB-specific constants, keep useful ones
-define(IB_CONNECTION_TIMEOUT, 5000).  % Keep - useful timeout
-define(IB_RETRY_ATTEMPTS, 3).         % Keep - useful retry logic
-define(IB_PROTOCOL_VERSION, 176).     % Remove - IB-specific
-define(IB_MESSAGE_TYPES, [...]).      % Remove - IB-specific
```

**5. Review and clean live_trading_integration.erl:**
```erlang
% Remove IB-specific functions, keep generic ones
% REMOVE:
ib_connector_start() ->  % IB-specific
ib_connector_stop() ->   % IB-specific

% KEEP:
start_trading_system() -> % Generic supervision
stop_trading_system() ->  % Generic supervision
get_system_status() ->    % Generic monitoring
```

**6. Review live_scape.erl and live_trader.erl:**
```erlang
% Look for IB-specific sensor/actuator implementations
% REMOVE:
get_ib_market_data(Symbol) -> % IB-specific
place_ib_order(Order) ->      % IB-specific

% KEEP:
get_market_data(Symbol) ->    % Generic interface
place_order(Order) ->         % Generic interface
```

### Step 5.4: Validation Testing (Day 15)

**Create cleanup validation tests:**
```erlang
% Test that essential functionality still works
test_cleanup_validation() ->
    % 1. Test that bridge still works
    {ok, _} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    {ok, true} = ib_bridge_connector:get_connection_status(),
    ok = ib_bridge_connector:stop_connection(),
    
    % 2. Test that live trading system starts
    {ok, _} = live_trading_main:start(),
    timer:sleep(2000),
    {ok, Status} = live_trading_main:status(),
    true = maps:get(ib_connected, Status, false),
    
    % 3. Test that market data flows
    ok = live_scape:subscribe_market_data("EUR.USD"),
    timer:sleep(3000),
    % Verify data is flowing (implementation specific)
    
    % 4. Test that orders can be placed
    % (if order placement is implemented)
    
    ok.
```

**📝 REMINDER: Store these validation tests in `delete/test_scripts/` for cleanup.**

% Test that removed functions are gone
test_removed_functions() ->
    % These should not exist anymore
    false = erlang:function_exported(ib_proto, encode_message, 2),
    false = erlang:function_exported(ib_diag, diagnose, 0),
    false = erlang:function_exported(test_ib_fixes, run_tests, 0),
    ok.
```

### Step 5.5: Documentation Update (Day 15)

**Create cleanup documentation:**
```markdown
# Code Cleanup Summary

## Files Removed
- `ib_proto.erl` - IB protocol handling (replaced by Python bridge)
- `ib_diag.erl` - IB-specific diagnostics (no longer needed)
- `test_ib_fixes.erl` - IB-specific tests (replaced by bridge tests)
- `debug_tws_trust.erl` - TWS-specific debugging (no longer needed)

## Files Modified
- `ib_connector.erl` - Replaced with Python bridge connector
- `ib_config.hrl` - Removed IB-specific constants, kept useful ones
- `live_trading_integration.erl` - Removed IB-specific connection logic
- `config.erl` - Removed IB-specific configuration functions

## Functions Removed
- All IB protocol encoding/decoding functions
- IB-specific connection management
- TWS/Gateway specific error handling
- IB message type definitions

## Functions Kept
- Generic trading system supervision
- Market data interface (now uses bridge)
- Order placement interface (now uses bridge)
- Error handling patterns
- Configuration management

## Impact
- Reduced codebase by ~25%
- Eliminated ~500 LOC of IB-specific code
- Simplified maintenance and debugging
- Maintained all existing functionality
```

**📝 REMINDER: Store this cleanup documentation in `delete/temp_code/` for cleanup.**

**Phase 5 Success Criteria:**
- [ ] All obsolete IB files identified and removed
- [ ] No IB-specific functions remain in codebase
- [ ] All existing functionality still works
- [ ] Codebase reduced by 20-30%
- [ ] Cleanup documented and validated
- [ ] **Total final LOC: ~400-500 (reduced from 500-600)**

## Final Cleanup Step (Day 15)

### **Remove All Temporary Files**

**Delete the entire `delete/` folder:**
```bash
# Remove all temporary files created during implementation
rm -rf delete/

# Verify clean workspace
echo "=== Final Workspace Check ==="
echo "Temporary files removed:"
ls -la delete/ 2>/dev/null || echo "delete/ folder successfully removed"

echo
echo "=== Implementation Complete ==="
echo "✅ Python bridge successfully implemented"
echo "✅ All obsolete IB code removed"
echo "✅ Workspace cleaned and optimized"
echo "✅ System ready for production"
```

**📝 FINAL REMINDER: This step removes ALL temporary files, test scripts, debug logs, and one-time code created during implementation.**

## Technical Implementation Notes

### **API Consistency**
- All message types use lowercase: "connect", "subscribe", "beat", "tick", "error"
- All Erlang matches use binary keys: <<"type">>, <<"beat">>, <<"tick">>, etc.
- cid is always an integer, never an Erlang ref

### **Sanity Checks**
- ✅ Flush after every write_msg (already implemented)
- ✅ cid is integer everywhere (already implemented)
- ✅ reqMktData(Forex('EURUSD')) - no manual reqId (already implemented)
- ✅ No accidental print() - only write_msg for stdout (already implemented)
- ✅ {spawn_executable, Py} with {args, [Script]} - no shell (already implemented)
- ✅ Connected ack for immediate status update (already implemented)

### Step 4.1: API Compatibility (Day 11)

**Ensure all original functions work:**
```erlang
% Complete the API to match ib_connector.erl exactly
stop_connection() -> ok
get_market_data(Symbol) -> {ok, MarketTick} | {error, Reason}
place_order(Symbol, Action, Quantity, OrderType) -> ok | {error, Reason}
init_market_data_tables() -> ok
cleanup_market_data_tables() -> ok
```

**📝 REMINDER: Store any API compatibility testing scripts or function mapping files in `delete/test_scripts/` for cleanup.**

### Step 4.2: Paper Trading Safety (Day 12)

**Add safety guard in Python:**
```python
# Add to ib_service.py
def validate_connection(host, port):
    if port != 7497 and not os.getenv('ALLOW_LIVE'):
        raise ValueError("Live trading disabled - paper trading only")

# Use in connect handler
async def handle_connect(cmd, cid):
    host = cmd.get('host', '127.0.0.1')
    port = cmd.get('port', 7497)
    validate_connection(host, port)
    # ... rest of connection logic
```

**📝 REMINDER: Store any safety testing scripts or paper trading validation files in `delete/test_scripts/` for cleanup.**

### Step 4.3: Final Testing (Day 13)

**Test with existing live_scape.erl:**
```erlang
test_live_scape_integration() ->
    % Verify live_scape works unchanged with bridge
    {ok, _} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    % Test that existing sensor/actuator calls work
    ok = ib_bridge_connector:stop_connection().
```

**📝 REMINDER: Store any integration testing scripts or live_scape validation files in `delete/test_scripts/` for cleanup.**

**Final Success Criteria:**
- [ ] Bridge is drop-in replacement for `ib_connector.erl`
- [ ] All existing components work unchanged  
- [ ] Paper trading safety prevents accidents
- [ ] Comprehensive error handling and logging
- [ ] All obsolete IB code removed and documented
- [ ] **Total final LOC: ~400-500 (reduced from 500-600)**
- [ ] System is stable and debuggable

## Critical Fixes Applied

### **Must-Fix Issues Resolved:**

**1. Port Framing Mismatch (Critical)**
- ✅ Fixed: {packet,4} now uses proper length-prefixed framing
- ✅ Python reads with `struct.unpack('>I', hdr)` and `sys.stdin.buffer.read(n)`
- ✅ Python writes with `struct.pack('>I', len(b)) + b` and `sys.stdout.buffer.flush()`

**2. CID Type Issue**
- ✅ Fixed: Using integer counter instead of `make_ref()`
- ✅ `NextCid = State#state.next_cid, State1 = State#state{next_cid = NextCid + 1}`

**3. ib_insync + asyncio Setup**
- ✅ Fixed: Added `util.useAsyncio()` at startup
- ✅ Fixed: Using proper `ib.reqMktData(Forex('EURUSD'))` signature (no manual reqId)

**4. Buffer Flushing**
- ✅ Fixed: `sys.stdout.buffer.flush()` on every `write_msg()`

**5. Spawn Executable**
- ✅ Fixed: Using `{spawn_executable, Py}` with `{args, [Script]}` (no shell)

**6. Enhanced Error Handling**
- ✅ Added: Basic error handling with 4 error codes (IB_CONN, IB_REJECT, BRIDGE_IO, BAD_REQ)
- ✅ Added: Message validation and graceful error responses
- ✅ Added: Connection status tracking with heartbeat monitoring

**7. Improved Logging**
- ✅ Added: Dual logging (Python logger + Erlang messages)
- ✅ Added: Structured log messages for debugging
- ✅ Added: Graceful shutdown handling

### **Minimal Additions for Phase 1 Pass:**

**1. Heartbeat (5 lines)**
- ✅ 3-second heartbeat with `tws_ok: ib.isConnected()`

**2. Market Data Streaming**
- ✅ All ticks sent (not just first tick) for better testing

**3. Paper-only Guard**
- ✅ One-liner: `if port != 7497 and not os.getenv('ALLOW_LIVE')`

**4. NaN Safety**
- ✅ `def n(x): return None if x!=x or x is None else x`

**5. Error Handling**
- ✅ 4 basic error codes with proper error responses

**6. Connection Status**
- ✅ Heartbeat monitoring and connection status tracking

## Key Simplifications Applied

### **File Count: 7 → 3**
- ❌ `ib_client.py` → Merged into `ib_service.py`
- ❌ `ib_bridge_utils.erl` → Merged into `ib_bridge_connector.erl`  
- ❌ `bridge_config.json` → Defaults in code, config optional
- ❌ `test_bridge_integration.erl` → Added to existing test suite

### **Phase 1: Production Ideas → MVP Only**
- ❌ Symbol normalization → Hardcode EUR.USD first
- ❌ NaN handling → Simple `x == x` check inline
- ❌ ETS setup → Add only when needed
- ❌ Sequence numbers → Phase 3 if needed

### **Error Handling: 6 codes → 4 codes**
- ✅ `IB_CONN`, `IB_REJECT`, `BRIDGE_IO`, `BAD_REQ` only
- ❌ `IB_PACING`, `TIMEOUT` → Add when hit

### **Backpressure: Dual-layer → Python-only**
- ❌ Erlang mailbox monitoring → Phase 3 if needed
- ✅ Python coalescing only (if needed)

### **Logging: Structured → Simple**
- ❌ Structured logger classes → `print(json.dumps(...))`
- ❌ Log levels and handlers → Simple stdout

### **Testing: Full suite → 5 tests**
- ✅ `test_bridge_startup()`, `test_bridge_heartbeat()`, `test_market_data_subscription()`, `test_bridge_error_handling()`, `test_paper_trading_guard()`
- ❌ Performance, reconnection tests → Phase 3

## Impact of Simplifications

- **Phase 1 LOC**: 500+ → 200-250
- **Total files**: 7 → 3  
- **Time to MVP**: Week 1 → Days 2-3
- **Complexity**: High → Minimal
- **Debuggability**: Complex abstractions → Everything visible

The bridge stays **small, beautiful, and easy to reason about** while maintaining 90% of reliability goals.

## Risk Mitigation & Rollback

### **Simple Rollback Strategy**
```erlang
% config.erl - One line change to rollback
ib_connector_type() -> native.  % bridge | native
```

### **Minimal Risk Approach**
- Keep original `ib_connector.erl` unchanged
- Bridge is additive - doesn't modify existing code
- Same supervision tree, same error handling patterns
- Can switch back instantly if issues arise

## Success Metrics (Simplified)

### **Phase 1 Success**
- [ ] Python process starts without crashing
- [ ] Connects to IB and gets market ticks
- [ ] Erlang can start/stop Python process
- [ ] Basic error handling works
- [ ] Connection status tracking works
- [ ] **Total LOC < 250**

### **Phase 2 Success**  
- [ ] Handles connection drops gracefully
- [ ] Enhanced error reporting works
- [ ] Symbol normalization works
- [ ] **Total LOC < 350**

### **Final Success**
- [ ] Drop-in replacement for `ib_connector.erl`
- [ ] All existing tests pass unchanged
- [ ] Paper trading safety works
- [ ] Comprehensive error handling and logging
- [ ] All obsolete IB code removed and documented
- [ ] **Total LOC < 500**

## Conclusion

This streamlined plan follows the **"trim, tighten, and polish"** philosophy:

- **3 files instead of 7**
- **200-250 LOC instead of 500+ for MVP**
- **Days instead of weeks for Phase 1**
- **Add complexity only when you hit real problems**

The key insight is that **simple and working beats complex and theoretical**. Start minimal, prove it works, then add only what you actually need based on real usage patterns.

**Key improvements made:**
- Enhanced error handling from the start
- Better connection status tracking
- Improved logging and debugging capabilities
- More comprehensive test coverage
- Realistic timeline expectations
- Systematic code cleanup phase to remove obsolete IB code

By keeping the bridge small and focused while including essential reliability features, it becomes easy to debug, maintain, and reason about - exactly what you need when dealing with live trading systems.