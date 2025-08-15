# Python Bridge Architecture for Live Trading System

## Overview

This document describes a minimal, robust Python bridge architecture that replaces the native Erlang IB connector while maintaining complete compatibility with the existing DXNN live trading system. The bridge uses `ib_insync` for reliable IB API communication and provides a drop-in replacement for `ib_connector.erl`.

## Design Principles

- **Minimal Impact**: Drop-in replacement requiring zero changes to existing modules
- **Simple & Safe**: Single transport mechanism with proven reliability patterns
- **Small & Beautiful**: ~500-800 LOC total for MVP, ≤1.3k LOC when hardened
- **Production Ready**: Built-in error handling, reconnection, and backpressure management

## Architecture Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Live Trading System                          │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │  live_trading_  │  │ live_trading_   │  │ test_live_      │  │
│  │  main.erl       │  │ integration.erl │  │ trading_        │  │
│  │ (UNCHANGED)     │  │ (UNCHANGED)     │  │ integration.erl │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│           Supervisor Hierarchy (UNCHANGED)                      │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ib_bridge_      │  │   live_scape    │  │   live_trader   │  │
│  │connector.erl    │  │ (UNCHANGED)     │  │ (UNCHANGED)     │  │
│  │ (DROP-IN)       │  │                 │  │                 │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                    Python Bridge (NEW)                         │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌─────────────────┐                      │
│  │  ib_service.py  │  │  bridge.json    │                      │
│  │ (ib_insync)     │  │ (config)        │                      │
│  └─────────────────┘  └─────────────────┘                      │
├─────────────────────────────────────────────────────────────────┤
│                      IB TWS/Gateway                             │
└─────────────────────────────────────────────────────────────────┘
```

### Data Flow

```
Market Data Flow:
IB TWS/Gateway → ib_service.py → ib_bridge_connector.erl → ETS Tables → live_scape.erl

Trading Flow:
live_scape.erl → ib_bridge_connector.erl → ib_service.py → IB TWS/Gateway

Error/Status Flow:
ib_service.py → ib_bridge_connector.erl → live_trading_integration.erl
```

## Core Components

### 1. File Structure (3 Files Total)

```
DXNN_test_v2/
├── ib_bridge_connector.erl    # Drop-in replacement for ib_connector.erl
├── ib_service.py              # Python IB service using ib_insync
└── bridge.json                # Optional runtime configuration
```

### 2. Transport Layer - Erlang Port with Binary Protocol

**Communication Method**: Erlang Port with `{packet, 4}` for length-prefixed binary messages
- **No** `{line, 1024}` - avoids line length limits
- **No** TCP server - eliminates network complexity
- **No** ZMQ/gRPC - keeps dependencies minimal
- **One** long-lived Python process supervised by OTP

**Message Format**: NDJSON (upgradeable to MessagePack for performance)

### 3. Message Protocol - Single Tiny Envelope

**All messages follow one unified shape**:
```json
{
  "v": 1,                    // Schema version (integer)
  "type": "Connect",         // Message type (string)
  "cid": "req_001",         // Correlation ID for request/response pairing
  "host": "127.0.0.1",     // Payload fields (varies by type)
  "port": 7497,
  "client_id": 1
}
```

**Design Benefits**:
- No nested schemas or complex structures
- Easy to parse and validate
- Version-aware for future compatibility
- Request/response correlation built-in

## Message Types (12 Total)

### Requests (Erlang → Python)

| Type | Purpose | Payload Fields |
|------|---------|----------------|
| `Connect` | Establish IB connection | `host`, `port`, `client_id` |
| `SubMkt` | Subscribe to market data | `req_id`, `symbol`, `what` |
| `UnsubMkt` | Unsubscribe from market data | `req_id` |
| `Hist` | Request historical data | `req_id`, `symbol`, `duration`, `bar_size`, `what` |
| `Place` | Place order | `ext_id`, `symbol`, `action`, `qty`, `order_type`, `lmt_price?`, `tif?` |
| `Cancel` | Cancel order | `order_id` |
| `AcctSub` | Subscribe to account updates | (none) |
| `Shutdown` | Graceful shutdown | (none) |

### Responses/Events (Python → Erlang)

| Type | Purpose | Payload Fields |
|------|---------|----------------|
| `Ack` | Request acknowledgment | `cid` |
| `Err` | Error response | `cid?`, `code`, `msg` |
| `Tick` | Market data tick | `req_id`, `ts`, `bid?`, `ask?`, `last?`, `vol?` |
| `Bar` | Historical bar data | `req_id`, `ts`, `o`, `h`, `l`, `c`, `v` |
| `Ord` | Order status update | `ext_id`, `order_id`, `status`, `filled`, `avg_price?` |
| `Acct` | Account value update | `key`, `value`, `ccy?` |
| `Beat` | Heartbeat | `ts`, `tws_ok`, `server_ver` |
| `Resync` | Reconnection sync | `phase` ("start" or "done") |

## Implementation Details

### 1. ib_bridge_connector.erl - Erlang Side

**Purpose**: Drop-in replacement for `ib_connector.erl` maintaining identical API

**Key Features**:
```erlang
-module(ib_bridge_connector).
-behaviour(gen_server).
-compile(export_all).

%% Identical API to ib_connector.erl
start_connection(Host, Port, ClientId) -> {ok, Pid} | {error, Reason}
stop_connection() -> ok
subscribe_market_data(Symbol, ReqId) -> ok | {error, Reason}
place_order(Symbol, Action, Quantity, OrderType) -> ok | {error, Reason}
get_market_data(Symbol) -> {ok, MarketTick} | {error, Reason}
% ... all other functions unchanged

%% Internal state
-record(state, {
    port,                    % Python process port
    pending_requests = #{},  % cid -> {From, Timestamp}
    next_cid = 1,           % Correlation ID counter
    heartbeat_timer,        % Heartbeat monitoring
    healthy = false,        % Bridge health status
    reconnect_count = 0     % Reconnection attempts
}).
```

**Core Implementation**:
```erlang
init({Host, Port, ClientId}) ->
    process_flag(trap_exit, true),
    case start_python_bridge() of
        {ok, PythonPort} ->
            State = #state{port = PythonPort},
            case send_connect_request(Host, Port, ClientId, State) of
                {ok, NewState} -> {ok, NewState};
                {error, Reason} -> {stop, {connect_failed, Reason}}
            end;
        {error, Reason} ->
            {stop, {bridge_start_failed, Reason}}
    end.

start_python_bridge() ->
    Cmd = "python3 ib_service.py",
    Port = open_port({spawn, Cmd}, [binary, {packet, 4}, exit_status]),
    {ok, Port}.

send_request(Type, Payload, State) ->
    Cid = generate_correlation_id(State),
    Message = #{
        <<"v">> => 1,
        <<"type">> => Type,
        <<"cid">> => Cid
    },
    FullMessage = maps:merge(Message, Payload),
    JsonData = jsx:encode(FullMessage),
    port_command(State#state.port, JsonData),
    {ok, Cid, State#state{next_cid = State#state.next_cid + 1}}.
```

**Error Handling**:
```erlang
handle_info({Port, {data, Data}}, State) when Port =:= State#state.port ->
    case jsx:decode(Data) of
        #{<<"type">> := <<"Err">>, <<"code">> := Code, <<"msg">> := Msg} ->
            handle_bridge_error(Code, Msg, State);
        #{<<"type">> := <<"Beat">>, <<"tws_ok">> := TwsOk} ->
            handle_heartbeat(TwsOk, State);
        #{<<"type">> := Type} = Message ->
            handle_bridge_message(Type, Message, State)
    end;

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port ->
    io:format("Python bridge crashed: ~p~n", [Reason]),
    case restart_python_bridge(State) of
        {ok, NewState} -> {noreply, NewState};
        {error, _} -> {stop, bridge_failure, State}
    end.
```

**Backpressure Management**:
```erlang
handle_info({mailbox_size, Size}, State) when Size > 1000 ->
    %% Switch to latest-tick-only mode
    NewState = State#state{backpressure_mode = latest_only},
    {noreply, NewState};

process_tick_with_backpressure(Tick, State) ->
    case State#state.backpressure_mode of
        latest_only ->
            %% Only keep latest tick per symbol
            update_latest_tick_only(Tick, State);
        normal ->
            %% Process all ticks
            process_tick_normal(Tick, State)
    end.
```

### 2. ib_service.py - Python Side

**Purpose**: Single-file IB service using `ib_insync` with robust error handling

**Key Features**:
```python
#!/usr/bin/env python3
"""
IB Service Bridge - Single file ib_insync bridge for Erlang DXNN system
Handles IB TWS/Gateway communication with automatic reconnection and error recovery
"""

import asyncio
import json
import sys
import struct
import time
from typing import Dict, Optional, Any
from ib_insync import IB, Stock, Forex, MarketOrder, LimitOrder, util

class IBService:
    def __init__(self):
        self.ib = IB()
        self.subs: Dict[int, Any] = {}      # req_id -> ib handler
        self.orders: Dict[str, int] = {}    # ext_id -> ib_order_id
        self.connected = False
        self.server_version = 0
        self.tick_coalescers = {}           # symbol -> tick coalescer
        self.config = self.load_config()
        
    def load_config(self) -> Dict[str, Any]:
        """Load configuration with sensible defaults"""
        try:
            with open('bridge.json', 'r') as f:
                config = json.load(f)
        except FileNotFoundError:
            config = {}
        
        return {
            'tick_hz': config.get('tick_hz', 50),
            'heartbeat_interval': config.get('heartbeat_interval', 3),
            'connect_timeout': config.get('connect_timeout', 5),
            'reconnect_backoff_min': config.get('reconnect_backoff_min', 1),
            'reconnect_backoff_max': config.get('reconnect_backoff_max', 8),
            'paper_only': config.get('paper_only', True)
        }
```

**Message Handling**:
```python
async def handle_message(self, message: Dict[str, Any]) -> None:
    """Handle incoming message from Erlang"""
    msg_type = message.get('type')
    cid = message.get('cid')
    
    try:
        if msg_type == 'Connect':
            await self.handle_connect(message, cid)
        elif msg_type == 'SubMkt':
            await self.handle_sub_market(message, cid)
        elif msg_type == 'Place':
            await self.handle_place_order(message, cid)
        elif msg_type == 'Shutdown':
            await self.handle_shutdown(message, cid)
        else:
            await self.send_error(cid, 'BAD_REQ', f'Unknown message type: {msg_type}')
    except Exception as e:
        await self.send_error(cid, 'BRIDGE_IO', str(e))

async def handle_connect(self, message: Dict[str, Any], cid: str) -> None:
    """Handle connection request with paper trading enforcement"""
    host = message.get('host', '127.0.0.1')
    port = message.get('port', 7497)
    client_id = message.get('client_id', 1)
    
    # Enforce paper trading unless explicitly allowed
    if port != 7497 and not os.getenv('ALLOW_LIVE'):
        await self.send_error(cid, 'IB_REJECT', 'Live trading not allowed - use port 7497')
        return
    
    try:
        await self.ib.connectAsync(host, port, clientId=client_id, 
                                 timeout=self.config['connect_timeout'])
        self.connected = True
        self.server_version = self.ib.client.serverVersion()
        await self.send_ack(cid)
        
        # Start heartbeat
        asyncio.create_task(self.heartbeat_loop())
        
    except Exception as e:
        await self.send_error(cid, 'IB_CONN', str(e))
```

**Tick Coalescing**:
```python
class TickCoalescer:
    """Coalesce ticks to prevent overwhelming Erlang side"""
    def __init__(self, symbol: str, max_hz: int = 50):
        self.symbol = symbol
        self.max_hz = max_hz
        self.min_interval = 1.0 / max_hz
        self.last_sent = 0
        self.pending_tick = None
        
    def add_tick(self, tick_data: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Add tick, return tick to send if ready"""
        now = time.time()
        self.pending_tick = tick_data
        
        if now - self.last_sent >= self.min_interval:
            self.last_sent = now
            result = self.pending_tick
            self.pending_tick = None
            return result
        return None

async def handle_tick(self, ticker) -> None:
    """Handle incoming tick with coalescing"""
    req_id = self.get_req_id_for_symbol(ticker.contract.symbol)
    if req_id is None:
        return
        
    tick_data = {
        'v': 1,
        'type': 'Tick',
        'req_id': req_id,
        'ts': int(time.time() * 1000),
        'bid': ticker.bid if ticker.bid == ticker.bid else None,  # NaN check
        'ask': ticker.ask if ticker.ask == ticker.ask else None,
        'last': ticker.last if ticker.last == ticker.last else None,
        'vol': ticker.volume if ticker.volume == ticker.volume else None
    }
    
    coalescer = self.tick_coalescers.get(ticker.contract.symbol)
    if coalescer is None:
        coalescer = TickCoalescer(ticker.contract.symbol, self.config['tick_hz'])
        self.tick_coalescers[ticker.contract.symbol] = coalescer
    
    tick_to_send = coalescer.add_tick(tick_data)
    if tick_to_send:
        await self.send_message(tick_to_send)
```

**Idempotent Orders**:
```python
async def handle_place_order(self, message: Dict[str, Any], cid: str) -> None:
    """Handle order placement with idempotency"""
    ext_id = message.get('ext_id')
    
    # Check if order already exists
    if ext_id in self.orders:
        existing_order_id = self.orders[ext_id]
        # Return existing order info instead of placing new order
        await self.send_message({
            'v': 1,
            'type': 'Ord',
            'ext_id': ext_id,
            'order_id': existing_order_id,
            'status': 'Submitted',  # Or get actual status
            'filled': 0,
            'avg_price': None
        })
        await self.send_ack(cid)
        return
    
    # Place new order
    symbol = message.get('symbol')
    action = message.get('action')
    quantity = message.get('qty')
    order_type = message.get('order_type', 'MKT')
    
    try:
        contract = self.create_contract(symbol)
        order = self.create_order(action, quantity, order_type, message)
        
        trade = self.ib.placeOrder(contract, order)
        self.orders[ext_id] = trade.order.orderId
        
        await self.send_ack(cid)
        
    except Exception as e:
        await self.send_error(cid, 'IB_REJECT', str(e))
```

**Auto-Reconnection with Resync**:
```python
async def reconnect_loop(self) -> None:
    """Handle automatic reconnection with exponential backoff"""
    backoff = self.config['reconnect_backoff_min']
    
    while True:
        try:
            if not self.ib.isConnected():
                await self.send_message({
                    'v': 1,
                    'type': 'Resync',
                    'phase': 'start'
                })
                
                # Attempt reconnection
                await self.ib.connectAsync(self.last_host, self.last_port, 
                                         clientId=self.last_client_id)
                
                # Resubscribe to all active subscriptions
                await self.resubscribe_all()
                
                await self.send_message({
                    'v': 1,
                    'type': 'Resync',
                    'phase': 'done'
                })
                
                backoff = self.config['reconnect_backoff_min']  # Reset backoff
                
            await asyncio.sleep(backoff)
            
        except Exception as e:
            backoff = min(backoff * 2, self.config['reconnect_backoff_max'])
            await asyncio.sleep(backoff + random.uniform(0, 1))  # Add jitter
```

### 3. bridge.json - Configuration

**Optional runtime configuration**:
```json
{
  "tick_hz": 50,
  "heartbeat_interval": 3,
  "connect_timeout": 5,
  "reconnect_backoff_min": 1,
  "reconnect_backoff_max": 8,
  "paper_only": true,
  "coalesce_window_ms": 20,
  "max_pending_orders": 100,
  "log_level": "INFO"
}
```

## Error Handling

### Error Codes (Fixed Set)

| Code | Description | Retry? |
|------|-------------|--------|
| `IB_CONN` | IB connection failure | Yes |
| `IB_PACING` | IB pacing violation | Yes (with backoff) |
| `IB_REJECT` | IB order rejection | No |
| `BRIDGE_IO` | Bridge I/O error | Yes |
| `BAD_REQ` | Invalid request format | No |
| `TIMEOUT` | Request timeout | Yes |

### Error Response Format

```json
{
  "v": 1,
  "type": "Err",
  "cid": "req_001",
  "code": "IB_CONN",
  "msg": "Connection to TWS failed: Connection refused"
}
```

### Health Monitoring

**Heartbeat System**:
```python
async def heartbeat_loop(self) -> None:
    """Send periodic heartbeat to Erlang"""
    while self.connected:
        await self.send_message({
            'v': 1,
            'type': 'Beat',
            'ts': int(time.time() * 1000),
            'tws_ok': self.ib.isConnected(),
            'server_ver': self.server_version
        })
        await asyncio.sleep(self.config['heartbeat_interval'])
```

**Erlang Health Monitoring**:
```erlang
handle_heartbeat_timeout(State) ->
    MissedBeats = State#state.missed_heartbeats + 1,
    if
        MissedBeats >= 3 ->
            %% Mark unhealthy, halt new orders but allow emergency close
            NewState = State#state{
                healthy = false,
                missed_heartbeats = MissedBeats
            },
            {noreply, NewState};
        true ->
            {noreply, State#state{missed_heartbeats = MissedBeats}}
    end.
```

## Backpressure Management

### Two Lines of Defense

**1. Python Side - Tick Coalescing**:
- Coalesce ticks per symbol to max 50Hz
- Never coalesce order or account updates
- Use time-based coalescing with pending tick storage

**2. Erlang Side - Mailbox Watermark**:
- Monitor mailbox size
- Switch to "latest tick only" mode when overwhelmed
- Preserve all order and account messages

```erlang
check_mailbox_pressure(State) ->
    {message_queue_len, QueueLen} = process_info(self(), message_queue_len),
    if
        QueueLen > 1000 ->
            State#state{backpressure_mode = latest_only};
        QueueLen < 500 ->
            State#state{backpressure_mode = normal};
        true ->
            State
    end.
```

## Testing Strategy

### Minimal Tests (That You'll Actually Run)

**1. Python Unit Tests**:
```python
def test_tick_coalescing():
    """Test that coalescer emits ≤N Hz"""
    coalescer = TickCoalescer("EURUSD", 50)
    
    # Send 1000 ticks in 1 second
    start_time = time.time()
    sent_count = 0
    
    for i in range(1000):
        tick = {'price': 1.1000 + i * 0.0001}
        if coalescer.add_tick(tick):
            sent_count += 1
    
    elapsed = time.time() - start_time
    max_expected = int(elapsed * 50) + 1  # Allow for timing variance
    assert sent_count <= max_expected

def test_idempotent_orders():
    """Test that duplicate ext_id doesn't place new order"""
    service = IBService()
    
    # First order
    result1 = service.handle_place_order_sync({
        'ext_id': 'test_001',
        'symbol': 'EURUSD',
        'action': 'BUY',
        'qty': 1000
    })
    
    # Duplicate order
    result2 = service.handle_place_order_sync({
        'ext_id': 'test_001',
        'symbol': 'EURUSD', 
        'action': 'BUY',
        'qty': 1000
    })
    
    assert result1['order_id'] == result2['order_id']
```

**2. E2E Test (Paper Trading)**:
```erlang
test_e2e_paper_trading() ->
    %% Start bridge
    {ok, _Pid} = ib_bridge_connector:start_connection("127.0.0.1", 7497, 1),
    
    %% Subscribe to EUR.USD
    ok = ib_bridge_connector:subscribe_market_data("EUR.USD", 1),
    
    %% Wait for tick
    receive
        {market_data, "EUR.USD", _Tick} ->
            io:format("✓ Received market data~n")
    after 10000 ->
        throw(no_market_data)
    end,
    
    %% Place and cancel order
    ok = ib_bridge_connector:place_order("EUR.USD", "BUY", 1000, "MKT"),
    
    %% Kill TWS and verify resync
    %% (Manual step - restart TWS)
    
    %% Should see Resync(start) then Resync(done) and continued ticks
    ok.
```

## Deployment and Operations

### Installation

**Dependencies**:
```bash
# Python dependencies
pip install ib_insync

# Erlang dependencies (already present)
# jsx for JSON encoding/decoding
```

**File Placement**:
```bash
# Replace existing ib_connector.erl
mv ib_connector.erl ib_connector.erl.backup
cp ib_bridge_connector.erl ./

# Add Python service
cp ib_service.py ./
cp bridge.json ./  # Optional

# No other changes needed
```

### Configuration

**Erlang Side** (no changes to config.erl needed):
```erlang
%% All existing configuration works unchanged
ib_host() -> "127.0.0.1".
ib_port() -> 7497.
ib_client_id() -> 1.
```

**Python Side** (bridge.json):
```json
{
  "tick_hz": 50,
  "heartbeat_interval": 3,
  "paper_only": true
}
```

### Startup Procedure

**Same as before** - no changes needed:
```erlang
%% 1. Initialize System
make:all([load]).
mnesia:start().

%% 2. Start Trading (automatically starts Python bridge)
live_trading_main:start().

%% 3. Monitor (same commands work)
live_trading_main:status().
```

### Monitoring

**Bridge-Specific Monitoring**:
```erlang
%% Check bridge health
ib_bridge_connector:get_bridge_health().
%% Returns: {ok, #{healthy => true, heartbeat_age => 1500, python_pid => 12345}}

%% Check Python process
ib_bridge_connector:get_python_status().
%% Returns: {ok, #{connected => true, server_version => 176, uptime => 3600}}
```

**Existing monitoring continues to work unchanged**:
```erlang
live_trading_main:diagnostics().
live_trading_main:performance().
```

## Advantages of This Architecture

### 1. **Minimal Impact**
- **Zero changes** to `live_scape.erl`, `live_trader.erl`, `live_trading_integration.erl`
- **Same API** - all existing function calls work unchanged
- **Same supervision tree** - OTP patterns preserved
- **Same error handling** - existing recovery mechanisms intact

### 2. **Proven Reliability**
- **ib_insync** is battle-tested Python IB library
- **Erlang Port** is OTP-supervised and robust
- **Simple protocol** reduces failure modes
- **Built-in reconnection** with exponential backoff

### 3. **Performance Optimized**
- **Tick coalescing** prevents message flooding
- **Binary protocol** with length prefixes
- **Backpressure handling** at both layers
- **Minimal serialization** overhead

### 4. **Maintainable**
- **Single file** Python service (~400-600 LOC)
- **Single file** Erlang connector (~300-400 LOC)
- **No complex dependencies** or frameworks
- **Clear error messages** and logging

### 5. **Production Ready**
- **Paper trading enforcement** built-in
- **Idempotent orders** prevent duplicates
- **Health monitoring** with heartbeats
- **Graceful degradation** under load

## Migration Strategy

### Phase 1: Drop-in Replacement (1-2 days)
1. Backup existing `ib_connector.erl`
2. Install `ib_bridge_connector.erl` and `ib_service.py`
3. Test basic connectivity and market data
4. Verify existing tests pass

### Phase 2: Validation (2-3 days)
1. Run comprehensive integration tests
2. Validate order placement and cancellation
3. Test reconnection scenarios
4. Performance testing under load

### Phase 3: Production Deployment (1 day)
1. Deploy to paper trading environment
2. Monitor for 24-48 hours
3. Validate all existing functionality
4. Document any behavioral differences

## Risk Mitigation

### Fallback Strategy
Keep both implementations available:
```erlang
%% In config.erl
ib_connector_type() -> bridge.  % or native

%% In live_trading_integration.erl
start_ib_connector() ->
    case config:ib_connector_type() of
        bridge -> ib_bridge_connector:start_connection(...);
        native -> ib_connector:start_connection(...)
    end.
```

### Safety Measures
- **Paper trading enforcement** in Python service
- **Order size limits** in bridge validation
- **Connection health monitoring** with automatic fallback
- **Comprehensive logging** for debugging

## Conclusion

This Python bridge architecture provides:

- **Minimal risk** - only one module changes
- **Maximum compatibility** - identical API surface
- **Proven reliability** - battle-tested components
- **Easy maintenance** - simple, focused codebase
- **Production readiness** - comprehensive error handling

The design maintains all the sophisticated orchestration, error handling, and risk management of your existing system while solving the IB connectivity challenges through a mature, well-supported Python library.

**Total Implementation**: ~800 LOC across 3 files
**Migration Time**: 3-5 days including testing
**Risk Level**: Minimal - drop-in replacement with fallback option