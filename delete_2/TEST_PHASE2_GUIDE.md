# Phase 2 Testing Guide

## Pre-Test Setup

### 1. Prepare TWS
- Start Interactive Brokers TWS on your local machine
- Configure API settings:
  - Port: 7497 (paper trading)
  - Enable "ActiveX and Socket Clients"
  - Master API client ID: 0
  - Uncheck "Read-Only API"

### 2. Build Docker Image
```bash
./docker_test_phase2.sh
```

## Test Sequence

### Test 1: Basic Compilation and Functionality
```bash
docker run --rm -v ${PWD}:/app -w /app erlang-dev erl -noshell -eval "
    test_phase2:quick_test(),
    init:stop().
"
```

### Test 2: Full Phase 2 Test Suite (No TWS Required)
```bash
docker run --rm -v ${PWD}:/app -w /app erlang-dev erl -noshell -eval "
    test_phase2:test_all(),
    init:stop().
"
```

### Test 3: Interactive Testing with TWS
```bash
docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev
```

Then in Erlang shell:

#### 3.1 Basic Connection Test
```erlang
% Check configuration
config:log_ib_config().

% Test basic connection
{ok, Pid} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101).
timer:sleep(3000).
{ok, Status} = ib_bridge_connector:get_connection_status().
```

#### 3.2 Multi-Symbol Test (Phase 2 Feature)
```erlang
% Test multiple currency pairs
ib_bridge_connector:subscribe_market_data("EUR.USD", 1).
ib_bridge_connector:subscribe_market_data("GBP.USD", 2).
ib_bridge_connector:subscribe_market_data("USD.JPY", 3).

% Wait for ticks (check logs)
timer:sleep(10000).
```

#### 3.3 Error Handling Test
```erlang
% Test enhanced error handling
ib_bridge_connector:subscribe_market_data("INVALID.SYMBOL", 999).
% Should see enhanced error message in logs
```

#### 3.4 Reconnection Test (Simulate)
```erlang
% Monitor connection status
{ok, Status1} = ib_bridge_connector:get_connection_status().
io:format("Status before: ~p~n", [Status1]).

% Disconnect TWS briefly, then reconnect
% Watch logs for reconnection attempts
timer:sleep(15000).

{ok, Status2} = ib_bridge_connector:get_connection_status().
io:format("Status after: ~p~n", [Status2]).
```

#### 3.5 Clean Shutdown Test
```erlang
% Test graceful shutdown
ib_bridge_connector:stop_connection().
timer:sleep(2000).

% Verify process is gone
whereis(ib_bridge_connector).
```

## Expected Results

### ✅ Success Indicators

#### Compilation & Basic Tests:
- All modules compile without errors
- Basic functionality tests pass
- JSON encoding/decoding works

#### Connection Tests:
- Bridge connects to TWS successfully
- Heartbeat messages every 3 seconds
- Connection status tracking works

#### Multi-Symbol Tests:
- Multiple currency pairs subscribe successfully
- Symbol normalization works (EUR.USD → EURUSD → EUR.USD)
- Tick data flows for all subscribed symbols

#### Error Handling Tests:
- Enhanced error messages with proper codes
- Invalid symbols rejected with IB_REJECT
- Connection errors show IB_CONN with helpful messages

#### Reconnection Tests:
- Connection monitor detects disconnections
- Automatic reconnection attempts (up to 5 times)
- Resync messages (start/done/failed) in logs

#### Shutdown Tests:
- Graceful termination without errors
- Python process exits cleanly
- Erlang process cleanup verified

### ⚠️ Troubleshooting

#### Connection Issues:
```
Error: Connection refused
→ Check TWS is running and API is enabled

Error: Paper only (port 7497)
→ Correct - bridge blocks live trading ports

Error: Client ID already in use
→ Change client ID or restart TWS
```

#### Symbol Issues:
```
Error: Invalid symbol format
→ Use format: "EUR.USD", "GBP.USD", etc.

Error: No market data permissions
→ Check TWS market data subscriptions
```

#### Reconnection Issues:
```
Warning: Max reconnection attempts reached
→ Check TWS connection stability
→ Verify network connectivity
```

## Performance Monitoring

### Watch for These Logs:
```
Bridge: Python bridge starting up
Bridge: Connected to IB successfully
Bridge: Tick EUR.USD: bid=1.0850 ask=1.0852
Bridge: Connection lost, reconnecting...
Bridge: Reconnection successful
Bridge: Bridge terminated cleanly
```

### Monitor Resource Usage:
- Python process should use minimal CPU
- Memory usage should be stable
- No memory leaks during reconnections

## Test Completion Checklist

- [ ] Docker builds successfully
- [ ] All compilation tests pass
- [ ] Basic connection works
- [ ] Multiple symbols subscribe
- [ ] Tick data flows properly
- [ ] Error handling works
- [ ] Reconnection logic functions
- [ ] Clean shutdown works
- [ ] No memory leaks or crashes
- [ ] Performance is acceptable

## Next Steps

If all tests pass:
- ✅ **Phase 2 is production ready**
- 🚀 **Ready for Phase 3** (order placement) if needed
- 📊 **Ready for live market data** usage

If tests fail:
- 📋 Review error messages
- 🔧 Check TWS configuration
- 🐛 Debug specific issues
- 📞 Report any bugs found