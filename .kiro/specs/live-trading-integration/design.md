# Design Document

## Overview

This design extends the existing neuroevolutionary forex trading system to support live paper trading through Interactive Brokers (IB) API. The system will deploy evolved neural network models to make real-time trading decisions using live market data while maintaining complete separation from the existing backtesting codebase.

The design follows a minimal approach with three new Erlang modules that interface with the existing system through the established sensor/actuator pattern and Mnesia database records.

## Architecture

### High-Level Components

```
┌─────────────────┐    ┌──────────────────┐    ┌─────────────────┐
│   Existing      │    │   Live Trading   │    │  Interactive    │
│   System        │    │   Bridge         │    │  Brokers API    │
│                 │    │                  │    │                 │
│ • Mnesia DB     │◄──►│ • ib_connector   │◄──►│ • TWS/Gateway   │
│ • Agent Records │    │ • live_trader    │    │ • Paper Account │
│ • Genotypes     │    │ • live_scape     │    │ • Market Data   │
└─────────────────┘    └──────────────────┘    └─────────────────┘
```

### Module Responsibilities

1. **ib_connector.erl** - Handles IB API communication, connection management, and data translation
2. **live_trader.erl** - Manages model deployment, trading logic, and performance monitoring  
3. **live_scape.erl** - Provides scape interface compatible with existing sensor/actuator pattern

## Components and Interfaces

### IB Connector Module (ib_connector.erl)

**Purpose**: Manages Interactive Brokers API connection and data translation

**Key Functions**:
- `start_connection/3` - Establishes TWS API connection with host, port, client_id
- `subscribe_market_data/2` - Subscribes to live forex data for specified currency pairs
- `place_order/4` - Executes paper trading orders (buy/sell/close)
- `get_account_info/0` - Retrieves current account balance and positions
- `handle_market_data/2` - Processes incoming market data and converts to internal format

**Data Structures**:
```erlang
-record(ib_connection, {
    socket,
    client_id,
    next_order_id,
    subscriptions = [],
    account_info
}).

-record(market_tick, {
    symbol,
    timestamp,
    bid,
    ask,
    last,
    volume
}).
```

**Interface with IB API**:
- Uses native Erlang `gen_tcp` sockets to communicate with TWS API
- Implements IB binary message protocol directly in Erlang (no external dependencies)
- Handles message encoding/decoding for authentication, market data requests, and order placement
- All API communication coded in pure Erlang to maintain consistency with existing codebase
- Minimal codebase with no external language dependencies
- Handles connection errors with exponential backoff retry logic

### Live Trader Module (live_trader.erl)

**Purpose**: Orchestrates model deployment and trading operations

**Key Functions**:
- `deploy_model/1` - Loads agent genotype from Mnesia and initializes neural network
- `start_trading/2` - Begins live trading with specified model and risk parameters
- `stop_trading/0` - Safely stops trading and closes positions
- `get_performance/0` - Returns current trading performance metrics

**Model Loading Process**:
1. Read agent record from Mnesia database using existing genotype module
2. Reconstruct neural network topology from cortex, neuron, sensor, actuator records
3. Initialize neural processes using existing exoself pattern
4. Connect to live_scape for market data and trade execution

**Risk Management**:
- Fixed position sizing based on account balance percentage
- Maximum daily loss limits with automatic trading halt
- Position limits per currency pair

### Live Scape Module (live_scape.erl)

**Purpose**: Provides scape interface compatible with existing sensor/actuator pattern

**Key Functions**:
- `start/0` - Initializes live scape process
- `sense/4` - Handles sensor requests for live market data
- `trade/3` - Processes actuator trade signals and executes through IB connector

**Sensor Data Translation**:
- Converts live IB market data to format expected by fx_PLI and fx_PCI sensors
- Maintains sliding window buffers for historical data required by sensors
- Provides real-time price data in same format as backtesting system

**Trade Execution**:
- Receives trade signals from fx_Trade actuator (-1, 0, 1)
- Translates to IB order types (sell, close, buy)
- Returns fitness and halt flags to maintain compatibility

## Data Models

### Configuration Extension

Extend existing config.erl with live trading parameters:

```erlang
%% === Live Trading Parameters ===
ib_host() -> "127.0.0.1".
ib_port() -> 7497.  % TWS paper trading port
ib_client_id() -> 1.
live_position_size() -> 0.1.  % 10% of account per trade
live_max_daily_loss() -> 0.05.  % 5% max daily loss
live_currency_pairs() -> ['EUR.USD'].  % IB format
```

### Market Data Storage

Use ETS tables for live market data buffering:

```erlang
-record(live_market_data, {
    timestamp,
    symbol,
    open,
    high,
    low,
    close,
    volume
}).
```

### Performance Tracking

```erlang
-record(live_performance, {
    start_time,
    total_trades,
    winning_trades,
    total_pnl,
    current_position,
    daily_pnl,
    max_drawdown
}).
```

## Error Handling

### Connection Management
- Automatic reconnection with exponential backoff (1s, 2s, 4s, 8s, max 60s)
- Connection health monitoring with heartbeat messages
- Graceful degradation when market data feed is interrupted

### Trading Errors
- Order rejection handling with logging and notification
- Position reconciliation between system state and IB account
- Emergency stop mechanisms for critical errors

### Data Quality
- Market data validation and gap detection
- Stale data detection with configurable timeouts
- Fallback to last known good data during brief interruptions

## Testing Strategy

### Consolidated Test Module
All testing functionality will be contained in a single **live_trading_tests.erl** module that can be deleted after validation is complete. This module will include:

- Mock IB API responses for connector module testing
- Isolated testing of data translation functions  
- Neural network deployment testing with known genotypes
- End-to-end testing with IB paper trading account
- Market data flow validation from IB to sensors
- Trade execution verification with small position sizes
- Connection stability and error handling tests

### Test Organization
The test module will use simple test functions that can be called individually:
- `test_ib_connection/0` - Validates IB API connection
- `test_market_data_flow/0` - Tests data from IB to sensors
- `test_trade_execution/0` - Validates order placement
- `test_model_deployment/0` - Tests neural network loading
- `run_all_tests/0` - Executes complete test suite

## Deployment Architecture

### Process Hierarchy
```
live_trader (supervisor)
├── ib_connector (worker)
├── live_scape (worker)  
└── neural_network (exoself + components)
```

### Startup Sequence
1. Initialize IB connector and establish TWS connection
2. Verify paper trading mode and account access
3. Start live scape process and register with connector
4. Load selected agent genotype from Mnesia
5. Deploy neural network with live scape as environment
6. Begin trading operations

### Shutdown Sequence
1. Stop accepting new trade signals
2. Close any open positions
3. Disconnect from IB API
4. Terminate neural network processes
5. Save performance data and logs

## Security Considerations

### API Access Control
- Verify paper trading mode before allowing any operations
- Implement client ID management to prevent conflicts
- Use read-only account queries where possible

### Position Limits
- Hard-coded maximum position sizes to prevent runaway trading
- Daily loss limits with automatic system shutdown
- Currency pair restrictions to limit exposure

### Data Protection
- No storage of account credentials in code
- Secure handling of account balance and position information
- Audit logging of all trading decisions and executions

## Performance Considerations

### Latency Optimization
- Direct socket communication with IB API
- Minimal data transformation between IB format and internal format
- Efficient ETS table operations for market data buffering

### Memory Management
- Bounded market data buffers with automatic cleanup
- Efficient neural network process management
- Regular garbage collection scheduling

### Scalability Constraints
- Single model deployment per instance (as per requirements)
- Single currency pair focus to minimize complexity
- Limited to paper trading account constraints