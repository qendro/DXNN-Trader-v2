Live Trading System - Consolidated Architecture
Overview

This document provides the complete architecture for the revolutionary Python-centric live trading system, where ib_service.py handles ALL Interactive Brokers operations while Erlang focuses purely on neural network coordination and data storage.

Table of Contents

Architectural Philosophy

Python-Centric Architecture

Simplified Erlang Architecture

Data Flow

Implementation Strategy

Dependencies and Integration

Cleanup and Migration

Architectural Philosophy
Python-Centric IB Operations

All IB communication handled in ib_service.py

Live data streaming and tick processing in Python

Historical data loading (last x weeks at x bar size) in Python

Real-time tick aggregation to 1-minute bars in Python

Trade execution managed entirely by Python service

Only processed OHLC bars sent to Erlang for ETS storage

Erlang-Centric Neural Network Operations

Neural network coordination in simplified Erlang modules

ETS data storage for processed OHLC bars only

Agent deployment and monitoring

System orchestration and high-level control

Python-Centric Architecture
Revolutionary System Design
┌─────────────────────────────────────────────────────────────────┐
│                    Python-Centric Architecture                  │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                    ib_service.py                            │ │
│  │                 (COMPREHENSIVE IB HANDLER)                  │ │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────────┐ │ │
│  │  │IB Connection│ │Live Data    │ │Historical Data Loading  │ │ │
│  │  │Management   │ │Streaming    │ │(x weeks, x bar size)    │ │ │
│  │  └─────────────┘ └─────────────┘ └─────────────────────────┘ │ │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────────┐ │ │
│  │  │Tick-to-OHLC │ │1-Min Bar    │ │Trade Execution          │ │ │
│  │  │Conversion   │ │Aggregation  │ │& Order Management       │ │ │
│  │  └─────────────┘ └─────────────┘ └─────────────────────────┘ │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                              │                                   │
│                              ▼ (Only OHLC Bars)                  │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                 Erlang System                               │ │
│  │                                                             │ │
│  │  ┌─────────────────┐                    ┌─────────────────┐ │ │
│  │  │ live_trading.erl│                    │ live_scape.erl  │ │ │
│  │  │ (SIMPLIFIED)    │                    │ (SIMPLIFIED)    │ │ │
│  │  │                 │                    │                 │ │ │
│  │  │ - System coord  │ ←──── Control ───→ │ - NN interface  │ │ │
│  │  │ - NN management │                    │ - ETS storage   │ │ │
│  │  │ - State mgmt    │                    │ - Data serving  │ │ │
│  │  └─────────────────┘                    └─────────────────┘ │ │
│  └─────────────────────────────────────────────────────────────┘ │
│                              │                                   │
│                              ▼                                   │
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                    ETS Tables                               │ │
│  │              (Only Processed OHLC Bars)                     │ │
│  │                                                             │ │
│  │  live_EURUSD1, live_GBPUSD1, live_USDJPY1, etc.             │ │
│  │  ├─ Historical bars (loaded by Python, sent one-by-one)     │ │
│  │  └─ Real-time 1-min bars (aggregated by Python)             │ │
│  └─────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘

Enhanced ib_service.py Architecture
Core Responsibilities

IB Connection Management

Live Data Streaming & Processing

Historical Data Loading

Real-time Tick Aggregation

Trade Execution

OHLC Bar Delivery to Erlang

1. IB Connection Management
class IBConnectionManager:
    def __init__(self):
        self.ib = IB()
        self.connection_status = False
        self.subscribed_symbols = set()

    async def connect(self, host="127.0.0.1", port=7497, client_id=101):
        """Establish connection to IB TWS"""
        try:
            await self.ib.connectAsync(host, port, clientId=client_id)
            self.connection_status = True
            await self.setup_market_data_subscriptions()
            return {"status": "connected", "client_id": client_id}
        except Exception as e:
            return {"status": "error", "message": str(e)}

    async def setup_market_data_subscriptions(self):
        """Subscribe to live market data for configured symbols"""
        symbols = ['EUR.USD', 'GBP.USD', 'USD.JPY']  # From config
        for symbol in symbols:
            contract = Forex(symbol)
            self.ib.reqMktData(contract, '', False, False)
            self.subscribed_symbols.add(symbol)

2. Historical Data Loading (looped single-bar delivery)
class HistoricalDataLoader:
    def __init__(self, ib_connection, bridge):
        self.ib = ib_connection
        self.bridge = bridge  # expects send_ohlc_bar(dict) API

    async def load_weeks_of_data(self, symbol, weeks=4, bar_size='1 min'):
        """Load historical data for specified weeks and bar size, then
        send to Erlang one bar at a time via bridge.send_ohlc_bar()."""
        contract = Forex(symbol)
        duration = f"{weeks} W"  # e.g., "4 W" for 4 weeks

        try:
            bars = await self.ib.reqHistoricalDataAsync(
                contract=contract,
                endDateTime='',           # Current time
                durationStr=duration,
                barSizeSetting=bar_size,
                whatToShow='MIDPOINT',
                useRTH=False,             # Include extended hours
                formatDate=1
            )

            # Convert and stream to Erlang bar-by-bar
            for bar in bars:
                ohlc_bar = {
                    'symbol': symbol,
                    'timestamp': bar.date.isoformat(),
                    'open': float(bar.open),
                    'high': float(bar.high),
                    'low': float(bar.low),
                    'close': float(bar.close),
                    'volume': int(bar.volume),
                    'source': 'historical'
                }
                self.bridge.send_ohlc_bar(ohlc_bar)

            # Optional: return count for logging
            return len(bars)

        except Exception as e:
            print(f"Error loading historical data for {symbol}: {e}")
            return 0

3. Live Tick Aggregation
class LiveTickAggregator:
    def __init__(self, bridge):
        self.current_bars = {}  # symbol -> current 1-min bar
        self.bar_duration = timedelta(minutes=1)
        self.bridge = bridge

    def process_tick(self, symbol, price, volume, timestamp):
        """Process individual tick and aggregate into 1-minute bars"""
        bar_start = self.get_bar_start_time(timestamp)

        if symbol not in self.current_bars:
            # Start new bar
            self.current_bars[symbol] = {
                'symbol': symbol,
                'start_time': bar_start,
                'open': price,
                'high': price,
                'low': price,
                'close': price,
                'volume': volume,
                'tick_count': 1
            }
        else:
            current_bar = self.current_bars[symbol]

            # Check if we need to complete current bar and start new one
            if timestamp >= current_bar['start_time'] + self.bar_duration:
                # Complete current bar
                completed_bar = self.finalize_bar(symbol, current_bar)

                # Send completed bar to Erlang
                self.bridge.send_ohlc_bar({
                    'symbol': symbol,
                    'timestamp': completed_bar['start_time'].isoformat(),
                    'open': completed_bar['open'],
                    'high': completed_bar['high'],
                    'low': completed_bar['low'],
                    'close': completed_bar['close'],
                    'volume': completed_bar['volume'],
                    'source': 'live'
                })

                # Start new bar
                self.start_new_bar(symbol, price, volume, bar_start)
            else:
                # Update current bar
                current_bar['high'] = max(current_bar['high'], price)
                current_bar['low'] = min(current_bar['low'], price)
                current_bar['close'] = price
                current_bar['volume'] += volume
                current_bar['tick_count'] += 1

4. Trade Execution
class TradeExecutor:
    def __init__(self, ib_connection):
        self.ib = ib_connection
        self.pending_orders = {}

    async def execute_trade(self, symbol, action, quantity, order_type='MKT'):
        """Execute trade directly with IB"""
        try:
            contract = Forex(symbol)

            order = MarketOrder(action, quantity)
            if order_type == 'LMT':
                # Add limit price logic as needed
                pass

            trade = self.ib.placeOrder(contract, order)

            # Track order
            order_id = trade.order.orderId
            self.pending_orders[order_id] = {
                'symbol': symbol,
                'action': action,
                'quantity': quantity,
                'status': 'pending',
                'timestamp': datetime.now()
            }

            return {
                'order_id': order_id,
                'status': 'submitted',
                'symbol': symbol,
                'action': action,
                'quantity': quantity
            }

        except Exception as e:
            return {
                'status': 'error',
                'message': str(e)
            }

5. Erlang Communication Bridge (no batch API)
class ErlangBridge:
    def __init__(self):
        self.erlang_port = None

    def send_ohlc_bar(self, ohlc_bar):
        """Send processed OHLC bar to Erlang (historical or live)"""
        message = {
            'type': 'ohlc_bar',
            'data': ohlc_bar
        }
        self.send_to_erlang(message)

    def send_trade_confirmation(self, trade_result):
        """Send trade execution result to Erlang"""
        message = {
            'type': 'trade_confirmation',
            'data': trade_result
        }
        self.send_to_erlang(message)

    # Implement send_to_erlang(message) with framing & acks

Simplified Erlang Architecture
1. live_trading.erl - System Coordinator (SIMPLIFIED)

Purpose: High-level system coordination and neural network management only.

Responsibilities:

Start/stop Python service

Deploy and monitor neural networks

Coordinate system state

Handle user commands

Removed Responsibilities:

IB connection management → Python

Market data processing → Python

Trade execution → Python

Historical data loading → Python

%% Simplified live_trading.erl
-module(live_trading).

%% Simplified API - focus on neural networks only
-export([
    start/0, stop/0,
    start_with_agent/1,
    deploy_agent/1,
    get_system_status/0,
    get_performance/0
]).

-record(live_trading_state, {
    status = stopped,           % System status
    python_service_pid,         % Python service process
    agent_id,                   % Current agent
    exoself_pid,                % Neural network process
    performance_metrics,        % Trading performance
    start_time                  % System start time
}).

start() ->
    %% 1. Start Python IB service
    start_python_service(),
    %% 2. Wait for Python to stream historical bars (per-bar)
    wait_for_historical_data_ready(),
    %% 3. Deploy neural network
    deploy_best_agent(),
    %% 4. Enable trading
    enable_trading().

2. live_scape.erl - Data Interface (SIMPLIFIED)

Purpose: Interface between neural networks and ETS data storage only.

Responsibilities:

Receive OHLC bars from Python (historical + live, same message type)

Store bars in ETS tables

Serve data to neural networks

Handle trade signals from neural networks

Removed Responsibilities:

IB connection management → Python

Tick processing → Python

Historical data loading → Python

Trade execution → Python

%% Simplified live_scape.erl
-module(live_scape).

-export([
    start_link/0,
    handle_ohlc_bar/1,          % Receive from Python
    handle_trade_signal/2,      % Send to Python
    sense/2                     % Serve to neural networks
]).

handle_ohlc_bar(OHLCBar) ->
    Symbol = maps:get(<<"symbol">>, OHLCBar),
    LiveTable = get_live_table_name(Symbol),

    %% Direct ETS upsert by (symbol, timestamp) semantics handled by caller/table key
    ets:insert(LiveTable, #technical{
        id = parse_timestamp(maps:get(<<"timestamp">>, OHLCBar)),
        open = maps:get(<<"open">>, OHLCBar),
        high = maps:get(<<"high">>, OHLCBar),
        low = maps:get(<<"low">>, OHLCBar),
        close = maps:get(<<"close">>, OHLCBar),
        volume = maps:get(<<"volume">>, OHLCBar)
    }).

handle_trade_signal(Signal, Symbol) ->
    TradeCommand = #{
        type => trade_signal,
        symbol => Symbol,
        signal => Signal,  % 1 (long), -1 (short), 0 (close)
        timestamp => erlang:system_time(millisecond)
    },
    send_to_python_service(TradeCommand).

Data Flow
1. System Startup Flow
Erlang live_trading:start()
    ↓
Start Python ib_service.py
    ↓
Python connects to IB TWS
    ↓
Python loads historical data (x weeks, x bar size)
    ↓
Python sends historical OHLC bars to Erlang (one bar per message via send_ohlc_bar)
    ↓
Erlang stores in ETS tables
    ↓
Erlang deploys neural network
    ↓
System ready for live trading

2. Live Trading Flow
IB TWS sends live ticks
    ↓
Python ib_service.py receives ticks
    ↓
Python aggregates ticks to 1-min OHLC bars
    ↓
Python sends completed OHLC bars to Erlang (send_ohlc_bar)
    ↓
Erlang stores in ETS tables
    ↓
Neural network requests data via sensor
    ↓
Erlang serves data from ETS
    ↓
Neural network makes trading decision
    ↓
Erlang sends trade signal to Python
    ↓
Python executes trade with IB TWS
    ↓
Python sends trade confirmation to Erlang

3. Data Storage Flow
Python Historical Data → Erlang ETS Tables (streamed per-bar)
Python Live OHLC Bars → Erlang ETS Tables (per completed minute)
Neural Networks ← Erlang ETS Tables (via sensors)

Implementation Strategy
Phase 1: Enhanced Python Service

Extend ib_service.py with comprehensive IB operations

Implement historical data loading and stream each bar via send_ohlc_bar()

Add live tick aggregation

Create trade execution system

Phase 2: Simplified Erlang Modules

Remove IB operations from Erlang modules

Simplify live_trading.erl to system coordination only

Simplify live_scape.erl to data serving only

Remove ib_bridge_connector.erl entirely

Phase 3: Integration & Testing

Test Python-Erlang communication

Validate neural network compatibility

Performance testing

End-to-end trading workflow testing

Dependencies and Integration
External Dependencies Analysis
config.erl Integration

Status: NO CHANGES REQUIRED
Functions Available: 25+ live trading configuration functions

%% IB Connection Configuration
ib_host/0 -> "host.docker.internal" | ENV_VAR
ib_port/0 -> 7497 | ENV_VAR
ib_client_id/0 -> 101 | ENV_VAR

%% Trading Parameters
live_position_size/0 -> 0.1
live_max_daily_loss/0 -> 0.05
live_currency_pairs/0 -> ['EUR.USD']

%% Risk Management
live_max_position_per_pair/0 -> 0.2
live_max_total_exposure/0 -> 0.5
live_min_account_balance/0 -> 100
live_margin_requirement/0 -> 0.02
live_max_drawdown_limit/0 -> 0.15
live_daily_trade_limit/0 -> 50
live_position_timeout/0 -> 3600

fx.erl Integration

Status: NO CHANGES REQUIRED
Integration Points: Correctly delegates to live_scape.erl

%% Live data delegation (CORRECT)
sense(S, Parameters) ->
    case is_live_table_request(TableName) of
        true -> live_scape:sense(S, Parameters);
        false -> sense_historical(S, Parameters)
    end.

External Integration Points Summary
Modules That Will Reference New live_trading.erl

User Shell Commands: Interactive usage

Documentation: README and guide examples

No other external modules reference live trading functions

Configuration Dependencies

config.erl: Provides configuration TO live trading (no changes needed)

No modules depend on live trading for configuration

Data Dependencies

fx.erl: Delegates live data requests to live_scape.erl (correct)

No modules directly access live trading ETS tables

Cleanup and Migration
Modules to Delete After Consolidation
Primary Modules (3 modules)

live_trading_main.erl (400 lines) - DELETE after migration

live_trading_integration.erl (1070+ lines) - DELETE after consolidation

live_trader.erl (1724+ lines) - DELETE after neural network migration

Temporary Test Modules (16+ modules with "delete" labels)

live_trading_temp_tests.erl (LABEL: delete)

live_trading_test_utils.erl (LABEL: delete)

mock_ib_connector.erl (LABEL: delete)

mock_market_data.erl (LABEL: delete)

test_agent_factory.erl (LABEL: delete)

config_test_utils.erl (LABEL: delete)

error_handling_tests.erl (LABEL: delete)

integration_test_suite.erl (LABEL: delete)

e2e_trading_tests.erl (LABEL: delete)

parallel_test_framework.erl (LABEL: delete)

behavior_comparison.erl (LABEL: delete)

migration_utils.erl (LABEL: delete)

backup_restore.erl (LABEL: delete)

Registered Processes to Clean Up

live_trading_integration - REMOVE after consolidation

live_trading_supervisor - REMOVE after consolidation

live_trader - REMOVE after consolidation

ETS Tables to Transfer

live_trade_history - Transfer from live_trader.erl to live_trading.erl

live_performance_snapshots - Transfer ownership

backtesting_comparison - Transfer ownership

Documentation Updates Required

README.md - Update 2 process name references

.ReadMe_Files/LIVE_TRADING_README.md - Update API documentation

Cleanup Execution Order

Pre-Migration Backup: Backup all existing modules

Module Consolidation: Create and test new module

Process Migration: Transfer processes and resources

File Cleanup: Remove old modules and temporary files

Documentation Updates: Update references

Verification: Comprehensive testing

Benefits of Python-Centric Architecture
1. Dramatic Simplification

Erlang code reduced by 60% (focus only on neural networks)

No IB connection complexity in Erlang

No tick processing in Erlang

No trade execution logic in Erlang

2. Better Performance

Python handles data processing (pandas, numpy efficiency where appropriate, but no pandas in hot path)

Reduced Erlang-Python communication (only OHLC bars)

No real-time tick processing in Erlang

Optimized data aggregation in Python

3. Enhanced Reliability

Python manages all IB operations (single point of control)

Erlang focuses on neural networks (its strength)

Clear separation of concerns

Easier debugging and monitoring

4. Maintainability

Python code easier to modify for IB changes

Erlang code purely neural network focused

Independent testing of Python and Erlang components

Clear interfaces between components

This Python-centric architecture represents a revolutionary simplification that leverages each language's strengths while dramatically reducing system complexity and maintaining 100% neural network compatibility.

