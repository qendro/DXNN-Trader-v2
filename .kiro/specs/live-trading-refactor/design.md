# Design Document

## Overview

This design document outlines the consolidation approach for the existing live trading system. The current system has 5 interdependent modules with overlapping responsibilities, race conditions, and complex state management. We will consolidate these into 3 focused modules that eliminate complexity while maintaining full neural network compatibility.

## Current System Analysis

### Current Problems
- **5 modules with overlapping responsibilities**
- **Race conditions in startup sequence** (6 sequential steps with timing issues)
- **Scattered state management** (multiple sources of truth)
- **Complex inter-module communication** (supervisor trees, message passing)
- **Inconsistent error handling** across modules
- **Difficult debugging** due to distributed logic

### Current Module Issues

| Module | Current Issues | Lines of Code | Complexity |
|--------|---------------|---------------|------------|
| `live_trading_main.erl` | Mixed responsibilities, direct component access | ~400 | Medium |
| `live_trading_integration.erl` | Complex startup, supervisor overhead | ~1000+ | High |
| `live_trader.erl` | Overlapping with integration, complex state | ~1700+ | High |
| `live_scape.erl` | Good implementation, some error handling gaps | ~400 | Low |
| `ib_bridge_connector.erl` | Good Python bridge, some unnecessary complexity | ~600 | Medium |

## Consolidated Architecture

### New Architecture: 5 → 3 Modules

```
┌─────────────────────────────────────────────────────────────────┐
│                    Consolidated Live Trading System             │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐ │
│  │                live_trading.erl                             │ │
│  │                (CONSOLIDATED CORE)                          │ │
│  │  ┌─────────────┐ ┌─────────────┐ ┌─────────────────────────┐ │ │
│  │  │User Interface│ │Orchestration│ │Neural Network Management│ │ │
│  │  │(from main)   │ │(from integ) │ │(from trader)            │ │ │
│  │  └─────────────┘ └─────────────┘ └─────────────────────────┘ │ │
│  │  ┌─────────────────────────────────────────────────────────┐ │ │
│  │  │           Centralized State Management                  │ │ │
│  │  └─────────────────────────────────────────────────────────┘ │ │
│  └─────────────────────────────────────────────────────────────┘ │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐                    ┌─────────────────┐     │
│  │ live_scape.erl  │                    │ ib_bridge_      │     │
│  │ (ENHANCED)      │                    │ connector.erl   │     │
│  │                 │                    │ (SIMPLIFIED)    │     │
│  │ - Market data   │ ←──── Data ─────→  │ - IB connection │     │
│  │ - NN interface  │                    │ - Market data   │     │
│  │ - Trade exec    │ ←─── Orders ────→  │ - Order mgmt    │     │
│  └─────────────────┘                    └─────────────────┘     │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │     Mnesia      │  │   ETS Tables    │  │   IB TWS        │  │
│  │ (Agent Storage) │  │ (Market Data)   │  │ (via Python)    │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

### Data Flow (Simplified)

```
Startup Flow:
live_trading.erl → start components sequentially → ready

Market Data Flow:
IB TWS → ib_bridge_connector → live_scape → neural network

Trading Decision Flow:
neural network → live_scape → ib_bridge_connector → IB TWS

State Management Flow:
All state → live_trading.erl (single source of truth)

Error Handling Flow:
Any error → live_trading.erl → coordinated recovery
```

## Module Specifications

### 1. live_trading.erl - Consolidated Core Module (NEW)

**Purpose**: Single core module that consolidates user interface, system orchestration, and neural network management.

#### Consolidates From:
- **live_trading_main.erl**: User interface and API functions
- **live_trading_integration.erl**: System orchestration and startup logic
- **live_trader.erl**: Neural network deployment and management

#### Core Responsibilities:

##### User Interface Layer
```erlang
%% Public API (consolidated from live_trading_main.erl)
start() -> {ok, started} | {error, Reason}
start_with_agent(AgentId) -> {ok, started} | {error, Reason}
stop() -> {ok, stopped} | {error, Reason}
emergency_stop() -> {ok, emergency_stopped} | {error, Reason}
restart() -> {ok, restarted} | {error, Reason}

%% Status and monitoring
status() -> {ok, Status} | {error, Reason}
performance() -> {ok, Performance} | {error, Reason}
diagnostics() -> {ok, Diagnostics} | {error, Reason}
market_status() -> {ok, MarketData} | {error, Reason}

%% Agent management
list_agents() -> {ok, Agents} | {error, Reason}
agent_info(AgentId) -> {ok, AgentInfo} | {error, Reason}
find_best_agent() -> {ok, AgentId} | {error, Reason}

%% Configuration
show_config() -> {ok, Config}
validate_config() -> ok | {error, Reason}

%% Quick commands
go() -> start()
halt() -> stop()
st() -> status()
ms() -> market_status()
perf() -> performance()
help() -> ok
```

##### System Orchestration Layer
```erlang
%% System lifecycle (consolidated from live_trading_integration.erl)
start_system_internal(AgentId) -> {ok, State} | {error, Reason}
stop_system_internal() -> {ok, stopped} | {error, Reason}
restart_system_internal() -> {ok, restarted} | {error, Reason}

%% Simplified startup sequence (replaces complex 6-step process)
execute_startup_sequence(AgentId) ->
    Steps = [
        {validate_config, fun validate_system_config/0},
        {start_ib_connector, fun start_ib_bridge/0},
        {start_live_scape, fun start_market_data_manager/0},
        {deploy_neural_network, fun() -> deploy_agent_internal(AgentId) end},
        {enable_trading, fun enable_trading_operations/0}
    ],
    execute_steps_sequentially(Steps).

%% Component management
start_ib_bridge() -> {ok, Pid} | {error, Reason}
start_market_data_manager() -> {ok, Pid} | {error, Reason}
monitor_components() -> ok
handle_component_failure(Component, Reason) -> ok
```

##### Neural Network Management Layer
```erlang
%% Neural network operations (consolidated from live_trader.erl)
deploy_agent_internal(AgentId) -> {ok, ExoselfPid} | {error, Reason}
start_neural_network(AgentId) -> {ok, Pid} | {error, Reason}
monitor_neural_network(Pid) -> ok
handle_neural_network_message(Message, State) -> {noreply, NewState}

%% Performance tracking
calculate_performance_metrics(State) -> Metrics
update_performance_data(TradeData, State) -> NewState
get_performance_report() -> {ok, Report} | {error, Reason}

%% Risk management
check_risk_limits(State) -> ok | {error, RiskViolation}
update_risk_state(TradeData, State) -> NewState
handle_risk_violation(Violation, State) -> NewState
```

#### State Management (Centralized)
```erlang
-record(live_trading_state, {
    %% System status
    status = stopped,           % stopped | starting | running | stopping | error
    agent_id,                   % Currently deployed agent ID
    start_time,                 % System start timestamp
    
    %% Component PIDs and status
    ib_connector_pid,           % IB bridge connector process
    live_scape_pid,             % Market data and trading process
    exoself_pid,                % Neural network process
    
    %% Component status tracking
    ib_status = disconnected,   % IB connection status
    scape_status = stopped,     % Live scape status
    nn_status = not_deployed,   % Neural network status
    
    %% Trading state
    positions = [],             % Current positions [#position{}]
    pending_orders = [],        % Pending orders [#order{}]
    trading_active = false,     % Trading enabled flag
    
    %% Performance and risk
    performance_metrics = #performance_metrics{},
    risk_state = #risk_state{},
    
    %% Configuration
    config = #live_trading_config{},
    
    %% Error tracking and recovery
    errors = [],                % Recent errors [#error_info{}]
    recovery_attempts = 0,      % Current recovery attempt count
    last_error_time = 0,        % Last error timestamp
    
    %% Market data cache (for quick access)
    market_data_cache = #{},    % Symbol -> latest tick
    
    %% Statistics
    uptime_start,               % Uptime calculation start
    total_trades = 0,           % Total trades executed
    last_trade_time = 0         % Last trade timestamp
}).
```

#### Internal Process Structure
```erlang
%% Main process loop
main_loop(State) ->
    receive
        %% User API calls
        {From, start} -> handle_start_request(From, State);
        {From, stop} -> handle_stop_request(From, State);
        {From, status} -> handle_status_request(From, State);
        
        %% Component messages
        {ib_connector, connected} -> handle_ib_connected(State);
        {ib_connector, disconnected, Reason} -> handle_ib_disconnected(Reason, State);
        {live_scape, market_data, Tick} -> handle_market_data(Tick, State);
        {live_scape, trade_executed, Trade} -> handle_trade_executed(Trade, State);
        {exoself, evaluation_completed, Result} -> handle_nn_result(Result, State);
        
        %% Error handling
        {'EXIT', Pid, Reason} -> handle_component_exit(Pid, Reason, State);
        {error_report, Component, Error} -> handle_error_report(Component, Error, State);
        
        %% Periodic tasks
        {timeout, health_check} -> perform_health_check(State);
        {timeout, performance_update} -> update_performance_metrics(State);
        
        %% System messages
        system_shutdown -> handle_system_shutdown(State);
        
        Other -> handle_unknown_message(Other, State)
    after 5000 ->
        %% Periodic health check every 5 seconds
        perform_periodic_tasks(State)
    end.
```

### 2. live_scape.erl - Market Data & Trading Interface (ENHANCED)

**Purpose**: Manages market data and provides the neural network sensor/actuator interface. This module is essential for maintaining compatibility with your existing neural network agents.

#### Current Strengths (Keep):
- **Excellent neural network interface** - maintains compatibility with existing agents
- **Good market data management** - historical preloading and real-time processing
- **Clean trade execution** - proper order handling and position tracking
- **ETS table management** - efficient data storage and retrieval

#### Enhancements Needed:

##### Enhanced Error Handling
```erlang
%% Current: Basic error handling
handle_trade(Signal, State) ->
    case Signal of
        1 -> open_position(1, State);
        -1 -> open_position(-1, State);
        0 -> close_position(State)
    end.

%% Enhanced: Comprehensive error handling
handle_trade(Signal, State) ->
    try
        case validate_trade_signal(Signal, State) of
            ok ->
                case validate_market_conditions(State) of
                    ok -> execute_trade_with_confirmation(Signal, State);
                    {error, MarketReason} -> 
                        report_error(market_conditions, MarketReason),
                        {0, 0, State}
                end;
            {error, SignalReason} ->
                report_error(invalid_signal, SignalReason),
                {0, 0, State}
        end
    catch
        Error:Reason ->
            report_error(trade_execution, {Error, Reason}),
            {0, 0, State}
    end.
```

##### Enhanced Data Validation
```erlang
%% Enhanced market data validation
validate_market_tick(Tick) ->
    Validations = [
        fun validate_price_range/1,
        fun validate_timestamp/1,
        fun validate_bid_ask_spread/1,
        fun validate_volume/1
    ],
    case run_validations(Tick, Validations) of
        ok -> {ok, Tick};
        {error, Reason} -> {error, {data_quality, Reason}}
    end.

%% Data quality monitoring
monitor_data_quality(Symbol, Tick, State) ->
    Quality = calculate_data_quality(Tick),
    update_quality_metrics(Symbol, Quality, State).
```

##### Enhanced Position Management
```erlang
%% Enhanced position tracking with risk metrics
-record(enhanced_position, {
    symbol,                     % Trading symbol
    side,                      % long | short
    quantity,                  % Position size
    entry_price,               % Entry price
    entry_time,                % Entry timestamp
    current_price,             % Current market price
    unrealized_pnl = 0.0,      % Unrealized P&L
    realized_pnl = 0.0,        % Realized P&L
    max_favorable = 0.0,       % Maximum favorable excursion
    max_adverse = 0.0,         % Maximum adverse excursion
    risk_metrics = #{},        % Risk calculations
    trade_id                   % Unique trade identifier
}).
```

#### Neural Network Interface (Maintain Compatibility)
```erlang
%% Existing sensor interface (keep unchanged)
handle_sense_request(TableName, Feature, Parameters, State) ->
    case Parameters of
        [HRes, list_sensor] ->
            {PriceList, NewState} = get_price_list(TableName, HRes, State),
            {[Close || {_O, Close, _H, _L} <- PriceList], NewState};
        [HRes, VRes, graph_sensor] ->
            {PriceList, NewState} = get_price_list(TableName, HRes, State),
            {encode_to_plane(HRes, VRes, PriceList), NewState};
        _ ->
            {[], State}
    end.

%% Existing actuator interface (keep unchanged)
handle_trade_request(TradeSignal, State) ->
    case TradeSignal of
        1 -> open_long_position(State);
        -1 -> open_short_position(State);
        0 -> close_current_position(State);
        _ -> {0, 0, State}  % Invalid signal
    end.
```

### 3. ib_bridge_connector.erl - IB Interface (SIMPLIFIED)

**Purpose**: Clean, focused interface to Interactive Brokers via Python bridge. This module is already well-implemented but needs simplification.

#### Current Strengths (Keep):
- **Excellent Python bridge architecture** - reliable ib_insync integration
- **Good connection management** - heartbeat monitoring and auto-reconnection
- **Proper message protocol** - JSON with {packet,4} framing
- **Error categorization** - structured error handling

#### Simplifications Needed:

##### Remove Unnecessary Complexity
```erlang
%% Current: Complex state with many fields
-record(bridge_state, {
    port,
    next_cid = 1,
    connection_status = false,
    last_heartbeat = 0,
    python_pid = undefined,
    market_tickers = #{},
    subscriptions = [],
    pending_orders = #{},
    order_confirmations = [],
    account_info = #{},
    error_count = 0,
    reconnect_attempts = 0
}).

%% Simplified: Focus on core functionality
-record(bridge_state, {
    port,                       % Python process port
    connection_status = false,  % Connection status
    subscriptions = [],         % Active market data subscriptions
    last_heartbeat = 0,        % Last heartbeat timestamp
    reconnect_attempts = 0     % Reconnection attempt counter
}).
```

##### Simplified API
```erlang
%% Core connection management
start_link() -> {ok, Pid} | {error, Reason}
connect(Host, Port, ClientId) -> ok | {error, Reason}
disconnect() -> ok
get_connection_status() -> connected | disconnected | {error, Reason}

%% Market data (simplified)
subscribe_market_data(Symbol) -> ok | {error, Reason}
unsubscribe_market_data(Symbol) -> ok | {error, Reason}
get_market_data(Symbol) -> {ok, Tick} | {error, Reason}

%% Order management (simplified)
place_order(Symbol, Action, Quantity, OrderType) -> {ok, OrderId} | {error, Reason}
get_order_status(OrderId) -> {ok, Status} | {error, Reason}

%% Remove complex functions
%% - get_pending_orders/0 (move to live_trading.erl)
%% - get_order_confirmations/0 (move to live_trading.erl)
%% - wait_for_order_confirmation/2 (move to live_trading.erl)
%% - init_market_data_tables/0 (move to live_scape.erl)
```

##### Focus on Core Messaging
```erlang
%% Simplified message handling
handle_python_message(Message, State) ->
    Type = maps:get(<<"type">>, Message, undefined),
    case Type of
        "connected" -> handle_connected(State);
        "tick" -> handle_market_tick(Message, State);
        "order_status" -> handle_order_status(Message, State);
        "error" -> handle_error(Message, State);
        "beat" -> handle_heartbeat(Message, State);
        _ -> {noreply, State}
    end.
```

## Data Models

### Centralized Configuration
```erlang
-record(live_trading_config, {
    %% IB Connection
    ib_host = "host.docker.internal",
    ib_port = 7497,
    ib_client_id = 101,
    
    %% Trading Parameters
    symbols = ['EUR.USD'],              % Currency pairs to trade
    position_size = 0.1,                % Position size (10% of account)
    max_daily_loss = 0.05,              % Max daily loss (5%)
    
    %% System Parameters
    startup_timeout = 30000,            % Startup timeout (30 seconds)
    heartbeat_interval = 3000,          % Heartbeat interval (3 seconds)
    data_preload_duration = {weeks, 1}, % Historical data preload
    
    %% Risk Management
    max_drawdown = 0.15,                % Max drawdown before halt (15%)
    daily_trade_limit = 50,             % Max trades per day
    emergency_stop_enabled = true,      % Enable emergency stop
    
    %% Performance
    performance_update_interval = 60000, % Performance update (1 minute)
    health_check_interval = 5000,       % Health check (5 seconds)
    
    %% Data Management
    max_cache_size = 10000,             % Max ETS cache size
    data_quality_threshold = 0.95,      % Data quality threshold
    archive_after_days = 7              % Archive data after 7 days
}).
```

### Performance Metrics
```erlang
-record(performance_metrics, {
    start_time,                         % Trading start time
    total_trades = 0,                   % Total number of trades
    winning_trades = 0,                 % Number of winning trades
    losing_trades = 0,                  % Number of losing trades
    total_pnl = 0.0,                   % Total P&L
    daily_pnl = 0.0,                   % Daily P&L
    max_drawdown = 0.0,                % Maximum drawdown
    current_drawdown = 0.0,            % Current drawdown
    win_rate = 0.0,                    % Win rate percentage
    avg_win = 0.0,                     % Average winning trade
    avg_loss = 0.0,                    % Average losing trade
    profit_factor = 0.0,               % Profit factor
    sharpe_ratio = 0.0,                % Sharpe ratio
    max_consecutive_wins = 0,          % Max consecutive wins
    max_consecutive_losses = 0,        % Max consecutive losses
    last_update                        % Last update timestamp
}).
```

### Risk Management
```erlang
-record(risk_state, {
    daily_start_balance,               % Balance at start of day
    daily_pnl = 0.0,                  % Daily P&L
    daily_trades = 0,                 % Daily trade count
    max_drawdown = 0.0,               % Maximum drawdown
    current_drawdown = 0.0,           % Current drawdown
    position_exposures = [],          % Position exposure list
    total_exposure = 0.0,             % Total exposure amount
    last_reset_date,                  % Last daily reset date
    risk_violations = [],             % Risk violation history
    emergency_stop_triggered = false  % Emergency stop status
}).
```

## Benefits of Consolidation

### Complexity Reduction
- **5 modules → 3 modules** (40% reduction in module count)
- **~3000+ lines → ~2000 lines** (estimated 30% code reduction)
- **No inter-module race conditions** (sequential startup)
- **Single source of truth** for all system state
- **Simplified debugging** (all core logic in one place)

### Performance Improvements
- **Reduced inter-process communication** (no message passing between main/integration/trader)
- **Faster startup** (no complex component coordination)
- **Lower memory overhead** (consolidated state management)
- **Simplified monitoring** (single process to monitor)

### Maintainability Improvements
- **Easier to understand** (all core logic in one module)
- **Consistent error handling** (unified error patterns)
- **Simplified testing** (fewer integration points)
- **Better observability** (centralized logging and metrics)

### Neural Network Compatibility
- **Maintains existing sensor/actuator interface** in live_scape.erl
- **Preserves agent deployment patterns** (exoself integration)
- **Keeps existing data formats** (price lists, OHLC data)
- **No changes to neural network code** required

This consolidated design dramatically simplifies your live trading system while maintaining full compatibility with your existing neural network infrastructure.