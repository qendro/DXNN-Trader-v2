# Live Trading System - User Guide

## Overview

This guide explains how to run the neuroevolutionary live trading system for forex markets. The system deploys evolved neural network agents to make automated trading decisions in a paper trading environment.

⚠️ **IMPORTANT**: This system is configured for **PAPER TRADING ONLY** for safety.

## Prerequisites

Before running the live trading system, ensure you have:

1. **Interactive Brokers TWS or Gateway** running in paper trading mode
2. **Mnesia database** with trained agents (from evolution runs)
3. **Docker** environment set up
4. **Evolved agents** in the database (run `benchmarker:start(forex_trader)` first if needed)

## Step-by-Step Setup

### 1. Start Interactive Brokers (Paper Trading)

1. Launch IB TWS (Trader Workstation) or IB Gateway
2. Configure for paper trading:
   - **Host**: 127.0.0.1 (localhost)
   - **Port**: 7497 (paper trading port)
   - **API Settings**: Enable API connections
   - **Client ID**: Allow client ID 1

### 2. Start Docker Environment

```bash
# Build the Docker image (if not already built)
docker build -t erlang-dev .

# Run the Docker container
# For macOS/Windows (recommended):
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev

# For Linux (alternative with host networking):
docker run -it --rm -v ${PWD}:/app -w /app --network host erlang-dev

# For troubleshooting (with environment variable):
docker run -it --rm -v ${PWD}:/app -w /app -e DOCKER_ENV=1 erlang-dev
```

**Note**: The system is configured to use `"host.docker.internal"` for Docker networking. This works automatically on macOS and Windows. For Linux, use the `--network host` option.

### 3. Initialize the System

In the Erlang shell:

```erlang
% 1. Compile all modules
make:all([load]).

% 2. Start Mnesia database
mnesia:create_schema([node()]).
mnesia:start().

% 3. Initialize forex data (if needed)
fx:init().
fx:start().
```

### 4. Run the Live Trading System

#### Option A: Quick Start (Automatic Agent Selection)
```erlang
% Start with the best available agent
live_trading_main:start().
```

#### Option B: Start with Specific Agent
```erlang
% First, list available agents
live_trading_main:list_agents().

% Start with a specific agent ID
live_trading_main:start_with_agent(AgentId).
```

#### Option C: Quick Command
```erlang
% Even quicker start
live_trading_main:go().
```

## System Commands

### Basic Operations

```erlang
% Check system status
live_trading_main:status().
% Quick version:
live_trading_main:st().

% Get performance metrics
live_trading_main:performance().
% Quick version:
live_trading_main:perf().

% Stop the system gracefully
live_trading_main:stop().
% Quick version:
live_trading_main:halt().

% Emergency stop (immediate shutdown)
live_trading_main:emergency_stop().
```

### System Management

```erlang
% Restart the system
live_trading_main:restart().

% Show current configuration
live_trading_main:show_config().

% Validate configuration
live_trading_main:validate_config().

% Run system diagnostics
live_trading_main:diagnostics().

% Show all available commands
live_trading_main:help().
```

### Agent Management

```erlang
% List all available agents
live_trading_main:list_agents().

% Get detailed agent information
live_trading_main:agent_info(AgentId).
```

### Performance Monitoring

```erlang
% Get basic performance summary
live_trading_main:performance().

% Get detailed performance report
live_trading_main:performance_report().
```

## Testing Before Live Use

### Test IB Connection Fixes

```erlang
% Test the new IB connection fixes
test_ib_fixes:test_all().

% Quick connectivity test
test_ib_fixes:quick_test().

% Test IB connection specifically
live_trading_main:test_ib_connection().
```

### Run System Tests

```erlang
% Quick validation tests
live_trading_main:test().

% Full integration tests
live_trading_main:test_full().

% Test specific components
live_trading_main:test_component(ib_connector).
live_trading_main:test_component(live_scape).
live_trading_main:test_component(live_trader).
```

### Run Comprehensive Diagnostics

```erlang
% Run full system diagnostics (includes IB connectivity test)
live_trading_main:diagnostics().
```

## Complete Example Session

```bash
# 1. Start Docker environment
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
```

```erlang
% 2. In Erlang shell - Initialize system
make:all([load]).
mnesia:start().

% 3. Validate setup
live_trading_main:validate_config().
live_trading_main:test().

% 4. Check available agents
live_trading_main:list_agents().

% 5. Start live trading
live_trading_main:start().

% 6. Monitor the system
live_trading_main:status().
live_trading_main:performance().

% 7. Stop when done
live_trading_main:stop().
```

## Expected Output

### Successful Startup
```
=== LIVE TRADING STARTED ===
Agent ID: {agent_id, timestamp}
IB Host: 127.0.0.1
IB Port: 7497
Currency Pairs: ['EUR.USD']
Position Size: 10.0%
Max Daily Loss: 5.0%
===========================
```

### System Status
```
=== SYSTEM STATUS ===
Status: running
Agent ID: {agent_id, timestamp}
Uptime: 120.5 seconds
Components:
  ib_connector: running
  live_scape: running
  live_trader: running
====================
```

### Performance Summary
```
=== PERFORMANCE SUMMARY ===
Performance data: {performance_metrics, ...}
===========================
```

## Configuration

### Key Configuration Parameters

```erlang
% IB Connection Settings
config:ib_host().           % "127.0.0.1"
config:ib_port().           % 7497 (paper trading)
config:ib_client_id().      % 1

% Risk Management
config:live_position_size().      % 0.1 (10% of account)
config:live_max_daily_loss().     % 0.05 (5% max daily loss)
config:live_currency_pairs().     % ['EUR.USD']

% Trading Parameters
config:account_initial_balance(). % 300
config:account_leverage().        % 50
config:account_lot_size().        % 10000
```

### Modify Configuration

Edit `config.erl` to change trading parameters:

```erlang
% Example: Change position size to 5%
live_position_size() -> 0.05.

% Example: Add more currency pairs
live_currency_pairs() -> ['EUR.USD', 'GBP.USD'].
```

## Troubleshooting

### IB Connection Issues

```erlang
% Check IB configuration
config:ib_host().      % Should be "host.docker.internal" in Docker
config:ib_port().      % Should be 7497 (paper trading)
config:ib_client_id(). % Should be 1

% Test basic connectivity
ib_connector:test_connectivity().

% Test full connection with handshake
live_trading_main:test_ib_connection().

% Run comprehensive diagnostics
live_trading_main:diagnostics().
```

**Common Solutions**:
- Ensure IB TWS/Gateway is running
- Check API settings are enabled in IB
- Verify paper trading mode is active
- Confirm port 7497 is not blocked by firewall
- For Docker: ensure `host.docker.internal` resolves correctly
- For Linux Docker: use `--network host` flag

**Docker-Specific Issues**:
- **macOS/Windows**: `host.docker.internal` should work automatically
- **Linux**: Use `--network host` or set `DOCKER_ENV=1` environment variable
- **Connection refused**: Check if TWS is running and accepting connections
- **Handshake timeout**: Verify IB API version compatibility

### No Agents Available

```erlang
% Check if agents exist in database
genotype_utils:list_all_agents().

% If no agents, run evolution first
benchmarker:start(forex_trader).
```

### System Won't Start

```erlang
% Run comprehensive diagnostics
live_trading_main:diagnostics().

% Test individual components
live_trading_main:test_component(ib_connector).
live_trading_main:test_component(live_scape).
live_trading_main:test_component(live_trader).

% Check configuration
live_trading_main:validate_config().
```

### Database Issues

```erlang
% Check Mnesia status
mnesia:system_info(is_running).

% Restart Mnesia if needed
mnesia:stop().
mnesia:start().
```

### Performance Issues

```erlang
% Check system resources
live_trading_main:diagnostics().

% Monitor component health
live_trading_integration:get_system_status().
```

## Safety Features

### Built-in Safety Mechanisms

- **Paper Trading Only**: System enforces paper trading port (7497)
- **Position Limits**: Maximum position size per trade
- **Daily Loss Limits**: Automatic shutdown on daily loss threshold
- **Emergency Stop**: Immediate system shutdown capability
- **Risk Management**: Built-in position and exposure limits
- **Error Handling**: Comprehensive error detection and recovery

### Risk Parameters

```erlang
% Default risk settings
live_position_size() -> 0.1.          % 10% max per trade
live_max_daily_loss() -> 0.05.        % 5% max daily loss
live_max_position_per_pair() -> 0.2.   % 20% max per currency pair
live_max_total_exposure() -> 0.5.      % 50% max total exposure
live_daily_trade_limit() -> 50.        % Max 50 trades per day
```

## System Architecture

### Process Hierarchy
```
live_trading_supervisor
├── ib_connector (IB API communication)
├── live_scape (sensor/actuator interface)
└── live_trader (trading orchestration)
    └── neural_network (exoself + neurons)
```

### Data Flow
```
Market Data → IB Connector → Live Scape → Neural Network → Trading Decisions → IB Connector → Market
```

## Advanced Usage

### Custom Agent Selection

```erlang
% Find agents by specific criteria
Agents = genotype_utils:list_all_agents(),
BestAgent = lists:max(fun(A, B) -> A#agent.fitness > B#agent.fitness end, Agents),
live_trading_main:start_with_agent(BestAgent#agent.id).
```

### Performance Analysis

```erlang
% Get detailed performance metrics
{ok, Report} = live_trading_main:performance_report(),
TradeHistory = maps:get(trade_history, Report),
PerformanceMetrics = maps:get(performance_metrics, Report).
```

### System Monitoring

```erlang
% Continuous monitoring loop
monitor_system() ->
    {ok, Status} = live_trading_main:status(),
    io:format("System Status: ~p~n", [Status]),
    timer:sleep(30000), % Wait 30 seconds
    monitor_system().
```

## Support and Maintenance

### Log Files

- System logs are displayed in the Erlang console
- Critical errors are logged with timestamps
- Trade executions are recorded with full details

### Backup and Recovery

```erlang
% Backup Mnesia database
mnesia:backup("backup_file").

% Restore from backup
mnesia:restore("backup_file", []).
```

### System Updates

1. Stop the live trading system
2. Update code files
3. Recompile: `make:all([load])`
4. Restart the system

## Getting Help

```erlang
% Show all available commands
live_trading_main:help().

% Run diagnostics for issues
live_trading_main:diagnostics().

% Test system components
live_trading_main:test().
```

For additional support, check the system logs and error messages for specific guidance on resolving issues.

---

**Remember**: This system is designed for paper trading only. Always verify you're connected to the paper trading port (7497) before starting the system.