# Performance Monitoring Usage Guide

## Overview

The live trader now includes comprehensive performance monitoring and reporting capabilities that track real-time trading metrics and compare them with backtesting results.

## Key Features

### 1. Real-Time Performance Tracking
- **Trade History**: All trades are recorded with timestamps, symbols, actions, quantities, prices, and P&L
- **Performance Snapshots**: Periodic snapshots of performance metrics for trend analysis
- **Enhanced Metrics**: Win rate, Sharpe ratio, profit factor, drawdown analysis, and more

### 2. Performance Comparison
- **Backtesting Comparison**: Compare live performance with historical backtesting results
- **Performance Classification**: Automatic categorization (excellent, good, poor, etc.)
- **Deviation Detection**: Identify significant performance differences from expectations

### 3. Risk Metrics
- **Drawdown Analysis**: Current and maximum drawdown tracking
- **Consecutive Loss Tracking**: Monitor losing streaks
- **Recovery Factor**: Measure ability to recover from losses

## Usage Examples

### Getting Performance Metrics

```erlang
%% Get comprehensive performance report
{ok, Performance} = live_trader:get_performance().

%% Access specific metrics
WinRate = maps:get(win_rate, Performance),
TotalPnL = maps:get(total_pnl, Performance),
SharpeRatio = maps:get(sharpe_ratio, Performance),
MaxDrawdown = maps:get(max_drawdown, Performance).
```

### Getting Detailed Performance Report

```erlang
%% Get full performance report including trade history
{ok, Report} = live_trader:get_performance_report().

%% Extract components
Metrics = maps:get(performance_metrics, Report),
TradeHistory = maps:get(trade_history, Report),
Snapshots = maps:get(performance_snapshots, Report).
```

### Comparing with Backtesting Results

```erlang
%% Compare current live performance with backtesting
AgentId = my_agent_123,
{ok, Comparison} = live_trader:get_performance_comparison(AgentId).

%% Check performance status
Status = maps:get(performance_status, Comparison),
PerformanceRatio = maps:get(performance_ratio, Comparison),
WinRateDiff = maps:get(win_rate_difference, Comparison).
```

## Available Metrics

### Basic Metrics
- `start_time` - When trading session started
- `total_trades` - Total number of trades executed
- `winning_trades` - Number of profitable trades
- `total_pnl` - Total profit/loss
- `daily_pnl` - Profit/loss for current day
- `max_drawdown` - Maximum drawdown experienced
- `current_position` - Current position size

### Enhanced Metrics
- `win_rate` - Percentage of winning trades
- `avg_trade_pnl` - Average profit/loss per trade
- `sharpe_ratio` - Risk-adjusted return measure
- `max_consecutive_losses` - Longest losing streak
- `current_drawdown` - Current drawdown from peak
- `profit_factor` - Gross profit / gross loss ratio
- `recovery_factor` - Net profit / max drawdown ratio

### Session Metrics
- `session_duration` - Trading session duration in hours
- `trades_per_hour` - Trading frequency

## Performance Comparison Metrics

### Comparison Data
- `live_pnl` - Current live trading P&L
- `backtest_fitness` - Historical backtesting fitness
- `performance_ratio` - Live P&L / backtest fitness
- `performance_difference` - Absolute difference
- `win_rate_difference` - Difference in win rates
- `drawdown_difference` - Difference in maximum drawdowns

### Performance Status Classifications
- `excellent` - Performing at or above backtesting expectations
- `good_but_risky` - Good returns but higher risk
- `profitable_but_inconsistent` - Profitable but lower win rate
- `underperforming` - Below backtesting expectations
- `poor` - Significantly underperforming
- `mixed_results` - Mixed performance indicators

## Integration with Live Trading

The performance monitoring is automatically integrated with the live trading system:

1. **Automatic Recording**: All trades are automatically recorded for performance tracking
2. **Real-Time Updates**: Metrics are updated after each trade execution
3. **Risk Integration**: Performance metrics feed into risk management decisions
4. **Periodic Snapshots**: Performance snapshots are created at regular intervals

## Testing

Use the provided test modules to verify functionality:

```erlang
%% Test basic performance monitoring
test_performance_monitoring:run_all_tests().

%% Test integration with live trader
test_live_performance_integration:run_all_tests().
```

## Data Storage

Performance data is stored in ETS tables:
- `live_trade_history` - Individual trade records
- `live_performance_snapshots` - Performance snapshots over time
- `backtesting_comparison` - Comparison data with backtesting results

Tables are automatically created when live trading starts and cleaned up when trading stops.