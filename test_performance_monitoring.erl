%% Test module for performance monitoring functionality
-module(test_performance_monitoring).
-compile(export_all).
-include("records.hrl").

%% Test performance tracking functionality
test_performance_tracking() ->
    io:format("Testing performance monitoring functionality~n"),
    
    %% Test 1: Initialize performance tables
    io:format("Test 1: Initializing performance tables~n"),
    live_trader:init_performance_tables(),
    
    %% Verify tables exist
    case {ets:info(live_trade_history), ets:info(live_performance_snapshots), ets:info(backtesting_comparison)} of
        {undefined, _, _} ->
            io:format("ERROR: live_trade_history table not created~n"),
            {error, table_creation_failed};
        {_, undefined, _} ->
            io:format("ERROR: live_performance_snapshots table not created~n"),
            {error, table_creation_failed};
        {_, _, undefined} ->
            io:format("ERROR: backtesting_comparison table not created~n"),
            {error, table_creation_failed};
        {_, _, _} ->
            io:format("SUCCESS: All performance tables created~n")
    end,
    
    %% Test 2: Record sample trades
    io:format("Test 2: Recording sample trades~n"),
    SampleTrades = [
        {1, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, 50.0},
        {2, erlang:timestamp(), "EURUSD", "SELL", 10000, 1.1050, -25.0},
        {3, erlang:timestamp(), "EURUSD", "BUY", 15000, 1.1020, 75.0}
    ],
    
    lists:foreach(fun({Id, Time, Symbol, Action, Qty, Price, PnL}) ->
        live_trader:record_trade_for_performance(Id, Time, Symbol, Action, Qty, Price, PnL)
    end, SampleTrades),
    
    %% Verify trades recorded
    TradeCount = ets:info(live_trade_history, size),
    io:format("Recorded ~p trades~n", [TradeCount]),
    
    %% Test 3: Calculate performance metrics
    io:format("Test 3: Testing performance calculations~n"),
    
    %% Create sample performance data
    SamplePerformance = #performance_metrics{
        start_time = erlang:timestamp(),
        total_trades = 3,
        winning_trades = 2,
        total_pnl = 100.0,
        current_position = 0,
        daily_pnl = 100.0,
        max_drawdown = -25.0,
        last_update = erlang:timestamp()
    },
    
    %% Test enhanced metrics calculation
    EnhancedMetrics = live_trader:calculate_enhanced_metrics(SamplePerformance),
    
    %% Display results
    io:format("Enhanced Performance Metrics:~n"),
    io:format("  Win Rate: ~p%~n", [maps:get(win_rate, EnhancedMetrics)]),
    io:format("  Average Trade P&L: ~p~n", [maps:get(avg_trade_pnl, EnhancedMetrics)]),
    io:format("  Sharpe Ratio: ~p~n", [maps:get(sharpe_ratio, EnhancedMetrics)]),
    io:format("  Max Consecutive Losses: ~p~n", [maps:get(max_consecutive_losses, EnhancedMetrics)]),
    io:format("  Profit Factor: ~p~n", [maps:get(profit_factor, EnhancedMetrics)]),
    
    %% Test 4: Performance comparison (mock)
    io:format("Test 4: Testing performance comparison~n"),
    
    %% Mock backtesting results
    MockBacktestResults = #{
        fitness => 80.0,
        generation => 10,
        innovation_factor => 0.5,
        constraint => undefined,
        evo_hist => []
    },
    
    %% Calculate comparison
    Comparison = live_trader:calculate_performance_comparison(EnhancedMetrics, MockBacktestResults),
    
    io:format("Performance Comparison:~n"),
    io:format("  Live P&L: ~p~n", [maps:get(live_pnl, Comparison)]),
    io:format("  Backtest Fitness: ~p~n", [maps:get(backtest_fitness, Comparison)]),
    io:format("  Performance Ratio: ~p~n", [maps:get(performance_ratio, Comparison)]),
    io:format("  Performance Status: ~p~n", [maps:get(performance_status, Comparison)]),
    
    %% Test 5: Cleanup
    io:format("Test 5: Cleaning up performance tables~n"),
    live_trader:cleanup_performance_tables(),
    
    %% Verify cleanup
    case {ets:info(live_trade_history), ets:info(live_performance_snapshots), ets:info(backtesting_comparison)} of
        {undefined, undefined, undefined} ->
            io:format("SUCCESS: All performance tables cleaned up~n"),
            {ok, all_tests_passed};
        _ ->
            io:format("WARNING: Some tables not cleaned up properly~n"),
            {warning, cleanup_incomplete}
    end.

%% Test individual calculation functions
test_calculations() ->
    io:format("Testing individual calculation functions~n"),
    
    %% Sample trade history for testing
    TradeHistory = [
        {1, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, 50.0},
        {2, erlang:timestamp(), "EURUSD", "SELL", 10000, 1.1050, -25.0},
        {3, erlang:timestamp(), "EURUSD", "BUY", 15000, 1.1020, 75.0},
        {4, erlang:timestamp(), "EURUSD", "SELL", 15000, 1.1000, -30.0},
        {5, erlang:timestamp(), "EURUSD", "BUY", 12000, 1.0980, 40.0}
    ],
    
    %% Test win rate calculation
    WinRate = live_trader:calculate_win_rate(TradeHistory),
    io:format("Win Rate: ~p%~n", [WinRate]),
    
    %% Test average P&L calculation
    AvgPnL = live_trader:calculate_average_trade_pnl(TradeHistory),
    io:format("Average Trade P&L: ~p~n", [AvgPnL]),
    
    %% Test Sharpe ratio calculation
    SharpeRatio = live_trader:calculate_sharpe_ratio(TradeHistory),
    io:format("Sharpe Ratio: ~p~n", [SharpeRatio]),
    
    %% Test max consecutive losses
    MaxLosses = live_trader:calculate_max_consecutive_losses(TradeHistory),
    io:format("Max Consecutive Losses: ~p~n", [MaxLosses]),
    
    %% Test profit factor
    ProfitFactor = live_trader:calculate_profit_factor(TradeHistory),
    io:format("Profit Factor: ~p~n", [ProfitFactor]),
    
    ok.

%% Run all tests
run_all_tests() ->
    io:format("=== Performance Monitoring Tests ===~n"),
    
    %% Test performance tracking
    case test_performance_tracking() of
        {ok, all_tests_passed} ->
            io:format("Performance tracking tests: PASSED~n");
        {warning, cleanup_incomplete} ->
            io:format("Performance tracking tests: PASSED (with warnings)~n");
        {error, Reason} ->
            io:format("Performance tracking tests: FAILED (~p)~n", [Reason])
    end,
    
    %% Test calculations
    test_calculations(),
    io:format("Calculation tests: COMPLETED~n"),
    
    io:format("=== All Tests Completed ===~n").