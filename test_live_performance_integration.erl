%% Integration test for live trader performance monitoring
-module(test_live_performance_integration).
-compile(export_all).
-include("records.hrl").

%% Test integration with live trader performance functions
test_integration() ->
    io:format("Testing live trader performance integration~n"),
    
    %% Test get_performance_report function
    io:format("Test 1: Testing get_performance_report (without running trader)~n"),
    case live_trader:get_performance_report() of
        {error, not_running} ->
            io:format("SUCCESS: Correctly detected trader not running~n");
        Other1 ->
            io:format("UNEXPECTED: Got ~p~n", [Other1])
    end,
    
    %% Test performance comparison function
    io:format("Test 2: Testing get_performance_comparison~n"),
    
    %% Initialize tables for testing
    live_trader:init_performance_tables(),
    
    %% Test with non-existent agent
    case live_trader:get_performance_comparison(non_existent_agent) of
        {error, {backtesting_data_unavailable, _}} ->
            io:format("SUCCESS: Correctly handled non-existent agent~n");
        {error, {live_performance_unavailable, _}} ->
            io:format("SUCCESS: Correctly detected live performance unavailable~n");
        Other2 ->
            io:format("Got result: ~p~n", [Other2])
    end,
    
    %% Test 3: Test performance metrics calculation functions
    io:format("Test 3: Testing performance metrics functions~n"),
    
    %% Test with empty trade history
    EmptyPerformance = #performance_metrics{
        start_time = erlang:timestamp(),
        total_trades = 0,
        winning_trades = 0,
        total_pnl = 0.0,
        current_position = 0,
        daily_pnl = 0.0,
        max_drawdown = 0.0,
        last_update = erlang:timestamp()
    },
    
    EmptyMetrics = live_trader:calculate_enhanced_metrics(EmptyPerformance),
    io:format("Empty metrics calculated successfully~n"),
    io:format("  Win Rate: ~p%~n", [maps:get(win_rate, EmptyMetrics)]),
    io:format("  Avg Trade P&L: ~p~n", [maps:get(avg_trade_pnl, EmptyMetrics)]),
    
    %% Test 4: Test trade recording
    io:format("Test 4: Testing trade recording~n"),
    
    TradeId = live_trader:generate_trade_id(),
    live_trader:record_trade_for_performance(TradeId, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, 25.0),
    
    %% Verify trade was recorded
    TradeCount = ets:info(live_trade_history, size),
    io:format("Trade count after recording: ~p~n", [TradeCount]),
    
    %% Test 5: Test performance snapshot
    io:format("Test 5: Testing performance snapshot creation~n"),
    live_trader:create_performance_snapshot(erlang:timestamp()),
    
    SnapshotCount = ets:info(live_performance_snapshots, size),
    io:format("Snapshot count: ~p~n", [SnapshotCount]),
    
    %% Cleanup
    live_trader:cleanup_performance_tables(),
    io:format("Integration tests completed~n"),
    
    ok.

%% Test performance calculation edge cases
test_edge_cases() ->
    io:format("Testing performance calculation edge cases~n"),
    
    %% Test with single trade
    SingleTrade = [
        {1, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, 50.0}
    ],
    
    WinRate1 = live_trader:calculate_win_rate(SingleTrade),
    AvgPnL1 = live_trader:calculate_average_trade_pnl(SingleTrade),
    Sharpe1 = live_trader:calculate_sharpe_ratio(SingleTrade),
    
    io:format("Single trade metrics:~n"),
    io:format("  Win Rate: ~p%~n", [WinRate1]),
    io:format("  Avg P&L: ~p~n", [AvgPnL1]),
    io:format("  Sharpe: ~p~n", [Sharpe1]),
    
    %% Test with all losing trades
    LosingTrades = [
        {1, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, -25.0},
        {2, erlang:timestamp(), "EURUSD", "SELL", 10000, 1.1050, -30.0},
        {3, erlang:timestamp(), "EURUSD", "BUY", 15000, 1.1020, -15.0}
    ],
    
    WinRate2 = live_trader:calculate_win_rate(LosingTrades),
    ProfitFactor2 = live_trader:calculate_profit_factor(LosingTrades),
    MaxLosses2 = live_trader:calculate_max_consecutive_losses(LosingTrades),
    
    io:format("All losing trades metrics:~n"),
    io:format("  Win Rate: ~p%~n", [WinRate2]),
    io:format("  Profit Factor: ~p~n", [ProfitFactor2]),
    io:format("  Max Consecutive Losses: ~p~n", [MaxLosses2]),
    
    %% Test with all winning trades
    WinningTrades = [
        {1, erlang:timestamp(), "EURUSD", "BUY", 10000, 1.1000, 25.0},
        {2, erlang:timestamp(), "EURUSD", "SELL", 10000, 1.1050, 30.0},
        {3, erlang:timestamp(), "EURUSD", "BUY", 15000, 1.1020, 15.0}
    ],
    
    WinRate3 = live_trader:calculate_win_rate(WinningTrades),
    ProfitFactor3 = live_trader:calculate_profit_factor(WinningTrades),
    MaxLosses3 = live_trader:calculate_max_consecutive_losses(WinningTrades),
    
    io:format("All winning trades metrics:~n"),
    io:format("  Win Rate: ~p%~n", [WinRate3]),
    io:format("  Profit Factor: ~p~n", [ProfitFactor3]),
    io:format("  Max Consecutive Losses: ~p~n", [MaxLosses3]),
    
    ok.

%% Run all integration tests
run_all_tests() ->
    io:format("=== Live Trader Performance Integration Tests ===~n"),
    
    test_integration(),
    test_edge_cases(),
    
    io:format("=== All Integration Tests Completed ===~n").