%% Simple integration test for result formatting
-module(test_format_integration).
-compile(export_all).

%% Include the trading_decision and agent_run_results records
-record(trading_decision, {
    timestamp,
    index,
    price,
    signal,      % -1, 0, 1 for sell, hold, buy
    confidence,  % derived from fitness or other metrics
    profit_loss  % calculated profit/loss from decision
}).

-record(agent_run_results, {
    agent_id,
    data_source,
    start_time,
    end_time,
    total_decisions,
    buy_decisions,
    sell_decisions,
    hold_decisions,
    total_profit_loss,
    max_profit,
    max_loss,
    trading_decisions,  % List of #trading_decision{}
    execution_stats     % Performance metrics
}).

%% Simple test to verify the main formatting functionality works
test_basic_formatting() ->
    io:format("Testing basic result formatting...~n"),
    
    % Create simple test data
    Decisions = [
        #trading_decision{
            timestamp = erlang:timestamp(),
            signal = 1,
            price = 1.0500,
            profit_loss = 2.5
        },
        #trading_decision{
            timestamp = erlang:timestamp(),
            signal = -1,
            price = 1.0520,
            profit_loss = -1.2
        }
    ],
    
    Results = #agent_run_results{
        agent_id = test_agent,
        data_source = "test.csv",
        start_time = erlang:timestamp(),
        end_time = erlang:timestamp(),
        total_decisions = 2,
        buy_decisions = 1,
        sell_decisions = 1,
        hold_decisions = 0,
        total_profit_loss = 1.3,
        max_profit = 2.5,
        max_loss = -1.2,
        trading_decisions = Decisions,
        execution_stats = #{}
    },
    
    % Test summary formatting
    io:format("Testing summary format...~n"),
    Summary = best_agent_runner:format_summary_results(Results, none),
    io:format("Summary generated: ~p keys~n", [maps:size(Summary)]),
    
    % Test CSV formatting
    io:format("Testing CSV format...~n"),
    CSV = best_agent_runner:format_csv_results(Results, string),
    io:format("CSV generated: ~p characters~n", [length(CSV)]),
    
    % Test detailed formatting
    io:format("Testing detailed format...~n"),
    Detailed = best_agent_runner:format_detailed_results(Results, none),
    io:format("Detailed results generated: ~p keys~n", [maps:size(Detailed)]),
    
    io:format("Basic formatting test completed successfully!~n"),
    ok.

%% Run the integration test
run() ->
    test_basic_formatting().