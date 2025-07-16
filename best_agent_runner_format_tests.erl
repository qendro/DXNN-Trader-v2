%% Test module for best_agent_runner result formatting and reporting functions
-module(best_agent_runner_format_tests).
-compile(export_all).
-include("records.hrl").

%% Test record definitions (matching best_agent_runner.erl)
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

%% ============================================================================
%% TEST SUITE FOR TASK 7: Result Formatting and Reporting
%% ============================================================================

%% Main test runner
run_all_tests() ->
    io:format("~n=== RUNNING RESULT FORMATTING TESTS ===~n"),
    
    Tests = [
        fun test_format_results_basic/0,
        fun test_format_summary_results/0,
        fun test_format_detailed_results/0,
        fun test_format_csv_results/0,
        fun test_format_json_results/0,
        fun test_analyze_trading_decisions/0,
        fun test_calculate_statistics/0,
        fun test_profit_distribution_analysis/0,
        fun test_signal_sequence_analysis/0,
        fun test_time_pattern_analysis/0,
        fun test_decision_quality_assessment/0,
        fun test_file_output/0,
        fun test_edge_cases/0,
        fun test_large_dataset/0
    ],
    
    Results = lists:map(fun(Test) ->
        try
            Test(),
            {ok, Test}
        catch
            Error:Reason ->
                io:format("Test ~p failed: ~p:~p~n", [Test, Error, Reason]),
                {error, Test, Error, Reason}
        end
    end, Tests),
    
    Passed = length([R || {ok, _} <- Results]),
    Failed = length([R || {error, _, _, _} <- Results]),
    
    io:format("~n=== TEST RESULTS ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total: ~p~n", [Passed + Failed]),
    
    case Failed of
        0 -> 
            io:format("All tests passed!~n"),
            ok;
        _ -> 
            io:format("Some tests failed!~n"),
            {error, failed_tests}
    end.

%% Test basic format_results function
test_format_results_basic() ->
    io:format("Testing basic format_results function...~n"),
    
    % Create test data
    TestResults = create_test_results(),
    
    % Test default formatting (detailed, console)
    FormattedResults = best_agent_runner:format_results(TestResults),
    
    % Verify it returns a map with expected keys
    assert_is_map(FormattedResults),
    assert_has_key(agent_id, FormattedResults),
    assert_has_key(total_decisions, FormattedResults),
    assert_has_key(success_rate, FormattedResults),
    
    io:format("  ✓ Basic format_results test passed~n").

%% Test summary results formatting
test_format_summary_results() ->
    io:format("Testing summary results formatting...~n"),
    
    TestResults = create_test_results(),
    
    % Test summary format with different outputs
    SummaryConsole = best_agent_runner:format_results(TestResults, #{format => summary, output => console}),
    SummaryString = best_agent_runner:format_results(TestResults, #{format => summary, output => string}),
    SummaryNone = best_agent_runner:format_results(TestResults, #{format => summary, output => none}),
    
    % Verify summary contains expected metrics
    assert_is_map(SummaryConsole),
    assert_has_key(success_rate, SummaryConsole),
    assert_has_key(win_loss_ratio, SummaryConsole),
    assert_has_key(avg_profit_per_trade, SummaryConsole),
    assert_has_key(profitable_trades, SummaryConsole),
    assert_has_key(losing_trades, SummaryConsole),
    
    % Verify string output is actually a string
    assert_is_list(SummaryString),
    
    % Verify none output is a map
    assert_is_map(SummaryNone),
    
    io:format("  ✓ Summary formatting test passed~n").

%% Test detailed results formatting
test_format_detailed_results() ->
    io:format("Testing detailed results formatting...~n"),
    
    TestResults = create_test_results(),
    
    % Test detailed format
    DetailedResults = best_agent_runner:format_results(TestResults, #{format => detailed, output => none}),
    
    % Verify detailed results contain analysis
    assert_is_map(DetailedResults),
    assert_has_key(decision_analysis, DetailedResults),
    assert_has_key(execution_stats, DetailedResults),
    
    % Verify decision analysis structure
    Analysis = maps:get(decision_analysis, DetailedResults),
    assert_is_map(Analysis),
    assert_has_key(signal_sequences, Analysis),
    assert_has_key(profit_distribution, Analysis),
    assert_has_key(time_patterns, Analysis),
    assert_has_key(decision_quality, Analysis),
    
    io:format("  ✓ Detailed formatting test passed~n").

%% Test CSV results formatting
test_format_csv_results() ->
    io:format("Testing CSV results formatting...~n"),
    
    TestResults = create_test_results(),
    
    % Test CSV format
    CSVResults = best_agent_runner:format_results(TestResults, #{format => csv, output => string}),
    
    % Verify CSV structure
    assert_is_list(CSVResults),
    
    % Check CSV header
    Lines = string:split(CSVResults, "\n", all),
    [Header | DataLines] = Lines,
    
    % Verify header contains expected columns
    assert_contains("Timestamp", Header),
    assert_contains("Price", Header),
    assert_contains("Signal", Header),
    assert_contains("ProfitLoss", Header),
    
    % Verify we have data lines
    DataLinesFiltered = [L || L <- DataLines, L =/= ""],
    assert_true(length(DataLinesFiltered) > 0),
    
    io:format("  ✓ CSV formatting test passed~n").

%% Test JSON results formatting
test_format_json_results() ->
    io:format("Testing JSON results formatting...~n"),
    
    TestResults = create_test_results(),
    
    % Test JSON format
    JSONResults = best_agent_runner:format_results(TestResults, #{format => json, output => string}),
    
    % Verify JSON structure (basic check since we have simple JSON formatting)
    assert_is_list(JSONResults),
    assert_contains("{", JSONResults),
    assert_contains("}", JSONResults),
    assert_contains("agent_id", JSONResults),
    assert_contains("total_decisions", JSONResults),
    
    io:format("  ✓ JSON formatting test passed~n").

%% Test trading decision analysis
test_analyze_trading_decisions() ->
    io:format("Testing trading decision analysis...~n"),
    
    % Create test decisions with specific patterns
    Decisions = create_test_decisions_with_patterns(),
    
    % Analyze decisions
    Analysis = best_agent_runner:analyze_trading_decisions(Decisions),
    
    % Verify analysis structure
    assert_is_map(Analysis),
    assert_has_key(signal_sequences, Analysis),
    assert_has_key(profit_distribution, Analysis),
    assert_has_key(time_patterns, Analysis),
    assert_has_key(decision_quality, Analysis),
    
    % Verify signal sequences analysis
    SignalSeq = maps:get(signal_sequences, Analysis),
    assert_is_map(SignalSeq),
    assert_has_key(consecutive_buys, SignalSeq),
    assert_has_key(consecutive_sells, SignalSeq),
    assert_has_key(signal_changes, SignalSeq),
    
    io:format("  ✓ Decision analysis test passed~n").

%% Test statistics calculations
test_calculate_statistics() ->
    io:format("Testing statistics calculations...~n"),
    
    % Test median calculation
    assert_equal(3.0, best_agent_runner:calculate_median([1, 2, 3, 4, 5])),
    assert_equal(2.5, best_agent_runner:calculate_median([1, 2, 3, 4])),
    assert_equal(1.0, best_agent_runner:calculate_median([1])),
    
    % Test standard deviation calculation
    Values = [1, 2, 3, 4, 5],
    Mean = 3.0,
    StdDev = best_agent_runner:calculate_std_dev(Values, Mean),
    assert_true(StdDev > 1.0 andalso StdDev < 2.0),  % Approximate check
    
    % Test quartiles calculation
    {Q1, Q2, Q3} = best_agent_runner:calculate_quartiles([1, 2, 3, 4, 5, 6, 7, 8]),
    assert_true(Q1 < Q2 andalso Q2 < Q3),
    
    io:format("  ✓ Statistics calculations test passed~n").

%% Test profit distribution analysis
test_profit_distribution_analysis() ->
    io:format("Testing profit distribution analysis...~n"),
    
    % Create decisions with known profit distribution
    Decisions = [
        create_decision(1, 10.0, 1, 5.0),   % Profitable
        create_decision(2, 11.0, -1, -2.0), % Loss
        create_decision(3, 12.0, 1, 3.0),   % Profitable
        create_decision(4, 13.0, 0, 0.0),   % Hold
        create_decision(5, 14.0, 1, 8.0)    % Profitable
    ],
    
    Analysis = best_agent_runner:analyze_profit_distribution(Decisions),
    
    % Verify analysis structure
    assert_is_map(Analysis),
    assert_has_key(mean, Analysis),
    assert_has_key(median, Analysis),
    assert_has_key(std_dev, Analysis),
    assert_has_key(quartiles, Analysis),
    
    % Verify calculated values make sense
    Mean = maps:get(mean, Analysis),
    assert_true(Mean > 0),  % Should be positive overall
    
    io:format("  ✓ Profit distribution analysis test passed~n").

%% Test signal sequence analysis
test_signal_sequence_analysis() ->
    io:format("Testing signal sequence analysis...~n"),
    
    % Create decisions with specific signal patterns
    Decisions = [
        create_decision(1, 10.0, 1, 1.0),   % Buy
        create_decision(2, 11.0, 1, 2.0),   % Buy (consecutive)
        create_decision(3, 12.0, 1, 3.0),   % Buy (consecutive)
        create_decision(4, 13.0, -1, -1.0), % Sell
        create_decision(5, 14.0, 0, 0.0),   % Hold
        create_decision(6, 15.0, 0, 0.0)    % Hold (consecutive)
    ],
    
    Analysis = best_agent_runner:analyze_signal_sequences(Decisions),
    
    % Verify analysis structure
    assert_is_map(Analysis),
    assert_has_key(consecutive_buys, Analysis),
    assert_has_key(consecutive_sells, Analysis),
    assert_has_key(consecutive_holds, Analysis),
    assert_has_key(signal_changes, Analysis),
    
    % Verify signal change count
    Changes = maps:get(signal_changes, Analysis),
    assert_true(Changes >= 2),  % Should have at least 2 changes (buy->sell, sell->hold)
    
    io:format("  ✓ Signal sequence analysis test passed~n").

%% Test time pattern analysis
test_time_pattern_analysis() ->
    io:format("Testing time pattern analysis...~n"),
    
    % Create decisions with specific time patterns
    BaseTime = erlang:timestamp(),
    Decisions = [
        create_decision_with_time(1, 10.0, 1, 1.0, BaseTime),
        create_decision_with_time(2, 11.0, -1, -1.0, add_seconds(BaseTime, 10)),
        create_decision_with_time(3, 12.0, 0, 0.0, add_seconds(BaseTime, 25)),
        create_decision_with_time(4, 13.0, 1, 2.0, add_seconds(BaseTime, 30))
    ],
    
    Analysis = best_agent_runner:analyze_time_patterns(Decisions),
    
    % Verify analysis structure
    assert_is_map(Analysis),
    assert_has_key(total_duration, Analysis),
    assert_has_key(avg_interval, Analysis),
    assert_has_key(min_interval, Analysis),
    assert_has_key(max_interval, Analysis),
    
    % Verify time calculations make sense
    TotalDuration = maps:get(total_duration, Analysis),
    assert_true(TotalDuration >= 30),  % Should be at least 30 seconds
    
    io:format("  ✓ Time pattern analysis test passed~n").

%% Test decision quality assessment
test_decision_quality_assessment() ->
    io:format("Testing decision quality assessment...~n"),
    
    % Create decisions with known quality patterns
    GoodDecisions = [
        create_decision(1, 10.0, 1, 5.0),   % Good buy
        create_decision(2, 11.0, -1, 3.0),  % Good sell
        create_decision(3, 12.0, 1, 4.0),   % Good buy
        create_decision(4, 13.0, -1, 2.0)   % Good sell
    ],
    
    PoorDecisions = [
        create_decision(1, 10.0, 1, -2.0),  % Bad buy
        create_decision(2, 11.0, -1, -3.0), % Bad sell
        create_decision(3, 12.0, 1, -1.0),  % Bad buy
        create_decision(4, 13.0, 0, 0.0)    % Hold
    ],
    
    % Test good decisions
    GoodQuality = best_agent_runner:assess_decision_quality(GoodDecisions),
    assert_is_map(GoodQuality),
    assert_has_key(success_rate, GoodQuality),
    
    GoodSuccessRate = maps:get(success_rate, GoodQuality),
    assert_true(GoodSuccessRate > 50),  % Should be high success rate
    
    % Test poor decisions
    PoorQuality = best_agent_runner:assess_decision_quality(PoorDecisions),
    PoorSuccessRate = maps:get(success_rate, PoorQuality),
    assert_true(PoorSuccessRate < 50),  % Should be low success rate
    
    io:format("  ✓ Decision quality assessment test passed~n").

%% Test file output functionality
test_file_output() ->
    io:format("Testing file output functionality...~n"),
    
    TestResults = create_test_results(),
    TestFile = "test_results_output.txt",
    
    % Test saving formatted results to file
    Result = best_agent_runner:save_formatted_results_to_file(TestResults, TestFile, detailed),
    assert_equal(ok, Result),
    
    % Verify file was created and contains expected content
    {ok, FileContent} = file:read_file(TestFile),
    ContentStr = binary_to_list(FileContent),
    
    assert_contains("Agent:", ContentStr),
    assert_contains("Total Decisions:", ContentStr),
    assert_contains("Success Rate:", ContentStr),
    
    % Test CSV file output
    CSVFile = "test_results_output.csv",
    CSVResult = best_agent_runner:save_formatted_results_to_file(TestResults, CSVFile, csv),
    assert_equal(ok, CSVResult),
    
    {ok, CSVContent} = file:read_file(CSVFile),
    CSVStr = binary_to_list(CSVContent),
    assert_contains("Timestamp,Index,Price", CSVStr),
    
    % Cleanup test files
    file:delete(TestFile),
    file:delete(CSVFile),
    
    io:format("  ✓ File output test passed~n").

%% Test edge cases
test_edge_cases() ->
    io:format("Testing edge cases...~n"),
    
    % Test with empty decisions
    EmptyResults = create_empty_results(),
    EmptyFormatted = best_agent_runner:format_results(EmptyResults, #{format => summary, output => none}),
    
    assert_is_map(EmptyFormatted),
    assert_equal(0, maps:get(total_decisions, EmptyFormatted)),
    assert_equal(0.0, maps:get(success_rate, EmptyFormatted)),
    
    % Test with single decision
    SingleDecision = [create_decision(1, 10.0, 1, 5.0)],
    SingleResults = create_test_results_with_decisions(SingleDecision),
    SingleFormatted = best_agent_runner:format_results(SingleResults, #{format => detailed, output => none}),
    
    assert_is_map(SingleFormatted),
    assert_equal(1, maps:get(total_decisions, SingleFormatted)),
    
    % Test with all hold decisions
    HoldDecisions = [
        create_decision(1, 10.0, 0, 0.0),
        create_decision(2, 11.0, 0, 0.0),
        create_decision(3, 12.0, 0, 0.0)
    ],
    HoldResults = create_test_results_with_decisions(HoldDecisions),
    HoldFormatted = best_agent_runner:format_results(HoldResults, #{format => summary, output => none}),
    
    assert_equal(3, maps:get(hold_decisions, HoldFormatted)),
    assert_equal(0, maps:get(buy_decisions, HoldFormatted)),
    assert_equal(0, maps:get(sell_decisions, HoldFormatted)),
    
    io:format("  ✓ Edge cases test passed~n").

%% Test with large dataset
test_large_dataset() ->
    io:format("Testing with large dataset...~n"),
    
    % Create a large dataset (1000 decisions)
    LargeDecisions = create_large_decision_set(1000),
    LargeResults = create_test_results_with_decisions(LargeDecisions),
    
    % Test that formatting completes in reasonable time
    StartTime = erlang:timestamp(),
    FormattedResults = best_agent_runner:format_results(LargeResults, #{format => detailed, output => none}),
    EndTime = erlang:timestamp(),
    
    ElapsedTime = timer:now_diff(EndTime, StartTime) / 1000000,  % Convert to seconds
    
    % Verify results
    assert_is_map(FormattedResults),
    assert_equal(1000, maps:get(total_decisions, FormattedResults)),
    
    % Verify performance (should complete within 5 seconds)
    assert_true(ElapsedTime < 5.0),
    
    % Test CSV output with large dataset
    CSVResults = best_agent_runner:format_results(LargeResults, #{format => csv, output => string}),
    assert_is_list(CSVResults),
    
    % Count lines in CSV (should be 1001: header + 1000 data lines)
    Lines = string:split(CSVResults, "\n", all),
    NonEmptyLines = [L || L <- Lines, L =/= ""],
    assert_true(length(NonEmptyLines) >= 1000),
    
    io:format("  ✓ Large dataset test passed (processed ~p decisions in ~.2f seconds)~n", 
              [1000, ElapsedTime]).

%% ============================================================================
%% HELPER FUNCTIONS FOR TESTING
%% ============================================================================

%% Create test results with sample data
create_test_results() ->
    Decisions = [
        create_decision(1, 10.0, 1, 2.5),   % Buy, profitable
        create_decision(2, 11.0, -1, 1.8),  % Sell, profitable  
        create_decision(3, 12.0, 1, -1.2),  % Buy, loss
        create_decision(4, 13.0, 0, 0.0),   % Hold
        create_decision(5, 14.0, -1, 3.1),  % Sell, profitable
        create_decision(6, 15.0, 1, -0.8)   % Buy, loss
    ],
    
    create_test_results_with_decisions(Decisions).

%% Create test results with specific decisions
create_test_results_with_decisions(Decisions) ->
    StartTime = erlang:timestamp(),
    EndTime = add_seconds(StartTime, 60),
    
    BuyCount = length([D || D <- Decisions, D#trading_decision.signal == 1]),
    SellCount = length([D || D <- Decisions, D#trading_decision.signal == -1]),
    HoldCount = length([D || D <- Decisions, D#trading_decision.signal == 0]),
    
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    TotalPL = case ProfitLosses of
        [] -> 0;
        _ -> lists:sum(ProfitLosses)
    end,
    
    MaxProfit = case [PL || PL <- ProfitLosses, PL > 0] of
        [] -> 0;
        Profits -> lists:max(Profits)
    end,
    
    MaxLoss = case [PL || PL <- ProfitLosses, PL < 0] of
        [] -> 0;
        Losses -> lists:min(Losses)
    end,
    
    #agent_run_results{
        agent_id = test_agent_123,
        data_source = "test_data.csv",
        start_time = StartTime,
        end_time = EndTime,
        total_decisions = length(Decisions),
        buy_decisions = BuyCount,
        sell_decisions = SellCount,
        hold_decisions = HoldCount,
        total_profit_loss = TotalPL,
        max_profit = MaxProfit,
        max_loss = MaxLoss,
        trading_decisions = Decisions,
        execution_stats = #{
            fitness => 85.5,
            cycles => 1000,
            goal_reached => true
        }
    }.

%% Create empty test results
create_empty_results() ->
    #agent_run_results{
        agent_id = empty_agent,
        data_source = "empty_data.csv",
        start_time = erlang:timestamp(),
        end_time = erlang:timestamp(),
        total_decisions = 0,
        buy_decisions = 0,
        sell_decisions = 0,
        hold_decisions = 0,
        total_profit_loss = 0,
        max_profit = 0,
        max_loss = 0,
        trading_decisions = [],
        execution_stats = #{}
    }.

%% Create a single trading decision
create_decision(Index, Price, Signal, ProfitLoss) ->
    #trading_decision{
        timestamp = erlang:timestamp(),
        index = Index,
        price = Price,
        signal = Signal,
        confidence = abs(ProfitLoss),
        profit_loss = ProfitLoss
    }.

%% Create a trading decision with specific timestamp
create_decision_with_time(Index, Price, Signal, ProfitLoss, Timestamp) ->
    #trading_decision{
        timestamp = Timestamp,
        index = Index,
        price = Price,
        signal = Signal,
        confidence = abs(ProfitLoss),
        profit_loss = ProfitLoss
    }.

%% Create test decisions with specific patterns for analysis
create_test_decisions_with_patterns() ->
    BaseTime = erlang:timestamp(),
    [
        create_decision_with_time(1, 10.0, 1, 2.0, BaseTime),
        create_decision_with_time(2, 11.0, 1, 1.5, add_seconds(BaseTime, 5)),
        create_decision_with_time(3, 12.0, -1, 3.0, add_seconds(BaseTime, 10)),
        create_decision_with_time(4, 13.0, -1, -1.0, add_seconds(BaseTime, 15)),
        create_decision_with_time(5, 14.0, 0, 0.0, add_seconds(BaseTime, 20)),
        create_decision_with_time(6, 15.0, 1, 4.0, add_seconds(BaseTime, 25))
    ].

%% Create a large set of decisions for performance testing
create_large_decision_set(Count) ->
    create_large_decision_set(Count, 1, []).

create_large_decision_set(0, _Index, Acc) ->
    lists:reverse(Acc);
create_large_decision_set(Count, Index, Acc) ->
    % Create random-ish decision
    Signal = case Index rem 3 of
        0 -> 1;   % Buy
        1 -> -1;  % Sell
        2 -> 0    % Hold
    end,
    
    Price = 10.0 + (Index * 0.01),
    ProfitLoss = case Signal of
        0 -> 0.0;
        _ -> (Index rem 10) - 5.0  % Range from -5 to 4
    end,
    
    Decision = create_decision(Index, Price, Signal, ProfitLoss),
    create_large_decision_set(Count - 1, Index + 1, [Decision | Acc]).

%% Add seconds to a timestamp
add_seconds(Timestamp, Seconds) ->
    {MegaSecs, Secs, MicroSecs} = Timestamp,
    NewSecs = Secs + Seconds,
    case NewSecs >= 1000000 of
        true ->
            {MegaSecs + 1, NewSecs - 1000000, MicroSecs};
        false ->
            {MegaSecs, NewSecs, MicroSecs}
    end.

%% ============================================================================
%% ASSERTION HELPER FUNCTIONS
%% ============================================================================

assert_equal(Expected, Actual) ->
    case Expected == Actual of
        true -> ok;
        false -> throw({assertion_failed, {expected, Expected}, {actual, Actual}})
    end.

assert_true(Condition) ->
    case Condition of
        true -> ok;
        false -> throw({assertion_failed, expected_true, got_false})
    end.

assert_is_map(Value) ->
    case is_map(Value) of
        true -> ok;
        false -> throw({assertion_failed, expected_map, {got, Value}})
    end.

assert_is_list(Value) ->
    case is_list(Value) of
        true -> ok;
        false -> throw({assertion_failed, expected_list, {got, Value}})
    end.

assert_has_key(Key, Map) ->
    case maps:is_key(Key, Map) of
        true -> ok;
        false -> throw({assertion_failed, {missing_key, Key}, {in_map, maps:keys(Map)}})
    end.

assert_contains(Substring, String) ->
    case string:find(String, Substring) of
        nomatch -> throw({assertion_failed, {substring_not_found, Substring}, {in_string, String}});
        _ -> ok
    end.