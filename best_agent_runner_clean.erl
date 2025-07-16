%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(best_agent_runner_clean).
-compile(export_all).
-include("records.hrl").

%% Record definitions for best agent runner functionality
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

-record(run_options, {
    start_index = first,     % Where to start in the data
    end_index = last,        % Where to end in the data
    output_file = undefined, % Optional file to save results
    verbose = false,         % Detailed logging
    collect_all_decisions = true % Store all individual decisions
}).

%% ============================================================================
%% TASK 7: Result Formatting and Reporting Functions
%% ============================================================================

%% Main result formatting function - generates comprehensive trading reports
format_results(Results) ->
    format_results(Results, #{format => detailed, output => console}).

format_results(Results, Options) ->
    Format = maps:get(format, Options, detailed),
    Output = maps:get(output, Options, console),
    
    case Format of
        summary -> format_summary_results(Results, Output);
        detailed -> format_detailed_results(Results, Output);
        csv -> format_csv_results(Results, Output);
        json -> format_json_results(Results, Output);
        _ -> format_detailed_results(Results, Output)
    end.

%% Format summary results
format_summary_results(Results, Output) ->
    % Calculate success rate
    Decisions = Results#agent_run_results.trading_decisions,
    ProfitableDecisions = [D || D <- Decisions, 
                          D#trading_decision.profit_loss =/= undefined,
                          D#trading_decision.profit_loss > 0],
    
    SuccessRate = case length(Decisions) of
        0 -> 0.0;
        Total -> (length(ProfitableDecisions) / Total) * 100
    end,
    
    % Calculate win/loss ratio
    LosingDecisions = [D || D <- Decisions, 
                      D#trading_decision.profit_loss =/= undefined,
                      D#trading_decision.profit_loss < 0],
    
    WinLossRatio = case length(LosingDecisions) of
        0 -> case length(ProfitableDecisions) of
                0 -> 0.0;
                _ -> 999.0  % Infinite ratio approximation
             end;
        Losses -> length(ProfitableDecisions) / Losses
    end,
    
    % Calculate average profit per trade
    AllProfits = [D#trading_decision.profit_loss || D <- Decisions, 
                  D#trading_decision.profit_loss =/= undefined],
    AvgProfitPerTrade = case AllProfits of
        [] -> 0.0;
        _ -> lists:sum(AllProfits) / length(AllProfits)
    end,
    
    Summary = #{
        agent_id => Results#agent_run_results.agent_id,
        data_source => Results#agent_run_results.data_source,
        total_decisions => Results#agent_run_results.total_decisions,
        buy_decisions => Results#agent_run_results.buy_decisions,
        sell_decisions => Results#agent_run_results.sell_decisions,
        hold_decisions => Results#agent_run_results.hold_decisions,
        success_rate => SuccessRate,
        avg_profit_per_trade => AvgProfitPerTrade,
        win_loss_ratio => WinLossRatio,
        profitable_trades => length(ProfitableDecisions),
        losing_trades => length(LosingDecisions),
        total_profit_loss => Results#agent_run_results.total_profit_loss,
        max_profit => Results#agent_run_results.max_profit,
        max_loss => Results#agent_run_results.max_loss,
        execution_time => case {Results#agent_run_results.start_time, Results#agent_run_results.end_time} of
            {undefined, _} -> 0.0;
            {_, undefined} -> 0.0;
            {Start, End} -> timer:now_diff(End, Start) / 1000000
        end
    },
    
    case Output of
        console -> 
            io:format("~s", [format_summary_string(Summary)]),
            Summary;
        string -> format_summary_string(Summary);
        none -> Summary;
        _ -> Summary
    end.

%% Format detailed results
format_detailed_results(Results, Output) ->
    Summary = format_summary_results(Results, none),
    
    % Perform decision analysis
    DecisionAnalysis = analyze_trading_decisions(Results#agent_run_results.trading_decisions),
    
    DetailedResults = Summary#{
        decision_analysis => DecisionAnalysis,
        execution_stats => Results#agent_run_results.execution_stats,
        trading_decisions => Results#agent_run_results.trading_decisions
    },
    
    case Output of
        console -> 
            io:format("~s", [format_detailed_string(DetailedResults)]),
            DetailedResults;
        string -> format_detailed_string(DetailedResults);
        none -> DetailedResults;
        _ -> DetailedResults
    end.

%% Format results as CSV
format_csv_results(Results, Output) ->
    Decisions = Results#agent_run_results.trading_decisions,
    
    % CSV Header
    Header = "Timestamp,Index,Price,Signal,Confidence,ProfitLoss\n",
    
    % CSV Data rows
    DataRows = lists:map(fun(Decision) ->
        TimestampStr = format_timestamp(Decision#trading_decision.timestamp),
        Index = case Decision#trading_decision.index of
            undefined -> "";
            I -> integer_to_list(I)
        end,
        Price = case Decision#trading_decision.price of
            undefined -> "";
            P -> io_lib:format("~.4f", [P])
        end,
        Signal = integer_to_list(Decision#trading_decision.signal),
        Confidence = case Decision#trading_decision.confidence of
            undefined -> "";
            C -> io_lib:format("~.4f", [C])
        end,
        ProfitLoss = case Decision#trading_decision.profit_loss of
            undefined -> "";
            PL -> io_lib:format("~.4f", [PL])
        end,
        
        io_lib:format("~s,~s,~s,~s,~s,~s~n", 
                     [TimestampStr, Index, Price, Signal, Confidence, ProfitLoss])
    end, Decisions),
    
    CSV = lists:flatten([Header | DataRows]),
    
    case Output of
        console -> 
            io:format("~s", [CSV]),
            CSV;
        string -> CSV;
        file -> CSV;  % Caller handles file writing
        _ -> CSV
    end.

%% Format results as JSON (simplified)
format_json_results(Results, Output) ->
    Summary = format_summary_results(Results, none),
    
    % Convert decisions to simple format for JSON
    DecisionsList = lists:map(fun(Decision) ->
        #{
            timestamp => format_timestamp(Decision#trading_decision.timestamp),
            index => Decision#trading_decision.index,
            price => Decision#trading_decision.price,
            signal => Decision#trading_decision.signal,
            confidence => Decision#trading_decision.confidence,
            profit_loss => Decision#trading_decision.profit_loss
        }
    end, Results#agent_run_results.trading_decisions),
    
    JSONData = Summary#{
        trading_decisions => DecisionsList
    },
    
    JSON = format_simple_json(JSONData),
    
    case Output of
        console -> 
            io:format("~s", [JSON]),
            JSON;
        string -> JSON;
        file -> JSON;  % Caller handles file writing
        _ -> JSON
    end.

%% Analyze trading decisions for detailed reporting
analyze_trading_decisions(Decisions) ->
    % Signal sequence analysis
    SignalSequences = analyze_signal_sequences(Decisions),
    
    % Profit distribution analysis
    ProfitDistribution = analyze_profit_distribution(Decisions),
    
    % Time pattern analysis
    TimePatterns = analyze_time_patterns(Decisions),
    
    % Decision quality assessment
    DecisionQuality = assess_decision_quality(Decisions),
    
    #{
        signal_sequences => SignalSequences,
        profit_distribution => ProfitDistribution,
        time_patterns => TimePatterns,
        decision_quality => DecisionQuality
    }.

%% Analyze signal sequences (consecutive buys, sells, etc.)
analyze_signal_sequences(Decisions) ->
    Signals = [D#trading_decision.signal || D <- Decisions],
    
    % Count consecutive sequences
    ConsecutiveBuys = count_consecutive_signals(Signals, 1),
    ConsecutiveSells = count_consecutive_signals(Signals, -1),
    ConsecutiveHolds = count_consecutive_signals(Signals, 0),
    
    #{
        consecutive_buys => ConsecutiveBuys,
        consecutive_sells => ConsecutiveSells,
        consecutive_holds => ConsecutiveHolds,
        signal_changes => count_signal_changes(Signals)
    }.

%% Count consecutive occurrences of a specific signal
count_consecutive_signals(Signals, TargetSignal) ->
    count_consecutive_signals(Signals, TargetSignal, 0, 0, []).

count_consecutive_signals([], _TargetSignal, CurrentCount, _MaxCount, Sequences) ->
    case CurrentCount of
        0 -> Sequences;
        _ -> [CurrentCount | Sequences]
    end;
count_consecutive_signals([Signal | Rest], TargetSignal, CurrentCount, MaxCount, Sequences) ->
    case Signal of
        TargetSignal ->
            count_consecutive_signals(Rest, TargetSignal, CurrentCount + 1, 
                                    max(MaxCount, CurrentCount + 1), Sequences);
        _ ->
            NewSequences = case CurrentCount of
                0 -> Sequences;
                _ -> [CurrentCount | Sequences]
            end,
            count_consecutive_signals(Rest, TargetSignal, 0, MaxCount, NewSequences)
    end.

%% Count signal changes (buy->sell, sell->buy, etc.)
count_signal_changes([]) -> 0;
count_signal_changes([_]) -> 0;
count_signal_changes([S1, S2 | Rest]) ->
    case S1 =/= S2 of
        true -> 1 + count_signal_changes([S2 | Rest]);
        false -> count_signal_changes([S2 | Rest])
    end.

%% Analyze profit distribution
analyze_profit_distribution(Decisions) ->
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    
    case ProfitLosses of
        [] ->
            #{mean => 0, median => 0, std_dev => 0, quartiles => {0, 0, 0}};
        _ ->
            SortedPL = lists:sort(ProfitLosses),
            Mean = lists:sum(ProfitLosses) / length(ProfitLosses),
            Median = calculate_median(SortedPL),
            StdDev = calculate_std_dev(ProfitLosses, Mean),
            Quartiles = calculate_quartiles(SortedPL),
            
            #{
                mean => Mean,
                median => Median,
                std_dev => StdDev,
                quartiles => Quartiles
            }
    end.

%% Calculate median
calculate_median(SortedList) ->
    Length = length(SortedList),
    case Length rem 2 of
        0 ->
            Mid1 = lists:nth(Length div 2, SortedList),
            Mid2 = lists:nth((Length div 2) + 1, SortedList),
            (Mid1 + Mid2) / 2;
        1 ->
            lists:nth((Length + 1) div 2, SortedList)
    end.

%% Calculate standard deviation
calculate_std_dev(Values, Mean) ->
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / length(Values),
    math:sqrt(Variance).

%% Calculate quartiles
calculate_quartiles(SortedList) ->
    Length = length(SortedList),
    Q1Index = max(1, Length div 4),
    Q2Index = max(1, Length div 2),
    Q3Index = max(1, (3 * Length) div 4),
    
    Q1 = lists:nth(Q1Index, SortedList),
    Q2 = lists:nth(Q2Index, SortedList),
    Q3 = lists:nth(Q3Index, SortedList),
    
    {Q1, Q2, Q3}.

%% Analyze time patterns in decisions
analyze_time_patterns(Decisions) ->
    case Decisions of
        [] -> #{total_duration => 0, avg_interval => 0, min_interval => 0, max_interval => 0};
        [_] -> #{total_duration => 0, avg_interval => 0, min_interval => 0, max_interval => 0};
        _ ->
            Timestamps = [D#trading_decision.timestamp || D <- Decisions],
            SortedTimestamps = lists:sort(Timestamps),
            
            FirstTime = hd(SortedTimestamps),
            LastTime = lists:last(SortedTimestamps),
            TotalDuration = timer:now_diff(LastTime, FirstTime) / 1000000,
            
            % Calculate intervals between decisions
            Intervals = calculate_time_intervals(SortedTimestamps),
            AvgInterval = case Intervals of
                [] -> 0;
                _ -> lists:sum(Intervals) / length(Intervals)
            end,
            
            #{
                total_duration => TotalDuration,
                avg_interval => AvgInterval,
                min_interval => case Intervals of [] -> 0; _ -> lists:min(Intervals) end,
                max_interval => case Intervals of [] -> 0; _ -> lists:max(Intervals) end
            }
    end.

%% Calculate time intervals between timestamps
calculate_time_intervals([]) -> [];
calculate_time_intervals([_]) -> [];
calculate_time_intervals([T1, T2 | Rest]) ->
    Interval = timer:now_diff(T2, T1) / 1000000,
    [Interval | calculate_time_intervals([T2 | Rest])].

%% Assess decision quality
assess_decision_quality(Decisions) ->
    ProfitableDecisions = [D || D <- Decisions, 
                          D#trading_decision.profit_loss =/= undefined,
                          D#trading_decision.profit_loss > 0],
    
    TotalDecisions = length([D || D <- Decisions, 
                            D#trading_decision.profit_loss =/= undefined]),
    
    SuccessRate = case TotalDecisions of
        0 -> 0.0;
        _ -> (length(ProfitableDecisions) / TotalDecisions) * 100
    end,
    
    % Calculate average confidence for profitable vs unprofitable decisions
    ProfitableConfidences = [D#trading_decision.confidence || D <- ProfitableDecisions,
                            D#trading_decision.confidence =/= undefined],
    
    UnprofitableDecisions = [D || D <- Decisions, 
                            D#trading_decision.profit_loss =/= undefined,
                            D#trading_decision.profit_loss =< 0],
    UnprofitableConfidences = [D#trading_decision.confidence || D <- UnprofitableDecisions,
                              D#trading_decision.confidence =/= undefined],
    
    AvgProfitableConfidence = case ProfitableConfidences of
        [] -> 0.0;
        _ -> lists:sum(ProfitableConfidences) / length(ProfitableConfidences)
    end,
    
    AvgUnprofitableConfidence = case UnprofitableConfidences of
        [] -> 0.0;
        _ -> lists:sum(UnprofitableConfidences) / length(UnprofitableConfidences)
    end,
    
    #{
        success_rate => SuccessRate,
        avg_profitable_confidence => AvgProfitableConfidence,
        avg_unprofitable_confidence => AvgUnprofitableConfidence,
        confidence_correlation => AvgProfitableConfidence - AvgUnprofitableConfidence
    }.

%% Format summary as string
format_summary_string(Summary) ->
    lists:flatten(io_lib:format(
        "=== TRADING RESULTS SUMMARY ===~n"
        "Agent: ~p | Data: ~s~n"
        "Execution Time: ~.2f seconds~n"
        "~nDecision Statistics:~n"
        "  Total Decisions: ~p~n"
        "  Buy Decisions: ~p~n"
        "  Sell Decisions: ~p~n"
        "  Hold Decisions: ~p~n"
        "~nPerformance Metrics:~n"
        "  Success Rate: ~.2f%~n"
        "  Win/Loss Ratio: ~.2f~n"
        "  Profitable Trades: ~p~n"
        "  Losing Trades: ~p~n"
        "~nProfit/Loss Analysis:~n"
        "  Total P&L: ~.4f%~n"
        "  Average P&L per Trade: ~.4f%~n"
        "  Max Profit: ~.4f%~n"
        "  Max Loss: ~.4f%~n"
        "=== END SUMMARY ===~n",
        [maps:get(agent_id, Summary),
         filename:basename(maps:get(data_source, Summary, "unknown")),
         maps:get(execution_time, Summary),
         maps:get(total_decisions, Summary),
         maps:get(buy_decisions, Summary),
         maps:get(sell_decisions, Summary),
         maps:get(hold_decisions, Summary),
         maps:get(success_rate, Summary),
         maps:get(win_loss_ratio, Summary),
         maps:get(profitable_trades, Summary),
         maps:get(losing_trades, Summary),
         maps:get(total_profit_loss, Summary),
         maps:get(avg_profit_per_trade, Summary),
         maps:get(max_profit, Summary),
         maps:get(max_loss, Summary)])).

%% Format detailed results as string
format_detailed_string(DetailedResults) ->
    SummaryStr = format_summary_string(DetailedResults),
    
    % Add detailed analysis
    Analysis = maps:get(decision_analysis, DetailedResults, #{}),
    
    % Signal sequence analysis
    SignalSeq = maps:get(signal_sequences, Analysis, #{}),
    SignalStr = io_lib:format(
        "~n=== DETAILED ANALYSIS ===~n"
        "Signal Patterns:~n"
        "  Consecutive Buys: ~p~n"
        "  Consecutive Sells: ~p~n"
        "  Consecutive Holds: ~p~n"
        "  Signal Changes: ~p~n",
        [maps:get(consecutive_buys, SignalSeq, []),
         maps:get(consecutive_sells, SignalSeq, []),
         maps:get(consecutive_holds, SignalSeq, []),
         maps:get(signal_changes, SignalSeq, 0)]),
    
    % Profit distribution analysis
    ProfitDist = maps:get(profit_distribution, Analysis, #{}),
    ProfitStr = io_lib:format(
        "~nProfit Distribution:~n"
        "  Mean: ~.4f%~n"
        "  Median: ~.4f%~n"
        "  Standard Deviation: ~.4f~n",
        [maps:get(mean, ProfitDist, 0),
         maps:get(median, ProfitDist, 0),
         maps:get(std_dev, ProfitDist, 0)]),
    
    % Time patterns
    TimePatterns = maps:get(time_patterns, Analysis, #{}),
    TimeStr = io_lib:format(
        "~nTime Patterns:~n"
        "  Total Duration: ~.2f seconds~n"
        "  Average Interval: ~.2f seconds~n"
        "  Min Interval: ~.2f seconds~n"
        "  Max Interval: ~.2f seconds~n",
        [maps:get(total_duration, TimePatterns, 0),
         maps:get(avg_interval, TimePatterns, 0),
         maps:get(min_interval, TimePatterns, 0),
         maps:get(max_interval, TimePatterns, 0)]),
    
    % Decision quality
    Quality = maps:get(decision_quality, Analysis, #{}),
    QualityStr = io_lib:format(
        "~nDecision Quality:~n"
        "  Success Rate: ~.2f%~n"
        "  Avg Profitable Confidence: ~.2f~n"
        "  Avg Unprofitable Confidence: ~.2f~n"
        "  Confidence Correlation: ~.2f~n"
        "=== END DETAILED ANALYSIS ===~n",
        [maps:get(success_rate, Quality, 0),
         maps:get(avg_profitable_confidence, Quality, 0),
         maps:get(avg_unprofitable_confidence, Quality, 0),
         maps:get(confidence_correlation, Quality, 0)]),
    
    lists:flatten([SummaryStr, SignalStr, ProfitStr, TimeStr, QualityStr]).

%% Format timestamp for display
format_timestamp(Timestamp) ->
    case Timestamp of
        undefined -> "N/A";
        {MegaSecs, Secs, MicroSecs} ->
            DateTime = calendar:now_to_datetime({MegaSecs, Secs, MicroSecs}),
            {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
            io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                         [Year, Month, Day, Hour, Min, Sec]);
        _ -> "Invalid"
    end.

%% Simple JSON formatting (basic implementation)
format_simple_json(Map) when is_map(Map) ->
    Pairs = maps:fold(fun(K, V, Acc) ->
        KeyStr = io_lib:format("\"~p\"", [K]),
        ValueStr = format_json_value(V),
        [io_lib:format("~s: ~s", [KeyStr, ValueStr]) | Acc]
    end, [], Map),
    
    "{" ++ string:join(lists:reverse(Pairs), ", ") ++ "}".

format_json_value(V) when is_number(V) ->
    io_lib:format("~p", [V]);
format_json_value(V) when is_atom(V) ->
    io_lib:format("\"~p\"", [V]);
format_json_value(V) when is_list(V) ->
    case io_lib:printable_list(V) of
        true -> io_lib:format("\"~s\"", [V]);
        false -> 
            Elements = [format_json_value(E) || E <- V],
            "[" ++ string:join(Elements, ", ") ++ "]"
    end;
format_json_value(V) when is_map(V) ->
    format_simple_json(V);
format_json_value(V) ->
    io_lib:format("\"~p\"", [V]).

%% Save formatted results to file with different format options
save_formatted_results_to_file(Results, FilePath, Format) ->
    try
        FormattedContent = case Format of
            summary -> format_results(Results, #{format => summary, output => string});
            detailed -> format_results(Results, #{format => detailed, output => string});
            csv -> format_results(Results, #{format => csv, output => string});
            json -> format_results(Results, #{format => json, output => string});
            _ -> format_results(Results, #{format => detailed, output => string})
        end,
        
        case file:write_file(FilePath, FormattedContent) of
            ok ->
                io:format("Results saved to ~s in ~p format~n", [FilePath, Format]),
                ok;
            {error, Reason} ->
                io:format("Error saving results to file: ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        Error:SaveReason ->
            io:format("Exception saving results: ~p:~p~n", [Error, SaveReason]),
            {error, {save_exception, Error, SaveReason}}
    end.