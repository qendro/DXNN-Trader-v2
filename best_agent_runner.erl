%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(best_agent_runner).
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
%% TASK 6: Main Interface Functions
%% ============================================================================

%% Main interface function - orchestrates the entire workflow
run_best_agent_on_data(DataFilePath) ->
    run_best_agent_on_data(DataFilePath, #run_options{}).

run_best_agent_on_data(DataFilePath, Options) ->
    io:format("Starting best agent run on data file: ~p~n", [DataFilePath]),
    
    % Step 1: Identify the best agent
    case find_best_agent() of
        {ok, Agent_Id} ->
            io:format("Best agent identified: ~p~n", [Agent_Id]),
            
            % Step 2: Load the forex data
            case load_forex_data(DataFilePath) of
                {ok, TableName} ->
                    io:format("Data loaded into table: ~p~n", [TableName]),
                    
                    % Step 3: Execute the agent on the data
                    case execute_agent_on_data(Agent_Id, TableName, Options) of
                        {ok, Results} ->
                            io:format("Agent execution completed successfully~n"),
                            
                            % Step 4: Format and finalize results
                            FinalResults = finalize_results(Results, Agent_Id, DataFilePath, Options),
                            
                            % Step 5: Cleanup temporary data
                            cleanup_temporary_data_safe(TableName),
                            
                            % Step 6: Optional file output
                            case Options#run_options.output_file of
                                undefined -> 
                                    {ok, FinalResults};
                                OutputFile ->
                                    case save_results_to_file(FinalResults, OutputFile) of
                                        ok -> {ok, FinalResults};
                                        {error, SaveError} ->
                                            io:format("Warning: Could not save to file ~p: ~p~n", 
                                                     [OutputFile, SaveError]),
                                            {ok, FinalResults}
                                    end
                            end;
                            
                        {error, ExecutionError} ->
                            cleanup_temporary_data_safe(TableName),
                            {error, {execution_failed, ExecutionError}}
                    end;
                    
                {error, DataError} ->
                    {error, {data_loading_failed, DataError}}
            end;
            
        {error, AgentError} ->
            {error, {agent_identification_failed, AgentError}}
    end.

%% Find the best agent using existing genotype_utils functionality
find_best_agent() ->
    try
        % Check if mnesia is running first
        case mnesia:system_info(is_running) of
            yes ->
                % Use the existing genotype_utils function to find best agent
                case genotype_utils:find_best_agent() of
                    {atomic, {ok, Agent_Id}} ->
                        {ok, Agent_Id};
                    {atomic, {error, Reason}} ->
                        {error, Reason};
                    {ok, Agent_Id} ->
                        {ok, Agent_Id};
                    {error, Reason} ->
                        {error, Reason};
                    {aborted, Reason} ->
                        {error, {transaction_aborted, Reason}};
                    Other ->
                        {error, {unexpected_result, Other}}
                end;
            no ->
                {error, mnesia_not_running};
            stopping ->
                {error, mnesia_stopping};
            starting ->
                {error, mnesia_starting}
        end
    catch
        exit:{aborted, Reason} ->
            io:format("Mnesia transaction aborted finding best agent: ~p~n", [Reason]),
            {error, {mnesia_transaction_failed, Reason}};
        Error:FindReason ->
            io:format("Error finding best agent: ~p:~p~n", [Error, FindReason]),
            {error, {find_agent_exception, Error, FindReason}}
    end.

%% Load forex data using fx module functionality with enhanced error handling
load_forex_data(DataFilePath) ->
    try
        % Validate file path first
        case validate_file_path(DataFilePath) of
            ok ->
                % Use the existing fx module to load data
                case fx:load_data_file(DataFilePath) of
                    {ok, TableName} ->
                        % Verify table was created successfully
                        case verify_table_creation(TableName) of
                            ok ->
                                {ok, TableName};
                            {error, VerifyReason} ->
                                % Cleanup failed table
                                fx:delete_temporary_table(TableName),
                                {error, {table_verification_failed, VerifyReason}}
                        end;
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, ValidationReason} ->
                {error, ValidationReason}
        end
    catch
        Error:LoadReason ->
            io:format("Error loading forex data: ~p:~p~n", [Error, LoadReason]),
            {error, {data_loading_exception, Error, LoadReason}}
    end.

%% Validate file path and accessibility
validate_file_path(FilePath) ->
    try
        case filelib:is_file(FilePath) of
            false ->
                {error, {file_not_found, FilePath}};
            true ->
                case file:read_file_info(FilePath) of
                    {ok, FileInfo} ->
                        case FileInfo#file_info.access of
                            read -> ok;
                            read_write -> ok;
                            _ -> {error, {file_not_readable, FilePath}}
                        end;
                    {error, Reason} ->
                        {error, {file_info_error, Reason}}
                end
        end
    catch
        Error:Reason ->
            {error, {file_validation_exception, Error, Reason}}
    end.

%% Verify table was created and contains data
verify_table_creation(TableName) ->
    try
        case ets:info(TableName) of
            undefined ->
                {error, {table_not_created, TableName}};
            TableInfo ->
                Size = proplists:get_value(size, TableInfo, 0),
                case Size > 0 of
                    true ->
                        ok;
                    false ->
                        {error, {empty_table, TableName}}
                end
        end
    catch
        Error:Reason ->
            {error, {table_verification_exception, Error, Reason}}
    end.

%% Execute agent on data with comprehensive orchestration
execute_agent_on_data(Agent_Id, TableName, Options) ->
    io:format("Executing agent ~p on table ~p~n", [Agent_Id, TableName]),
    
    try
        % Run the agent standalone with the loaded data
        case run_agent_standalone(Agent_Id, TableName, Options) of
            {ok, Results} ->
                io:format("Agent execution successful~n"),
                {ok, Results};
            {error, Reason} ->
                io:format("Agent execution failed: ~p~n", [Reason]),
                {error, Reason}
        end
    catch
        Error:ExecReason ->
            io:format("Exception during agent execution: ~p:~p~n", [Error, ExecReason]),
            {error, {execution_exception, Error, ExecReason}}
    end.

%% Finalize results with comprehensive statistics and metadata
finalize_results(Results, Agent_Id, DataFilePath, Options) ->
    % Calculate comprehensive statistics from trading decisions
    Decisions = Results#agent_run_results.trading_decisions,
    Stats = calculate_decision_stats(Decisions),
    
    % Calculate profit/loss metrics
    ProfitDecisions = [D || D <- Decisions, 
                       D#trading_decision.profit_loss =/= undefined,
                       D#trading_decision.profit_loss > 0],
    LossDecisions = [D || D <- Decisions, 
                     D#trading_decision.profit_loss =/= undefined,
                     D#trading_decision.profit_loss < 0],
    
    TotalProfit = case ProfitDecisions of
        [] -> 0;
        _ -> lists:sum([D#trading_decision.profit_loss || D <- ProfitDecisions])
    end,
    
    TotalLoss = case LossDecisions of
        [] -> 0;
        _ -> lists:sum([D#trading_decision.profit_loss || D <- LossDecisions])
    end,
    
    MaxProfit = case ProfitDecisions of
        [] -> 0;
        _ -> lists:max([D#trading_decision.profit_loss || D <- ProfitDecisions])
    end,
    
    MaxLoss = case LossDecisions of
        [] -> 0;
        _ -> lists:min([D#trading_decision.profit_loss || D <- LossDecisions])
    end,
    
    % Update results with calculated values
    FinalResults = Results#agent_run_results{
        agent_id = Agent_Id,
        data_source = DataFilePath,
        total_decisions = maps:get(total_decisions, Stats, 0),
        buy_decisions = maps:get(buy_decisions, Stats, 0),
        sell_decisions = maps:get(sell_decisions, Stats, 0),
        hold_decisions = maps:get(hold_decisions, Stats, 0),
        total_profit_loss = TotalProfit + TotalLoss,
        max_profit = MaxProfit,
        max_loss = MaxLoss
    },
    
    % Add verbose output if requested
    case Options#run_options.verbose of
        true ->
            print_detailed_results(FinalResults);
        false ->
            print_summary_results(FinalResults)
    end,
    
    FinalResults.

%% Print detailed results for verbose mode
print_detailed_results(Results) ->
    io:format("~n=== DETAILED AGENT EXECUTION RESULTS ===~n"),
    io:format("Agent ID: ~p~n", [Results#agent_run_results.agent_id]),
    io:format("Data Source: ~p~n", [Results#agent_run_results.data_source]),
    io:format("Execution Time: ~p seconds~n", 
              [timer:now_diff(Results#agent_run_results.end_time, 
                             Results#agent_run_results.start_time) / 1000000]),
    io:format("~nTrading Statistics:~n"),
    io:format("  Total Decisions: ~p~n", [Results#agent_run_results.total_decisions]),
    io:format("  Buy Decisions: ~p~n", [Results#agent_run_results.buy_decisions]),
    io:format("  Sell Decisions: ~p~n", [Results#agent_run_results.sell_decisions]),
    io:format("  Hold Decisions: ~p~n", [Results#agent_run_results.hold_decisions]),
    io:format("~nProfit/Loss Analysis:~n"),
    io:format("  Total P&L: ~.4f%~n", [Results#agent_run_results.total_profit_loss]),
    io:format("  Max Profit: ~.4f%~n", [Results#agent_run_results.max_profit]),
    io:format("  Max Loss: ~.4f%~n", [Results#agent_run_results.max_loss]),
    
    % Print individual decisions if requested
    case length(Results#agent_run_results.trading_decisions) < 20 of
        true ->
            io:format("~nIndividual Trading Decisions:~n"),
            lists:foreach(fun(Decision) ->
                io:format("  ~p: Signal=~p, Price=~p, P&L=~.4f%~n",
                         [Decision#trading_decision.timestamp,
                          Decision#trading_decision.signal,
                          Decision#trading_decision.price,
                          case Decision#trading_decision.profit_loss of
                              undefined -> 0.0;
                              PL -> PL
                          end])
            end, Results#agent_run_results.trading_decisions);
        false ->
            io:format("~n(~p trading decisions - too many to display individually)~n",
                     [length(Results#agent_run_results.trading_decisions)])
    end,
    
    io:format("~n=== END RESULTS ===~n~n").

%% Print summary results for normal mode
print_summary_results(Results) ->
    io:format("~n=== AGENT EXECUTION SUMMARY ===~n"),
    io:format("Agent: ~p | Data: ~p~n", 
              [Results#agent_run_results.agent_id, 
               filename:basename(Results#agent_run_results.data_source)]),
    io:format("Decisions: ~p total (~p buy, ~p sell, ~p hold)~n",
              [Results#agent_run_results.total_decisions,
               Results#agent_run_results.buy_decisions,
               Results#agent_run_results.sell_decisions,
               Results#agent_run_results.hold_decisions]),
    io:format("P&L: ~.4f% (Max: +~.4f%, ~.4f%)~n",
              [Results#agent_run_results.total_profit_loss,
               Results#agent_run_results.max_profit,
               Results#agent_run_results.max_loss]),
    io:format("=== END SUMMARY ===~n~n").

%% Cleanup temporary data tables
cleanup_temporary_data(TableName) ->
    try
        case fx:delete_temporary_table(TableName) of
            ok ->
                io:format("Temporary table ~p cleaned up~n", [TableName]);
            {error, Reason} ->
                io:format("Warning: Could not cleanup table ~p: ~p~n", [TableName, Reason])
        end
    catch
        Error:CleanupReason ->
            io:format("Exception during cleanup: ~p:~p~n", [Error, CleanupReason])
    end.

%% Save results to file
save_results_to_file(Results, OutputFile) ->
    try
        % Format results as a readable report
        Report = format_results_report(Results),
        
        % Write to file
        case file:write_file(OutputFile, Report) of
            ok ->
                io:format("Results saved to: ~p~n", [OutputFile]),
                ok;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Error:FileReason ->
            {error, {file_write_exception, Error, FileReason}}
    end.

%% Format results as a comprehensive report
format_results_report(Results) ->
    Header = "DXNN Best Agent Trading Results Report\n"
             "=====================================\n\n",
    
    AgentInfo = io_lib:format("Agent ID: ~p~n", [Results#agent_run_results.agent_id]),
    DataInfo = io_lib:format("Data Source: ~p~n", [Results#agent_run_results.data_source]),
    TimeInfo = io_lib:format("Execution Time: ~p seconds~n~n", 
                            [timer:now_diff(Results#agent_run_results.end_time, 
                                           Results#agent_run_results.start_time) / 1000000]),
    
    TradingStats = io_lib:format(
        "Trading Statistics:~n"
        "  Total Decisions: ~p~n"
        "  Buy Decisions: ~p~n"
        "  Sell Decisions: ~p~n"
        "  Hold Decisions: ~p~n~n",
        [Results#agent_run_results.total_decisions,
         Results#agent_run_results.buy_decisions,
         Results#agent_run_results.sell_decisions,
         Results#agent_run_results.hold_decisions]),
    
    ProfitStats = io_lib:format(
        "Profit/Loss Analysis:~n"
        "  Total P&L: ~.4f%~n"
        "  Max Profit: ~.4f%~n"
        "  Max Loss: ~.4f%~n~n",
        [Results#agent_run_results.total_profit_loss,
         Results#agent_run_results.max_profit,
         Results#agent_run_results.max_loss]),
    
    % Add execution stats if available
    ExecStats = case Results#agent_run_results.execution_stats of
        undefined -> "";
        Stats when is_map(Stats) ->
            io_lib:format(
                "Execution Statistics:~n"
                "  Fitness: ~p~n"
                "  Cycles: ~p~n"
                "  Goal Reached: ~p~n~n",
                [maps:get(fitness, Stats, undefined),
                 maps:get(cycles, Stats, undefined),
                 maps:get(goal_reached, Stats, undefined)]);
        _ -> ""
    end,
    
    Footer = "\nReport generated: " ++ 
             calendar:system_time_to_rfc3339(erlang:system_time(second)) ++ "\n",
    
    lists:flatten([Header, AgentInfo, DataInfo, TimeInfo, TradingStats, ProfitStats, ExecStats, Footer]).

%% ============================================================================
%% TASK 4: Standalone Agent Execution Controller
%% ============================================================================

%% Run an agent standalone with data table and collect trading decisions
run_agent_standalone(Agent_Id, TableName) ->
    run_agent_standalone(Agent_Id, TableName, #run_options{}).

run_agent_standalone(Agent_Id, TableName, Options) ->
    io:format("Starting standalone agent execution: ~p on table ~p~n", [Agent_Id, TableName]),
    
    Agent_PId = undefined,
    Monitor_PId = undefined,
    Collector_PId = undefined,
    
    try
        % Start the agent process with enhanced error handling
        case start_agent_process_with_timeout(Agent_Id, 5000) of
            {ok, NewAgent_PId} ->
                Agent_PId_Updated = NewAgent_PId,
                io:format("Agent process started: ~p~n", [Agent_PId_Updated]),
                
                % Start monitoring and message collection with error handling
                case start_monitoring_processes(Agent_PId_Updated, TableName, Options) of
                    {ok, NewMonitor_PId, NewCollector_PId} ->
                        Monitor_PId_Updated = NewMonitor_PId,
                        Collector_PId_Updated = NewCollector_PId,
                        
                        % Wait for completion or timeout with enhanced error handling
                        StartTime = erlang:timestamp(),
                        Result = wait_for_completion_with_timeout(Agent_PId_Updated, Monitor_PId_Updated, 
                                                                Collector_PId_Updated, StartTime, Options),
                        
                        % Ensure cleanup in all cases
                        cleanup_agent_processes_safe(Agent_PId_Updated, Monitor_PId_Updated, Collector_PId_Updated),
                        
                        Result;
                    {error, MonitoringReason} ->
                        % Cleanup agent if monitoring failed
                        cleanup_agent_processes_safe(Agent_PId_Updated, undefined, undefined),
                        {error, {monitoring_setup_failed, MonitoringReason}}
                end;
            {error, Reason} ->
                io:format("Failed to start agent: ~p~n", [Reason]),
                {error, {agent_startup_failed, Reason}}
        end
    catch
        Error:ExceptionReason ->
            io:format("Exception in agent execution: ~p:~p~n", [Error, ExceptionReason]),
            % Emergency cleanup
            cleanup_agent_processes_safe(Agent_PId, Monitor_PId, Collector_PId),
            {error, {agent_execution_exception, Error, ExceptionReason}}
    end.

%% Start agent process with timeout and enhanced error handling
start_agent_process_with_timeout(Agent_Id, Timeout) ->
    try
        % Validate agent exists in database first
        case validate_agent_exists(Agent_Id) of
            ok ->
                % Use the existing exoself:start mechanism but with timeout
                StartTime = erlang:timestamp(),
                Agent_PId = exoself:start(Agent_Id, self(), test),
                
                % Verify the process is alive and responsive
                case verify_agent_startup(Agent_PId, StartTime, Timeout) of
                    ok ->
                        {ok, Agent_PId};
                    {error, Reason} ->
                        % Kill the process if it's not responsive
                        case is_process_alive(Agent_PId) of
                            true -> exit(Agent_PId, kill);
                            false -> ok
                        end,
                        {error, Reason}
                end;
            {error, ValidationReason} ->
                {error, ValidationReason}
        end
    catch
        Error:Reason ->
            io:format("Error starting agent: ~p:~p~n", [Error, Reason]),
            {error, {startup_exception, Error, Reason}}
    end.

%% Validate that agent exists in the database
validate_agent_exists(Agent_Id) ->
    try
        case mnesia:system_info(is_running) of
            yes ->
                case mnesia:dirty_read({agent, Agent_Id}) of
                    [] ->
                        {error, {agent_not_found, Agent_Id}};
                    [_Agent] ->
                        ok;
                    Other ->
                        {error, {unexpected_agent_read_result, Other}}
                end;
            Status ->
                {error, {mnesia_not_available, Status}}
        end
    catch
        Error:Reason ->
            {error, {agent_validation_exception, Error, Reason}}
    end.

%% Verify agent startup with timeout
verify_agent_startup(Agent_PId, StartTime, Timeout) ->
    case is_process_alive(Agent_PId) of
        false ->
            {error, process_died_immediately};
        true ->
            % Check if startup took too long
            ElapsedTime = timer:now_diff(erlang:timestamp(), StartTime) / 1000,
            case ElapsedTime > Timeout of
                true ->
                    {error, startup_timeout};
                false ->
                    % Send a test message to verify responsiveness
                    case test_agent_responsiveness(Agent_PId, 2000) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            {error, {agent_not_responsive, Reason}}
                    end
            end
    end.

%% Test agent responsiveness
test_agent_responsiveness(Agent_PId, Timeout) ->
    TestRef = make_ref(),
    Agent_PId ! {self(), test_ping, TestRef},
    
    receive
        {Agent_PId, test_pong, TestRef} ->
            ok;
        {Agent_PId, _, TestRef} ->
            ok;  % Any response is good
        Other ->
            io:format("Unexpected response during agent test: ~p~n", [Other]),
            ok  % Still consider it responsive
    after Timeout ->
        {error, no_response}
    end.

%% Start monitoring processes with error handling
start_monitoring_processes(Agent_PId, TableName, Options) ->
    try
        % Start monitor process
        Monitor_PId = spawn_link(?MODULE, monitor_agent_execution, [Agent_PId, TableName, Options, self()]),
        
        % Verify monitor started
        case is_process_alive(Monitor_PId) of
            false ->
                {error, monitor_failed_to_start};
            true ->
                % Start collector process
                Collector_PId = spawn_link(?MODULE, collect_trading_decisions, [Agent_PId, [], self()]),
                
                % Verify collector started
                case is_process_alive(Collector_PId) of
                    false ->
                        exit(Monitor_PId, kill),
                        {error, collector_failed_to_start};
                    true ->
                        {ok, Monitor_PId, Collector_PId}
                end
        end
    catch
        Error:Reason ->
            {error, {monitoring_process_exception, Error, Reason}}
    end.

%% Start an agent process using the existing exoself mechanism
start_agent_process(Agent_Id) ->
    try
        % Use the existing exoself:start mechanism but with a custom PM_PId (ourselves)
        Agent_PId = exoself:start(Agent_Id, self(), test),
        
        % Verify the process is alive
        case is_process_alive(Agent_PId) of
            true ->
                {ok, Agent_PId};
            false ->
                {error, process_died_immediately}
        end
    catch
        Error:Reason ->
            io:format("Error starting agent: ~p:~p~n", [Error, Reason]),
            {error, {startup_exception, Error, Reason}}
    end.

%% Monitor agent execution and handle timeouts
monitor_agent_execution(Agent_PId, TableName, Options, Parent_PId) ->
    monitor_agent_execution(Agent_PId, TableName, Options, Parent_PId, erlang:timestamp(), 0).

monitor_agent_execution(Agent_PId, TableName, Options, Parent_PId, StartTime, CycleCount) ->
    Timeout = case Options#run_options.verbose of
        true -> 30000;  % 30 seconds for verbose mode
        false -> 10000  % 10 seconds for normal mode
    end,
    
    receive
        {Agent_PId, evaluation_completed, Fitness, Cycles, Time, GoalReached} ->
            io:format("Agent evaluation completed: Fitness=~p, Cycles=~p, Time=~p, Goal=~p~n", 
                     [Fitness, Cycles, Time, GoalReached]),
            
            ExecutionStats = #{
                fitness => Fitness,
                cycles => Cycles,
                time => Time,
                goal_reached => GoalReached,
                total_cycles => CycleCount + Cycles,
                execution_time => timer:now_diff(erlang:timestamp(), StartTime) / 1000000
            },
            
            Parent_PId ! {self(), execution_completed, ExecutionStats};
            
        {Agent_PId, terminated, Fitness} ->
            io:format("Agent terminated with fitness: ~p~n", [Fitness]),
            
            ExecutionStats = #{
                fitness => Fitness,
                total_cycles => CycleCount,
                execution_time => timer:now_diff(erlang:timestamp(), StartTime) / 1000000,
                termination_reason => normal
            },
            
            Parent_PId ! {self(), execution_completed, ExecutionStats};
            
        {Parent_PId, terminate} ->
            io:format("Monitor received termination signal~n"),
            % Send termination to agent if still alive
            case is_process_alive(Agent_PId) of
                true ->
                    Agent_PId ! {self(), terminate};
                false ->
                    ok
            end,
            Parent_PId ! {self(), monitor_terminated};
            
        Other ->
            io:format("Monitor received unexpected message: ~p~n", [Other]),
            monitor_agent_execution(Agent_PId, TableName, Options, Parent_PId, StartTime, CycleCount)
            
    after Timeout ->
        case is_process_alive(Agent_PId) of
            true ->
                io:format("Agent still running, continuing to monitor...~n"),
                monitor_agent_execution(Agent_PId, TableName, Options, Parent_PId, StartTime, CycleCount);
            false ->
                io:format("Agent process died unexpectedly~n"),
                ExecutionStats = #{
                    total_cycles => CycleCount,
                    execution_time => timer:now_diff(erlang:timestamp(), StartTime) / 1000000,
                    termination_reason => unexpected_death
                },
                Parent_PId ! {self(), execution_completed, ExecutionStats}
        end
    end.

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

%% ============================================================================
%% TASK 5: Trading Decision Collection and Tracking
%% ============================================================================

%% Collect trading decisions from the agent with enhanced tracking
collect_trading_decisions(Agent_PId) ->
    collect_trading_decisions(Agent_PId, [], self()).

collect_trading_decisions(Agent_PId, Decisions, Parent_PId) ->
    receive
        % Direct trading decision messages (if sent by modified agents)
        {Agent_PId, trade_decision, Signal, Price, Timestamp} ->
            Decision = #trading_decision{
                timestamp = Timestamp,
                index = undefined,
                price = Price,
                signal = Signal,
                confidence = undefined,
                profit_loss = undefined
            },
            collect_trading_decisions(Agent_PId, [Decision | Decisions], Parent_PId);
            
        % Control messages from parent
        {Parent_PId, get_decisions} ->
            Parent_PId ! {self(), decisions, lists:reverse(Decisions)},
            collect_trading_decisions(Agent_PId, Decisions, Parent_PId);
            
        {Parent_PId, terminate} ->
            io:format("Decision collector terminating with ~p decisions~n", [length(Decisions)]),
            FinalDecisions = lists:reverse(Decisions),
            Parent_PId ! {self(), final_decisions, FinalDecisions};
            
        Other ->
            io:format("Decision collector received unexpected: ~p~n", [Other]),
            collect_trading_decisions(Agent_PId, Decisions, Parent_PId)
            
    after 3000 ->
        % Periodic check - continue collecting
        collect_trading_decisions(Agent_PId, Decisions, Parent_PId)
    end.

%% Calculate statistics for trading decisions
calculate_decision_stats(Decisions) ->
    TotalDecisions = length(Decisions),
    BuyDecisions = length([D || D <- Decisions, D#trading_decision.signal == 1]),
    SellDecisions = length([D || D <- Decisions, D#trading_decision.signal == -1]),
    HoldDecisions = length([D || D <- Decisions, D#trading_decision.signal == 0]),
    
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    
    {TotalProfit, MaxProfit, MaxLoss} = case ProfitLosses of
        [] -> {0, 0, 0};
        _ -> 
            Total = lists:sum(ProfitLosses),
            Max = lists:max(ProfitLosses),
            Min = lists:min(ProfitLosses),
            {Total, Max, Min}
    end,
    
    #{
        total_decisions => TotalDecisions,
        buy_decisions => BuyDecisions,
        sell_decisions => SellDecisions,
        hold_decisions => HoldDecisions,
        total_profit_loss => TotalProfit,
        max_profit => MaxProfit,
        max_loss => MaxLoss,
        avg_profit_per_trade => case TotalDecisions of
            0 -> 0;
            _ -> TotalProfit / TotalDecisions
        end
    }.

%% Wait for agent execution completion
wait_for_completion(Agent_PId, Monitor_PId, Collector_PId, StartTime, _Options) ->
    MaxWaitTime = 60000,  % 60 seconds maximum wait time
    
    receive
        {Monitor_PId, execution_completed, ExecutionStats} ->
            io:format("Execution completed, collecting final decisions...~n"),
            
            % Get final decisions from collector
            Collector_PId ! {self(), terminate},
            receive
                {Collector_PId, final_decisions, Decisions} ->
                    Results = #agent_run_results{
                        agent_id = Agent_PId,  % We'll update this with actual agent ID
                        data_source = undefined,  % Will be filled by caller
                        start_time = StartTime,
                        end_time = erlang:timestamp(),
                        total_decisions = length(Decisions),
                        buy_decisions = length([D || D <- Decisions, D#trading_decision.signal == 1]),
                        sell_decisions = length([D || D <- Decisions, D#trading_decision.signal == -1]),
                        hold_decisions = length([D || D <- Decisions, D#trading_decision.signal == 0]),
                        total_profit_loss = 0,  % Will be calculated later
                        max_profit = 0,
                        max_loss = 0,
                        trading_decisions = Decisions,
                        execution_stats = ExecutionStats
                    },
                    
                    {ok, Results}
            after 5000 ->
                io:format("Timeout waiting for final decisions~n"),
                {error, decision_collection_timeout}
            end;
            
        {error, Reason} ->
            {error, Reason}
            
    after MaxWaitTime ->
        io:format("Execution timeout after ~p ms~n", [MaxWaitTime]),
        {error, execution_timeout}
    end.

%% Clean up agent processes
cleanup_agent_processes(Agent_PId, Monitor_PId, Collector_PId) ->
    % Terminate monitor
    case is_process_alive(Monitor_PId) of
        true ->
            Monitor_PId ! {self(), terminate},
            receive
                {Monitor_PId, monitor_terminated} -> ok
            after 2000 -> 
                exit(Monitor_PId, kill)
            end;
        false -> ok
    end,
    
    % Terminate collector
    case is_process_alive(Collector_PId) of
        true ->
            Collector_PId ! {self(), terminate};
        false -> ok
    end,
    
    % Terminate agent if still alive
    case is_process_alive(Agent_PId) of
        true ->
            Agent_PId ! {self(), terminate},
            timer:sleep(1000);  % Give it time to clean up
        false -> ok
    end,
    
    io:format("Agent processes cleaned up~n").

%% Analyze time patterns in decisions
analyze_time_patterns(Decisions) ->
    case Decisions of
        [] -> #{};
        _ ->
            Timestamps = [D#trading_decision.timestamp || D <- Decisions],
            
            % Calculate time intervals between decisions
            Intervals = calculate_time_intervals(Timestamps),
            
            #{
                total_duration => timer:now_diff(lists:last(Timestamps), hd(Timestamps)) / 1000000,
                avg_interval => case length(Intervals) of
                    0 -> 0;
                    N -> lists:sum(Intervals) / N
                end,
                min_interval => case Intervals of [] -> 0; _ -> lists:min(Intervals) end,
                max_interval => case Intervals of [] -> 0; _ -> lists:max(Intervals) end
            }
    end.

%% Assess overall decision quality
assess_decision_quality(Decisions) ->
    ProfitableDecisions = [D || D <- Decisions, 
                          D#trading_decision.profit_loss =/= undefined,
                          D#trading_decision.profit_loss > 0],
    
    TotalDecisions = length(Decisions),
    ProfitableCount = length(ProfitableDecisions),
    
    % Calculate quality metrics
    SuccessRate = case TotalDecisions of
        0 -> 0.0;
        _ -> (ProfitableCount / TotalDecisions) * 100
    end,
    
    % Calculate average confidence for profitable vs unprofitable decisions
    ProfitableConfidence = case ProfitableDecisions of
        [] -> 0.0;
        _ -> 
            Confidences = [D#trading_decision.confidence || D <- ProfitableDecisions,
                          D#trading_decision.confidence =/= undefined],
            case Confidences of
                [] -> 0.0;
                _ -> lists:sum(Confidences) / length(Confidences)
            end
    end,
    
    #{
        success_rate => SuccessRate,
        profitable_confidence => ProfitableConfidence,
        decision_consistency => calculate_consistency(Decisions),
        overall_quality => case SuccessRate of
            Rate when Rate >= 70 -> excellent;
            Rate when Rate >= 60 -> good;
            Rate when Rate >= 50 -> average;
            Rate when Rate >= 40 -> below_average;
            _ -> poor
        end
    }.

%% Helper functions for analysis

count_sequences(Signals) ->
    count_sequences(Signals, #{}, undefined, 0).

count_sequences([], Acc, _Current, _Count) ->
    Acc;
count_sequences([Signal | Rest], Acc, Current, Count) ->
    case Signal of
        Current ->
            count_sequences(Rest, Acc, Current, Count + 1);
        _ ->
            NewAcc = case Current of
                undefined -> Acc;
                _ -> maps:put(Current, maps:get(Current, Acc, 0) + 1, Acc)
            end,
            count_sequences(Rest, NewAcc, Signal, 1)
    end.

count_signal_changes(Signals) ->
    count_signal_changes(Signals, 0, undefined).

count_signal_changes([], Changes, _Prev) ->
    Changes;
count_signal_changes([Signal | Rest], Changes, Prev) ->
    case Signal of
        Prev -> count_signal_changes(Rest, Changes, Signal);
        _ -> count_signal_changes(Rest, Changes + 1, Signal)
    end.

calculate_time_intervals([]) -> [];
calculate_time_intervals([_]) -> [];
calculate_time_intervals([T1, T2 | Rest]) ->
    Interval = timer:now_diff(T2, T1) / 1000000,  % Convert to seconds
    [Interval | calculate_time_intervals([T2 | Rest])].

calculate_consistency(Decisions) ->
    % Calculate consistency based on signal patterns and profit correlation
    Signals = [D#trading_decision.signal || D <- Decisions],
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    
    case {length(Signals), length(ProfitLosses)} of
        {0, _} -> 0.0;
        {_, 0} -> 0.0;
        {SigLen, PLLen} when SigLen < 3 -> 50.0;  % Not enough data
        _ ->
            % Calculate signal consistency (fewer random changes = higher consistency)
            Changes = count_signal_changes(Signals),
            SignalConsistency = max(0, 100 - (Changes / length(Signals) * 100)),
            
            % Calculate profit consistency (less variance = higher consistency)
            Mean = lists:sum(ProfitLosses) / length(ProfitLosses),
            StdDev = calculate_std_dev(ProfitLosses, Mean),
            ProfitConsistency = case Mean of
                0 -> 0;
                _ -> max(0, 100 - (StdDev / abs(Mean) * 100))
            end,
            
            % Weighted average
            (SignalConsistency * 0.4) + (ProfitConsistency * 0.6)
    end.

%% Format summary as string
format_summary_string(Summary) ->
    io_lib:format(
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
         maps:get(data_source, Summary),
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
         maps:get(max_loss, Summary)]).

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
        [maps:get(consecutive_buys, SignalSeq, 0),
         maps:get(consecutive_sells, SignalSeq, 0),
         maps:get(consecutive_holds, SignalSeq, 0),
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
        "  Consistency Score: ~.2f%~n"
        "  Overall Quality: ~p~n"
        "=== END DETAILED ANALYSIS ===~n",
        [maps:get(success_rate, Quality, 0),
         maps:get(decision_consistency, Quality, 0),
         maps:get(overall_quality, Quality, unknown)]),
    
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

%% Analyze time patterns
analyze_time_patterns(Decisions) ->
    case Decisions of
        [] ->
            #{total_duration => 0, avg_interval => 0};
        [_] ->
            #{total_duration => 0, avg_interval => 0};
        _ ->
            Timestamps = [D#trading_decision.timestamp || D <- Decisions],
            SortedTimestamps = lists:sort(Timestamps),
            
            FirstTime = hd(SortedTimestamps),
            LastTime = lists:last(SortedTimestamps),
            TotalDuration = timer:now_diff(LastTime, FirstTime) / 1000000,  % seconds
            
            % Calculate intervals between decisions
            Intervals = calculate_intervals(SortedTimestamps),
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

%% Calculate intervals between timestamps
calculate_intervals([]) -> [];
calculate_intervals([_]) -> [];
calculate_intervals([T1, T2 | Rest]) ->
    Interval = timer:now_diff(T2, T1) / 1000000,  % seconds
    [Interval | calculate_intervals([T2 | Rest])].

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

%% Helper functions for string formatting
format_summary_string(Summary) ->
    lists:flatten(io_lib:format(
        "Agent: ~p | Data: ~s | Time: ~.2fs~n"
        "Decisions: ~p total (~p buy, ~p sell, ~p hold)~n"
        "Success Rate: ~.2f% | Win/Loss Ratio: ~.2f~n"
        "Avg P&L per Trade: ~.4f% | Total P&L: ~.4f%~n"
        "Max Profit: ~.4f% | Max Loss: ~.4f%~n~n",
        [maps:get(agent_id, Summary),
         filename:basename(maps:get(data_source, Summary, "unknown")),
         maps:get(execution_time, Summary),
         maps:get(total_decisions, Summary),
         maps:get(buy_decisions, Summary),
         maps:get(sell_decisions, Summary),
         maps:get(hold_decisions, Summary),
         maps:get(success_rate, Summary),
         maps:get(win_loss_ratio, Summary),
         maps:get(avg_profit_per_trade, Summary),
         maps:get(total_profit_loss, Summary),
         maps:get(max_profit, Summary),
         maps:get(max_loss, Summary)])).

format_detailed_string(DetailedResults) ->
    Summary = format_summary_string(DetailedResults),
    Analysis = maps:get(decision_analysis, DetailedResults, #{}),
    
    AnalysisStr = io_lib:format(
        "Decision Analysis:~n"
        "  Signal Changes: ~p~n"
        "  Decision Quality Score: ~.2f%~n~n",
        [maps:get(signal_changes, maps:get(signal_sequences, Analysis, #{}), 0),
         maps:get(success_rate, maps:get(decision_quality, Analysis, #{}), 0)]),
    
    lists:flatten([Summary, AnalysisStr]).

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                 [Year, Month, Day, Hour, Min, Sec]).

%% Simple JSON formatting helper
format_simple_json(Map) ->
    % Very simple JSON formatting - would use proper JSON library in production
    "{" ++ format_json_pairs(maps:to_list(Map)) ++ "}".

format_json_pairs([]) -> "";
format_json_pairs([{Key, Value}]) ->
    format_json_pair(Key, Value);
format_json_pairs([{Key, Value} | Rest]) ->
    format_json_pair(Key, Value) ++ "," ++ format_json_pairs(Rest).

format_json_pair(Key, Value) when is_atom(Key) ->
    format_json_pair(atom_to_list(Key), Value);
format_json_pair(Key, Value) when is_list(Key) ->
    "\"" ++ Key ++ "\":" ++ format_json_value(Value).

format_json_value(Value) when is_number(Value) ->
    io_lib:format("~p", [Value]);
format_json_value(Value) when is_atom(Value) ->
    "\"" ++ atom_to_list(Value) ++ "\"";
format_json_value(Value) when is_list(Value) ->
    "\"" ++ Value ++ "\"";
format_json_value(Value) when is_map(Value) ->
    format_simple_json(Value);
format_json_value(_) ->
    "null".

%% ============================================================================
%% TASK 5: Trading Decision Collection and Tracking
%% ============================================================================

%% Collect trading decisions from the agent with enhanced tracking
collect_trading_decisions(Agent_PId) ->
    collect_trading_decisions(Agent_PId, [], self()).

collect_trading_decisions(Agent_PId, Decisions, Parent_PId) ->
    receive
        % Direct trading decision messages (if sent by modified agents)
        {Agent_PId, trade_decision, Signal, Price, Timestamp} ->
            Decision = #trading_decision{
                timestamp = Timestamp,
                index = undefined,
                price = Price,
                signal = Signal,
                confidence = undefined,
                profit_loss = undefined
            },
            collect_trading_decisions(Agent_PId, [Decision | Decisions], Parent_PId);
            
        % Intercept trade messages sent to fx scape
        {From, trade, TableName, TradeSignal} when is_pid(From) ->
            Timestamp = erlang:timestamp(),
            
            % Get current price from the table if available
            Price = get_current_price(TableName),
            Index = get_current_index(TableName),
            
            Decision = #trading_decision{
                timestamp = Timestamp,
                index = Index,
                price = Price,
                signal = TradeSignal,
                confidence = undefined,  % Will be filled later
                profit_loss = undefined  % Will be calculated later
            },
            
            % Forward the message to the actual fx scape
            forward_trade_message(From, TableName, TradeSignal),
            
            collect_trading_decisions(Agent_PId, [Decision | Decisions], Parent_PId);
            
        % Messages from fx scape with fitness results
        {From, Fitness, HaltFlag} when is_number(Fitness) ->
            % This is a response from fx scape - update the last decision with profit info
            UpdatedDecisions = update_last_decision_profit(Decisions, Fitness),
            
            % Forward the response back to the agent
            Agent_PId ! {From, Fitness, HaltFlag},
            
            collect_trading_decisions(Agent_PId, UpdatedDecisions, Parent_PId);
            
        % Control messages from parent
        {Parent_PId, get_decisions} ->
            Parent_PId ! {self(), decisions, lists:reverse(Decisions)},
            collect_trading_decisions(Agent_PId, Decisions, Parent_PId);
            
        {Parent_PId, get_decisions_with_stats} ->
            Stats = calculate_decision_stats(Decisions),
            Parent_PId ! {self(), decisions_with_stats, {lists:reverse(Decisions), Stats}},
            collect_trading_decisions(Agent_PId, Decisions, Parent_PId);
            
        {Parent_PId, terminate} ->
            io:format("Decision collector terminating with ~p decisions~n", [length(Decisions)]),
            FinalDecisions = calculate_all_profits(lists:reverse(Decisions)),
            Parent_PId ! {self(), final_decisions, FinalDecisions};
            
        Other ->
            io:format("Decision collector received unexpected: ~p~n", [Other]),
            collect_trading_decisions(Agent_PId, Decisions, Parent_PId)
            
    after 3000 ->
        % Periodic check - continue collecting
        collect_trading_decisions(Agent_PId, Decisions, Parent_PId)
    end.

%% Get current price from the trading table
get_current_price(TableName) ->
    try
        % First try to get from process dictionary (for testing)
        case get(current_price) of
            undefined -> 
                % Try to get from fx module if available
                case catch fx:current_price(TableName) of
                    {ok, Price} -> Price;
                    _ -> undefined
                end;
            Price -> Price
        end
    catch
        _:_ -> undefined
    end.

%% Get current index from the trading table
get_current_index(TableName) ->
    try
        case get(current_index) of
            undefined -> 
                % Try to get from fx state if available
                case fx:current_index(TableName) of
                    {ok, Index} -> Index;
                    _ -> undefined
                end;
            Index -> Index
        end
    catch
        _:_ -> undefined
    end.

%% Forward trade message to the actual fx scape
forward_trade_message(From, TableName, TradeSignal) ->
    % Find the fx scape process and forward the message
    case whereis(fx_sim) of
        undefined ->
            % If no named process, try to find it via registered processes
            case find_fx_scape() of
                {ok, FxPid} ->
                    FxPid ! {From, trade, TableName, TradeSignal};
                error ->
                    io:format("Warning: Could not find fx scape to forward trade message~n")
            end;
        FxPid ->
            FxPid ! {From, trade, TableName, TradeSignal}
    end.

%% Find the fx scape process
find_fx_scape() ->
    % Look for processes that might be the fx scape
    Processes = processes(),
    case find_fx_process(Processes) of
        {ok, Pid} -> {ok, Pid};
        error -> error
    end.

find_fx_process([]) ->
    error;
find_fx_process([Pid | Rest]) ->
    try
        case process_info(Pid, dictionary) of
            {dictionary, Dict} ->
                case lists:keyfind(fx_scape, 1, Dict) of
                    {fx_scape, true} -> {ok, Pid};
                    _ -> find_fx_process(Rest)
                end;
            _ ->
                find_fx_process(Rest)
        end
    catch
        _:_ -> find_fx_process(Rest)
    end.

%% Update the last decision with profit information
update_last_decision_profit([], _Fitness) ->
    [];
update_last_decision_profit([LastDecision | RestDecisions], Fitness) ->
    UpdatedDecision = LastDecision#trading_decision{
        profit_loss = Fitness,
        confidence = abs(Fitness)  % Use fitness as confidence measure
    },
    [UpdatedDecision | RestDecisions].

%% Calculate profit/loss for all decisions
calculate_all_profits(Decisions) ->
    calculate_all_profits(Decisions, [], undefined).

calculate_all_profits([], Acc, _PrevPrice) ->
    lists:reverse(Acc);
calculate_all_profits([Decision | Rest], Acc, PrevPrice) ->
    CurrentPrice = case Decision#trading_decision.price of
        undefined -> PrevPrice;
        Price -> Price
    end,
    
    % Calculate profit based on signal and price movement
    % For trading decisions, we need to look ahead to the next price to calculate profit
    ProfitLoss = case Decision#trading_decision.signal of
        1 when CurrentPrice > 0 ->  % Buy signal
            case Rest of
                [NextDecision | _] ->
                    NextPrice = case NextDecision#trading_decision.price of
                        undefined -> CurrentPrice;
                        NP -> NP
                    end,
                    (NextPrice - CurrentPrice) / CurrentPrice * 100;  % Percentage gain from buy
                _ when PrevPrice > 0 ->
                    (CurrentPrice - PrevPrice) / PrevPrice * 100;  % Use previous price if no next
                _ -> 0
            end;
            
        -1 when CurrentPrice > 0 -> % Sell signal
            case Rest of
                [NextDecision | _] ->
                    NextPrice = case NextDecision#trading_decision.price of
                        undefined -> CurrentPrice;
                        NP -> NP
                    end,
                    (CurrentPrice - NextPrice) / CurrentPrice * 100;  % Percentage gain from short
                _ when PrevPrice > 0 ->
                    (PrevPrice - CurrentPrice) / PrevPrice * 100;  % Use previous price if no next
                _ -> 0
            end;
            
        0 -> 0;  % Hold signal
        _ -> 0   % Invalid signal or no price data
    end,
    
    UpdatedDecision = Decision#trading_decision{
        profit_loss = ProfitLoss,
        confidence = abs(ProfitLoss)
    },
    
    calculate_all_profits(Rest, [UpdatedDecision | Acc], CurrentPrice).

%% Calculate statistics for trading decisions
calculate_decision_stats(Decisions) ->
    TotalDecisions = length(Decisions),
    BuyDecisions = length([D || D <- Decisions, D#trading_decision.signal == 1]),
    SellDecisions = length([D || D <- Decisions, D#trading_decision.signal == -1]),
    HoldDecisions = length([D || D <- Decisions, D#trading_decision.signal == 0]),
    
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    
    {TotalProfit, MaxProfit, MaxLoss} = case ProfitLosses of
        [] -> {0, 0, 0};
        _ -> 
            Total = lists:sum(ProfitLosses),
            Max = lists:max(ProfitLosses),
            Min = lists:min(ProfitLosses),
            {Total, Max, Min}
    end,
    
    #{
        total_decisions => TotalDecisions,
        buy_decisions => BuyDecisions,
        sell_decisions => SellDecisions,
        hold_decisions => HoldDecisions,
        total_profit_loss => TotalProfit,
        max_profit => MaxProfit,
        max_loss => MaxLoss,
        avg_profit_per_trade => case TotalDecisions of
            0 -> 0;
            _ -> TotalProfit / TotalDecisions
        end
    }.

%% Wait for agent execution completion
wait_for_completion(Agent_PId, Monitor_PId, Collector_PId, StartTime, Options) ->
    MaxWaitTime = 60000,  % 60 seconds maximum wait time
    
    receive
        {Monitor_PId, execution_completed, ExecutionStats} ->
            io:format("Execution completed, collecting final decisions...~n"),
            
            % Get final decisions from collector
            Collector_PId ! {self(), terminate},
            receive
                {Collector_PId, final_decisions, Decisions} ->
                    TotalTime = timer:now_diff(erlang:timestamp(), StartTime) / 1000000,
                    
                    Results = #agent_run_results{
                        agent_id = Agent_PId,  % We'll update this with actual agent ID
                        data_source = undefined,  % Will be filled by caller
                        start_time = StartTime,
                        end_time = erlang:timestamp(),
                        total_decisions = length(Decisions),
                        buy_decisions = length([D || D <- Decisions, D#trading_decision.signal == 1]),
                        sell_decisions = length([D || D <- Decisions, D#trading_decision.signal == -1]),
                        hold_decisions = length([D || D <- Decisions, D#trading_decision.signal == 0]),
                        total_profit_loss = 0,  % Will be calculated later
                        max_profit = 0,
                        max_loss = 0,
                        trading_decisions = Decisions,
                        execution_stats = ExecutionStats
                    },
                    
                    {ok, Results}
            after 5000 ->
                io:format("Timeout waiting for final decisions~n"),
                {error, decision_collection_timeout}
            end;
            
        {error, Reason} ->
            {error, Reason}
            
    after MaxWaitTime ->
        io:format("Execution timeout after ~p ms~n", [MaxWaitTime]),
        {error, execution_timeout}
    end.

%% Clean up agent processes
cleanup_agent_processes(Agent_PId, Monitor_PId, Collector_PId) ->
    % Terminate monitor
    case is_process_alive(Monitor_PId) of
        true ->
            Monitor_PId ! {self(), terminate},
            receive
                {Monitor_PId, monitor_terminated} -> ok
            after 2000 -> 
                exit(Monitor_PId, kill)
            end;
        false -> ok
    end,
    
    % Terminate collector
    case is_process_alive(Collector_PId) of
        true ->
            Collector_PId ! {self(), terminate};
        false -> ok
    end,
    
    % Terminate agent if still alive
    case is_process_alive(Agent_PId) of
        true ->
            Agent_PId ! {self(), terminate},
            timer:sleep(1000);  % Give it time to clean up
        false -> ok
    end,
    
    io:format("Agent processes cleaned up~n").

%% ============================================================================
%% TASK 7: Result Formatting and Reporting
%% ============================================================================

%% Main result formatting function - generates comprehensive trading reports
format_results(Results) ->
    format_results(Results, #{format => detailed, output => console}).

format_results(Results, Options) ->
    Format = maps:get(format, Options, detailed),
    Output = maps:get(output, Options, console),
    
    case Format of
        summary ->
            format_summary_results(Results, Output);
        detailed ->
            format_detailed_results(Results, Output);
        csv ->
            format_csv_results(Results, Output);
        json ->
            format_json_results(Results, Output);
        _ ->
            format_detailed_results(Results, Output)
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
    
    % Calculate average profit per trade
    TotalPL = Results#agent_run_results.total_profit_loss,
    AvgProfitPerTrade = case Results#agent_run_results.total_decisions of
        0 -> 0.0;
        TotalDecisions -> TotalPL / TotalDecisions
    end,
    
    % Calculate win/loss ratio
    LossDecisions = [D || D <- Decisions, 
                     D#trading_decision.profit_loss =/= undefined,
                     D#trading_decision.profit_loss < 0],
    
    WinLossRatio = case length(LossDecisions) of
        0 -> case length(ProfitableDecisions) of
            0 -> 0.0;
            _ -> 999.0  % Use large number instead of infinity
        end;
        Losses -> length(ProfitableDecisions) / Losses
    end,
    
    Summary = #{
        agent_id => Results#agent_run_results.agent_id,
        data_source => filename:basename(Results#agent_run_results.data_source),
        execution_time => timer:now_diff(Results#agent_run_results.end_time, 
                                       Results#agent_run_results.start_time) / 1000000,
        total_decisions => Results#agent_run_results.total_decisions,
        buy_decisions => Results#agent_run_results.buy_decisions,
        sell_decisions => Results#agent_run_results.sell_decisions,
        hold_decisions => Results#agent_run_results.hold_decisions,
        total_profit_loss => TotalPL,
        max_profit => Results#agent_run_results.max_profit,
        max_loss => Results#agent_run_results.max_loss,
        success_rate => SuccessRate,
        avg_profit_per_trade => AvgProfitPerTrade,
        win_loss_ratio => WinLossRatio,
        profitable_trades => length(ProfitableDecisions),
        losing_trades => length(LossDecisions)
    },
    
    case Output of
        console ->
            print_formatted_summary(Summary),
            Summary;
        string ->
            format_summary_string(Summary);
        _ ->
            Summary
    end.

%% Format detailed results
format_detailed_results(Results, Output) ->
    Summary = format_summary_results(Results, none),
    
    % Add detailed decision analysis
    Decisions = Results#agent_run_results.trading_decisions,
    DecisionAnalysis = analyze_trading_decisions(Decisions),
    
    % Add execution statistics
    ExecStats = case Results#agent_run_results.execution_stats of
        undefined -> #{};
        Stats -> Stats
    end,
    
    DetailedResults = Summary#{
        decision_analysis => DecisionAnalysis,
        execution_stats => ExecStats,
        trading_decisions => case length(Decisions) =< 100 of
            true -> Decisions;  % Include all decisions if not too many
            false -> lists:sublist(Decisions, 100)  % Limit to first 100
        end
    },
    
    case Output of
        console ->
            print_formatted_detailed(DetailedResults),
            DetailedResults;
        string ->
            format_detailed_string(DetailedResults);
        _ ->
            DetailedResults
    end.

%% Format results as CSV
format_csv_results(Results, Output) ->
    Decisions = Results#agent_run_results.trading_decisions,
    
    % CSV Header
    Header = "Timestamp,Index,Price,Signal,Confidence,ProfitLoss\n",
    
    % CSV Rows
    Rows = lists:map(fun(Decision) ->
        TimestampStr = format_timestamp(Decision#trading_decision.timestamp),
        IndexStr = case Decision#trading_decision.index of
            undefined -> "";
            Index -> integer_to_list(Index)
        end,
        PriceStr = case Decision#trading_decision.price of
            undefined -> "";
            Price -> float_to_list(Price, [{decimals, 4}])
        end,
        SignalStr = case Decision#trading_decision.signal of
            1 -> "BUY";
            -1 -> "SELL";
            0 -> "HOLD";
            _ -> "UNKNOWN"
        end,
        ConfidenceStr = case Decision#trading_decision.confidence of
            undefined -> "";
            Conf -> float_to_list(Conf, [{decimals, 4}])
        end,
        PLStr = case Decision#trading_decision.profit_loss of
            undefined -> "";
            PL -> float_to_list(PL, [{decimals, 4}])
        end,
        
        io_lib:format("~s,~s,~s,~s,~s,~s~n", 
                     [TimestampStr, IndexStr, PriceStr, SignalStr, ConfidenceStr, PLStr])
    end, Decisions),
    
    CSV = Header ++ lists:flatten(Rows),
    
    case Output of
        console ->
            io:format("~s", [CSV]),
            CSV;
        string ->
            CSV;
        _ ->
            CSV
    end.

%% Format results as JSON (simplified)
format_json_results(Results, Output) ->
    Summary = format_summary_results(Results, none),
    
    % Convert to JSON-like structure (simplified since Erlang doesn't have built-in JSON)
    JsonMap = #{
        summary => Summary,
        decisions => [decision_to_map(D) || D <- Results#agent_run_results.trading_decisions]
    },
    
    % Simple JSON formatting (would use a proper JSON library in production)
    JsonString = format_simple_json(JsonMap),
    
    case Output of
        console ->
            io:format("~s~n", [JsonString]),
            JsonString;
        string ->
            JsonString;
        _ ->
            JsonMap
    end.

%% Analyze trading decisions for patterns and insights
analyze_trading_decisions(Decisions) ->
    % Analyze decision patterns
    SignalSequences = analyze_signal_sequences(Decisions),
    ProfitDistribution = analyze_profit_distribution(Decisions),
    TimePatterns = analyze_time_patterns(Decisions),
    
    #{
        signal_sequences => SignalSequences,
        profit_distribution => ProfitDistribution,
        time_patterns => TimePatterns,
        decision_quality => assess_decision_quality(Decisions)
    }.

%% Analyze signal sequences (e.g., consecutive buys, sells)
analyze_signal_sequences(Decisions) ->
    Signals = [D#trading_decision.signal || D <- Decisions],
    
    % Count consecutive sequences
    Sequences = count_sequences(Signals),
    
    #{
        consecutive_buys => maps:get(1, Sequences, 0),
        consecutive_sells => maps:get(-1, Sequences, 0),
        consecutive_holds => maps:get(0, Sequences, 0),
        signal_changes => count_signal_changes(Signals)
    }.

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
                quartiles => Quartiles,
                range => {lists:min(ProfitLosses), lists:max(ProfitLosses)}
            }
    end.

%% Analyze time patterns in decisions
analyze_time_patterns(Decisions) ->
    case Decisions of
        [] -> #{};
        _ ->
            Timestamps = [D#trading_decision.timestamp || D <- Decisions],
            
            % Calculate time intervals between decisions
            Intervals = calculate_time_intervals(Timestamps),
            
            #{
                total_duration => timer:now_diff(lists:last(Timestamps), hd(Timestamps)) / 1000000,
                avg_interval => case length(Intervals) of
                    0 -> 0;
                    N -> lists:sum(Intervals) / N
                end,
                min_interval => case Intervals of [] -> 0; _ -> lists:min(Intervals) end,
                max_interval => case Intervals of [] -> 0; _ -> lists:max(Intervals) end
            }
    end.

%% Assess overall decision quality
assess_decision_quality(Decisions) ->
    ProfitableDecisions = [D || D <- Decisions, 
                          D#trading_decision.profit_loss =/= undefined,
                          D#trading_decision.profit_loss > 0],
    
    TotalDecisions = length(Decisions),
    ProfitableCount = length(ProfitableDecisions),
    
    % Calculate quality metrics
    SuccessRate = case TotalDecisions of
        0 -> 0.0;
        _ -> (ProfitableCount / TotalDecisions) * 100
    end,
    
    % Calculate average confidence for profitable vs unprofitable decisions
    ProfitableConfidence = case ProfitableDecisions of
        [] -> 0.0;
        _ -> 
            Confidences = [D#trading_decision.confidence || D <- ProfitableDecisions,
                          D#trading_decision.confidence =/= undefined],
            case Confidences of
                [] -> 0.0;
                _ -> lists:sum(Confidences) / length(Confidences)
            end
    end,
    
    #{
        success_rate => SuccessRate,
        profitable_confidence => ProfitableConfidence,
        decision_consistency => calculate_consistency(Decisions),
        overall_quality => case SuccessRate of
            Rate when Rate >= 70 -> excellent;
            Rate when Rate >= 60 -> good;
            Rate when Rate >= 50 -> average;
            Rate when Rate >= 40 -> below_average;
            _ -> poor
        end
    }.

%% Helper functions for analysis

count_sequences(Signals) ->
    count_sequences(Signals, #{}, undefined, 0).

count_sequences([], Acc, _Current, _Count) ->
    Acc;
count_sequences([Signal | Rest], Acc, Current, Count) ->
    case Signal of
        Current ->
            count_sequences(Rest, Acc, Current, Count + 1);
        _ ->
            NewAcc = case Current of
                undefined -> Acc;
                _ -> maps:put(Current, maps:get(Current, Acc, 0) + 1, Acc)
            end,
            count_sequences(Rest, NewAcc, Signal, 1)
    end.

count_signal_changes(Signals) ->
    count_signal_changes(Signals, 0, undefined).

count_signal_changes([], Changes, _Prev) ->
    Changes;
count_signal_changes([Signal | Rest], Changes, Prev) ->
    case Signal of
        Prev -> count_signal_changes(Rest, Changes, Signal);
        _ -> count_signal_changes(Rest, Changes + 1, Signal)
    end.

calculate_median([]) -> 0;
calculate_median(SortedList) ->
    N = length(SortedList),
    case N rem 2 of
        1 -> lists:nth((N + 1) div 2, SortedList);
        0 -> 
            Mid1 = lists:nth(N div 2, SortedList),
            Mid2 = lists:nth((N div 2) + 1, SortedList),
            (Mid1 + Mid2) / 2
    end.

calculate_std_dev(Values, Mean) ->
    Variances = [(X - Mean) * (X - Mean) || X <- Values],
    Variance = lists:sum(Variances) / length(Values),
    math:sqrt(Variance).

calculate_quartiles(SortedList) ->
    N = length(SortedList),
    Q1_Pos = N div 4,
    Q2_Pos = N div 2,
    Q3_Pos = (3 * N) div 4,
    
    Q1 = case Q1_Pos of 0 -> hd(SortedList); _ -> lists:nth(Q1_Pos, SortedList) end,
    Q2 = calculate_median(SortedList),
    Q3 = case Q3_Pos >= N of
        true -> lists:last(SortedList);
        false -> lists:nth(Q3_Pos + 1, SortedList)
    end,
    
    {Q1, Q2, Q3}.

calculate_time_intervals([]) -> [];
calculate_time_intervals([_]) -> [];
calculate_time_intervals([T1, T2 | Rest]) ->
    Interval = timer:now_diff(T2, T1) / 1000000,  % Convert to seconds
    [Interval | calculate_time_intervals([T2 | Rest])].

calculate_consistency(Decisions) ->
    % Calculate consistency based on signal patterns and profit correlation
    Signals = [D#trading_decision.signal || D <- Decisions],
    ProfitLosses = [D#trading_decision.profit_loss || D <- Decisions, 
                    D#trading_decision.profit_loss =/= undefined],
    
    case {length(Signals), length(ProfitLosses)} of
        {0, _} -> 0.0;
        {_, 0} -> 0.0;
        {SigLen, PLLen} when SigLen < 3 -> 50.0;  % Not enough data
        _ ->
            % Calculate signal consistency (fewer random changes = higher consistency)
            Changes = count_signal_changes(Signals),
            SignalConsistency = max(0, 100 - (Changes / length(Signals) * 100)),
            
            % Calculate profit consistency (less variance = higher consistency)
            Mean = lists:sum(ProfitLosses) / length(ProfitLosses),
            StdDev = calculate_std_dev(ProfitLosses, Mean),
            ProfitConsistency = case Mean of
                0 -> 0;
                _ -> max(0, 100 - (StdDev / abs(Mean) * 100))
            end,
            
            % Weighted average
            (SignalConsistency * 0.4) + (ProfitConsistency * 0.6)
    end.

%% Format summary as string
format_summary_string(Summary) ->
    io_lib:format(
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
         maps:get(data_source, Summary),
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
         maps:get(max_loss, Summary)]).

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
        [maps:get(consecutive_buys, SignalSeq, 0),
         maps:get(consecutive_sells, SignalSeq, 0),
         maps:get(consecutive_holds, SignalSeq, 0),
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
        "  Consistency Score: ~.2f%~n"
        "  Overall Quality: ~p~n"
        "=== END DETAILED ANALYSIS ===~n",
        [maps:get(success_rate, Quality, 0),
         maps:get(decision_consistency, Quality, 0),
         maps:get(overall_quality, Quality, unknown)]),
    
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

%% Print formatted summary to console
print_formatted_summary(Summary) ->
    io:format("~n=== TRADING RESULTS SUMMARY ===~n"),
    io:format("Agent: ~p~n", [maps:get(agent_id, Summary)]),
    io:format("Data Source: ~s~n", [maps:get(data_source, Summary)]),
    io:format("Execution Time: ~.2f seconds~n", [maps:get(execution_time, Summary)]),
    io:format("~nDecision Statistics:~n"),
    io:format("  Total Decisions: ~p~n", [maps:get(total_decisions, Summary)]),
    io:format("  Buy Decisions: ~p~n", [maps:get(buy_decisions, Summary)]),
    io:format("  Sell Decisions: ~p~n", [maps:get(sell_decisions, Summary)]),
    io:format("  Hold Decisions: ~p~n", [maps:get(hold_decisions, Summary)]),
    io:format("~nPerformance Metrics:~n"),
    io:format("  Success Rate: ~.2f%~n", [maps:get(success_rate, Summary)]),
    io:format("  Win/Loss Ratio: ~.2f~n", [maps:get(win_loss_ratio, Summary)]),
    io:format("  Profitable Trades: ~p~n", [maps:get(profitable_trades, Summary)]),
    io:format("  Losing Trades: ~p~n", [maps:get(losing_trades, Summary)]),
    io:format("~nProfit/Loss Analysis:~n"),
    io:format("  Total P&L: ~.4f%~n", [maps:get(total_profit_loss, Summary)]),
    io:format("  Average P&L per Trade: ~.4f%~n", [maps:get(avg_profit_per_trade, Summary)]),
    io:format("  Max Profit: ~.4f%~n", [maps:get(max_profit, Summary)]),
    io:format("  Max Loss: ~.4f%~n", [maps:get(max_loss, Summary)]),
    io:format("=== END SUMMARY ===~n~n").

%% Print formatted detailed results to console
print_formatted_detailed(DetailedResults) ->
    print_formatted_summary(DetailedResults),
    
    % Print decision analysis
    Analysis = maps:get(decision_analysis, DetailedResults, #{}),
    io:format("=== DETAILED ANALYSIS ===~n"),
    
    % Signal sequence analysis
    SignalSeq = maps:get(signal_sequences, Analysis, #{}),
    io:format("Signal Patterns:~n"),
    io:format("  Consecutive Buys: ~p~n", [maps:get(consecutive_buys, SignalSeq, 0)]),
    io:format("  Consecutive Sells: ~p~n", [maps:get(consecutive_sells, SignalSeq, 0)]),
    io:format("  Consecutive Holds: ~p~n", [maps:get(consecutive_holds, SignalSeq, 0)]),
    io:format("  Signal Changes: ~p~n", [maps:get(signal_changes, SignalSeq, 0)]),
    
    % Profit distribution analysis
    ProfitDist = maps:get(profit_distribution, Analysis, #{}),
    io:format("~nProfit Distribution:~n"),
    io:format("  Mean: ~.4f%~n", [maps:get(mean, ProfitDist, 0)]),
    io:format("  Median: ~.4f%~n", [maps:get(median, ProfitDist, 0)]),
    io:format("  Standard Deviation: ~.4f~n", [maps:get(std_dev, ProfitDist, 0)]),
    
    case maps:get(quartiles, ProfitDist, undefined) of
        {Q1, Q2, Q3} ->
            io:format("  Quartiles: Q1=~.4f, Q2=~.4f, Q3=~.4f~n", [Q1, Q2, Q3]);
        _ -> ok
    end,
    
    % Time patterns
    TimePatterns = maps:get(time_patterns, Analysis, #{}),
    io:format("~nTime Patterns:~n"),
    io:format("  Total Duration: ~.2f seconds~n", [maps:get(total_duration, TimePatterns, 0)]),
    io:format("  Average Interval: ~.2f seconds~n", [maps:get(avg_interval, TimePatterns, 0)]),
    io:format("  Min Interval: ~.2f seconds~n", [maps:get(min_interval, TimePatterns, 0)]),
    io:format("  Max Interval: ~.2f seconds~n", [maps:get(max_interval, TimePatterns, 0)]),
    
    % Decision quality
    Quality = maps:get(decision_quality, Analysis, #{}),
    io:format("~nDecision Quality:~n"),
    io:format("  Success Rate: ~.2f%~n", [maps:get(success_rate, Quality, 0)]),
    io:format("  Consistency Score: ~.2f%~n", [maps:get(decision_consistency, Quality, 0)]),
    io:format("  Overall Quality: ~p~n", [maps:get(overall_quality, Quality, unknown)]),
    
    % Execution statistics
    ExecStats = maps:get(execution_stats, DetailedResults, #{}),
    case maps:size(ExecStats) of
        0 -> ok;
        _ ->
            io:format("~nExecution Statistics:~n"),
            case maps:get(fitness, ExecStats, undefined) of
                undefined -> ok;
                Fitness -> io:format("  Final Fitness: ~.4f~n", [Fitness])
            end,
            case maps:get(cycles, ExecStats, undefined) of
                undefined -> ok;
                Cycles -> io:format("  Cycles: ~p~n", [Cycles])
            end,
            case maps:get(goal_reached, ExecStats, undefined) of
                undefined -> ok;
                Goal -> io:format("  Goal Reached: ~p~n", [Goal])
            end
    end,
    
    io:format("=== END DETAILED ANALYSIS ===~n~n").

%% Convert decision record to map for JSON serialization
decision_to_map(Decision) ->
    #{
        timestamp => format_timestamp(Decision#trading_decision.timestamp),
        index => Decision#trading_decision.index,
        price => Decision#trading_decision.price,
        signal => case Decision#trading_decision.signal of
            1 -> "BUY";
            -1 -> "SELL";
            0 -> "HOLD";
            _ -> "UNKNOWN"
        end,
        confidence => Decision#trading_decision.confidence,
        profit_loss => Decision#trading_decision.profit_loss
    }.

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
    [Interval | calculate_time_intervals([T2 | Rest])].

calculate_consistency(Decisions) ->
    % Simple consistency measure based on signal patterns
    Signals = [D#trading_decision.signal || D <- Decisions],
    Changes = count_signal_changes(Signals),
    case length(Signals) of
        0 -> 0.0;
        N -> (N - Changes) / N * 100  % Percentage of consistent decisions
    end.

%% Formatting helper functions

print_formatted_summary(Summary) ->
    io:format("~n=== TRADING RESULTS SUMMARY ===~n"),
    io:format("Agent: ~p | Data: ~s~n", 
              [maps:get(agent_id, Summary), maps:get(data_source, Summary)]),
    io:format("Execution Time: ~.2f seconds~n", [maps:get(execution_time, Summary)]),
    io:format("~nDecision Breakdown:~n"),
    io:format("  Total: ~p | Buy: ~p | Sell: ~p | Hold: ~p~n",
              [maps:get(total_decisions, Summary),
               maps:get(buy_decisions, Summary),
               maps:get(sell_decisions, Summary),
               maps:get(hold_decisions, Summary)]),
    io:format("~nPerformance Metrics:~n"),
    io:format("  Total P&L: ~.4f%~n", [maps:get(total_profit_loss, Summary)]),
    io:format("  Success Rate: ~.2f%~n", [maps:get(success_rate, Summary)]),
    io:format("  Avg Profit/Trade: ~.4f%~n", [maps:get(avg_profit_per_trade, Summary)]),
    io:format("  Win/Loss Ratio: ~.2f~n", [maps:get(win_loss_ratio, Summary)]),
    io:format("  Max Profit: ~.4f% | Max Loss: ~.4f%~n",
              [maps:get(max_profit, Summary), maps:get(max_loss, Summary)]),
    io:format("=== END SUMMARY ===~n~n").

print_formatted_detailed(DetailedResults) ->
    print_formatted_summary(DetailedResults),
    
    io:format("=== DETAILED ANALYSIS ===~n"),
    
    % Print decision analysis
    Analysis = maps:get(decision_analysis, DetailedResults, #{}),
    print_decision_analysis(Analysis),
    
    % Print execution stats
    ExecStats = maps:get(execution_stats, DetailedResults, #{}),
    print_execution_stats(ExecStats),
    
    io:format("=== END DETAILED ANALYSIS ===~n~n").

print_decision_analysis(Analysis) ->
    io:format("~nDecision Pattern Analysis:~n"),
    
    Sequences = maps:get(signal_sequences, Analysis, #{}),
    io:format("  Signal Sequences: Buy=~p, Sell=~p, Hold=~p, Changes=~p~n",
              [maps:get(consecutive_buys, Sequences, 0),
               maps:get(consecutive_sells, Sequences, 0),
               maps:get(consecutive_holds, Sequences, 0),
               maps:get(signal_changes, Sequences, 0)]),
    
    ProfitDist = maps:get(profit_distribution, Analysis, #{}),
    io:format("  Profit Distribution: Mean=~.4f, Median=~.4f, StdDev=~.4f~n",
              [maps:get(mean, ProfitDist, 0),
               maps:get(median, ProfitDist, 0),
               maps:get(std_dev, ProfitDist, 0)]),
    
    Quality = maps:get(decision_quality, Analysis, #{}),
    io:format("  Decision Quality: ~p (~.2f% success rate)~n",
              [maps:get(overall_quality, Quality, unknown),
               maps:get(success_rate, Quality, 0)]).

print_execution_stats(ExecStats) ->
    case maps:size(ExecStats) of
        0 -> 
            io:format("~nNo execution statistics available~n");
        _ ->
            io:format("~nExecution Statistics:~n"),
            case maps:get(fitness, ExecStats, undefined) of
                undefined -> ok;
                Fitness -> io:format("  Final Fitness: ~p~n", [Fitness])
            end,
            case maps:get(cycles, ExecStats, undefined) of
                undefined -> ok;
                Cycles -> io:format("  Cycles: ~p~n", [Cycles])
            end,
            case maps:get(goal_reached, ExecStats, undefined) of
                undefined -> ok;
                Goal -> io:format("  Goal Reached: ~p~n", [Goal])
            end
    end.

format_summary_string(Summary) ->
    lists:flatten(io_lib:format(
        "Agent: ~p | Data: ~s | Time: ~.2fs~n"
        "Decisions: ~p total (~p buy, ~p sell, ~p hold)~n"
        "P&L: ~.4f% | Success: ~.2f% | Avg/Trade: ~.4f%~n"
        "Win/Loss: ~.2f | Max: +~.4f%/~.4f%~n",
        [maps:get(agent_id, Summary),
         maps:get(data_source, Summary),
         maps:get(execution_time, Summary),
         maps:get(total_decisions, Summary),
         maps:get(buy_decisions, Summary),
         maps:get(sell_decisions, Summary),
         maps:get(hold_decisions, Summary),
         maps:get(total_profit_loss, Summary),
         maps:get(success_rate, Summary),
         maps:get(avg_profit_per_trade, Summary),
         maps:get(win_loss_ratio, Summary),
         maps:get(max_profit, Summary),
         maps:get(max_loss, Summary)])).

format_detailed_string(DetailedResults) ->
    Summary = format_summary_string(DetailedResults),
    Analysis = maps:get(decision_analysis, DetailedResults, #{}),
    
    AnalysisStr = case maps:size(Analysis) of
        0 -> "";
        _ ->
            Quality = maps:get(decision_quality, Analysis, #{}),
            io_lib:format("Decision Quality: ~p | Consistency: ~.2f%~n",
                         [maps:get(overall_quality, Quality, unknown),
                          maps:get(decision_consistency, Quality, 0)])
    end,
    
    lists:flatten([Summary, AnalysisStr]).

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(Timestamp),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                 [Year, Month, Day, Hour, Min, Sec]).

decision_to_map(Decision) ->
    #{
        timestamp => format_timestamp(Decision#trading_decision.timestamp),
        index => Decision#trading_decision.index,
        price => Decision#trading_decision.price,
        signal => Decision#trading_decision.signal,
        confidence => Decision#trading_decision.confidence,
        profit_loss => Decision#trading_decision.profit_loss
    }.

format_simple_json(Map) ->
    % Very simple JSON formatting - would use proper JSON library in production
    "{" ++ format_json_pairs(maps:to_list(Map)) ++ "}".

format_json_pairs([]) -> "";
format_json_pairs([{Key, Value}]) ->
    format_json_pair(Key, Value);
format_json_pairs([{Key, Value} | Rest]) ->
    format_json_pair(Key, Value) ++ "," ++ format_json_pairs(Rest).

format_json_pair(Key, Value) when is_atom(Key) ->
    format_json_pair(atom_to_list(Key), Value);
format_json_pair(Key, Value) when is_list(Key) ->
    "\"" ++ Key ++ "\":" ++ format_json_value(Value).

format_json_value(Value) when is_number(Value) ->
    io_lib:format("~p", [Value]);
format_json_value(Value) when is_atom(Value) ->
    "\"" ++ atom_to_list(Value) ++ "\"";
format_json_value(Value) when is_list(Value) ->
    "\"" ++ Value ++ "\"";
format_json_value(Value) when is_map(Value) ->
    format_simple_json(Value);
format_json_value(_) ->
    "null".

%% Module initialization
init() ->
    io:format("Best Agent Runner module initialized~n"),
    ok.
%%
 ============================================================================
%% TASK 8: COMPREHENSIVE ERROR HANDLING FUNCTIONS
%% ============================================================================

%% Enhanced wait for completion with timeout and better error handling
wait_for_completion_with_timeout(Agent_PId, Monitor_PId, Collector_PId, StartTime, Options) ->
    % Calculate total timeout based on options
    TotalTimeout = case Options#run_options.verbose of
        true -> 120000;  % 2 minutes for verbose mode
        false -> 60000   % 1 minute for normal mode
    end,
    
    wait_for_completion_with_timeout(Agent_PId, Monitor_PId, Collector_PId, StartTime, Options, TotalTimeout).

wait_for_completion_with_timeout(Agent_PId, Monitor_PId, Collector_PId, StartTime, Options, Timeout) ->
    receive
        {Monitor_PId, execution_completed, ExecutionStats} ->
            io:format("Agent execution completed, collecting final results~n"),
            
            % Get final decisions from collector with timeout
            Collector_PId ! {self(), terminate},
            receive
                {Collector_PId, final_decisions, Decisions} ->
                    EndTime = erlang:timestamp(),
                    
                    % Create comprehensive results
                    Results = #agent_run_results{
                        start_time = StartTime,
                        end_time = EndTime,
                        trading_decisions = Decisions,
                        execution_stats = ExecutionStats
                    },
                    
                    {ok, Results};
                {Collector_PId, final_decisions_with_stats, {Decisions, _Stats}} ->
                    EndTime = erlang:timestamp(),
                    
                    % Create comprehensive results
                    Results = #agent_run_results{
                        start_time = StartTime,
                        end_time = EndTime,
                        trading_decisions = Decisions,
                        execution_stats = ExecutionStats
                    },
                    
                    {ok, Results}
            after 5000 ->
                io:format("Warning: Collector did not respond, using empty decisions~n"),
                EndTime = erlang:timestamp(),
                
                Results = #agent_run_results{
                    start_time = StartTime,
                    end_time = EndTime,
                    trading_decisions = [],
                    execution_stats = ExecutionStats
                },
                
                {ok, Results}
            end;
            
        {Monitor_PId, monitor_terminated} ->
            {error, monitor_terminated_unexpectedly};
            
        {error, Reason} ->
            {error, Reason};
            
        Other ->
            io:format("Unexpected message during completion wait: ~p~n", [Other]),
            wait_for_completion_with_timeout(Agent_PId, Monitor_PId, Collector_PId, StartTime, Options, Timeout)
            
    after Timeout ->
        io:format("Agent execution timed out after ~p ms~n", [Timeout]),
        
        % Force termination of all processes
        terminate_processes_forcefully([Agent_PId, Monitor_PId, Collector_PId]),
        
        {error, {execution_timeout, Timeout}}
    end.

%% Forcefully terminate processes
terminate_processes_forcefully(ProcessList) ->
    lists:foreach(fun(PId) ->
        case is_process_alive(PId) of
            true ->
                io:format("Force terminating process: ~p~n", [PId]),
                exit(PId, kill);
            false ->
                ok
        end
    end, ProcessList).

%% Safe cleanup of agent processes with enhanced error handling
cleanup_agent_processes_safe(Agent_PId, Monitor_PId, Collector_PId) ->
    io:format("Starting safe cleanup of agent processes~n"),
    
    % Cleanup monitor process
    cleanup_process_safe(Monitor_PId, "Monitor", fun(PId) ->
        PId ! {self(), terminate},
        receive
            {PId, monitor_terminated} -> ok
        after 3000 -> 
            io:format("Monitor did not respond to termination, forcing kill~n"),
            exit(PId, kill)
        end
    end),
    
    % Cleanup collector process
    cleanup_process_safe(Collector_PId, "Collector", fun(PId) ->
        PId ! {self(), terminate},
        timer:sleep(1000)  % Give collector time to finish
    end),
    
    % Cleanup agent process
    cleanup_process_safe(Agent_PId, "Agent", fun(PId) ->
        PId ! {self(), terminate},
        timer:sleep(2000)  % Give agent time to clean up properly
    end),
    
    io:format("Agent process cleanup completed~n").

%% Safe cleanup of individual process
cleanup_process_safe(undefined, _ProcessName, _CleanupFun) ->
    ok;
cleanup_process_safe(PId, ProcessName, CleanupFun) ->
    case is_process_alive(PId) of
        true ->
            try
                io:format("Cleaning up ~s process: ~p~n", [ProcessName, PId]),
                CleanupFun(PId),
                
                % Verify process terminated
                case is_process_alive(PId) of
                    true ->
                        io:format("~s process still alive, force killing~n", [ProcessName]),
                        exit(PId, kill);
                    false ->
                        io:format("~s process terminated successfully~n", [ProcessName])
                end
            catch
                Error:Reason ->
                    io:format("Error cleaning up ~s process: ~p:~p~n", [ProcessName, Error, Reason]),
                    % Force kill as last resort
                    case is_process_alive(PId) of
                        true -> exit(PId, kill);
                        false -> ok
                    end
            end;
        false ->
            io:format("~s process already terminated~n", [ProcessName])
    end.

%% Enhanced cleanup temporary data with better error handling
cleanup_temporary_data_safe(TableName) ->
    try
        case ets:info(TableName) of
            undefined ->
                io:format("Table ~p does not exist, no cleanup needed~n", [TableName]);
            _TableInfo ->
                case fx:delete_temporary_table(TableName) of
                    ok ->
                        io:format("Temporary table ~p cleaned up successfully~n", [TableName]);
                    {error, Reason} ->
                        io:format("Warning: Could not cleanup table ~p: ~p~n", [TableName, Reason]),
                        % Try direct ETS deletion as fallback
                        try
                            ets:delete(TableName),
                            io:format("Table ~p deleted directly via ETS~n", [TableName])
                        catch
                            EtsError:EtsReason ->
                                io:format("Failed to delete table ~p directly: ~p:~p~n", 
                                         [TableName, EtsError, EtsReason])
                        end
                end
        end
    catch
        Error:CleanupReason ->
            io:format("Exception during table cleanup: ~p:~p~n", [Error, CleanupReason]),
            % Try emergency cleanup
            try
                ets:delete(TableName),
                io:format("Emergency cleanup of table ~p successful~n", [TableName])
            catch
                _:_ ->
                    io:format("Emergency cleanup of table ~p failed~n", [TableName])
            end
    end.

%% Enhanced file error handling
handle_file_error({error, enoent}, FilePath) ->
    {error, {file_not_found, FilePath}};
handle_file_error({error, eacces}, FilePath) ->
    {error, {file_access_denied, FilePath}};
handle_file_error({error, eisdir}, FilePath) ->
    {error, {path_is_directory, FilePath}};
handle_file_error({error, emfile}, _FilePath) ->
    {error, too_many_open_files};
handle_file_error({error, enfile}, _FilePath) ->
    {error, system_file_table_full};
handle_file_error({error, enospc}, _FilePath) ->
    {error, no_space_left_on_device};
handle_file_error({error, Reason}, FilePath) ->
    {error, {file_error, Reason, FilePath}}.

%% Enhanced agent error handling
handle_agent_error({error, {find_agent_exception, Error, Reason}}) ->
    {error, {agent_lookup_failed, Error, Reason}};
handle_agent_error({error, {agent_startup_failed, Reason}}) ->
    {error, {cannot_start_agent, Reason}};
handle_agent_error({error, {execution_timeout, Timeout}}) ->
    {error, {agent_execution_timeout, Timeout}};
handle_agent_error({error, {monitoring_setup_failed, Reason}}) ->
    {error, {agent_monitoring_failed, Reason}};
handle_agent_error({error, Reason}) ->
    {error, {agent_error, Reason}}.

%% Enhanced data loading error handling
handle_data_loading_error({error, {file_not_found, FilePath}}) ->
    {error, {data_file_not_found, FilePath}};
handle_data_loading_error({error, {invalid_format, Details}}) ->
    {error, {data_format_invalid, Details}};
handle_data_loading_error({error, {parse_error, LineNumber, Details}}) ->
    {error, {data_parse_error, LineNumber, Details}};
handle_data_loading_error({error, {table_verification_failed, Reason}}) ->
    {error, {data_table_creation_failed, Reason}};
handle_data_loading_error({error, Reason}) ->
    {error, {data_loading_error, Reason}}.

%% Comprehensive error recovery
attempt_error_recovery(ErrorType, Context, RetryCount) when RetryCount > 0 ->
    io:format("Attempting error recovery for ~p (attempt ~p)~n", [ErrorType, 4 - RetryCount]),
    
    case ErrorType of
        agent_startup_failed ->
            timer:sleep(1000),  % Wait before retry
            {retry, RetryCount - 1};
        data_loading_failed ->
            timer:sleep(500),   % Brief wait for file system
            {retry, RetryCount - 1};
        execution_timeout ->
            % No retry for timeouts
            {no_retry, timeout_not_recoverable};
        _ ->
            {retry, RetryCount - 1}
    end;
attempt_error_recovery(ErrorType, _Context, 0) ->
    io:format("Error recovery failed for ~p after maximum retries~n", [ErrorType]),
    {no_retry, max_retries_exceeded}.

%% Validate system resources before execution
validate_system_resources() ->
    try
        % Check memory usage
        MemoryInfo = erlang:memory(),
        TotalMemory = proplists:get_value(total, MemoryInfo, 0),
        ProcessMemory = proplists:get_value(processes, MemoryInfo, 0),
        
        % Check if we have enough memory (basic check)
        case TotalMemory > 50000000 of  % 50MB minimum
            true ->
                % Check process count
                ProcessCount = erlang:system_info(process_count),
                ProcessLimit = erlang:system_info(process_limit),
                
                case ProcessCount < (ProcessLimit * 0.8) of
                    true ->
                        % Check ETS table count
                        EtsCount = length(ets:all()),
                        EtsLimit = erlang:system_info(ets_limit),
                        
                        case EtsCount < (EtsLimit * 0.8) of
                            true ->
                                ok;
                            false ->
                                {error, {ets_table_limit_approaching, EtsCount, EtsLimit}}
                        end;
                    false ->
                        {error, {process_limit_approaching, ProcessCount, ProcessLimit}}
                end;
            false ->
                {error, {insufficient_memory, TotalMemory}}
        end
    catch
        Error:Reason ->
            {error, {resource_validation_failed, Error, Reason}}
    end.

%% Monitor system resources during execution
monitor_system_resources(MonitorPid) ->
    spawn(fun() -> resource_monitor_loop(MonitorPid, 5000) end).

resource_monitor_loop(MonitorPid, Interval) ->
    case validate_system_resources() of
        ok ->
            timer:sleep(Interval),
            resource_monitor_loop(MonitorPid, Interval);
        {error, Reason} ->
            MonitorPid ! {resource_warning, Reason},
            timer:sleep(Interval),
            resource_monitor_loop(MonitorPid, Interval)
    end.