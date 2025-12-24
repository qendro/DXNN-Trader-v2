-module(substrate_profiler).
-compile(export_all).
-include("records.hrl").

%% ============================================================================
%% SUBSTRATE PROFILER - Capture detailed timing data for weight calculation
%% ============================================================================
%%
%% Usage:
%%   1. Enable profiling: substrate_profiler:start()
%%   2. Run your experiment: exp_runner:start(new_evo)
%%   3. Wait a few seconds (5-10 seconds)
%%   4. Stop profiling: substrate_profiler:stop()
%%   5. View results: substrate_profiler:report()
%%
%% This will show you exactly where time is being spent in weight calculation.

-record(profiler_state, {
    enabled = false,
    start_time,
    samples = [],
    weight_calc_count = 0,
    fanin_block_count = 0,
    total_fanin_block_time = 0,
    connection_count = 0,
    cpp_send_count = 0,
    cep_receive_count = 0
}).

-define(PROFILER_TABLE, substrate_profiler_data).

%% Start profiling
start() ->
    case whereis(substrate_profiler) of
        undefined ->
            Pid = spawn(fun() -> profiler_loop(#profiler_state{enabled = true, start_time = erlang:timestamp()}) end),
            register(substrate_profiler, Pid),
            ets:new(?PROFILER_TABLE, [set, named_table, public]),
            io:format("Substrate profiler started. Run your experiment now.~n"),
            ok;
        _Pid ->
            io:format("Profiler already running.~n"),
            ok
    end.

%% Stop profiling
stop() ->
    case whereis(substrate_profiler) of
        undefined ->
            io:format("Profiler not running.~n"),
            ok;
        Pid ->
            Pid ! stop,
            timer:sleep(100),
            io:format("Profiler stopped.~n"),
            ok
    end.

%% Profiler loop
profiler_loop(State) ->
    receive
        {log_weight_calc, Duration, ConnectionCount} ->
            NewSamples = [{weight_calc, erlang:timestamp(), Duration, ConnectionCount} | State#profiler_state.samples],
            NewState = State#profiler_state{
                samples = NewSamples,
                weight_calc_count = State#profiler_state.weight_calc_count + 1,
                connection_count = State#profiler_state.connection_count + ConnectionCount
            },
            profiler_loop(NewState);
        {log_fanin_block, Duration} ->
            NewSamples = [{fanin_block, erlang:timestamp(), Duration} | State#profiler_state.samples],
            NewState = State#profiler_state{
                samples = NewSamples,
                fanin_block_count = State#profiler_state.fanin_block_count + 1,
                total_fanin_block_time = State#profiler_state.total_fanin_block_time + Duration
            },
            profiler_loop(NewState);
        {log_cpp_send, Count} ->
            NewState = State#profiler_state{
                cpp_send_count = State#profiler_state.cpp_send_count + Count
            },
            profiler_loop(NewState);
        {log_cep_receive, Count} ->
            NewState = State#profiler_state{
                cep_receive_count = State#profiler_state.cep_receive_count + Count
            },
            profiler_loop(NewState);
        {get_state, From} ->
            From ! {profiler_state, State},
            profiler_loop(State);
        stop ->
            io:format("Profiler received stop signal.~n"),
            ok;
        _ ->
            profiler_loop(State)
    end.

%% Check if profiling is enabled
is_enabled() ->
    case whereis(substrate_profiler) of
        undefined -> false;
        _Pid -> true
    end.

%% Log weight calculation timing
log_weight_calc(Duration, ConnectionCount) ->
    case is_enabled() of
        true ->
            substrate_profiler ! {log_weight_calc, Duration, ConnectionCount};
        false ->
            ok
    end.

%% Log fanin blocking time
log_fanin_block(Duration) ->
    case is_enabled() of
        true ->
            substrate_profiler ! {log_fanin_block, Duration};
        false ->
            ok
    end.

%% Log CPP send count
log_cpp_send(Count) ->
    case is_enabled() of
        true ->
            substrate_profiler ! {log_cpp_send, Count};
        false ->
            ok
    end.

%% Log CEP receive count
log_cep_receive(Count) ->
    case is_enabled() of
        true ->
            substrate_profiler ! {log_cep_receive, Count};
        false ->
            ok
    end.

%% Generate report
report() ->
    case whereis(substrate_profiler) of
        undefined ->
            io:format("Profiler not running. Start it first with substrate_profiler:start()~n"),
            ok;
        Pid ->
            Pid ! {get_state, self()},
            receive
                {profiler_state, State} ->
                    print_report(State)
            after 1000 ->
                io:format("Timeout waiting for profiler state.~n")
            end
    end.

%% Print detailed report
print_report(State) ->
    io:format("~n========================================~n"),
    io:format("SUBSTRATE PROFILER REPORT~n"),
    io:format("========================================~n~n"),
    
    % Overall statistics
    io:format("=== OVERALL STATISTICS ===~n"),
    io:format("Profiling Duration: ~p seconds~n", [get_duration_seconds(State#profiler_state.start_time)]),
    io:format("Weight Calculations: ~p~n", [State#profiler_state.weight_calc_count]),
    io:format("Total Connections Processed: ~p~n", [State#profiler_state.connection_count]),
    io:format("Fanin Block Operations: ~p~n", [State#profiler_state.fanin_block_count]),
    io:format("Total Fanin Block Time: ~p microseconds (~.2f seconds)~n", 
              [State#profiler_state.total_fanin_block_time, 
               State#profiler_state.total_fanin_block_time / 1000000]),
    io:format("CPP Send Operations: ~p~n", [State#profiler_state.cpp_send_count]),
    io:format("CEP Receive Operations: ~p~n", [State#profiler_state.cep_receive_count]),
    io:format("~n"),
    
    % Analyze samples
    Samples = State#profiler_state.samples,
    analyze_weight_calcs(Samples),
    analyze_fanin_blocks(Samples),
    
    % Time distribution
    io:format("~n=== TIME DISTRIBUTION ===~n"),
    TimeDistribution = calculate_time_distribution(Samples),
    print_time_distribution(TimeDistribution),
    
    io:format("~n========================================~n").

%% Analyze weight calculation samples
analyze_weight_calcs(Samples) ->
    WeightCalcs = [S || S = {weight_calc, _, _, _} <- Samples],
    case WeightCalcs of
        [] ->
            io:format("~n=== WEIGHT CALCULATION ANALYSIS ===~n"),
            io:format("No weight calculation samples captured.~n");
        _ ->
            Durations = [Duration || {weight_calc, _, Duration, _} <- WeightCalcs],
            ConnectionCounts = [Count || {weight_calc, _, _, Count} <- WeightCalcs],
            
            io:format("~n=== WEIGHT CALCULATION ANALYSIS ===~n"),
            io:format("Total Samples: ~p~n", [length(WeightCalcs)]),
            io:format("Total Time: ~p microseconds (~.2f seconds)~n", 
                      [lists:sum(Durations), lists:sum(Durations) / 1000000]),
            io:format("Average Time per Calculation: ~p microseconds (~.3f ms)~n", 
                      [avg(Durations), avg(Durations) / 1000]),
            io:format("Min Time: ~p microseconds~n", [lists:min(Durations)]),
            io:format("Max Time: ~p microseconds (~.2f ms)~n", 
                      [lists:max(Durations), lists:max(Durations) / 1000]),
            io:format("Average Connections per Calculation: ~.1f~n", [avg(ConnectionCounts)]),
            io:format("Time per Connection: ~p microseconds (~.3f ms)~n", 
                      [avg(Durations) / max(avg(ConnectionCounts), 1), 
                       (avg(Durations) / max(avg(ConnectionCounts), 1)) / 1000])
    end.

%% Analyze fanin block samples
analyze_fanin_blocks(Samples) ->
    FaninBlocks = [S || S = {fanin_block, _, _} <- Samples],
    case FaninBlocks of
        [] ->
            io:format("~n=== FANIN BLOCKING ANALYSIS ===~n"),
            io:format("No fanin blocking samples captured.~n");
        _ ->
            Durations = [Duration || {fanin_block, _, Duration} <- FaninBlocks],
            
            io:format("~n=== FANIN BLOCKING ANALYSIS ===~n"),
            io:format("Total Block Operations: ~p~n", [length(FaninBlocks)]),
            io:format("Total Block Time: ~p microseconds (~.2f seconds)~n", 
                      [lists:sum(Durations), lists:sum(Durations) / 1000000]),
            io:format("Average Block Time: ~p microseconds (~.3f ms)~n", 
                      [avg(Durations), avg(Durations) / 1000]),
            io:format("Min Block Time: ~p microseconds~n", [lists:min(Durations)]),
            io:format("Max Block Time: ~p microseconds (~.2f ms)~n", 
                      [lists:max(Durations), lists:max(Durations) / 1000]),
            
            % Show worst offenders
            Sorted = lists:reverse(lists:sort([{D, T} || {fanin_block, T, D} <- FaninBlocks])),
            Worst = lists:sublist(Sorted, min(10, length(Sorted))),
            io:format("~nTop 10 Longest Blocks:~n"),
            lists:foreach(fun({Duration, Timestamp}) ->
                io:format("  ~p microseconds at ~p~n", [Duration, format_timestamp(Timestamp)])
            end, Worst)
    end.

%% Calculate time distribution
calculate_time_distribution(Samples) ->
    Now = erlang:timestamp(),
    % Group samples into 1-second buckets
    Buckets = lists:foldl(fun(Sample, Acc) ->
        {Type, Timestamp, Duration, _} = case Sample of
            {weight_calc, T, D, C} -> {weight_calc, T, D, C};
            {fanin_block, T, D} -> {fanin_block, T, D, 0}
        end,
        Bucket = get_bucket(Timestamp),
        case lists:keyfind(Bucket, 1, Acc) of
            false ->
                [{Bucket, [{Type, Duration}]} | Acc];
            {Bucket, Durations} ->
                lists:keyreplace(Bucket, 1, Acc, {Bucket, [{Type, Duration} | Durations]})
        end
    end, [], Samples),
    Buckets.

%% Get time bucket (1 second intervals)
get_bucket(Timestamp) ->
    {Mega, Sec, _} = Timestamp,
    (Mega * 1000000 + Sec) div 1.

%% Print time distribution
print_time_distribution(Buckets) ->
    Sorted = lists:keysort(1, Buckets),
    io:format("Time (seconds) | Weight Calcs | Fanin Blocks | Total Time (ms)~n"),
    io:format("--------------------------------------------------------------~n"),
    lists:foreach(fun({Bucket, Durations}) ->
        WeightCalcs = [D || {weight_calc, D} <- Durations],
        FaninBlocks = [D || {fanin_block, D} <- Durations],
        TotalTime = lists:sum(WeightCalcs) + lists:sum(FaninBlocks),
        io:format("~12p | ~13p | ~13p | ~15.2f~n", 
                  [Bucket, length(WeightCalcs), length(FaninBlocks), TotalTime / 1000])
    end, Sorted).

%% Helper functions
get_duration_seconds(StartTime) ->
    {Mega1, Sec1, Micro1} = StartTime,
    {Mega2, Sec2, Micro2} = erlang:timestamp(),
    (Mega2 * 1000000 + Sec2 + Micro2 / 1000000) - (Mega1 * 1000000 + Sec1 + Micro1 / 1000000).

avg(List) ->
    case List of
        [] -> 0;
        _ -> lists:sum(List) / length(List)
    end.

format_timestamp({Mega, Sec, Micro}) ->
    TotalSec = Mega * 1000000 + Sec,
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) + TotalSec),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~6..0B", 
                  [Y,Mo,D,H,Mi,S,Micro]).

%% Export samples to file
export_samples(Filename) ->
    case whereis(substrate_profiler) of
        undefined ->
            io:format("Profiler not running.~n"),
            ok;
        Pid ->
            Pid ! {get_state, self()},
            receive
                {profiler_state, State} ->
                    {ok, File} = file:open(Filename, [write]),
                    io:format(File, "~p.~n", [State#profiler_state.samples]),
                    file:close(File),
                    io:format("Samples exported to ~p~n", [Filename])
            after 1000 ->
                io:format("Timeout waiting for profiler state.~n")
            end
    end.



