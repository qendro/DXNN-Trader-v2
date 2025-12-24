-module(system_profiler).
-compile(export_all).

%% ============================================================================
%% SYSTEM PROFILER - Non-invasive profiling for all agents (neural + substrate)
%% ============================================================================
%%
%% This profiler uses Erlang tracing to monitor system activity WITHOUT
%% modifying any existing code. It tracks:
%% - Function call times
%% - Message passing patterns
%% - Process activity (reductions, message queues)
%% - CPU usage per process
%% - Blocking operations
%%
%% Usage:
%%   1. Start profiler: system_profiler:start(5).  % Profile for 5 seconds
%%   2. Run your experiment: exp_runner:start(new_evo)
%%   3. Wait for profiling to complete
%%   4. View results: system_profiler:report()
%%
%% Or use continuous mode:
%%   1. system_profiler:start_continuous()
%%   2. Run experiment
%%   3. system_profiler:stop()
%%   4. system_profiler:report()

-define(TRACE_TABLE, system_profiler_trace).
-define(SNAPSHOT_TABLE, system_profiler_snapshots).

%% Start profiling for N seconds
start(DurationSeconds) ->
    io:format("Starting system profiler for ~p seconds...~n", [DurationSeconds]),
    init_tables(),
    StartTime = erlang:timestamp(),
    
    % Start tracing all processes (including existing ones)
    start_tracing(),
    
    % Start snapshot collector
    SnapshotPid = spawn(fun() -> snapshot_loop(StartTime) end),
    register(profiler_snapshot, SnapshotPid),
    
    % Start message tracer FIRST (needed for tracing setup)
    MsgPid = spawn(fun() -> message_tracer_loop() end),
    register(profiler_messages, MsgPid),
    
    % Small delay to ensure tracer is ready
    timer:sleep(10),
    
    % Store start time
    ets:insert(?TRACE_TABLE, {start_time, StartTime}),
    ets:insert(?TRACE_TABLE, {duration, DurationSeconds}),
    
    % Auto-stop after duration
    spawn(fun() ->
        timer:sleep(DurationSeconds * 1000),
        stop(),
        io:format("Profiling completed after ~p seconds.~n", [DurationSeconds])
    end),
    
    io:format("Profiler started. Monitoring all processes (including existing ones)...~n"),
    {ok, StartTime}.

%% Attach profiler to already-running system
%% Use this if your experiment is already running
attach(DurationSeconds) ->
    io:format("Attaching profiler to running system for ~p seconds...~n", [DurationSeconds]),
    init_tables(),
    StartTime = erlang:timestamp(),
    
    % Start message tracer FIRST
    MsgPid = spawn(fun() -> message_tracer_loop() end),
    register(profiler_messages, MsgPid),
    timer:sleep(10),
    
    % Trace all existing processes
    AllProcesses = erlang:processes(),
    io:format("Attaching to ~p existing processes...~n", [length(AllProcesses)]),
    attach_to_processes(AllProcesses),
    
    % Trace new processes too
    erlang:trace(new, true, [
        send,
        'receive',
        timestamp,
        {tracer, MsgPid}
    ]),
    
    % Start snapshot collector
    SnapshotPid = spawn(fun() -> snapshot_loop(StartTime) end),
    register(profiler_snapshot, SnapshotPid),
    
    % Store start time
    ets:insert(?TRACE_TABLE, {start_time, StartTime}),
    ets:insert(?TRACE_TABLE, {duration, DurationSeconds}),
    
    % Auto-stop after duration
    spawn(fun() ->
        timer:sleep(DurationSeconds * 1000),
        stop(),
        io:format("Profiling completed after ~p seconds.~n", [DurationSeconds])
    end),
    
    io:format("Profiler attached and monitoring.~n"),
    {ok, StartTime}.

%% Attach tracing to specific processes
attach_to_processes([Pid | Pids]) ->
    TracerPid = whereis(profiler_messages),
    case is_process_alive(Pid) of
        true ->
            erlang:trace(Pid, true, [
                send,
                'receive',
                timestamp,
                {tracer, TracerPid}
            ]);
        false ->
            ok
    end,
    attach_to_processes(Pids);
attach_to_processes([]) ->
    ok.

%% Start continuous profiling (must call stop() manually)
start_continuous() ->
    io:format("Starting continuous system profiler...~n"),
    init_tables(),
    StartTime = erlang:timestamp(),
    
    start_tracing(),
    
    SnapshotPid = spawn(fun() -> snapshot_loop(StartTime) end),
    register(profiler_snapshot, SnapshotPid),
    
    MsgPid = spawn(fun() -> message_tracer_loop() end),
    register(profiler_messages, MsgPid),
    
    % Small delay to ensure tracer is ready
    timer:sleep(10),
    
    ets:insert(?TRACE_TABLE, {start_time, StartTime}),
    ets:insert(?TRACE_TABLE, {duration, continuous}),
    
    io:format("Continuous profiler started. Call system_profiler:stop() when done.~n"),
    {ok, StartTime}.

%% Attach continuous profiler to running system
attach_continuous() ->
    io:format("Attaching continuous profiler to running system...~n"),
    init_tables(),
    StartTime = erlang:timestamp(),
    
    % Start message tracer FIRST
    MsgPid = spawn(fun() -> message_tracer_loop() end),
    register(profiler_messages, MsgPid),
    timer:sleep(10),
    
    % Trace all existing processes
    AllProcesses = erlang:processes(),
    io:format("Attaching to ~p existing processes...~n", [length(AllProcesses)]),
    attach_to_processes(AllProcesses),
    
    % Trace new processes too
    erlang:trace(new, true, [
        send,
        'receive',
        timestamp,
        {tracer, MsgPid}
    ]),
    
    % Start snapshot collector
    SnapshotPid = spawn(fun() -> snapshot_loop(StartTime) end),
    register(profiler_snapshot, SnapshotPid),
    
    ets:insert(?TRACE_TABLE, {start_time, StartTime}),
    ets:insert(?TRACE_TABLE, {duration, continuous}),
    
    io:format("Continuous profiler attached. Call system_profiler:stop() when done.~n"),
    {ok, StartTime}.

%% Stop profiling
stop() ->
    io:format("Stopping profiler...~n"),
    EndTime = erlang:timestamp(),
    ets:insert(?TRACE_TABLE, {end_time, EndTime}),
    
    % Stop tracing
    stop_tracing(),
    
    % Stop snapshot collector
    case whereis(profiler_snapshot) of
        undefined -> ok;
        SnapshotPid -> SnapshotPid ! stop
    end,
    
    % Stop message tracer
    case whereis(profiler_messages) of
        undefined -> ok;
        MsgPid -> MsgPid ! stop
    end,
    
    io:format("Profiler stopped.~n"),
    ok.

%% Initialize ETS tables
init_tables() ->
    case ets:whereis(?TRACE_TABLE) of
        undefined ->
            ets:new(?TRACE_TABLE, [set, named_table, public]);
        _ ->
            ets:delete_all_objects(?TRACE_TABLE)
    end,
    case ets:whereis(?SNAPSHOT_TABLE) of
        undefined ->
            ets:new(?SNAPSHOT_TABLE, [ordered_set, named_table, public]);
        _ ->
            ets:delete_all_objects(?SNAPSHOT_TABLE)
    end.

%% Start tracing all processes
start_tracing() ->
    % Get tracer PID (the message tracer process)
    TracerPid = whereis(profiler_messages),
    
    % Trace all existing processes
    AllProcesses = erlang:processes(),
    io:format("Tracing ~p processes...~n", [length(AllProcesses)]),
    
    % Set trace flags: send, 'receive', timestamp
    % Note: We use a tracer process to receive trace messages
    lists:foreach(fun(Pid) ->
        erlang:trace(Pid, true, [
            send,           % Message sends
            'receive',      % Message receives
            timestamp,      % Timestamps
            {tracer, TracerPid}  % Send trace messages to our tracer
        ])
    end, AllProcesses),
    
    % Trace new processes
    erlang:trace(new, true, [
        send,
        'receive',
        timestamp,
        {tracer, TracerPid}
    ]),
    
    % Set trace pattern for key modules (with tracer)
    trace_key_modules(TracerPid),
    
    ok.

%% Trace key modules (substrate, neuron, sensor, actuator, cortex, exoself)
trace_key_modules(TracerPid) ->
    KeyModules = [
        substrate,
        neuron,
        sensor,
        actuator,
        cortex,
        exoself,
        substrate_cpp,
        substrate_cep
    ],
    lists:foreach(fun(Module) ->
        erlang:trace_pattern({Module, '_', '_'}, true, [
            call_count,     % Count calls
            call_time,      % Measure time
            {tracer, TracerPid}  % Send to tracer
        ])
    end, KeyModules).

%% Stop tracing
stop_tracing() ->
    erlang:trace(all, false, [all]),
    erlang:trace(new, false, [all]),
    ok.

%% Snapshot loop - periodically capture process state
snapshot_loop(StartTime) ->
    receive
        stop ->
            ok
    after 100 ->  % Every 100ms
        capture_snapshot(),
        snapshot_loop(StartTime)
    end.

%% Capture process snapshot
capture_snapshot() ->
    Timestamp = erlang:timestamp(),
    Processes = erlang:processes(),
    
    Snapshot = lists:foldl(fun(Pid, Acc) ->
        case catch process_info(Pid) of
            Info when is_list(Info) ->
                Reductions = proplists:get_value(reductions, Info, 0),
                MessageQLen = proplists:get_value(message_queue_len, Info, 0),
                CurrentFunction = proplists:get_value(current_function, Info, undefined),
                Status = proplists:get_value(status, Info, undefined),
                
                [{Pid, Timestamp, Reductions, MessageQLen, CurrentFunction, Status} | Acc];
            _ ->
                Acc
        end
    end, [], Processes),
    
    ets:insert(?SNAPSHOT_TABLE, {Timestamp, Snapshot}).

%% Message tracer loop - receives trace messages from Erlang runtime
message_tracer_loop() ->
    receive
        stop ->
            ok;
        {trace_ts, Pid, send, Msg, To, Timestamp} ->
            % Message send with timestamp
            ets:insert(?TRACE_TABLE, {
                {msg_send, erlang:phash2({Pid, To, Timestamp})},
                {send, Pid, To, Msg, Timestamp}
            }),
            message_tracer_loop();
        {trace_ts, Pid, 'receive', Msg, Timestamp} ->
            % Message receive with timestamp
            ets:insert(?TRACE_TABLE, {
                {msg_receive, erlang:phash2({Pid, Timestamp})},
                {'receive', Pid, Msg, Timestamp}
            }),
            message_tracer_loop();
        {trace, Pid, send, Msg, To} ->
            % Message send without timestamp
            Timestamp = erlang:timestamp(),
            ets:insert(?TRACE_TABLE, {
                {msg_send, erlang:phash2({Pid, To, Timestamp})},
                {send, Pid, To, Msg, Timestamp}
            }),
            message_tracer_loop();
        {trace, Pid, 'receive', Msg} ->
            % Message receive without timestamp
            Timestamp = erlang:timestamp(),
            ets:insert(?TRACE_TABLE, {
                {msg_receive, erlang:phash2({Pid, Timestamp})},
                {'receive', Pid, Msg, Timestamp}
            }),
            message_tracer_loop();
        {trace_ts, Pid, call, {M, F, A}, Timestamp} ->
            % Function call with timestamp
            ets:insert(?TRACE_TABLE, {
                {func_call, erlang:phash2({Pid, Timestamp})},
                {call, Pid, M, F, A, Timestamp}
            }),
            message_tracer_loop();
        {trace_ts, Pid, return_from, {M, F, A}, Return, Timestamp} ->
            % Function return with timestamp
            ets:insert(?TRACE_TABLE, {
                {func_return, erlang:phash2({Pid, Timestamp})},
                {return, Pid, M, F, A, Return, Timestamp}
            }),
            message_tracer_loop();
        _Other ->
            % Ignore other trace messages
            message_tracer_loop()
    end.

%% Generate comprehensive report
report() ->
    io:format("~n========================================~n"),
    io:format("SYSTEM PROFILER REPORT~n"),
    io:format("========================================~n~n"),
    
    StartTime = case ets:lookup(?TRACE_TABLE, start_time) of
        [{start_time, ST}] -> ST;
        [] -> undefined
    end,
    
    EndTime = case ets:lookup(?TRACE_TABLE, end_time) of
        [{end_time, ET}] -> ET;
        [] -> erlang:timestamp()
    end,
    
    case StartTime of
        undefined ->
            io:format("No profiling data found. Start profiler first.~n"),
            ok;
        StartT when is_tuple(StartT), is_tuple(EndTime) ->
            Duration = timer:now_diff(EndTime, StartT) / 1000000,
            io:format("Profiling Duration: ~.2f seconds~n~n", [Duration]),
            
            % Analyze snapshots
            analyze_process_activity(),
            
            % Analyze message patterns
            analyze_messages(),
            
            % Analyze function calls
            analyze_function_calls(),
            
            % Find bottlenecks
            find_bottlenecks(),
            
            % Substrate-specific analysis
            analyze_substrate_activity(),
            
            % Neural network activity
            analyze_neural_activity(),
            
            io:format("~n========================================~n"),
            ok
    end.

%% Analyze process activity
analyze_process_activity() ->
    io:format("=== PROCESS ACTIVITY ANALYSIS ===~n"),
    
    AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
    case AllSnapshots of
        [] ->
            io:format("No snapshot data available.~n~n");
        _ ->
            % Group by process
            ProcessData = lists:foldl(fun({_Timestamp, Snapshot}, Acc) ->
                lists:foldl(fun({Pid, TS, Red, MQL, CF, Status}, PAcc) ->
                    case lists:keyfind(Pid, 1, PAcc) of
                        false ->
                            [{Pid, [{TS, Red, MQL, CF, Status}]} | PAcc];
                        {Pid, History} ->
                            lists:keyreplace(Pid, 1, PAcc, {Pid, [{TS, Red, MQL, CF, Status} | History]})
                    end
                end, Acc, Snapshot)
            end, [], AllSnapshots),
            
            % Calculate statistics per process
            ProcessStats = lists:map(fun({Pid, History}) ->
                Reductions = [R || {_, R, _, _, _} <- History],
                MessageQueues = [MQ || {_, _, MQ, _, _} <- History],
                Functions = [CF || {_, _, _, CF, _} <- History],
                
                {Pid,
                 length(History),
                 case Reductions of [] -> 0; _ -> lists:max(Reductions) - lists:min(Reductions) end,
                 case MessageQueues of [] -> 0; _ -> lists:max(MessageQueues) end,
                 most_common(Functions)}
            end, ProcessData),
            
            % Sort by reduction count (most active)
            Sorted = lists:reverse(lists:keysort(3, ProcessStats)),
            Top20 = lists:sublist(Sorted, 20),
            
            io:format("Top 20 Most Active Processes:~n"),
            io:format("~-20s ~-12s ~-20s ~-12s~n", ["PID", "Snapshots", "Reductions", "Max MQ"]),
            io:format(string:copies("-", 70) ++ "~n"),
            lists:foreach(fun({Pid, SnapCount, RedDiff, MaxMQ, CommonFunc}) ->
                % Format as strings to avoid ~p formatting issues
                PidStr = lists:flatten(io_lib:format("~p", [Pid])),
                SnapStr = integer_to_list(SnapCount),
                RedStr = integer_to_list(RedDiff),
                MQStr = integer_to_list(MaxMQ),
                io:format("~-20s ~-12s ~-20s ~-12s~n", [PidStr, SnapStr, RedStr, MQStr]),
                % Print function name on separate line to avoid formatting issues
                FuncDisplay = case CommonFunc of
                    undefined -> "  Function: unknown";
                    {M, F, A} -> lists:flatten(io_lib:format("  Function: ~p:~p/~p", [M, F, A]));
                    Other -> lists:flatten(io_lib:format("  Function: ~p", [Other]))
                end,
                io:format("~s~n", [FuncDisplay]),
                io:format("~n")
            end, Top20),
            io:format("~n")
    end.

%% Analyze message patterns
analyze_messages() ->
    io:format("=== MESSAGE PATTERN ANALYSIS ===~n"),
    
    AllTraces = ets:tab2list(?TRACE_TABLE),
    Sends = [Data || {Key, Data} <- AllTraces, element(1, Key) =:= msg_send],
    Receives = [Data || {Key, Data} <- AllTraces, element(1, Key) =:= msg_receive],
    
    io:format("Total Messages Sent: ~p~n", [length(Sends)]),
    io:format("Total Messages Received: ~p~n", [length(Receives)]),
    
    % Group by message type
    SendTypes = lists:foldl(fun({send, _From, _To, Msg, _TS}, Acc) ->
        Type = case Msg of
            {Tag, _, _} when is_atom(Tag) -> Tag;
            {Tag, _} when is_atom(Tag) -> Tag;
            Tag when is_atom(Tag) -> Tag;
            _ -> other
        end,
        case lists:keyfind(Type, 1, Acc) of
            false -> [{Type, 1} | Acc];
            {Type, Count} -> lists:keyreplace(Type, 1, Acc, {Type, Count + 1})
        end
    end, [], Sends),
    
    SortedTypes = lists:reverse(lists:keysort(2, SendTypes)),
    io:format("~nTop Message Types Sent:~n"),
    lists:foreach(fun({Type, Count}) ->
        io:format("  ~p: ~p messages~n", [Type, Count])
    end, lists:sublist(SortedTypes, 10)),
    io:format("~n").

%% Find bottlenecks
find_bottlenecks() ->
    io:format("=== BOTTLENECK ANALYSIS ===~n"),
    
    AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
    case AllSnapshots of
        [] ->
            io:format("No data for bottleneck analysis.~n~n");
        _ ->
            % Find processes with consistently high message queues
            HighMQ = lists:foldl(fun({_TS, Snapshot}, Acc) ->
                lists:foldl(fun({Pid, _, _, MQ, _, _}, PAcc) when MQ > 10 ->
                    case lists:keyfind(Pid, 1, PAcc) of
                        false -> [{Pid, 1, MQ} | PAcc];
                        {Pid, Count, MaxMQ} -> 
                            lists:keyreplace(Pid, 1, PAcc, {Pid, Count + 1, max(MaxMQ, MQ)})
                    end;
                (_, PAcc) -> PAcc
                end, Acc, Snapshot)
            end, [], AllSnapshots),
            
            case HighMQ of
                [] ->
                    io:format("No processes with consistently high message queues.~n");
                _ ->
                    Sorted = lists:reverse(lists:keysort(2, HighMQ)),
                    io:format("Processes with High Message Queues (>10 messages):~n"),
                    lists:foreach(fun({Pid, Count, MaxMQ}) ->
                        io:format("  ~p: ~p snapshots with high MQ, max: ~p~n", [Pid, Count, MaxMQ])
                    end, Sorted)
            end,
            io:format("~n")
    end.

%% Analyze substrate-specific activity
analyze_substrate_activity() ->
    io:format("=== SUBSTRATE ACTIVITY ANALYSIS ===~n"),
    
    AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
    SubstrateProcesses = lists:foldl(fun({_TS, Snapshot}, Acc) ->
        lists:foldl(fun({Pid, TS, Red, MQ, {M, F, _A}, _Status}, PAcc) 
                      when M =:= substrate orelse M =:= substrate_cpp orelse M =:= substrate_cep ->
            [{Pid, TS, Red, MQ, {M, F}} | PAcc];
        (_, PAcc) -> PAcc
        end, Acc, Snapshot)
    end, [], AllSnapshots),
    
    case SubstrateProcesses of
        [] ->
            io:format("No substrate process activity captured.~n");
        _ ->
            % Group by function
            FuncCounts = lists:foldl(fun({_Pid, _TS, _Red, _MQ, Func}, Acc) ->
                case lists:keyfind(Func, 1, Acc) of
                    false -> [{Func, 1} | Acc];
                    {Func, Count} -> lists:keyreplace(Func, 1, Acc, {Func, Count + 1})
                end
            end, [], SubstrateProcesses),
            
            Sorted = lists:reverse(lists:keysort(2, FuncCounts)),
            io:format("Substrate Function Activity:~n"),
            lists:foreach(fun({{M, F}, Count}) ->
                io:format("  ~p:~p: ~p occurrences~n", [M, F, Count])
            end, Sorted)
    end,
    io:format("~n").

%% Analyze function calls
analyze_function_calls() ->
    io:format("=== FUNCTION CALL ANALYSIS ===~n"),
    
    AllTraces = ets:tab2list(?TRACE_TABLE),
    Calls = [Data || {Key, Data} <- AllTraces, element(1, Key) =:= func_call],
    Returns = [Data || {Key, Data} <- AllTraces, element(1, Key) =:= func_return],
    
    % Match calls with returns to calculate durations
    CallReturns = match_calls_returns(Calls, Returns),
    
    case CallReturns of
        [] ->
            io:format("No function call timing data available.~n");
        _ ->
            % Group by function
            FuncTimes = lists:foldl(fun({M, F, A, Duration}, Acc) ->
                Key = {M, F, A},
                case lists:keyfind(Key, 1, Acc) of
                    false -> [{Key, [Duration]} | Acc];
                    {Key, Durations} -> lists:keyreplace(Key, 1, Acc, {Key, [Duration | Durations]})
                end
            end, [], CallReturns),
            
            % Calculate statistics per function
            FuncStats = lists:map(fun({Key, Durations}) ->
                {Key, length(Durations), lists:sum(Durations), avg(Durations), 
                 lists:min(Durations), lists:max(Durations)}
            end, FuncTimes),
            
            % Sort by total time
            Sorted = lists:reverse(lists:keysort(3, FuncStats)),
            Top20 = lists:sublist(Sorted, 20),
            
            io:format("Top 20 Functions by Total Time:~n"),
            io:format("~-30s ~-10s ~-15s ~-15s ~-15s ~-15s~n", 
                      ["Function", "Calls", "Total (us)", "Avg (us)", "Min (us)", "Max (us)"]),
            io:format(string:copies("-", 100) ++ "~n"),
            lists:foreach(fun({{M, F, A}, Count, Total, Avg, Min, Max}) ->
                FuncStr = lists:flatten(io_lib:format("~p:~p/~p", [M, F, A])),
                io:format("~-30s ~-10p ~-15p ~-15.2f ~-15p ~-15p~n", 
                          [FuncStr, Count, Total, Avg, Min, Max])
            end, Top20)
    end,
    io:format("~n").

%% Match function calls with returns to calculate durations
match_calls_returns(Calls, Returns) ->
    % Create a map of Pid -> [{CallTime, M, F, A}, ...]
    CallMap = lists:foldl(fun({call, Pid, M, F, A, CallTime}, Acc) ->
        case lists:keyfind(Pid, 1, Acc) of
            false -> [{Pid, [{CallTime, M, F, A}]} | Acc];
            {Pid, CallList} -> lists:keyreplace(Pid, 1, Acc, {Pid, [{CallTime, M, F, A} | CallList]})
        end
    end, [], Calls),
    
    % Match returns with calls
    lists:foldl(fun({return, Pid, M, F, A, _Return, ReturnTime}, Acc) ->
        case lists:keyfind(Pid, 1, CallMap) of
            false -> Acc;
            {Pid, CallList} ->
                % Find matching call (same M, F, A, closest time before return)
                MatchingCalls = [C || C = {CT, CM, CF, CA} <- CallList, 
                                     CM =:= M, CF =:= F, CA =:= A, CT < ReturnTime],
                case MatchingCalls of
                    [] -> Acc;
                    _ ->
                        % Take the most recent call
                        {CallTime, _, _, _} = lists:last(lists:keysort(1, MatchingCalls)),
                        Duration = timer:now_diff(ReturnTime, CallTime),
                        [{M, F, A, Duration} | Acc]
                end
        end
    end, [], Returns).

%% Analyze neural network activity
analyze_neural_activity() ->
    io:format("=== NEURAL NETWORK ACTIVITY ANALYSIS ===~n"),
    
    AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
    NeuralProcesses = lists:foldl(fun({_TS, Snapshot}, Acc) ->
        lists:foldl(fun({Pid, TS, Red, MQ, {M, F, _A}, _Status}, PAcc) 
                      when M =:= neuron orelse M =:= sensor orelse M =:= actuator orelse M =:= cortex ->
            [{Pid, TS, Red, MQ, {M, F}} | PAcc];
        (_, PAcc) -> PAcc
        end, Acc, Snapshot)
    end, [], AllSnapshots),
    
    case NeuralProcesses of
        [] ->
            io:format("No neural process activity captured.~n");
        _ ->
            FuncCounts = lists:foldl(fun({_Pid, _TS, _Red, _MQ, Func}, Acc) ->
                case lists:keyfind(Func, 1, Acc) of
                    false -> [{Func, 1} | Acc];
                    {Func, Count} -> lists:keyreplace(Func, 1, Acc, {Func, Count + 1})
                end
            end, [], NeuralProcesses),
            
            Sorted = lists:reverse(lists:keysort(2, FuncCounts)),
            io:format("Neural Function Activity:~n"),
            lists:foreach(fun({{M, F}, Count}) ->
                io:format("  ~p:~p: ~p occurrences~n", [M, F, Count])
            end, Sorted)
    end,
    io:format("~n").

%% Helper: Find most common element
most_common([]) -> undefined;
most_common(List) ->
    Counts = lists:foldl(fun(Item, Acc) ->
        case lists:keyfind(Item, 1, Acc) of
            false -> [{Item, 1} | Acc];
            {Item, Count} -> lists:keyreplace(Item, 1, Acc, {Item, Count + 1})
        end
    end, [], List),
    {MostCommon, _} = lists:last(lists:keysort(2, Counts)),
    MostCommon.

%% Helper: Calculate average
avg([]) -> 0;
avg(List) -> lists:sum(List) / length(List).

%% Helper: Truncate string to max length
truncate_string(String, MaxLen) when is_list(String) ->
    try
        case length(String) > MaxLen of
            true -> 
                Truncated = lists:sublist(String, MaxLen - 3),
                Truncated ++ "...";
            false -> String
        end
    catch
        _:_ -> 
            % If string has issues, convert to printable and truncate
            SafeStr = lists:flatten(io_lib:format("~p", [String])),
            case length(SafeStr) > MaxLen of
                true -> lists:sublist(SafeStr, MaxLen - 3) ++ "...";
                false -> SafeStr
            end
    end;
truncate_string(Other, MaxLen) ->
    SafeStr = lists:flatten(io_lib:format("~p", [Other])),
    case length(SafeStr) > MaxLen of
        true -> lists:sublist(SafeStr, MaxLen - 3) ++ "...";
        false -> SafeStr
    end.

%% Export data to file
export_data(Filename) ->
    AllTraces = ets:tab2list(?TRACE_TABLE),
    AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
    {ok, File} = file:open(Filename, [write]),
    io:format(File, "~p.~n", [{AllTraces, AllSnapshots}]),
    file:close(File),
    io:format("Data exported to ~p~n", [Filename]).

%% Quick summary
summary() ->
    StartTime = case ets:lookup(?TRACE_TABLE, start_time) of
        [{start_time, ST}] -> ST;
        [] -> undefined
    end,
    EndTime = case ets:lookup(?TRACE_TABLE, end_time) of
        [{end_time, ET}] -> ET;
        [] -> erlang:timestamp()
    end,
    
    case StartTime of
        undefined ->
            io:format("No profiling data.~n");
        _ ->
            Duration = timer:now_diff(EndTime, StartTime) / 1000000,
            AllSnapshots = ets:tab2list(?SNAPSHOT_TABLE),
            TotalSnapshots = length(AllSnapshots),
            AllTraces = ets:tab2list(?TRACE_TABLE),
            Messages = length([T || {K, T} <- AllTraces, element(1, K) =:= msg_send]),
            
            io:format("Profiling Summary:~n"),
            io:format("  Duration: ~.2f seconds~n", [Duration]),
            io:format("  Snapshots: ~p~n", [TotalSnapshots]),
            io:format("  Messages tracked: ~p~n", [Messages])
    end.



