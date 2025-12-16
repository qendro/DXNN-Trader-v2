%% Process monitoring module for DXNN system
%% Provides functions to log process memory usage, message queue, and comprehensive process information
-module(process_monitor).
-export([log_memory_usage/1, log_message_queue/1, log_process_info/1]).

%% Identify process type from function, initial_call, and registered_name
identify_process_type(Fun, Init, Reg) ->
    case Reg of
        _ when Reg =/= undefined, Reg =/= [] -> atom_to_list(Reg);
        _ ->
            case Init of
                {cortex, prep, _} -> "Cortex";
                {sensor, prep, _} -> "Sensor";
                {neuron, prep, _} -> "Neuron";
                {actuator, prep, _} -> "Actuator";
                {substrate, prep, _} -> "Substrate";
                {substrate_cpp, prep, _} -> "Substrate_CPP";
                {substrate_cep, prep, _} -> "Substrate_CEP";
                {exoself, prep, _} -> "ExoSelf";
                {scape, gen, _} -> "Scape";
                {M, F, _} when is_atom(M), is_atom(F) -> atom_to_list(M);
                _ ->
                    case Fun of
                        {cortex, loop, _} -> "Cortex";
                        {sensor, loop, _} -> "Sensor";
                        {neuron, loop, _} -> "Neuron";
                        {actuator, loop, _} -> "Actuator";
                        {substrate, loop, _} -> "Substrate";
                        {substrate_cpp, loop, _} -> "Substrate_CPP";
                        {substrate_cep, loop, _} -> "Substrate_CEP";
                        {exoself, loop, _} -> "ExoSelf";
                        {M, F, _} when is_atom(M), is_atom(F) -> atom_to_list(M);
                        _ -> "Unknown"
                    end
            end
    end.

%% Format current function tuple to string
format_current_function(undefined) -> "undefined";
format_current_function({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) ->
    lists:flatten(io_lib:format("~s:~s/~p", [atom_to_list(M), atom_to_list(F), A]));
format_current_function(Other) -> lists:flatten(io_lib:format("~p", [Other])).

%% Collect process information for all provided PIDs
collect_process_info(Pids, _Options) ->
    lists:foldl(fun(Pid, Acc) ->
        case erlang:process_info(Pid, [memory, message_queue_len, current_function, initial_call, registered_name, reductions, status, priority, heap_size, total_heap_size]) of
            undefined -> Acc;
            Info ->
                Memory = proplists:get_value(memory, Info, 0),
                QueueLen = proplists:get_value(message_queue_len, Info, 0),
                Fun = proplists:get_value(current_function, Info, undefined),
                Init = proplists:get_value(initial_call, Info, undefined),
                Reg = proplists:get_value(registered_name, Info, undefined),
                Reductions = proplists:get_value(reductions, Info, 0),
                Status = proplists:get_value(status, Info, unknown),
                Priority = proplists:get_value(priority, Info, normal),
                HeapSize = proplists:get_value(heap_size, Info, 0),
                TotalHeapSize = proplists:get_value(total_heap_size, Info, 0),
                Type = identify_process_type(Fun, Init, Reg),
                Function = format_current_function(Fun),
                [{Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize} | Acc]
        end
    end, [], Pids).

%% Format process entry to string
format_process_entry(Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize) ->
    PidStr = pid_to_list(Pid),
    lists:flatten(io_lib:format("Process ID: ~s | Type: ~s | Function: ~s | Memory: ~p | Heap: ~p | TotalHeap: ~p | Queue: ~p | Reductions: ~p | Status: ~p | Priority: ~p", 
        [PidStr, Type, Function, Memory, HeapSize, TotalHeapSize, QueueLen, Reductions, Status, Priority])).

%% Sort processes by specified metric
sort_processes(Processes, memory) -> lists:sort(fun({_, _, _, M1, _, _, _, _, _, _}, {_, _, _, M2, _, _, _, _, _, _}) -> M1 >= M2 end, Processes);
sort_processes(Processes, message_queue) -> lists:sort(fun({_, _, _, _, Q1, _, _, _, _, _}, {_, _, _, _, Q2, _, _, _, _, _}) -> Q1 >= Q2 end, Processes);
sort_processes(Processes, reductions) -> lists:sort(fun({_, _, _, _, _, R1, _, _, _, _}, {_, _, _, _, _, R2, _, _, _, _}) -> R1 >= R2 end, Processes);
sort_processes(Processes, heap_size) -> lists:sort(fun({_, _, _, _, _, _, _, _, H1, _}, {_, _, _, _, _, _, _, _, H2, _}) -> H1 >= H2 end, Processes);
sort_processes(Processes, total_heap_size) -> lists:sort(fun({_, _, _, _, _, _, _, _, _, T1}, {_, _, _, _, _, _, _, _, _, T2}) -> T1 >= T2 end, Processes);
sort_processes(Processes, _) -> sort_processes(Processes, memory).

%% Limit processes to top N
limit_processes(Processes, all) -> Processes;
limit_processes(Processes, undefined) -> Processes;
limit_processes(Processes, N) when is_integer(N), N > 0 -> lists:sublist(Processes, N);
limit_processes(Processes, _) -> Processes.

%% Get option value with default
get_option(Options, Key, Default) ->
    case proplists:get_value(Key, Options) of
        undefined -> Default;
        Value -> Value
    end.

%% Collect system-wide metrics
collect_system_metrics() ->
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    ProcessMemory = proplists:get_value(processes, MemoryInfo, 0),
    SystemMemory = proplists:get_value(system, MemoryInfo, 0),
    AtomMemory = proplists:get_value(atom, MemoryInfo, 0),
    BinaryMemory = proplists:get_value(binary, MemoryInfo, 0),
    CodeMemory = proplists:get_value(code, MemoryInfo, 0),
    EtsMemory = proplists:get_value(ets, MemoryInfo, 0),
    ProcessCount = length(erlang:processes()),
    Runtime = case erlang:statistics(runtime) of
        {RT_Total, _} -> RT_Total;
        _ -> 0
    end,
    WallClock = case erlang:statistics(wall_clock) of
        {WC_Total, _} -> WC_Total;
        _ -> 0
    end,
    {GCRuns, WordsReclaimed} = case erlang:statistics(garbage_collection) of
        {Runs, Words} -> {Runs, Words};
        _ -> {0, 0}
    end,
    {TotalReductions, _} = case erlang:statistics(reductions) of
        {Red_Total, _} -> {Red_Total, 0};
        _ -> {0, 0}
    end,
    {TotalMemory, ProcessMemory, SystemMemory, AtomMemory, BinaryMemory, CodeMemory, EtsMemory, ProcessCount, Runtime, WallClock, GCRuns, WordsReclaimed, TotalReductions}.

%% Format system metrics summary line
format_system_summary() ->
    {TotalMem, ProcMem, SysMem, AtomMem, BinMem, CodeMem, EtsMem, ProcCount, RunTime, WallTime, GCRuns, WordsReclaimed, TotalRed} = collect_system_metrics(),
    lists:flatten(io_lib:format("=== SYSTEM SUMMARY === TotalMemory: ~p | ProcessMemory: ~p | SystemMemory: ~p | AtomMemory: ~p | BinaryMemory: ~p | CodeMemory: ~p | EtsMemory: ~p | ProcessCount: ~p | Runtime: ~p ms | WallClock: ~p ms | GCRuns: ~p | WordsReclaimed: ~p | TotalReductions: ~p",
        [TotalMem, ProcMem, SysMem, AtomMem, BinMem, CodeMem, EtsMem, ProcCount, RunTime, WallTime, GCRuns, WordsReclaimed, TotalRed])).

%% Log memory usage for all processes
log_memory_usage(Options) ->
    case get_option(Options, full_system, false) of
        true -> qlog:process_monitor(format_system_summary());
        _ -> ok
    end,
    Pids = erlang:processes(),
    ProcessInfo = collect_process_info(Pids, Options),
    Sorted = sort_processes(ProcessInfo, memory),
    Limit = get_option(Options, limit, all),
    Limited = limit_processes(Sorted, Limit),
    Format = get_option(Options, format, per_line),
    Entries = [format_process_entry(Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize) || 
        {Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize} <- Limited],
    case Format of
        per_line -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries);
        single_line -> qlog:process_monitor(string:join(Entries, " | "));
        _ -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries)
    end,
    {ok, logged, length(Entries)}.

%% Log message queue information for all processes
log_message_queue(Options) ->
    case get_option(Options, full_system, false) of
        true -> qlog:process_monitor(format_system_summary());
        _ -> ok
    end,
    Pids = erlang:processes(),
    ProcessInfo = collect_process_info(Pids, Options),
    Sorted = sort_processes(ProcessInfo, message_queue),
    Limit = get_option(Options, limit, all),
    Limited = limit_processes(Sorted, Limit),
    Format = get_option(Options, format, per_line),
    Entries = [format_process_entry(Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize) || 
        {Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize} <- Limited],
    case Format of
        per_line -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries);
        single_line -> qlog:process_monitor(string:join(Entries, " | "));
        _ -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries)
    end,
    {ok, logged, length(Entries)}.

%% Log comprehensive process information
log_process_info(Options) ->
    case get_option(Options, full_system, false) of
        true -> qlog:process_monitor(format_system_summary());
        _ -> ok
    end,
    Pids = erlang:processes(),
    ProcessInfo = collect_process_info(Pids, Options),
    SortBy = get_option(Options, sort_by, memory),
    Sorted = sort_processes(ProcessInfo, SortBy),
    Limit = get_option(Options, limit, all),
    Limited = limit_processes(Sorted, Limit),
    Format = get_option(Options, format, per_line),
    Entries = [format_process_entry(Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize) || 
        {Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize} <- Limited],
    case Format of
        per_line -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries);
        single_line -> qlog:process_monitor(string:join(Entries, " | "));
        _ -> lists:foreach(fun(Entry) -> qlog:process_monitor(Entry) end, Entries)
    end,
    {ok, logged, length(Entries)}.

