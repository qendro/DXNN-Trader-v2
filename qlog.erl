
-module(qlog).
-export([agent/2, l1msg/2, l2msg/2, l3msg/2, morph/2, agent_morph/2, delete_agent_folder/1, init_debug/2, spawn_debug/2, ets_debug/2, process_debug/2, population/2, architecture/2, training/2, trading/2, genotype_snapshot/2, genotype_creation/1, genotype_mutation/3, genotype_fitness/3, genotype_weight_update/3, log_comment/2, generation_boundary/3, lineage_tracking/3, population_summary/2, evolution_milestone/2, benchmarker/2, agent_trades/2, delete_log_folder/0, delete_all/0]).
-include("records.hrl").

%% ============================================================================
%% SIMPLE LOGGING - One log per ExoSelf
%% ============================================================================

%% Agent-level information (structure, initialization, lifecycle events)
agent(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[AGENT] " ++ Msg).

%% Level 1: Critical messages (errors, important state changes)
l1msg(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[L1] " ++ Msg).

%% Level 2: Important messages (training loop, I/O events)
l2msg(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[L2] " ++ Msg).

%% Level 3: Detailed messages (all inter-process communication)
l3msg(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[L3] " ++ Msg).

%% Morphology/Architecture tracking (structural changes and evolution)
morph(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[MORPH] " ++ Msg).

%% Agent morphology tracking (persistent across runs, uses Specie_Id)
agent_morph(Specie_Id, Msg) ->
    write_specie_log(Specie_Id, "[MORPH] " ++ Msg).

%% ============================================================================
%% DEBUG LOGGING - For troubleshooting initialization and process issues
%% ============================================================================

%% Initialization debug logging
init_debug(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[INIT] " ++ Msg).

%% Spawn/debug process creation
spawn_debug(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[SPAWN] " ++ Msg).

%% ETS table operations debug
ets_debug(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[ETS] " ++ Msg).

%% Process lifecycle debug
process_debug(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[PROC] " ++ Msg).

%% ============================================================================
%% SPECIALIZED LOGGING - For comprehensive neural network tracking
%% ============================================================================

%% Population-level tracking (generation transitions, species evolution, overall stats)
population(Population_Id, Msg) ->
    write_population_log(Population_Id, "[POP] " ++ Msg).

%% Architecture tracking (structural mutations, topology changes, substrate processing)
architecture(Agent_Id, Msg) ->
    write_architecture_log(Agent_Id, "[ARCH] " ++ Msg).

%% Training tracking (fitness progression, weight perturbations, learning curves)
training(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[TRAIN] " ++ Msg).

%% Trading tracking (decisions, outcomes, market interactions)
trading(ExoSelf_PId, Msg) ->
    write_log(ExoSelf_PId, "[TRADE] " ++ Msg).

%% ============================================================================
%% GENOTYPE EVOLUTION LOGGING - Complete genotype tracking over time
%% ============================================================================

%% Log complete genotype snapshot with context
genotype_snapshot(Agent_Id, Context) ->
    Filename = get_agent_logfile(Agent_Id),
    F = fun() ->
        A = genotype:read({agent,Agent_Id}),
        Cx = genotype:read({cortex,A#agent.cx_id}),
        {ok, File} = file:open(Filename, [append]),
        
        Timestamp = format_timestamp(),
        io:format(File, "~s | [GENOTYPE_SNAPSHOT] ~s | Agent: ~p | Gen: ~p | Fitness: ~p~n", 
                  [Timestamp, Context, Agent_Id, A#agent.generation, A#agent.fitness]),
        
        %% Write complete genotype using existing genotype:print logic
        io:format(File, "~p~n", [A]),
        io:format(File, "~p~n", [Cx]),
        [io:format(File, "~p~n", [genotype:read({sensor,Id})]) || Id <- Cx#cortex.sensor_ids],
        [io:format(File, "~p~n", [genotype:read({neuron,Id})]) || Id <- Cx#cortex.neuron_ids],
        [io:format(File, "~p~n", [genotype:read({actuator,Id})]) || Id <- Cx#cortex.actuator_ids],
        case A#agent.substrate_id of
            undefined -> ok;
            Substrate_Id ->
                Substrate = genotype:read({substrate,Substrate_Id}),
                io:format(File, "~p~n", [Substrate]),
                [io:format(File, "~p~n", [genotype:read({sensor,Id})]) || Id <- Substrate#substrate.cpp_ids],
                [io:format(File, "~p~n", [genotype:read({actuator,Id})]) || Id <- Substrate#substrate.cep_ids]
        end,
        io:format(File, "~n", []),
        file:close(File)
    end,
    mnesia:transaction(F).

%% ============================================================================
%% EVOLUTIONARY TRACKING - Generation and lineage tracking
%% ============================================================================

%% Log generation boundary events (start/end of generation)
generation_boundary(Population_Id, Generation, Event) ->
    Dir = population_log_dir(),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, lists:flatten(io_lib:format("generation_~p.log", [Generation]))),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [GENERATION_~s] Population: ~p | Gen: ~p~n", [Timestamp, Event, Population_Id, Generation]),
    file:close(File).

%% Log parent-child lineage relationships
lineage_tracking(Parent_Id, Child_Id, Mutation_Details) ->
    Dir = population_log_dir(),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "lineage.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [LINEAGE] Parent: ~p -> Child: ~p | Mutation: ~s~n", [Timestamp, Parent_Id, Child_Id, Mutation_Details]),
    file:close(File).

%% Log population summary statistics
population_summary(Population_Id, Summary_Data) ->
    Dir = population_log_dir(),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, lists:flatten(io_lib:format("population_~p.log", [Population_Id]))),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [POPULATION_SUMMARY] ~s~n", [Timestamp, Summary_Data]),
    file:close(File).

%% Log evolution milestones and key events
evolution_milestone(Event_Type, Event_Details) ->
    Dir = population_log_dir(),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "evolution_milestones.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [MILESTONE] ~s: ~s~n", [Timestamp, Event_Type, Event_Details]),
    file:close(File).

%% High-level benchmarker/training pipeline logging (new dedicated log file)
benchmarker(Run_Id, Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "benchmarker.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [RUN:~p] ~s~n", [Timestamp, Run_Id, Msg]),
    file:close(File).

agent_trades(Agent_Id, Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_trades.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [AGENT:~p] ~s~n", [Timestamp, Agent_Id, Msg]),
    file:close(File).

%% Helper function to ensure directory exists
ensure_directory_exists(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            case filelib:ensure_dir(filename:join(Dir, "dummy")) of
                ok -> ok;
                {error, eexist} -> ok;
                Error -> Error
            end
    end.

population_log_dir() ->
    filename:join([get_log_root_dir(), "Population"]).

get_log_root_dir() ->
    case application:get_env(qlog, log_root_dir) of
        {ok, Dir} -> Dir;
        undefined ->
            case os:getenv("QLOG_ROOT") of
                false -> filename:absname("logs");
                EnvDir -> EnvDir
            end
    end.

%% Log agent creation with complete initial genotype
genotype_creation(Agent_Id) ->
    genotype_snapshot(Agent_Id, "CREATION").

%% Log mutation with before/after states and details
genotype_mutation(Agent_Id, Operation, Details) ->
    Filename = get_agent_logfile(Agent_Id),
    F = fun() ->
        A = genotype:read({agent,Agent_Id}),
        {ok, File} = file:open(Filename, [append]),
        Timestamp = format_timestamp(),
        io:format(File, "~s | [MUTATION] ~s | Agent: ~p | Gen: ~p | Details: ~s~n", 
                  [Timestamp, Operation, Agent_Id, A#agent.generation, Details]),
        file:close(File)
    end,
    mnesia:transaction(F).

%% Log fitness progression
genotype_fitness(Agent_Id, Generation, FitnessData) ->
    Filename = get_agent_logfile(Agent_Id),
    F = fun() ->
        {ok, File} = file:open(Filename, [append]),
        Timestamp = format_timestamp(),
        io:format(File, "~s | [FITNESS] Gen: ~p | ~s~n", [Timestamp, Generation, FitnessData]),
        file:close(File)
    end,
    mnesia:transaction(F).

%% Log weight updates during training
genotype_weight_update(Agent_Id, Generation, UpdateData) ->
    Filename = get_agent_logfile(Agent_Id),
    F = fun() ->
        {ok, File} = file:open(Filename, [append]),
        Timestamp = format_timestamp(),
        io:format(File, "~s | [WEIGHT_UPDATE] Gen: ~p | ~s~n", [Timestamp, Generation, UpdateData]),
        file:close(File)
    end,
    mnesia:transaction(F).

%% Log comments/messages to agent log file instead of terminal
log_comment(Agent_Id, Comment) ->
    Filename = get_agent_logfile(Agent_Id),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [COMMENT] ~s~n", [Timestamp, Comment]),
    file:close(File).

%% ============================================================================
%% HELPER
%% ============================================================================

write_log(ExoSelf_PId, Msg) ->
    LogFile = get_logfile(ExoSelf_PId),
    {ok, F} = file:open(LogFile, [append]),
    Timestamp = format_timestamp(),
    io:format(F, "~s | ~s~n", [Timestamp, Msg]),
    file:close(F).

get_logfile(ExoSelf_PId) ->
    % Convert PID to string: <0.123.0> -> "0.123.0"
    PidStr = pid_to_list(ExoSelf_PId),
    CleanPid = lists:filter(fun(C) -> C =/= $< andalso C =/= $> end, PidStr),
    % Ensure logs/Agents/Exoself directory exists
    filelib:ensure_dir("logs/Agents/Exoself/"),
    lists:flatten(io_lib:format("logs/Agents/Exoself/~s.log", [CleanPid])).

write_specie_log(Specie_Id, Msg) ->
    LogFile = get_specie_logfile(Specie_Id),
    {ok, F} = file:open(LogFile, [append]),
    Timestamp = format_timestamp(),
    io:format(F, "~s | ~s~n", [Timestamp, Msg]),
    file:close(F).

get_specie_logfile(Specie_Id) ->
    % Specie_Id can be atom or tuple - convert to safe filename
    IdStr = lists:flatten(io_lib:format("~p", [Specie_Id])),
    % Remove unsafe characters, keep alphanumeric, dots, underscores, hyphens
    CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
    % Ensure logs/Agents/Morphology directory exists
    filelib:ensure_dir("logs/Agents/Morphology/"),
    lists:flatten(io_lib:format("logs/Agents/Morphology/~s.morph.log", [CleanId])).

write_population_log(Population_Id, Msg) ->
    LogFile = get_population_logfile(Population_Id),
    {ok, F} = file:open(LogFile, [append]),
    Timestamp = format_timestamp(),
    io:format(F, "~s | ~s~n", [Timestamp, Msg]),
    file:close(F).

get_population_logfile(Population_Id) ->
    % Population_Id can be atom or tuple - convert to safe filename
    IdStr = lists:flatten(io_lib:format("~p", [Population_Id])),
    % Remove unsafe characters, keep alphanumeric, dots, underscores, hyphens
    CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
    % Ensure logs/Agents/Population directory exists
    filelib:ensure_dir("logs/Agents/Population/"),
    lists:flatten(io_lib:format("logs/Agents/Population/~s.pop.log", [CleanId])).

write_architecture_log(Agent_Id, Msg) ->
    LogFile = get_architecture_logfile(Agent_Id),
    {ok, F} = file:open(LogFile, [append]),
    Timestamp = format_timestamp(),
    io:format(F, "~s | ~s~n", [Timestamp, Msg]),
    file:close(F).

get_architecture_logfile(Agent_Id) ->
    % Agent_Id can be atom or tuple - convert to safe filename
    IdStr = lists:flatten(io_lib:format("~p", [Agent_Id])),
    % Remove unsafe characters, keep alphanumeric, dots, underscores, hyphens
    CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
    % Ensure logs/Agents/Architecture directory exists
    filelib:ensure_dir("logs/Agents/Architecture/"),
    lists:flatten(io_lib:format("logs/Agents/Architecture/~s.arch.log", [CleanId])).

get_agent_logfile(Agent_Id) ->
    % Agent_Id can be atom or tuple - convert to safe filename
    IdStr = lists:flatten(io_lib:format("~p", [Agent_Id])),
    % Remove unsafe characters, keep alphanumeric, dots, underscores, hyphens
    CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
    % Ensure logs/Agents directory exists
    filelib:ensure_dir("logs/Agents/"),
    lists:flatten(io_lib:format("logs/Agents/~s.log", [CleanId])).

format_timestamp() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Y,Mo,D,H,Mi,S])).

%% ============================================================================
%% LOG FOLDER MANAGEMENT
%% ============================================================================

%% Delete the entire log folder and all its contents
%% Uses log_directory() from config.erl
%% Output: ok | {error, Reason}
delete_log_folder() ->
    LogRoot = get_log_root_dir(),
    LogResult = case filelib:is_dir(LogRoot) of
        true ->
            % Recursively delete all files and directories
            case delete_directory_recursive(LogRoot) of
                ok ->
                    io:format("Successfully deleted log folder: ~s~n", [LogRoot]),
                    ok;
                LogError ->
                    io:format("Error deleting log folder ~s: ~p~n", [LogRoot, LogError]),
                    LogError
            end;
        false ->
            io:format("Log folder does not exist: ~s~n", [LogRoot]),
            ok
    end,
    GpuSpecsRoot = filename:absname("../data/gpu_specs/"),
    GpuSpecsResult = case filelib:is_dir(GpuSpecsRoot) of
        true ->
            case delete_directory_recursive(GpuSpecsRoot) of
                ok ->
                    io:format("Successfully deleted gpu_specs folder: ~s~n", [GpuSpecsRoot]),
                    ok;
                GpuError ->
                    io:format("Error deleting gpu_specs folder ~s: ~p~n", [GpuSpecsRoot, GpuError]),
                    GpuError
            end;
        false ->
            io:format("Gpu_specs folder does not exist: ~s~n", [GpuSpecsRoot]),
            ok
    end,
    case LogResult of
        ok -> GpuSpecsResult;
        LogErr -> LogErr
    end.

%% Delete all system-generated folders and files (logs, specs, mnesia, etc.)
%% Output: ok | {error, Reason}
delete_all() ->
    io:format("Starting comprehensive cleanup...~n", []),
    Results = [
        delete_all_logs(),
        delete_all_gpu_specs(),
        delete_fx_tables_non_txt(),
        delete_mnesia_folder(),
        delete_crash_dumps(),
        delete_python_caches()
        %% Add more deletion functions here as needed
    ],
    case lists:filter(fun(R) -> R =/= ok end, Results) of
        [] ->
            io:format("All cleanup operations completed successfully.~n", []),
            ok;
        Errors ->
            io:format("Some cleanup operations failed: ~p~n", [Errors]),
            {error, Errors}
    end.

%% Helper: Delete all log folders
delete_all_logs() ->
    LogRoot = get_log_root_dir(),
    case filelib:is_dir(LogRoot) of
        true ->
            case delete_directory_recursive(LogRoot) of
                ok ->
                    io:format("Deleted log folder: ~s~n", [LogRoot]),
                    ok;
                Error ->
                    io:format("Error deleting log folder ~s: ~p~n", [LogRoot, Error]),
                    Error
            end;
        false ->
            io:format("Log folder does not exist: ~s~n", [LogRoot]),
            ok
    end.

%% Helper: Delete gpu_specs folder
delete_all_gpu_specs() ->
    GpuSpecsRoot = filename:absname("../data/gpu_specs/"),
    case filelib:is_dir(GpuSpecsRoot) of
        true ->
            case delete_directory_recursive(GpuSpecsRoot) of
                ok ->
                    io:format("Deleted gpu_specs folder: ~s~n", [GpuSpecsRoot]),
                    ok;
                Error ->
                    io:format("Error deleting gpu_specs folder ~s: ~p~n", [GpuSpecsRoot, Error]),
                    Error
            end;
        false ->
            io:format("Gpu_specs folder does not exist: ~s~n", [GpuSpecsRoot]),
            ok
    end.

%% Helper: Delete all files in fx_tables that don't end in .txt
delete_fx_tables_non_txt() ->
    FxTablesDir = filename:absname(config:fx_tables_directory()),
    case filelib:is_dir(FxTablesDir) of
        true ->
            case file:list_dir(FxTablesDir) of
                {ok, Files} ->
                    NonTxtFiles = lists:filter(
                        fun(Filename) ->
                            case filename:extension(Filename) of
                                ".txt" -> false;
                                _ -> true
                            end
                        end,
                        Files
                    ),
                    Results = lists:map(
                        fun(Filename) ->
                            Path = filename:join(FxTablesDir, Filename),
                            case filelib:is_dir(Path) of
                                true ->
                                    case delete_directory_recursive(Path) of
                                        ok ->
                                            io:format("Deleted fx_tables directory: ~s~n", [Path]),
                                            ok;
                                        Error ->
                                            io:format("Error deleting fx_tables directory ~s: ~p~n", [Path, Error]),
                                            Error
                                    end;
                                false ->
                                    case file:delete(Path) of
                                        ok ->
                                            io:format("Deleted fx_tables file: ~s~n", [Path]),
                                            ok;
                                        {error, enoent} -> ok;
                                        Error ->
                                            io:format("Error deleting fx_tables file ~s: ~p~n", [Path, Error]),
                                            Error
                                    end
                            end
                        end,
                        NonTxtFiles
                    ),
                    case lists:filter(fun(R) -> R =/= ok end, Results) of
                        [] -> ok;
                        Errors -> {error, Errors}
                    end;
                Error ->
                    io:format("Error listing fx_tables directory ~s: ~p~n", [FxTablesDir, Error]),
                    Error
            end;
        false ->
            io:format("Fx_tables directory does not exist: ~s~n", [FxTablesDir]),
            ok
    end.

%% Helper: Delete Mnesia.nonode@nohost folder
delete_mnesia_folder() ->
    MnesiaDir = filename:absname("Mnesia.nonode@nohost"),
    case filelib:is_dir(MnesiaDir) of
        true ->
            case delete_directory_recursive(MnesiaDir) of
                ok ->
                    io:format("Deleted Mnesia folder: ~s~n", [MnesiaDir]),
                    ok;
                Error ->
                    io:format("Error deleting Mnesia folder ~s: ~p~n", [MnesiaDir, Error]),
                    Error
            end;
        false ->
            io:format("Mnesia folder does not exist: ~s~n", [MnesiaDir]),
            ok
    end.

%% Helper: Delete Erlang crash dump files
delete_crash_dumps() ->
    ErlangDir = filename:absname("."),
    case file:list_dir(ErlangDir) of
        {ok, Files} ->
            CrashDumps = lists:filter(
                fun(Filename) ->
                    case filename:extension(Filename) of
                        ".dump" -> true;
                        _ -> false
                    end
                end,
                Files
            ),
            Results = lists:map(
                fun(Filename) ->
                    Path = filename:join(ErlangDir, Filename),
                    case file:delete(Path) of
                        ok ->
                            io:format("Deleted crash dump: ~s~n", [Path]),
                            ok;
                        {error, enoent} -> ok;
                        Error ->
                            io:format("Error deleting crash dump ~s: ~p~n", [Path, Error]),
                            Error
                    end
                end,
                CrashDumps
            ),
            case lists:filter(fun(R) -> R =/= ok end, Results) of
                [] -> ok;
                Errors -> {error, Errors}
            end;
        Error ->
            io:format("Error listing directory for crash dumps: ~p~n", [Error]),
            Error
    end.

%% Helper: Delete Python cache files and directories (__pycache__, .pyc, .pyo)
delete_python_caches() ->
    PythonDir = filename:absname("../python/"),
    case filelib:is_dir(PythonDir) of
        true ->
            case delete_python_caches_recursive(PythonDir) of
                ok ->
                    io:format("Deleted Python caches in: ~s~n", [PythonDir]),
                    ok;
                Error ->
                    io:format("Error deleting Python caches: ~p~n", [Error]),
                    Error
            end;
        false ->
            io:format("Python directory does not exist: ~s~n", [PythonDir]),
            ok
    end.

%% Helper: Recursively delete Python cache files and directories
delete_python_caches_recursive(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            case file:list_dir(Dir) of
                {ok, Items} ->
                    Results = lists:map(
                        fun(Item) ->
                            Path = filename:join(Dir, Item),
                            case Item of
                                "__pycache__" ->
                                    % Delete entire __pycache__ directory
                                    case delete_directory_recursive(Path) of
                                        ok ->
                                            io:format("Deleted __pycache__: ~s~n", [Path]),
                                            ok;
                                        Error ->
                                            io:format("Error deleting __pycache__ ~s: ~p~n", [Path, Error]),
                                            Error
                                    end;
                                _ ->
                                    % Check if it's a .pyc or .pyo file
                                    case filename:extension(Item) of
                                        ".pyc" ->
                                            case file:delete(Path) of
                                                ok ->
                                                    io:format("Deleted .pyc: ~s~n", [Path]),
                                                    ok;
                                                {error, enoent} -> ok;
                                                Error ->
                                                    io:format("Error deleting .pyc ~s: ~p~n", [Path, Error]),
                                                    Error
                                            end;
                                        ".pyo" ->
                                            case file:delete(Path) of
                                                ok ->
                                                    io:format("Deleted .pyo: ~s~n", [Path]),
                                                    ok;
                                                {error, enoent} -> ok;
                                                Error ->
                                                    io:format("Error deleting .pyo ~s: ~p~n", [Path, Error]),
                                                    Error
                                            end;
                                        _ ->
                                            % Recursively process subdirectories (but skip .venv)
                                            case filelib:is_dir(Path) of
                                                true ->
                                                    case Item of
                                                        ".venv" -> ok; % Skip virtual environment
                                                        _ -> delete_python_caches_recursive(Path)
                                                    end;
                                                false -> ok
                                            end
                                    end
                            end
                        end,
                        Items
                    ),
                    case lists:filter(fun(R) -> R =/= ok end, Results) of
                        [] -> ok;
                        Errors -> {error, Errors}
                    end;
                Error ->
                    Error
            end;
        false ->
            ok
    end.

%% Helper function to recursively delete a directory and all its contents
delete_directory_recursive(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            % Get all files and directories
            case file:list_dir(Dir) of
                {ok, Files} ->
                    % Delete each item (file or subdirectory)
                    DeleteResult = lists:foldl(
                        fun(Item, Acc) ->
                            Path = filename:join(Dir, Item),
                            case filelib:is_dir(Path) of
                                true ->
                                    % Recursively delete subdirectory
                                    case delete_directory_recursive(Path) of
                                        ok -> Acc;
                                        Error -> [{subdir_error, Path, Error} | Acc]
                                    end;
                                false ->
                                    % Delete file
                                    case file:delete(Path) of
                                        ok -> Acc;
                                        {error, enoent} -> Acc; % Already deleted
                                        Error -> [{file_error, Path, Error} | Acc]
                                    end
                            end
                        end,
                        [],
                        Files
                    ),
                    % If any deletions failed, return error
                    case DeleteResult of
                        [] ->
                            % All files deleted, now delete the directory itself
                            case file:del_dir(Dir) of
                                ok -> ok;
                                {error, enoent} -> ok; % Already deleted
                                {error, enotempty} ->
                                    % Directory still not empty, try again
                                    delete_directory_recursive(Dir);
                                Error -> Error
                            end;
                        Errors ->
                            {error, {deletion_errors, Errors}}
                    end;
                Error ->
                    Error
            end;
        false ->
            ok
    end.


        %% ============================================================================
%% AGENT FOLDER MANAGEMENT
%% ============================================================================

%% Delete a top-level agent folder completely (removes all logs for that agent)
%% Input: Agent_Pid or Specie_Id (atom/tuple)
%% Output: ok | {error, Reason}
delete_agent_folder(Agent_Identifier) ->
    case Agent_Identifier of
        Pid when is_pid(Pid) ->
            % Delete agent-specific logs (PID-based)
            PidStr = pid_to_list(Pid),
            CleanPid = lists:filter(fun(C) -> C =/= $< andalso C =/= $> end, PidStr),
            AgentLogFile = lists:flatten(io_lib:format("logs/Agents/Exoself/~s.log", [CleanPid])),
            case file:delete(AgentLogFile) of
                ok -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Deleted agent log: ~s", [AgentLogFile]))),
                    ok;
                {error, enoent} -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Agent log file not found: ~s", [AgentLogFile]))),
                    ok;
                Error -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Error deleting agent log ~s: ~p", [AgentLogFile, Error]))),
                    Error
            end;
        Specie_Id ->
            % Delete morphology logs (Specie_Id-based)
            IdStr = lists:flatten(io_lib:format("~p", [Specie_Id])),
            CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
            MorphLogFile = lists:flatten(io_lib:format("logs/Agents/Morphology/~s.morph.log", [CleanId])),
            case file:delete(MorphLogFile) of
                ok -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Deleted morphology log: ~s", [MorphLogFile]))),
                    ok;
                {error, enoent} -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Morphology log file not found: ~s", [MorphLogFile]))),
                    ok;
                Error -> 
                    log_comment({system,delete_agent_folder}, lists:flatten(io_lib:format("Error deleting morphology log ~s: ~p", [MorphLogFile, Error]))),
                    Error
            end
    end.

