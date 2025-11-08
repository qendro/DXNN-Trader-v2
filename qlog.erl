
-module(qlog).
-export([agent/2, l1msg/2, l2msg/2, l3msg/2, morph/2, agent_morph/2, delete_agent_folder/1, init_debug/2, spawn_debug/2, ets_debug/2, process_debug/2, population/2, architecture/2, training/2, trading/2, genotype_snapshot/2, genotype_creation/1, genotype_mutation/3, genotype_fitness/3, genotype_weight_update/3, log_comment/2, generation_boundary/3, lineage_tracking/3, population_summary/2, evolution_milestone/2]).
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

