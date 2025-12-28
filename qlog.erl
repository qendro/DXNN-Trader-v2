
-module(qlog).
-export([agent/2, l1msg/2, l2msg/2, l3msg/2, morph/2, agent_morph/2, delete_agent_folder/1, init_debug/2, spawn_debug/2, ets_debug/2, process_debug/2, population/2, architecture/2, training/2, trading/2, agent_trades/2, log_agent_metadata/3, genotype_snapshot/2, genotype_creation/1, genotype_mutation/3, genotype_fitness/3, genotype_weight_update/3, log_comment/2, generation_boundary/3, lineage_tracking/3, population_summary/2, evolution_milestone/2, benchmarker/2, exp_runner/2, delete_log_folder/0, delete_all/0, xLog/3, register_agent/2, process_monitor/1, genotype_log/3, pid_map_log/4, agent_reductions_log/3, get_run_id_from_population_id/1, log_agent_memory_usage/0, print_genotype/1, agent_gen/2]).
-include("records.hrl").

-define(AGENT_PID_MAP, agent_pid_map).

init_ets() ->
    case ets:whereis(?AGENT_PID_MAP) of
        undefined -> 
            try 
                ets:new(?AGENT_PID_MAP, [set, public, named_table])
            catch
                error:badarg -> ?AGENT_PID_MAP  % Table created by another process concurrently
            end;
        TableId -> TableId
    end.

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

%% Agent trades tracking (fitness evaluation with trade details)
%% Writes to a single consolidated log file: logs/Benchmarker/agent_trades.log
agent_trades(Agent_Id, Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_trades.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [AGENT:~p] ~s~n", [Timestamp, Agent_Id, Msg]),
    file:close(File).

%% Agent metadata tracking (encoding type, neurons, sensors, generation, run number, ex_pid)
%% Writes to logs/Benchmarker/agent_meta.log
agent_meta(Agent_Id, Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_meta.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | [AGENT:~p] ~s~n", [Timestamp, Agent_Id, Msg]),
    file:close(File).

%% Log agent metadata (encoding type, neurons, sensors, generation, run number, ex_pid)
%% Writes to logs/Benchmarker/agent_meta.log
log_agent_metadata(Agent_Id, Generation, ExoSelf_PId) ->
    case catch genotype:dirty_read({agent, Agent_Id}) of
        A when is_record(A, agent) ->
            % Extract encoding type
            EncodingType = A#agent.encoding_type,
            
            % Read cortex to get neuron count and sensors
            Cx = genotype:dirty_read({cortex, A#agent.cx_id}),
            NumNeurons = length(Cx#cortex.neuron_ids),
            
            % Extract sensor names and sizes
            SensorInfos = [begin
                S = genotype:dirty_read({sensor, SId}),
                io_lib:format("~p(~p)", [S#sensor.name, S#sensor.vl])
            end || SId <- Cx#cortex.sensor_ids],
            SensorsStr = string:join(SensorInfos, ","),
            
            % Get run number from population_id
            RunNumber = get_run_id_from_population_id(A#agent.population_id),
            
            % Format and log (including ex_pid)
            Msg = io_lib:format(" | ex_pid=~p | run=~p | generation=~p | encoding=~p | neurons=~p | sensors=~s", 
                                [ExoSelf_PId, RunNumber, Generation, EncodingType, NumNeurons, SensorsStr]),
            agent_meta(Agent_Id, Msg);
        _ ->
            % Agent not found, skip logging
            ok
    end.

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
    io:format(File, "~s | [Eval:~p] ~s~n", [Timestamp, Run_Id, Msg]),
    file:close(File).

%% Process monitoring logging (writes to Benchmarker folder)
process_monitor(Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "process_monitor.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | ~s~n", [Timestamp, Msg]),
    file:close(File).

%% Agent generation logging (writes to Benchmarker folder)
agent_gen(Population_Id, Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_gen.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    PopIdStr = case is_atom(Population_Id) of
        true -> atom_to_list(Population_Id);
        false -> lists:flatten(io_lib:format("~p", [Population_Id]))
    end,
    io:format(File, "~s | [POP:~s] ~s~n", [Timestamp, PopIdStr, Msg]),
    file:close(File).

%% ============================================================================
%% NEW LOGGING FUNCTIONS - Genotypes, PID Maps, and Reductions
%% ============================================================================

%% Helper: Get Run_Id from Population_Id by querying experiment record
get_run_id_from_population_id(Population_Id) ->
    F = fun() ->
        % Try to find experiment where this population_id matches
        % Check if population_id is in any experiment's pm_parameters
        Experiments = mnesia:match_object({experiment, '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_', '_'}),
        case lists:filter(fun(E) ->
            case E#experiment.pm_parameters of
                undefined -> false;
                PMP -> PMP#pmp.population_id == Population_Id
            end
        end, Experiments) of
            [E | _] -> E#experiment.run_index;
            [] -> 
                % Try to extract run number from population_id format
                % Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
                case Population_Id of
                    undefined -> "unknown";
                    PopId when is_binary(PopId) ->
                        % Convert binary to string and extract run number
                        PopIdStr = binary_to_list(PopId),
                        case string:str(PopIdStr, "_run") of
                            0 -> PopId;  % No "_run" found, return original
                            Pos ->
                                RunPart = string:substr(PopIdStr, Pos + 4),  % Skip "_run"
                                case catch list_to_integer(RunPart) of
                                    RunNum when is_integer(RunNum) -> RunNum;
                                    _ -> PopId  % Conversion failed, return original
                                end
                        end;
                    _ -> Population_Id  % Not binary, return as-is
                end
        end
    end,
    case catch mnesia:transaction(F) of
        {atomic, RunId} -> RunId;
        _ -> 
            % Fallback: try to extract from population_id directly
            case Population_Id of
                undefined -> "unknown";
                PopId when is_binary(PopId) ->
                    PopIdStr = binary_to_list(PopId),
                    case string:str(PopIdStr, "_run") of
                        0 -> PopId;
                        Pos ->
                            RunPart = string:substr(PopIdStr, Pos + 4),
                            case catch list_to_integer(RunPart) of
                                RunNum when is_integer(RunNum) -> RunNum;
                                _ -> PopId
                            end
                    end;
                _ -> Population_Id
            end
    end.

%% Log full genotype for an agent (called at generation boundaries)
genotype_log(Run_Id, Generation, Agent_Id) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "genotypes.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    
    % Read full genotype
    A = genotype:dirty_read({agent, Agent_Id}),
    Cx = genotype:dirty_read({cortex, A#agent.cx_id}),
    Sensors = [genotype:dirty_read({sensor, Id}) || Id <- Cx#cortex.sensor_ids],
    Neurons = [genotype:dirty_read({neuron, Id}) || Id <- Cx#cortex.neuron_ids],
    Actuators = [genotype:dirty_read({actuator, Id}) || Id <- Cx#cortex.actuator_ids],
    Substrate = case A#agent.substrate_id of
        undefined -> undefined;
        SubId -> genotype:dirty_read({substrate, SubId})
    end,
    
    % Format as single line (using erlang term format - ~w for compact one-line format)
    GenotypeData = {agent, A, cortex, Cx, sensors, Sensors, neurons, Neurons, 
                    actuators, Actuators, substrate, Substrate},
    % Format Agent_Id as compact string to keep on one line
    AgentStr = lists:flatten(io_lib:format("~w", [Agent_Id])),
    % Format Run_Id - handle binary by converting to string
    RunStr = case is_binary(Run_Id) of
        true -> binary_to_list(Run_Id);
        false -> lists:flatten(io_lib:format("~w", [Run_Id]))
    end,
    % Format GenotypeData as compact string to keep on one line
    GenotypeStr = lists:flatten(io_lib:format("~w", [GenotypeData])),
    io:format(File, "~s | RUN:~s | GEN:~p | AGENT:~s | GENOTYPE:~s~n", 
              [Timestamp, RunStr, Generation, AgentStr, GenotypeStr]),
    file:close(File).

%% Print and save genotype of an agent to genotypes.log (uses genotype:print logic)
%% Usage: qlog:print_genotype(Agent_Id)
print_genotype(Agent_Id) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "genotypes.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    
    % Read full genotype using dirty_read (faster than transaction-based read)
    case genotype:dirty_read({agent, Agent_Id}) of
        undefined ->
            io:format("Error: Agent ~p not found~n", [Agent_Id]),
            io:format(File, "~s | ERROR: Agent ~p not found~n", [Timestamp, Agent_Id]),
            file:close(File);
        A ->
            case genotype:dirty_read({cortex, A#agent.cx_id}) of
                undefined ->
                    io:format("Error: Cortex ~p not found for agent ~p~n", [A#agent.cx_id, Agent_Id]),
                    io:format(File, "~s | ERROR: Cortex ~p not found for agent ~p~n", [Timestamp, A#agent.cx_id, Agent_Id]),
                    file:close(File);
                Cx ->
                    % Print header to both console and file
                    Header = io_lib:format("~s | [PRINT_GENOTYPE] Agent: ~p | Generation: ~p | Fitness: ~p~n", 
                                           [Timestamp, Agent_Id, A#agent.generation, A#agent.fitness]),
                    io:format("~s", [Header]),
                    io:format(File, "~s", [Header]),
                    
                    % Print agent record
                    io:format("~p~n", [A]),
                    io:format(File, "~p~n", [A]),
                    
                    % Print cortex record
                    io:format("~p~n", [Cx]),
                    io:format(File, "~p~n", [Cx]),
                    
                    % Print sensors
                    [begin
                        Sensor = genotype:dirty_read({sensor, Id}),
                        io:format("~p~n", [Sensor]),
                        io:format(File, "~p~n", [Sensor])
                    end || Id <- Cx#cortex.sensor_ids],
                    
                    % Print neurons
                    [begin
                        Neuron = genotype:dirty_read({neuron, Id}),
                        io:format("~p~n", [Neuron]),
                        io:format(File, "~p~n", [Neuron])
                    end || Id <- Cx#cortex.neuron_ids],
                    
                    % Print actuators
                    [begin
                        Actuator = genotype:dirty_read({actuator, Id}),
                        io:format("~p~n", [Actuator]),
                        io:format(File, "~p~n", [Actuator])
                    end || Id <- Cx#cortex.actuator_ids],
                    
                    % Print substrate if it exists
                    case A#agent.substrate_id of
                        undefined ->
                            ok;
                        Substrate_Id ->
                            Substrate = genotype:dirty_read({substrate, Substrate_Id}),
                            io:format("~p~n", [Substrate]),
                            io:format(File, "~p~n", [Substrate]),
                            
                            % Print CPP sensors
                            [begin
                                CPP = genotype:dirty_read({sensor, Id}),
                                io:format("~p~n", [CPP]),
                                io:format(File, "~p~n", [CPP])
                            end || Id <- Substrate#substrate.cpp_ids],
                            
                            % Print CEP actuators
                            [begin
                                CEP = genotype:dirty_read({actuator, Id}),
                                io:format("~p~n", [CEP]),
                                io:format(File, "~p~n", [CEP])
                            end || Id <- Substrate#substrate.cep_ids]
                    end,
                    
                    % Print separator line
                    io:format("---~n", []),
                    io:format(File, "---~n", []),
                    
                    file:close(File)
            end
    end.

%% Log PID map for an agent (called when agent spawns)
pid_map_log(Run_Id, Generation, Agent_Id, All_Pids) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "pid_map.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    % Format PIDs as comma-separated string to keep on one line
    PidsStr = string:join([pid_to_list(Pid) || Pid <- All_Pids], ","),
    % Format Agent_Id as compact string to keep on one line
    AgentStr = lists:flatten(io_lib:format("~w", [Agent_Id])),
    % Format Run_Id - handle binary by converting to string
    RunStr = case is_binary(Run_Id) of
        true -> binary_to_list(Run_Id);
        false -> lists:flatten(io_lib:format("~w", [Run_Id]))
    end,
    io:format(File, "~s | RUN:~s | GEN:~p | AGENT:~s | PIDS:[~s]~n", 
              [Timestamp, RunStr, Generation, AgentStr, PidsStr]),
    file:close(File).

%% Log agent reductions (called at each evaluation)
agent_reductions_log(Agent_Id, Eval_Num, Total_Reductions) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_reductions.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | AGENT:~p | EVAL:~p | REDUCTIONS:~p~n", 
              [Timestamp, Agent_Id, Eval_Num, Total_Reductions]),
    file:close(File).

%% Log agent memory usage aggregated by agent (manual/sporadic use)
log_agent_memory_usage() ->
    % Get active agents
    ActiveAgent_Ids = genotype_utils:get_active_agents(),
    case ActiveAgent_Ids of
        [] ->
            log_agent_memory_to_file("=== AGENT MEMORY USAGE === No active agents found");
        _ ->
            % Read and parse pid_map.log
            PidMapFile = filename:join([get_log_root_dir(), "Benchmarker", "pid_map.log"]),
            case file:read_file(PidMapFile) of
                {ok, Content} ->
                    Lines = string:tokens(binary_to_list(Content), "\n"),
                    % Parse lines and build map of Agent_Id -> {LatestTimestamp, PIDs}
                    AgentPidMap = parse_pid_map_log(Lines),
                    
                    % For each active agent, get PIDs and sum memory
                    AgentMemories = lists:foldl(fun(Agent_Id, Acc) ->
                        case maps:get(Agent_Id, AgentPidMap, undefined) of
                            undefined -> Acc;  % Agent not in log
                            {_Timestamp, Pids} ->
                                % Get memory for each PID and sum
                                TotalMemory = lists:foldl(fun(PidStr, Sum) ->
                                    try
                                        Pid = list_to_pid(string:strip(PidStr)),
                                        case erlang:process_info(Pid, memory) of
                                            {memory, Mem} -> Sum + Mem;
                                            undefined -> Sum  % Process dead
                                        end
                                    catch
                                        _:_ -> Sum  % Invalid PID string or dead process
                                    end
                                end, 0, Pids),
                                
                                [{Agent_Id, TotalMemory, length(Pids)} | Acc]
                        end
                    end, [], ActiveAgent_Ids),
                    
                    % Sort by memory (descending)
                    Sorted = lists:sort(fun({_, M1, _}, {_, M2, _}) -> M1 >= M2 end, AgentMemories),
                    
                    % Log to agent_memory.log
                    log_agent_memory_to_file("=== AGENT MEMORY USAGE (sorted by total memory) ==="),
                    lists:foreach(fun({Agent_Id, TotalMem, PidCount}) ->
                        AgentStr = lists:flatten(io_lib:format("~w", [Agent_Id])),
                        MemKB = TotalMem div 1024,
                        log_agent_memory_to_file(io_lib:format("AGENT:~s | TOTAL_MEMORY:~p bytes (~p KB) | PROCESS_COUNT:~p", 
                                                 [AgentStr, TotalMem, MemKB, PidCount]))
                    end, Sorted),
                    log_agent_memory_to_file("=== END AGENT MEMORY USAGE ===");
                {error, Reason} ->
                    log_agent_memory_to_file(io_lib:format("ERROR: Could not read pid_map.log: ~p", [Reason]))
            end
    end.

%% Helper: Log to agent_memory.log file in Benchmarker folder
log_agent_memory_to_file(Msg) ->
    Dir = filename:join(get_log_root_dir(), "Benchmarker"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "agent_memory.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    io:format(File, "~s | ~s~n", [Timestamp, Msg]),
    file:close(File).

%% Helper: Parse pid_map.log and return map of Agent_Id -> {Timestamp, PIDs}
parse_pid_map_log(Lines) ->
    % Parse each line and keep only the latest entry per agent
    lists:foldl(fun(Line, Acc) ->
        case parse_pid_map_line(Line) of
            {ok, {Agent_Id, Timestamp, Pids}} ->
                % Keep only latest entry per agent (by timestamp string comparison)
                case maps:get(Agent_Id, Acc, undefined) of
                    undefined -> maps:put(Agent_Id, {Timestamp, Pids}, Acc);
                    {ExistingTs, _} when Timestamp > ExistingTs -> 
                        maps:put(Agent_Id, {Timestamp, Pids}, Acc);
                    _ -> Acc  % Keep existing (older entry)
                end;
            error -> Acc
        end
    end, maps:new(), Lines).

%% Helper: Parse a single line from pid_map.log
parse_pid_map_line(Line) ->
    % Format: "[timestamp] | RUN:... | GEN:... | AGENT:{...} | PIDS:[<0.123.0>,<0.124.0>,...]"
    % Find positions directly in the line
    try
        AgentPos = string:str(Line, "AGENT:"),
        PidsPos = string:str(Line, "PIDS:"),
        if AgentPos > 0 andalso PidsPos > AgentPos ->
            % Extract agent ID: from "AGENT:" to " | PIDS:"
            AgentStart = AgentPos + 6,  % After "AGENT:"
            % Find where " | PIDS:" starts (4 chars before PidsPos)
            AgentEnd = PidsPos - 4,  % Position before " | PIDS:"
            AgentLength = AgentEnd - AgentStart + 1,
            if AgentLength > 0 ->
                AgentStr = string:substr(Line, AgentStart, AgentLength),
                % Extract PIDs: from "PIDS:[" to "]"
                PidsBracketPos = string:str(Line, "PIDS:[") + 6,  % After "PIDS:["
                PidsEndPos = string:rstr(Line, "]"),
                if PidsEndPos > PidsBracketPos ->
                    PidsLength = PidsEndPos - PidsBracketPos,
                    PidsStr = string:substr(Line, PidsBracketPos, PidsLength),
                    % Parse Agent_Id (format: {number,agent})
                    case parse_agent_id(string:strip(AgentStr)) of
                        undefined -> error;
                        Agent_Id ->
                            % Parse PIDs (comma-separated list like "<0.123.0>,<0.124.0>")
                            Pids = string:tokens(PidsStr, ","),
                            % Extract timestamp for comparison (first part before |)
                            Timestamp = extract_timestamp(Line),
                            {ok, {Agent_Id, Timestamp, Pids}}
                    end;
                true -> error
                end;
            true -> error
            end;
        true -> error
        end
    catch
        _:_ -> error
    end.

%% Helper: Parse agent ID from string representation
parse_agent_id(AgentStr) ->
    % AgentStr is like "{5.661944423739226e-10,agent}"
    % Use regex to extract number and atom, then construct tuple
    try
        case re:run(AgentStr, "\\{([0-9.eE+-]+),([a-z_]+)\\}", [{capture, all_but_first, list}]) of
            {match, [NumStr, AtomStr]} ->
                Num = list_to_float(NumStr),
                Atom = list_to_atom(AtomStr),
                {Num, Atom};
            _ -> 
                % Fallback: try erl_scan/erl_parse
                case erl_scan:string(AgentStr ++ ".") of
                    {ok, Tokens, _} ->
                        case erl_parse:parse_term(Tokens) of
                            {ok, Term} -> Term;
                            _ -> undefined
                        end;
                    _ -> undefined
                end
        end
    catch
        _:_ -> undefined
    end.

%% Helper: Extract timestamp from log line
extract_timestamp(Line) ->
    % Extract timestamp from "[2025-12-19 14:55:39]"
    case re:run(Line, "^\\[(.+?)\\]", [{capture, all_but_first, list}]) of
        {match, [TsStr]} -> TsStr;  % Use string comparison for timestamps
        _ -> ""
    end.

%% ============================================================================
%% EXP RUNNER LOGGING - Human-readable experiment run logging
%% ============================================================================

%% Exp Runner logging - human-readable, max ~5 lines per run
exp_runner(Event, Data) ->
    Dir = filename:join(get_log_root_dir(), "exp_runner"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "exp_runner.log"),
    {ok, File} = file:open(Filename, [append]),
    Timestamp = format_timestamp(),
    
    case Event of
        experiment_start ->
            {RunId, PopId, TotRuns, ConfigCount} = Data,
            io:format(File, "~n=== EXPERIMENT START ===~n", []),
            io:format(File, "[~s] | experiment: ~p | tot_runs: ~p~n",
                [Timestamp, RunId, TotRuns]);
            
        run_start ->
            {RunId, RunIndex, PopId, Mode, ConfigStr} = Data,
            % ConfigStr is pre-formatted by exp_runner
            io:format(File, "~n=== RUN ~p ===~n", [RunIndex]),
            io:format(File, "[~s] | population: ~p | mode: ~p~n",
                [Timestamp, PopId, Mode]),
            io:format(File, "config: ~s~n", [ConfigStr]);
            
        run_end ->
            {RunId, RunIndex, PopId, Trace} = Data,
            % Extract stats directly from trace and population (no record needed)
            TotEvals = Trace#trace.tot_evaluations,
            % Read population and collect agent stats
            P = genotype:dirty_read({population, PopId}),
            {AgentIds, Agents, Fitnesses, Generations} = case P of
                undefined ->
                    {[], [], [], []};
                _ ->
                    AIds = lists:flatten([
                        Specie#specie.agent_ids 
                        || SId <- P#population.specie_ids,
                           Specie <- [genotype:dirty_read({specie, SId})],
                           Specie =/= undefined
                    ]),
                    % Read all agents once and extract stats
                    Ags = [A || AId <- AIds, A <- [genotype:dirty_read({agent, AId})], A =/= undefined],
                    Fits = [A#agent.fitness || A <- Ags, A#agent.fitness =/= undefined],
                    Gens = [A#agent.generation || A <- Ags],
                    {AIds, Ags, Fits, Gens}
            end,
            BestFitness = case Fitnesses of [] -> 0.0; _ -> lists:max(Fitnesses) end,
            AvgFitness = case Fitnesses of [] -> 0.0; _ -> lists:sum(Fitnesses) / length(Fitnesses) end,
            MaxGen = case Generations of [] -> 0; _ -> lists:max(Generations) end,
            AgentCount = length(AgentIds),
            
            io:format(File, "[~s] | status: completed | best_fitness: ~.4f | avg_fitness: ~.4f | generations: ~p | tot_evaluations: ~p | agent_count: ~p~n~n",
                [Timestamp, BestFitness, AvgFitness, MaxGen, TotEvals, AgentCount]);
            
        generation_start ->
            {PopId, Generation, TotAgents} = Data,
            io:format(File, "[~s] | generation_start | generation: ~p | total_agents: ~p~n",
                [Timestamp, Generation, TotAgents]);
            
        generation_end ->
            {PopId, Generation, TotAgents} = Data,
            io:format(File, "[~s] | generation_end   | generation: ~p | total_agents: ~p~n",
                [Timestamp, Generation, TotAgents]);
            
        mutate_specie ->
            {Specie_Id, PopulationLimit, AvgFitness, MaxFitness, MinFitness, NeuralEnergyCost} = Data,
            io:format(File, "[~s] | mutate_specie | specie: ~p | population_limit: ~p | avg_fitness: ~.4f | max_fitness: ~.4f | min_fitness: ~.4f | neural_energy_cost: ~.4f~n",
                [Timestamp, Specie_Id, PopulationLimit, AvgFitness, MaxFitness, MinFitness, NeuralEnergyCost]);
            
        run_failed ->
            {RunId, RunIndex, Reason} = Data,
            io:format(File, "[~s] | status: failed | reason: ~p~n",
                [Timestamp, Reason]);
            
        config_loaded ->
            % Removed config_loaded logging - redundant with run_start config line
            ok;
            
        experiment_complete ->
            {RunId, TotRuns} = Data,
            io:format(File, "~n=== EXPERIMENT COMPLETE ===~n", []),
            io:format(File, "[~s] | experiment: ~p | total_runs: ~p~n",
                [Timestamp, RunId, TotRuns]);
            
        experiment_terminate ->
            {RunId, PopId} = Data,
            io:format(File, "[~s] | experiment: ~p | population: ~p | status: terminated~n",
                [Timestamp, RunId, PopId]);
            
        population_monitor_terminated ->
            {Population_Id, Reason, OpTag, OpMode, TotAgents, PopGen, TotEvals} = Data,
            io:format(File, "[~s] | population_monitor: ~p | status: terminated | reason: ~p | op_tag: ~p | op_mode: ~p | total_agents: ~p | generation: ~p | total_evaluations: ~p~n",
                [Timestamp, Population_Id, Reason, OpTag, OpMode, TotAgents, PopGen, TotEvals])
    end,
    file:close(File).

format_iso8601_for_log({MegaSecs, Secs, _MicroSecs}) ->
    % Convert Unix epoch to Gregorian seconds
    UnixEpoch = MegaSecs * 1000000 + Secs,
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    TotalSecs = GregorianEpoch + UnixEpoch,
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Y,Mo,D,H,Mi,S])).

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
%% xLog - Legacy per-agent logging function
%% Supports both per-agent logging (PID string) and system status (qStatus)
%% ============================================================================
xLog(PidString, Format, Args) when is_list(PidString) ->
    case PidString of
        "qStatus" ->
            write_status_log(io_lib:format(Format, Args));
        _ ->
            % Convert PID string back to PID: "<0.123.0>" -> <0.123.0>
            try
                ExoSelf_PId = list_to_pid(PidString),
                FormattedMsg = io_lib:format(Format, Args),
                write_log(ExoSelf_PId, lists:flatten(FormattedMsg))
            catch
                _:_ ->
                    % If conversion fails, write to status log with safe formatting
                    % Convert all Args to safe string representations
                    SafeArgs = [safe_format_arg(A) || A <- Args],
                    try
                        SafeFormat = "xLog ERROR: Invalid PID string ~s | " ++ Format,
                        write_status_log(io_lib:format(SafeFormat, [PidString | SafeArgs]))
                    catch
                        _:_ ->
                            % Ultimate fallback: minimal error message
                            write_status_log_safe("xLog ERROR: Failed to log message (PID: " ++ PidString ++ ")")
                    end
            end
    end;
xLog(qStatus, Format, Args) ->
    write_status_log(io_lib:format(Format, Args));
xLog(Other, Format, Args) ->
    % Convert Other and Args to safe string representations
    SafeOther = safe_format_arg(Other),
    SafeArgs = [safe_format_arg(A) || A <- Args],
    try
        write_status_log(io_lib:format("xLog ERROR: Invalid first arg ~s | " ++ Format, [SafeOther | SafeArgs]))
    catch
        _:_ ->
            write_status_log_safe("xLog ERROR: Failed to log message (Invalid first arg)")
    end.

%% ============================================================================
%% HELPER
%% ============================================================================

register_agent(ExoSelf_PId, Agent_Id) ->
    init_ets(),
    ets:insert(?AGENT_PID_MAP, {ExoSelf_PId, Agent_Id}).

write_log(ExoSelf_PId, Msg) ->
    LogFile = get_logfile(ExoSelf_PId),
    {ok, F} = file:open(LogFile, [append]),
    Timestamp = format_timestamp(),
    io:format(F, "~s ~s~n", [Timestamp, Msg]),
    file:close(F).

get_logfile(ExoSelf_PId) ->
    init_ets(),
    case ets:lookup(?AGENT_PID_MAP, ExoSelf_PId) of
        [{ExoSelf_PId, Agent_Id}] ->
            get_agent_logfile(Agent_Id);
        [] ->
            PidStr = pid_to_list(ExoSelf_PId),
            CleanPid = lists:filter(fun(C) -> C =/= $< andalso C =/= $> end, PidStr),
            filelib:ensure_dir("logs/Agents/Exoself/"),
            lists:flatten(io_lib:format("logs/Agents/Exoself/~s.log", [CleanPid]))
    end.

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
    IdStr = lists:flatten(io_lib:format("~p", [Agent_Id])),
    CleanId = lists:filter(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) orelse C == $_ orelse C == $- end, IdStr),
    Dir = filename:join(get_log_root_dir(), "agents"),
    ensure_directory_exists(Dir),
    filename:join(Dir, lists:flatten(io_lib:format("~s.log", [CleanId]))).

format_timestamp() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("[~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B]",
        [Y,Mo,D,H,Mi,S])).

write_status_log(Msg) ->
    Dir = filename:join(get_log_root_dir(), "System"),
    ensure_directory_exists(Dir),
    Filename = filename:join(Dir, "qStatus.log"),
    case file:open(Filename, [append]) of
        {ok, F} ->
            try
                Timestamp = format_timestamp(),
                % Safely flatten and convert Msg to string
                FlatMsg = case is_list(Msg) of
                    true ->
                        try
                            lists:flatten(io_lib:format("~s", [Msg]))
                        catch
                            _:_ ->
                                % If flatten fails, try to format as term
                                lists:flatten(io_lib:format("~p", [Msg]))
                        end;
                    false ->
                        lists:flatten(io_lib:format("~p", [Msg]))
                end,
                io:format(F, "~s | ~s~n", [Timestamp, FlatMsg]),
                file:close(F)
            catch
                Error:Reason ->
                    file:close(F),
                    % Fallback: try to log the error to stderr
                    try
                        io:format(standard_error, "qlog:write_status_log error: ~p:~p~n", [Error, Reason])
                    catch
                        _:_ -> ok
                    end
            end;
        {error, Reason} ->
            % If file open fails, try to log to stderr
            try
                io:format(standard_error, "qlog:write_status_log failed to open file: ~p~n", [Reason])
            catch
                _:_ -> ok
            end
    end.

%% Safe fallback function for critical error logging
write_status_log_safe(Msg) when is_list(Msg) ->
    try
        Dir = filename:join(get_log_root_dir(), "System"),
        ensure_directory_exists(Dir),
        Filename = filename:join(Dir, "qStatus.log"),
        {ok, F} = file:open(Filename, [append]),
        Timestamp = format_timestamp(),
        io:format(F, "~s | ~s~n", [Timestamp, Msg]),
        file:close(F)
    catch
        _:_ -> ok  % Silently fail if even this doesn't work
    end;
write_status_log_safe(_) -> ok.

%% Helper function to safely convert arguments to string format
safe_format_arg(Arg) ->
    try
        case Arg of
            P when is_pid(P) -> pid_to_list(P);
            A when is_atom(A) -> atom_to_list(A);
            T when is_tuple(T) -> lists:flatten(io_lib:format("~p", [T]));
            L when is_list(L) ->
                case io_lib:printable_list(L) of
                    true -> L;
                    false -> lists:flatten(io_lib:format("~p", [L]))
                end;
            _ -> lists:flatten(io_lib:format("~p", [Arg]))
        end
    catch
        _:_ -> "<unprintable>"
    end.

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
