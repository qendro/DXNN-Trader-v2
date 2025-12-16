%% Utility functions for working with genotypes in DXNN
-module(genotype_utils).
-compile(export_all).
-include("records.hrl").

%% Find and print the best genotype from the Mnesia database
print_best_genotype() ->
    print_best_genotype(test).

print_best_genotype(Population_Id) ->
    F = fun() ->
        % Get all agent keys from the agent table
        Agent_Keys = mnesia:dirty_all_keys(agent),
        io:format("Found ~p agents in the database~n", [length(Agent_Keys)]),
        
        % Filter agents by population if specified
        Filtered_Agents = case Population_Id of
            all ->
                Agent_Keys;
            _ ->
                [Agent_Id || Agent_Id <- Agent_Keys, 
                 case mnesia:dirty_read({agent, Agent_Id}) of
                     [] -> false;
                     [Agent] -> Agent#agent.population_id == Population_Id
                 end]
        end,
        
        io:format("Found ~p agents in population ~p~n", [length(Filtered_Agents), Population_Id]),
        
        % Find the best agent (highest fitness)
        case Filtered_Agents of
            [] ->
                io:format("No agents found in population ~p~n", [Population_Id]);
            _ ->
                Best_Agent = lists:foldl(fun(Agent_Id, Best_Acc) ->
                    case mnesia:dirty_read({agent, Agent_Id}) of
                        [] -> Best_Acc;
                        [Agent] ->
                            case Best_Acc of
                                undefined -> Agent;
                                Best when Agent#agent.fitness > Best#agent.fitness -> Agent;
                                _ -> Best_Acc
                            end
                    end
                end, undefined, Filtered_Agents),
                
                case Best_Agent of
                    undefined ->
                        io:format("Could not find best agent~n");
                    Agent ->
                        io:format("Best agent found: ~p~n", [Agent#agent.id]),
                        io:format("Fitness: ~p~n", [Agent#agent.fitness]),
                        io:format("Generation: ~p~n", [Agent#agent.generation]),
                        io:format("Specie ID: ~p~n", [Agent#agent.specie_id]),
                        io:format("~n--- Complete Genotype ---~n"),
                        genotype:print(Agent#agent.id)
                end
        end
    end,
    mnesia:transaction(F).

%% List all agents with their fitness values
list_all_agents() ->
    list_all_agents(test).

list_all_agents(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        io:format("=== All Agents in Population ~p ===~n", [Population_Id]),
        
        Agents = lists:foldl(fun(Agent_Id, Acc) ->
            case mnesia:dirty_read({agent, Agent_Id}) of
                [] -> Acc;
                [Agent] when Population_Id == all ->
                    [{Agent#agent.id, Agent#agent.fitness, Agent#agent.generation, Agent#agent.specie_id} | Acc];
                [Agent] when Agent#agent.population_id == Population_Id ->
                    [{Agent#agent.id, Agent#agent.fitness, Agent#agent.generation, Agent#agent.specie_id} | Acc];
                _ -> Acc
            end
        end, [], Agent_Keys),
        
        % Sort by fitness (descending)
        Sorted_Agents = lists:sort(fun({_, F1, _, _}, {_, F2, _, _}) -> F1 >= F2 end, Agents),
        
        io:format("Found ~p agents:~n", [length(Sorted_Agents)]),
        lists:foreach(fun({Agent_Id, Fitness, Generation, Specie_Id}) ->
            io:format("Agent: ~p, Fitness: ~p, Generation: ~p, Specie: ~p~n", 
                     [Agent_Id, Fitness, Generation, Specie_Id])
        end, Sorted_Agents),
        
        Sorted_Agents
    end,
    mnesia:transaction(F).

%% Print the top N agents
print_top_agents(N) ->
    print_top_agents(N, test).

print_top_agents(N, Population_Id) ->
    {atomic, Agents} = list_all_agents(Population_Id),
    Top_Agents = lists:sublist(Agents, N),
    
    io:format("~n=== Top ~p Agents ===~n", [N]),
    lists:foreach(fun({Agent_Id, Fitness, Generation, Specie_Id}) ->
        io:format("~nAgent: ~p (Fitness: ~p, Generation: ~p, Specie: ~p)~n", 
                 [Agent_Id, Fitness, Generation, Specie_Id]),
        io:format("--- Genotype ---~n"),
        genotype:print(Agent_Id)
    end, Top_Agents).

%% Get agent statistics
get_agent_stats() ->
    get_agent_stats(test).

get_agent_stats(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        
        Agents = lists:foldl(fun(Agent_Id, Acc) ->
            case mnesia:dirty_read({agent, Agent_Id}) of
                [] -> Acc;
                [Agent] when Population_Id == all ->
                    [Agent | Acc];
                [Agent] when Agent#agent.population_id == Population_Id ->
                    [Agent | Acc];
                _ -> Acc
            end
        end, [], Agent_Keys),
        
        case Agents of
            [] ->
                io:format("No agents found in population ~p~n", [Population_Id]);
            _ ->
                Fitnesses = [Agent#agent.fitness || Agent <- Agents],
                Generations = [Agent#agent.generation || Agent <- Agents],
                
                Max_Fitness = lists:max(Fitnesses),
                Min_Fitness = lists:min(Fitnesses),
                Avg_Fitness = lists:sum(Fitnesses) / length(Fitnesses),
                
                Max_Generation = lists:max(Generations),
                Min_Generation = lists:min(Generations),
                Avg_Generation = lists:sum(Generations) / length(Generations),
                
                io:format("=== Agent Statistics for Population ~p ===~n", [Population_Id]),
                io:format("Total Agents: ~p~n", [length(Agents)]),
                io:format("Fitness - Max: ~p, Min: ~p, Avg: ~.2f~n", [Max_Fitness, Min_Fitness, Avg_Fitness]),
                io:format("Generation - Max: ~p, Min: ~p, Avg: ~.2f~n", [Max_Generation, Min_Generation, Avg_Generation]),
                
                {length(Agents), Max_Fitness, Min_Fitness, Avg_Fitness, Max_Generation, Min_Generation, Avg_Generation}
        end
    end,
    mnesia:transaction(F).
%% Return the best agent id (by fitness)
find_best_agent() -> find_best_agent(test).

find_best_agent(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        Filtered = case Population_Id of
            all -> Agent_Keys;
            _ -> [Agent_Id || Agent_Id <- Agent_Keys,
                   case mnesia:dirty_read({agent, Agent_Id}) of
                       [] -> false;
                       [Agent] -> Agent#agent.population_id == Population_Id
                   end]
        end,
        case Filtered of
            [] -> undefined;
            _ ->
                Best = lists:foldl(
                    fun(Agent_Id, Acc) ->
                        case mnesia:dirty_read({agent, Agent_Id}) of
                            [] -> Acc;
                            [Agent] ->
                                case Acc of
                                    undefined -> Agent;
                                    BestSoFar when Agent#agent.fitness > BestSoFar#agent.fitness -> Agent;
                                    _ -> Acc
                                end
                        end
                    end, undefined, Filtered),
                case Best of
                    undefined -> undefined;
                    Agent -> Agent#agent.id
                end
        end
    end,
    mnesia:transaction(F).

%% Get list of active agents from the population monitor
get_active_agents() ->
    case whereis(monitor) of
        undefined -> 
            io:format("Monitor not running~n"),
            [];
        MonitorPid -> 
            case catch gen_server:call(MonitorPid, get_active_agents) of
                {active_agents, ActiveAgent_IdPs, AgentsLeft, TotAgents} ->
                    io:format("Active Agents: ~p/~p (Left: ~p)~n", [length(ActiveAgent_IdPs), TotAgents, AgentsLeft]),
                    [Agent_Id || {Agent_Id, _PId} <- ActiveAgent_IdPs];
                {'EXIT', _} ->
                    io:format("Error querying monitor~n"),
                    [];
                Other ->
                    io:format("Unexpected response: ~p~n", [Other]),
                    []
            end
    end.

%% Get total number of agents in the database
get_total_agents() ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        Total = length(Agent_Keys),
        io:format("Total agents in database: ~p~n", [Total]),
        Total
    end,
    mnesia:transaction(F).

%% Get total number of agents by population ID
get_total_agents_by_population(Population_Id) ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        Filtered = case Population_Id of
            all -> Agent_Keys;
            _ -> [Agent_Id || Agent_Id <- Agent_Keys,
                   case mnesia:dirty_read({agent, Agent_Id}) of
                       [] -> false;
                       [Agent] -> Agent#agent.population_id == Population_Id
                   end]
        end,
        Total = length(Filtered),
        io:format("Total agents in population ~p: ~p~n", [Population_Id, Total]),
        Total
    end,
    mnesia:transaction(F).

%% List all agents grouped by population ID
list_agents_by_population() ->
    F = fun() ->
        Agent_Keys = mnesia:dirty_all_keys(agent),
        
        % Group agents by population_id
        Agents_By_Pop = lists:foldl(fun(Agent_Id, Acc) ->
            case mnesia:dirty_read({agent, Agent_Id}) of
                [] -> Acc;
                [Agent] ->
                    Pop_Id = Agent#agent.population_id,
                    case lists:keyfind(Pop_Id, 1, Acc) of
                        false ->
                            [{Pop_Id, [Agent#agent.id]} | Acc];
                        {Pop_Id, Agent_List} ->
                            lists:keyreplace(Pop_Id, 1, Acc, {Pop_Id, [Agent#agent.id | Agent_List]})
                    end
            end
        end, [], Agent_Keys),
        
        % Sort by population ID and print
        Sorted = lists:keysort(1, Agents_By_Pop),
        io:format("~n=== Agents by Population ===~n"),
        lists:foreach(fun({Pop_Id, Agent_List}) ->
            io:format("Population ~p: ~p agents~n", [Pop_Id, length(Agent_List)]),
            lists:foreach(fun(Agent_Id) ->
                io:format("  - ~p~n", [Agent_Id])
            end, Agent_List)
        end, Sorted),
        
        Sorted
    end,
    mnesia:transaction(F).

%% Print the genotype of all active agents
print_active_agent_genotypes() ->
    case whereis(monitor) of
        undefined -> 
            io:format("Monitor not running~n");
        MonitorPid -> 
            case catch gen_server:call(MonitorPid, get_active_agents) of
                {active_agents, ActiveAgent_IdPs, AgentsLeft, TotAgents} ->
                    io:format("=== Active Agent Genotypes ===~n"),
                    io:format("Active Agents: ~p/~p (Left: ~p)~n", [length(ActiveAgent_IdPs), TotAgents, AgentsLeft]),
                    lists:foreach(fun({Agent_Id, _PId}) ->
                        io:format("~n--- Agent: ~p ---~n", [Agent_Id]),
                        genotype:print(Agent_Id)
                    end, ActiveAgent_IdPs);
                {'EXIT', Reason} ->
                    io:format("Error querying monitor: ~p~n", [Reason]);
                Other ->
                    io:format("Unexpected response: ~p~n", [Other])
            end
    end.

%% Check active agents and identify if any processes are dead
%% Can be called with a single agent ID: active_agents_process_check({5.668390814050843e-10,agent})
%% Or without arguments to check all active agents: active_agents_process_check()
active_agents_process_check() ->
    active_agents_process_check(all).

active_agents_process_check(Agent_Id) when is_tuple(Agent_Id) ->
    % Single agent check - check all processes
    case whereis(monitor) of
        undefined -> 
            io:format("Monitor not running~n"),
            [];
        MonitorPid -> 
            case catch gen_server:call(MonitorPid, get_active_agents) of
                {active_agents, ActiveAgent_IdPs, _AgentsLeft, _TotAgents} ->
                    case lists:keyfind(Agent_Id, 1, ActiveAgent_IdPs) of
                        false ->
                            io:format("Agent ~p is not in the active agents list~n", [Agent_Id]),
                            [];
                        {Agent_Id, Agent_PId} ->
                            Result = check_all_agent_processes(Agent_Id, Agent_PId),
                            [Result]
                    end;
                {'EXIT', Reason} ->
                    io:format("Error querying monitor: ~p~n", [Reason]),
                    [];
                Other ->
                    io:format("Unexpected response: ~p~n", [Other]),
                    []
            end
    end;
active_agents_process_check(all) ->
    % Check all active agents - check all processes for each
    case whereis(monitor) of
        undefined -> 
            io:format("Monitor not running~n"),
            [];
        MonitorPid -> 
            case catch gen_server:call(MonitorPid, get_active_agents) of
                {active_agents, ActiveAgent_IdPs, AgentsLeft, TotAgents} ->
                    io:format("=== Active Agents Process Check (All Processes) ===~n"),
                    io:format("Total Active Agents: ~p/~p (Left: ~p)~n", [length(ActiveAgent_IdPs), TotAgents, AgentsLeft]),
                    Results = lists:map(fun({Agent_Id, Agent_PId}) ->
                        check_all_agent_processes(Agent_Id, Agent_PId)
                    end, ActiveAgent_IdPs),
                    % Extract dead processes from all agents
                    All_Dead_Processes = lists:foldl(fun({_Agent_Id, _All_Results, Dead_Procs, _Alive_Procs, _Expected}, Acc) ->
                        Acc ++ Dead_Procs
                    end, [], Results),
                    All_Alive_Processes = lists:foldl(fun({_Agent_Id, _All_Results, _Dead_Procs, Alive_Procs, _Expected}, Acc) ->
                        Acc ++ Alive_Procs
                    end, [], Results),
                    Total_Expected = lists:foldl(fun({_Agent_Id, _All_Results, _Dead_Procs, _Alive_Procs, Expected}, Acc) ->
                        Acc + Expected
                    end, 0, Results),
                    io:format("~n=== Overall Summary ===~n"),
                    io:format("Processes found: ~p (Expected: ~p)~n", [length(All_Dead_Processes) + length(All_Alive_Processes), Total_Expected]),
                    io:format("Alive: ~p, Dead: ~p~n", [length(All_Alive_Processes), length(All_Dead_Processes)]),
                    case All_Dead_Processes of
                        [] ->
                            io:format("All processes are alive~n");
                        _ ->
                            io:format("~nDead processes across all agents:~n"),
                            lists:foreach(fun({Type, Pid, _}) ->
                                io:format("  ~s (~p)~n", [Type, Pid])
                            end, All_Dead_Processes)
                    end,
                    Results;
                {'EXIT', Reason} ->
                    io:format("Error querying monitor: ~p~n", [Reason]),
                    [];
                Other ->
                    io:format("Unexpected response: ~p~n", [Other]),
                    []
            end
    end.

%% Helper function to check if an agent's process is alive
check_agent_process(Agent_Id, Agent_PId) ->
    Is_Alive = is_process_alive(Agent_PId),
    Status = case Is_Alive of
        true -> alive;
        false -> dead
    end,
    io:format("Agent ~p (PID: ~p): ~p~n", [Agent_Id, Agent_PId, Status]),
    {Agent_Id, Status, Agent_PId}.

%% Check all processes for an agent (exoself, cortex, neurons, sensors, actuators, substrate, etc.)
%% 
%% NOTE: This function can only verify exoself and any linked/monitored processes.
%% To check all child processes (cortex, neurons, sensors, etc.), exoself needs to be modified
%% to handle {From, get_process_list} messages and return {ExoSelf_PId, process_list, [AllPids]}.
%% The child processes are stored in exoself's state but are not accessible from outside.
check_all_agent_processes(Agent_Id, ExoSelf_PId) ->
    io:format("~n=== Checking all processes for Agent ~p ===~n", [Agent_Id]),
    
    % Check exoself
    ExoSelf_Alive = is_process_alive(ExoSelf_PId),
    io:format("ExoSelf (~p): ~p~n", [ExoSelf_PId, case ExoSelf_Alive of true -> alive; false -> dead end]),
    
    Process_Results = [{exoself, ExoSelf_PId, ExoSelf_Alive}],
    
    % Read genotype to get expected structure and try to find processes
    Genotype_Info = case ExoSelf_Alive of
        true ->
            try
                F = fun() ->
                    case mnesia:dirty_read({agent, Agent_Id}) of
                        [] -> undefined;
                        [Agent] ->
                            Cx = mnesia:dirty_read({cortex, Agent#agent.cx_id}),
                            case Cx of
                                [] -> undefined;
                                [Cortex] ->
                                    Substrate_Info = case Agent#agent.substrate_id of
                                        undefined -> undefined;
                                        Sub_Id ->
                                            case mnesia:dirty_read({substrate, Sub_Id}) of
                                                [] -> undefined;
                                                [Sub] -> Sub
                                            end
                                    end,
                                    {Agent, Cortex, Substrate_Info}
                            end
                    end
                end,
                mnesia:transaction(F)
            catch
                _ -> undefined
            end;
        false -> undefined
    end,
    
    % Try to get linked processes from exoself
    Linked_Pids = case ExoSelf_Alive of
        true ->
            case catch erlang:process_info(ExoSelf_PId, links) of
                {links, Links} -> 
                    % Filter out self and parent (monitor)
                    [Pid || Pid <- Links, Pid =/= ExoSelf_PId, Pid =/= self()];
                _ -> []
            end;
        false -> []
    end,
    
    % Try to get monitored processes
    Monitored_Pids = case ExoSelf_Alive of
        true ->
            case catch erlang:process_info(ExoSelf_PId, monitors) of
                {monitors, Monitors} -> [Pid || {process, Pid} <- Monitors];
                _ -> []
            end;
        false -> []
    end,
    
    % Try sending a message to exoself to get process list (with timeout)
    % Note: This will timeout since exoself doesn't have a handler for this message.
    % To enable full process checking, exoself.erl needs to be modified to handle
    % {From, get_process_list} in its loop functions and return all PIDs from state.
    Requested_Pids = case ExoSelf_Alive of
        true ->
            ExoSelf_PId ! {self(), get_process_list},
            receive
                {ExoSelf_PId, process_list, Pids} ->
                    io:format("Received process list from exoself: ~p processes~n", [length(Pids)]),
                    Pids
            after
                100 ->  % 100ms timeout
                    % Expected - exoself doesn't have this handler
                    []
            end;
        false -> []
    end,
    
    % Note: There's no existing record or map that stores all child process PIDs.
    % They're only stored in exoself's internal state record, which we can't access
    % without modifying exoself.erl to add a handler for get_process_list.
    Discovered_Pids = [],
    
    
    % Check linked processes
    Linked_Results = lists:map(fun(Pid) ->
        Alive = is_process_alive(Pid),
        io:format("Linked Process (~p): ~p~n", [Pid, case Alive of true -> alive; false -> dead end]),
        {linked, Pid, Alive}
    end, Linked_Pids),
    
    % Check monitored processes
    Monitored_Results = lists:map(fun(Pid) ->
        Alive = is_process_alive(Pid),
        io:format("Monitored Process (~p): ~p~n", [Pid, case Alive of true -> alive; false -> dead end]),
        {monitored, Pid, Alive}
    end, Monitored_Pids),
    
    % Try to get process info to identify process types
    All_Processes = lists:usort(Linked_Pids ++ Monitored_Pids ++ Requested_Pids ++ Discovered_Pids),
    Detailed_Results = lists:map(fun(Pid) ->
        case catch erlang:process_info(Pid, [current_function, initial_call, registered_name]) of
            Process_Info when is_list(Process_Info) ->
                Fun = proplists:get_value(current_function, Process_Info, unknown),
                Init = proplists:get_value(initial_call, Process_Info, unknown),
                Reg = proplists:get_value(registered_name, Process_Info, undefined),
                Alive = is_process_alive(Pid),
                Type = identify_process_type(Fun, Init, Reg),
                io:format("  ~s (~p): ~p~n", [Type, Pid, case Alive of true -> alive; false -> dead end]),
                {Type, Pid, Alive};
            _ ->
                Alive = is_process_alive(Pid),
                io:format("  Unknown Process (~p): ~p~n", [Pid, case Alive of true -> alive; false -> dead end]),
                {unknown, Pid, Alive}
        end
    end, All_Processes),
    
    % Print genotype-based expected structure if available and calculate expected count
    Expected_Count = case Genotype_Info of
        {atomic, {_Agent, Cortex, Substrate_Info}} ->
            % Print expected structure
            io:format("~nExpected structure from genotype:~n"),
            io:format("  Sensors: ~p~n", [length(Cortex#cortex.sensor_ids)]),
            io:format("  Neurons: ~p~n", [length(Cortex#cortex.neuron_ids)]),
            io:format("  Actuators: ~p~n", [length(Cortex#cortex.actuator_ids)]),
            Base_Count = 1 + % exoself
                         1 + % cortex
                         length(Cortex#cortex.sensor_ids) +
                         length(Cortex#cortex.neuron_ids) +
                         length(Cortex#cortex.actuator_ids),
            Final_Count = case Substrate_Info of
                undefined -> 
                    Base_Count;
                Substrate ->
                    io:format("  Substrate: 1~n"),
                    io:format("  CPPs: ~p~n", [length(Substrate#substrate.cpp_ids)]),
                    io:format("  CEPs: ~p~n", [length(Substrate#substrate.cep_ids)]),
                    Base_Count + 1 + length(Substrate#substrate.cpp_ids) + length(Substrate#substrate.cep_ids)
            end,
            Final_Count;
        _ -> 
            1  % Only exoself
    end,
    
    % Summary
    All_Results = Process_Results ++ Linked_Results ++ Monitored_Results ++ Detailed_Results,
    Dead_Processes = [R || R <- All_Results, element(3, R) == false],
    Alive_Processes = [R || R <- All_Results, element(3, R) == true],
    
    io:format("~n=== Summary for Agent ~p ===~n", [Agent_Id]),
    io:format("Processes found: ~p (Expected: ~p)~n", [length(All_Results), Expected_Count]),
    io:format("Alive: ~p, Dead: ~p~n", [length(Alive_Processes), length(Dead_Processes)]),
    
    if
        length(All_Results) < Expected_Count ->
            io:format("~nWARNING: Only found ~p processes, but expected ~p~n", [length(All_Results), Expected_Count]),
            io:format("~nLIMITATION: Child process PIDs (cortex, neurons, sensors, actuators, substrate, etc.)~n"),
            io:format("are stored in exoself's internal state record but are not accessible from outside.~n"),
            io:format("There is no existing record or map that stores all PIDs for each agent.~n"),
            io:format("~nTo enable full process checking, modify exoself.erl to add a handler in all loop functions:~n"),
            io:format("  {From, get_process_list} ->~n"),
            io:format("      All_Pids = [S#state.cx_pid | S#state.spids] ++ S#state.npids ++ ...~n"),
            io:format("      From ! {self(), process_list, All_Pids},~n"),
            io:format("      exoself:loop(S, OpMode);~n");
        true -> ok
    end,
    
    case Dead_Processes of
        [] ->
            if
                length(All_Results) >= Expected_Count ->
                    io:format("All processes are alive~n");
                true ->
                    io:format("ExoSelf is alive (child processes cannot be verified without exoself modification)~n")
            end;
        _ ->
            io:format("~nDead processes:~n"),
            lists:foreach(fun({Type, Pid, _}) ->
                io:format("  ~s (~p)~n", [Type, Pid])
            end, Dead_Processes)
    end,
    
    {Agent_Id, All_Results, Dead_Processes, Alive_Processes, Expected_Count}.


%% Helper to identify process type from process info
identify_process_type(Fun, Init, Reg) ->
    case Reg of
        _ when Reg =/= undefined -> 
            atom_to_list(Reg);
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
                {M, F, _} when is_atom(M), is_atom(F) ->
                    atom_to_list(M);
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
                        {M, F, _} when is_atom(M), is_atom(F) ->
                            atom_to_list(M);
                        _ -> "Unknown"
                    end
            end
    end.
