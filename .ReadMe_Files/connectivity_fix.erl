-module(connectivity_fix).
-compile(export_all).
-include("records.hrl").

-define(MAX_FIX_ATTEMPTS, 3).

fix_connectivity_if_needed(Agent_Id) ->
    case has_valid_connectivity(Agent_Id) of
        true -> ok;
        {false, Reason} ->
            qlog:xLog(qStatus, "Agent ~p connectivity issue: ~p", [Agent_Id, Reason]),
            case try_fix_and_verify(Agent_Id, Reason) of
                ok -> 
                    qlog:xLog(qStatus, "Agent ~p connectivity fixed successfully", [Agent_Id]),
                    ok;
                retry ->
                    qlog:xLog(qStatus, "Agent ~p connectivity fix FAILED after max attempts", [Agent_Id]),
                    error("********ERROR:apply_Mutators:: Connectivity fix failed after all mutations completed.")
            end
    end.

has_valid_connectivity(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    case A#agent.encoding_type of
        neural -> has_neural_path(Agent_Id, Cx, A);
        substrate ->
            Substrate_Id = A#agent.substrate_id,
            Substrate = genotype:read({substrate,Substrate_Id}),
            case has_substrate_main_path(Agent_Id, Cx, Substrate_Id) of
                true -> has_substrate_weight_path(Agent_Id, Substrate);
                {false, Reason} -> {false, Reason}
            end
    end.

has_neural_path(Agent_Id, Cx, A) ->
    S_Ids = Cx#cortex.sensor_ids,
    A_Ids = Cx#cortex.actuator_ids,
    Constraint = A#agent.constraint,
    ConnectionArch = Constraint#constraint.connection_architecture,
    case verify_sensor_connections_bidirectional(Agent_Id, S_Ids) of
        false -> {false, bidirectional_mismatch};
        true ->
            case lists:any(fun(S_Id) ->
                lists:any(fun(A_Id) ->
                    has_path_bfs(Agent_Id, S_Id, A_Id, [S_Id], [S_Id], ConnectionArch)
                end, A_Ids)
            end, S_Ids) of
                true -> true;
                false -> {false, no_path}
            end
    end.

has_substrate_main_path(_Agent_Id, Cx, Substrate_Id) ->
    S_Ids = Cx#cortex.sensor_ids,
    A_Ids = Cx#cortex.actuator_ids,
    Sensors_Connected = lists:any(fun(S_Id) ->
        S = genotype:read({sensor,S_Id}),
        lists:member(Substrate_Id, S#sensor.fanout_ids)
    end, S_Ids),
    Actuators_Connected = lists:any(fun(A_Id) ->
        A = genotype:read({actuator,A_Id}),
        lists:member(Substrate_Id, A#actuator.fanin_ids)
    end, A_Ids),
    case Sensors_Connected andalso Actuators_Connected of
        true -> true;
        false -> {false, substrate_main_path_broken}
    end.

has_substrate_weight_path(Agent_Id, Substrate) ->
    CPP_Ids = Substrate#substrate.cpp_ids,
    CEP_Ids = Substrate#substrate.cep_ids,
    A = genotype:read({agent,Agent_Id}),
    Constraint = A#agent.constraint,
    ConnectionArch = Constraint#constraint.connection_architecture,
    case verify_cpp_connections_bidirectional(Agent_Id, CPP_Ids) of
        false -> {false, cpp_bidirectional_mismatch};
        true ->
            case lists:any(fun(CPP_Id) ->
                lists:any(fun(CEP_Id) ->
                    has_path_bfs(Agent_Id, CPP_Id, CEP_Id, [CPP_Id], [CPP_Id], ConnectionArch)
                end, CEP_Ids)
            end, CPP_Ids) of
                true -> true;
                false -> {false, cpp_to_cep_no_path}
            end
    end.

verify_sensor_connections_bidirectional(_Agent_Id, S_Ids) ->
    lists:all(fun(S_Id) ->
        S = genotype:read({sensor,S_Id}),
        Fanout_Ids = S#sensor.fanout_ids,
        Neuron_Connections = [FId || FId <- Fanout_Ids, case FId of {_,neuron} -> true; _ -> false end],
        case Neuron_Connections of
            [] -> true;
            _ ->
                lists:all(fun(Fanout_Id) ->
                    case Fanout_Id of
                        {_,neuron} ->
                            N = genotype:read({neuron,Fanout_Id}),
                            {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                            lists:member(S_Id, Input_Ids);
                        {_,substrate} -> true;
                        _ -> false
                    end
                end, Fanout_Ids)
        end
    end, S_Ids).

verify_cpp_connections_bidirectional(_Agent_Id, CPP_Ids) ->
    lists:all(fun(CPP_Id) ->
        CPP = genotype:read({sensor,CPP_Id}),
        Fanout_Ids = CPP#sensor.fanout_ids,
        lists:all(fun(N_Id) ->
            case N_Id of
                {_,neuron} ->
                    N = genotype:read({neuron,N_Id}),
                    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                    lists:member(CPP_Id, Input_Ids);
                _ -> false
            end
        end, Fanout_Ids)
    end, CPP_Ids).

has_path_bfs(_Agent_Id, Target_Id, Target_Id, _Visited, _Queue, _ConnectionArch) -> true;
has_path_bfs(_Agent_Id, _From_Id, _Target_Id, _Visited, [], _ConnectionArch) -> false;
has_path_bfs(Agent_Id, From_Id, Target_Id, Visited, [Current_Id|Queue], ConnectionArch) ->
    case Current_Id of
        Target_Id -> true;
        _ ->
            Neighbors = get_neighbors(Agent_Id, Current_Id, ConnectionArch),
            New_Neighbors = [N || N <- Neighbors, not lists:member(N, Visited)],
            New_Queue = Queue ++ New_Neighbors,
            New_Visited = Visited ++ New_Neighbors,
            has_path_bfs(Agent_Id, From_Id, Target_Id, New_Visited, New_Queue, ConnectionArch)
    end.

get_neighbors(Agent_Id, Element_Id, ConnectionArch) ->
    case Element_Id of
        {_,sensor} ->
            S = genotype:read({sensor,Element_Id}),
            S#sensor.fanout_ids;
        {_,neuron} ->
            N = genotype:read({neuron,Element_Id}),
            case ConnectionArch of
                feedforward ->
                    {{Current_LI,_},neuron} = Element_Id,
                    lists:filter(fun(Out_Id) ->
                        case Out_Id of
                            {{Out_LI,_},neuron} -> Out_LI > Current_LI;
                            {_,actuator} -> true;
                            _ -> true
                        end
                    end, N#neuron.output_ids);
                recurrent -> N#neuron.output_ids
            end;
        {_,actuator} -> [];
        {_,substrate} ->
            A = genotype:read({agent,Agent_Id}),
            Cx_Id = A#agent.cx_id,
            Cx = genotype:read({cortex,Cx_Id}),
            Cx#cortex.actuator_ids
    end.

try_fix_and_verify(Agent_Id, Reason) ->
    try_fix_and_verify(Agent_Id, Reason, ?MAX_FIX_ATTEMPTS).

try_fix_and_verify(_Agent_Id, _Reason, 0) -> retry;
try_fix_and_verify(Agent_Id, Reason, Attempts) ->
    case Reason of
        no_path -> fix_neural_path(Agent_Id);
        substrate_main_path_broken -> fix_substrate_main_path(Agent_Id);
        cpp_to_cep_no_path -> fix_cpp_cep_path(Agent_Id);
        bidirectional_mismatch -> fix_bidirectional_mismatch(Agent_Id);
        cpp_bidirectional_mismatch -> fix_cpp_bidirectional_mismatch(Agent_Id);
        _ -> ok
    end,
    case has_valid_connectivity(Agent_Id) of
        true -> ok;
        {false, NewReason} -> try_fix_and_verify(Agent_Id, NewReason, Attempts - 1)
    end.

fix_neural_path(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Generation = A#agent.generation,
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    S_Ids = Cx#cortex.sensor_ids,
    A_Ids = Cx#cortex.actuator_ids,
    N_Ids = Cx#cortex.neuron_ids,
    case {S_Ids, N_Ids, A_Ids} of
        {[], _, _} -> ok;
        {_, [], _} -> ok;
        {_, _, []} -> ok;
        _ ->
            S_Id = pick_random(S_Ids),
            N_Id = pick_random(N_Ids),
            A_Id = pick_random(A_Ids),
            link_sensor_to_neuron_if_absent(Agent_Id, Generation, S_Id, N_Id),
            link_neuron_to_actuator_if_absent(Agent_Id, Generation, N_Id, A_Id),
            ok
    end.

fix_substrate_main_path(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    Substrate_Id = A#agent.substrate_id,
    S_Ids = Cx#cortex.sensor_ids,
    A_Ids = Cx#cortex.actuator_ids,
    case S_Ids of
        [] -> ok;
        _ ->
            S_Id = pick_random(S_Ids),
            S = genotype:read({sensor,S_Id}),
            case lists:member(Substrate_Id, S#sensor.fanout_ids) of
                false ->
                    U_S = S#sensor{fanout_ids = [Substrate_Id|S#sensor.fanout_ids]},
                    genotype:write(U_S);
                true -> ok
            end
    end,
    case A_Ids of
        [] -> ok;
        _ ->
            Act = genotype:read({actuator,lists:nth(1, A_Ids)}),
            case lists:member(Substrate_Id, Act#actuator.fanin_ids) of
                false ->
                    A_Id = pick_random(A_Ids),
                    Act2 = genotype:read({actuator,A_Id}),
                    U_A = Act2#actuator{fanin_ids = [Substrate_Id|Act2#actuator.fanin_ids]},
                    genotype:write(U_A);
                true -> ok
            end
    end,
    ok.

fix_cpp_cep_path(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Generation = A#agent.generation,
    Substrate_Id = A#agent.substrate_id,
    Substrate = genotype:read({substrate,Substrate_Id}),
    CPP_Ids = Substrate#substrate.cpp_ids,
    CEP_Ids = Substrate#substrate.cep_ids,
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    N_Ids = Cx#cortex.neuron_ids,
    case {CPP_Ids, N_Ids, CEP_Ids} of
        {[], _, _} -> ok;
        {_, [], _} -> ok;
        {_, _, []} -> ok;
        _ ->
            CPP_Id = pick_random(CPP_Ids),
            N_Id = pick_random(N_Ids),
            CEP_Id = pick_random(CEP_Ids),
            link_sensor_to_neuron_if_absent(Agent_Id, Generation, CPP_Id, N_Id),
            link_neuron_to_actuator_if_absent(Agent_Id, Generation, N_Id, CEP_Id),
            ok
    end.

fix_bidirectional_mismatch(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    S_Ids = Cx#cortex.sensor_ids,
    lists:foreach(fun(S_Id) ->
        S = genotype:read({sensor,S_Id}),
        Fanout_Ids = S#sensor.fanout_ids,
        lists:foreach(fun(Fanout_Id) ->
            case Fanout_Id of
                {_,neuron} ->
                    N = genotype:read({neuron,Fanout_Id}),
                    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                    case lists:member(S_Id, Input_Ids) of
                        false -> genome_mutator:link_ToNeuron(S_Id, S#sensor.vl, N, A#agent.generation);
                        true -> ok
                    end;
                _ -> ok
            end
        end, Fanout_Ids)
    end, S_Ids),
    ok.

fix_cpp_bidirectional_mismatch(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Substrate_Id = A#agent.substrate_id,
    Substrate = genotype:read({substrate,Substrate_Id}),
    CPP_Ids = Substrate#substrate.cpp_ids,
    lists:foreach(fun(CPP_Id) ->
        CPP = genotype:read({sensor,CPP_Id}),
        Fanout_Ids = CPP#sensor.fanout_ids,
        lists:foreach(fun(N_Id) ->
            case N_Id of
                {_,neuron} ->
                    N = genotype:read({neuron,N_Id}),
                    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                    case lists:member(CPP_Id, Input_Ids) of
                        false -> genome_mutator:link_ToNeuron(CPP_Id, CPP#sensor.vl, N, A#agent.generation);
                        true -> ok
                    end;
                _ -> ok
            end
        end, Fanout_Ids)
    end, CPP_Ids),
    ok.

link_sensor_to_neuron_if_absent(Agent_Id, Generation, S_Id, N_Id) ->
    S = genotype:read({sensor,S_Id}),
    N = genotype:read({neuron,N_Id}),
    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
    case {lists:member(N_Id, S#sensor.fanout_ids), lists:member(S_Id, Input_Ids)} of
        {true, true} -> ok;
        {false, false} -> genome_mutator:link_FromElementToElement(Agent_Id, S_Id, N_Id);
        {true, false} ->
            U_N = genome_mutator:link_ToNeuron(S_Id, S#sensor.vl, N, Generation),
            genotype:write(U_N);
        {false, true} ->
            U_S = S#sensor{fanout_ids = [N_Id|S#sensor.fanout_ids], generation = Generation},
            genotype:write(U_S)
    end.

link_neuron_to_actuator_if_absent(Agent_Id, Generation, N_Id, A_Id) ->
    N = genotype:read({neuron,N_Id}),
    A = genotype:read({actuator,A_Id}),
    case {lists:member(A_Id, N#neuron.output_ids), lists:member(N_Id, A#actuator.fanin_ids)} of
        {true, true} -> ok;
        {false, false} ->
            case actuator_has_capacity(A) of
                true -> genome_mutator:link_FromElementToElement(Agent_Id, N_Id, A_Id);
                false -> ok
            end;
        {true, false} ->
            case actuator_has_capacity(A) of
                true ->
                    U_A = A#actuator{fanin_ids = [N_Id|A#actuator.fanin_ids], generation = Generation},
                    genotype:write(U_A);
                false -> ok
            end;
        {false, true} ->
            U_N = genome_mutator:link_FromNeuron(N, A_Id, Generation),
            genotype:write(U_N)
    end.

actuator_has_capacity(A) -> length(A#actuator.fanin_ids) < A#actuator.vl.

pick_random([Only]) -> Only;
pick_random(List) when is_list(List), List =/= [] ->
    lists:nth(random:uniform(length(List)), List).

