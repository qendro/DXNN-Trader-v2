-module(mutate_check).
-compile(export_all).
-include("records.hrl").

-define(MAX_FIX_ATTEMPTS, 10).

fix_connectivity_if_needed(Agent_Id) ->
    case has_valid_connectivity(Agent_Id) of
        true -> ok;
        {false, Reason} ->
            %qlog:xLog(qStatus, "Agent ~p connectivity issue: ~p", [Agent_Id, Reason]),
            case try_fix_and_verify(Agent_Id, Reason) of
                ok -> 
                    %qlog:xLog(qStatus, "Agent ~p connectivity fixed successfully", [Agent_Id]),
                    ok;
                retry ->
                    qlog:xLog(qStatus, "Agent ~p connectivity fix FAILED after max attempts", [Agent_Id]),
                    qlog:genotype_snapshot(Agent_Id, "Connectivity fix failed after all mutations completed"),
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
            Constraint = A#agent.constraint,
            case has_substrate_main_path(Agent_Id, Cx, Substrate_Id) of
                true -> has_substrate_weight_path(Agent_Id, Substrate, A, Constraint);
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
                true -> 
                    case has_isolated_self_recurrent_neurons(Agent_Id, Cx) of
                        true -> true;
                        {false, _} -> {false, isolated_self_recurrent}
                    end;
                false -> {false, no_path}
            end
    end.

has_isolated_self_recurrent_neurons(Agent_Id, Cx) ->
    N_Ids = Cx#cortex.neuron_ids,
    Isolated = [N_Id || N_Id <- N_Ids, is_isolated_self_recurrent(Agent_Id, N_Id)],
    case Isolated of
        [] -> true;
        _ -> 
            %qlog:xLog(qStatus, "Agent ~p detected ~p isolated self-recurrent neuron(s): ~p", [Agent_Id, length(Isolated), Isolated]),
            {false, isolated_self_recurrent}
    end.

is_isolated_self_recurrent(Agent_Id, N_Id) ->
    N = genotype:read({neuron,N_Id}),
    % Early exit: must be self-recurrent first
    case lists:member(N_Id, N#neuron.ro_ids) of
        false -> false;  % Not self-recurrent, skip rest
        true ->
            {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
            Self_In_Inputs = lists:member(N_Id, Input_Ids),
            Only_Self_Or_Bias = lists:all(fun(Id) -> Id == N_Id orelse Id == bias end, Input_Ids),
            Self_In_Inputs andalso Only_Self_Or_Bias
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

has_substrate_weight_path(Agent_Id, Substrate, _A, Constraint) ->
    CPP_Ids = Substrate#substrate.cpp_ids,
    CEP_Ids = Substrate#substrate.cep_ids,
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

verify_sensor_connections_bidirectional(Agent_Id, S_Ids) ->
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    N_Ids = Cx#cortex.neuron_ids,
    
    case {S_Ids, N_Ids} of
        {[], _} -> true;
        {_, []} -> true;
        _ ->
            Sensors = [genotype:read({sensor,S_Id}) || S_Id <- S_Ids],
            Neurons = [genotype:read({neuron,N_Id}) || N_Id <- N_Ids],
            
            Sensor_Map = maps:from_list([{S#sensor.id, S} || S <- Sensors]),
            Neuron_Map = maps:from_list([{N#neuron.id, N} || N <- Neurons]),
            
            Neuron_Input_Map = build_neuron_input_map(Neurons),
            
            verify_sensor_bidirectional(Sensors, Sensor_Map, Neuron_Map, Neuron_Input_Map)
    end.

build_neuron_input_map(Neurons) ->
    lists:foldl(fun(N, Acc) ->
        {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
        Sensor_Inputs = [I || I <- Input_Ids, case I of {_,sensor} -> true; _ -> false end],
        [{N#neuron.id, Sensor_Inputs}|Acc]
    end, [], Neurons).

verify_sensor_bidirectional([], _Sensor_Map, _Neuron_Map, _Neuron_Input_Map) -> true;
verify_sensor_bidirectional([S|Rest], Sensor_Map, Neuron_Map, Neuron_Input_Map) ->
    S_Id = S#sensor.id,
    Fanout_Ids = S#sensor.fanout_ids,
    Neuron_Fanouts = [F || F <- Fanout_Ids, case F of {_,neuron} -> true; _ -> false end],
    
    Direction1_Ok = case Neuron_Fanouts of
        [] -> true;
        _ ->
            lists:all(fun(N_Id) ->
                case maps:find(N_Id, maps:from_list(Neuron_Input_Map)) of
                    {ok, Sensor_Inputs} -> lists:member(S_Id, Sensor_Inputs);
                    error -> false
                end
            end, Neuron_Fanouts)
    end,
    
    Direction2_Ok = lists:all(fun({N_Id, Sensor_Inputs}) ->
        case lists:member(S_Id, Sensor_Inputs) of
            true -> lists:member(N_Id, Fanout_Ids);
            false -> true
        end
    end, Neuron_Input_Map),
    
    case Direction1_Ok andalso Direction2_Ok of
        false -> false;
        true -> verify_sensor_bidirectional(Rest, Sensor_Map, Neuron_Map, Neuron_Input_Map)
    end.

verify_cpp_connections_bidirectional(Agent_Id, CPP_Ids) ->
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    N_Ids = Cx#cortex.neuron_ids,
    
    case {CPP_Ids, N_Ids} of
        {[], _} -> true;
        {_, []} -> true;
        _ ->
            CPPs = [genotype:read({sensor,CPP_Id}) || CPP_Id <- CPP_Ids],
            Neurons = [genotype:read({neuron,N_Id}) || N_Id <- N_Ids],
            
            CPP_Map = maps:from_list([{CPP#sensor.id, CPP} || CPP <- CPPs]),
            Neuron_Map = maps:from_list([{N#neuron.id, N} || N <- Neurons]),
            
            Neuron_Input_Map = build_neuron_input_map(Neurons),
            
            verify_cpp_bidirectional(CPPs, CPP_Map, Neuron_Map, Neuron_Input_Map)
    end.

verify_cpp_bidirectional([], _CPP_Map, _Neuron_Map, _Neuron_Input_Map) -> true;
verify_cpp_bidirectional([CPP|Rest], CPP_Map, Neuron_Map, Neuron_Input_Map) ->
    CPP_Id = CPP#sensor.id,
    Fanout_Ids = CPP#sensor.fanout_ids,
    Neuron_Fanouts = [F || F <- Fanout_Ids, case F of {_,neuron} -> true; _ -> false end],
    
    Direction1_Ok = case Neuron_Fanouts of
        [] -> true;
        _ ->
            lists:all(fun(N_Id) ->
                case maps:find(N_Id, maps:from_list(Neuron_Input_Map)) of
                    {ok, Sensor_Inputs} -> lists:member(CPP_Id, Sensor_Inputs);
                    error -> false
                end
            end, Neuron_Fanouts)
    end,
    
    Direction2_Ok = lists:all(fun({N_Id, Sensor_Inputs}) ->
        case lists:member(CPP_Id, Sensor_Inputs) of
            true -> lists:member(N_Id, Fanout_Ids);
            false -> true
        end
    end, Neuron_Input_Map),
    
    case Direction1_Ok andalso Direction2_Ok of
        false -> false;
        true -> verify_cpp_bidirectional(Rest, CPP_Map, Neuron_Map, Neuron_Input_Map)
    end.

has_path_bfs(_Agent_Id, Target_Id, Target_Id, _Visited, _Queue, _ConnectionArch) -> true;
has_path_bfs(_Agent_Id, _From_Id, _Target_Id, _Visited, [], _ConnectionArch) -> false;
has_path_bfs(Agent_Id, From_Id, Target_Id, Visited, [Current_Id|Queue], ConnectionArch) ->
    case Current_Id of
        Target_Id -> true;
        _ ->
            Neighbors = get_neighbors(Agent_Id, Current_Id, ConnectionArch),
            New_Neighbors = [N || N <- Neighbors, not lists:member(N, Visited)],
            New_Queue = New_Neighbors ++ Queue,
            New_Visited = New_Neighbors ++ Visited,
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
        isolated_self_recurrent -> fix_isolated_self_recurrent(Agent_Id);
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
    % Use transaction to ensure atomicity
    F = fun() ->
        A = genotype:read({agent,Agent_Id}),
        Cx_Id = A#agent.cx_id,
        Cx = genotype:read({cortex,Cx_Id}),
        Substrate_Id = A#agent.substrate_id,
        S_Ids = Cx#cortex.sensor_ids,
        A_Ids = Cx#cortex.actuator_ids,
        
        % Collect updates
        Updates = case S_Ids of
            [] -> [];
            _ ->
                S_Id = pick_random(S_Ids),
                S = genotype:read({sensor,S_Id}),
                case lists:member(Substrate_Id, S#sensor.fanout_ids) of
                    false ->
                        U_S = S#sensor{fanout_ids = [Substrate_Id|S#sensor.fanout_ids]},
                        case A_Ids of
                            [] -> [U_S];
                            _ ->
                                A_Id = pick_random(A_Ids),
                                Act = genotype:read({actuator,A_Id}),
                                case lists:member(Substrate_Id, Act#actuator.fanin_ids) of
                                    false ->
                                        U_A = Act#actuator{fanin_ids = [Substrate_Id|Act#actuator.fanin_ids]},
                                        [U_S, U_A];
                                    true -> [U_S]
                                end
                        end;
                    true ->
                        case A_Ids of
                            [] -> [];
                            _ ->
                                A_Id = pick_random(A_Ids),
                                Act = genotype:read({actuator,A_Id}),
                                case lists:member(Substrate_Id, Act#actuator.fanin_ids) of
                                    false ->
                                        U_A = Act#actuator{fanin_ids = [Substrate_Id|Act#actuator.fanin_ids]},
                                        [U_A];
                                    true -> []
                                end
                        end
                end
        end,
        
        % Write all updates in a single transaction
        [mnesia:write(U) || U <- Updates],
        ok
    end,
    mnesia:transaction(F).

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
    % Use transaction to ensure atomicity and prevent race conditions
    F = fun() ->
        A = genotype:read({agent,Agent_Id}),
        Cx_Id = A#agent.cx_id,
        Cx = genotype:read({cortex,Cx_Id}),
        S_Ids = Cx#cortex.sensor_ids,
        N_Ids = Cx#cortex.neuron_ids,
        
        % Collect all updates first: check sensors -> neurons, then neurons -> sensors
        Updates1 = collect_sensor_to_neuron_fixes_tx(Agent_Id, S_Ids, A#agent.generation, []),
        Updates2 = collect_neuron_sensor_fixes_tx(Agent_Id, N_Ids, A#agent.generation, []),
        Updates = Updates1 ++ Updates2,
        
        % Write all updates in a single transaction (atomic and prevents race conditions)
        [mnesia:write(U) || U <- Updates],
        ok
    end,
    mnesia:transaction(F).

collect_sensor_to_neuron_fixes_tx(_Agent_Id, [], _Generation, Acc) -> Acc;
collect_sensor_to_neuron_fixes_tx(Agent_Id, [S_Id|S_Rest], Generation, Acc) ->
    S = genotype:read({sensor,S_Id}),
    Fanout_Ids = S#sensor.fanout_ids,
    Acc1 = lists:foldl(fun(Fanout_Id, AccIn) ->
        case Fanout_Id of
            {_,neuron} ->
                N = genotype:read({neuron,Fanout_Id}),
                {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                case lists:member(S_Id, Input_Ids) of
                    false -> 
                        U_N = genome_mutator:link_ToNeuron(S_Id, S#sensor.vl, N, Generation),
                        [U_N|AccIn];
                    true -> AccIn
                end;
            _ -> AccIn
        end
    end, Acc, Fanout_Ids),
    collect_sensor_to_neuron_fixes_tx(Agent_Id, S_Rest, Generation, Acc1).

collect_neuron_sensor_fixes_tx(_Agent_Id, [], _Generation, Acc) -> Acc;
collect_neuron_sensor_fixes_tx(Agent_Id, [N_Id|N_Rest], Generation, Acc) ->
    N = genotype:read({neuron,N_Id}),
    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
    Acc1 = lists:foldl(fun(Input_Id, AccIn) ->
        case Input_Id of
            {_,sensor} ->
                S = genotype:read({sensor,Input_Id}),
                case lists:member(N_Id, S#sensor.fanout_ids) of
                    false ->
                        U_S = S#sensor{fanout_ids = [N_Id|S#sensor.fanout_ids], generation = Generation},
                        [U_S|AccIn];
                    true -> AccIn
                end;
            _ -> AccIn
        end
    end, Acc, Input_Ids),
    collect_neuron_sensor_fixes_tx(Agent_Id, N_Rest, Generation, Acc1).

fix_cpp_bidirectional_mismatch(Agent_Id) ->
    % Use transaction to ensure atomicity and prevent race conditions
    F = fun() ->
        A = genotype:read({agent,Agent_Id}),
        Substrate_Id = A#agent.substrate_id,
        Substrate = genotype:read({substrate,Substrate_Id}),
        CPP_Ids = Substrate#substrate.cpp_ids,
        Cx_Id = A#agent.cx_id,
        Cx = genotype:read({cortex,Cx_Id}),
        N_Ids = Cx#cortex.neuron_ids,
        
        % Collect all updates first: check CPP -> neurons, then neurons -> CPP
        Updates1 = collect_cpp_to_neuron_fixes_tx(Agent_Id, CPP_Ids, A#agent.generation, []),
        Updates2 = collect_neuron_to_cpp_fixes_tx(Agent_Id, N_Ids, CPP_Ids, A#agent.generation, []),
        Updates = Updates1 ++ Updates2,
        
        % Write all updates in a single transaction
        [mnesia:write(U) || U <- Updates],
        ok
    end,
    mnesia:transaction(F).

collect_cpp_to_neuron_fixes_tx(_Agent_Id, [], _Generation, Acc) -> Acc;
collect_cpp_to_neuron_fixes_tx(Agent_Id, [CPP_Id|CPP_Rest], Generation, Acc) ->
    CPP = genotype:read({sensor,CPP_Id}),
    Fanout_Ids = CPP#sensor.fanout_ids,
    Acc1 = lists:foldl(fun(N_Id, AccIn) ->
        case N_Id of
            {_,neuron} ->
                N = genotype:read({neuron,N_Id}),
                {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                case lists:member(CPP_Id, Input_Ids) of
                    false -> 
                        U_N = genome_mutator:link_ToNeuron(CPP_Id, CPP#sensor.vl, N, Generation),
                        [U_N|AccIn];
                    true -> AccIn
                end;
            _ -> AccIn
        end
    end, Acc, Fanout_Ids),
    collect_cpp_to_neuron_fixes_tx(Agent_Id, CPP_Rest, Generation, Acc1).

collect_neuron_to_cpp_fixes_tx(_Agent_Id, [], _CPP_Ids, _Generation, Acc) -> Acc;
collect_neuron_to_cpp_fixes_tx(Agent_Id, [N_Id|N_Rest], CPP_Ids, Generation, Acc) ->
    N = genotype:read({neuron,N_Id}),
    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
    Acc1 = lists:foldl(fun(Input_Id, AccIn) ->
        case Input_Id of
            {_,sensor} ->
                case lists:member(Input_Id, CPP_Ids) of
                    true ->
                        CPP = genotype:read({sensor,Input_Id}),
                        case lists:member(N_Id, CPP#sensor.fanout_ids) of
                            false ->
                                U_CPP = CPP#sensor{fanout_ids = [N_Id|CPP#sensor.fanout_ids], generation = Generation},
                                [U_CPP|AccIn];
                            true -> AccIn
                        end;
                    false -> AccIn
                end;
            _ -> AccIn
        end
    end, Acc, Input_Ids),
    collect_neuron_to_cpp_fixes_tx(Agent_Id, N_Rest, CPP_Ids, Generation, Acc1).

fix_isolated_self_recurrent(Agent_Id) ->
    A = genotype:read({agent,Agent_Id}),
    Generation = A#agent.generation,
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    S_Ids = Cx#cortex.sensor_ids,
    N_Ids = Cx#cortex.neuron_ids,
    Isolated = [N_Id || N_Id <- N_Ids, is_isolated_self_recurrent(Agent_Id, N_Id)],
    case Isolated of
        [] -> ok;
        _ ->
            %qlog:xLog(qStatus, "Agent ~p fixing ~p isolated self-recurrent neuron(s)", [Agent_Id, length(Isolated)]),
            lists:foreach(fun(Isolated_N_Id) ->
                case {S_Ids, N_Ids} of
                    {[], []} -> ok;
                    {[], _} -> 
                        % Only neurons: respect feedforward constraints
                        Available_Neurons = case A#agent.constraint#constraint.connection_architecture of
                            feedforward -> 
                                genome_mutator:filter_InlinkIdPool(A#agent.constraint, Isolated_N_Id, N_Ids);
                            recurrent -> N_Ids
                        end,
                        case Available_Neurons -- [Isolated_N_Id] of
                            [] -> ok;
                            Available -> 
                                From_N_Id = pick_random(Available),
                                genome_mutator:link_FromElementToElement(Agent_Id, From_N_Id, Isolated_N_Id)
                                %qlog:xLog(qStatus, "Agent ~p fixed isolated neuron ~p: connected from neuron ~p", [Agent_Id, Isolated_N_Id, From_N_Id])
                        end;
                    {_, []} -> 
                        % Only sensors
                        S_Id = pick_random(S_Ids),
                        link_sensor_to_neuron_if_absent(Agent_Id, Generation, S_Id, Isolated_N_Id);
                        %qlog:xLog(qStatus, "Agent ~p fixed isolated neuron ~p: connected from sensor ~p", [Agent_Id, Isolated_N_Id, S_Id]);
                    _ ->
                        % Both available: randomly choose
                        case rand:uniform(2) of
                            1 -> 
                                S_Id = pick_random(S_Ids),
                                link_sensor_to_neuron_if_absent(Agent_Id, Generation, S_Id, Isolated_N_Id);
                                %qlog:xLog(qStatus, "Agent ~p fixed isolated neuron ~p: connected from sensor ~p", [Agent_Id, Isolated_N_Id, S_Id]);
                            2 ->
                                Available_Neurons = case A#agent.constraint#constraint.connection_architecture of
                                    feedforward -> 
                                        genome_mutator:filter_InlinkIdPool(A#agent.constraint, Isolated_N_Id, N_Ids);
                                    recurrent -> N_Ids
                                end,
                                case Available_Neurons -- [Isolated_N_Id] of
                                    [] -> 
                                        S_Id = pick_random(S_Ids),
                                        link_sensor_to_neuron_if_absent(Agent_Id, Generation, S_Id, Isolated_N_Id);
                                        %qlog:xLog(qStatus, "Agent ~p fixed isolated neuron ~p: connected from sensor ~p (fallback)", [Agent_Id, Isolated_N_Id, S_Id]);
                                    Available -> 
                                        From_N_Id = pick_random(Available),
                                        genome_mutator:link_FromElementToElement(Agent_Id, From_N_Id, Isolated_N_Id)
                                        %qlog:xLog(qStatus, "Agent ~p fixed isolated neuron ~p: connected from neuron ~p", [Agent_Id, Isolated_N_Id, From_N_Id])
                                end
                        end
                end
            end, Isolated),
            ok
    end.

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
    lists:nth(rand:uniform(length(List)), List).

