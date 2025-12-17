-module(verify_agent_mapping).
-compile(export_all).
-include("records.hrl").

%% Verify genotype connectivity for agent 5.663108109859942e-10
verify_agent() ->
    Agent_Id = {5.663108109859942e-10,agent},
    io:format("=== Verifying Agent ~p ===~n~n", [Agent_Id]),
    
    A = genotype:read({agent,Agent_Id}),
    Cx_Id = A#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    
    S_Ids = Cx#cortex.sensor_ids,
    N_Ids = Cx#cortex.neuron_ids,
    A_Ids = Cx#cortex.actuator_ids,
    
    io:format("Cortex: ~p~n", [Cx_Id]),
    io:format("Sensors: ~p~n", [S_Ids]),
    io:format("Neurons: ~p~n", [N_Ids]),
    io:format("Actuators: ~p~n~n", [A_Ids]),
    
    Sensors = [genotype:read({sensor,S_Id}) || S_Id <- S_Ids],
    Neurons = [genotype:read({neuron,N_Id}) || N_Id <- N_Ids],
    Actuators = [genotype:read({actuator,A_Id}) || A_Id <- A_Ids],
    
    io:format("=== Sensor Connections ===~n"),
    lists:foreach(fun(S) ->
        S_Id = S#sensor.id,
        Fanout_Ids = S#sensor.fanout_ids,
        io:format("Sensor ~p:~n", [S_Id]),
        io:format("  vl: ~p~n", [S#sensor.vl]),
        io:format("  fanout_ids: ~p~n", [Fanout_Ids]),
        io:format("~n")
    end, Sensors),
    
    io:format("=== Neuron Connections ===~n"),
    lists:foreach(fun(N) ->
        N_Id = N#neuron.id,
        Input_IdPs = N#neuron.input_idps,
        Output_Ids = N#neuron.output_ids,
        RO_Ids = N#neuron.ro_ids,
        io:format("Neuron ~p:~n", [N_Id]),
        io:format("  input_idps: ~p~n", [Input_IdPs]),
        io:format("  output_ids: ~p~n", [Output_Ids]),
        io:format("  ro_ids: ~p~n", [RO_Ids]),
        
        % Extract sensor inputs
        Sensor_Inputs = [{Id, length(Weights)} || {Id, Weights} <- Input_IdPs, 
                                                   case Id of {_,sensor} -> true; _ -> false end],
        io:format("  sensor inputs: ~p~n", [Sensor_Inputs]),
        io:format("~n")
    end, Neurons),
    
    io:format("=== Actuator Connections ===~n"),
    lists:foreach(fun(A) ->
        A_Id = A#actuator.id,
        Fanin_Ids = A#actuator.fanin_ids,
        io:format("Actuator ~p:~n", [A_Id]),
        io:format("  fanin_ids: ~p~n", [Fanin_Ids]),
        io:format("~n")
    end, Actuators),
    
    io:format("=== Bidirectional Verification ===~n"),
    
    % Check sensor -> neuron bidirectional
    Sensor_Ok = lists:all(fun(S) ->
        S_Id = S#sensor.id,
        Fanout_Ids = S#sensor.fanout_ids,
        Neuron_Fanouts = [F || F <- Fanout_Ids, case F of {_,neuron} -> true; _ -> false end],
        lists:all(fun(N_Id) ->
            N = lists:keyfind(N_Id, #neuron.id, Neurons),
            case N of
                false -> 
                    io:format("ERROR: Neuron ~p not found in neuron list~n", [N_Id]),
                    false;
                _ ->
                    {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
                    Sensor_Inputs = [I || I <- Input_Ids, case I of {_,sensor} -> true; _ -> false end],
                    case lists:member(S_Id, Sensor_Inputs) of
                        true -> true;
                        false ->
                            io:format("ERROR: Sensor ~p has neuron ~p in fanout_ids, but neuron doesn't have sensor in input_idps~n", [S_Id, N_Id]),
                            io:format("  Neuron input_idps sensors: ~p~n", [Sensor_Inputs]),
                            false
                    end
            end
        end, Neuron_Fanouts)
    end, Sensors),
    
    % Check neuron -> sensor bidirectional
    Neuron_Ok = lists:all(fun(N) ->
        N_Id = N#neuron.id,
        {Input_Ids,_} = lists:unzip(N#neuron.input_idps),
        Sensor_Inputs = [I || I <- Input_Ids, case I of {_,sensor} -> true; _ -> false end],
        lists:all(fun(S_Id) ->
            S = lists:keyfind(S_Id, #sensor.id, Sensors),
            case S of
                false ->
                    io:format("ERROR: Sensor ~p not found in sensor list~n", [S_Id]),
                    false;
                _ ->
                    case lists:member(N_Id, S#sensor.fanout_ids) of
                        true -> true;
                        false ->
                            io:format("ERROR: Neuron ~p has sensor ~p in input_idps, but sensor doesn't have neuron in fanout_ids~n", [N_Id, S_Id]),
                            io:format("  Sensor fanout_ids: ~p~n", [S#sensor.fanout_ids]),
                            false
                    end
            end
        end, Sensor_Inputs)
    end, Neurons),
    
    % Check neuron -> actuator bidirectional
    Actuator_Ok = lists:all(fun(A) ->
        A_Id = A#actuator.id,
        Fanin_Ids = A#actuator.fanin_ids,
        lists:all(fun(N_Id) ->
            N = lists:keyfind(N_Id, #neuron.id, Neurons),
            case N of
                false ->
                    io:format("ERROR: Neuron ~p not found in neuron list~n", [N_Id]),
                    false;
                _ ->
                    case lists:member(A_Id, N#neuron.output_ids) of
                        true -> true;
                        false ->
                            io:format("ERROR: Actuator ~p has neuron ~p in fanin_ids, but neuron doesn't have actuator in output_ids~n", [A_Id, N_Id]),
                            io:format("  Neuron output_ids: ~p~n", [N#neuron.output_ids]),
                            false
                    end
            end
        end, Fanin_Ids)
    end, Actuators),
    
    % Check neuron -> neuron bidirectional (output_ids and ro_ids)
    Neuron_Neuron_Ok = lists:all(fun(N) ->
        N_Id = N#neuron.id,
        Output_Ids = N#neuron.output_ids,
        Neuron_Outputs = [O || O <- Output_Ids, case O of {_,neuron} -> true; _ -> false end],
        lists:all(fun(Target_N_Id) ->
            Target_N = lists:keyfind(Target_N_Id, #neuron.id, Neurons),
            case Target_N of
                false ->
                    io:format("ERROR: Target neuron ~p not found in neuron list~n", [Target_N_Id]),
                    false;
                _ ->
                    {Input_Ids,_} = lists:unzip(Target_N#neuron.input_idps),
                    Neuron_Inputs = [I || I <- Input_Ids, case I of {_,neuron} -> true; _ -> false end],
                    case lists:member(N_Id, Neuron_Inputs) of
                        true -> true;
                        false ->
                            io:format("ERROR: Neuron ~p has neuron ~p in output_ids, but target neuron doesn't have source in input_idps~n", [N_Id, Target_N_Id]),
                            io:format("  Target neuron input_idps neurons: ~p~n", [Neuron_Inputs]),
                            false
                    end
            end
        end, Neuron_Outputs)
    end, Neurons),
    
    % Check dimension matching
    io:format("~n=== Dimension Verification ===~n"),
    Dimension_Ok = lists:all(fun(N) ->
        N_Id = N#neuron.id,
        Input_IdPs = N#neuron.input_idps,
        lists:all(fun({Source_Id, Weights}) ->
            case Source_Id of
                {_,sensor} ->
                    S = lists:keyfind(Source_Id, #sensor.id, Sensors),
                    case S of
                        false -> 
                            io:format("ERROR: Sensor ~p not found~n", [Source_Id]),
                            false;
                        _ ->
                            Expected_VL = S#sensor.vl,
                            Actual_Weights = length(Weights),
                            case Expected_VL =:= Actual_Weights of
                                true -> true;
                                false ->
                                    io:format("ERROR: Neuron ~p expects ~p weights from sensor ~p (vl=~p), but has ~p weights~n", 
                                             [N_Id, Expected_VL, Source_Id, Expected_VL, Actual_Weights]),
                                    false
                            end
                    end;
                _ -> true  % Skip non-sensor inputs for dimension check
            end
        end, Input_IdPs)
    end, Neurons),
    
    All_Ok = Sensor_Ok andalso Neuron_Ok andalso Actuator_Ok andalso Neuron_Neuron_Ok andalso Dimension_Ok,
    
    io:format("~n=== Summary ===~n"),
    io:format("Sensor->Neuron bidirectional: ~p~n", [Sensor_Ok]),
    io:format("Neuron->Sensor bidirectional: ~p~n", [Neuron_Ok]),
    io:format("Neuron->Actuator bidirectional: ~p~n", [Actuator_Ok]),
    io:format("Neuron->Neuron bidirectional: ~p~n", [Neuron_Neuron_Ok]),
    io:format("Dimension matching: ~p~n", [Dimension_Ok]),
    io:format("~nOverall: ~p~n", [All_Ok]),
    
    All_Ok.


