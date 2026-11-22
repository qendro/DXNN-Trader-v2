-module(agent_logger).
-export([log_message/5, log_agent_spawn/3, log_agent_terminate/4]).
-include("records.hrl").

%% Log individual messages
%% ExoSelf_PId: The exoself process ID
%% ProcessType: atom describing the process type (sensor, neuron, actuator, cortex, exoself, substrate, etc.)
%% ProcessId: The ID of the process (can be a tuple like {sensor, 12.3} or a PID)
%% Action: atom describing the action (sent, received, decision, etc.)
%% MessageInfo: term describing the message or action details
log_message(ExoSelf_PId, ProcessType, ProcessId, Action, MessageInfo) ->
    Agent_Id = case get({exoself_to_agent, ExoSelf_PId}) of
        undefined ->
            case catch genotype:dirty_read({agent, ExoSelf_PId}) of
                {'EXIT', _} -> ExoSelf_PId;
                #agent{id = Agent_Id_Val} -> Agent_Id_Val;
                _ -> ExoSelf_PId
            end;
        Cached_Agent_Id -> Cached_Agent_Id
    end,
    
    LogData = io_lib:format(
        "[~s] ~p(~p) ~p: ~p~n",
        [timestamp(), ProcessType, ProcessId, Action, MessageInfo]
    ),
    
    write_log(Agent_Id, LogData).

%% Log agent spawn event
log_agent_spawn(Agent_Id, ExoSelf_PId, OpMode) ->
    LogData = io_lib:format(
        "[~s] agent_spawn: agent_id=~p, exoself_pid=~p, mode=~p~n",
        [timestamp(), Agent_Id, ExoSelf_PId, OpMode]
    ),
    write_log(Agent_Id, LogData).

%% Log agent termination
log_agent_terminate(Agent_Id, Reason, Fitness, Cycles) ->
    LogData = io_lib:format(
        "[~s] agent_terminate: reason=~p, fitness=~p, cycles=~p~n",
        [timestamp(), Reason, Fitness, Cycles]
    ),
    write_log(Agent_Id, LogData).

%% Internal helper to write to log file
write_log(Agent_Id, LogData) ->
    LogDir = "logs/agents",
    filelib:ensure_dir(LogDir ++ "/"),
    LogFile = lists:flatten(io_lib:format("~s/agent_~p.log", [LogDir, Agent_Id])),
    file:write_file(LogFile, LogData, [append]).

%% Generate timestamp string
timestamp() ->
    {{Y,M,D},{H,Min,S}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", 
                                [Y,M,D,H,Min,S])).

