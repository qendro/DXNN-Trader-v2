-module(agent_runtime_registry).
-compile(export_all).

%% Phase 2: Runtime Registry for PM-managed agents
%% ETS-backed runtime metadata storage

-define(TABLE_NAME, agent_runtime_registry).

%% API
-export([
    init/0,
    clear_population/1,
    upsert_manifest/10,
    update_status/5,
    update_memory/4,
    update_progress/5,
    update_monitor_ref/3,
    update_exoself/6,
    increment_eviction_count/3,
    get_agent/2,
    list_population/1,
    list_active/1,
    list_by_status/2,
    delete_agent/2,
    sum_agent_memory/2
]).

%% Initialize the ETS table
init() ->
    case ets:info(?TABLE_NAME) of
        undefined ->
            ets:new(?TABLE_NAME, [named_table, public, set, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

%% Clear all agents belonging to a population
clear_population(PopulationId) ->
    case ets:info(?TABLE_NAME) of
        undefined ->
            ok;
        _ ->
            ets:match_delete(?TABLE_NAME, {{PopulationId, '_'}, '_'}),
            ok
    end.

%% Upsert agent manifest (overwrites if exists)
upsert_manifest(PopulationId, AgentId, RunRef, ExoSelfPid, OwnedPids, OpMode, Status, SpawnTsMs, MonitorRef, StatusReason) ->
    Key = {PopulationId, AgentId},
    EvictionCount = case ets:lookup(?TABLE_NAME, Key) of
        [{Key, ExistingValue}] ->
            maps:get(eviction_count, ExistingValue, 0);
        [] ->
            0
    end,
    Value = #{
        agent_id => AgentId,
        population_id => PopulationId,
        run_ref => RunRef,
        exoself_pid => ExoSelfPid,
        owned_pids => lists:usort([ExoSelfPid | OwnedPids]),
        op_mode => OpMode,
        status => Status,
        status_reason => StatusReason,
        spawn_ts_ms => SpawnTsMs,
        last_progress => undefined,
        last_progress_ts_ms => undefined,
        last_memory_bytes => 0,
        eviction_count => EvictionCount,
        monitor_ref => MonitorRef
    },
    ets:insert(?TABLE_NAME, {Key, Value}),
    ok.

%% Update agent status
update_status(PopulationId, AgentId, Status, StatusReason, _TsMs) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            UpdatedValue = Value#{
                status => Status,
                status_reason => StatusReason
            },
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Update agent memory usage
update_memory(PopulationId, AgentId, MemoryBytes, _TsMs) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            UpdatedValue = Value#{last_memory_bytes => MemoryBytes},
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Update agent progress
update_progress(PopulationId, AgentId, Progress, TsMs, _ProgressData) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            UpdatedValue = Value#{
                last_progress => Progress,
                last_progress_ts_ms => TsMs
            },
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Update monitor reference after PM starts monitoring exoself
update_monitor_ref(PopulationId, AgentId, MonitorRef) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            UpdatedValue = Value#{monitor_ref => MonitorRef},
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Update exoself PID and RunRef (for restarts)
update_exoself(PopulationId, AgentId, NewExoSelfPid, NewRunRef, NewMonitorRef, SpawnTsMs) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            OldOwnedPids = maps:get(owned_pids, Value, []),
            % Remove old exoself PID if present
            OldExoSelfPid = maps:get(exoself_pid, Value, undefined),
            CleanedPids = case OldExoSelfPid of
                undefined -> OldOwnedPids;
                _ -> lists:delete(OldExoSelfPid, OldOwnedPids)
            end,
            UpdatedValue = Value#{
                exoself_pid => NewExoSelfPid,
                run_ref => NewRunRef,
                monitor_ref => NewMonitorRef,
                spawn_ts_ms => SpawnTsMs,
                owned_pids => lists:usort([NewExoSelfPid | CleanedPids])
            },
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Increment eviction count
increment_eviction_count(PopulationId, AgentId, _TsMs) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            CurrentCount = maps:get(eviction_count, Value, 0),
            UpdatedValue = Value#{eviction_count => CurrentCount + 1},
            ets:insert(?TABLE_NAME, {Key, UpdatedValue}),
            ok;
        [] ->
            {error, not_found}
    end.

%% Get agent record
get_agent(PopulationId, AgentId) ->
    Key = {PopulationId, AgentId},
    case ets:lookup(?TABLE_NAME, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

%% List all agents in a population
list_population(PopulationId) ->
    case ets:info(?TABLE_NAME) of
        undefined ->
            [];
        _ ->
            MatchSpec = [{{{'$1', '$2'}, '$3'}, [{'==', '$1', PopulationId}], ['$3']}],
            ets:select(?TABLE_NAME, MatchSpec)
    end.

%% List active agents (status = running)
list_active(PopulationId) ->
    case ets:info(?TABLE_NAME) of
        undefined ->
            [];
        _ ->
            AllAgents = list_population(PopulationId),
            [Agent || Agent <- AllAgents, maps:get(status, Agent, undefined) == running]
    end.

%% List agents by status
list_by_status(PopulationId, Status) ->
    case ets:info(?TABLE_NAME) of
        undefined ->
            [];
        _ ->
            AllAgents = list_population(PopulationId),
            [Agent || Agent <- AllAgents, maps:get(status, Agent, undefined) == Status]
    end.

%% Delete agent record
delete_agent(PopulationId, AgentId) ->
    Key = {PopulationId, AgentId},
    ets:delete(?TABLE_NAME, Key),
    ok.

%% Sum agent memory across all owned PIDs
sum_agent_memory(PopulationId, AgentId) ->
    case get_agent(PopulationId, AgentId) of
        {ok, Agent} ->
            OwnedPids = maps:get(owned_pids, Agent, []),
            TotalMemory = lists:foldl(fun(Pid, Acc) ->
                case catch erlang:process_info(Pid, memory) of
                    {memory, Bytes} -> Acc + Bytes;
                    _ -> Acc
                end
            end, 0, OwnedPids),
            {ok, TotalMemory};
        {error, not_found} ->
            {error, not_found}
    end.
