%% Phase 5: Agent Auditor - Lightweight Memory Governor
%% Periodically monitors BEAM memory and suggests eviction/restart actions to population_monitor

-module(agent_auditor).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    pm_pid,
    population_id,
    interval_ms,
    high_water_bytes,
    low_water_bytes,
    victim_policy,
    restart_batch_size,
    tick_ref = undefined
}).

%% ===================================================================
%% API
%% ===================================================================

start_link(PM_Pid, PopulationId) ->
    gen_server:start_link(?MODULE, [PM_Pid, PopulationId], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([PM_Pid, PopulationId]) ->
    IntervalMs = config:auditor_interval_ms(),
    HighWaterBytes = config:auditor_high_water_bytes(),
    LowWaterBytes = config:auditor_low_water_bytes(),
    VictimPolicy = config:auditor_victim_policy(),
    RestartBatchSize = config:auditor_restart_batch_size(),
    
    State = #state{
        pm_pid = PM_Pid,
        population_id = PopulationId,
        interval_ms = IntervalMs,
        high_water_bytes = HighWaterBytes,
        low_water_bytes = LowWaterBytes,
        victim_policy = VictimPolicy,
        restart_batch_size = RestartBatchSize
    },
    
    % Schedule first tick
    TickRef = erlang:send_after(IntervalMs, self(), tick),
    {ok, State#state{tick_ref = TickRef}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    NewState = process_tick(State),
    % Schedule next tick
    TickRef = erlang:send_after(State#state.interval_ms, self(), tick),
    {noreply, NewState#state{tick_ref = TickRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel pending tick
    case State#state.tick_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

process_tick(State) ->
    #state{
        pm_pid = PM_Pid,
        population_id = PopulationId,
        high_water_bytes = HighWater,
        low_water_bytes = LowWater,
        victim_policy = VictimPolicy,
        restart_batch_size = RestartBatchSize
    } = State,
    
    % Fast path: check total memory once
    MemoryInfo = erlang:memory(),
    TotalMemory = proplists:get_value(total, MemoryInfo, 0),
    ProcessMemory = case proplists:get_value(processes_used, MemoryInfo) of
        undefined -> proplists:get_value(processes, MemoryInfo, 0);
        Val -> Val
    end,
    
    if
        TotalMemory > HighWater ->
            % Phase 6: Memory pressure detected - compute per-agent memory and suggest eviction
            handle_high_water(PopulationId, PM_Pid, VictimPolicy, TotalMemory, ProcessMemory);
        TotalMemory < LowWater ->
            % Phase 6: Memory recovered - suggest restart permit (hysteresis)
            % Only send permit if there might be agents waiting (PM will check queue)
            qlog:benchmarker(PopulationId, io_lib:format("AUDITOR_LOW_WATER | batch_size=~p | total_memory=~p | process_memory=~p", [RestartBatchSize, TotalMemory, ProcessMemory])),
            gen_server:cast(PM_Pid, {auditor_restart_permit, RestartBatchSize, TotalMemory, ProcessMemory});
        true ->
            % Normal operation - memory between low and high water, no action needed
            ok
    end,
    
    State.

handle_high_water(PopulationId, PM_Pid, VictimPolicy, TotalMemory, ProcessMemory) ->
    % Fetch active agents from registry
    ActiveAgents = agent_runtime_registry:list_active(PopulationId),
    
    case ActiveAgents of
        [] ->
            % No active agents to evict
            qlog:benchmarker(PopulationId, io_lib:format("AUDITOR_HIGH_WATER | no_active_agents | total_memory=~p | process_memory=~p", [TotalMemory, ProcessMemory])),
            ok;
        _ ->
            % Compute memory for each agent and update registry
            AgentsWithMemory = lists:map(fun(Agent) ->
                AgentId = maps:get(agent_id, Agent),
                {ok, MemoryBytes} = agent_runtime_registry:sum_agent_memory(PopulationId, AgentId),
                agent_runtime_registry:update_memory(PopulationId, AgentId, MemoryBytes, erlang:system_time(millisecond)),
                {AgentId, MemoryBytes, Agent}
            end, ActiveAgents),
            
            % Select victim based on policy
            case select_victim(VictimPolicy, AgentsWithMemory) of
                {ok, {VictimId, VictimBytes, _VictimAgent}} ->
                    % Suggest eviction to PM
                    qlog:benchmarker(PopulationId, io_lib:format("AUDITOR_EVICT_SUGGESTION | agent_id=~p | agent_memory=~p | total_memory=~p | process_memory=~p", [VictimId, VictimBytes, TotalMemory, ProcessMemory])),
                    gen_server:cast(PM_Pid, {auditor_evict_candidate, VictimId, VictimBytes, TotalMemory, ProcessMemory});
                {error, no_victim} ->
                    qlog:benchmarker(PopulationId, io_lib:format("AUDITOR_NO_VICTIM | total_memory=~p | process_memory=~p", [TotalMemory, ProcessMemory])),
                    ok
            end
    end.

select_victim(highest_agent_memory, AgentsWithMemory) ->
    case AgentsWithMemory of
        [] ->
            {error, no_victim};
        _ ->
            % Sort by memory descending, pick highest
            Sorted = lists:sort(fun({_IdA, BytesA, _AgentA}, {_IdB, BytesB, _AgentB}) ->
                BytesA >= BytesB
            end, AgentsWithMemory),
            {ok, hd(Sorted)}
    end;

select_victim(_UnknownPolicy, _AgentsWithMemory) ->
    {error, no_victim}.
