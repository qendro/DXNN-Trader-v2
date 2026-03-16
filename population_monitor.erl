
-module(population_monitor).
-include("records.hrl").
%% API
-export([start_link/1,start_link/0,start/1,start/0,stop/0,init/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,create_MutantAgentCopy/1,test/0, create_specie/3, continue/0, continue/1, init_population/2, prep_PopState/2, extract_AgentIds/2,delete_population/1]).
-behaviour(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Population Monitor Options & Parameters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Fixed: Use function call instead of macro for dynamic config
get_init_constraints() ->
    [#constraint{morphology=Morphology,connection_architecture=CA, population_evo_alg_f=config:population_evo_alg_f(),neural_pfns=config:neural_plasticity_functions(),agent_encoding_types=config:agent_encoding_types()} || Morphology<-[config:morphology()],CA<-[config:connection_architecture()]].
-record(state,{
	op_mode = gt,
	population_id = config:population_id(),
	activeAgent_IdPs = [],
	agent_ids = [],
	tot_agents,
	agents_left,
	op_tag,
	agent_summaries = [],
	pop_gen = 0,
	eval_acc = 0,
	cycle_acc = 0,
	time_acc = 0,
	tot_evaluations = 0,
	step_size,
	goal_status,
	evolutionary_algorithm,
	fitness_postprocessor,
	selection_algorithm,
	best_fitness,
	survival_percentage = config:survival_percentage(),
	specie_size_limit = config:specie_size_limit(),
	init_specie_size = config:init_specie_size(),
	polis_id = mathema,
	generation_limit = config:generation_limit(),
	evaluations_limit = config:evaluations_limit(),
	fitness_goal = inf,
	benchmarker_pid,
	goal_reached=false,
	% Phase 1 & 4: Runtime tracking for managed agents
	agent_pid_to_id = #{},          % ExoSelfPid => AgentId
	agent_id_to_pid = #{},          % AgentId => ExoSelfPid
	agent_id_to_ref = #{},          % AgentId => MonitorRef
	agent_id_to_runref = #{},       % AgentId => RunRef
	runtime_status = #{},           % AgentId => running|evicting|evicted|restarting|completed|crashed
	inflight_evictions = #{},       % AgentId => #{run_ref => RunRef, requested_ts_ms => Ts, reason => Reason}
	restart_queue = queue:new(),    % queue of AgentId
	% Phase 5: Auditor process
	auditor_pid = undefined         % Auditor gen_server PID
}).
%%==================================================================== API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Start_Parameters) ->
	gen_server:start_link(?MODULE, Start_Parameters, []).

start(Start_Parameters) -> 
	gen_server:start(?MODULE, Start_Parameters, []).
	
start_link() ->
	gen_server:start_link(?MODULE, [], []).
    
start() -> 
	init_population(#state{},get_init_constraints()).

stop() ->
	gen_server:cast(monitor,{stop,normal}).
	
init(Pid,InitState)->
	gen_server:cast(Pid,{init,InitState}).

%%==================================================================== gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(S) ->
	process_flag(trap_exit,true),
	register(monitor,self()),
	% Phase 2: Initialize runtime registry
	agent_runtime_registry:init(),
	Population_Id = S#state.population_id,
	OpMode = S#state.op_mode,
	qlog:benchmarker(Population_Id,io_lib:format("POP_MONITOR_INIT | op_mode=~p | population_id=~p",[OpMode,Population_Id])),
	io:format("******** Population monitor started with parameters:~p~n",[S]),
	Agent_Ids = extract_AgentIds(Population_Id,all),
	SpawnedAgents = summon_agents(OpMode,Agent_Ids),
	P = genotype:dirty_read({population,Population_Id}),
	[put({evaluations,Specie_Id},0) || Specie_Id<-P#population.specie_ids],
	T = P#population.trace,
	TotEvaluations=T#trace.tot_evaluations,
	io:format("Initial Tot Evaluations:~p~n",[TotEvaluations]),
	% Phase 5: Start auditor if enabled
	AuditorPid = case config:auditor_enabled() of
		true ->
			{ok, Pid} = agent_auditor:start_link(self(), Population_Id),
			qlog:benchmarker(Population_Id, io_lib:format("AUDITOR_STARTED | auditor_pid=~p", [Pid])),
			Pid;
		false ->
			undefined
	end,
	State = S#state{
		population_id = Population_Id,
		tot_agents = length(Agent_Ids),
		agents_left = length(Agent_Ids),
		op_tag = continue,
		evolutionary_algorithm = P#population.evo_alg_f,
		fitness_postprocessor = P#population.fitness_postprocessor_f,
		selection_algorithm = P#population.selection_f,
		best_fitness = 0,
		step_size = T#trace.step_size,
		tot_evaluations = TotEvaluations,
		auditor_pid = AuditorPid
	},
	U_State = register_spawned_agents(SpawnedAgents, State),
	% Log generation_start for generation 0
	qlog:exp_runner(generation_start, {Population_Id, 0, length(Agent_Ids)}),
	qlog:agent_trades_generation(generation_start, {Population_Id, 0, length(Agent_Ids)}),
	qlog:benchmarker(Population_Id,io_lib:format("GENERATION_START | population_id=~p | generation=~p | total_agents=~p",[Population_Id, 0, length(Agent_Ids)])),
	{ok, U_State}.
%In init/1 the population_monitor proces registers itself with the node under the name monitor, and sets all the needed parameters within its #state record. The function first extracts all the Agent_Ids that belong to the population using the extract_AgentIds/2 function. Each agent is then spawned/activated, converted from genotype to phenotype in the summon_agents/2 function. The summon_agents/2 function summons the agents and returns to the caller a list of tuples with the following format: [{Agent_Id,Agent_PId}...]. Once the state record's parameters have been set, the function drops into the main gen_server loop.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({stop,normal},_From, S)->
	ActiveAgent_IdPs = S#state.activeAgent_IdPs,
	[Agent_PId ! {self(),terminate} || {_DAgent_Id,Agent_PId}<-ActiveAgent_IdPs],
	{stop, normal, S};
handle_call({stop,shutdown},_From,State)->
	{stop, shutdown, State};
handle_call(get_active_agents, _From, S) ->
	ActiveAgent_IdPs = S#state.activeAgent_IdPs,
	AgentsLeft = S#state.agents_left,
	TotAgents = S#state.tot_agents,
	Reply = {active_agents, ActiveAgent_IdPs, AgentsLeft, TotAgents},
	{reply, Reply, S};
handle_call({request_agent_eviction, AgentId, Reason}, _From, S) ->
	% Phase 4: External eviction request (e.g., from genotype_utils)
	CurrentStatus = maps:get(AgentId, S#state.runtime_status, undefined),
	case CurrentStatus of
		running ->
			case maps:get(AgentId, S#state.agent_id_to_pid, undefined) of
				Pid when is_pid(Pid) ->
					U_S = evict_running_agent(AgentId, Reason, S),
					{reply, {ok, eviction_requested}, U_S};
				_ ->
					{reply, {error, agent_not_found}, S}
			end;
		undefined ->
			{reply, {error, agent_not_found}, S};
		OtherStatus ->
			{reply, {error, {invalid_status, OtherStatus}}, S}
	end;
handle_call(get_runtime_state, _From, S) ->
	% Phase 4: Return runtime state for debugging/monitoring
	RuntimeState = #{
		agent_pid_to_id => S#state.agent_pid_to_id,
		agent_id_to_pid => S#state.agent_id_to_pid,
		runtime_status => S#state.runtime_status,
		inflight_evictions => S#state.inflight_evictions,
		restart_queue_length => queue:len(S#state.restart_queue)
	},
	{reply, RuntimeState, S}.
%If the population_monitor process receives a {stop,normal} call, it checks if there are any still active agents. If there are any, it terminates them, and then itself terminates.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({agent_runtime_event,RunRef,Agent_Id,completed,EventData},S) ->
	% Phase 4: Handle managed agent completion
	case current_run_matches(Agent_Id, RunRef, S) of
		true ->
			% Valid completion from current run
			Fitness = maps:get(fitness, EventData, 0),
			U_S = mark_agent_status(Agent_Id, completed, normal_completion, S),
			handle_agent_completion(Agent_Id, Fitness, true, U_S);
		false ->
			% Stale event from old run, ignore
			io:format("Ignoring stale completion event for Agent ~p~n", [Agent_Id]),
			{noreply, S}
	end;

handle_cast({agent_runtime_event,RunRef,Agent_Id,evicted,EventData},S) ->
	% Phase 4: Handle managed agent eviction
	case current_run_matches(Agent_Id, RunRef, S) of
		true ->
			Reason = maps:get(reason, EventData, unknown),
			AgentBytes = eviction_agent_bytes(Agent_Id, Reason, S),
			io:format("Agent ~p evicted: ~p~n", [Agent_Id, Reason]),
			qlog:xLog(qStatus, "GRACEFUL_STOP_CONFIRMED | population_id=~p | agent_id=~p | run_ref=~p | agent_bytes=~p | reason=~p", [S#state.population_id, Agent_Id, RunRef, AgentBytes, Reason]),
			qlog:benchmarker(S#state.population_id, io_lib:format("AGENT_EVICTED | agent_id=~p | reason=~p", [Agent_Id, Reason])),
			{noreply, finalize_eviction(Agent_Id, Reason, S)};
		false ->
			io:format("Ignoring stale evicted event for Agent ~p~n", [Agent_Id]),
			{noreply, S}
	end;

handle_cast({Agent_Id,terminated,Fitness},S) ->
	% Legacy unmanaged agent termination
	handle_agent_completion(Agent_Id, Fitness, false, S);
%This clause accepts the cast signals sent by the agents which terminate after finishing with their evaluations. The clause specialises in the "competition" selection algorithm, which is a generational selection algorithm. As a generation selection algorithm, it waits untill the entire population has finished being evaluated, and only then selects the fit from the unfit, and creates the updated population of the next generation. The OpTag can be set from the outsie to shutdown the population_monitor by setting it to done. Once an ending condition is reached, either through a generation limit, an evaluations limit, or fitness goal, the population_monitor exits normally. If the ending condition is not reached, the population_monitor spawns the new generation of agents and awaits again for all the agents in the population to complete their evaluations. If the OpTag is set to pause, it does not generate a new population, and instead goes into a waiting mode, and awaits to be restarted or terminated.

	handle_cast({op_tag,pause},S) when S#state.op_tag == continue ->
	U_S = S#state{op_tag = pause},
	{noreply,U_S};
%The population_monitor process accepts a pause command cast, which if it recieves, it then goes into pause mode after all the agents have completed with their evaluations. The process can only go into pause mode if it is currently in the continue mode (its op_tag is set to continue).

	handle_cast({op_tag,continue},S) when S#state.op_tag == pause ->
		Population_Id = S#state.population_id,
		OpMode = S#state.op_mode,
		Agent_Ids = extract_AgentIds(Population_Id,all),
		SpawnedAgents = summon_agents(OpMode,Agent_Ids),
		TotAgents=length(Agent_Ids),
		U_S0=S#state{
			tot_agents=TotAgents,
			agents_left=TotAgents,
			op_tag=continue,
			inflight_evictions = #{},
			restart_queue = queue:new()
		},
		U_S = register_spawned_agents(SpawnedAgents, U_S0),
		{noreply,U_S};
%The population_monitor process can accept a continue command if its current op_tag is set to pause. When it receives a continue command, it summons all the agents in the population, and continues with its neuroevolution synchronization duties.

handle_cast({agent_runtime_event,RunRef,Agent_Id,evaluations,EventData},S)->
	% Phase 1: Handle managed agent evaluation events
	case maps:get(Agent_Id, S#state.agent_id_to_runref, undefined) of
		RunRef ->
			% Valid event from current run
			Specie_Id = maps:get(specie_id, EventData),
			AgentEvalAcc = maps:get(evals, EventData, 0),
			AgentCycleAcc = maps:get(cycles, EventData, 0),
			AgentTimeAcc = maps:get(time, EventData, 0),
			handle_evaluations(Specie_Id, AgentEvalAcc, AgentCycleAcc, AgentTimeAcc, S);
		_ ->
			% Stale event, ignore
			{noreply, S}
	end;

handle_cast({_From,evaluations,Specie_Id,AEA,AgentCycleAcc,AgentTimeAcc},S)->
	% Legacy unmanaged evaluation event
	handle_evaluations(Specie_Id, AEA, AgentCycleAcc, AgentTimeAcc, S);

handle_cast({_From,goal_reached},S)->
	U_S=S#state{goal_reached=true},
	{noreply,U_S};

handle_cast({agent_runtime_event,RunRef,Agent_Id,goal_reached,_EventData},S)->
	% Phase 1: Handle managed agent goal_reached event
	case maps:get(Agent_Id, S#state.agent_id_to_runref, undefined) of
		RunRef ->
			U_S=S#state{goal_reached=true},
			{noreply,U_S};
		_ ->
			{noreply, S}
	end;

handle_cast({auditor_evict_candidate, AgentId, AgentBytes, TotalMemory, _ProcessMemory}, S) ->
	% Phase 4: Handle auditor eviction suggestion
	CurrentStatus = maps:get(AgentId, S#state.runtime_status, undefined),
	InFlight = maps:is_key(AgentId, S#state.inflight_evictions),
	Reason = {memory_pressure, TotalMemory, AgentBytes},
	case {CurrentStatus, InFlight} of
		{running, false} ->
			io:format("Auditor suggests evicting Agent ~p (~p bytes)~n", [AgentId, AgentBytes]),
			case maps:get(AgentId, S#state.agent_id_to_pid, undefined) of
				Pid when is_pid(Pid) ->
					{noreply, evict_running_agent(AgentId, Reason, S)};
				_ ->
					io:format("Cannot evict Agent ~p: PID not found~n", [AgentId]),
					{noreply, S}
			end;
		{running, true} ->
			io:format("Ignoring duplicate eviction request for Agent ~p~n", [AgentId]),
			{noreply, S};
		{OtherStatus, _} ->
			io:format("Ignoring eviction request for Agent ~p in status ~p~n", [AgentId, OtherStatus]),
			{noreply, S}
	end;

handle_cast({auditor_restart_permit, RestartBatchSize, TotalMemory, ProcessMemory}, S) ->
	% Phase 6: Handle auditor restart permit (hysteresis - only restart when below low water)
	QueueLength = queue:len(S#state.restart_queue),
	case QueueLength of
		0 ->
			% No agents waiting to restart
			{noreply, S};
		_ ->
			io:format("Auditor permits restart of ~p agents (memory: ~p/~p, queue: ~p)~n", 
				[RestartBatchSize, ProcessMemory, TotalMemory, QueueLength]),
			qlog:benchmarker(S#state.population_id, 
				io_lib:format("RESTART_PERMIT | batch_size=~p | total_memory=~p | process_memory=~p | queue_length=~p", 
					[RestartBatchSize, TotalMemory, ProcessMemory, QueueLength])),
			U_S = restart_batch(RestartBatchSize, S),
			{noreply, U_S}
	end;

handle_cast({_From,print_TRACE},S)->
	Population_Id = S#state.population_id,
	P = genotype:dirty_read({population,Population_Id}),
	io:format("******** TRACE ********:~n~p~n",[P#population.trace]),
	{noreply,S};

handle_cast({init,InitState},_State)->
	{noreply,InitState};
handle_cast({stop,normal},State)->
	{stop, normal,State};
handle_cast({stop,shutdown},State)->
	{stop, shutdown, State}.

%% Helper function for agent completion (both managed and unmanaged)
handle_agent_completion(Agent_Id, Fitness, Managed, S) ->
	case S#state.evolutionary_algorithm of
		generational ->
			handle_generational_completion(Agent_Id, Fitness, Managed, S);
		steady_state ->
			handle_steady_state_completion(Agent_Id, Fitness, Managed, S)
	end.

handle_generational_completion(Agent_Id, _Fitness, Managed, S) ->
	Population_Id = S#state.population_id,
	OpTag = S#state.op_tag,
	OpMode = S#state.op_mode,
	AgentsLeft = S#state.agents_left,
	S1 = cleanup_completed_agent(Agent_Id, Managed, S),
	case (AgentsLeft-1) =< 0 of
		true ->
			CurrentGen = S1#state.pop_gen,
			qlog:exp_runner(generation_end, {Population_Id, CurrentGen, S1#state.tot_agents}),
			qlog:agent_trades_generation(generation_end, {Population_Id, CurrentGen, S1#state.tot_agents}),
			qlog:benchmarker(Population_Id,io_lib:format("GENERATION_END | population_id=~p | generation=~p | total_agents=~p",[Population_Id, CurrentGen, S1#state.tot_agents])),
			mutate_population(Population_Id,S1#state.specie_size_limit,S1#state.fitness_postprocessor,S1#state.selection_algorithm,S1#state.survival_percentage),
			U_PopGen = S1#state.pop_gen+1,
			qlog:benchmarker(Population_Id,io_lib:format("GENERATION_START | population_id=~p | generation=~p | total_agents=~p",[Population_Id, U_PopGen, S1#state.tot_agents])),
			qlog:agent_trades_generation(generation_start, {Population_Id, U_PopGen, S1#state.tot_agents}),
			qlog:exp_runner(generation_start, {Population_Id, U_PopGen, S1#state.tot_agents}),
			case OpTag of
				continue ->
					Specie_Ids = (genotype:dirty_read({population,Population_Id}))#population.specie_ids,
					SpecFitList=[(genotype:dirty_read({specie,Specie_Id}))#specie.fitness || Specie_Id <- Specie_Ids],
					BestFitness=lists:nth(1,lists:reverse(lists:sort([MaxFitness || {_,_,MaxFitness,_} <- SpecFitList]))),
					Generation_Limit=S1#state.generation_limit,
					Evaluation_Limit=S1#state.evaluations_limit,
					Fitness_Goal=S1#state.fitness_goal,
					qlog:benchmarker(Population_Id,io_lib:format("Total Evaluations: ~p, Total Generations: ~p", [S1#state.tot_evaluations, U_PopGen])),
					case (U_PopGen >= Generation_Limit) or (S1#state.tot_evaluations >= Evaluation_Limit) or (BestFitness >= Fitness_Goal) or S1#state.goal_reached of
						true ->
							FinalAgent_Ids = extract_AgentIds(Population_Id,all),
							FinalTotAgents = length(FinalAgent_Ids),
							qlog:exp_runner(generation_end, {Population_Id, U_PopGen, FinalTotAgents}),
							qlog:agent_trades_generation(generation_end, {Population_Id, U_PopGen, FinalTotAgents}),
							qlog:benchmarker(Population_Id,io_lib:format("GENERATION_END | population_id=~p | generation=~p | total_agents=~p",[Population_Id, U_PopGen, FinalTotAgents])),
							TotAgents=FinalTotAgents,
							U_S=S1#state{agent_ids=FinalAgent_Ids, tot_agents=TotAgents, agents_left=TotAgents, pop_gen=U_PopGen},
							{stop,normal,U_S};
						false ->
							Agent_Ids = extract_AgentIds(Population_Id,all),
							SpawnedAgents = summon_agents(OpMode,Agent_Ids),
							TotAgents=length(Agent_Ids),
							U_S0=S1#state{
								tot_agents=TotAgents,
								agents_left=TotAgents,
								pop_gen=U_PopGen,
								inflight_evictions = #{},
								restart_queue = queue:new()
							},
							U_S = register_spawned_agents(SpawnedAgents, U_S0),
							{noreply,U_S}
					end;
				done ->
					io:format("Shutting down Population Monitor~n"),
					U_S = S1#state{agents_left = 0,pop_gen=U_PopGen},
					{stop,normal,U_S};
				pause ->
					io:format("Population Monitor has paused.~n"),
					U_S = S1#state{agents_left=0,pop_gen=U_PopGen},
					{noreply,U_S}
			end;
		false ->
			io:format("Agents Left:~p~n",[AgentsLeft-1]),
			U_S = S1#state{agents_left = AgentsLeft-1},
			{noreply,U_S}
	end.

handle_steady_state_completion(Agent_Id, Fitness, Managed, S) ->
	Evaluation_Limit = S#state.evaluations_limit,
	Fitness_Goal = S#state.fitness_goal,
	NewBestFitness = case Fitness >= S#state.best_fitness of
		true ->
			Fitness;
		false ->
			S#state.best_fitness
	end,
	S1 = cleanup_completed_agent(Agent_Id, Managed, S),
	case (S#state.tot_evaluations >= Evaluation_Limit) or (NewBestFitness > Fitness_Goal) or S#state.goal_reached of
		true ->
			case S1#state.activeAgent_IdPs of
				[] ->
					U_S = S1#state{best_fitness = NewBestFitness},
					{stop,normal,U_S};
				_ ->
					U_S = S1#state{best_fitness = NewBestFitness},
					{noreply,U_S}
			end;
		false ->
			FitnessPostprocessorName = S#state.fitness_postprocessor,
			SelectionAlgorithmName = S#state.selection_algorithm,
			A = genotype:dirty_read({agent,Agent_Id}),
			Specie_Id = A#agent.specie_id,
			Specie = genotype:dirty_read({specie,Specie_Id}),
			Old_DeadPool_AgentSummaries = Specie#specie.dead_pool,
			[AgentSummary] = construct_AgentSummaries([Agent_Id],[]),
			DeadPool_AgentSummaries = [AgentSummary|Old_DeadPool_AgentSummaries],
			ProperlySorted_AgentSummaries = fitness_postprocessor:FitnessPostprocessorName(DeadPool_AgentSummaries),
			Valid_AgentSummaries = case length(ProperlySorted_AgentSummaries) >= S#state.specie_size_limit of
				true ->
					[{_InvalidFitness,_InvalidTotN,InvalidAgent_Id}|Remaining_AgentSummaries] = lists:reverse(ProperlySorted_AgentSummaries),
					genotype:delete_Agent(InvalidAgent_Id,safe),
					lists:reverse(Remaining_AgentSummaries);
				false ->
					ProperlySorted_AgentSummaries
			end,
			io:format("Valid_AgentSummaries:~p~n",[Valid_AgentSummaries]),
			{WinnerFitness,WinnerProfile,WinnerAgent_Id} = selection_algorithm:SelectionAlgorithmName(Valid_AgentSummaries),
			{U_DeadPool_AgentSummaries, U_S2} = case rand:uniform() < 0.1 of
				true ->
					U_DeadPool = lists:delete({WinnerFitness,WinnerProfile,WinnerAgent_Id},Valid_AgentSummaries),
					case Managed of
						true ->
							{_WinnerAgent_PId, _RunRef, _MonRef, U_Spawned} = spawn_managed_agent(WinnerAgent_Id, S#state.op_mode, S1),
							{U_DeadPool, U_Spawned};
						false ->
							WinnerAgent_PId = exoself:start(WinnerAgent_Id,self(),agent_run_mode(S#state.op_mode)),
							{U_DeadPool, S1#state{activeAgent_IdPs = [{WinnerAgent_Id,WinnerAgent_PId}|S1#state.activeAgent_IdPs]}}
					end;
				false ->
					CloneAgent_Id = create_MutantAgentCopy(WinnerAgent_Id,safe),
					case Managed of
						true ->
							{_CloneAgent_PId, _RunRef, _MonRef, U_Spawned} = spawn_managed_agent(CloneAgent_Id, S#state.op_mode, S1),
							{Valid_AgentSummaries, U_Spawned};
						false ->
							CloneAgent_PId = exoself:start(CloneAgent_Id,self(),agent_run_mode(S#state.op_mode)),
							{Valid_AgentSummaries, S1#state{activeAgent_IdPs = [{CloneAgent_Id,CloneAgent_PId}|S1#state.activeAgent_IdPs]}}
					end
			end,
			Top_AgentSummaries = lists:sublist(U_DeadPool_AgentSummaries,round(S#state.specie_size_limit*S#state.survival_percentage)),
			{_,_,TopAgent_Ids} = lists:unzip3(lists:sublist(Top_AgentSummaries,3)),
			USpecie=genotype:dirty_read({specie,Specie_Id}),
			genotype:write(USpecie#specie{dead_pool = U_DeadPool_AgentSummaries,champion_ids = TopAgent_Ids}),
			U_S=U_S2#state{best_fitness=NewBestFitness},
			{noreply,U_S}
	end.

%% Helper function for evaluation events (both managed and unmanaged)
handle_evaluations(Specie_Id, AEA, AgentCycleAcc, AgentTimeAcc, S) ->
	AgentEvalAcc = case S#state.goal_reached of
		true ->
			0;
		false ->
			AEA
	end,
	Eval_Acc = S#state.eval_acc,
	U_EvalAcc = S#state.eval_acc+AgentEvalAcc,
	U_CycleAcc = S#state.cycle_acc+AgentCycleAcc,
	U_TimeAcc = S#state.time_acc+AgentTimeAcc,
	U_TotEvaluations = S#state.tot_evaluations + AgentEvalAcc,
	SEval_Acc=get({evaluations,Specie_Id}),
	put({evaluations,Specie_Id},SEval_Acc+AgentEvalAcc),
	io:format("Eval_Acc:~p~n",[Eval_Acc]),
	case Eval_Acc rem S#state.step_size of
		0 ->
			io:format("Evaluations/Step:~p~n",[Eval_Acc]);
		_ ->
			done
	end,
	U_S=case U_EvalAcc >= S#state.step_size of 
		true ->
			gather_STATS(S#state.population_id,U_EvalAcc,S#state.op_mode),
			Population_Id = S#state.population_id,
			P = genotype:dirty_read({population,Population_Id}),
			T = P#population.trace,
			TotEvaluations=T#trace.tot_evaluations,
			io:format("Tot Evaluations:~p~n",[TotEvaluations]),
			S#state{eval_acc=0, cycle_acc=0, time_acc=0, tot_evaluations=U_TotEvaluations};
		false ->
			io:format("Eval_Acc:~p~n",[U_EvalAcc]),
			S#state{eval_acc=U_EvalAcc,cycle_acc=U_CycleAcc,time_acc=U_TimeAcc,tot_evaluations=U_TotEvaluations}
	end,
	{noreply,U_S}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({eviction_grace_expired, AgentId, RunRef}, S) ->
	% Phase 4: Grace period expired, force kill if still running
	case current_run_matches(AgentId, RunRef, S) of
		true ->
			CurrentStatus = maps:get(AgentId, S#state.runtime_status, undefined),
				case CurrentStatus of
					evicting ->
						% Agent didn't exit gracefully, force kill
						AgentBytes = current_agent_memory_bytes(AgentId, S),
						io:format("Grace period expired for Agent ~p, forcing kill~n", [AgentId]),
						qlog:xLog(qStatus, "GRACE_TIMEOUT | population_id=~p | agent_id=~p | run_ref=~p | agent_bytes=~p | grace_ms=~p", [S#state.population_id, AgentId, RunRef, AgentBytes, config:auditor_grace_ms()]),
						% Phase 8: Log forced kill after grace timeout
						qlog:benchmarker(S#state.population_id, io_lib:format("FORCED_KILL_AFTER_GRACE | agent_id=~p | run_ref=~p | grace_ms=~p", [AgentId, RunRef, config:auditor_grace_ms()])),
						UpdatedEvictions = case maps:get(AgentId, S#state.inflight_evictions, undefined) of
							undefined ->
								S#state.inflight_evictions;
						EvictionData ->
							maps:put(AgentId, EvictionData#{reason => forced_after_grace_timeout}, S#state.inflight_evictions)
					end,
					U_S1 = mark_agent_status(AgentId, evicting, forced_after_grace_timeout, S),
					U_S2 = U_S1#state{inflight_evictions = UpdatedEvictions},
					U_S = force_kill_owned_pids(AgentId, U_S2),
					{noreply, U_S};
				_ ->
					% Agent already exited, ignore
					io:format("Grace timer fired for Agent ~p but status is ~p, ignoring~n", [AgentId, CurrentStatus]),
					{noreply, S}
			end;
		false ->
			% Stale timer from old run
			io:format("Ignoring stale grace timer for Agent ~p~n", [AgentId]),
			{noreply, S}
	end;
handle_info({_ExoSelfPid, manifest_published, Agent_Id, RunRef}, S) ->
	% Phase 2: Update registry with MonitorRef after manifest is published
	case maps:get(Agent_Id, S#state.agent_id_to_runref, undefined) of
		RunRef ->
			% Valid manifest from current run
			MonRef = maps:get(Agent_Id, S#state.agent_id_to_ref, undefined),
			PopulationId = S#state.population_id,
			case agent_runtime_registry:get_agent(PopulationId, Agent_Id) of
				{ok, AgentData} ->
					agent_runtime_registry:update_monitor_ref(PopulationId, Agent_Id, MonRef),
					% Phase 8: Log restart completed manifest publish (if this is a restart)
					CurrentStatus = maps:get(Agent_Id, S#state.runtime_status, undefined),
					case CurrentStatus of
						running ->
							% Check if this is a restart by looking at eviction count
							EvictionCount = maps:get(eviction_count, AgentData, 0),
							case EvictionCount > 0 of
								true ->
									qlog:benchmarker(PopulationId, io_lib:format("RESTART_COMPLETED | agent_id=~p | run_ref=~p | eviction_count=~p", [Agent_Id, RunRef, EvictionCount]));
								false ->
									ok % Initial spawn, not a restart
							end;
						_ ->
							ok
					end;
				{error, not_found} ->
					ok
			end;
		_ ->
			% Stale manifest from old run, ignore
			ok
	end,
	{noreply, S};
handle_info({From, benchmark_complete, Specie_Id, Fitness, Cycles, Time}, S) ->
	handle_terminal_mode_completion(From, Specie_Id, Fitness, Cycles, Time, S);
handle_info({From, live_trading_complete, Specie_Id, Fitness, Cycles, Time}, S) ->
	handle_terminal_mode_completion(From, Specie_Id, Fitness, Cycles, Time, S);
handle_info({'DOWN', _MonRef, process, Pid, Reason}, S) ->
	% Phase 4: Handle monitored exoself process death
	case maps:get(Pid, S#state.agent_pid_to_id, undefined) of
		undefined ->
			% Not a managed agent, ignore
			{noreply, S};
		Agent_Id ->
			io:format("Monitored agent ~p (PID ~p) died with reason: ~p~n", [Agent_Id, Pid, Reason]),
			% Check current status
			CurrentStatus = maps:get(Agent_Id, S#state.runtime_status, undefined),
			case CurrentStatus of
				evicting ->
					EvictionReason = case maps:get(Agent_Id, S#state.inflight_evictions, undefined) of
						undefined ->
							down_during_eviction;
						EvictionData ->
							maps:get(reason, EvictionData, down_during_eviction)
					end,
					io:format("Agent ~p exited during eviction (reason: ~p)~n", [Agent_Id, EvictionReason]),
					qlog:benchmarker(S#state.population_id, io_lib:format("EVICTION_PROCESS_DOWN | agent_id=~p | pid=~p | reason=~p | status=finalized", [Agent_Id, Pid, EvictionReason])),
					{noreply, finalize_eviction(Agent_Id, EvictionReason, S)};
				completed ->
					% Already marked as completed, this is expected
					{noreply, S};
				running ->
					% Unexpected death, treat as crash
					io:format("Agent ~p crashed unexpectedly, enqueueing restart~n", [Agent_Id]),
					% Phase 8: Log crash detected
					qlog:benchmarker(S#state.population_id, io_lib:format("CRASH_DETECTED | agent_id=~p | pid=~p | reason=~p", [Agent_Id, Pid, Reason])),
					U_S1 = mark_agent_status(Agent_Id, crashed, Reason, S),
					U_S2 = remove_running_agent(Agent_Id, U_S1),
					U_S3 = enqueue_restart(Agent_Id, U_S2),
					% Don't decrement agents_left for crashes - they will restart
					{noreply, U_S3};
				_ ->
					io:format("Agent ~p died in status ~p~n", [Agent_Id, CurrentStatus]),
					{noreply, S}
			end
	end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, S) ->
	case S of
		[] ->
			io:format("******** Population_Monitor shut down with Reason:~p, with State: []~n",[Reason]);
		_ ->
			% Phase 5: Stop auditor if running
			case S#state.auditor_pid of
				undefined -> ok;
				AuditorPid when is_pid(AuditorPid) ->
					agent_auditor:stop(AuditorPid),
					qlog:benchmarker(S#state.population_id, io_lib:format("AUDITOR_STOPPED | auditor_pid=~p", [AuditorPid]))
			end,
			% Phase 2: Clear runtime registry for this population
			Population_Id = S#state.population_id,
			agent_runtime_registry:clear_population(Population_Id),
			OpMode = S#state.op_mode,
			OpTag = S#state.op_tag,
			TotEvaluations=S#state.tot_evaluations,
			case TotEvaluations < 500 of
				true ->%So that there is at least one stat in the stats list.
					gather_STATS(Population_Id,0,OpMode);
				false ->
					ok
			end,
			P = genotype:dirty_read({population,Population_Id}),
			T = P#population.trace,
			U_T = T#trace{tot_evaluations=TotEvaluations},
			U_P = P#population{trace=U_T},
			genotype:write(U_P),
			io:format("******** TRACE START ********~n"),
			io:format("~p~n",[U_T]),
			io:format("******** ^^^^ TRACE END ^^^^ ********~n"),
			io:format("******** Population_Monitor:~p shut down with Reason:~p OpTag:~p, while in OpMode:~p~n",[Population_Id,Reason,OpTag,OpMode]),
			io:format("******** Tot Agents:~p Population Generation:~p Tot_Evals:~p~n",[S#state.tot_agents,S#state.pop_gen,S#state.tot_evaluations]),
			qlog:benchmarker(Population_Id,io_lib:format("POP_MONITOR_TERMINATED | reason=~p | op_tag=~p | op_mode=~p | total_agents=~p | population_generation=~p | total_evaluations=~p",[Reason,OpTag,OpMode,S#state.tot_agents,S#state.pop_gen,S#state.tot_evaluations])),
			%qlog:exp_runner(population_monitor_terminated, {Population_Id, Reason, OpTag, OpMode, S#state.tot_agents, S#state.pop_gen, S#state.tot_evaluations}),
			case S#state.benchmarker_pid of
				undefined ->
					ok;
				PId ->
					PId ! {S#state.population_id,completed,U_T}
			end
	end.

handle_terminal_mode_completion(From, Specie_Id, Fitness, Cycles, Time, S) ->
	case lookup_agent_completion_context(From, S) of
		{ok, Agent_Id, Managed} ->
			{noreply, S1} = handle_evaluations(Specie_Id, 1, Cycles, Time, S),
			S2 = case Managed of
				true ->
					mark_agent_status(Agent_Id, completed, normal_completion, S1);
				false ->
					S1
			end,
			handle_agent_completion(Agent_Id, Fitness, Managed, S2);
		error ->
			io:format("Ignoring completion from unknown agent pid ~p~n", [From]),
			{noreply, S}
	end.

lookup_agent_completion_context(Pid, S) ->
	case maps:get(Pid, S#state.agent_pid_to_id, undefined) of
		undefined ->
			case lists:keyfind(Pid, 2, S#state.activeAgent_IdPs) of
				{AgentId, Pid} ->
					{ok, AgentId, false};
				false ->
					error
			end;
		AgentId ->
			{ok, AgentId, true}
	end.
%When the population_monitor process terminates, it states so, notifies with what op_tag and op_mode it terminated, all the stats gathered, and then shuts down.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
extract_AgentIds(Population_Id,AgentType)->
	P = genotype:dirty_read({population,Population_Id}),
	Specie_Ids = P#population.specie_ids,
	%io:format("Specie_Ids:~p~n",[Specie_Ids]),
	case AgentType of
		champions ->
			extract_ChampionAgentIds(Specie_Ids,[]);
		all ->
			extract_AllAgentIds(Specie_Ids,[])
	end.
%The extract_AgentIds/2 function accepts the Population_Id and a parameter which specifies what type of agents (all agent ids, or just those of the champions) to extract from the population, after which it extracts those agents. Depending on the AgentType parameter, the function either calls extract_ChampionAgentIds/2 or extract_AllAgentIds/2, which return the list of agent ids to the caller.

	extract_ChampionAgentIds([Specie_Id|Specie_Ids],Acc)->
		S = genotype:dirty_read({specie,Specie_Id}),
		ChampionAgent_Ids = S#specie.champion_ids,
		extract_ChampionAgentIds(Specie_Ids,lists:append(ChampionAgent_Ids,Acc));
	extract_ChampionAgentIds([],Acc)->
		Acc.
%extract_ChampionAgentIds/2 accumulates the ids of champion agents from every specie in the Specie_Ids list, and then returns that list to the caller.
	
	extract_AllAgentIds([Specie_Id|Specie_Ids],Acc)->
		extract_AllAgentIds(Specie_Ids,lists:append(extract_SpecieAgentIds(Specie_Id),Acc));
	extract_AllAgentIds([],Acc)->
		Acc.
%extract_AllAgentIds/2 accumulates and returns to the caller an id list of all the agents belonging to the species in the Specie_Ids list.
		
extract_SpecieAgentIds(Specie_Id)->
	S = genotype:dirty_read({specie,Specie_Id}),
	S#specie.agent_ids.
%extract_SpecieAgentIds/1 returns a list of agent ids to the caller.

summon_agents(OpMode,Agent_Ids)->
	io:format("Summoning agents:~p with OpMode:~p~n",[Agent_Ids,OpMode]),
	qlog:benchmarker(self(),io_lib:format("SUMMONING_AGENTS | op_mode=~p | agent_ids=~p",[OpMode,Agent_Ids])),
	qlog:xLog(qStatus, "SUMMONING_AGENTS | op_mode=~p", [OpMode]),
	summon_agents(OpMode,Agent_Ids,[]).
summon_agents(OpMode,[Agent_Id|Agent_Ids],Acc)->
	RunRef = make_ref(),
	Agent_PId = exoself:start(Agent_Id,self(),agent_run_mode(OpMode),RunRef),
	MonRef = erlang:monitor(process, Agent_PId),
	summon_agents(OpMode,Agent_Ids,[{Agent_Id,Agent_PId,RunRef,MonRef}|Acc]);
summon_agents(_OpMode,[],Acc)->
	Acc.
%The summon_agents/2 and summon_agents/3 spawns all the agents in the Agent_ids list, and returns to the caller a list of tuples as follows: [{Agent_Id,Agent_PId,RunRef,MonRef}...].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test()->
	init_population(#state{},get_init_constraints()).
%The test/0 function starts the population monitor through init_population/1 with a set of default parameters specified by the macros of this module.

prep_PopState(PMP,Specie_Constraints)->
	#state{
		op_mode=PMP#pmp.op_mode,
		population_id = PMP#pmp.population_id,
		survival_percentage=PMP#pmp.survival_percentage,
		specie_size_limit=PMP#pmp.specie_size_limit,
		init_specie_size=PMP#pmp.init_specie_size,
		polis_id=PMP#pmp.polis_id,
		generation_limit=PMP#pmp.generation_limit,
		evaluations_limit=PMP#pmp.evaluations_limit,
		fitness_goal=PMP#pmp.fitness_goal,
		benchmarker_pid=PMP#pmp.benchmarker_pid
	}.

init_population(Init_State,Specie_Constraints)->
	Population_Id = Init_State#state.population_id,
	F = fun()->
		case genotype:read({population,Population_Id}) of
			undefined ->
				create_Population(Population_Id,Init_State#state.init_specie_size,Specie_Constraints);
			_ ->
				delete_population(Population_Id),
				create_Population(Population_Id,Init_State#state.init_specie_size,Specie_Constraints)
		end
	end,
	Result = mnesia:transaction(F),
	case Result of
		{atomic,_} ->
			population_monitor:start(Init_State);
		Error ->
			io:format("******** ERROR in PopulationMonitor:~p~n",[Error])
	end.
%The function init_population/1 creates a new population with the id Population_Id, composed of length(Specie_Constraints) species, where each specie uses the particular specie constraint specified within the Specie_Constraints list. The function first checks if a population with the noted Population_Id already exists, if a population does exist, then the function first delets it, and then creates a fresh one. Since the ids are usually generated with the genotype:create_UniqueId/0, the only way an already existing Population_Id is dropped into the function as a parameter is if it is intended, usually when runing tests, with the Population_Id = test.

	create_Population(Population_Id,SpecieSize,Specie_Constraints)->
		qlog:benchmarker(Population_Id,io_lib:format("CREATING_POPULATION | population_id=~p | specie_count=~p",[Population_Id,length(Specie_Constraints)])),
		Specie_Ids = [create_specie(Population_Id,SpecCon,origin,SpecieSize) || SpecCon <- Specie_Constraints],
		[C|_]=Specie_Constraints,
		Population = #population{
			id = Population_Id,
			specie_ids = Specie_Ids,
			evo_alg_f = C#constraint.population_evo_alg_f,
			fitness_postprocessor_f = C#constraint.population_fitness_postprocessor_f,
			selection_f = C#constraint.population_selection_f
		},
		genotype:write(Population).

		create_specie(Population_Id,SpeCon,Fingerprint)->
			Specie_Id = genotype:generate_UniqueId(),
			create_specie(Population_Id,Specie_Id,0,[],SpeCon,Fingerprint).
		create_specie(Population_Id,SpeCon,Fingerprint,SpecieSize)->
			Specie_Id = genotype:generate_UniqueId(),
			create_specie(Population_Id,Specie_Id,SpecieSize,[],SpeCon,Fingerprint).
		create_specie(Population_Id,Specie_Id,0,IdAcc,SpeCon,Fingerprint)->
			Specie = #specie{
				id = Specie_Id,
				population_id = Population_Id,
				fingerprint = Fingerprint,
				constraint = SpeCon,
				agent_ids = IdAcc
			},
			genotype:write(Specie),
			Specie_Id;
		create_specie(Population_Id,Specie_Id,Agent_Index,IdAcc,SpeCon,Fingerprint)->
			Agent_Id = {genotype:generate_UniqueId(),agent},
			genotype:construct_Agent(Specie_Id,Agent_Id,SpeCon,Population_Id),
			create_specie(Population_Id,Specie_Id,Agent_Index-1,[Agent_Id|IdAcc],SpeCon,Fingerprint).
%The create_Population/3 generates length(Specie_Constraints) number of specie, each composed of ?INIT_SPECIE_SIZE number of agents. The function uses the create_specie/4 to generate the species. The create_specie/3 and create_specie/4 functions are simplified versions which use default parameters to call the create_specie/6 function. The create_specie/6 function constructs the agents using the genotype:construct_Agent/3 function, accumulating the Agent_Ids in the IdAcc list. Once all the agents have been created, the function creates the specie record, fills in the required elements, writes the specie to database, and then finally returns the Specie_Id to the caller.

continue()->
	population_monitor:start(#state{op_mode = [gt,benchmark]}).
continue(Population_Id)->
	S = #state{population_id=Population_Id,op_mode = [gt,benchmark]},
	population_monitor:start(S).
%The function continue/0 and continue/1 are used to summon an already existing population with either the default population Id, or the specified Population_Id.

mutate_population(Population_Id,KeepTot,Fitness_Postprocessor,Selection_Algorithm,Survival_Percentage)->
	qlog:benchmarker(Population_Id,io_lib:format("MUTATING_POPULATION | population_id=~p | keep_total=~p | fitness_postprocessor=~p | selection_algorithm=~p | survival_percentage=~p",[Population_Id,KeepTot,Fitness_Postprocessor,Selection_Algorithm,Survival_Percentage])),
	NeuralEnergyCost = calculate_EnergyCost(Population_Id),
	P = genotype:dirty_read({population,Population_Id}),
	Specie_Ids = P#population.specie_ids,
	[mutate_Specie(Specie_Id,KeepTot,NeuralEnergyCost,Fitness_Postprocessor,Selection_Algorithm,Survival_Percentage) || Specie_Id <- Specie_Ids],
	ok.
%The function mutate_population/3 mutates the agents within every specie in its specie_ids list, maintianing each specie within the size of KeepTot. The function first calculates the average cost of each neuron, and then calls each specie seperately with the Fitness_Postprocessor and Selection_Algorithm parameters, which are used to mutate the species.

	mutate_Specie(Specie_Id,PopulationLimit,NeuralEnergyCost,Fitness_Postprocessor_Name,Selection_Algorithm_Name,Survival_Percentage)->
		qlog:benchmarker(Specie_Id,io_lib:format("MUTATING_SPECIE | specie_id=~p | population_limit=~p | neural_energy_cost=~p | fitness_postprocessor=~p | selection_algorithm=~p | survival_percentage=~p",[Specie_Id,PopulationLimit,NeuralEnergyCost,Fitness_Postprocessor_Name,Selection_Algorithm_Name,Survival_Percentage])),
		S = genotype:dirty_read({specie,Specie_Id}),
		{AvgFitness,Std,MaxFitness,MinFitness} = calculate_SpecieFitness({specie,S}),
		qlog:benchmarker(Specie_Id,io_lib:format("Specie=~p | Agents: ~p | Population_limit=~p | Avg Fitness=~p Max=~p Min=~p | Neural Energy Cost=~p",[Specie_Id,length(S#specie.agent_ids),PopulationLimit,AvgFitness,MaxFitness,MinFitness,NeuralEnergyCost])),
		% Removed mutate_specie logging for cleaner exp_runner.log
		% spawn(fun() -> qlog:exp_runner(mutate_specie, {Specie_Id,PopulationLimit,AvgFitness,MaxFitness,MinFitness,NeuralEnergyCost}) end),
		Agent_Ids = S#specie.agent_ids,
		Sorted_AgentSummaries = lists:reverse(lists:sort(construct_AgentSummaries(Agent_Ids,[]))),
		io:format("Using: Fitness Postprocessor:~p Selection Algorirthm:~p~n",[Fitness_Postprocessor_Name,Selection_Algorithm_Name]),
		ProperlySorted_AgentSummaries = fitness_postprocessor:Fitness_Postprocessor_Name(Sorted_AgentSummaries),
		{NewGenAgent_Ids,TopAgent_Ids} = selection_algorithm:Selection_Algorithm_Name(ProperlySorted_AgentSummaries,NeuralEnergyCost,PopulationLimit,Survival_Percentage),
		{FList,_TNList,_AgentIds}=lists:unzip3(Sorted_AgentSummaries),
		[TopFitness|_] = FList,
		{Factor,Fitness}=S#specie.innovation_factor,
		U_InnovationFactor = case TopFitness > Fitness of
			true ->
				{0,TopFitness};
			false ->
				{Factor-1,Fitness}
		end,
		F = fun()->
			case mnesia:read({specie,Specie_Id}) of
				[US] ->
					mnesia:write(US#specie{
						agent_ids = NewGenAgent_Ids,
						champion_ids = TopAgent_Ids,
						fitness = {AvgFitness,Std,MaxFitness,MinFitness},
						innovation_factor = U_InnovationFactor});
				[] ->
					mnesia:abort({specie_not_found,Specie_Id})
			end
		end,
		{atomic,_} = mnesia:transaction(F).
		% Log genotypes for all new generation agents
		%Population_Id = S#specie.population_id,
		%Run_Id = qlog:get_run_id_from_population_id(Population_Id),
		%[begin
		%	A = genotype:dirty_read({agent, Agent_Id}),
		%	qlog:genotype_log(Run_Id, A#agent.generation, Agent_Id)
		%end || Agent_Id <- NewGenAgent_Ids].
%The function mutate_Specie/5 calls the selection algorithm function to seperate the fit from the unfit organisms in the specie, and then mutates the fit organisms to produce offspring, maintaning the total specie size within PopulationLimit. The function first calls the fitness_postprocessor which sorts the agent summaries. Then, the resorted updated summaries are split into a valid (fit) and invalid (unfit) lists of agents by the selection algorithm. The invalid agents are deleted, and the valid agents are used to create offspring using the particular Selection_Algorithm_Name function. The agent ids belonging to the next generation (the valid agents and their offspring) are then produced by the selection function. Finally, the innovation factor (the last time the specie's top fitness improved) is updated, the ids of the 3 top agents within the species are noted, and the updated specie record is written to database.

	construct_AgentSummaries([Agent_Id|Agent_Ids],Acc)->
		A = genotype:dirty_read({agent,Agent_Id}),
		construct_AgentSummaries(Agent_Ids,[{A#agent.fitness,length((genotype:dirty_read({cortex,A#agent.cx_id}))#cortex.neuron_ids),Agent_Id}|Acc]);
	construct_AgentSummaries([],Acc)->
		Acc.
%The construct_AgentSummaries/2 reads the agents in the Agent_Ids list, and composes a list of tuples of the following format: [{AgentFitness,AgentTotNeurons,Agent_Id}...]. This list of tuples is reffered to as AgentSummaries. Once the AgentSummaries list is composed, it is returned to the caller.

	create_MutantAgentCopy(Agent_Id)->
		AgentClone_Id = genotype:clone_Agent(Agent_Id),
		genome_mutator:mutate(AgentClone_Id),
		AgentClone_Id.
%The create_MutantAgentCopy/1 first creates a clone of the Agent_Id, and then uses the genome_mutator:mutate/1 function to mutate that clone, returning the id of the cloned agent to the caller.
				
	create_MutantAgentCopy(Agent_Id,safe)->%TODO
		A = genotype:dirty_read({agent,Agent_Id}),
		S = genotype:dirty_read({specie,A#agent.specie_id}),
		AgentClone_Id = genotype:clone_Agent(Agent_Id),
		qlog:lineage_tracking(Agent_Id, AgentClone_Id, "SAFE_CLONE_CREATED"),
		Agent_Ids = S#specie.agent_ids,
		genotype:write(S#specie{agent_ids = [AgentClone_Id|Agent_Ids]}),
		genome_mutator:mutate(AgentClone_Id),
		AgentClone_Id.
%The create_MutantAgentCopy/2 is similar to arity 1 function of the same name, but it also adds the id of the cloned mutant agent to the specie record to which the original belonged. The specie with its updated agent_ids is then written to database, and the id of the mutant clone is returned to the caller.

delete_population(Population_Id)->
	qlog:benchmarker(Population_Id,io_lib:format("DELETING_POPULATION | population_id=~p",[Population_Id])),
	P = genotype:dirty_read({population,Population_Id}),
	Specie_Ids = P#population.specie_ids,
	[delete_specie(Specie_Id) || Specie_Id <- Specie_Ids],
	mnesia:delete({population,Population_Id}).
%The delete_population/1 function delets the entire population, by deleting the specie records belonging to the Population_Id, deleting the agent records belonging to those species, and then deleting the population record itself.
	
	delete_specie(Specie_Id)->
		qlog:benchmarker(Specie_Id,io_lib:format("DELETING_SPECIE | specie_id=~p",[Specie_Id])),
		S = genotype:dirty_read({specie,Specie_Id}),
		Agent_Ids = S#specie.agent_ids,
		[genotype:delete_Agent(Agent_Id) || Agent_Id <- Agent_Ids],
		mnesia:delete({specie,Specie_Id}).
%The delete_specie/1 function delets the agents associated with the Specie_Id, and then delets the specie record itself.
			
calculate_EnergyCost(Population_Id)->
	Agent_Ids = extract_AgentIds(Population_Id,all),
	TotEnergy = lists:sum([extract_AgentFitness(Agent_Id) || Agent_Id<-Agent_Ids]),
	TotNeurons = lists:sum([extract_AgentTotNeurons(Agent_Id) || Agent_Id <- Agent_Ids]),
	EnergyCost = TotEnergy/TotNeurons,
	EnergyCost.
%The calculate_EnergyCost/1 calculates the average cost of each neuron, based on the fitness of each agent in the population, and the total number of neurons in the population. The value is calcualted by first adding up all the fitness scores of the agents belonging to the population. Then adding up the total number of neurons composing each agent in the population. And then finally producing the EnergyCost value by dividing the TotEnergy by TotNeurons, returning the value to the caller.

	extract_AgentTotNeurons(Agent_Id)->
		A = genotype:dirty_read({agent,Agent_Id}),
		Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
		Neuron_Ids = Cx#cortex.neuron_ids,
		length(Neuron_Ids).
	
	extract_AgentFitness(Agent_Id)->
		A = genotype:dirty_read({agent,Agent_Id}),
		A#agent.fitness.
%The function extract_AgentTotNeurons simply extracts the neuron_ids list, and returns the length of that list, which is the total number of neurons belonging to the NN based system.

calculate_PopulationFitness(Population_Id,[Specie_Id|Specie_Ids],AvgFAcc,MaxFAcc,MinFAcc)->
	{AvgFitness,Std,MaxF,MinF}=calculate_SpecieFitness(Specie_Id),
	case get({fitness,Specie_Id}) of
		undefined ->
			put({fitness,Specie_Id},[{AvgFitness,Std}]);
		PrevGenFitness->
			put({fitness,Specie_Id},[{AvgFitness,Std}|PrevGenFitness])
	end,
	calculate_PopulationFitness(Population_Id,Specie_Ids,[{Specie_Id,AvgFitness}|AvgFAcc],[{Specie_Id,MaxF}|MaxFAcc],[{Specie_Id,MinF}|MinFAcc]);
calculate_PopulationFitness(_Population_Id,[],AvgFAcc,MaxFAcc,MinFAcc)->
	{AvgFAcc,MaxFAcc,MinFAcc}.

	calculate_SpecieFitness({specie,S})->
		Agent_Ids = S#specie.agent_ids,
		FitnessAcc = calculate_fitness(Agent_Ids),
		Sorted_FitnessAcc=lists:sort(FitnessAcc),
		case Sorted_FitnessAcc of
			[] ->
				MinFitness = 0,
				MaxFitness = 0,
				AvgFitness = 0,
				Std = inf;
			[MinFitness] ->
				MaxFitness = MinFitness,
				AvgFitness = MinFitness,
				Std = inf;
			_ ->
				[MinFitness|_] = Sorted_FitnessAcc,
				[MaxFitness|_] = lists:reverse(Sorted_FitnessAcc),
				AvgFitness = functions:avg(FitnessAcc),
				Std = functions:std(FitnessAcc)
		end,
		{AvgFitness,Std,MaxFitness,MinFitness};
	calculate_SpecieFitness(Specie_Id)->
		S = genotype:dirty_read({specie,Specie_Id}),
		calculate_SpecieFitness({specie,S}).
%The calculate_SpecieFitness/1 function calculates the general fitness statistic of the specie, the averate, max, min, and standard deviation of the specie's fitness. The function first composes a fitness list by accessing the fitness scores of each agent belonging to it, and then calculates the noted above statistics from that list, returning the tuple to the caller.

	calculate_fitness(Agent_Ids)->
		calculate_fitness(Agent_Ids,[]).
	calculate_fitness([Agent_Id|Agent_Ids],FitnessAcc)->
		A = genotype:dirty_read({agent,Agent_Id}),
		case A#agent.fitness of
			undefined ->
				calculate_fitness(Agent_Ids,FitnessAcc);
			Fitness ->
				calculate_fitness(Agent_Ids,[Fitness|FitnessAcc])
		end;
	calculate_fitness([],FitnessAcc)->
		FitnessAcc.
%The calculate_fitness/1 composes a fitness list composed of the fitness values belonging to the agents in the Agent_Ids list. If the agent does not yet have a fitness score, if for example it has just been created/mutated but not yet evaluated, it is skipped. The composed fitness list is returned to the caller.

gather_STATS(Population_Id,EvaluationsAcc,OpMode)->
	io:format("Gathering Species STATS in progress~n"),
	TimeStamp = now(),
	F = fun() ->
		P = genotype:read({population,Population_Id}),
		T = P#population.trace,
		SpecieSTATS = [update_SpecieSTAT(Specie_Id,TimeStamp,OpMode) || Specie_Id<-P#population.specie_ids],
		PopulationSTATS = T#trace.stats,
		U_PopulationSTATS = [SpecieSTATS|PopulationSTATS],
		U_TotEvaluations = T#trace.tot_evaluations+EvaluationsAcc,
		U_Trace = T#trace{
			stats = U_PopulationSTATS,
			tot_evaluations=U_TotEvaluations
		},
		io:format("Population Trace:~p~n",[U_Trace]),
		mnesia:write(P#population{trace=U_Trace})
	end,
	Result=mnesia:transaction(F),
	io:format("Result:~p~n",[Result]).
	
	update_SpecieSTAT(Specie_Id,TimeStamp,OpMode)->
		Specie_Evaluations = get({evaluations,Specie_Id}),
		put({evaluations,Specie_Id},0),
		S = genotype:read({specie,Specie_Id}),
		{Avg_Neurons,Neurons_Std} = calculate_SpecieAvgNodes({specie,S}),
		{AvgFitness,Fitness_Std,MaxFitness,MinFitness} = calculate_SpecieFitness({specie,S}),
		SpecieDiversity = calculate_SpecieDiversity({specie,S}),
		
		STAT = #stat{
			morphology = (S#specie.constraint)#constraint.morphology,
			specie_id = Specie_Id,
			avg_neurons=Avg_Neurons,
			std_neurons=Neurons_Std,
			avg_fitness=AvgFitness,
			std_fitness=Fitness_Std,
			max_fitness=MaxFitness,
			min_fitness=MinFitness,
			avg_diversity=SpecieDiversity,
			evaluations = Specie_Evaluations,
			time_stamp=TimeStamp,
			gentest_fitness = run_GenTest(S,OpMode)
		},
		STATS = S#specie.stats,
		U_STATS = [STAT|STATS],
		mnesia:dirty_write(S#specie{stats=U_STATS}),
		STAT.

	run_GenTest(S,benchmark)->
		TopAgent_Id = case S#specie.champion_ids of
			[Id] ->
				Id;
			[Id|_] ->
				Id;
			[]->
				void
		end,
		case TopAgent_Id of
			void ->
				0;
			_ ->
				case genotype:dirty_read({agent,TopAgent_Id}) of
					undefined ->
						qlog:xLog(qStatus, "BENCHMARK_SKIP | agent_not_found=~p", [TopAgent_Id]),
						0;
					_ ->
						% Log that we are entering a benchmark evaluation for the current top agent
						qlog:benchmarker(S#specie.id,io_lib:format("BENCHMARK_START | top_agent=~p", [TopAgent_Id])),
						Agent_PId=exoself:start(TopAgent_Id,self(),benchmark),
						receive
							{Agent_PId,benchmark_complete,Specie_Id,Fitness,Cycles,Time}->
								qlog:benchmarker(Specie_Id,io_lib:format("BENCHMARK_COMPLETE | fitness=~p | cycles=~p | time=~p", [Fitness, Cycles, Time])), genotype:print(TopAgent_Id), Fitness
							%Msg ->
							%	io:format("Msg:~p~n",[Msg])
						end
				end
		end;
	run_GenTest(_S,_)->
		0.

calculate_SpecieAvgNodes({specie,S})->
	Agent_Ids = S#specie.agent_ids,
	calculate_AvgNodes(Agent_Ids,[]);
calculate_SpecieAvgNodes(Specie_Id)->
	S = genotype:read({specie,Specie_Id}),
	calculate_SpecieAvgNodes({specie,S}).
	
	calculate_AvgNodes([Agent_Id|Agent_Ids],NAcc)->
		A = genotype:read({agent,Agent_Id}),
		Cx = genotype:read({cortex,A#agent.cx_id}),
		Tot_Neurons = length(Cx#cortex.neuron_ids),
		calculate_AvgNodes(Agent_Ids,[Tot_Neurons|NAcc]);
	calculate_AvgNodes([],NAcc)->
		{functions:avg(NAcc),functions:std(NAcc)}.

calculate_PopulationDiversity(Population_Id,[Specie_Id|Specie_Ids],Acc)->
	Diversity = calculate_SpecieDiversity(Specie_Id),
	case get({diversity,Specie_Id}) of
		undefined->
			put({diversity,Specie_Id},[Diversity]);
		PrevGenDiversity ->
			put({diversity,Specie_Id},[Diversity|PrevGenDiversity])
	end,
	calculate_PopulationDiversity(Population_Id,Specie_Ids,[{Specie_Id,Diversity}|Acc]);
calculate_PopulationDiversity(_Tot_Population_Id,[],Acc)->
	Acc.

	calculate_SpecieDiversity({specie,S})->
		Agent_Ids = S#specie.agent_ids,
		Diversity = calculate_diversity(Agent_Ids);
	calculate_SpecieDiversity(Specie_Id)->
		S = genotype:dirty_read({specie,Specie_Id}),
		calculate_SpecieDiversity({specie,S}).
		
		calculate_diversity(Agent_Ids)->
			calculate_diversity(Agent_Ids,[]).
		calculate_diversity([Agent_Id|Agent_Ids],DiversityAcc)->
			A = genotype:read({agent,Agent_Id}),
			Fingerprint = A#agent.fingerprint,
			U_DiversityAcc = (DiversityAcc -- [Fingerprint]) ++ [Fingerprint],
			calculate_diversity(Agent_Ids,U_DiversityAcc);
		calculate_diversity([],DiversityAcc)->
			length(DiversityAcc).

print_SpecieDiversity([Specie_Id|Specie_Ids])->
	S = genotype:dirty_read({specie,Specie_Id}),
	Morphology=(S#specie.constraint)#constraint.morphology,
	io:format("Specie id:~p~n Specie morphology:~p~n Diversity:~p~n",[Specie_Id,Morphology,get({diversity,Specie_Id})]),
	print_SpecieDiversity(Specie_Ids);
print_SpecieDiversity([])->
	done.

%% ===================================================================
%% Phase 4: Runtime Exit Semantics Helper Functions
%% ===================================================================

%% current_run_matches/3 - Check if RunRef matches current run for agent
current_run_matches(AgentId, RunRef, S) ->
	case maps:get(AgentId, S#state.agent_id_to_runref, undefined) of
		RunRef -> true;
		_ -> false
	end.

%% mark_agent_status/4 - Update agent runtime status
mark_agent_status(AgentId, Status, Reason, S) ->
	U_RuntimeStatus = maps:put(AgentId, Status, S#state.runtime_status),
	TsMs = erlang:system_time(millisecond),
	agent_runtime_registry:update_status(S#state.population_id, AgentId, Status, Reason, TsMs),
	S#state{runtime_status = U_RuntimeStatus}.

register_spawned_agents(SpawnedAgents, S) ->
	ActiveAgent_IdPs = [{AgentId, Pid} || {AgentId, Pid, _RunRef, _MonRef} <- SpawnedAgents],
	{AgentPidToId, AgentIdToPid, AgentIdToRef, AgentIdToRunRef, RuntimeStatus} =
		lists:foldl(fun({AgentId, Pid, RunRef, MonRef}, {P2I, I2P, I2R, I2RR, RS}) ->
			{
				P2I#{Pid => AgentId},
				I2P#{AgentId => Pid},
				I2R#{AgentId => MonRef},
				I2RR#{AgentId => RunRef},
				RS#{AgentId => running}
			}
		end, {#{}, #{}, #{}, #{}, #{}}, SpawnedAgents),
	S#state{
		activeAgent_IdPs = ActiveAgent_IdPs,
		agent_pid_to_id = AgentPidToId,
		agent_id_to_pid = AgentIdToPid,
		agent_id_to_ref = AgentIdToRef,
		agent_id_to_runref = AgentIdToRunRef,
		runtime_status = RuntimeStatus
	}.

spawn_managed_agent(AgentId, OpMode, S) ->
	RunRef = make_ref(),
	Agent_PId = exoself:start(AgentId, self(), agent_run_mode(OpMode), RunRef),
	MonRef = erlang:monitor(process, Agent_PId),
	U_S = register_running_agent(AgentId, Agent_PId, RunRef, MonRef, S),
	{Agent_PId, RunRef, MonRef, U_S}.

agent_run_mode(benchmark) ->
	gt;
agent_run_mode([gt | _]) ->
	gt;
agent_run_mode(gt) ->
	gt;
agent_run_mode(Mode) ->
	Mode.

register_running_agent(AgentId, Pid, RunRef, MonRef, S) ->
	CleanedPidToId = case maps:get(AgentId, S#state.agent_id_to_pid, undefined) of
		undefined -> S#state.agent_pid_to_id;
		OldPid -> maps:remove(OldPid, S#state.agent_pid_to_id)
	end,
	CleanedActiveAgent_IdPs = lists:keydelete(AgentId, 1, S#state.activeAgent_IdPs),
	S#state{
		activeAgent_IdPs = [{AgentId, Pid} | CleanedActiveAgent_IdPs],
		agent_pid_to_id = CleanedPidToId#{Pid => AgentId},
		agent_id_to_pid = maps:put(AgentId, Pid, S#state.agent_id_to_pid),
		agent_id_to_ref = maps:put(AgentId, MonRef, S#state.agent_id_to_ref),
		agent_id_to_runref = maps:put(AgentId, RunRef, S#state.agent_id_to_runref),
		runtime_status = maps:put(AgentId, running, S#state.runtime_status)
	}.

%% remove_running_agent/2 - Remove agent from active tracking maps
remove_running_agent(AgentId, S) ->
	U_AgentIdToPid = maps:remove(AgentId, S#state.agent_id_to_pid),
	U_AgentIdToRef = maps:remove(AgentId, S#state.agent_id_to_ref),
	U_AgentIdToRunRef = maps:remove(AgentId, S#state.agent_id_to_runref),
	% Remove PID mapping if exists
	U_AgentPidToId = case maps:get(AgentId, S#state.agent_id_to_pid, undefined) of
		undefined -> S#state.agent_pid_to_id;
		Pid -> maps:remove(Pid, S#state.agent_pid_to_id)
	end,
	S#state{
		activeAgent_IdPs = lists:keydelete(AgentId, 1, S#state.activeAgent_IdPs),
		agent_pid_to_id = U_AgentPidToId,
		agent_id_to_pid = U_AgentIdToPid,
		agent_id_to_ref = U_AgentIdToRef,
		agent_id_to_runref = U_AgentIdToRunRef
	}.

cleanup_completed_agent(AgentId, true, S) ->
	U_S = remove_running_agent(AgentId, S),
	U_RuntimeStatus = maps:remove(AgentId, U_S#state.runtime_status),
	agent_runtime_registry:delete_agent(S#state.population_id, AgentId),
	U_S#state{runtime_status = U_RuntimeStatus};
cleanup_completed_agent(AgentId, false, S) ->
	S#state{activeAgent_IdPs = lists:keydelete(AgentId, 1, S#state.activeAgent_IdPs)}.

finalize_eviction(AgentId, Reason, S) ->
	RunRef = maps:get(AgentId, S#state.agent_id_to_runref, undefined),
	AgentBytes = eviction_agent_bytes(AgentId, Reason, S),
	qlog:xLog(qStatus, "EVICTION_FINALIZED | population_id=~p | agent_id=~p | run_ref=~p | agent_bytes=~p | reason=~p", [S#state.population_id, AgentId, RunRef, AgentBytes, Reason]),
	qlog:benchmarker(S#state.population_id, io_lib:format("EVICTION_COMPLETED | agent_id=~p | run_ref=~p | reason=~p", [AgentId, RunRef, Reason])),
	U_S1 = mark_agent_status(AgentId, evicted, Reason, S),
	U_S2 = remove_running_agent(AgentId, U_S1),
	U_S3 = enqueue_restart(AgentId, U_S2),
	U_S3#state{inflight_evictions = maps:remove(AgentId, U_S3#state.inflight_evictions)}.

evict_running_agent(AgentId, Reason, S) ->
	RunRef = maps:get(AgentId, S#state.agent_id_to_runref),
	AgentBytes = eviction_agent_bytes(AgentId, Reason, S),
	qlog:xLog(qStatus, "EVICTION_IDENTIFIED | population_id=~p | agent_id=~p | run_ref=~p | agent_bytes=~p | reason=~p", [S#state.population_id, AgentId, RunRef, AgentBytes, Reason]),
	qlog:benchmarker(S#state.population_id, io_lib:format("EVICTION_REQUESTED | agent_id=~p | reason=~p", [AgentId, Reason])),
	U_S1 = mark_agent_status(AgentId, evicting, Reason, S),
	AgentPid = maps:get(AgentId, S#state.agent_id_to_pid),
	AgentPid ! {admin, graceful_stop, self(), Reason},
	TsMs = erlang:system_time(millisecond),
	EvictionData = #{
		run_ref => RunRef,
		requested_ts_ms => TsMs,
		reason => Reason
	},
	GraceMs = config:auditor_grace_ms(),
	erlang:send_after(GraceMs, self(), {eviction_grace_expired, AgentId, RunRef}),
	U_S1#state{inflight_evictions = maps:put(AgentId, EvictionData, U_S1#state.inflight_evictions)}.

%% enqueue_restart/2 - Add agent to restart queue
enqueue_restart(AgentId, S) ->
	U_RestartQueue = queue:in(AgentId, S#state.restart_queue),
	io:format("Agent ~p enqueued for restart (queue length: ~p)~n", [AgentId, queue:len(U_RestartQueue)]),
	qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_ENQUEUED | agent_id=~p | queue_length=~p", [AgentId, queue:len(U_RestartQueue)])),
	S#state{restart_queue = U_RestartQueue}.

%% restart_agent/2 - Restart a single agent
%% Phase 6: Enhanced with inflight eviction check
restart_agent(AgentId, S) ->
	% Phase 6: Do not restart if eviction is inflight for this agent
	InFlight = maps:is_key(AgentId, S#state.inflight_evictions),
	CurrentStatus = maps:get(AgentId, S#state.runtime_status, undefined),
	
	case {InFlight, CurrentStatus} of
		{true, _} ->
			% Eviction inflight, skip restart and re-enqueue
			io:format("Skipping restart for Agent ~p: eviction inflight~n", [AgentId]),
			qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_SKIPPED | agent_id=~p | reason=eviction_inflight", [AgentId])),
			U_RestartQueue = queue:in(AgentId, S#state.restart_queue),
			S#state{restart_queue = U_RestartQueue};
		{false, evicting} ->
			% Still in evicting state but no inflight record, skip and re-enqueue
			io:format("Skipping restart for Agent ~p: still evicting~n", [AgentId]),
			qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_SKIPPED | agent_id=~p | reason=still_evicting", [AgentId])),
			U_RestartQueue = queue:in(AgentId, S#state.restart_queue),
			S#state{restart_queue = U_RestartQueue};
		{false, running} ->
			% Already running (shouldn't happen), skip
			io:format("Skipping restart for Agent ~p: already running~n", [AgentId]),
			qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_SKIPPED | agent_id=~p | reason=already_running", [AgentId])),
			S;
		_ ->
			% Valid restart
			io:format("Restarting Agent ~p~n", [AgentId]),
			% Phase 8: Log restart started
			qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_STARTED | agent_id=~p | previous_status=~p", [AgentId, CurrentStatus])),
			U_S0 = mark_agent_status(AgentId, restarting, CurrentStatus, S),
			% Phase 6: Increment eviction count in registry
			TsMs = erlang:system_time(millisecond),
			agent_runtime_registry:increment_eviction_count(S#state.population_id, AgentId, TsMs),
			{Agent_PId, RunRef, _MonRef, U_S} = spawn_managed_agent(AgentId, S#state.op_mode, U_S0),
			% Phase 8: Log restart will complete when manifest is published
			qlog:benchmarker(S#state.population_id, io_lib:format("RESTART_SPAWNED | agent_id=~p | new_run_ref=~p | new_pid=~p", [AgentId, RunRef, Agent_PId])),
			U_S
	end.

%% restart_batch/2 - Restart up to BatchSize agents from queue
%% Phase 6: Enhanced with better tracking and logging
restart_batch(BatchSize, S) ->
	QueueLength = queue:len(S#state.restart_queue),
	ActualBatchSize = min(BatchSize, QueueLength),
	case ActualBatchSize of
		0 ->
			S;
		_ ->
			io:format("Starting restart batch: ~p agents (queue: ~p)~n", [ActualBatchSize, QueueLength]),
			qlog:benchmarker(S#state.population_id, 
				io_lib:format("RESTART_BATCH_START | batch_size=~p | queue_length=~p", [ActualBatchSize, QueueLength])),
			U_S = restart_batch_loop(ActualBatchSize, S),
			FinalQueueLength = queue:len(U_S#state.restart_queue),
			io:format("Restart batch complete: ~p agents restarted, ~p remaining in queue~n", 
				[ActualBatchSize, FinalQueueLength]),
			qlog:benchmarker(S#state.population_id, 
				io_lib:format("RESTART_BATCH_COMPLETE | restarted=~p | remaining_queue=~p", 
					[ActualBatchSize, FinalQueueLength])),
			U_S
	end.

restart_batch_loop(0, S) ->
	S;
restart_batch_loop(BatchSize, S) when BatchSize > 0 ->
	case queue:out(S#state.restart_queue) of
		{{value, AgentId}, U_RestartQueue} ->
			U_S1 = S#state{restart_queue = U_RestartQueue},
			U_S2 = restart_agent(AgentId, U_S1),
			restart_batch_loop(BatchSize - 1, U_S2);
		{empty, _} ->
			S
	end.

%% force_kill_owned_pids/2 - Force kill all PIDs owned by agent
force_kill_owned_pids(AgentId, S) ->
	case agent_runtime_registry:get_agent(S#state.population_id, AgentId) of
		{ok, AgentData} ->
			OwnedPids = maps:get(owned_pids, AgentData, []),
			AgentBytes = current_agent_memory_bytes(AgentId, S),
			io:format("Force killing ~p PIDs for Agent ~p~n", [length(OwnedPids), AgentId]),
			[catch exit(Pid, kill) || Pid <- OwnedPids],
			qlog:xLog(qStatus, "FORCE_KILL_DISPATCHED | population_id=~p | agent_id=~p | agent_bytes=~p | pid_count=~p", [S#state.population_id, AgentId, AgentBytes, length(OwnedPids)]),
			S;
		{error, not_found} ->
			io:format("Cannot force kill Agent ~p: not found in registry~n", [AgentId]),
			qlog:xLog(qStatus, "FORCE_KILL_SKIPPED | population_id=~p | agent_id=~p | reason=registry_not_found", [S#state.population_id, AgentId]),
			S
	end.

eviction_agent_bytes(_AgentId, {memory_pressure, _TotalMemory, AgentBytes}, _S) when is_integer(AgentBytes) ->
	AgentBytes;
eviction_agent_bytes(AgentId, _Reason, S) ->
	current_agent_memory_bytes(AgentId, S).

current_agent_memory_bytes(AgentId, S) ->
	case agent_runtime_registry:sum_agent_memory(S#state.population_id, AgentId) of
		{ok, Bytes} when Bytes > 0 ->
			Bytes;
		_ ->
			case agent_runtime_registry:get_agent(S#state.population_id, AgentId) of
				{ok, AgentData} ->
					maps:get(last_memory_bytes, AgentData, undefined);
				{error, not_found} ->
					undefined
			end
	end.
