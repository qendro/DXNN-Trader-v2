
-module(exoself).
-compile(export_all).
-include("records.hrl").
-record(state,{
	agent_id,
	generation,
	pm_pid,
	idsNpids,
	cx_pid,
	specie_id,
	spids=[],
	npids=[],
	nids=[],
	apids=[],
	scape_pids=[],
	highest_fitness=-2,
	eval_acc=0,
	cycle_acc=0,
	time_acc=0,
	max_attempts=10,
	attempt=1,
	tuning_duration_f,
	tuning_selection_f,
	annealing_parameter,
	perturbation_range,
	substrate_pid,
	cpp_pids=[],
	cep_pids=[],
	opmode,
	episode_mode=false,%true when config:evaluation_mode() is episodes (and the agent is not live trading)
	eval_phase=legacy,%legacy | tune | score | validation: what the evaluation in progress is for
	score_fitness,%fitness measured on the score episodes; this is what selection sees in episode mode
	score_stats=[],
	score_cycles=0,%bars and microseconds of the score evaluation, for the generation summary's ms_per_bar
	score_time=0,
	last_bar=0,%last bar the cortex reported in the evaluation in progress (see cortex:report_progress/2)
	last_progress_ms=0,%when the evaluation in progress started or last reported a bar (monotonic milliseconds)
	last_check_ms=0%when the mailboxes of the network were last checked for a flood
}).
-define(WATCH_INTERVAL_MS,100).%how often an exoself looks at its network while an evaluation runs (see the watchdog section)

start(Agent_Id)->
	case whereis(monitor) of
		undefined ->
			io:format("start(Agent_Id):: 'monitor' is not registered~n");
		PId ->
			start(Agent_Id,PId,gt)
	end.

start(Agent_Id,PM_PId)->
	start(Agent_Id,PM_PId,gt).

start(Agent_Id,PM_PId,OpMode)->
	spawn(exoself,prep,[Agent_Id,PM_PId,OpMode]).
%The start/3 function spawns a new Agent_Id exoself process, belonging to the population_monitor process with the pid PM_PId, and using the OpMode with which it was spawned.

prep(Agent_Id,PM_PId,OpMode)->
	IdsNPIds = ets:new(idsNpids,[set,private]),
	A = genotype:dirty_read({agent,Agent_Id}),
	case A of
		undefined ->
			ErrorMsg = io_lib:format("ERROR: Agent ~p not found in database. This may indicate a population reuse/transaction issue.", [Agent_Id]),
			qlog:benchmarker(Agent_Id, ErrorMsg),
			error({agent_not_found, Agent_Id, PM_PId, OpMode});
		_ ->
			ok
	end,
	HeredityType = A#agent.heredity_type,
	%With config:evolution_seed() set this process's random choices (the repair of a network whose sensors are all
	%unconnected in link_Sensors/4, and weight tuning when it is on) depend on the seed and on what the agent is,
	%not on the clock. The fingerprint describes the network without its ids, which differ from run to run.
	case config:evolution_seed() of
		undefined -> ok;
		EvolutionSeed -> rand:seed(exsss,{erlang:phash2(EvolutionSeed),erlang:phash2(A#agent.fingerprint),A#agent.generation})
	end,
	Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
	SIds = Cx#cortex.sensor_ids,
	AIds = Cx#cortex.actuator_ids,
	NIds = Cx#cortex.neuron_ids,
	ScapePIds = spawn_Scapes(IdsNPIds,SIds,AIds,Agent_Id,OpMode),
	%Episode mode: tell the scapes which episode set the first evaluation uses, before any sensor can reach them.
	%Training starts on the tune set when config:episode_tuning() is on, and goes straight to the score set when it is
	%off; a benchmark or test run uses the fixed validation set.
	EpisodeMode = (config:evaluation_mode() == episodes) and (OpMode =/= live_trading),
	EvalPhase = case {EpisodeMode,OpMode} of
		{false,_} -> legacy;
		{true,gt} ->
			case config:episode_tuning() of
				true -> tune;
				_ -> score
			end;
		{true,_} -> validation
	end,
	case EpisodeMode of
		true -> [ScapePId ! {self(),eval_mode,EvalPhase} || ScapePId <- ScapePIds];
		false -> ok
	end,
	%config:sense_before_trade/0: the simulator holds each bar's trade until every sensor has read the bar, so it is told
	%how many sensors read it. spawn_Scapes/5 starts one scape per sensor that names it and links every sensor to the
	%last one (the others stay idle), so the count goes to the scape each sensor is actually linked to.
	case {EpisodeMode,config:sense_before_trade()} of
		{true,true} ->
			SensorScapes = [ets:lookup_element(IdsNPIds,maybe_map_scape_name(ScapeName,OpMode),2)
				|| SId <- SIds, {private,ScapeName} <- [(genotype:dirty_read({sensor,SId}))#sensor.scape]],
			[ScapePId ! {self(),sensors,length([P || P <- SensorScapes, P == ScapePId])} || ScapePId <- lists:usort(SensorScapes)];
		_ ->
			ok
	end,
	spawn_CerebralUnits(IdsNPIds,cortex,[Cx#cortex.id]),
	spawn_CerebralUnits(IdsNPIds,sensor,SIds),
	spawn_CerebralUnits(IdsNPIds,actuator,AIds),
	spawn_CerebralUnits(IdsNPIds,neuron,NIds),
	case A#agent.encoding_type of
		substrate ->
			Substrate_Id=A#agent.substrate_id,
			Substrate = genotype:dirty_read({substrate,Substrate_Id}),
			CPP_Ids = Substrate#substrate.cpp_ids,
			CEP_Ids = Substrate#substrate.cep_ids,
			spawn_CerebralUnits(IdsNPIds,substrate_cpp,CPP_Ids),
			spawn_CerebralUnits(IdsNPIds,substrate_cep,CEP_Ids),
			spawn_CerebralUnits(IdsNPIds,substrate,[Substrate_Id]),
			
			Substrate_PId=ets:lookup_element(IdsNPIds,Substrate_Id,2),
			link_SubstrateCPPs(CPP_Ids,IdsNPIds,Substrate_PId),
			link_SubstrateCEPs(CEP_Ids,IdsNPIds,Substrate_PId),
			SDensities = Substrate#substrate.densities,
			SPlasticity = Substrate#substrate.plasticity,
			SLinkform = Substrate#substrate.linkform,
			Sensors=[genotype:dirty_read({sensor,SId})||SId <- SIds],
			Actuators=[genotype:dirty_read({actuator,AId})||AId <- AIds],		
			CPP_PIds=[ets:lookup_element(IdsNPIds,Id,2)||Id<-CPP_Ids],
			CEP_PIds=[ets:lookup_element(IdsNPIds,Id,2)||Id<-CEP_Ids],
			Substrate_PId ! {self(),init,{Sensors,Actuators,[ets:lookup_element(IdsNPIds,Id,2)||Id<-SIds],[ets:lookup_element(IdsNPIds,Id,2)||Id<-AIds],CPP_PIds,CEP_PIds,SDensities,SPlasticity,SLinkform}};
		_ ->
			CPP_PIds=[],
			CEP_PIds=[],
			Substrate_PId = undefined
	end,
	link_Sensors(Agent_Id,SIds,IdsNPIds,OpMode),
	link_Actuators(AIds,IdsNPIds,OpMode),
	link_Neurons(NIds,IdsNPIds,HeredityType),
		{SPIds,NPIds,APIds}=link_Cortex(Cx,IdsNPIds),
	Cx_PId = ets:lookup_element(IdsNPIds,Cx#cortex.id,2),
	{TuningDurationFunction,Parameter} = A#agent.tuning_duration_f,
		S = #state{
			agent_id=Agent_Id,
			generation=A#agent.generation,
			pm_pid=PM_PId,
			idsNpids=IdsNPIds,
			cx_pid=Cx_PId,
			specie_id=A#agent.specie_id,
			spids=SPIds,
			npids=NPIds,
			nids=NIds,
			apids=APIds,
			substrate_pid=Substrate_PId,
			cpp_pids = CPP_PIds,
			cep_pids = CEP_PIds,
			scape_pids=ScapePIds,
			max_attempts= tuning_duration:TuningDurationFunction(Parameter,NIds,A#agent.generation),
			tuning_selection_f=A#agent.tuning_selection_f,
			annealing_parameter=A#agent.annealing_parameter,
			tuning_duration_f=A#agent.tuning_duration_f,
			perturbation_range=A#agent.perturbation_range,
			opmode=OpMode,
			episode_mode=EpisodeMode,
			eval_phase=EvalPhase,
			%Episode fitness can be far below zero (see fitness_functions:aggregate_episodes/2), so the first
			%evaluation must always count as an improvement.
			highest_fitness=case EpisodeMode of true -> -1.0e300; false -> -2 end,
			%Monotonic time starts at an arbitrary, usually negative, value, so both watchdog clocks must start from now, not 0.
			last_progress_ms=erlang:monotonic_time(millisecond),
			last_check_ms=erlang:monotonic_time(millisecond)
		},
	%qlog:register_agent(self(), Agent_Id),
	%qlog:xLog(pid_to_list(self()), "ExoSelf: ~p generated phenotype for Agent_Id: ~p, Gen: ~p, Specie_Id: ~p. Spawning main loop. Genotype: ~p", [self(), Agent_Id, A#agent.generation, A#agent.specie_id, A]),
	% Log PID map for this agent spawn
	All_Pids = [self(), S#state.cx_pid | S#state.spids] ++ S#state.npids ++ 
	           S#state.apids ++ S#state.scape_pids ++ S#state.cpp_pids ++ 
	           S#state.cep_pids ++ 
	           case S#state.substrate_pid of
	               undefined -> [];
	               SubPid -> [SubPid]
	           end,
	Run_Id = qlog:get_run_id_from_population_id(A#agent.population_id),
	qlog:pid_map_log(Run_Id, A#agent.generation, Agent_Id, All_Pids),
	loop(S,OpMode).
%The prep/2 function prepares and sets up the exoself's state before dropping into the main loop. The function first reads the agent and cortex records belonging to the Agent_Id NN based system. The function then reads the sensor, actuator, and neuron ids, then spawns the private scapes using the spawn_Scapes/3 function, spawns the cortex, sensor, actuator, and neuron processes, and then finally links up all these processes together using the link_.../2 processes. Once the phenotype has been generated from the genotype, the exoself drops into its main loop.

loop(S,gt)->
	receive
		{_Cx_PId,progress,Bar} ->
			%The cortex is still completing bars.
			watch_progress(S,gt,Bar);
		{ScapePId,episode_boundary} ->
			%config:episode_isolation/0: the simulator finished an episode and holds the next bar (fx:boundary_wait/6).
			episode_boundary(S,gt,ScapePId);
		{_Cx_PId,evaluation_completed,Fitness,Cycles,Time,_GoalReachedFlag} when S#state.eval_phase == score ->
			%Episode mode: the weights were just run once on the score episodes. This fitness is what the agent reports
			%to selection (with tuning on it replaces the best-of-N tuning fitness).
			qlog:benchmarker(S#state.agent_id,io_lib:format("SCORE_EVAL | tune_fitness=~p | score_fitness=~p | cycles=~p | time=~p",[tune_fitness(S),Fitness,Cycles,Time])),
			gen_server:cast(S#state.pm_pid,{self(),evaluations,S#state.specie_id,1,Cycles,Time}),
			U_S = pass_started(S#state{score_fitness=Fitness,score_stats=take_eval_stats(score),score_cycles=Cycles,score_time=Time}),
			%The score and the weights are stored now, while the network is known to be responsive, so that nothing
			%that happens in the optional validation pass can change or lose them.
			save_scored_agent(U_S),
			case validation_due() of
				true ->
					case reset_network(U_S) of
						ok ->
							start_pass(U_S,validation),
							exoself:loop(pass_started(U_S#state{eval_phase=validation}),gt);
						{failed,Reason} ->
							%The score is stored; only the validation pass is lost, as for a stall inside it.
							give_up(U_S#state{eval_phase=validation},gt,Reason)
					end;
				false ->
					finish_episode_agent(U_S,undefined,[])
			end;
		{_Cx_PId,evaluation_completed,Fitness,_Cycles,_Time,_GoalReachedFlag} when S#state.eval_phase == validation ->
			%Episode mode, optional third stage: the same weights on the validation episodes. Logged only; it never
			%reaches selection.
			finish_episode_agent(S,Fitness,take_eval_stats(validation));
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			qlog:benchmarker(S#state.agent_id,io_lib:format("TRAIN_EVAL | attempt=~p | fitness=~p | cycles=~p | time=~p",[S#state.attempt,Fitness,Cycles,Time])),
			IdsNPIds = S#state.idsNpids,
			{U_HighestFitness,U_Attempt}=case Fitness > S#state.highest_fitness of
				true ->
					[NPId ! {self(),weight_backup} || NPId <- S#state.npids],
					{Fitness,0};
				false ->
					Perturbed_NIdPs=get(perturbed),
					[ets:lookup_element(IdsNPIds,NId,2) ! {self(),weight_restore} || {NId,_Spread} <- Perturbed_NIdPs],
					{S#state.highest_fitness,S#state.attempt+1}
			end,
			case reset_network(S) of
				ok ->
					tuning_evaluated(S,Cx_PId,Cycles,Time,GoalReachedFlag,U_HighestFitness,U_Attempt);
				{failed,Reason} ->
					give_up(S,gt,Reason)
			end
		after ?WATCH_INTERVAL_MS ->
			watch_silence(S,gt)
	end;
loop(S,benchmark)->
	receive
		{_Cx_PId,progress,Bar} ->
			watch_progress(S,benchmark,Bar);
		{ScapePId,episode_boundary} ->
			episode_boundary(S,benchmark,ScapePId);
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			A=genotype:dirty_read({agent,S#state.agent_id}),
			shutdown_phenotype(S),
			io:format("Benchmark complete, agent:~p terminating. Fitness:~p~n TotCycles:~p~n TimeAcc:~p Goal:~p~n",[self(),Fitness,Cycles,Time,GoalReachedFlag]),
			S#state.pm_pid ! {self(),benchmark_complete,S#state.specie_id,Fitness,Cycles,Time}
		after ?WATCH_INTERVAL_MS ->
			watch_silence(S,benchmark)
	end;
loop(S,test)->
	receive
		{_Cx_PId,progress,Bar} ->
			watch_progress(S,test,Bar);
		{ScapePId,episode_boundary} ->
			episode_boundary(S,test,ScapePId);
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			A=genotype:dirty_read({agent,S#state.agent_id}),
			shutdown_phenotype(S),
			io:format("Test complete, agent:~p terminating. Fitness:~p~n TotCycles:~p~n TimeAcc:~p Goal:~p~n",[self(),Fitness,Cycles,Time,GoalReachedFlag]),
			S#state.pm_pid ! {self(),benchmark_complete,S#state.specie_id,Fitness,Cycles,Time}
		after ?WATCH_INTERVAL_MS ->
			watch_silence(S,test)
	end;
loop(S,live_trading)->
	receive
		{_Cx_PId,progress,_Bar} ->
			%Live bars arrive in real time, so there is no stall clock here; the report is only taken off the mailbox.
			exoself:loop(S,live_trading);
		{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}->
			A=genotype:dirty_read({agent,S#state.agent_id}),
			terminate_phenotype(S#state.cx_pid,S#state.spids,S#state.npids,S#state.apids,S#state.scape_pids,S#state.cpp_pids,S#state.cep_pids,S#state.substrate_pid),
			io:format("Live trading complete, agent:~p terminating. Fitness:~p~n TotCycles:~p~n TimeAcc:~p Goal:~p~n",[self(),Fitness,Cycles,Time,GoalReachedFlag]),
			S#state.pm_pid ! {self(),live_trading_complete,S#state.specie_id,Fitness,Cycles,Time}
		end.

%tuning_evaluated/7 is the rest of a tuning evaluation's handling, once the network has been reset: stop tuning (and
%score, in episode mode) or perturb the weights again and restart the cortex.
tuning_evaluated(S,Cx_PId,Cycles,Time,GoalReachedFlag,U_HighestFitness,U_Attempt)->
			IdsNPIds = S#state.idsNpids,
			U_CycleAcc = S#state.cycle_acc+Cycles,
			U_TimeAcc = S#state.time_acc+Time,
			U_EvalAcc = S#state.eval_acc+1,
			%QQ Collect and log total reductions for this evaluation
			%Total_Reductions = collect_total_reductions(S),
			%qlog:agent_reductions_log(S#state.agent_id, U_EvalAcc, Total_Reductions),
			gen_server:cast(S#state.pm_pid,{self(),evaluations,S#state.specie_id,1,Cycles,Time}),
			case (U_Attempt >= S#state.max_attempts) or (GoalReachedFlag == true) of
				true when S#state.episode_mode ->	%End tuning, start scoring
					%The network was reset above. Put every neuron back on the best weights found (the last attempt may
					%have failed, and plasticity may have moved the working copy), then run them once on the score episodes.
					start_pass(S,score),
					exoself:loop(pass_started(S#state{
						cycle_acc=U_CycleAcc,
						time_acc=U_TimeAcc,
						eval_acc=U_EvalAcc,
						attempt=U_Attempt,
						highest_fitness=U_HighestFitness,
						eval_phase=score
					}),gt);
				true ->	%End training
					%case U_Attempt >= S#state.max_attempts of
					%	true ->
							%qlog:l1msg(self(), io_lib:format("Max attempts reached | Attempts: ~p | BestFitness: ~p", [S#state.max_attempts, U_HighestFitness]));
					%	false -> ok
					%end,
					A=genotype:dirty_read({agent,S#state.agent_id}),
					qlog:log_agent_metadata(S#state.agent_id, S#state.generation, self()),
					genotype:write(A#agent{fitness=U_HighestFitness}),
					backup_genotype(S#state.idsNpids,S#state.npids),
					shutdown_phenotype(S),
					case GoalReachedFlag of
						true ->
							gen_server:cast(S#state.pm_pid,{S#state.agent_id,goal_reached});
						_ ->
							ok
					end,
					gen_server:cast(S#state.pm_pid,{S#state.agent_id,terminated,U_HighestFitness});
				false -> %Continue training
					TuningSelectionFunction=S#state.tuning_selection_f,
					PerturbationRange = S#state.perturbation_range,
					AnnealingParameter = S#state.annealing_parameter,
					ChosenNIdPs=tuning_selection:TuningSelectionFunction(S#state.nids,S#state.generation,PerturbationRange,AnnealingParameter),
					[ets:lookup_element(IdsNPIds,NId,2) ! {self(),weight_perturb,Spread} || {NId,Spread} <- ChosenNIdPs],
					put(perturbed,ChosenNIdPs),
					Cx_PId ! {self(),reactivate},
					U_S =pass_started(S#state{
						cycle_acc=U_CycleAcc,
						time_acc=U_TimeAcc,
						eval_acc=U_EvalAcc,
						attempt=U_Attempt,
						highest_fitness=U_HighestFitness
					}),
					exoself:loop(U_S,gt)
			end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Watchdog: stalled and runaway networks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Mutation can produce a network that does not behave, and the post-mutation checks do not catch every case, so the
%exoself is the backstop. While an evaluation runs it looks at its network every ?WATCH_INTERVAL_MS for two things:
%
%	stalled   No bar has been completed for config:stall_timeout_ms(): neurons connected in a circle that wait for each
%	          other, or a process that crashed. The cortex reports every config:progress_every_bars() completed bars
%	          (cortex:report_progress/2), so this is silence, not slowness: a large network on many episodes keeps
%	          reporting and is never failed for taking long.
%	runaway   A process of the network has more than config:runaway_queue_limit() messages waiting. A neuron with no
%	          input (or a group of neurons that only feed each other) fires as fast as the scheduler lets it and never
%	          waits for the sensors. If it reaches an actuator the network even keeps "completing bars", so it is not a
%	          stall, while the mailboxes it floods grow by about 100 MB per second. In a healthy network a process never
%	          has more messages waiting than it has inputs.
%
%	overrun   The cortex has reported more bars than the evaluation has (bar_limit/1). The network is alive and nothing
%	          piles up, but the simulator is not advancing, so the evaluation would never end. Logged as a runaway.
%
%Either way the agent is ended by give_up/3. And every agent's processes are killed when its turn ends, stalled or not
%(shutdown_phenotype/1): a neuron that fires forever never reads its mailbox, so the terminate message alone would leave it
%running and using a core for the rest of the run.
%Not applied in live trading, where bars arrive in real time and the scape is shared.

%pass_started/1 marks the start of an evaluation: the stall clock runs from here.
pass_started(S)->
	S#state{last_bar=0,last_progress_ms=erlang:monotonic_time(millisecond)}.

%watch_progress/3: the cortex reported bar Bar. Coming at least every progress_every_bars bars, this is also where a
%runaway is noticed when bars keep completing, so the mailboxes are checked here too, at most every ?WATCH_INTERVAL_MS.
watch_progress(S,OpMode,Bar)->
	Now = erlang:monotonic_time(millisecond),
	U_S = S#state{last_bar=Bar,last_progress_ms=Now},
	case Bar > bar_limit(U_S) of
		true ->
			give_up(U_S,OpMode,{overrun,Bar});
		false ->
			case Now - S#state.last_check_ms >= ?WATCH_INTERVAL_MS of
				true -> check_network(U_S#state{last_check_ms=Now},OpMode,Now);
				false -> exoself:loop(U_S,OpMode)
			end
	end.

%bar_limit/1: twice the bars of the episode set being evaluated, plus 1,000. An evaluation cannot legitimately report
%more: the simulator ends it after the last bar of the last episode. One that does is cycling without the simulator
%advancing, which neither the stall clock (bars keep "completing") nor the mailbox check (nothing piles up) can see.
%In legacy mode the number of bars is not known here, so there is no limit (a number is never greater than an atom).
bar_limit(S)->
	case S#state.episode_mode of
		true ->
			case lists:sum([Bars || {_StartKey,Bars} <- episodes:current_set(S#state.eval_phase)]) of
				0 -> infinity;
				Expected -> 2*Expected + 1000
			end;
		false ->
			infinity
	end.

%watch_silence/2: nothing arrived for ?WATCH_INTERVAL_MS.
watch_silence(S,OpMode)->
	Now = erlang:monotonic_time(millisecond),
	check_network(S#state{last_check_ms=Now},OpMode,Now).

check_network(S,OpMode,Now)->
	Limit = runaway_queue_limit(),
	case longest_queue(S) of
		{Length,PId} when Length > Limit ->
			give_up(S,OpMode,{runaway,Length,PId});
		_ ->
			case Now - S#state.last_progress_ms >= stall_timeout() of
				true -> give_up(S,OpMode,stalled);
				false -> exoself:loop(S,OpMode)
			end
	end.

stall_timeout()->
	case config:stall_timeout_ms() of
		Ms when is_integer(Ms), Ms > 0 -> Ms;
		_ -> 120000
	end.

runaway_queue_limit()->
	case config:runaway_queue_limit() of
		N when is_integer(N), N > 0 -> N;
		_ -> 5000
	end.

%longest_queue/1 returns {Length,PId} of the fullest mailbox among this agent's processes (private scapes included).
longest_queue(S)->
	lists:max([{0,undefined}|[{Length,PId} || PId <- phenotype_pids(S), {message_queue_len,Length} <- [erlang:process_info(PId,message_queue_len)]]]).

phenotype_pids(S)->
	[PId || PId <- [S#state.cx_pid,S#state.substrate_pid|S#state.spids] ++ S#state.npids ++ S#state.apids ++ S#state.scape_pids ++ S#state.cpp_pids ++ S#state.cep_pids, is_pid(PId)].

%give_up/3 ends an agent whose network stalled or ran away (Reason: stalled | {runaway,QueueLength,PId} | {overrun,Bars}).
%	gt, validation phase       the score was already stored by save_scored_agent/1 and stays; validation is logged as missing
%	gt, any other phase        the agent gets the failure fitness (a ruined account in episode mode, 0.0 in legacy mode)
%	benchmark, test            the caller is told with {ExoselfPId,benchmark_stalled,Specie_Id}
give_up(S,OpMode,Reason)->
	Agent_Id = S#state.agent_id,
	Phase = S#state.eval_phase,
	{Kind,What} = case Reason of
		stalled ->
			{stalled,io_lib:format("stalled, no bar completed for ~p ms (last bar reported: ~p)",[stall_timeout(),S#state.last_bar])};
		{stalled_in_reset,Step,Missing} ->
			{stalled,io_lib:format("stalled in a network reset: ~p of the processes asked ~p did not answer within ~p ms (last bar reported: ~p)",[Missing,Step,stall_timeout(),S#state.last_bar])};
		{overrun,Bars} ->
			%Counted with the runaways in the generation summary: the network is running, and will not stop by itself.
			{runaway,io_lib:format("overrun, ~p bars reported by an evaluation of ~p bars (the simulator never ended it)",[Bars,(bar_limit(S)-1000) div 2])};
		{runaway,Length,PId} ->
			Role = case lists:member(PId,S#state.npids) of
				true -> neuron;
				false ->
					case lists:member(PId,S#state.apids) of
						true -> actuator;
						false -> other
					end
			end,
			{runaway,io_lib:format("runaway, ~p messages waiting in one ~p process (limit ~p, last bar reported: ~p)",[Length,Role,runaway_queue_limit(),S#state.last_bar])}
	end,
	case {OpMode,Phase} of
		{gt,validation} ->
			qlog:xLog(qStatus, "Agent ~p timeout: ~s in validation phase, score ~p kept", [Agent_Id,What,S#state.score_fitness]),
			qlog:benchmarker(Agent_Id,io_lib:format("STALL | kind=~p | phase=validation | ~s | score kept",[Kind,What])),
			qlog:genotype_snapshot(Agent_Id, lists:flatten(io_lib:format("Network ~p in the validation pass, score kept",[Kind]))),
			record_stall(Agent_Id,Kind,validation),
			finish_episode_agent(S,undefined,[]);
		{gt,_} ->
			%In episode mode 0 would be a good fitness, so a failed agent gets the same value as a ruined account.
			FailFitness = case S#state.episode_mode of
				true -> fitness_functions:episode_ruin();
				false -> 0.0
			end,
			qlog:xLog(qStatus, "Agent ~p timeout: ~s in ~p phase, assigning ~p fitness", [Agent_Id,What,Phase,FailFitness]),
			qlog:benchmarker(Agent_Id,io_lib:format("STALL | kind=~p | phase=~p | ~s | fitness=~p",[Kind,Phase,What,FailFitness])),
			qlog:genotype_snapshot(Agent_Id, lists:flatten(io_lib:format("Network ~p, assigning failure fitness",[Kind]))),
			record_stall(Agent_Id,Kind,Phase),
			A=genotype:dirty_read({agent,Agent_Id}),
			genotype:write(A#agent{fitness=FailFitness}),
			shutdown_phenotype(S),
			gen_server:cast(S#state.pm_pid,{Agent_Id,terminated,FailFitness});
		_ ->
			qlog:xLog(qStatus, "Agent ~p timeout: ~s in a ~p run", [Agent_Id,What,OpMode]),
			qlog:benchmarker(Agent_Id,io_lib:format("STALL | kind=~p | phase=~p | ~s",[Kind,OpMode,What])),
			shutdown_phenotype(S),
			S#state.pm_pid ! {self(),benchmark_stalled,S#state.specie_id}
	end.

%shutdown_phenotype/1 ends this agent's network for certain: the usual terminate messages, then a kill for anything that
%did not read them. By the time it is called nothing more is needed from these processes (weights and statistics have
%been collected), and none of them does any work on terminate.
shutdown_phenotype(S)->
	terminate_phenotype(S#state.cx_pid,S#state.spids,S#state.npids,S#state.apids,S#state.scape_pids,S#state.cpp_pids,S#state.cep_pids,S#state.substrate_pid),
	case S#state.opmode of
		live_trading -> ok;
		_ -> [exit(PId,kill) || PId <- phenotype_pids(S)], ok
	end.

%record_stall/3 leaves a note in the generation's ledger so the generation summary can count them (stalls= and runaways=).
record_stall(Agent_Id,Kind,Phase)->
	case ets:whereis(eval_ledger) of
		undefined -> ok;
		_ -> ets:insert(eval_ledger,{{stall,Agent_Id},{Kind,Phase}})
	end.
%The exoself process' main loop awaits from its cortex proccess the evoluation_completed message. Once the message is received, based on the fitness achieved, exoself decides whether to continue tunning the weights or terminate the system. Exoself tries to improve the fitness by perturbing/tuning the weights of its neurons, after each tuning session, the Neural Network based system performs another evaluation by interacting with the scape until completion (the NN solves a problem, or dies within the scape or...). The order of events is important: When evaluation_completed message is received, the function first checks whether the newly achieved fitness is higher than the highest fitness achieved so far. If it is not, the function sends the neurons a message to restore their weights to previous state, during which it last acehived the highest fitness instead of their current state which yielded the current lower fitness score. If on the other hand the new fitness is higher than the previously highest achieved fitness, then the function tells the neurons to backup their current weights, as these weights represent the NN's best, most fit form yet. Exoself then tells all the neurons to prepare for a reset by sending each neuron the {self(),reset_prep} message. Since the NN can have recursive connections, and the manner in which initial recursive messages are sent, it is important for each neuron to flush their buffers to be reset into an initial fresh state, which is achieved after the neurons receive the reset_prep message. The function then sends the reset message to the neurons, which returns them into their main loop. Finally, the function checks whether exoself has already tried to improve the NN's fitness a maximum S#state.max_attempts number of times. If that is the case, the exoself process backs up the updated NN (the updated, tuned weights) to database using the backup_genotype/2 function, prints to screen that it is terminating, and sends to the population_monitor the acumulated statistics (highest fitness, evaluation count, cycle count...). On the other hand, if the exoself is not yet done tuning the neural weights, it has not yet reached its ending condition, it uses a tuning_selection_function to compose a list of tuples: [{NId,Spread}...] of neuron ids and the perturbation spread values, where the spread is the range from which the perturbation is randomly chosen. The spread itself is based on the age of the slected neuron, using the annealing_factor value, which when set to 1 implies that there is no annealing, and when set to a value less than 1, decreases the Spread. Once this list of elements is composed, the exoself sends each of the neurons a message to perturb their synaptic weights using the Spread value. The exoself then reactivates the cortex, and drops back into its main loop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Episode mode helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%In episode mode an agent's life in one generation is:
%	tune       (only when config:episode_tuning() is true) evaluations on the generation's tune episodes; weight
%	           perturbations are kept or undone on that fitness
%	score      one evaluation of the weights on the generation's score episodes; this fitness goes to selection
%	validation (every config:validate_population_every() generations) one evaluation on the validation episodes, logged only
%With tuning off, weights change only through the mutate_weights mutation operator, and every change is judged on the
%score episodes like any other mutation.

%tune_fitness/1 is the best tuning fitness, or undefined when the agent was not tuned.
tune_fitness(S)->
	case S#state.eval_acc of
		0 -> undefined;
		_ -> S#state.highest_fitness
	end.

%start_pass/2 restores the best weights in every neuron, switches the scapes to the given episode set and restarts the
%cortex. The network must already have been reset.
start_pass(S,Phase)->
	[NPId ! {self(),weight_restore} || NPId <- S#state.npids],
	[ScapePId ! {self(),eval_mode,Phase} || ScapePId <- S#state.scape_pids],
	S#state.cx_pid ! {self(),reactivate}.

%reset_network/1 puts the network back in its starting state between two evaluations. Returns ok | {failed,Reason}.
%With config:episode_isolation/0 off it is the reset this code always had (legacy_reset/1): the neurons empty their
%mailboxes and send their recurrent outputs a zero, but keep plastic weights and the diff_product memory, and a message
%sent by a neuron that was still computing when its target emptied its mailbox survives into the next evaluation.
%With it on it is full_reset/2, the reset used at every episode boundary as well.
reset_network(S)->
	case config:episode_isolation() of
		true -> full_reset(S,erlang:monotonic_time(millisecond)+stall_timeout());
		_ -> legacy_reset(S)
	end.

legacy_reset(S)->
	[PId ! {self(), reset_prep} || PId <- S#state.npids],
	gather_acks(length(S#state.npids)),
	[PId ! {self(), reset} || PId <- S#state.npids],
	case S#state.substrate_pid of
		undefined ->
			ok;
		Substrate_PId ->
			Substrate_PId ! {self(),reset_substrate},
			receive
				{Substrate_PId,ready}->
					ok
			end
	end.

%full_reset/2 resets the whole network with no bar in flight, in four steps. Each waits for every process it addresses.
%	1 sensors     reset_prep: a sensor answers once it has sent everything it was asked for (sensor.erl), so after this
%	              step no sensor sends anything until the cortex starts the next bar.
%	2 neurons     reset_prep: each empties its mailbox and holds (neuron:reset_hold/2).
%	3 neurons     reset_flush: once every neuron has answered step 2 none is computing, so this second emptying removes
%	              whatever a neuron that was still busy in step 2 sent after its target had emptied its mailbox.
%	4 neurons     reset_full: back to the weights the evaluation started with (plasticity undone), no diff_product memory,
%	              and a zero on every recurrent output, as a freshly spawned network has. A substrate is reset as before.
%The cortex and the actuators need nothing: the cortex is waiting for the actuator, and the actuator for the simulator.
%The exoself's own watchdog does not run while it waits here, so the wait has its own: a process with more than
%config:runaway_queue_limit() messages waiting is a runaway, and no answer by Deadline (config:stall_timeout_ms() after
%the reset began) is a stall. Returns ok | {failed,Reason} for give_up/3.
full_reset(S,Deadline)->
	Steps = [{sensors,S#state.spids,reset_prep},{neurons,S#state.npids,reset_prep},{neurons,S#state.npids,reset_flush}],
	case reset_steps(Steps,S,Deadline) of
		ok ->
			[PId ! {self(),reset_full} || PId <- S#state.npids],
			case S#state.substrate_pid of
				undefined ->
					ok;
				Substrate_PId ->
					Substrate_PId ! {self(),reset_substrate},
					await_ready([Substrate_PId],S,Deadline,substrate)
			end;
		Failed ->
			Failed
	end.

	reset_steps([],_S,_Deadline)->
		ok;
	reset_steps([{What,PIds,Msg}|Steps],S,Deadline)->
		[PId ! {self(),Msg} || PId <- PIds],
		case await_ready(PIds,S,Deadline,{What,Msg}) of
			ok -> reset_steps(Steps,S,Deadline);
			Failed -> Failed
		end.

	await_ready([],_S,_Deadline,_Step)->
		ok;
	await_ready(Pending,S,Deadline,Step)->
		receive
			{PId,ready} when is_pid(PId) ->
				await_ready(lists:delete(PId,Pending),S,Deadline,Step)
		after ?WATCH_INTERVAL_MS ->
			Limit = runaway_queue_limit(),
			case longest_queue(S) of
				{Length,PId} when Length > Limit ->
					{failed,{runaway,Length,PId}};
				_ ->
					case erlang:monotonic_time(millisecond) >= Deadline of
						true -> {failed,{stalled_in_reset,Step,length(Pending)}};
						false -> await_ready(Pending,S,Deadline,Step)
					end
			end
		end.

%episode_boundary/3: the simulator ScapePId ended an episode and holds the next bar until it hears go. The network is
%reset (full_reset/2); the stall clock then starts again from now, so the time spent here never counts as silence.
episode_boundary(S,OpMode,ScapePId)->
	case full_reset(S,erlang:monotonic_time(millisecond)+stall_timeout()) of
		ok ->
			ScapePId ! {self(),episode_go},
			Now = erlang:monotonic_time(millisecond),
			exoself:loop(S#state{last_progress_ms=Now,last_check_ms=Now},OpMode);
		{failed,Reason} ->
			give_up(S,OpMode,Reason)
	end.

%validation_due/0: every config:validate_population_every() generations, and with config:validate_last_generation() on
%also at the run's last generation (generation_limit - 1: the run ends when the next one would reach the limit).
validation_due()->
	Generation = config:get_val(episodes_generation,0),
	Every = case config:validate_population_every() of
		N when is_integer(N), N > 0 -> (Generation rem N) == 0;
		_ -> false
	end,
	Last = case {config:validate_last_generation(),config:generation_limit()} of
		{true,Limit} when is_integer(Limit) -> Generation == Limit-1;
		_ -> false
	end,
	Every orelse Last.

%take_eval_stats/1 collects the per-episode statistics the scape published for this exoself's latest evaluation.
take_eval_stats(Phase)->
	case ets:whereis(eval_stats) of
		undefined ->
			[];
		_ ->
			case ets:take(eval_stats,{self(),Phase}) of
				[{_Key,_Fitness,EpStats}] -> EpStats;
				[] -> []
			end
	end.

%save_scored_agent/1 stores what selection needs as soon as the score evaluation ends: the score fitness and the
%(possibly tuned) weights go to the genotype. The network has just completed a pass, so every neuron answers get_backup.
save_scored_agent(S)->
	Agent_Id = S#state.agent_id,
	A=genotype:dirty_read({agent,Agent_Id}),
	qlog:log_agent_metadata(Agent_Id, S#state.generation, self()),
	genotype:write(A#agent{fitness=S#state.score_fitness}),
	backup_genotype(S#state.idsNpids,S#state.npids).

%finish_episode_agent/3 ends the agent's turn: everything measured goes to the eval_ledger table for the generation
%summary, the network is shut down, and the score fitness (already stored by save_scored_agent/1) is reported to the
%population monitor. It talks to the network only through terminate messages, so it is safe to call on a stalled one.
finish_episode_agent(S,ValFitness,ValStats)->
	Agent_Id = S#state.agent_id,
	ScoreFitness = S#state.score_fitness,
	case ets:whereis(eval_ledger) of
		undefined ->
			ok;
		_ ->
			ets:insert(eval_ledger,{Agent_Id,#{
				tune=>tune_fitness(S),
				score=>ScoreFitness,
				score_summary=>fitness_functions:episode_summary(S#state.score_stats),
				score_cycles=>S#state.score_cycles,
				score_time_us=>S#state.score_time,
				validation=>ValFitness,
				validation_summary=>fitness_functions:episode_summary(ValStats),
				neurons=>length(S#state.nids),
				specie=>S#state.specie_id,
				evaluations=>S#state.eval_acc+1
			}})
	end,
	shutdown_phenotype(S),
	gen_server:cast(S#state.pm_pid,{Agent_Id,terminated,ScoreFitness}).

spawn_CerebralUnits(IdsNPIds,CerebralUnitType,[Id|Ids])->
	PId = CerebralUnitType:gen(self(),node()),
	ets:insert(IdsNPIds,{Id,PId}), 
	ets:insert(IdsNPIds,{PId,Id}), 
	spawn_CerebralUnits(IdsNPIds,CerebralUnitType,Ids); 
spawn_CerebralUnits(IdsNPIds,_CerebralUnitType,[])-> 
	ets:insert(IdsNPIds,{bias,bias}).
%We spawn the process for each element based on its type: CerebralUnitType, and the gen function that belongs to the CerebralUnitType module. We then enter the {Id,PId} tuple into our ETS table for later use.

spawn_Scapes(IdsNPIds,Sensor_Ids,Actuator_Ids,Agent_Id,OpMode)->
        Sensor_Scapes = [(genotype:dirty_read({sensor,Id}))#sensor.scape || Id<-Sensor_Ids], 
        Actuator_Scapes = [(genotype:dirty_read({actuator,Id}))#actuator.scape || Id<-Actuator_Ids], 
        Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes), 
        Mapped_Scapes = map_live_scapes(Unique_Scapes, OpMode),
        %% Preferred wiring: in live_trading, map live_sim to the registered live_scape PID
        Private_SN_Tuples=[
            case {OpMode, ScapeName} of
                {live_trading, live_sim} ->
                    ensure_live_scape_started(),
                    {whereis(live_scape), live_sim};
                _ ->
                    {scape:gen(self(),node()), ScapeName}
            end
        || {private, ScapeName} <- Mapped_Scapes],
        [ets:insert(IdsNPIds,{ScapeName,PId}) || {PId,ScapeName} <- Private_SN_Tuples], 
        [ets:insert(IdsNPIds,{PId,ScapeName}) || {PId,ScapeName} <-Private_SN_Tuples],
        %% Send handshake to all scapes (live_scape exits init on this)
        [PId ! {self(), ScapeName} || {PId, ScapeName} <- Private_SN_Tuples],
        [PId || {PId,_ScapeName} <-Private_SN_Tuples].
    map_live_scapes(Unique_Scapes, live_trading) ->
        [maybe_live(Sc) || Sc <- Unique_Scapes];
    map_live_scapes(Unique_Scapes, _Other) ->
        Unique_Scapes.

    maybe_live({private, fx_sim}) -> {private, live_sim};
    maybe_live(Other) -> Other.

    %% Map a single scape atom when linking elements
    maybe_map_scape_name(fx_sim, live_trading) -> live_sim;
    maybe_map_scape_name(ScapeName, _OpMode) -> ScapeName.
%The spawn_Scapes/3 function first extracts all the scapes that the sensors and actuators interface with, it then creates a filtered scape list which only holds unique scape records, after which it further only selects those scapes that are private, and spawns them.

ensure_live_scape_started() ->
    case whereis(live_scape) of
        undefined ->
            case live_scape:start_link() of
                {ok, _Pid} -> ok;
                _ -> ok
            end;
        _Pid -> ok
    end.

		enter_PublicScape(IdsNPIds,Sensor_Ids,Actuator_Ids,Agent_Id)->
			A = genotype:dirty_read({agent,Agent_Id}),
			Sensors = [genotype:dirty_read({sensor,Id}) || Id<-Sensor_Ids],
			Actuators = [genotype:dirty_read({actuator,Id}) || Id<-Actuator_Ids],
			TotNeurons = length((genotype:dirty_read({cortex,A#agent.cx_id}))#cortex.neuron_ids),
			Morphology = (A#agent.constraint)#constraint.morphology,
			Sensor_Scapes = [Sensor#sensor.scape || Sensor<-Sensors], 
			Actuator_Scapes = [Actuator#actuator.scape || Actuator<-Actuators], 
			Unique_Scapes = Sensor_Scapes++(Actuator_Scapes--Sensor_Scapes),
			Public_SN_Tuples=[{gen_server:call(polis,{get_scape,ScapeName}),ScapeName} || {public,ScapeName}<-Unique_Scapes],
			[gen_server:call(PId,{enter,Morphology,Agent_Id,Sensors,Actuators,TotNeurons}) || {PId,ScapeName} <- Public_SN_Tuples].
			
		reenter_PublicScape([PS_PId|PS_PIds],Sensors,Actuators,Agent_Id,Morphology,TotNeurons)->
			gen_server:call(PS_PId,{enter,Morphology,Agent_Id,Sensors,Actuators,TotNeurons}),
			reenter_PublicScape(PS_PIds,Sensors,Actuators,Agent_Id,Morphology,TotNeurons);
		reenter_PublicScape([],_Sensors,_Actuators,_Agent_Id,_Morphology,_TotNeurons)->
			ok.

	% Wrapper that checks sensors and ensures at least one is connected
	link_Sensors(Agent_Id, SIds, IdsNPIds, OpMode) when SIds =/= [] ->
		case [SId || SId <- SIds, (genotype:dirty_read({sensor, SId}))#sensor.fanout_ids == []] of
			SIds ->  % All sensors disconnected
				case catch genome_mutator:add_sensorlink(Agent_Id) of
					{atomic,_} -> link_Sensors(Agent_Id, SIds, IdsNPIds, OpMode);  % Retry
					_ -> 
						qlog:benchmarker(Agent_Id, io_lib:format("Error adding sensor link, proceeding anyway")),
						link_Sensors(SIds, IdsNPIds, OpMode)  % Proceed anyway
				end;
			_ ->  % At least one sensor connected
				link_Sensors(SIds, IdsNPIds, OpMode)
		end;
	link_Sensors(Agent_Id, [], IdsNPIds, OpMode) -> % No sensors, add a new sensor
		case catch genome_mutator:add_sensor(Agent_Id) of
			{atomic,_} -> 
				% Re-read cortex to get updated sensor list
				A = genotype:dirty_read({agent,Agent_Id}),
				Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
				link_Sensors(Agent_Id, Cx#cortex.sensor_ids, IdsNPIds, OpMode);  % Retry with new sensors
			_ -> 
				qlog:benchmarker(Agent_Id, io_lib:format("Error adding sensor, no sensors available")),
				ok  % Can't proceed without sensors
		end.

	link_Sensors([SId|Sensor_Ids],IdsNPIds,OpMode) ->
		S=genotype:dirty_read({sensor,SId}),
		SPId = ets:lookup_element(IdsNPIds,SId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,S#sensor.cx_id,2),
		SName = S#sensor.name,
		Fanout_Ids = S#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],
		Scape=case S#sensor.scape of
			{private,ScapeName}->
            MappedScapeName = maybe_map_scape_name(ScapeName, OpMode),
				ets:lookup_element(IdsNPIds,MappedScapeName,2)
    	end,
		SPId ! {self(),{SId,Cx_PId,Scape,SName,S#sensor.vl,S#sensor.parameters,Fanout_PIds,OpMode}},
		link_Sensors(Sensor_Ids,IdsNPIds,OpMode);
	link_Sensors([],_IdsNPIds,_OpMode)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the PId lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_Actuators([AId|Actuator_Ids],IdsNPIds,OpMode) ->
		A=genotype:dirty_read({actuator,AId}),
		APId = ets:lookup_element(IdsNPIds,AId,2),
		Cx_PId = ets:lookup_element(IdsNPIds,A#actuator.cx_id,2),
		AName = A#actuator.name,
		Fanin_Ids = A#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
		Scape=case A#actuator.scape of
			{private,ScapeName}->
            MappedScapeName = maybe_map_scape_name(ScapeName, OpMode),
				ets:lookup_element(IdsNPIds,MappedScapeName,2)
		end,
		APId ! {self(),{AId,Cx_PId,Scape,AName,A#actuator.parameters,Fanin_PIds,OpMode}},
		link_Actuators(Actuator_Ids,IdsNPIds,OpMode);
	link_Actuators([],_IdsNPIds,_OpMode)->
		ok.
%The link_Actuators2 function sends to the already spawned and waiting actuators their states, composed of the PId lists and other information which are needed by the actuators to link up and interface with other elements in the distributed phenotype.

	link_SubstrateCPPs([CPP_Id|CPP_Ids],IdsNPIds,Substrate_PId) ->
		CPP=genotype:dirty_read({sensor,CPP_Id}),
		CPP_PId = ets:lookup_element(IdsNPIds,CPP_Id,2),
		Cx_PId = ets:lookup_element(IdsNPIds,CPP#sensor.cx_id,2),
		CPPName = CPP#sensor.name,
		Fanout_Ids = CPP#sensor.fanout_ids,
		Fanout_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanout_Ids],
		CPP_PId ! {self(),{CPP_Id,Cx_PId,Substrate_PId,CPPName,CPP#sensor.vl,CPP#sensor.parameters,Fanout_PIds}},
		link_SubstrateCPPs(CPP_Ids,IdsNPIds,Substrate_PId);
	link_SubstrateCPPs([],_IdsNPIds,_Substrate_PId)->
		ok.
%The link_Sensors/2 function sends to the already spawned and waiting sensors their states, composed of the PId lists and other information which are needed by the sensors to link up and interface with other elements in the distributed phenotype.

	link_SubstrateCEPs([CEP_Id|CEP_Ids],IdsNPIds,Substrate_PId) ->
		CEP=genotype:dirty_read({actuator,CEP_Id}),
		CEP_PId = ets:lookup_element(IdsNPIds,CEP_Id,2),
		Cx_PId = ets:lookup_element(IdsNPIds,CEP#actuator.cx_id,2),
		CEPName = CEP#actuator.name,
		Fanin_Ids = CEP#actuator.fanin_ids,
		Fanin_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Fanin_Ids],
		CEP_PId ! {self(),{CEP_Id,Cx_PId,Substrate_PId,CEPName,CEP#actuator.parameters,Fanin_PIds}},
		link_SubstrateCEPs(CEP_Ids,IdsNPIds,Substrate_PId);
	link_SubstrateCEPs([],_IdsNPIds,_Substrate_PId)->
		ok.
%The link_SubstrateCEPs/2 function sends to the already spawned and waiting substrate_ceps their states, composed of the PId lists and other information which are needed by the substrate_ceps to link up and interface with other elements in the distributed phenotype.

link_Neurons([NId|Neuron_Ids],IdsNPIds,HeredityType) ->
	N=genotype:dirty_read({neuron,NId}),
	NPId = try
		ets:lookup_element(IdsNPIds,NId,2)
	catch
		error:badarg ->
			throw({neuron_not_found, NId, IdsNPIds})
	end,
	Cx_PId = ets:lookup_element(IdsNPIds,N#neuron.cx_id,2),
		AFName = N#neuron.af,
		PFName = N#neuron.pf,
		AggrFName = N#neuron.aggr_f,
		Input_IdPs = N#neuron.input_idps,
		Input_IdPs_Modulation = N#neuron.input_idps_modulation,
		Output_Ids = N#neuron.output_ids,
		RO_Ids = N#neuron.ro_ids,
		SI_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs,[]),
		MI_PIdPs = convert_IdPs2PIdPs(IdsNPIds,Input_IdPs_Modulation,[]),
		O_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- Output_Ids],
		RO_PIds = [ets:lookup_element(IdsNPIds,Id,2) || Id <- RO_Ids],
		NPId ! {self(),{NId,Cx_PId,AFName,PFName,AggrFName,HeredityType,SI_PIdPs,MI_PIdPs,O_PIds,RO_PIds}},
		link_Neurons(Neuron_Ids,IdsNPIds,HeredityType);
link_Neurons([],_IdsNPIds,_HeredityType)->
	ok.
%The link_Neurons/2 function sends to the already spawned and waiting neurons their states, composed of the PId lists and other information needed by the neurons to link up and interface with other elements in the distributed phenotype.

		convert_IdPs2PIdPs(IdsNPIds,[{Id,WeightsP}|Fanin_IdPs],Acc)->
			convert_IdPs2PIdPs(IdsNPIds,Fanin_IdPs,[{ets:lookup_element(IdsNPIds,Id,2),WeightsP}|Acc]);
		convert_IdPs2PIdPs(_IdsNPIds,[],Acc)->
			lists:reverse(Acc).
%The convert_IdPs2PIdPs/3 converts the IdPs tuples into tuples that use PIds instead of Ids, such that the Neuron will know which weights are to be associated with which incoming vector signals. The last element is the bias, which is added to the list in a non tuple form. Afterwards, the list is reversed to take its proper order.

	link_Cortex(Cx,IdsNPIds) ->
		Cx_Id = Cx#cortex.id,
		Cx_PId = ets:lookup_element(IdsNPIds,Cx_Id,2),
		SIds = Cx#cortex.sensor_ids,
		AIds = Cx#cortex.actuator_ids,
		NIds = Cx#cortex.neuron_ids,
		SPIds = [ets:lookup_element(IdsNPIds,SId,2) || SId <- SIds],
		NPIds = [ets:lookup_element(IdsNPIds,NId,2) || NId <- NIds],
		APIds = [ets:lookup_element(IdsNPIds,AId,2) || AId <- AIds],
		Cx_PId ! {self(),Cx_Id,SPIds,NPIds,APIds},
		{SPIds,NPIds,APIds}.
%The link_Cortex/2 function sends to the already spawned and waiting cortex its state, composed of the PId lists and other information which is needed by the cortex to link up and interface with other elements in the distributed phenotype.

backup_genotype(IdsNPIds,NPIds)->
	Neuron_IdsNWeights = get_backup(NPIds,[]),
	update_genotype(IdsNPIds,Neuron_IdsNWeights),
	io:format("Finished updating genotype~n").

	get_backup([NPId|NPIds],Acc)->
		NPId ! {self(),get_backup},
		receive
			{NPId,NId,SWeightTuples,MWeightTuples}->
				get_backup(NPIds,[{NId,SWeightTuples,MWeightTuples}|Acc])
		end;
	get_backup([],Acc)->
		Acc.
%The backup_genotype/2 uses get_backup/2 to contact all the neurons in its NN and request for the neuron's Ids and their Input_IdPs. Once the updated Input_IdPs from all the neurons have been accumulated, they are passed through the update_genotype/2 function to produce updated neurons, and write them to database.

	update_genotype(IdsNPIds,[{N_Id,SI_PIdPs,MI_PIdPs}|WeightPs])->
		N = genotype:dirty_read({neuron,N_Id}),
		Updated_SI_IdPs = convert_PIdPs2IdPs(IdsNPIds,SI_PIdPs,[]),
		Updated_MI_IdPs = convert_PIdPs2IdPs(IdsNPIds,MI_PIdPs,[]),
		U_N = N#neuron{input_idps = Updated_SI_IdPs,input_idps_modulation=Updated_MI_IdPs},
		genotype:write(U_N),
		update_genotype(IdsNPIds,WeightPs);
	update_genotype(_IdsNPIds,[])->
		ok.
%For every {N_Id,PIdPs} tuple the update_genotype/3 function extracts the neuron with the id: N_Id, updates the neuron's input_IdPs, and writes the updated neuron to database.

		convert_PIdPs2IdPs(IdsNPIds,[{PId,WeightsP}|Input_PIdPs],Acc)->
			convert_PIdPs2IdPs(IdsNPIds,Input_PIdPs,[{ets:lookup_element(IdsNPIds,PId,2),WeightsP}|Acc]);
		convert_PIdPs2IdPs(_IdsNPIds,[],Acc)->
			lists:reverse(Acc).
%The convert_PIdPs2IdPs/3 performs the conversion from PIds to Ids of every {PId,Weights} tuple in the Input_PIdPs list. The updated Input_IdPs are then returned to the caller.
	
%% Collect total reductions from all agent processes
collect_total_reductions(S) ->
    All_Pids = [S#state.cx_pid | S#state.spids] ++ S#state.npids ++ 
                S#state.apids ++ S#state.scape_pids ++ S#state.cpp_pids ++ 
                S#state.cep_pids ++ 
                case S#state.substrate_pid of
                    undefined -> [];
                    SubPid -> [SubPid]
                end,
    lists:foldl(fun(Pid, Acc) ->
        case catch erlang:process_info(Pid, reductions) of
            {reductions, Reductions} -> Acc + Reductions;
            _ -> Acc  % Process might be dead
        end
    end, 0, All_Pids).

terminate_phenotype(Cx_PId,SPIds,NPIds,APIds,ScapePIds,CPP_PIds,CEP_PIds,Substrate_PId)->
	%io:format("Terminating the phenotype:~nCx_PId:~p~nSPIds:~p~nNPIds:~p~nAPIds:~p~nScapePids:~p~n",[Cx_PId,SPIds,NPIds,APIds,ScapePIds]),
	[PId ! {self(),terminate} || PId <- SPIds],
	[PId ! {self(),terminate} || PId <- APIds],
	[PId ! {self(),terminate} || PId <- NPIds],
	[PId ! {self(),terminate} || PId <- ScapePIds],
	case Substrate_PId == undefined of
		true ->
			ok;
		false ->
			[PId ! {self(),terminate} || PId <- CPP_PIds],
			[PId ! {self(),terminate} || PId <- CEP_PIds],
			Substrate_PId ! {self(),terminate}
	end,
	Cx_PId ! {self(),terminate}.
%The terminate_phenotype/5 function termiantes sensors, actuators, neurons, all private scapes, and the cortex which composes the NN based system.

gather_acks(0)->
	done;	
gather_acks(PId_Index)->
	receive
		{_From,ready}->
			gather_acks(PId_Index-1)
		after 100000 ->
			qlog:xLog(qStatus, "Not all acks received:~p~n", [PId_Index]),
			io:format("******** Not all acks received:~p~n",[PId_Index])
	end.
%gather_acks/1 ensures that the X number of {From,ready} messages are sent to it, before it returns with done. X is set by the caller of the function.
