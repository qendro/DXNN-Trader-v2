%% Result-folder analysis: the Erlang half of analysis/pipeline.js (REVIEW.md 8.23, the four-arm experiment and folds).
%%
%% Every function here runs in a VM started by pipeline.js, one at a time, in a work directory whose
%% fx_tables/EURUSD1.txt is a copy of the price file cut before 2018-01-01. Nothing here opens any other price file.
%%
%%   resim_job(SpecFile)   restore one result folder's Mnesia backup, then for each saved population named in the spec:
%%                         pick the best N agents by the run's own ranking (#agent.fitness, the value selection read),
%%                         re-simulate them with resim:run/4 on the run's training span and on its validation span under
%%                         the conditions asked for (all twelve of resim:conditions/0 by default), and run the six
%%                         baselines on the same episodes under the same conditions. Writes CSV files and halts.
%%   test_run(SpecFile)    a small evolution run in a fresh database: one arm with its seeds (environment variables,
%%                         as exp_runner:get_run_configs/0 reads them), shrunk to a few agents, episodes and
%%                         generations. Ends with mnesia:backup/1, so its folder looks like a cloud result folder.
%%
%% Hard rules enforced here:
%%   - check_span/1 refuses (erlang:error) any span that ends after 2017-12-31.
%%   - check_episodes/2 refuses any episode whose rows reach 2018 or run past the end of the table.
%%   - check_table/0 refuses to start when the loaded price table holds a row of 2018 or later.
-module(dxnn_results).
-export([resim_job/1, test_run/1, check_span/1, check_episodes/2, check_table/0, best_agents/2, policies/0]).
-include("records.hrl").

-define(LAST_DAY,{2017,12,31}).
-define(TABLE,'EURUSD1').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Guards %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_span({From,To}=Span) when is_tuple(From), is_tuple(To)->
	case date_of(To) =< ?LAST_DAY of
		true -> ok;
		false -> erlang:error({span_ends_after_2017_12_31,Span})
	end;
check_span(Span)->
	erlang:error({span_not_a_date_range,Span}).

	date_of({Y,M,D}) -> {Y,M,D};
	date_of({Y,M,D,_H,_Mi,_S,_R}) -> {Y,M,D}.

%check_episodes/2: every episode {StartKey,Bars} must have Bars+1 rows in the table (resim reads one row past the
%last bar), and its last row must be on or before 2017-12-31.
check_episodes(Table,Episodes)->
	[begin
		case date_of(StartKey) =< ?LAST_DAY of
			true -> ok;
			false -> erlang:error({episode_starts_after_2017,StartKey})
		end,
		Last = nth_key(Table,StartKey,Bars),
		case Last of
			'$end_of_table' -> erlang:error({episode_runs_past_table_end,StartKey,Bars});
			_ -> ok
		end,
		case date_of(Last) =< ?LAST_DAY of
			true -> ok;
			false -> erlang:error({episode_reaches_2018,StartKey,Last})
		end
	 end || {StartKey,Bars} <- Episodes],
	ok.

	nth_key(_Table,'$end_of_table',_N)-> '$end_of_table';
	nth_key(_Table,Key,0)-> Key;
	nth_key(Table,Key,N)-> nth_key(Table,ets:next(Table,Key),N-1).

check_table()->
	case ets:last(?TABLE) of
		'$end_of_table' -> erlang:error(price_table_empty);
		Last ->
			case date_of(Last) =< ?LAST_DAY of
				true -> {ok,ets:info(?TABLE,size),ets:first(?TABLE),Last};
				false -> erlang:error({price_table_reaches_2018,Last})
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Boot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%boot_restore/1: Mnesia from a backup into the (empty) directory given to the VM with -mnesia dir, then the prices.
boot_restore(Backup)->
	case application:load(mnesia) of ok -> ok; {error,{already_loaded,mnesia}} -> ok end,
	Dir = case application:get_env(mnesia,dir) of
		{ok,D} -> D;
		undefined -> erlang:error(no_mnesia_dir_given)
	end,
	ok = filelib:ensure_dir(filename:join(Dir,"x")),
	case filelib:wildcard(filename:join(Dir,"*")) of
		[] -> ok;
		_ -> erlang:error({mnesia_dir_not_empty,Dir})
	end,
	ok = mnesia:install_fallback(Backup,[{scope,local},{mnesia_dir,Dir}]),
	ok = mnesia:start(),
	ok = mnesia:wait_for_tables([population,specie,agent,cortex,neuron,sensor,actuator,experiment],300000),
	boot_prices().

boot_fresh()->
	polis:create(),
	ok = mnesia:wait_for_tables([population,specie,agent,cortex,neuron,sensor,actuator,substrate,experiment],120000),
	boot_prices().

boot_prices()->
	T0 = erlang:monotonic_time(millisecond),
	fx:init(),
	register(launcher,self()),
	fx:start(),
	receive fx_updated -> ok after 900000 -> erlang:error(fx_timeout) end,
	unregister(launcher),
	flush_fx(),
	config:init(),
	{ok,Rows,First,Last} = check_table(),
	io:format("BOOT price rows=~p first=~p last=~p in ~p ms~n",[Rows,First,Last,erlang:monotonic_time(millisecond)-T0]),
	ok.

	flush_fx()-> receive fx_updated -> flush_fx() after 0 -> ok end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Re-simulation job %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%resim_job/1. The spec file holds one map:
%  #{backup=>Path, out_dir=>Path, experiment_id=>Binary|undefined, n_best=>8, train_k=>900,
%    conditions=>all|[ConditionName], parallel=>N,
%    jobs=>[#{run_index=>I, population_id=>Binary, label=>String}]}
%For each job it writes into <out_dir>/<label>/: chosen.csv, Resim/train/*.csv, Resim/validation/*.csv,
%baselines.csv, episodes_used.csv and done.txt (last, so a job without done.txt did not finish).
resim_job(SpecFile)->
	run_and_halt(fun()->
		{ok,[Spec]} = file:consult(SpecFile),
		boot_restore(maps:get(backup,Spec)),
		E = experiment(maps:get(experiment_id,Spec,undefined)),
		[job(Spec,E,Job) || Job <- maps:get(jobs,Spec)],
		ok
	end).

experiment(Wanted)->
	Ids = mnesia:dirty_all_keys(experiment),
	Id = case {Wanted,Ids} of
		{undefined,[Only]} -> Only;
		{undefined,_} -> erlang:error({several_experiments_name_one,Ids});
		_ ->
			case lists:member(Wanted,Ids) of
				true -> Wanted;
				false -> erlang:error({experiment_not_in_backup,Wanted,Ids})
			end
	end,
	[E] = mnesia:dirty_read(experiment,Id),
	E.

job(Spec,E,Job)->
	#{run_index:=RunIndex,population_id:=PopId,label:=Label} = Job,
	OutDir = filename:join(maps:get(out_dir,Spec),Label),
	ok = filelib:ensure_dir(filename:join(OutDir,"x")),
	file:delete(filename:join(OutDir,"done.txt")),
	%The run's own settings, exactly as the run had them (the experiment record keeps every run's config list).
	case lists:keyfind(RunIndex,1,E#experiment.run_configs) of
		{RunIndex,List} -> config:clear(), config:load_from_list(List);
		false -> erlang:error({run_not_in_experiment,RunIndex})
	end,
	TrainSpan = config:train_span(),
	ValSpan = config:val_span(),
	check_span(TrainSpan),
	check_span(ValSpan),
	Bars = config:episode_bars(),
	Train = episodes:tile(?TABLE,TrainSpan,Bars,maps:get(train_k,Spec,900)),
	Val = episodes:tile(?TABLE,ValSpan,Bars,config:episodes_val()),
	check_episodes(?TABLE,Train),
	check_episodes(?TABLE,Val),
	write_episodes(filename:join(OutDir,"episodes_used.csv"),[{train,Train},{validation,Val}]),
	{Chosen,Skipped} = best_agents(PopId,maps:get(n_best,Spec,8)),
	write_chosen(filename:join(OutDir,"chosen.csv"),Chosen,Skipped,PopId),
	Conditions = case maps:get(conditions,Spec,all) of
		all -> resim:conditions();
		Names -> [C || C <- resim:conditions(), lists:member(resim:condition_name(C),Names)]
	end,
	io:format("JOB ~s run ~p pop ~s: ~p agents (~p clones skipped), train ~p episodes ~s..~s, validation ~p episodes ~s..~s, ~p conditions~n",
		[Label,RunIndex,PopId,length(Chosen),Skipped,length(Train),fmt_span(TrainSpan),fmt_last(Train),length(Val),fmt_span(ValSpan),fmt_last(Val),length(Conditions)]),
	Agent_Ids = [maps:get(id,A) || A <- Chosen],
	Opts = #{conditions=>Conditions,parallel=>maps:get(parallel,Spec,erlang:system_info(schedulers_online))},
	%resim writes under <log root>/Resim/<Label>.
	application:set_env(qlog,log_root_dir,OutDir),
	resim:run(Agent_Ids,Train,"train",Opts),
	resim:run(Agent_Ids,Val,"validation",Opts),
	baselines(filename:join(OutDir,"baselines.csv"),Conditions,[{train,Train},{validation,Val}]),
	ok = file:write_file(filename:join(OutDir,"done.txt"),io_lib:format("~s~n",[PopId])),
	ok.

	fmt_span({From,To})-> io_lib:format("~w..~w",[From,To]).
	fmt_last([])-> "none";
	fmt_last(Eps)-> episodes:describe(lists:last(Eps)).

%best_agents/2: the agents of a saved population ranked by their stored fitness (what selection ranked them by), best
%first. Offspring carry their parent's fitness until they are evaluated, so one agent per fitness value is kept: the
%one of the lowest generation (the parent; mutation raises the generation), ties by id order. Returns {Chosen,Skipped}.
best_agents(PopId,N)->
	Agents = [A || Id <- eval_protocol:population_agents(PopId), A <- [genotype:dirty_read({agent,Id})], is_record(A,agent)],
	Rows = [#{id=>A#agent.id,fitness=>A#agent.fitness,generation=>A#agent.generation,
		neurons=>neurons(A),specie=>A#agent.specie_id} || A <- Agents],
	Sorted = lists:sort(fun(X,Y)-> sort_key(X) =< sort_key(Y) end,Rows),
	{Unique,Skipped} = lists:foldl(fun(Row,{Acc,S})->
		F = maps:get(fitness,Row),
		case lists:any(fun(R)-> maps:get(fitness,R) == F end,Acc) of
			true -> {Acc,S+1};
			false -> {[Row|Acc],S}
		end end,{[],0},Sorted),
	{lists:sublist(lists:reverse(Unique),N),Skipped}.

	sort_key(#{fitness:=F,generation:=G,id:=Id})->
		{case is_number(F) of true -> -F; false -> 1.0e308 end,G,Id}.

	neurons(A)->
		case genotype:dirty_read({cortex,A#agent.cx_id}) of
			Cx when is_record(Cx,cortex) -> length(Cx#cortex.neuron_ids);
			_ -> undefined
		end.

write_chosen(File,Chosen,Skipped,PopId)->
	Head = "rank,agent,fitness,generation,neurons,clones_skipped_in_population,population\n",
	Rows = [io_lib:format("~p,\"~w\",~s,~p,~p,~p,~s~n",[R,maps:get(id,A),num(maps:get(fitness,A)),maps:get(generation,A),maps:get(neurons,A),Skipped,PopId])
		|| {R,A} <- lists:zip(lists:seq(1,length(Chosen)),Chosen)],
	ok = file:write_file(File,[Head|Rows]).

write_episodes(File,Sets)->
	Rows = [io_lib:format("~s,~s,~p~n",[Set,key_str(Start),Bars]) || {Set,Eps} <- Sets, {Start,Bars} <- Eps],
	ok = file:write_file(File,["span,episode_start,bars\n"|Rows]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Baselines %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%The six rules GEN_SUMMARY reports next to the agents (eval_protocol:rule_scores/2 and extra_baselines/2).
policies()->
	[{rev60,{reversal,60}},{rev240,{reversal,240}},{flat,never},{long,always_long},{mom60,{momentum,60}},{clock,clock}].

%baselines/3 runs each rule through the simulator on the same episodes and under the same conditions as the agents.
%Writes one row per condition, span and rule, and one row per condition, span, rule and episode (baseline_episodes.csv).
baselines(File,Conditions,Sets)->
	EpFile = filename:join(filename:dirname(File),"baseline_episodes.csv"),
	ok = file:write_file(EpFile,"condition,span,rule,episode_start,money_pips,positions\n"),
	Pip = config:pip_size(),
	Rows = [begin
		Spread = maps:get(cost,C)*Pip,
		Name = resim:condition_name(C),
		Overrides = [{account_spread,Spread},{validation_spread,Spread},{rollover_rule,maps:get(rollover,C)},{late_fill,maps:get(late,C)}],
		Stats = eval_protocol:with_config(Overrides,fun()-> eval_protocol:run(Policy,Eps,Set) end),
		ok = file:write_file(EpFile,[io_lib:format("~s,~s,~s,~s,~s,~p~n",[Name,Set,Rule,key_str(maps:get(start,S)),
			num((maps:get(gross,S)-maps:get(cost,S))/Pip),maps:get(entries,S)]) || S <- Stats],[append]),
		Sum = fitness_functions:episode_summary(Stats),
		K = length(Stats),
		io_lib:format("~s,~s,~s,~p,~s,~s,~s~n",[Name,Set,Rule,K,num(eval_protocol:net_pips_ep(Sum)),
			num(maps:get(entries,Sum)/erlang:max(1,K)),num(fitness_functions:aggregate_episodes(Stats))])
	 end || C <- Conditions, {Set,Eps} <- Sets, {Rule,Policy} <- policies()],
	ok = file:write_file(File,["condition,span,rule,episodes,money_pips_per_episode,positions_per_episode,fitness\n"|Rows]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Test run %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%test_run/1. The spec file holds one map:
%  #{backup=>Path, agents=>24, episodes_score=>8, episodes_val=>40,
%    runs=>[{SourceRunIndex,Generations}], timeout_s=>540}
%The arm and seeds come from the environment (DXNN_TEST_ARM, DXNN_EPISODE_SEED, DXNN_EVOLUTION_SEED), read by
%exp_runner:get_run_configs/0. The runs named are taken from that list in the order given, renumbered from 1, with the
%population size, episode counts and generation limit replaced; everything else is the arm's own setting.
%The experiment is started the way exp_runner:start_internal/2 starts one (that function is not exported).
test_run(SpecFile)->
	run_and_halt(fun()->
		{ok,[Spec]} = file:consult(SpecFile),
		boot_fresh(),
		Full = exp_runner:get_run_configs(),
		Size = maps:get(agents,Spec,24),
		RunConfigs = [{I,lists:foldl(fun({Key,_}=Setting,Acc)-> lists:keystore(Key,1,Acc,Setting) end,List,
				[{specie_size_limit,Size},{init_specie_size,Size},{episodes_score,maps:get(episodes_score,Spec,8)},
				 {episodes_val,maps:get(episodes_val,Spec,40)},{generation_limit,Gens}])}
			|| {I,{Source,Gens}} <- lists:zip(lists:seq(1,length(maps:get(runs,Spec))),maps:get(runs,Spec)),
			   {_,List} <- [lists:keyfind(Source,1,Full)]],
		[check_span(proplists:get_value(K,L)) || {_,L} <- RunConfigs, K <- [train_span,val_span], proplists:is_defined(K,L)],
		E = start_experiment(RunConfigs),
		wait_complete(E#experiment.id,erlang:monotonic_time(second)+maps:get(timeout_s,Spec,540)),
		Backup = maps:get(backup,Spec),
		ok = mnesia:backup(Backup),
		io:format("TEST_RUN complete: experiment ~s, backup ~s~n",[E#experiment.id,Backup]),
		ok
	end).

start_experiment(RunConfigs)->
	config:init(),
	Lineage = list_to_binary(lists:flatten([timestamp("-"),"_",random_code()])),
	PopId = list_to_binary([timestamp(":"),"_",Lineage,"_run1"]),
	{1,First} = lists:keyfind(1,1,RunConfigs),
	config:clear(),
	config:load_from_list(First),
	PMP = #pmp{op_mode=benchmark,population_id=PopId,survival_percentage=config:survival_percentage(),
		specie_size_limit=config:specie_size_limit(),init_specie_size=config:init_specie_size(),polis_id=mathema,
		generation_limit=config:generation_limit(),evaluations_limit=config:evaluations_limit(),fitness_goal=inf,benchmarker_pid=self()},
	Constraints = lists:append(lists:duplicate(erlang:max(1,config:families()),
		[#constraint{morphology=config:morphology(),connection_architecture=config:connection_architecture(),
			population_evo_alg_f=config:population_evo_alg_f(),neural_pfns=config:neural_plasticity_functions(),
			agent_encoding_types=config:agent_encoding_types()}])),
	E = #experiment{id=PopId,backup_flag=true,pm_parameters=PMP,init_constraints=Constraints,progress_flag=in_progress,
		run_index=1,tot_runs=length(RunConfigs),run_configs=RunConfigs,started={date(),time()},interruptions=[]},
	qlog:exp_runner(experiment_start,{PopId,PopId,length(RunConfigs),length(RunConfigs)}),
	genotype:write(E),
	spawn(exp_runner,prep,[E,new_evo,undefined]),
	E.

	timestamp(Sep)->
		{{Y,Mo,D},{H,Mi,S}} = calendar:universal_time(),
		lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B~s~2..0B~s~2..0BZ",[Y,Mo,D,H,Sep,Mi,Sep,S])).

	random_code()->
		Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
		[lists:nth(rand:uniform(length(Chars)),Chars) || _ <- lists:seq(1,4)].

wait_complete(ExpId,Deadline)->
	case mnesia:dirty_read(experiment,ExpId) of
		[#experiment{progress_flag=completed}] -> ok;
		_ ->
			case erlang:monotonic_time(second) > Deadline of
				true -> erlang:error(test_run_timeout);
				false -> timer:sleep(2000), wait_complete(ExpId,Deadline)
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_and_halt(Fun)->
	try Fun() of
		_ -> io:format("DXNN_RESULTS_OK~n"), timer:sleep(200), erlang:halt(0)
	catch Class:Reason:Stack ->
		io:format("DXNN_RESULTS_ERROR ~p:~p~n~p~n",[Class,Reason,Stack]),
		timer:sleep(200),
		erlang:halt(2)
	end.

key_str({Y,Mo,D,H,Mi,_S,_Rate})->
	lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B",[Y,Mo,D,H,Mi])).

num(undefined)-> "";
num(V) when is_integer(V)-> integer_to_list(V);
num(V) when is_float(V)-> lists:flatten(io_lib:format("~.6f",[V]));
num(V)-> lists:flatten(io_lib:format("~p",[V])).
