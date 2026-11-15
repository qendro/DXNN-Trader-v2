%% Checks for flat_eval.erl: does the single-process evaluator give the same positions and money as the process path,
%% and how much faster is it. Not used by any run.
%%
%%   boot()                 start Mnesia (the database in the current directory) and the price table
%%                          (fx_tables/EURUSD1.txt in the current directory), and load the night arm's run 6 settings
%%   census(Suffix)         eval_protocol:census/1 of every saved population whose id ends with Suffix
%%   support(Suffix)        how many of their agents flat_eval:compile/1 accepts, and why it refuses the others
%%   match(Agent_Ids,Episodes,Label)   every agent through both paths; per agent the episodes whose statistics differ
%%   saved_match(Suffixes,NTrain,Seed) the agents of those populations on NTrain train episodes (fixed seed) and on
%%                          the validation set
%%   fresh_match(N,Episodes,Seed)      N freshly created and mutated agents, the same comparison
%%   speed(Agent_Ids,Episodes,Repeats) bars per second, flat against process, on one core and on all
-module(flat_eval_check).
-compile([export_all,nowarn_export_all]).
-include("records.hrl").

-define(ROWS_2017,2225779).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Boot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boot()->
	boot(night_run6).
boot(Settings)->
	ok = mnesia:start(),
	ok = mnesia:wait_for_tables([agent,cortex,neuron,sensor,actuator,population,specie],60000),
	config:init(),
	config:clear(),
	config:load_from_list(settings(Settings)),
	load_prices(),
	ok.

	%The night arm's last run (DXNN_TEST_ARM=night, run 6): a quarter pip, the rollover rule on, fill now, isolation and
	%ordered sensing on.
	settings(night_run6)->
		{6,List} = lists:keyfind(6,1,exp_runner:arm_run_configs("night")),
		List;
	settings(List) when is_list(List)->
		List.

	load_prices()->
		case ets:whereis('EURUSD1') of
			undefined ->
				fx:init(),
				fx:start(),
				wait_rows(0);
			_ ->
				ok
		end,
		Size = ets:info('EURUSD1',size),
		Last = ets:last('EURUSD1'),
		%Nothing dated 2018 or later may be read: the copy in the current directory is cut at the last row of 2017.
		{2017,12,29,21,59,_,_} = Last,
		Size = ?ROWS_2017,
		io:format("price table: ~p rows, last ~p~n",[Size,Last]),
		ok.

	wait_rows(N) when N > 600 ->
		erlang:error(price_table_not_loaded);
	wait_rows(N)->
		case ets:whereis('EURUSD1') =/= undefined andalso ets:info('EURUSD1',size) == ?ROWS_2017 of
			true -> timer:sleep(1000);
			false -> timer:sleep(1000), wait_rows(N+1)
		end.

populations(Suffix)->
	[Id || Id <- mnesia:dirty_all_keys(population), lists:suffix(Suffix,to_list(Id))].

	to_list(A) when is_atom(A)-> atom_to_list(A);
	to_list(B) when is_binary(B)-> binary_to_list(B);
	to_list(L) when is_list(L)-> L;
	to_list(X)-> lists:flatten(io_lib:format("~p",[X])).

census(Suffix)->
	[{P,eval_protocol:census(P)} || P <- populations(Suffix)].

support(Suffix)->
	Agents = lists:append([eval_protocol:population_agents(P) || P <- populations(Suffix)]),
	Verdicts = [case flat_eval:compile(A) of {ok,_} -> ok; {unsupported,Why} -> {unsupported,reason_kind(Why)} end || A <- Agents],
	Count = lists:foldl(fun(V,M)-> maps:update_with(V,fun(C)-> C+1 end,1,M) end,#{},Verdicts),
	io:format("~s: ~p agents, ~p~n",[Suffix,length(Agents),Count]),
	Count.

	reason_kind(Why) when is_tuple(Why)-> element(1,Why);
	reason_kind(Why)-> Why.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Match %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%match/3 runs Agent_Ids on Episodes through the process path (eval_protocol:run_agents_on/4, which keeps the position at
%every scored bar) and through flat_eval, under the same settings, and compares every episode's statistics map field by
%field with =:= (positions, money, cost, entries, drift, z, alpha, ...). Returns a summary map.
match(Agent_Ids,Episodes,Label)->
	match(Agent_Ids,Episodes,Label,[]).
match(Agent_Ids,Episodes,Label,Overrides)->
	T0 = erlang:monotonic_time(millisecond),
	Process = eval_protocol:run_agents_on(Agent_Ids,Episodes,Overrides),
	T1 = erlang:monotonic_time(millisecond),
	put(flat_eval_fallbacks,0),
	Flat = flat_on(Agent_Ids,Episodes,Overrides),%the shared path (run_many's default)
	T2 = erlang:monotonic_time(millisecond),
	Fallbacks = get(flat_eval_fallbacks),
	V1 = flat_on(Agent_Ids,Episodes,Overrides,#{share=>false}),
	V1Differ = length([1 || {{Id,F},{Id,F1}} <- lists:zip(Flat,V1), F =/= F1]),
	Rows = [compare_agent(Id,P,F) || {{Id,P},{Id,F}} <- lists:zip(Process,Flat)],
	Sum = fun(K)-> lists:sum([maps:get(K,R) || R <- Rows]) end,
	Summary = #{label=>Label,agents=>length(Rows),
		compared=>length([1 || #{status:=compared} <- Rows]),
		unsupported=>length([1 || #{status:={unsupported,_}} <- Rows]),
		process_errors=>length([1 || #{status:={process_error,_}} <- Rows]),
		episodes=>Sum(episodes),bars=>Sum(bars),
		episode_mismatches=>Sum(episode_mismatches),bar_mismatches=>Sum(bar_mismatches),
		process_s=>(T1-T0)/1000,flat_s=>(T2-T1)/1000,fallback_bars=>Fallbacks,v1_differs=>V1Differ},
	io:format("MATCH ~s ~p~n",[Label,Summary]),
	[io:format("  MISMATCH ~p ~p~n",[maps:get(agent,R),maps:get(first_mismatch,R)]) || R <- Rows, maps:get(episode_mismatches,R) > 0],
	[io:format("  NOT COMPARED ~p ~p~n",[maps:get(agent,R),maps:get(status,R)]) || R <- Rows, maps:get(status,R) =/= compared],
	{Summary,Rows}.

	%flat_on/3,4: the same agents through flat_eval (run_many/3 with FlatOpts) with the settings run_agents_on/4 puts in
	%force for its own run.
	flat_on(Agent_Ids,Episodes,Overrides)->
		flat_on(Agent_Ids,Episodes,Overrides,#{}).
	flat_on(Agent_Ids,Episodes,Overrides,FlatOpts)->
		in_eval_config(Episodes,Overrides,fun()-> flat_eval:run_many(Agent_Ids,Episodes,maps:merge(#{mode=>validation},FlatOpts)) end).

	%in_eval_config/3: Fun() under the settings run_agents_on/4 puts in force for its own run on Episodes.
	in_eval_config(Episodes,Overrides,Fun)->
		Costs = [{validation_spread,proplists:get_value(validation_spread,Overrides,undefined)}],
		eval_protocol:with_config(Costs++proplists:delete(validation_spread,Overrides),fun()->
			Table = config:primary_currency_pair(),
			ValParams = {Table,config:val_span(),config:episode_bars(),config:episodes_val(),ets:info(Table,size)},
			DriftParams = {Table,config:val_span(),ets:info(Table,size),config:episode_bars(),config:episodes_val(),config:episode_warmup_bars(),config:episode_anchor_step()},
			Mu = episodes:set_drift(Table,Episodes),
			eval_protocol:with_config([{validation_series,true},{episodes_validation_set,Episodes},{episodes_validation_params,ValParams},
				{{episodes_drift,validation},{DriftParams,Mu}}],fun()->
				Fun()
			end)
		end).

	compare_agent(Id,{ok,PStats},{ok,FStats})->
		Pairs = lists:zip(PStats,FStats,trim),
		Bad = [{I,diff_fields(P,F)} || {I,{P,F}} <- lists:zip(lists:seq(1,length(Pairs)),Pairs), P =/= F],
		BarBad = lists:sum([bar_diff(P,F) || {P,F} <- Pairs]),
		#{agent=>Id,status=>compared,episodes=>length(PStats),
		  bars=>lists:sum([length(maps:get(u_series,P)) || P <- PStats]),
		  episode_mismatches=>length(Bad)+abs(length(PStats)-length(FStats)),
		  bar_mismatches=>BarBad,
		  first_mismatch=>case Bad of [] -> {lengths,length(PStats),length(FStats)}; [B|_] -> B end};
	compare_agent(Id,{ok,_},{unsupported,Why})->
		empty(Id,{unsupported,Why});
	compare_agent(Id,{error,Why},_)->
		empty(Id,{process_error,Why}).

	empty(Id,Status)->
		#{agent=>Id,status=>Status,episodes=>0,bars=>0,episode_mismatches=>0,bar_mismatches=>0,first_mismatch=>none}.

	diff_fields(P,F)->
		[{K,maps:get(K,P,missing),maps:get(K,F,missing)} || K <- lists:usort(maps:keys(P)++maps:keys(F)), K =/= u_series,
			maps:get(K,P,missing) =/= maps:get(K,F,missing)]
		++ [{u_series_bars_differ,bar_diff(P,F)} || maps:get(u_series,P,x) =/= maps:get(u_series,F,y)].

	bar_diff(P,F)->
		U1 = maps:get(u_series,P,[]),
		U2 = maps:get(u_series,F,[]),
		length([1 || {X,Y} <- lists:zip(U1,U2,trim), X =/= Y]) + abs(length(U1)-length(U2)).

%saved_match/3: every agent of the populations whose id ends with one of Suffixes, on NTrain train episodes drawn with
%Seed and on the validation set of the settings in force.
saved_match(Suffixes,NTrain,Seed)->
	Agents = lists:append([eval_protocol:population_agents(P) || S <- Suffixes, P <- populations(S)]),
	Train = train_episodes(NTrain,Seed),
	%the whole of 2017 (the night arm itself validated on 120 of these)
	Val = episodes:tile(config:primary_currency_pair(),config:val_span(),config:episode_bars(),400),
	io:format("saved_match: ~p agents, ~p train episodes (~s .. ~s), ~p validation episodes~n",
		[length(Agents),length(Train),episodes:describe(hd(Train)),episodes:describe(lists:last(Train)),length(Val)]),
	[{train,element(1,match_batches(Agents,Train,"train"))},{validation,element(1,match_batches(Agents,Val,"validation"))}].

	train_episodes(K,Seed)->
		{[Set],_} = episodes:sample(config:primary_currency_pair(),config:train_span(),[K],config:episode_bars(),rand:seed_s(exsss,{Seed,Seed+1,Seed+2})),
		Set.

	%match_batches/3: in batches, so that one failure does not lose everything, and progress shows.
	match_batches(Agents,Episodes,Label)->
		Results = [match(Batch,Episodes,Label) || Batch <- chunks(Agents,48)],
		Summaries = [S || {S,_} <- Results],
		Keys = [agents,compared,unsupported,process_errors,episodes,bars,episode_mismatches,bar_mismatches,process_s,flat_s,fallback_bars,v1_differs],
		Total = maps:from_list([{K,lists:sum([maps:get(K,S) || S <- Summaries])} || K <- Keys]),
		io:format("MATCH TOTAL ~s ~p~n",[Label,Total]),
		{Total,lists:append([Rows || {_,Rows} <- Results])}.

	chunks([],_N)-> [];
	chunks(L,N)->
		{A,B} = lists:split(erlang:min(N,length(L)),L),
		[A|chunks(B,N)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fresh agents %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%fresh_groups/0: settings under which agents are created and mutated, to cover shapes the saved populations lack:
%feedforward networks, price lists scaled by level, the four-value clock, no starting recurrent links, and (last group)
%activation functions other than tanh and linear.
fresh_groups()->
	[{rec_vol,[{connection_architecture,recurrent},{init_recurrent,true},{plt_encoding,vol}]},
	 {rec_level,[{connection_architecture,recurrent},{init_recurrent,true},{plt_encoding,level},{clock_harmonics,1}]},
	 {ff_vol,[{connection_architecture,feedforward},{init_recurrent,false},{plt_encoding,vol}]},
	 {ff_level,[{connection_architecture,feedforward},{init_recurrent,false},{plt_encoding,level},{clock_sensor,false}]},
	 {rec_other_afs,[{connection_architecture,recurrent},{init_recurrent,false},{plt_encoding,vol},{init_hidden_neurons,0},
		{neural_activation_functions,[tanh,linear,sin,cos,gaussian,absolute,sgn,sqrt,log]}]}].

%make_fresh/3: PerGroup agents in each group, each created and then mutated 0 to MaxRounds times (each round is one
%genome_mutator:mutate/1, itself several mutations), with a fixed seed. Returns [{Group,Agent_Id}].
make_fresh(PerGroup,MaxRounds,Seed)->
	lists:append([begin
		eval_protocol:with_config(Settings,fun()->
			rand:seed(exsss,{Seed,erlang:phash2(Group),1}),
			[begin
				Agent_Id = {genotype:generate_UniqueId(),agent},
				{atomic,_} = mnesia:transaction(fun()-> genotype:construct_Agent(flat_eval_fresh,Agent_Id,#constraint{},flat_eval_fresh) end),
				Rounds = rand:uniform(MaxRounds+1)-1,
				[catch genome_mutator:mutate(Agent_Id) || _ <- lists:seq(1,Rounds)],
				{Group,Agent_Id}
			end || _ <- lists:seq(1,PerGroup)]
		end)
	end || {Group,Settings} <- fresh_groups()]).

%make_grown/4: PerGroup agents in each group of fresh_groups/0, each created and then mutated until it has a target
%number of neurons drawn from MinNeurons..MinNeurons+Spread (at most 400 rounds), with a fixed seed. Returns
%[{{grown,Group},Agent_Id}].
make_grown(PerGroup,MinNeurons,Spread,Seed)->
	lists:append([begin
		eval_protocol:with_config(Settings,fun()->
			rand:seed(exsss,{Seed,erlang:phash2(Group),2}),
			[begin
				Agent_Id = {genotype:generate_UniqueId(),agent},
				{atomic,_} = mnesia:transaction(fun()-> genotype:construct_Agent(flat_eval_fresh,Agent_Id,#constraint{},flat_eval_fresh) end),
				Target = MinNeurons + rand:uniform(Spread+1) - 1,
				grow(Agent_Id,Target,400),
				{{grown,Group},Agent_Id}
			end || _ <- lists:seq(1,PerGroup)]
		end)
	end || {Group,Settings} <- fresh_groups()]).

	grow(_Agent_Id,_Target,0)-> ok;
	grow(Agent_Id,Target,Left)->
		case neuron_count(Agent_Id) >= Target of
			true -> ok;
			false -> catch genome_mutator:mutate(Agent_Id), grow(Agent_Id,Target,Left-1)
		end.

	neuron_count(Agent_Id)->
		A = genotype:dirty_read({agent,Agent_Id}),
		length((genotype:dirty_read({cortex,A#agent.cx_id}))#cortex.neuron_ids).

%fresh_match2/6: as fresh_match/4, with PerGroup agents per group mutated 0..MaxRounds times and GrownPerGroup more per
%group grown to MinNeurons..MinNeurons+Spread neurons. Shapes are reported per group with the neurons that reach the
%trade (what flat_eval computes) and the neurons in the genotype.
fresh_match2(PerGroup,MaxRounds,GrownPerGroup,MinNeurons,Spread,Seed,Episodes)->
	Made = make_fresh(PerGroup,MaxRounds,Seed) ++ make_grown(GrownPerGroup,MinNeurons,Spread,Seed),
	Verdicts = [{G,Id,flat_eval:compile(Id)} || {G,Id} <- Made],
	Supported = [Id || {_,Id,{ok,_}} <- Verdicts],
	Refused = lists:foldl(fun({G,_,{unsupported,Why}},M)-> maps:update_with({G,reason_kind(Why)},fun(C)-> C+1 end,1,M); (_,M)-> M end,#{},Verdicts),
	Shapes = [{G,neuron_count(Id),flat_eval:net_info(Net)} || {G,Id,{ok,Net}} <- Verdicts],
	Groups = lists:usort([G || {G,_} <- Made]),
	ShapeSummary = [{G,#{agents=>length(Is),
		genotype_neurons_min=>lists:min([GN || {GN,_} <- Is]),genotype_neurons_max=>lists:max([GN || {GN,_} <- Is]),
		genotype_neurons_mean=>functions:avg([GN || {GN,_} <- Is]),
		computed_neurons_max=>lists:max([maps:get(neurons,I) || {_,I} <- Is]),
		computed_neurons_mean=>functions:avg([maps:get(neurons,I) || {_,I} <- Is]),
		with_recurrent=>length([1 || {_,I} <- Is, maps:get(recurrent_inputs,I) > 0])}}
		|| G <- Groups, Is <- [[{GN,I} || {G2,GN,I} <- Shapes, G2 == G]], Is =/= []],
	AtLeast = fun(K)-> length([1 || {_,GN,_} <- Shapes, GN >= K]) end,
	AtLeastComputed = fun(K)-> length([1 || {_,_,I} <- Shapes, maps:get(neurons,I) >= K]) end,
	io:format("FRESH made ~p, flat_eval accepts ~p, refuses ~p~nFRESH genotype neurons >= ~p: ~p agents; computed neurons >= ~p: ~p agents~n",
		[length(Made),length(Supported),Refused,MinNeurons,AtLeast(MinNeurons),MinNeurons,AtLeastComputed(MinNeurons)]),
	[io:format("FRESH shape ~p ~p~n",[G,Sh]) || {G,Sh} <- ShapeSummary],
	{Total,Rows} = eval_protocol:with_config([{stall_timeout_ms,20000}],fun()-> match_batches(Supported,Episodes,"fresh") end),
	#{made=>length(Made),supported=>length(Supported),refused=>Refused,shapes=>ShapeSummary,match=>Total,
	  genotype_at_least=>AtLeast(MinNeurons),computed_at_least=>AtLeastComputed(MinNeurons),
	  mismatched_agents=>[maps:get(agent,R) || R <- Rows, maps:get(episode_mismatches,R) > 0]}.

%fresh_match/4: the fresh agents flat_eval accepts, through both paths on Episodes; the others are counted by reason
%(and not run on the process path, where a network that waits or fires for ever costs a stall timeout each).
fresh_match(PerGroup,MaxRounds,Seed,Episodes)->
	Made = make_fresh(PerGroup,MaxRounds,Seed),
	Verdicts = [{G,Id,flat_eval:compile(Id)} || {G,Id} <- Made],
	Supported = [Id || {_,Id,{ok,_}} <- Verdicts],
	Refused = lists:foldl(fun({G,_,{unsupported,Why}},M)-> maps:update_with({G,reason_kind(Why)},fun(C)-> C+1 end,1,M); (_,M)-> M end,#{},Verdicts),
	Shapes = [{G,flat_eval:net_info(Net)} || {G,_,{ok,Net}} <- Verdicts],
	ShapeSummary = [{G,#{agents=>length(Is),neurons_max=>lists:max([maps:get(neurons,I) || I <- Is]),
		neurons_mean=>functions:avg([maps:get(neurons,I) || I <- Is]),
		with_recurrent=>length([1 || I <- Is, maps:get(recurrent_inputs,I) > 0])}}
		|| {G,_} <- fresh_groups(), Is <- [[I || {G2,I} <- Shapes, G2 == G]], Is =/= []],
	io:format("FRESH made ~p, flat_eval accepts ~p, refuses ~p~nFRESH shapes ~p~n",[length(Made),length(Supported),Refused,ShapeSummary]),
	{Total,Rows} = eval_protocol:with_config([{stall_timeout_ms,20000}],fun()-> match_batches(Supported,Episodes,"fresh") end),
	#{made=>length(Made),supported=>length(Supported),refused=>Refused,shapes=>ShapeSummary,match=>Total,
	  mismatched_agents=>[maps:get(agent,R) || R <- Rows, maps:get(episode_mismatches,R) > 0]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Speed %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%speed/3: bars per second of both paths on the same agents and episodes, on one scheduler and on all, Repeats times
%each, interleaved. A bar is one trade (warm-up bars included), counted from the flat results, which equal the process
%path's (checked again here). Returns the measurements.
speed(Agent_Ids,Episodes,Repeats)->
	All = erlang:system_info(schedulers),
	Bars = lists:sum([lists:sum([maps:get(bars,E)+config:episode_warmup_bars()+1 || E <- Stats])
		|| {_,{ok,Stats}} <- flat_on(Agent_Ids,Episodes,[])]),
	io:format("SPEED ~p agents, ~p episodes, ~p bars per pass, ~p schedulers~n",[length(Agent_Ids),length(Episodes),Bars,All]),
	Runs = lists:append([[measure(Path,Cores,R,Agent_Ids,Episodes,Bars) || {Path,Cores} <- [{flat,1},{process,1},{flat,All},{process,All}]]
		|| R <- lists:seq(1,Repeats)]),
	erlang:system_flag(schedulers_online,All),
	Table = [begin
		Rates = [Rate || {P,C,_,Rate,_} <- Runs, P == Path, C == Cores],
		{Path,Cores,lists:min(Rates),functions:avg(Rates),lists:max(Rates)}
	end || {Path,Cores} <- [{flat,1},{process,1},{flat,All},{process,All}]],
	[io:format("SPEED_ROW ~p cores=~p bars_per_s min ~.1f mean ~.1f max ~.1f~n",[P,C,Mn,Me,Mx]) || {P,C,Mn,Me,Mx} <- Table],
	#{bars=>Bars,runs=>Runs,table=>Table}.

	measure(Path,Cores,Repeat,Agent_Ids,Episodes,Bars)->
		erlang:system_flag(schedulers_online,Cores),
		erlang:garbage_collect(),
		T0 = erlang:monotonic_time(microsecond),
		Results = case Path of
			flat -> flat_eval:run_many(Agent_Ids,Episodes,#{mode=>validation,parallel=>Cores});
			process -> eval_protocol:run_agents_on(Agent_Ids,Episodes,[],Cores)
		end,
		Seconds = (erlang:monotonic_time(microsecond)-T0)/1.0e6,
		Failed = length([1 || {_,R} <- Results, element(1,R) =/= ok]),
		Rate = Bars/Seconds,
		io:format("SPEED_RUN ~p cores=~p repeat=~p ~.2f s ~.1f bars/s failed=~p~n",[Path,Cores,Repeat,Seconds,Rate,Failed]),
		{Path,Cores,Seconds,Rate,Failed}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Profile %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%profile/2: where the flat path spends its time, on one core. (1) eprof over Agent_Ids on Episodes (counts every call,
%so small functions look dearer than they are); (2) a split without a profiler: the same bars timed with only the sensor
%answers, only the network, and only the simulator steps.
profile(Agent_Ids,Episodes)->
	erlang:system_flag(schedulers_online,1),
	Run = fun()-> flat_on(Agent_Ids,Episodes,[]) end,
	eprof:start(),
	eprof:profile([],Run),
	eprof:analyze(total,[{sort,time}]),
	eprof:stop(),
	split(Agent_Ids,Episodes),
	erlang:system_flag(schedulers_online,erlang:system_info(schedulers)).

	%split/2: whole bars, then each part alone over the same sequence of bars and the same networks
	split(Agent_Ids,Episodes)->
		Table = config:primary_currency_pair(),
		Nets = [Net || Id <- Agent_Ids, {ok,Net} <- [flat_eval:compile(Id)]],
		Keys = lists:append([episode_keys(Table,E) || E <- Episodes]),
		Series = fx_series:ensure(Table),
		A = fx:create_account(),
		N = length(Keys)*length(Nets),
		T = fun(F)-> T0 = erlang:monotonic_time(microsecond), F(), (erlang:monotonic_time(microsecond)-T0)/N end,
		Whole = T(fun()-> flat_on(Agent_Ids,Episodes,[]) end),
		Sense = T(fun()-> [[flat_eval:sense_bar(Net,Table,Series,K,A) || K <- Keys] || Net <- Nets] end),
		Inputs = [{Net,[flat_eval:sense_bar(Net,Table,Series,K,A) || K <- lists:sublist(Keys,1000)]} || Net <- Nets],
		Think = T(fun()-> [[flat_eval:think_bar(Net,I) || _ <- lists:seq(1,length(Keys) div 1000), I <- Is] || {Net,Is} <- Inputs] end),
		Sim = T(fun()-> [run_signals(Table,Episodes) || _ <- Nets] end),
		io:format("PROFILE_SPLIT microseconds per bar: whole ~.2f  sensors ~.2f  network ~.2f  simulator ~.2f  (bars ~p)~n",[Whole,Sense,Think,Sim,N]),
		#{whole=>Whole,sensors=>Sense,network=>Think,simulator=>Sim}.

	episode_keys(Table,{Start,Bars})->
		lists:reverse(element(2,lists:foldl(fun(_,{K,Acc})-> {ets:next(Table,K),[K|Acc]} end,{Start,[]},lists:seq(1,Bars)))).

	%run_signals/2: the simulator alone, with a fixed alternating signal
	run_signals(Table,Episodes)->
		[fx:run_policy(fun(Step,_Close,_Pos)-> case (Step div 50) rem 3 of 0 -> 1; 1 -> 0; _ -> -1 end end,[E],validation) || E <- Episodes, Table =/= x].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Refusals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%refusal_check/2: one hand-built agent for each network shape flat_eval refuses, run through the process path
%(eval_protocol:run_agents_on/4, one agent, stall timeout StallMs) and through flat_eval. Every agent is made from a
%freshly created one: its sensors and actuator are kept, its neurons replaced by the network of the case, written
%consistently at both ends of every link (build_case/2), and then, for the link cases, one end changed. Reports per case
%what flat_eval says, what the process path does (completed with statistics, stalled, ran away, crashed, or still
%running at the wall-clock limit), how long it took, and whether the genotype was changed by the run.
refusal_check(Episodes,StallMs)->
	%the packed series are built on first use, which can take longer than a short stall timeout: build them first
	fx_series:ensure(config:primary_currency_pair()),
	[refusal_case(Name,Episodes,StallMs) || Name <- [Name || {Name,_,_} <- refusal_cases()]].

refusal_case(Name,Episodes,StallMs)->
	{Name,Spec,Edit} = lists:keyfind(Name,1,refusal_cases()),
	rand:seed(exsss,{7,erlang:phash2(Name),7}),
	Agent_Id = build_case(Spec,Edit),
	Before = genotype_terms(Agent_Id),
	Flat = flat_eval:compile(Agent_Id),
	Parent = self(),
	T0 = erlang:monotonic_time(millisecond),
	Runner = spawn(fun()->
		R = eval_protocol:with_config([{stall_timeout_ms,StallMs}],fun()-> eval_protocol:run_agents_on([Agent_Id],Episodes,[],1) end),
		Parent ! {refusal_done,self(),R}
	end),
	Process = receive
		{refusal_done,Runner,[{_,R}]} -> R
	after 10*StallMs+60000 ->
		exit(Runner,kill),
		still_running
	end,
	Ms = erlang:monotonic_time(millisecond)-T0,
	After = genotype_terms(Agent_Id),
	Outcome = case Process of
		{ok,Stats} -> {completed,#{episodes=>length(Stats),bars=>lists:sum([maps:get(bars,E) || E <- Stats]),
			pnl=>lists:sum([maps:get(pnl,E) || E <- Stats]),entries=>lists:sum([maps:get(entries,E) || E <- Stats])}};
		Other -> Other
	end,
	%When the process path completes, the same network through flat_eval, the output-lead check skipped (to show what
	%the process path does with an output neuron that runs ahead): positions and money compared bar by bar.
	Compared = case Process of
		{ok,PStats} ->
			case flat_eval:compile(Agent_Id,#{allow_output_ahead=>true}) of
				{ok,Net} ->
					{ok,FStats} = in_eval_config(Episodes,[],fun()-> flat_eval:run_net(Net,Episodes,#{mode=>validation}) end),
					Row0 = compare_agent(Agent_Id,{ok,PStats},{ok,FStats}),
					#{episode_mismatches=>maps:get(episode_mismatches,Row0),bar_mismatches=>maps:get(bar_mismatches,Row0),
					  mismatched_episodes=>[I || {I,{P,F}} <- lists:zip(lists:seq(1,length(PStats)),lists:zip(PStats,FStats,trim)), P =/= F],
					  first_mismatch=>maps:get(first_mismatch,Row0)};
				Refused ->
					{flat_refuses_even_relaxed,Refused}
			end;
		_ ->
			not_run
	end,
	Row = #{case_name=>Name,agent=>Agent_Id,flat=>case Flat of {ok,_} -> ok; U -> U end,process=>Outcome,ms=>Ms,
		genotype_changed=>Before =/= After,flat_vs_process=>Compared},
	io:format("REFUSAL ~p~n",[Row]),
	Row#{process_stats=>Process}.

	genotype_terms(Agent_Id)->
		A = genotype:dirty_read({agent,Agent_Id}),
		Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
		{Cx,[genotype:dirty_read({neuron,N}) || N <- Cx#cortex.neuron_ids],[genotype:dirty_read({sensor,S}) || S <- Cx#cortex.sensor_ids],
		 [genotype:dirty_read({actuator,Ac}) || Ac <- Cx#cortex.actuator_ids]}.

%refusal_cases/0: {Name,Network,Edit}. Network: [{Neuron,Inputs}] with Inputs [{Source,delay|now}] (a source is s1, the
%agent's first sensor, or another neuron; delay marks a recurrent link) and every neuron given a bias, last; the neuron
%named out drives the trade. Edit: none, or a change made after the consistent network is written.
refusal_cases()->
	[{good,[{h1,[{s1,now}]},{out,[{h1,now},{out,delay}]}],none},
	 {cycle_without_recurrent_link,[{h1,[{s1,now},{h2,now}]},{h2,[{h1,now}]},{out,[{h1,now}]}],none},
	 {sensorless_loop_feeds_output,[{h1,[{s1,now}]},{h3,[{h3,delay}]},{out,[{h1,now},{h3,now}]}],none},
	 {sensorless_loop_isolated,[{h1,[{s1,now}]},{h3,[{h3,delay}]},{out,[{h1,now}]}],none},
	 {no_input_neuron_feeds_output,[{h1,[{s1,now}]},{h3,[]},{out,[{h1,now},{h3,now}]}],none},
	 {no_input_neuron_isolated,[{h1,[{s1,now}]},{h3,[]},{out,[{h1,now}]}],none},
	 {output_neuron_ahead,[{h1,[{s1,now}]},{out,[{h1,delay}]}],none},
	 {output_neuron_ahead_two,[{h1,[{s1,now}]},{h2,[{h1,delay}]},{out,[{h2,delay}]}],none},
	 {fanout_not_an_input,[{h1,[{s1,now}]},{h2,[{h1,now}]},{out,[{h1,now},{h2,now}]}],{add_fanout,s1,h2}},
	 {input_not_in_fanout,[{h1,[{s1,now}]},{h2,[{s1,now}]},{out,[{h1,now},{h2,now}]}],{drop_fanout,s1,h2}},
	 {output_not_an_input,[{h1,[{s1,now}]},{h2,[{s1,now}]},{out,[{h1,now},{h2,now}]}],{add_output,h1,h2}},
	 {input_not_an_output,[{h1,[{s1,now}]},{h2,[{h1,now}]},{out,[{h1,now},{h2,now}]}],{drop_output,h1,h2}},
	 {ro_not_an_output,[{h1,[{s1,now}]},{h2,[{s1,now}]},{out,[{h1,now},{h2,now}]}],{add_ro,h1,h2}},
	 {bias_not_last,[{h1,[{s1,now}]},{out,[{h1,now}]}],{bias_first,out}},
	 {weight_list_short,[{h1,[{s1,now}]},{out,[{h1,now}]}],{weights,h1,s1,-1}},
	 {weight_list_long,[{h1,[{s1,now}]},{out,[{h1,now}]}],{weights,h1,s1,1}},
	 {duplicate_input,[{h1,[{s1,now}]},{out,[{h1,now}]}],{duplicate_input,out,h1}},
	 {duplicate_output,[{h1,[{s1,now}]},{out,[{h1,now}]}],{duplicate_output,h1,out}},
	 {all_sensors_unconnected,[{h1,[{h1,delay}]},{out,[{h1,now}]}],none}].

%build_case/2: a fresh agent (the settings in force) with its neurons replaced by Spec, then Edit applied.
build_case(Spec,Edit)->
	Agent_Id = {genotype:generate_UniqueId(),agent},
	{atomic,_} = mnesia:transaction(fun()-> genotype:construct_Agent(flat_eval_refusal,Agent_Id,#constraint{},flat_eval_refusal) end),
	A = genotype:dirty_read({agent,Agent_Id}),
	Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
	Old = [genotype:dirty_read({neuron,N}) || N <- Cx#cortex.neuron_ids],
	Template = hd(Old),
	[mnesia:dirty_delete({neuron,N}) || N <- Cx#cortex.neuron_ids],
	[S1|OtherSensors] = [genotype:dirty_read({sensor,S}) || S <- Cx#cortex.sensor_ids],
	[Act] = [genotype:dirty_read({actuator,Ac}) || Ac <- Cx#cortex.actuator_ids],
	Ids = maps:from_list([{Name,{{0.5,genotype:generate_UniqueId()},neuron}} || {Name,_} <- Spec]),
	Id = fun(s1)-> S1#sensor.id; (Name)-> maps:get(Name,Ids) end,
	Width = fun(s1)-> S1#sensor.vl; (_)-> 1 end,
	Weights = fun(K)-> [{rand:uniform()-0.5,[]} || _ <- lists:seq(1,K)] end,
	Neurons = [begin
		Outputs = [Id(T) || {T,TIns} <- Spec, lists:keymember(Name,1,TIns)] ++ [Act#actuator.id || Name == out],
		Ro = [Id(T) || {T,TIns} <- Spec, {Src,delay} <- TIns, Src == Name],
		Template#neuron{id=Id(Name),af=tanh,aggr_f=dot_product,input_idps_modulation=[],
			input_idps=[{Id(Src),Weights(Width(Src))} || {Src,_} <- Ins] ++ [{bias,Weights(1)}],
			output_ids=Outputs,ro_ids=Ro}
	end || {Name,Ins} <- Spec],
	[mnesia:dirty_write(N) || N <- Neurons],
	mnesia:dirty_write(S1#sensor{fanout_ids=[Id(Name) || {Name,Ins} <- Spec, lists:keymember(s1,1,Ins)]}),
	[mnesia:dirty_write(S#sensor{fanout_ids=[]}) || S <- OtherSensors],
	mnesia:dirty_write(Act#actuator{fanin_ids=[Id(out)]}),
	mnesia:dirty_write(Cx#cortex{neuron_ids=[Id(Name) || {Name,_} <- Spec]}),
	edit(Edit,Id,Width),
	Agent_Id.

	edit(none,_Id,_Width)-> ok;
	edit({add_fanout,s1,T},Id,_)-> update(sensor,Id(s1),fun(S)-> S#sensor{fanout_ids=S#sensor.fanout_ids++[Id(T)]} end);
	edit({drop_fanout,s1,T},Id,_)-> update(sensor,Id(s1),fun(S)-> S#sensor{fanout_ids=S#sensor.fanout_ids--[Id(T)]} end);
	edit({add_output,X,T},Id,_)-> update(neuron,Id(X),fun(N)-> N#neuron{output_ids=N#neuron.output_ids++[Id(T)]} end);
	edit({drop_output,X,T},Id,_)-> update(neuron,Id(X),fun(N)-> N#neuron{output_ids=N#neuron.output_ids--[Id(T)]} end);
	edit({add_ro,X,T},Id,_)-> update(neuron,Id(X),fun(N)-> N#neuron{ro_ids=N#neuron.ro_ids++[Id(T)]} end);
	edit({bias_first,X},Id,_)->
		update(neuron,Id(X),fun(N)-> {value,B,Rest} = lists:keytake(bias,1,N#neuron.input_idps), N#neuron{input_idps=[B|Rest]} end);
	edit({weights,X,Src,Delta},Id,Width)->
		update(neuron,Id(X),fun(N)->
			{value,{_,Ws},Rest} = lists:keytake(Id(Src),1,N#neuron.input_idps),
			New = case Delta < 0 of true -> lists:sublist(Ws,Width(Src)+Delta); false -> Ws ++ [{0.1,[]} || _ <- lists:seq(1,Delta)] end,
			N#neuron{input_idps=[{Id(Src),New}|Rest]}
		end);
	edit({duplicate_input,X,Src},Id,_)->
		update(neuron,Id(X),fun(N)-> {value,In,Rest} = lists:keytake(Id(Src),1,N#neuron.input_idps), N#neuron{input_idps=[In,In|Rest]} end);
	edit({duplicate_output,X,T},Id,_)->
		update(neuron,Id(X),fun(N)-> N#neuron{output_ids=N#neuron.output_ids++[Id(T)]} end).

	update(Type,Key,Fun)->
		mnesia:dirty_write(Fun(genotype:dirty_read({Type,Key}))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Speed on 1..10 cores %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%speed2/4: bars per second of the version 1 flat path (share => false), the shared flat path and the process path, on
%each scheduler count of CoresList, Repeats times, interleaved (repeat 1 of everything, then repeat 2, ...). Each run
%also records how busy the online schedulers were (erlang:statistics(scheduler_wall_time)), garbage collections, and the
%machine's load average before it. Results are checked equal to the shared path's on every run.
speed2(Agent_Ids,Episodes,Repeats,CoresList)->
	All = erlang:system_info(schedulers),
	erlang:system_flag(scheduler_wall_time,true),
	Reference = flat_on(Agent_Ids,Episodes,[]),
	Bars = lists:sum([lists:sum([maps:get(bars,E)+config:episode_warmup_bars()+1 || E <- Stats]) || {_,{ok,Stats}} <- Reference]),
	io:format("SPEED2 ~p agents, ~p episodes, ~p bars per pass, ~p schedulers, load ~s~n",[length(Agent_Ids),length(Episodes),Bars,All,loadavg()]),
	Paths = [flat_v1,flat_shared,process],
	Runs = lists:append([[measure2(Path,Cores,R,Agent_Ids,Episodes,Bars,Reference) || Cores <- CoresList, Path <- Paths] || R <- lists:seq(1,Repeats)]),
	erlang:system_flag(schedulers_online,All),
	Table = [begin
		Rs = [Run || Run = #{path:=P,cores:=C} <- Runs, P == Path, C == Cores],
		Rates = [maps:get(rate,Run) || Run <- Rs],
		Busy = functions:avg([maps:get(busy,Run) || Run <- Rs]),
		{Path,Cores,lists:min(Rates),functions:avg(Rates),lists:max(Rates),Busy}
	end || Path <- Paths, Cores <- CoresList],
	[io:format("SPEED2_ROW ~p cores=~p bars_per_s min ~.0f mean ~.0f max ~.0f busy_schedulers ~.2f~n",[P,C,Mn,Me,Mx,B]) || {P,C,Mn,Me,Mx,B} <- Table],
	#{bars=>Bars,runs=>Runs,table=>Table}.

	measure2(Path,Cores,Repeat,Agent_Ids,Episodes,Bars,Reference)->
		erlang:system_flag(schedulers_online,Cores),
		erlang:garbage_collect(),
		Load = loadavg(),
		{GCs0,Words0,_} = erlang:statistics(garbage_collection),
		W0 = lists:sort(erlang:statistics(scheduler_wall_time)),
		T0 = erlang:monotonic_time(microsecond),
		Results = case Path of
			flat_v1 -> flat_on(Agent_Ids,Episodes,[],#{parallel=>Cores,share=>false});
			flat_shared -> flat_on(Agent_Ids,Episodes,[],#{parallel=>Cores});
			process -> eval_protocol:run_agents_on(Agent_Ids,Episodes,[],Cores)
		end,
		Seconds = (erlang:monotonic_time(microsecond)-T0)/1.0e6,
		W1 = lists:sort(erlang:statistics(scheduler_wall_time)),
		{GCs1,Words1,_} = erlang:statistics(garbage_collection),
		%busy: the sum over normal schedulers of active time / wall time, i.e. how many cores' worth of work the VM did
		Busy = lists:sum([(A1-A0)/max(1,T1-T0b) || {{I,A0,T0b},{I,A1,T1}} <- lists:zip(W0,W1), I =< erlang:system_info(schedulers)]),
		Same = Results =:= Reference,
		Rate = Bars/Seconds,
		io:format("SPEED2_RUN ~p cores=~p repeat=~p ~.2f s ~.0f bars/s busy=~.2f gcs=~p gc_mwords=~.0f same_as_reference=~p load_before=~s~n",
			[Path,Cores,Repeat,Seconds,Rate,Busy,GCs1-GCs0,(Words1-Words0)/1.0e6,Same,Load]),
		#{path=>Path,cores=>Cores,repeat=>Repeat,seconds=>Seconds,rate=>Rate,busy=>Busy,gcs=>GCs1-GCs0,same=>Same,load=>Load}.

	loadavg()->
		string:trim(os:cmd("sysctl -n vm.loadavg")).

%ceiling/3: what the machine gives for work that shares nothing. On each scheduler count, that many processes each run
%Kernel the same fixed number of times; reports units per second and the gain over one scheduler.
%	float   a loop of floating-point arithmetic and math:tanh, allocating only boxed floats
%	network flat_eval:think_bar/2 of one saved agent on fixed inputs: the flat path's own inner work, with its allocation
ceiling(Agent_Id,CoresList,Repeats)->
	All = erlang:system_info(schedulers),
	{ok,Net} = flat_eval:compile(Agent_Id),
	Table = config:primary_currency_pair(),
	Series = fx_series:ensure(Table),
	Key = element(1,hd(train_episodes(1,5))),
	Inputs = flat_eval:sense_bar(Net,Table,Series,Key,fx:create_account()),
	Kernels = [{float,fun()-> float_loop(200000,0.1) end},{network,fun()-> [flat_eval:think_bar(Net,Inputs) || _ <- lists:seq(1,2000)], ok end}],
	Rows = [begin
		erlang:system_flag(schedulers_online,Cores),
		Rate = run_kernel(Fun,Cores,20),
		io:format("CEILING ~p cores=~p repeat=~p ~.1f kernels/s load_before=~s~n",[Name,Cores,R,Rate,loadavg()]),
		{Name,Cores,Rate}
	end || R <- lists:seq(1,Repeats), Cores <- CoresList, {Name,Fun} <- Kernels],
	erlang:system_flag(schedulers_online,All),
	[begin
		Rates = [Rt || {N,C,Rt} <- Rows, N == Name, C == Cores],
		Base = functions:avg([Rt || {N,1,Rt} <- Rows, N == Name]),
		io:format("CEILING_ROW ~p cores=~p mean ~.1f min ~.1f max ~.1f gain_over_1 ~.2f~n",[Name,Cores,functions:avg(Rates),lists:min(Rates),lists:max(Rates),functions:avg(Rates)/Base])
	end || {Name,_} <- Kernels, Cores <- CoresList],
	Rows.

	run_kernel(Fun,Cores,PerProcess)->
		Parent = self(),
		T0 = erlang:monotonic_time(microsecond),
		Pids = [spawn(fun()-> [Fun() || _ <- lists:seq(1,PerProcess)], Parent ! {kernel_done,self()} end) || _ <- lists:seq(1,Cores)],
		[receive {kernel_done,P} -> ok end || P <- Pids],
		Cores*PerProcess/((erlang:monotonic_time(microsecond)-T0)/1.0e6).

	float_loop(0,X)-> X;
	float_loop(N,X)-> float_loop(N-1,math:tanh(X*1.0000001+0.3)-0.2).

%profile_shared/2: where the shared path spends its time, on one core: the whole run, the columns alone (once per
%episode), the network alone and the simulator alone, per agent-bar; then eprof over the whole run.
profile_shared(Agent_Ids,Episodes)->
	erlang:system_flag(schedulers_online,1),
	Table = config:primary_currency_pair(),
	Nets = [Net || Id <- Agent_Ids, {ok,Net} <- [flat_eval:compile(Id)]],
	Keys = lists:append([episode_keys(Table,E) || E <- Episodes]),
	Series = fx_series:ensure(Table),
	N = length(Keys)*length(Nets),
	T = fun(F)-> T0 = erlang:monotonic_time(microsecond), F(), (erlang:monotonic_time(microsecond)-T0)/N end,
	Whole = T(fun()-> flat_on(Agent_Ids,Episodes,[]) end),
	Columns = T(fun()-> [flat_eval:column_bytes(Agent_Ids,E) || E <- Episodes] end),
	Inputs = [{Net,[flat_eval:sense_bar(Net,Table,Series,K,fx:create_account()) || K <- lists:sublist(Keys,1000)]} || Net <- Nets],
	Think = T(fun()-> [[flat_eval:think_bar(Net,I) || _ <- lists:seq(1,length(Keys) div 1000), I <- Is] || {Net,Is} <- Inputs] end),
	Sim = T(fun()-> [run_signals(Table,Episodes) || _ <- Nets] end),
	Mem = flat_eval:column_bytes(Agent_Ids,hd(Episodes)),
	io:format("PROFILE_SHARED microseconds per agent-bar: whole ~.3f  columns ~.3f (includes compiling, once per episode)  network ~.3f  simulator ~.3f  (agent-bars ~p)~n",[Whole,Columns,Think,Sim,N]),
	io:format("PROFILE_SHARED column memory for one episode: ~p~n",[Mem]),
	eprof:start(),
	eprof:profile([],fun()-> flat_on(Agent_Ids,Episodes,[]) end),
	eprof:analyze(total,[{sort,time}]),
	eprof:stop(),
	erlang:system_flag(schedulers_online,erlang:system_info(schedulers)),
	#{whole=>Whole,columns=>Columns,network=>Think,simulator=>Sim,memory=>Mem}.
