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
	Flat = flat_on(Agent_Ids,Episodes,Overrides),
	T2 = erlang:monotonic_time(millisecond),
	Rows = [compare_agent(Id,P,F) || {{Id,P},{Id,F}} <- lists:zip(Process,Flat)],
	Sum = fun(K)-> lists:sum([maps:get(K,R) || R <- Rows]) end,
	Summary = #{label=>Label,agents=>length(Rows),
		compared=>length([1 || #{status:=compared} <- Rows]),
		unsupported=>length([1 || #{status:={unsupported,_}} <- Rows]),
		process_errors=>length([1 || #{status:={process_error,_}} <- Rows]),
		episodes=>Sum(episodes),bars=>Sum(bars),
		episode_mismatches=>Sum(episode_mismatches),bar_mismatches=>Sum(bar_mismatches),
		process_s=>(T1-T0)/1000,flat_s=>(T2-T1)/1000},
	io:format("MATCH ~s ~p~n",[Label,Summary]),
	[io:format("  MISMATCH ~p ~p~n",[maps:get(agent,R),maps:get(first_mismatch,R)]) || R <- Rows, maps:get(episode_mismatches,R) > 0],
	[io:format("  NOT COMPARED ~p ~p~n",[maps:get(agent,R),maps:get(status,R)]) || R <- Rows, maps:get(status,R) =/= compared],
	{Summary,Rows}.

	%flat_on/3: the same agents through flat_eval with the settings run_agents_on/4 puts in force for its own run.
	flat_on(Agent_Ids,Episodes,Overrides)->
		Costs = [{validation_spread,proplists:get_value(validation_spread,Overrides,undefined)}],
		eval_protocol:with_config(Costs++proplists:delete(validation_spread,Overrides),fun()->
			Table = config:primary_currency_pair(),
			ValParams = {Table,config:val_span(),config:episode_bars(),config:episodes_val(),ets:info(Table,size)},
			DriftParams = {Table,config:val_span(),ets:info(Table,size),config:episode_bars(),config:episodes_val(),config:episode_warmup_bars(),config:episode_anchor_step()},
			Mu = episodes:set_drift(Table,Episodes),
			eval_protocol:with_config([{validation_series,true},{episodes_validation_set,Episodes},{episodes_validation_params,ValParams},
				{{episodes_drift,validation},{DriftParams,Mu}}],fun()->
				flat_eval:run_many(Agent_Ids,Episodes,#{mode=>validation})
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
		Keys = [agents,compared,unsupported,process_errors,episodes,bars,episode_mismatches,bar_mismatches,process_s,flat_s],
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
