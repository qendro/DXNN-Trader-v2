%% A single-process evaluator for saved agents.
%%
%% The ordinary path builds an agent as a dozen processes (cortex, one per sensor, neuron and actuator, and a simulator)
%% that pass one message per connection per bar. This module reads the same genotype and runs the same network inside
%% the calling process, bar by bar, against the same simulator rules (fx:ep_step/4, called directly) and the same sensor
%% answers (fx_series), and returns the same per-episode statistics as the process path: the list of maps that the
%% simulator publishes (fx:ep_stat/4), in the order run, with u_series (the position at every scored bar) when the
%% settings keep it. Nothing in the population loop calls it yet.
%%
%%   run(Agent_Id,Episodes,Opts)      -> {ok,EpStats} | {unsupported,Why}
%%   run_many(Agent_Ids,Episodes,Opts)-> [{Agent_Id,{ok,EpStats}|{unsupported,Why}}], in the order given
%%   compile(Agent_Id)                -> {ok,Net} | {unsupported,Why}
%%   run_net(Net,Episodes,Opts)       -> {ok,EpStats}
%%
%%   Episodes  a list of {StartKey,Bars}, or validation (episodes:ensure_validation_set/0), or tune | score (the set the
%%             current generation published, episodes:current_set/1)
%%   Opts      #{mode => validation | score | tune}   which evaluation this stands for, as the exoself's eval_phase: it
%%                                                   decides the span (validation, or train for tune and score), so the
%%                                                   drift and cost, and whether positions are kept (fx:ep_acc_new/2).
%%                                                   Default validation, which is what eval_protocol:run_agents_on/4 and
%%                                                   resim use.
%%               parallel => N                       run_many/3 only: N agents at a time (default 1)
%%
%% How the process network is reproduced, and why the result is the same and not merely close:
%%   - Every neuron waits for one message from each of its inputs, in the order of its input list (a selective receive
%%     per input), and messages from one sender arrive in the order sent. So the k-th firing of a neuron uses the k-th
%%     message of each input, whatever the scheduling: the network is a deterministic dataflow graph.
%%   - A link X -> T is recurrent when T is in X's ro_ids: X sends T a zero (the integer 0, neuron's ?RO_SIGNAL) when it
%%     starts and after every reset, so T's k-th message from X is X's output of bar k-1, and 0 at an episode's first bar.
%%     Every other link carries the output of the same bar. Neurons are therefore computed in an order where every
%%     non-recurrent input comes first, recurrent inputs read the previous bar.
%%   - The weighted sum is computed by the same expressions as signal_aggregator:dot_product/2, in the same order: per
%%     input, I*W+Acc from the integer 0 over the input vector, then Dot+Acc over the inputs in the neuron's input order,
%%     then +Bias; then neuron:sat/2 at 2*pi and the activation function. Floating-point results are therefore identical.
%%   - The trade is functions:trinary/1 of the output neuron, filled by fx:ep_step/4 with the accumulator of
%%     fx:ep_acc_new/2 and the account of fx:create_account/0, exactly as fx:episode_trade/6. Each episode starts from a
%%     fresh network (config:episode_isolation/0) and every sensor reads the bar being decided (sense_before_trade).
%%   - An extinct episode ends the evaluation, as in fx:episode_trade/6.
%%
%% Supported (version 1): neural encoding; neurons with any activation function of functions.erl (tanh and linear are
%% computed inline), dot_product, no plasticity, no modulation inputs; feedforward and recurrent links; sensors fx_PLT
%% ([TF,N] and [TF,N,vol]), fx_IND, fx_Clock and fx_Position; one fx_Trade actuator fed by one neuron; episode mode with
%% isolation and ordered sensing on. Anything else is {unsupported,Why}, so a caller can use the process path instead.
%% That includes networks the process path cannot run as written: a circle of non-recurrent links (it waits for ever), a
%% neuron that no sensor reaches (it fires for ever), links whose two ends disagree, a bias that is not the last input,
%% weight lists of the wrong length, and an agent whose sensors are all unconnected (the process path mutates it first).
-module(flat_eval).
-export([run/3, run_many/3, compile/1, compile/2, run_net/3, check_config/0, net_info/1, sense_bar/5, think_bar/2, column_bytes/2]).
-include("records.hrl").

%The simulator state of fx.erl (a record private to that module), copied so that a fresh one can be made here. The
%field order must stay the same as in fx.erl; check_state_record/0 compares the size.
-record(state,{table_name,feature,index_start,index_end,index,price_list=[],cycle=0,realized_pl_by_cycle=[]}).

-define(SAT_LIMIT,math:pi()*2).%neuron.erl
-define(RO_SIGNAL,0).%neuron.erl: the first message on a recurrent link, an integer

%The compiled network.
%	sensors   tuple of sensor specs, each {series,Spec} | {clock,Spec} | position, one per distinct spec
%	neurons   list of {AF,Inputs,Bias} in evaluation order; Inputs [{Src,Weights}] in the neuron's own input order,
%	          Src = {s,I} (sensor I) | {n,J} (neuron J's output this bar) | {p,J} (neuron J's output the bar before);
%	          Bias = none | Number
%	size      number of neurons
%	out       index of the neuron that drives the trade
%	stray     the most messages per bar that one neuron receives on links it does not read (stray_links/4)
-record(net,{agent_id,sensors,neurons,size,out,uses_series,stray=0}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Running %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(Agent_Id,Episodes,Opts)->
	case check_config() of
		ok ->
			case compile(Agent_Id) of
				{ok,Net} -> run_net(Net,Episodes,Opts);
				Unsupported -> Unsupported
			end;
		Unsupported ->
			Unsupported
	end.

%run_many/3: what run/3 returns, for each agent, in the order of Agent_Ids.
%	share => true (default)  the sensor answers of a bar are computed once per episode and read by every agent run on
%	                         that episode (shared/3); only the position sensor, which depends on the agent's own
%	                         account, is answered per agent.
%	share => false           version 1: each agent alone, every sensor answered per agent and per bar.
run_many(Agent_Ids,Episodes,Opts)->
	Eps = resolve(Episodes),
	case maps:get(share,Opts,true) of
		true ->
			shared(Agent_Ids,Eps,Opts);
		false ->
			case maps:get(parallel,Opts,1) of
				N when is_integer(N), N > 1 ->
					Indexed = lists:zip(lists:seq(1,length(Agent_Ids)),Agent_Ids),
					Results = pool([{I,Id} || {I,Id} <- Indexed],fun({I,Id})-> [{I,{Id,safe_run(Id,Eps,Opts)}}] end,N),
					[maps:get(I,Results) || {I,_} <- Indexed];
				_ ->
					[{Id,safe_run(Id,Eps,Opts)} || Id <- Agent_Ids]
			end
	end.

	%pool/3: N worker processes take items off a shared queue and apply Fun, which returns [{Key,Value}]; the result
	%is the map of every Key to its Value.
	pool(Items,Fun,N)->
		Parent = self(),
		Queue = spawn_link(fun()-> queue_loop(Items) end),
		Workers = [spawn_link(fun()-> worker(Parent,Queue,Fun) end) || _ <- lists:seq(1,erlang:min(N,max(1,length(Items))))],
		Results = collect(length(Items),#{}),
		[unlink(W) || W <- Workers],
		unlink(Queue),
		Results.

	queue_loop([])->
		receive {take,From} -> From ! none, queue_loop([]) after 60000 -> ok end;
	queue_loop([Item|Rest])->
		receive {take,From} -> From ! {item,Item}, queue_loop(Rest) end.

	worker(Parent,Queue,Fun)->
		Queue ! {take,self()},
		receive
			{item,Item} ->
				Parent ! {flat_result,Fun(Item)},
				worker(Parent,Queue,Fun);
			none ->
				ok
		end.

	collect(0,Acc)-> Acc;
	collect(K,Acc)->
		receive {flat_result,KVs} -> collect(K-1,maps:merge(Acc,maps:from_list(KVs))) end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Shared sensor answers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%shared/3: every agent on every episode, with each episode's sensor answers computed once.
%The work is cut into units of one episode and one group of agents. A unit walks the episode's keys once, answers every
%distinct price-list, indicator and clock spec its agents use at every bar (a column per spec: a tuple with one answer
%per bar), and then runs its agents on the episode one after the other, reading those columns instead of asking
%fx_series. The answers are the same terms fx_series gives (the same function on the same row), so nothing changes but
%how often they are computed. The bar a column entry belongs to is checked against the simulator's own index at every
%bar; a bar the walk did not foresee is answered directly (and counted in the process dictionary, flat_eval_fallbacks).
%Episodes are independent (fresh network, account, accumulator and prev_PC each), so units can run in any order or in
%parallel; each agent's statistics are then put back in episode order and cut after its first extinct episode, where
%run/3 stops. With parallel => N, units go to N workers; agents are split into as many groups as it takes to give the
%workers about eight units each (so that the last units to finish leave little idle), so a group shares each episode's answers among length(Agent_Ids)/Groups agents.
%Memory: one unit holds its episode's columns, about 24 bytes per number (a list cell and a float) times the bars of
%the episode times the summed width of the distinct specs (column_bytes/1 measures it).
shared(Agent_Ids,Episodes,Opts)->
	case check_config() of
		ok ->
			Indexed = lists:zip(lists:seq(1,length(Agent_Ids)),Agent_Ids),
			Compiled = [{I,Id,case compile(Id) of
				{ok,Net} -> case stray_check(Net,Episodes) of ok -> {ok,Net}; U -> U end;
				U -> U
			 end} || {I,Id} <- Indexed],
			Nets = [{I,Net} || {I,_,{ok,Net}} <- Compiled],
			N = case maps:get(parallel,Opts,1) of P when is_integer(P), P > 1 -> P; _ -> 1 end,
			EpIndexed = lists:zip(lists:seq(1,length(Episodes)),Episodes),
			Groups = case N of
				1 -> 1;
				_ -> erlang:max(1,erlang:min(length(Nets),ceil_div(8*N,erlang:max(1,length(Episodes)))))
			end,
			Units = [{EI,Ep,Group} || {EI,Ep} <- EpIndexed, Group <- deal(Nets,Groups), Group =/= []],
			Mode = maps:get(mode,Opts,validation),
			Fun = fun(Unit)-> run_unit(Unit,Mode) end,
			Results = case N of
				1 -> maps:from_list(lists:append([Fun(U) || U <- Units]));
				_ -> pool(Units,Fun,N)
			end,
			[{Id,case C of
				{ok,_} -> assemble([maps:get({I,EI},Results) || {EI,_} <- EpIndexed],[]);
				Unsupported -> Unsupported
			 end} || {I,Id,C} <- Compiled];
		Unsupported ->
			[{Id,Unsupported} || Id <- Agent_Ids]
	end.

	ceil_div(A,B)-> (A+B-1) div B.

	%deal/2: Items dealt into G groups in turn
	deal(Items,G)->
		Tagged = lists:zip(lists:seq(0,length(Items)-1),Items),
		[[Item || {K,Item} <- Tagged, K rem G == R] || R <- lists:seq(0,G-1)].

	%assemble/2: one agent's per-episode results in episode order -> what run/3 returns
	assemble([],Done)-> {ok,lists:reverse(Done)};
	assemble([{episode_over,EpStat}|Rest],Done)-> assemble(Rest,[EpStat|Done]);
	assemble([{extinct,EpStat}|_],Done)-> {ok,lists:reverse([EpStat|Done])};
	assemble([{error,Why}|_],_Done)-> {error,Why}.

	%run_unit/2: one episode, one group of agents -> [{{AgentIndex,EpisodeIndex},Result}]
	run_unit({EI,Episode,Group},Mode)->
		Span = case Mode of validation -> validation; _ -> train end,
		Table = config:primary_currency_pair(),
		Specs = lists:usort([Spec || {_,Net} <- Group, Spec <- tuple_to_list(Net#net.sensors), Spec =/= position]),
		Series = case lists:any(fun({series,_})-> true; (_)-> false end,Specs) of
			true -> fx_series:ensure(Table);
			false -> undefined
		end,
		{Keys,Columns} = columns(Table,Series,Episode,Specs),
		Col = maps:from_list(lists:zip(Specs,tuple_to_list(Columns))),
		[{{I,EI},try
				Cols = list_to_tuple([case Spec of position -> position; _ -> maps:get(Spec,Col) end || Spec <- tuple_to_list(Net#net.sensors)]),
				shared_episode(Net,Cols,Keys,Table,Series,Episode,Mode,Span)
			catch Class:Reason:Stack -> {error,{Class,Reason,lists:sublist(Stack,3)}}
			end} || {I,Net} <- Group].

	%columns/4: the keys of the episode's bars, in the order the simulator visits them (fx:update_state/1: from the
	%start key by fx:next/2 until index_end), and for each spec a tuple of its answer at each of those bars.
	columns(Table,Series,Episode,Specs)->
		S = fx:init_episode_state(#state{},Table,close,Episode),
		Keys = episode_keys(Table,S#state.index,S#state.index_end,[]),
		Ps = case Series of undefined -> []; _ -> [fx_series:position(Table,K) || K <- Keys] end,
		Columns = [list_to_tuple(case Spec of
			{series,SSpec} -> [fx_series:answer_at(Series,P,SSpec) || P <- Ps];
			{clock,CSpec} -> [fx_series:answer(Table,K,CSpec) || K <- Keys]
		 end) || Spec <- Specs],
		{list_to_tuple(Keys),list_to_tuple(Columns)}.

		episode_keys(_Table,'$end_of_table',_End,Acc)-> lists:reverse(Acc);
		episode_keys(_Table,End,End,Acc)-> lists:reverse(Acc);
		episode_keys(Table,Key,End,Acc)-> episode_keys(Table,fx:next(Table,Key),End,[Key|Acc]).

	%shared_episode/8: as one step of episodes/7, reading the columns
	shared_episode(Net,Cols,Keys,Table,Series,Episode,Mode,Span)->
		put(prev_PC,0),
		S = fx:init_episode_state(#state{},Table,close,Episode),
		A = fx:create_account(),
		Acc = fx:ep_acc_new(Mode,Span),
		Fresh = erlang:make_tuple(Net#net.size,?RO_SIGNAL),
		shared_bars(Net,Cols,Keys,1,Table,Series,S,A,Acc,Fresh).

	shared_bars(Net,Cols,Keys,K,Table,Series,S,A,Acc,Prev)->
		Key = S#state.index,
		Inputs = case K =< tuple_size(Keys) andalso element(K,Keys) =:= Key of
			true ->
				read_columns(Cols,K,A);
			false ->
				put(flat_eval_fallbacks,case get(flat_eval_fallbacks) of undefined -> 1; F -> F+1 end),
				sense(Net#net.sensors,Table,case Series of undefined -> fx_series:ensure(Table); _ -> Series end,Key,A)
		end,
		Outputs = think(Net#net.neurons,Inputs,Prev,Prev,1),
		Signal = functions:trinary(element(Net#net.out,Outputs)),
		case fx:ep_step(S,A,Acc,Signal) of
			{continue,U_S,U_A,U_Acc} -> shared_bars(Net,Cols,Keys,K+1,Table,Series,U_S,U_A,U_Acc,Outputs);
			Over -> Over
		end.

		read_columns(Cols,K,A)->
			case tuple_size(Cols) of
				0 -> {};
				1 -> {read_column(element(1,Cols),K,A)};
				2 -> {read_column(element(1,Cols),K,A),read_column(element(2,Cols),K,A)};
				_ -> list_to_tuple([read_column(C,K,A) || C <- tuple_to_list(Cols)])
			end.

		read_column(position,_K,A)-> [fx:position(A)];
		read_column(Column,K,_A)-> element(K,Column).

%column_bytes/2: the memory one unit's columns take for this episode and these agents (flat_eval_check reports it).
column_bytes(Agent_Ids,Episode)->
	Table = config:primary_currency_pair(),
	Specs = lists:usort([Spec || Id <- Agent_Ids, {ok,Net} <- [compile(Id)], Spec <- tuple_to_list(Net#net.sensors), Spec =/= position]),
	{Keys,Columns} = columns(Table,fx_series:ensure(Table),Episode,Specs),
	#{specs=>Specs,bars=>tuple_size(Keys),numbers_per_bar=>lists:sum([width(S) || S <- Specs]),
	  bytes=>erts_debug:flat_size({Keys,Columns})*erlang:system_info(wordsize)}.

	%safe_run/3: a crash in one agent is reported as {error,Why} and does not stop the others.
	safe_run(Id,Episodes,Opts)->
		try run(Id,Episodes,Opts)
		catch Class:Reason:Stack -> {error,{Class,Reason,lists:sublist(Stack,3)}}
		end.

	resolve(validation)-> episodes:ensure_validation_set();
	resolve(tune)-> episodes:current_set(tune);
	resolve(score)-> episodes:current_set(score);
	resolve(List) when is_list(List)-> List.

%check_config/0: the settings this evaluator reproduces.
check_config()->
	Checks = [
		{evaluation_mode,config:evaluation_mode() == episodes},
		{episode_isolation,config:episode_isolation() == true},
		{sense_before_trade,config:sense_before_trade() == true},
		{state_record,check_state_record()}
	],
	case [Name || {Name,false} <- Checks] of
		[] -> ok;
		Failed -> {unsupported,{config,Failed}}
	end.

	check_state_record()->
		Fresh = fx:init_episode_state(#state{},config:primary_currency_pair(),close,{undefined,0}),
		tuple_size(Fresh) == record_info(size,state) andalso element(1,Fresh) == state.

run_net(Net,Episodes0,Opts)->
	Episodes = resolve(Episodes0),
	case stray_check(Net,Episodes) of
		ok -> run_net_checked(Net,Episodes,Opts);
		Unsupported -> Unsupported
	end.

	%stray_check/2: a network with stray links (check_links/6) is refused on episodes long enough for the strays to
	%pile up to half the process path's runaway limit (config:runaway_queue_limit/0, 5000 by default) within one episode:
	%there the process path gives up on the agent, at a moment that depends on when its watchdog looks.
	stray_check(#net{stray=0},_Episodes)->
		ok;
	stray_check(#net{stray=Stray},Episodes)->
		Limit = case config:runaway_queue_limit() of L when is_integer(L), L > 0 -> L; _ -> 5000 end,
		Longest = lists:max([0|[Bars || {_,Bars} <- Episodes]]),
		case 2*Stray*Longest >= Limit of
			true -> {unsupported,{stray_messages_near_runaway_limit,Stray,Longest,Limit}};
			false -> ok
		end.

run_net_checked(Net,Episodes,Opts)->
	Mode = maps:get(mode,Opts,validation),
	Span = case Mode of validation -> validation; _ -> train end,
	Table = config:primary_currency_pair(),
	Series = case Net#net.uses_series of
		true -> fx_series:ensure(Table);
		false -> undefined
	end,
	{ok,episodes(Net,Episodes,Table,Series,Mode,Span,[])}.

	%episodes/7: one episode after another; the evaluation ends after the last one or at an extinct one
	%(fx:episode_trade/6).
	episodes(_Net,[],_Table,_Series,_Mode,_Span,Done)->
		lists:reverse(Done);
	episodes(Net,[Episode|Rest],Table,Series,Mode,Span,Done)->
		put(prev_PC,0),
		S = fx:init_episode_state(#state{},Table,close,Episode),
		A = fx:create_account(),
		Acc = fx:ep_acc_new(Mode,Span),
		Fresh = erlang:make_tuple(Net#net.size,?RO_SIGNAL),
		case bars(Net,Table,Series,S,A,Acc,Fresh) of
			{episode_over,EpStat} -> episodes(Net,Rest,Table,Series,Mode,Span,[EpStat|Done]);
			{extinct,EpStat} -> lists:reverse([EpStat|Done])
		end.

	%bars/7: sense the bar, run the network, trade, and on to the next bar. Prev holds every neuron's output of the
	%bar before (all ?RO_SIGNAL at the first bar of an episode).
	bars(Net,Table,Series,S,A,Acc,Prev)->
		Key = S#state.index,
		Inputs = sense(Net#net.sensors,Table,Series,Key,A),
		Outputs = think(Net#net.neurons,Inputs,Prev,Prev,1),
		Signal = functions:trinary(element(Net#net.out,Outputs)),
		case fx:ep_step(S,A,Acc,Signal) of
			{continue,U_S,U_A,U_Acc} -> bars(Net,Table,Series,U_S,U_A,U_Acc,Outputs);
			Over -> Over
		end.

	%sense/5: the vector of every distinct sensor for the bar with key Key, as the simulator answers it
	%(fx:sense/2 and fx:internals/2).
	sense(Sensors,Table,Series,Key,A)->
		case tuple_size(Sensors) of
			0 -> {};
			_ ->
				P = case Series of undefined -> undefined; _ -> fx_series:position(Table,Key) end,
				list_to_tuple([sensor_vector(Spec,Table,Series,Key,P,A) || Spec <- tuple_to_list(Sensors)])
		end.

		sensor_vector({series,Spec},_Table,Series,_Key,P,_A)-> fx_series:answer_at(Series,P,Spec);
		sensor_vector({clock,Spec},Table,_Series,Key,_P,_A)-> fx_series:answer(Table,Key,Spec);
		sensor_vector(position,_Table,_Series,_Key,_P,A)-> [fx:position(A)].

	%think/5: every neuron in evaluation order. Cur starts as Prev and has each neuron's new output written in as it is
	%computed, so {n,J} (J earlier in the order) reads this bar and {p,J} reads Prev.
	think([],_Inputs,_Prev,Cur,_K)->
		Cur;
	think([{AF,Ins,Bias}|Neurons],Inputs,Prev,Cur,K)->
		Sum = case Bias of
			none -> aggregate(Ins,Inputs,Prev,Cur,0);
			_ -> aggregate(Ins,Inputs,Prev,Cur,0) + Bias
		end,
		Out = activate(AF,sat(Sum)),
		think(Neurons,Inputs,Prev,setelement(K,Cur,Out),K+1).

	%signal_aggregator:dot_product/3: Dot+Acc per input, from the integer 0
	aggregate([],_Inputs,_Prev,_Cur,Acc)->
		Acc;
	aggregate([{{s,I},Ws}|Ins],Inputs,Prev,Cur,Acc)->
		Dot = dot(element(I,Inputs),Ws,0),
		aggregate(Ins,Inputs,Prev,Cur,Dot+Acc);
	aggregate([{{n,J},[W]}|Ins],Inputs,Prev,Cur,Acc)->
		Dot = element(J,Cur)*W+0,
		aggregate(Ins,Inputs,Prev,Cur,Dot+Acc);
	aggregate([{{p,J},[W]}|Ins],Inputs,Prev,Cur,Acc)->
		Dot = element(J,Prev)*W+0,
		aggregate(Ins,Inputs,Prev,Cur,Dot+Acc).

	%signal_aggregator:dot/3
	dot([I|Input],[W|Ws],Acc)->
		dot(Input,Ws,I*W+Acc);
	dot([],[],Acc)->
		Acc.

	%neuron:sat/2 with ?SAT_LIMIT
	sat(Val)->
		Max = abs(?SAT_LIMIT),
		Min = -Max,
		if
			Val < Min -> Min;
			Val > Max -> Max;
			true -> Val
		end.

	activate(tanh,X)-> math:tanh(X);
	activate(linear,X)-> X;
	activate(AF,X)-> functions:AF(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Compiling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(Agent_Id)->
	compile(Agent_Id,#{}).

%compile/2 with #{allow_output_ahead => true} skips the output-lead check; only flat_eval_check uses it, to show what
%the process path does with such a network.
compile(Agent_Id,COpts)->
	{module,functions} = code:ensure_loaded(functions),
	try
		{ok,compile_agent(Agent_Id,COpts)}
	catch
		throw:{unsupported,Why} -> {unsupported,Why}
	end.

	unsupported(Why)-> throw({unsupported,Why}).

	require(true,_Why)-> ok;
	require(false,Why)-> unsupported(Why).

compile_agent(Agent_Id,COpts)->
	A = genotype:dirty_read({agent,Agent_Id}),
	require(is_record(A,agent),agent_not_found),
	require(A#agent.encoding_type == neural,{encoding,A#agent.encoding_type}),
	Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
	require(is_record(Cx,cortex),cortex_not_found),
	SIds = Cx#cortex.sensor_ids,
	NIds = Cx#cortex.neuron_ids,
	AIds = Cx#cortex.actuator_ids,
	require(SIds =/= [],no_sensors),
	Ss = [read(sensor,Id) || Id <- SIds],
	Ns = [read(neuron,Id) || Id <- NIds],
	As = [read(actuator,Id) || Id <- AIds],
	%exoself:link_Sensors/4 adds a sensor link (a random mutation) before it runs an agent whose sensors are all unconnected
	require([S || S <- Ss, S#sensor.fanout_ids =/= []] =/= [],all_sensors_unconnected),
	require(no_duplicates(SIds) andalso no_duplicates(NIds) andalso no_duplicates(AIds),duplicate_ids),
	%the actuator: one fx_Trade fed by one neuron (fx_Trade matches a one-element vector)
	Act = case As of
		[Single] -> Single;
		_ -> unsupported({actuators,length(As)})
	end,
	require(Act#actuator.name == fx_Trade,{actuator,Act#actuator.name}),
	require(Act#actuator.scape == {private,fx_sim},{actuator_scape,Act#actuator.scape}),
	OutId = case Act#actuator.fanin_ids of
		[OneNeuron] -> OneNeuron;
		Fanin -> unsupported({actuator_fanin,length(Fanin)})
	end,
	SensorSpec = maps:from_list([{S#sensor.id,sensor_spec(S)} || S <- Ss]),
	NeuronMap = maps:from_list([{N#neuron.id,N} || N <- Ns]),
	require(maps:is_key(OutId,NeuronMap),actuator_fanin_not_a_neuron),
	%every neuron: plain, its inputs known and of the right width, its bias last
	Plain = maps:from_list([{N#neuron.id,neuron_inputs(N,SensorSpec,NeuronMap)} || N <- Ns]),
	Stray = check_links(Ss,Ns,Act,SensorSpec,NeuronMap,Plain),
	%links: a recurrent link X -> T is one where T is in X's ro_ids
	Delayed = fun(X,T)-> lists:member(T,(maps:get(X,NeuronMap))#neuron.ro_ids) end,
	Order = order(NIds,Plain,Delayed),
	%only what can reach the trade is computed (sensors are read without side effects)
	Needed = reaching(OutId,Plain),
	check_leads(NIds,Plain,Delayed,OutId,Needed,NeuronMap,COpts),
	Kept = [Id || Id <- Order, sets:is_element(Id,Needed)],
	Index = maps:from_list(lists:zip(Kept,lists:seq(1,length(Kept)))),
	UsedSpecs = lists:usort([maps:get(Src,SensorSpec) || Id <- Kept, {Src,_Ws} <- element(1,maps:get(Id,Plain)), maps:is_key(Src,SensorSpec)]),
	SpecIndex = maps:from_list(lists:zip(UsedSpecs,lists:seq(1,length(UsedSpecs)))),
	Neurons = [begin
		{Ins,Bias} = maps:get(Id,Plain),
		N = maps:get(Id,NeuronMap),
		{N#neuron.af,
		 [{case maps:find(Src,SensorSpec) of
			{ok,Spec} -> {s,maps:get(Spec,SpecIndex)};
			error ->
				case Delayed(Src,Id) of
					true -> {p,maps:get(Src,Index)};
					false -> {n,maps:get(Src,Index)}
				end
		   end,Ws} || {Src,Ws} <- Ins],
		 Bias}
	end || Id <- Kept],
	#net{agent_id=Agent_Id,sensors=list_to_tuple(UsedSpecs),neurons=Neurons,size=length(Kept),out=maps:get(OutId,Index),
		uses_series=lists:any(fun({series,_})-> true; (_)-> false end,UsedSpecs),stray=Stray}.

	read(Type,Id)->
		case genotype:dirty_read({Type,Id}) of
			undefined -> unsupported({missing,Type,Id});
			R -> R
		end.

	no_duplicates(L)-> length(L) == length(lists:usort(L)).

	%sensor_spec/1: what the simulator answers for this sensor (sensor.erl, fx:sense/2, fx:internals/2)
	sensor_spec(S)->
		require(S#sensor.scape == {private,fx_sim},{sensor_scape,S#sensor.scape}),
		case {S#sensor.name,S#sensor.parameters} of
			{fx_PLT,[TF,N]} -> plt_check(TF,N), {series,{plt,TF,N}};
			{fx_PLT,[TF,N,vol]} -> plt_check(TF,N), {series,{plt,TF,N,vol}};
			{fx_PLT,Other} -> unsupported({fx_PLT_parameters,Other});
			{fx_IND,_} -> {series,ind};
			{fx_Clock,[clock,H]} when is_integer(H), H >= 1 -> {clock,{clock,H}};
			{fx_Clock,[clock,H]} -> unsupported({fx_Clock_harmonics,H});
			{fx_Clock,_} -> {clock,clock};
			{fx_Position,_} -> position;
			{Name,_} -> unsupported({sensor,Name})
		end.

		plt_check(TF,N)->
			require(lists:member(TF,[15,60,240,1440]) andalso is_integer(N) andalso N >= 1,{fx_PLT_parameters,[TF,N]}).

	width({series,Spec})-> fx_series:vl(Spec);
	width({clock,Spec})-> fx_series:vl(Spec);
	width(position)-> 1.

	%neuron_inputs/3: {[{Src,Weights}],Bias} with Weights a list of numbers, after checking the neuron is one this
	%module reproduces and that signal_aggregator:dot_product/2 would accept its input list.
	neuron_inputs(N,SensorSpec,NeuronMap)->
		Id = N#neuron.id,
		require(element(1,N#neuron.pf) == none,{plasticity,Id,N#neuron.pf}),
		require(N#neuron.aggr_f == dot_product,{aggregator,Id,N#neuron.aggr_f}),
		require(N#neuron.input_idps_modulation == [],{modulation_inputs,Id}),
		AF = N#neuron.af,
		require(is_atom(AF) andalso erlang:function_exported(functions,AF,1) orelse AF == tanh orelse AF == linear,{activation,Id,AF}),
		{Plain,Bias} = case lists:reverse(N#neuron.input_idps) of
			[{bias,BiasWs}|RevPlain] ->
				case BiasWs of
					[{B,_}] when is_number(B) -> {lists:reverse(RevPlain),B};
					_ -> unsupported({bias_weights,Id})
				end;
			_ -> {N#neuron.input_idps,none}
		end,
		require(not lists:keymember(bias,1,Plain),{bias_not_last,Id}),
		require(no_duplicates([Src || {Src,_} <- Plain]),{duplicate_input,Id}),
		Ins = [begin
			Width = case maps:find(Src,SensorSpec) of
				{ok,Spec} -> width(Spec);
				error ->
					case maps:is_key(Src,NeuronMap) of
						true -> 1;
						false -> unsupported({unknown_input,Id,Src})
					end
			end,
			Ws = [case WP of {W,_} when is_number(W) -> W; _ -> unsupported({weight,Id,Src}) end || WP <- WeightsP],
			require(length(Ws) == Width,{weight_length,Id,Src,length(Ws),Width}),
			{Src,Ws}
		end || {Src,WeightsP} <- Plain],
		{Ins,Bias}.

	%check_links/6: both ends of every link agree as far as the processes need, and returns the stray count. A sender
	%sends one message per bar to each entry of its fanout (sensors) or output list (neurons), and a recurrent zero to
	%each entry of its ro_ids after every reset; a receiver waits for one message from each entry of its input list.
	%	- An input nobody sends stops the network (input_not_in_fanout, input_not_an_output): refused.
	%	- A duplicate entry shifts a link by a bar (duplicate_fanout, duplicate_output, duplicate_ro): refused.
	%	- A message sent to a neuron that does not read that sender (a stray link: a fanout or output entry the target
	%	  does not list as input, or a ro_ids entry that is not a link the target reads) is never received: the neuron's
	%	  selective receive passes it by, and the full reset at every episode boundary empties neuron mailboxes. The
	%	  process path runs such a network as if the entry were not there (REFUSAL cases in flat_eval_check), so it is
	%	  accepted and ignored, as long as the target is a neuron of this agent. The strays pile up within an episode, one
	%	  per bar per stray link; the most any one neuron receives per bar is returned, and run_net/3 and run_many/3
	%	  refuse an episode long enough for that pile to reach the process path's runaway limit (stray_check/2).
	check_links(Ss,Ns,Act,SensorSpec,NeuronMap,Plain)->
		ActId = Act#actuator.id,
		InputsOf = fun(T)-> [Src || {Src,_} <- element(1,maps:get(T,Plain))] end,
		SensorStrays = lists:append([begin
			require(no_duplicates(S#sensor.fanout_ids),{duplicate_fanout,S#sensor.id}),
			[begin
				require(maps:is_key(T,NeuronMap),{fanout_not_a_neuron,S#sensor.id,T}),
				T
			 end || T <- S#sensor.fanout_ids, not (maps:is_key(T,NeuronMap) andalso lists:member(S#sensor.id,InputsOf(T)))]
		 end || S <- Ss]),
		NeuronStrays = lists:append([begin
			Id = N#neuron.id,
			require(no_duplicates(N#neuron.output_ids),{duplicate_output,Id}),
			require(no_duplicates(N#neuron.ro_ids),{duplicate_ro,Id}),
			[require(T =/= ActId andalso maps:is_key(T,NeuronMap),{ro_not_a_neuron,Id,T}) || T <- N#neuron.ro_ids],
			[require(lists:member(Id,(maps:get(Src,NeuronMap))#neuron.output_ids) orelse maps:is_key(Src,SensorSpec),{input_not_an_output,Id,Src})
				|| Src <- InputsOf(Id), maps:is_key(Src,NeuronMap)],
			[case maps:find(Src,SensorSpec) of
				{ok,_} ->
					S = lists:keyfind(Src,#sensor.id,Ss),
					require(lists:member(Id,S#sensor.fanout_ids),{input_not_in_fanout,Id,Src});
				error ->
					ok
			 end || Src <- InputsOf(Id)],
			[case T of
				ActId -> require(Act#actuator.fanin_ids == [Id],{actuator_link,Id});
				_ -> require(maps:is_key(T,NeuronMap),{output_not_a_neuron,Id,T})
			 end || T <- N#neuron.output_ids],
			[T || T <- N#neuron.output_ids, T =/= ActId, not lists:member(Id,InputsOf(T))]
		 end || N <- Ns]),
		require(lists:member(ActId,(maps:get(hd(Act#actuator.fanin_ids),NeuronMap))#neuron.output_ids),actuator_fanin_not_an_output),
		Counts = lists:foldl(fun(T,M)-> maps:update_with(T,fun(C)-> C+1 end,1,M) end,#{},SensorStrays++NeuronStrays),
		lists:max([0|maps:values(Counts)]).

	%order/3: the neurons with every non-recurrent neuron input before the neuron (a topological order of the
	%non-recurrent links). A circle of non-recurrent links waits for ever in the process path.
	order(NIds,Plain,Delayed)->
		Deps = maps:from_list([{Id,[Src || {Src,_} <- element(1,maps:get(Id,Plain)), lists:member(Src,NIds), not Delayed(Src,Id)]} || Id <- NIds]),
		order(NIds,Deps,[],sets:new()).

		order([],_Deps,Acc,_Done)->
			lists:reverse(Acc);
		order(Pending,Deps,Acc,Done)->
			{Ready,Waiting} = lists:partition(fun(Id)-> lists:all(fun(D)-> sets:is_element(D,Done) end,maps:get(Id,Deps)) end,Pending),
			case Ready of
				[] -> unsupported({cycle_without_recurrent_link,length(Waiting)});
				_ -> order(Waiting,Deps,lists:reverse(Ready)++Acc,lists:foldl(fun sets:add_element/2,Done,Ready))
			end.

	%check_leads/7: how far ahead of the bar each neuron can fire. A neuron's k-th firing needs the k-th message of
	%each input, and a recurrent link starts with one message already there, so lead(N) = min over inputs of
	%lead(input) + (1 if recurrent), with sensors at 0. A neuron no sensor reaches has no bound: it fires for ever.
	%	- If it can reach the trade, the process path floods a mailbox and gives up (a runaway): refused.
	%	- If it does not, and it is a plain self-loop (its only input and only output itself), it spins on its own
	%	  without touching the rest, takes part in resets, and the process path completes normally: accepted (it is not
	%	  computed). Any other shape is refused: a neuron with no input at all never reads its mailbox, so it never
	%	  answers the reset (a stall), and an unbounded neuron feeding another fills its mailbox.
	%The neuron that drives the trade must have lead 0: one firing ahead leaves a message in the actuator's mailbox,
	%which the reset at an episode boundary does not empty (only neurons empty theirs), so the next episode's trades
	%are shifted by a bar (shown by flat_eval_check:refusal_check/2).
	check_leads(NIds,Plain,Delayed,OutId,Needed,NeuronMap,COpts)->
		Leads = leads(NIds,Plain,Delayed,maps:from_list([{Id,infinity} || Id <- NIds]),length(NIds)+1),
		Unbounded = [Id || Id <- NIds, maps:get(Id,Leads) == infinity],
		require([Id || Id <- Unbounded, sets:is_element(Id,Needed)] == [],{neuron_without_sensor_input,length(Unbounded)}),
		SelfLoop = fun(Id)->
			N = maps:get(Id,NeuronMap),
			[Src || {Src,_} <- element(1,maps:get(Id,Plain))] == [Id] andalso N#neuron.output_ids == [Id]
		end,
		require(lists:all(SelfLoop,Unbounded),{neuron_without_sensor_input,length(Unbounded)}),
		case maps:get(allow_output_ahead,COpts,false) of
			true -> ok;
			false -> require(maps:get(OutId,Leads) == 0,{output_neuron_ahead,maps:get(OutId,Leads)})
		end.

		leads(_NIds,_Plain,_Delayed,Leads,0)->
			Leads;
		leads(NIds,Plain,Delayed,Leads,Rounds)->
			New = maps:from_list([{Id,lists:foldl(fun({Src,_},Best)->
				L = case maps:find(Src,Leads) of
					{ok,infinity} -> infinity;
					{ok,SL} -> SL + case Delayed(Src,Id) of true -> 1; false -> 0 end;
					error -> 0%a sensor
				end,
				min_lead(L,Best)
			end,infinity,element(1,maps:get(Id,Plain)))} || Id <- NIds]),
			case New == Leads of
				true -> Leads;
				false -> leads(NIds,Plain,Delayed,New,Rounds-1)
			end.

		min_lead(infinity,B)-> B;
		min_lead(A,infinity)-> A;
		min_lead(A,B)-> erlang:min(A,B).

	%reaching/2: the neurons whose output can reach the trade, through any links
	reaching(OutId,Plain)->
		reaching([OutId],Plain,sets:from_list([OutId])).

		reaching([],_Plain,Seen)->
			Seen;
		reaching([Id|Rest],Plain,Seen)->
			New = [Src || {Src,_} <- element(1,maps:get(Id,Plain)), maps:is_key(Src,Plain), not sets:is_element(Src,Seen)],
			reaching(New++Rest,Plain,lists:foldl(fun sets:add_element/2,Seen,New)).

%net_info/1: what a compiled network holds, for reports.
net_info(#net{neurons=Ns,sensors=Ss,size=Size})->
	#{neurons=>Size,sensors=>tuple_size(Ss),
	  recurrent_inputs=>length([1 || {_,Ins,_} <- Ns, {{p,_},_} <- Ins]),
	  weights=>lists:sum([length(Ws) || {_,Ins,_} <- Ns, {_,Ws} <- Ins])}.

%sense_bar/5 and think_bar/2: one bar's sensor answers and one bar of the network, for profiling (flat_eval_check).
sense_bar(#net{sensors=Sensors},Table,Series,Key,A)->
	sense(Sensors,Table,Series,Key,A).
think_bar(#net{neurons=Neurons,size=Size},Inputs)->
	Fresh = erlang:make_tuple(Size,?RO_SIGNAL),
	think(Neurons,Inputs,Fresh,Fresh,1).
