%% Evaluation harness for episode-based training (config:evaluation_mode() == episodes).
%%
%% Everything here goes through the same simulator code the agents use (fx:ep_step/4), so a baseline and an agent are
%% always measured by the same rules.
%%
%%   g0()                     integrity checks that must pass before any run is trusted
%%   random_walk_check()      the reversal rules on a series with no pattern: they must score zero
%%   baselines()              scripted policies (never trade, always long/short, random, simple rules) on train and validation
%%   legacy_windows(Spread)   constant-position P&L on the 12 windows of the old curriculum
%%   evaluate_agent(Id)       one agent on the validation episodes, with a shift test against "same activity, no information"
%%   evaluate_population(Id)  the same for every agent of a population
%%   generation_summary/2     called by population_monitor at the end of every generation
%%   record_trial/2           appends to the count of configurations that have been scored against validation
%%
%% All functions need the price table loaded (fx:start/0) and, for the agent functions, Mnesia running.

-module(eval_protocol).
-export([g0/0, baselines/0, baselines/1, legacy_windows/0, legacy_windows/1,
	evaluate_agent/1, evaluate_agent/2, evaluate_population/1, shift_test/3,
	generation_summary/2, record_trial/2, trial_count/0,
	policy/1, run/2, run/3, random_walk_check/0, random_walk_check/1, spearman/2, median/1,
	isolation_check/0, run_agents_on/3, run_agents_on/4, with_config/2, population_agents/1,
	order_effect/2, order_effect/3, census/1, net_pips_ep/1, fill_rule_check/0,
	sd_floor_table/2, money_score_check/0, money_score_check/1, with_random_walk/2,
	score_money_check/3, score_money_check/4]).
-include("records.hrl").

-define(SHIFTS,1000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Scripted policies %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%A policy is fun(StepNo,Close,Position) -> -1 | 0 | 1, called once per bar with the bar's close and the position held.

policy(never)->
	fun(_StepNo,_Close,_Position)-> 0 end;
policy(always_long)->
	fun(_StepNo,_Close,_Position)-> 1 end;
policy(always_short)->
	fun(_StepNo,_Close,_Position)-> -1 end;
policy({random,P})->%with probability P pick a new action uniformly from {-1,0,1}, otherwise keep the position
	fun(_StepNo,_Close,Position)->
		case rand:uniform() < P of
			true -> rand:uniform(3)-2;
			false -> Position
		end
	end;
policy({flip_every,N})->%long for N bars, short for N bars, and so on
	fun(StepNo,_Close,_Position)->
		case (StepNo div N) rem 2 of
			0 -> 1;
			1 -> -1
		end
	end;
policy({momentum,N})->%long if the close is above the close N bars ago, short if below
	fun(StepNo,Close,_Position)-> sign(Close - close_n_bars_ago(StepNo,Close,N)) end;
policy({reversal,N})->%the opposite
	fun(StepNo,Close,_Position)-> sign(close_n_bars_ago(StepNo,Close,N) - Close) end;
policy(always_in)->%no skill, always in the market: a random side at the start, switched with probability 1/250 per bar
	fun(StepNo,_Close,Position)->
		case (StepNo == 0) orelse (Position == 0) orelse (rand:uniform() < 1/250) of
			true -> rand_side();
			false -> Position
		end
	end;
policy({in_market,P,Hold})->%no skill: when flat, enter a random side with probability P per bar and hold it Hold bars
	fun(StepNo,_Close,_Position)->
		Left = case StepNo of 0 -> {0,0}; _ -> get({?MODULE,in_market}) end,
		{Side,U_Left} = case Left of
			{S,L} when L > 0 -> {S,L-1};
			_ ->
				case rand:uniform() < P of
					true -> {rand_side(),Hold-1};
					false -> {0,0}
				end
		end,
		put({?MODULE,in_market},{Side,U_Left}),
		Side
	end;
policy({gain_hold,XPips,Wait})->%REVIEW.md 8.26: enter a random side; leave as soon as the open position is ahead by
	%XPips; a position that is not ahead is held to the end of the episode. After a gain, Wait bars flat, then a new
	%random side. The entry price is the close of the bar on which the position appeared (fx:ep_step/4 fills at the close
	%of the bar the signal is given on), so a fill delayed by the rollover rule is priced where it happened.
	Pip = config:pip_size(),
	fun(StepNo,Close,Position)->
		#{side:=Side,entry:=Entry,last_pos:=LastPos,last_close:=LastClose,wait_until:=WaitUntil} = case StepNo of
			0 -> #{side=>rand_side(),entry=>undefined,last_pos=>0,last_close=>Close,wait_until=>0};
			_ -> get({?MODULE,gain_hold})
		end,
		U_Entry = case {LastPos,Position} of
			{_,0} -> undefined;
			{0,_} -> LastClose;
			_ -> Entry
		end,
		{Signal,U_Side,U_WaitUntil} = case Position of
			0 when StepNo >= WaitUntil -> {Side,Side,WaitUntil};
			0 -> {0,Side,WaitUntil};
			_ ->
				case Position*(Close-U_Entry) >= XPips*Pip - 1.0e-12 of
					true -> {0,rand_side(),StepNo+Wait};
					false -> {Position,Side,WaitUntil}
				end
		end,
		put({?MODULE,gain_hold},#{side=>U_Side,entry=>U_Entry,last_pos=>Position,last_close=>Close,wait_until=>U_WaitUntil}),
		Signal
	end;
policy({peek,Steps})->%REVIEW.md 8.26, "tiny and steady": on each of the given steps it reads the NEXT bar's close (look-ahead,
	%impossible for an agent) and holds that side for that one bar, flat otherwise. A small gain in nearly every episode with a
	%small spread: the case the sd floor of alpha_money_t is there for.
	Table = config:primary_currency_pair(),
	fun(StepNo,Close,_Position,Key)->
		case lists:member(StepNo,Steps) of
			true -> sign((fx:lookup(Table,fx:next(Table,Key)))#technical.close - Close);
			false -> 0
		end
	end;
policy(clock)->%the time-of-day rule of REVIEW.md 8.22, fixed in advance: short 08:00 to 12:00, long 16:00 to 20:00, flat otherwise (file's clock)
	fun(_StepNo,_Close,_Position,Key)->
		case element(4,Key) of
			H when H >= 8, H < 12 -> -1;
			H when H >= 16, H < 20 -> 1;
			_ -> 0
		end
	end.

	close_n_bars_ago(StepNo,Close,N)->
		Previous = case StepNo of
			0 -> [];
			_ -> get({?MODULE,closes})
		end,
		Closes = lists:sublist([Close|Previous],N+1),
		put({?MODULE,closes},Closes),
		case length(Closes) > N of
			true -> lists:last(Closes);
			false -> Close
		end.

	rand_side()->
		2*rand:uniform(2)-3.

	sign(X) when X > 0 -> 1;
	sign(X) when X < 0 -> -1;
	sign(_) -> 0.

%run/3 runs a named policy over a list of episodes and returns the per-episode statistics. Span (train | validation) is
%the span the episodes come from: it decides the drift taken out of the score and the cost per round trip, exactly as for
%an agent (fx:ep_acc_new/2). run/2 is for train episodes.
run(PolicyName,Episodes)->
	run(PolicyName,Episodes,train).
run(PolicyName,Episodes,Span)->
	fx:run_policy(policy(PolicyName),Episodes,validation,Span).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Baselines %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

baselines()->
	baselines(24).

%baselines/1 prints every scripted policy on TrainEpisodes episodes drawn from the train span (with a fixed seed, so the
%table is repeatable) and on the validation set.
baselines(TrainEpisodes)->
	Table = config:primary_currency_pair(),
	{[Train],_} = episodes:sample(Table,config:train_span(),[TrainEpisodes],config:episode_bars(),rand:seed_s(exsss,{11,22,33})),
	Val = episodes:ensure_validation_set(),
	rand:seed(exsss,{101,202,303}),
	Policies = [never,always_long,always_short,{random,1.0},{random,0.1},{random,0.01},{flip_every,50},
		{momentum,15},{momentum,60},{momentum,240},{reversal,15},{reversal,60},{reversal,240}],
	io:format("~nfitness_function=~p  spread=~p  episode_bars=~p  warmup=~p  min_trades_per_episode=~p~n",
		[config:fitness_function(),config:account_spread(),config:episode_bars(),config:episode_warmup_bars(),config:min_trades_per_episode()]),
	[begin
		io:format("~n~s (~p episodes)~n",[Label,length(Episodes)]),
		io:format("~-16s ~10s ~8s ~10s ~9s ~10s ~7s ~7s ~10s~n",["policy","fitness","timing_t","mean_alpha","std_alpha","entries/ep","active","holders","pnl_$/ep"]),
		[print_row(io_lib:format("~p",[Policy]),run(Policy,Episodes,Span)) || Policy <- Policies]
	end || {Label,Episodes,Span} <- [{"TRAIN sample",Train,train},{"VALIDATION",Val,validation}]],
	ok.

	%fitness is the configured fitness function; timing_t is mean(z)*sqrt(K) whatever the fitness function is.
	print_row(Name,EpStats)->
		Sum = fitness_functions:episode_summary(EpStats),
		#{episodes:=K,mean_alpha:=Mean,std_alpha:=Std,entries:=Entries,mean_active:=Active,holder_episodes:=Holders,pnl:=PnL} = Sum,
		io:format("~-16s ~10.3f ~8.2f ~10.4f ~9.4f ~10.1f ~7.3f ~7w ~10.2f~n",
			[lists:flatten(Name),float(fitness_functions:aggregate_episodes(EpStats)),timing_t(Sum),Mean,Std,Entries/K,Active,Holders,PnL/K]).

	timing_t(#{episodes:=K,mean_z:=MeanZ})->
		MeanZ*math:sqrt(K).

%legacy_windows/1 prints what a constant position earns, in dollars at the simulator's sizing, on each window of the old
%12-phase curriculum (gt_start/gt_end as bars back from the last row), at the given spread.
legacy_windows()->
	legacy_windows(config:account_spread()).
legacy_windows(Spread)->
	Table = config:primary_currency_pair(),
	Windows = [{2600,1800},{3600,2600},{5000,3400},{6200,4600},{7400,5600},{8600,6600},{7000,5200},{8200,6200},{9400,7000},{10600,7800},{11800,8600},{13000,9400}],
	with_config([{account_spread,Spread},{episode_warmup_bars,0}],fun()->
		io:format("~nspread=~p~n~5s ~6s ~22s ~12s ~14s ~14s~n",[Spread,"phase","bars","start+bars","drift_pips","always_long_$","always_short_$"]),
		lists:foldl(fun({GtStart,GtEnd},Phase)->
			Episode = {fx:prev(Table,ets:last(Table),prev,GtStart),GtStart-GtEnd},
			[Long] = run(always_long,[Episode]),
			[Short] = run(always_short,[Episode]),
			io:format("~5w ~6w ~22s ~12.1f ~14.2f ~14.2f~n",[Phase,GtStart-GtEnd,episodes:describe(Episode),maps:get(drift,Long)/0.0001,maps:get(pnl,Long),maps:get(pnl,Short)]),
			Phase+1
		end,1,Windows)
	end),
	ok.

	%with_config/2 runs Fun with temporary config overrides and puts the previous values back afterwards.
	with_config(Overrides,Fun)->
		config:init(),
		Saved = [{Key,config:get_val(Key,'$unset')} || {Key,_} <- Overrides],
		[config:set(Key,Value) || {Key,Value} <- Overrides],
		try Fun()
		after
			[case Old of '$unset' -> ets:delete(dxnn_config,Key); _ -> config:set(Key,Old) end || {Key,Old} <- Saved]
		end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Integrity checks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%g0/0 runs the checks of gate G0 at the current configuration and returns ok or {failed,Names}.
g0()->
	Table = config:primary_currency_pair(),
	Bars = config:episode_bars(),
	Spread = config:account_spread(),
	{[Tune,Score],_} = episodes:sample(Table,config:train_span(),[8,16],Bars,rand:seed_s(exsss,{5,6,7})),
	Val = episodes:ensure_validation_set(),
	Episodes = Tune++Score,
	K = length(Episodes),
	Long = run(always_long,Episodes),
	Short = run(always_short,Episodes),
	Never = run(never,Episodes),
	rand:seed(exsss,{9,8,7}),
	Random = lists:append([run({random,1.0},Episodes) || _ <- lists:seq(1,4)]),
	Flip = run({flip_every,50},Episodes),
	RandomAlphas = [maps:get(alpha,EpStat) || EpStat <- Random],
	RandomCosts = [maps:get(cost,EpStat)/maps:get(vol,EpStat) || EpStat <- Random],
	RandomMean = functions:avg(RandomAlphas),
	RandomSE = fitness_functions:sample_std(RandomAlphas)/math:sqrt(length(RandomAlphas)),
	{TrainLo,TrainHi} = episodes:span_positions(Table,config:train_span()),
	{ValLo,ValHi} = episodes:span_positions(Table,config:val_span()),
	RandomWalk = random_walk_check(),
	%What a constant position must score: the move of the episode beyond the average drift of the span, minus cost.
	Constant = fun(Side,EpStat)->
		#{alpha:=Alpha,vol:=Vol,drift:=Drift,bars:=N,mu:=Mu,cost:=Cost} = EpStat,
		abs(Alpha*Vol - (Side*(Drift - Mu*N) - Cost)) < 1.0e-9
	end,
	Checks = [
		{"a constant long position scores the move beyond the span drift, minus its cost",
			lists:all(fun(EpStat)-> Constant(1,EpStat) end,Long)},
		{"a constant short position scores the opposite, minus its cost",
			lists:all(fun(EpStat)-> Constant(-1,EpStat) end,Short)},
		{"a constant long position scores zero over the validation set as a whole",
			abs(lists:sum([maps:get(alpha,EpStat)*maps:get(vol,EpStat) + maps:get(cost,EpStat) || EpStat <- run(always_long,Val,validation)])) < 1.0e-9},
		{"on a random walk the reversal rules score zero (within 4 standard errors)",
			lists:all(fun({_Rule,MeanZ,RWEpisodes})-> abs(MeanZ) < 4/math:sqrt(RWEpisodes) end,RandomWalk)},
		{"long and short gross P&L are mirror images",
			lists:all(fun({L,S})-> abs(maps:get(gross,L) + maps:get(gross,S)) < 1.0e-9 end,lists:zip(Long,Short))},
		{"never trading is ineligible and gets the floor",
			fitness_functions:aggregate_episodes(Never,alpha_lcb) == fitness_functions:episode_floor()},
		{"buy and hold is ineligible when at least 2 trades per episode are required",
			(config:min_trades_per_episode() < 2) orelse (fitness_functions:aggregate_episodes(Long,alpha_lcb) == fitness_functions:episode_floor()+K)},
		{"random trading centres on minus its cost (within 4 standard errors)",
			abs(RandomMean + functions:avg(RandomCosts)) < 4*RandomSE},
		{"dollar P&L agrees with price-unit accounting",
			lists:all(fun(EpStat)-> dollars_agree(Table,EpStat) end,Flip++Long++Short)},
		{"tune and score episodes do not share a start",
			[E || E <- Tune, lists:member(E,Score)] == []},
		{"validation span starts after the train span ends",
			(ValLo > TrainHi) andalso (ValHi > ValLo) andalso (TrainHi > TrainLo)},
		{"every validation episode lies inside the validation span",
			lists:all(fun({StartKey,_})-> {Y,Mo,D,_,_,_,_} = StartKey, date_in_span({Y,Mo,D},config:val_span()) end,Val)}
	],
	io:format("~nG0 integrity checks  (table=~p rows=~p  train rows ~p..~p  validation rows ~p..~p  spread=~p)~n",
		[Table,ets:info(Table,size),TrainLo,TrainHi,ValLo,ValHi,Spread]),
	[io:format("  ~s  ~s~n",[case Passed of true -> "PASS"; false -> "FAIL" end,Name]) || {Name,Passed} <- Checks],
	io:format("  random policy: mean alpha ~.4f, standard error ~.4f, mean cost ~.4f, ~p episodes~n",[RandomMean,RandomSE,functions:avg(RandomCosts),length(Random)]),
	io:format("  span drift per bar: train ~.3e, validation ~.3e~n",[episodes:drift(train),episodes:drift(validation)]),
	[io:format("  random walk, ~p: mean z ~.4f over ~p episodes (standard error ~.4f)~n",[Rule,MeanZ,RWEpisodes,1/math:sqrt(RWEpisodes)]) || {Rule,MeanZ,RWEpisodes} <- RandomWalk],
	case [Name || {Name,false} <- Checks] of
		[] -> ok;
		Failed -> {failed,Failed}
	end.

	%The simulator trades round(Notional/entry) units, so the dollar P&L of an episode is close to
	%(Notional/price) * (gross - cost). The two differ by the rounding of units, by the drift of the entry price inside
	%the episode, and by the spread of a position opened on the very last bar (which is charged but not scored).
	dollars_agree(Table,EpStat)->
		#{pnl:=PnL,gross:=Gross,cost:=Cost,start:=StartKey} = EpStat,
		Notional = config:buy_money_fixed()*config:account_leverage(),
		Price = (fx:lookup(Table,StartKey))#technical.close,
		abs(PnL - Notional*(Gross-Cost)/Price) =< 0.02*abs(PnL) + 0.5 + Notional*config:account_spread().

	date_in_span(_Date,all)-> true;
	date_in_span(_Date,{bars_back,_,_})-> true;
	date_in_span(Date,{From,To})-> (Date >= From) andalso (Date =< To).

%random_walk_check/1 runs the 60-bar and the 240-bar reversal rule on K episodes of a price series that has no pattern at
%all (a random walk made here, in a table of its own) and returns [{Rule,MeanZ,K}]. Nothing can be predicted on such a
%series, so mean z must be zero to within its standard error 1/sqrt(K). The score formula used until November 2026 gave
%about +0.2 and +0.3 here (see fx.erl); g0/0 fails if that ever comes back.
random_walk_check()->
	random_walk_check(400).
random_walk_check(K)->
	with_random_walk(K,fun(Episodes)->
		[begin
			Zs = [maps:get(z,EpStat) || EpStat <- run(Rule,Episodes,train)],
			{Rule,functions:avg(Zs),length(Zs)}
		end || Rule <- [{reversal,60},{reversal,240}]]
	end).

%with_random_walk/2 makes a random-walk table of K episodes of 1,000 bars (fixed seed), points the config at it (no
%cost, no trade minimum) and calls Fun(Episodes); the table is deleted afterwards.
with_random_walk(K,Fun)->
	Table = 'RW_CHECK',
	Bars = 1000,
	Rows = 1000 + (K+1)*Bars,
	catch ets:delete(Table),
	ets:new(Table,[ordered_set,public,named_table,{keypos,2}]),
	rand:seed(exsss,{4,5,6}),
	lists:foldl(fun(I,Close)->
		ets:insert(Table,#technical{id={2000,1,1,0,0,I,1},open=Close,high=Close,low=Close,close=Close,volume=0.0}),
		Close + 1.0e-4*rand:normal()
	end,1.1,lists:seq(1,Rows)),
	try
		with_config([{primary_currency_pair,Table},{train_span,all},{val_span,all},{episode_bars,Bars},{episode_warmup_bars,30},
			{episode_min_history,1000},{account_spread,0.0},{validation_spread,undefined},{min_trades_per_episode,0}],fun()->
			Fun(episodes:tile(Table,all,Bars,K))
		end)
	after
		ets:delete(Table),
		persistent_term:erase({episodes,index,Table})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fill rules %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%fill_rule_check/0 tests the two fill rules of fx:ep_step/4 with scripted policies on 40 train episodes (REVIEW.md 8.23):
%	rollover_rule  no position changes on a scored bar whose hour is 21, 22 or 23 (a random policy that changes its
%	               position on 20% of bars; the same policy without the rule must change some there, or the check
%	               would prove nothing)
%	late_fill      the 15-bar momentum rule, whose signal does not depend on its position: with the late fill its
%	               position at scored bar I is the one it had at bar I-1 without it, and flat at the first scored bar.
%Returns ok or {failed,Names}.
fill_rule_check()->
	Table = config:primary_currency_pair(),
	Warmup = config:episode_warmup_bars(),
	{[Eps],_} = episodes:sample(Table,config:train_span(),[40],config:episode_bars(),rand:seed_s(exsss,{13,17,19})),
	Positions = fun(Policy,Overrides)->
		rand:seed(exsss,{1,2,3}),
		with_config([{validation_series,true},{late_fill,false},{rollover_rule,false}|Overrides],fun()-> [maps:get(u_series,E) || E <- run(Policy,Eps,train)] end)
	end,
	Hours = [[element(4,Key) || Key <- lists:sublist(keys(Table,Start,config:episode_bars()),Warmup+1,config:episode_bars())] || {Start,_} <- Eps],
	Changes = fun(Us)-> lists:sum([length([1 || {H,P,Prev} <- lists:zip3(lists:sublist(Hs,length(U)),U,[0|lists:droplast(U)]), H >= 21, P =/= Prev]) || {Hs,U} <- lists:zip(Hours,Us), U =/= []]) end,
	Free = Changes(Positions({random,0.2},[])),
	Ruled = Changes(Positions({random,0.2},[{rollover_rule,true}])),
	Now = Positions({momentum,15},[]),
	Late = Positions({momentum,15},[{late_fill,true}]),
	Shifted = [L == [0|lists:droplast(N)] || {N,L} <- lists:zip(Now,Late)],
	Checks = [
		{"rollover rule: no position changes in hours 21 to 23",Ruled == 0},
		{"rollover rule: without it the same policy changes position in those hours",Free > 0},
		{"late fill: every position is the one of the bar before, without it",lists:all(fun(X)-> X end,Shifted)}
	],
	io:format("~nFill rule checks (40 train episodes)~n"),
	[io:format("  ~s  ~s~n",[case Passed of true -> "PASS"; false -> "FAIL" end,Name]) || {Name,Passed} <- Checks],
	io:format("  position changes in hours 21 to 23: ~p without the rule, ~p with it~n",[Free,Ruled]),
	case [Name || {Name,false} <- Checks] of
		[] -> ok;
		Failed -> {failed,Failed}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% The money score %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%REVIEW.md 8.26. alpha_money_t = mean(alpha)/max(sd(alpha),Floor)*sqrt(K); alpha_pips_t the same on the drift-free money
%in pips (fitness_functions:money_t/2, episode_pips/1).

%sd_floor_table/2 measures what the floors of alpha_money_t and alpha_pips_t are derived from: the sd over episodes of the per-episode
%alpha of policies without skill that are in the market a given share of the time ({in_market,P,Hold}: enter a random
%side with probability P per flat bar, hold Hold bars), on NEpisodes train episodes (fixed draw), at CostPips per
%position, with the fill rules in force. Prints one row per policy and returns them as maps.
sd_floor_table(NEpisodes,CostPips)->
	Table = config:primary_currency_pair(),
	{[Eps],_} = episodes:sample(Table,config:train_span(),[NEpisodes],config:episode_bars(),rand:seed_s(exsss,{31,37,41})),
	Policies = [{lists:flatten(io_lib:format("target ~.1f% in, hold ~p",[100*F,Hold])),{in_market,in_market_p(F,Hold),Hold}} || Hold <- [10,60,240], F <- [0.005,0.01,0.02,0.03,0.05,0.10,0.25]] ++ [{"always in",always_in}],
	io:format("~nsd of per-episode alpha without skill: ~p train episodes, cost ~p pip per position, rollover rule ~p~n",[NEpisodes,CostPips,config:rollover_rule()]),
	io:format("~-34s ~10s ~10s ~10s ~10s ~10s ~12s ~12s ~10s ~12s~n",["policy","in_market","entries/ep","sd_alpha","mean_alpha","se_mean","sd32_median","sd32_min","sd_pips","sd32_pips_md"]),
	with_config([{account_spread,CostPips*config:pip_size()},{validation_spread,undefined}],fun()->
		[begin
			rand:seed(exsss,{43,47,53}),
			Stats = run(Policy,Eps,train),
			Sum = fitness_functions:episode_summary(Stats),
			#{mean_abs_u:=InMarket,entries:=Entries,std_alpha:=Sd,mean_alpha:=Mean,episodes:=K} = Sum,
			%The sd as one 32-episode evaluation sees it: median and smallest over disjoint blocks.
			BlockSds = [fitness_functions:sample_std([maps:get(alpha,E) || E <- B]) || B <- chunks(Stats,32)],
			%The same for alpha_pips_t: the sd of drift-free money in pips.
			SdPips = fitness_functions:sample_std([fitness_functions:episode_pips(E) || E <- Stats]),
			BlockPips = [fitness_functions:sample_std([fitness_functions:episode_pips(E) || E <- B]) || B <- chunks(Stats,32)],
			io:format("~-34s ~10.4f ~10.2f ~10.4f ~10.4f ~10.4f ~12.4f ~12.4f ~10.2f ~12.2f~n",[Label,InMarket,Entries/K,Sd,Mean,Sd/math:sqrt(K),median(BlockSds),lists:min(BlockSds),SdPips,median(BlockPips)]),
			#{policy=>Label,in_market=>InMarket,entries_per_ep=>Entries/K,sd_alpha=>Sd,mean_alpha=>Mean,sd32_median=>median(BlockSds),sd32_min=>lists:min(BlockSds),sd_pips=>SdPips,sd32_pips_median=>median(BlockPips)}
		end || {Label,Policy} <- Policies]
	end).

	%in_market_p/2: the entry probability per flat bar that keeps {in_market,P,Hold} in the market a share F of the time.
	in_market_p(F,Hold)->
		F/(Hold*(1-F)).

%money_score_check/1 runs the tests of REVIEW.md 8.26, step 2 (a) to (c), for Name = alpha_money_t or alpha_pips_t
%("score" below; money_score_check/0 takes the fitness function in force if it is one of the two, else alpha_money_t),
%under the config in force (load the arm's
%settings first; the trade minimum, the fill rules and the floor are read from it). Returns ok or {failed,Names}.
%	(a) no skill, no cost: always in, in a tenth of the time, trading often; on a random walk (640 episodes) and on train
%	    episodes (1,280): alpha_money_t of each disjoint 32-episode evaluation must average 0 within 3 standard errors
%	    and spread by at most 1.3 (a t-statistic: about 0 +/- 1). The trade minimum is off here, so every block is scored.
%	(b) take the gain, hold the loss ({gain_hold,X,60}, X = 3 and 8 pips) on 600 train episodes ({sample,train 2012 to
%	    2016,600,77}, as resim), 0.25 pip, the fixed seed and 20 more (the side of each entry is a coin). Decides:
%	    alpha_money_t of the fixed seed at or below 2 + max(0, the money's own t over the 600 episodes), and over the
%	    20 seeds alpha_money_t exceeds the money's own t by less than 1 on average. Printed: money (on these episodes
%	    X = 8 makes money, so "money at or below zero" is not a property of that policy), alpha_t and its excess.
%	(c) tiny and steady ({peek,..}, look-ahead on three bars per episode) on the same 600 episodes, without cost and at
%	    0.25 pip: the mean alpha_money_t of 32-episode evaluations must stay below 3. Printed next to it: the same with
%	    no floor.
money_score_check()->
	money_score_check(case config:fitness_function() of alpha_pips_t -> alpha_pips_t; _ -> alpha_money_t end).
money_score_check(Name)->
	FloorKey = case Name of alpha_pips_t -> alpha_pips_sd_floor; alpha_money_t -> alpha_money_sd_floor end,
	Floor = config:FloorKey(),
	Pip = config:pip_size(),
	Train = resim:episodes({sample,config:train_span(),600,77}),
	{[Train1280],_} = episodes:sample(config:primary_currency_pair(),config:train_span(),[1280],config:episode_bars(),rand:seed_s(exsss,{59,61,67})),
	NoSkill = [{always_in,always_in},{tenth,{in_market,in_market_p(0.1,20),20}},{often,{random,0.2}}],
	Blocks = fun(Stats,Fn)-> [fitness_functions:aggregate_episodes(B,Fn) || B <- chunks(Stats,32)] end,
	MeanSd = fun(Xs)-> {functions:avg(Xs),fitness_functions:sample_std(Xs),length(Xs)} end,
	NoSkillRow = fun(Label,Stats)->
		{M,Sd,N} = MeanSd(Blocks(Stats,Name)),
		{MT,SdT,_} = MeanSd(Blocks(Stats,alpha_t)),
		Sum = fitness_functions:episode_summary(Stats),
		io:format("  ~-28s in_market ~.3f entries/ep ~6.1f | score mean ~7.3f sd ~6.3f (~p evaluations) | alpha_t mean ~7.3f sd ~6.3f~n",
			[Label,maps:get(mean_abs_u,Sum),maps:get(entries,Sum)/maps:get(episodes,Sum),M,Sd,N,MT,SdT]),
		{Label,(abs(M) =< 3*Sd/math:sqrt(N)) andalso (Sd =< 1.3)}
	end,
	io:format("~nMoney score checks (score = ~p, sd floor ~p; trade minimum ~p, rollover rule ~p)~n",[Name,Floor,config:min_trades_per_episode(),config:rollover_rule()]),
	io:format(" (a) no skill, no cost, disjoint 32-episode evaluations~n"),
	RW = with_random_walk(640,fun(Episodes)->
		[begin rand:seed(exsss,{71,73,79}), NoSkillRow("random walk, "++atom_to_list(Label),run(Policy,Episodes,train)) end || {Label,Policy} <- NoSkill]
	end),
	Real = with_config([{account_spread,0.0},{validation_spread,undefined},{min_trades_per_episode,0}],fun()->
		[begin rand:seed(exsss,{71,73,79}), NoSkillRow("train, "++atom_to_list(Label),run(Policy,Train1280,train)) end || {Label,Policy} <- NoSkill]
	end),
	io:format(" (b) take the gain, hold the loss: 600 train episodes, 0.25 pip~n"),
	Quarter = [{account_spread,0.25*Pip},{validation_spread,undefined}],
	%The side of each entry is a coin, so one seed's money is the policy's expected money (minus its cost) plus about
	%+/-1.9 pips of luck. The fixed seed is reported as asked; the decision also uses 20 more seeds.
	Seeds = [{83,89,97}|[{101+S,103+S,107+S} || S <- lists:seq(1,20)]],
	Exploit = with_config(Quarter,fun()->
		lists:append([begin
			[Fixed|More] = [begin rand:seed(exsss,Seed), money_row(run({gain_hold,X,60},Train,train),Pip) end || Seed <- Seeds],
			#{money:=Money,se:=Se,alpha_t:=AT} = Fixed,
			AM = maps:get(Name,Fixed),
			io:format("  gain_hold X=~p pips, fixed seed: money ~.2f pips/episode (se ~.2f), alpha_t ~.2f, score ~.2f, in_market ~.3f, entries/ep ~.1f~n",
				[X,Money,Se,AT,AM,maps:get(in_market,Fixed),maps:get(entries_per_ep,Fixed)]),
			Col = fun(Key)-> [maps:get(Key,R) || R <- More] end,
			{MM,SdM,N} = MeanSd(Col(money)),
			{MAT,SdAT,_} = MeanSd(Col(alpha_t)),
			{MAM,SdAM,_} = MeanSd(Col(Name)),
			%Excess: a score minus the t-statistic of the money itself over the same episodes (money / its se). A score
			%that follows the money has an excess near zero; one the agent can inflate does not.
			Excess = fun(Key)-> [maps:get(Key,R) - maps:get(money,R)/maps:get(se,R) || R <- More] end,
			{EAT,_,_} = MeanSd(Excess(alpha_t)),
			{EAM,SdEAM,_} = MeanSd(Excess(Name)),
			io:format("  gain_hold X=~p pips, ~p other seeds: money ~.2f (sd ~.2f), alpha_t ~.2f (sd ~.2f, above 2 in ~p), score ~.2f (sd ~.2f, above 2 in ~p); rank correlation with money: alpha_t ~s, score ~s; excess over the money's own t: alpha_t ~.2f, score ~.2f (sd ~.2f)~n",
				[X,N,MM,SdM,MAT,SdAT,length([1 || V <- Col(alpha_t), V > 2]),MAM,SdAM,length([1 || V <- Col(Name), V > 2]),
				 fmt(spearman(Col(money),Col(alpha_t))),fmt(spearman(Col(money),Col(Name))),EAT,EAM,SdEAM]),
			[{lists:flatten(io_lib:format("take the gain, hold the loss, X=~p, fixed seed: score ~.2f =< 2 + max(0, money's own t ~.2f)",[X,AM,Money/Se])),AM =< 2.0 + erlang:max(0.0,Money/Se)},
			 {lists:flatten(io_lib:format("take the gain, hold the loss, X=~p, ~p seeds: score exceeds the money's own t by less than 1 on average (~.2f; alpha_t ~.2f)",[X,N,EAM,EAT])),EAM < 1.0}]
		end || X <- [3,8]])
	end),
	io:format(" (c) tiny and steady (look-ahead on three bars per episode), 600 train episodes~n"),
	Warmup = config:episode_warmup_bars(),
	Tiny = [begin
		Stats = with_config([{account_spread,Cost*Pip},{validation_spread,undefined}],fun()-> run({peek,[Warmup+100,Warmup+400,Warmup+700]},Train,train) end),
		R = money_row(Stats,Pip),
		{M,_,_} = MeanSd(Blocks(Stats,Name)),
		{MNoFloor,_,_} = with_config([{FloorKey,0.0}],fun()-> MeanSd(Blocks(Stats,Name)) end),
		{MT,_,_} = MeanSd(Blocks(Stats,alpha_t)),
		io:format("  cost ~p pip: money ~.3f pips/episode, sd_alpha ~.4f; mean of 32-episode evaluations: score ~.2f, without the floor ~.2f, alpha_t ~.2f~n",
			[Cost,maps:get(money,R),maps:get(sd_alpha,R),M,MNoFloor,MT]),
		{lists:flatten(io_lib:format("tiny and steady at ~p pip: score of a 32-episode evaluation below 3",[Cost])),M < 3.0}
	end || Cost <- [0.0,0.25]],
	Checks = RW ++ Real ++ Exploit ++ Tiny,
	[io:format("  ~s  ~s~n",[case Passed of true -> "PASS"; false -> "FAIL" end,Check]) || {Check,Passed} <- Checks],
	case [Check || {Check,false} <- Checks] of
		[] -> ok;
		Failed -> {failed,Failed}
	end.

%score_money_check/3,4 (REVIEW.md 8.26) replays the best N agents of each family of a saved population (by stored
%fitness) on K training episodes drawn with a fixed seed (Seed, default 97; not a generation's own episodes) at the
%validation cost (validation_spread, else account_spread), with the fill rules and fitness function in force, and prints
%per agent its money per episode after cost (pips) with its standard error over episodes, the score of the fitness in
%force, alpha_t and alpha_pips_t; then the rank correlation of each with money, over all and per family. Run it after
%any change to what agents can sense or do: a score that has stopped meaning money shows here first. Like every replay,
%the drift taken out is that of the K episodes themselves (episodes:set_drift/2); money is unaffected. Returns a map.
score_money_check(Population_Id,N,K)->
	score_money_check(Population_Id,N,K,97).
score_money_check(Population_Id,N,K,Seed)->
	P = genotype:dirty_read({population,Population_Id}),
	Chosen = lists:append([begin
		Agents = [A || A <- [genotype:dirty_read({agent,Id}) || Id <- (genotype:dirty_read({specie,Specie_Id}))#specie.agent_ids],
			is_record(A,agent), is_number(A#agent.fitness)],
		Best = lists:sublist(lists:reverse(lists:keysort(#agent.fitness,Agents)),N),
		[{Family,Rank,A#agent.id,A#agent.fitness} || {Rank,A} <- lists:zip(lists:seq(1,length(Best)),Best)]
	end || {Family,Specie_Id} <- lists:zip(lists:seq(1,length(P#population.specie_ids)),P#population.specie_ids)]),
	{[Episodes],_} = episodes:sample(config:primary_currency_pair(),config:train_span(),[K],config:episode_bars(),rand:seed_s(exsss,{Seed,Seed+1,Seed+2})),
	Cost = validation_spread(),
	Pip = config:pip_size(),
	Results = run_agents_on([Id || {_,_,Id,_} <- Chosen],Episodes,[{validation_spread,Cost}]),
	Rows = [begin
		Monies = [(maps:get(gross,E)-maps:get(cost,E))/Pip || E <- Stats],
		#{family=>Family,rank=>Rank,stored=>Stored,money=>functions:avg(Monies),se=>fitness_functions:sample_std(Monies)/math:sqrt(length(Monies)),
		  score=>fitness_functions:aggregate_episodes(Stats),alpha_t=>fitness_functions:aggregate_episodes(Stats,alpha_t),
		  alpha_pips_t=>fitness_functions:aggregate_episodes(Stats,alpha_pips_t),eligible=>fitness_functions:eligible(fitness_functions:episode_summary(Stats))}
	end || {{Family,Rank,Id,Stored},{Id,{ok,Stats}}} <- lists:zip(Chosen,Results)],
	io:format("~nSCORE_MONEY_CHECK ~p: best ~p of each family, ~p train episodes (seed ~p), cost ~p, fitness ~p, rollover rule ~p; ~p of ~p agents replayed~n",
		[Population_Id,N,K,Seed,Cost,config:fitness_function(),config:rollover_rule(),length(Rows),length(Chosen)]),
	io:format("~6s ~4s ~10s ~10s ~8s ~9s ~9s ~12s ~8s~n",["family","rank","stored","money_ep","se","score","alpha_t","alpha_pips_t","eligible"]),
	[io:format("~6w ~4w ~10s ~10.3f ~8.3f ~9s ~9s ~12s ~8w~n",[F,R,fmt(St),M,Se,fmt(Sc),fmt(AT),fmt(AP),El])
		|| #{family:=F,rank:=R,stored:=St,money:=M,se:=Se,score:=Sc,alpha_t:=AT,alpha_pips_t:=AP,eligible:=El} <- Rows],
	Rho = fun(Selected,Key)-> spearman([maps:get(Key,Row) || Row <- Selected],[maps:get(money,Row) || Row <- Selected]) end,
	Eligible = [Row || Row <- Rows, maps:get(eligible,Row)],
	Families = lists:usort([maps:get(family,Row) || Row <- Eligible]),
	io:format("rank correlation with money, eligible agents (~p): score ~s, alpha_t ~s, alpha_pips_t ~s~n",
		[length(Eligible),fmt(Rho(Eligible,score)),fmt(Rho(Eligible,alpha_t)),fmt(Rho(Eligible,alpha_pips_t))]),
	[io:format("  family ~p: score ~s (needs 3 agents or more)~n",[F,fmt(Rho([Row || Row <- Eligible, maps:get(family,Row) == F],score))]) || F <- Families],
	#{rows=>Rows,rho_score_money=>Rho(Eligible,score),rho_alpha_t_money=>Rho(Eligible,alpha_t),rho_alpha_pips_t_money=>Rho(Eligible,alpha_pips_t)}.

	%money_row/2: money after cost per episode (pips) with its standard error over episodes, both scores over all the
	%episodes, the sd of alpha, time in the market and positions per episode.
	money_row(Stats,Pip)->
		Monies = [(maps:get(gross,E)-maps:get(cost,E))/Pip || E <- Stats],
		Sum = fitness_functions:episode_summary(Stats),
		K = length(Stats),
		maps:merge(fitness_functions:scores(Stats),
			#{money=>functions:avg(Monies),se=>fitness_functions:sample_std(Monies)/math:sqrt(K),sd_alpha=>maps:get(std_alpha,Sum),
			  in_market=>maps:get(mean_abs_u,Sum),entries_per_ep=>maps:get(entries,Sum)/K}).

	chunks(List,N) when length(List) >= N->
		{Chunk,Rest} = lists:split(N,List),
		[Chunk|chunks(Rest,N)];
	chunks(_List,_N)->
		[].

	keys(_Table,_Key,0)-> [];
	keys(_Table,'$end_of_table',_N)-> [];
	keys(Table,Key,N)-> [Key|keys(Table,ets:next(Table,Key),N-1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Episode isolation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%isolation_check/0 tests config:episode_isolation/0 (REVIEW.md 8.23). It builds a small network by hand that carries
%every kind of memory a neuron can have: a neuron that feeds its own output back (recurrent input), the output neuron
%doing the same, a neuron with a hebbian plasticity rule (weights that change while it runs) and one with the
%diff_product aggregator (memory of its previous input). The network is run on three train episodes A, B and C in three
%orders: B alone, A then B, C then B. With isolation on, B's position at every scored bar and B's money must be
%identical in all three. The same is run with isolation off and reported (B differs there: that is what the setting
%removes), but only the isolation-on result decides. Needs Mnesia and the price table, like evaluate_agent/1.
%Returns ok or {failed,Details}.
isolation_check()->
	Table = config:primary_currency_pair(),
	{[[A,B,C]],_} = episodes:sample(Table,config:train_span(),[3],config:episode_bars(),rand:seed_s(exsss,{23,29,31})),
	Agent_Id = build_test_agent(),
	try
		Orders = [{alone,[B]},{after_a,[A,B]},{after_c,[C,B]}],
		io:format("~nEpisode isolation check  (A=~s  B=~s  C=~s)~n",[episodes:describe(A),episodes:describe(B),episodes:describe(C)]),
		Verdicts = [begin
			[{alone,Ref}|Others] = [{Order,b_stats(Agent_Id,Episodes,Isolation)} || {Order,Episodes} <- Orders],
			Diffs = [{Order,compare_b(Ref,Stat)} || {Order,Stat} <- Others],
			io:format("  isolation ~-5w  B alone: ~s; ~s~n",[Isolation,describe_b(Ref),
				string:join([lists:flatten(io_lib:format("~p: ~s",[Order,describe_diff(D)])) || {Order,D} <- Diffs],"; ")]),
			{Isolation,Ref,Diffs}
		end || Isolation <- [false,true]],
		{true,RefOn,DiffsOn} = lists:keyfind(true,1,Verdicts),
		Failures = [{Order,D} || {Order,D} <- DiffsOn, D =/= same] ++
			[{b_did_not_trade,RefOn} || not (is_map(RefOn) andalso maps:get(entries,RefOn) > 0)],
		case Failures of
			[] ->
				io:format("  PASS  with isolation on, B's position at every bar and its money are identical in all three orders~n"),
				ok;
			_ ->
				io:format("  FAIL  ~p~n",[Failures]),
				{failed,Failures}
		end
	after
		genotype:delete_Agent(Agent_Id)
	end.

	%b_stats/3: the statistics of the last episode of Episodes (always B) for the test agent. config:sense_before_trade/0
	%is on: H4's sensor is off the path to the trade, and without it that sensor can read the next bar (8.23).
	b_stats(Agent_Id,Episodes,Isolation)->
		case run_agents_on([Agent_Id],Episodes,[{episode_isolation,Isolation},{sense_before_trade,true}]) of
			[{Agent_Id,{ok,EpStats}}] when length(EpStats) == length(Episodes) -> lists:last(EpStats);
			[{Agent_Id,Other}] -> {error,Other}
		end.

	%z and alpha are not compared: they take out the drift of the whole episode set (episodes:set_drift/2), and the
	%three sets differ. Positions and money do not depend on it.
	compare_b(Ref,Stat) when is_map(Ref), is_map(Stat)->
		Fields = [u_series,entries,gross,cost,pnl,status,bars,mean_u,mean_abs_u,drift,vol],
		case [F || F <- Fields, maps:get(F,Ref) =/= maps:get(F,Stat)] of
			[] ->
				same;
			Fs ->
				U1 = maps:get(u_series,Ref),
				U2 = maps:get(u_series,Stat),
				{differs,Fs,length([1 || {X,Y} <- lists:zip(U1,U2,trim), X =/= Y]),length(U1),pips(Ref),pips(Stat)}
		end;
	compare_b(Ref,Stat)->
		{not_compared,Ref,Stat}.

	pips(#{gross:=Gross,cost:=Cost})-> (Gross-Cost)/config:pip_size().

	describe_b(Stat) when is_map(Stat)->
		lists:flatten(io_lib:format("~p positions opened, ~p scored bars, money ~.3f pips",[maps:get(entries,Stat),length(maps:get(u_series,Stat)),pips(Stat)]));
	describe_b(Other)->
		lists:flatten(io_lib:format("~p",[Other])).

	describe_diff(same)-> "identical";
	describe_diff({differs,_Fs,Bars,Of,P1,P2})->
		lists:flatten(io_lib:format("position differs at ~p of ~p bars, money ~.3f pips (B alone ~.3f)",[Bars,Of,P2,P1]));
	describe_diff(Other)->
		lists:flatten(io_lib:format("~p",[Other])).

	%build_test_agent/0 writes a five-neuron agent to Mnesia and returns its id. Weights come from a fixed seed.
	%	H1  tanh of a weighted sum of the indicators (fx_IND) and of its own previous output (weight 0.9)
	%	H2  tanh of the change of the hourly price list (fx_PLT [60,24]) since the previous bar (diff_product)
	%	H3  tanh of a weighted sum of the indicators, hebbian plasticity
	%	H4  tanh of the 15-minute price list (fx_PLT [15,32]); it feeds O only through a recurrent link, so it and its
	%	    sensor are off the path to the trade
	%	O   tanh of H1, H2, H3, H4's previous output and its own previous output (weight 0.5); drives the trade (fx_Trade)
	build_test_agent()->
		U = fun()-> genotype:generate_UniqueId() end,
		Agent_Id = {U(),agent},
		Cx_Id = {{origin,U()},cortex},
		[SInd,SPlt,SP15] = [{{-1,U()},sensor} || _ <- [1,2,3]],
		[H1,H2,H3] = [{{0,U()},neuron} || _ <- [1,2,3]],
		O = {{0.5,U()},neuron},
		H4 = {{0.75,U()},neuron},
		Act = {{1,U()},actuator},
		R0 = rand:seed_s(exsss,{41,43,47}),
		{WInd1,R1} = test_weights(12,0.5,R0),
		{WPlt,R2} = test_weights(24,1.5,R1),
		{WInd3,R3} = test_weights(12,0.5,R2),
		{WP15,_R4} = test_weights(32,0.5,R3),
		W = fun(X)-> [{X,[]}] end,
		Records = [
			#sensor{id=SInd,name=fx_IND,type=standard,cx_id=Cx_Id,scape={private,fx_sim},vl=12,fanout_ids=[H1,H3],generation=0,format=no_geo,parameters=[ind]},
			#sensor{id=SPlt,name=fx_PLT,type=standard,cx_id=Cx_Id,scape={private,fx_sim},vl=24,fanout_ids=[H2],generation=0,format=no_geo,parameters=[60,24]},
			#sensor{id=SP15,name=fx_PLT,type=standard,cx_id=Cx_Id,scape={private,fx_sim},vl=32,fanout_ids=[H4],generation=0,format=no_geo,parameters=[15,32]},
			#neuron{id=H4,generation=0,cx_id=Cx_Id,af=tanh,pf={none,[]},aggr_f=dot_product,
				input_idps=[{SP15,WP15},{bias,W(0.0)}],output_ids=[O],ro_ids=[O]},
			#neuron{id=H1,generation=0,cx_id=Cx_Id,af=tanh,pf={none,[]},aggr_f=dot_product,
				input_idps=[{SInd,WInd1},{H1,W(0.9)},{bias,W(0.1)}],output_ids=[O,H1],ro_ids=[H1]},
			#neuron{id=H2,generation=0,cx_id=Cx_Id,af=tanh,pf={none,[]},aggr_f=diff_product,
				input_idps=[{SPlt,WPlt},{bias,W(0.0)}],output_ids=[O],ro_ids=[]},
			#neuron{id=H3,generation=0,cx_id=Cx_Id,af=tanh,pf={hebbian,[0.02]},aggr_f=dot_product,
				input_idps=[{SInd,WInd3},{bias,W(-0.1)}],output_ids=[O],ro_ids=[]},
			#neuron{id=O,generation=0,cx_id=Cx_Id,af=tanh,pf={none,[]},aggr_f=dot_product,
				input_idps=[{H1,W(1.2)},{H2,W(-1.0)},{H3,W(0.9)},{H4,W(0.8)},{O,W(0.5)}],output_ids=[Act,O],ro_ids=[O]},
			#actuator{id=Act,name=fx_Trade,type=standard,cx_id=Cx_Id,scape={private,fx_sim},vl=1,fanin_ids=[O],generation=0,format=no_geo,parameters=[]},
			#cortex{id=Cx_Id,agent_id=Agent_Id,neuron_ids=[H1,H2,H3,O,H4],sensor_ids=[SInd,SPlt,SP15],actuator_ids=[Act]},
			#agent{id=Agent_Id,encoding_type=neural,generation=0,population_id=isolation_check,specie_id=isolation_check,cx_id=Cx_Id,
				constraint=#constraint{},pattern=[{0,[H1,H2,H3]},{0.5,[O]},{0.75,[H4]}],tuning_selection_f=dynamic_random,annealing_parameter=1,
				tuning_duration_f={const,1},perturbation_range=1,mutation_operators=[],tot_topological_mutations_f={ncount_exponential,0.5},
				heredity_type=darwinian,fitness=0}
		],
		{atomic,_} = mnesia:transaction(fun()-> [mnesia:write(Record) || Record <- Records] end),
		Agent_Id.

	test_weights(N,Scale,R)->
		lists:mapfoldl(fun(_,RA)-> {X,RB} = rand:uniform_s(RA), {{(X-0.5)*2*Scale,[]},RB} end,R,lists:seq(1,N)).

%run_agents_on/3,4 runs saved agents, unchanged, on Episodes (a list of {StartKey,Bars}) through the path every
%evaluation takes (an exoself in benchmark mode, the simulator's validation phase), Parallel at a time (default: one
%per scheduler), with Overrides in force: any config settings, for example costs, fill rules or episode_isolation.
%Returns [{Agent_Id,{ok,EpStats}|{error,Why}}] in the order of Agent_Ids, one EpStat per episode in the order run,
%each with u_series, the position at every scored bar. For as long as it runs the episodes stand in for the validation
%set, with the drift of their own scored bars taken out of the score (episodes:set_drift/2); money is unaffected by
%that. The cost per position is validation_spread if Overrides sets it, account_spread otherwise.
%Like every replay, an agent whose sensors are all unconnected is repaired by exoself:link_Sensors/4 first.
run_agents_on(Agent_Ids,Episodes,Overrides)->
	run_agents_on(Agent_Ids,Episodes,Overrides,erlang:system_info(schedulers_online)).
run_agents_on(Agent_Ids,Episodes,Overrides,Parallel)->
	case ets:whereis(eval_stats) of
		undefined -> ets:new(eval_stats,[set,public,named_table]);
		_ -> ok
	end,
	Costs = [{validation_spread,proplists:get_value(validation_spread,Overrides,undefined)}],
	with_config(Costs++proplists:delete(validation_spread,Overrides),fun()->
		Table = config:primary_currency_pair(),
		ValParams = {Table,config:val_span(),config:episode_bars(),config:episodes_val(),ets:info(Table,size)},
		DriftParams = {Table,config:val_span(),ets:info(Table,size),config:episode_bars(),config:episodes_val(),config:episode_warmup_bars(),config:episode_anchor_step()},
		Mu = episodes:set_drift(Table,Episodes),
		with_config([{validation_series,true},{episodes_validation_set,Episodes},{episodes_validation_params,ValParams},
			{{episodes_drift,validation},{DriftParams,Mu}}],fun()->
			run_batches(Agent_Ids,erlang:max(1,Parallel),[])
		end)
	end).

	run_batches([],_Parallel,Acc)->
		lists:reverse(Acc);
	run_batches(Agent_Ids,Parallel,Acc)->
		{Batch,Rest} = lists:split(erlang:min(Parallel,length(Agent_Ids)),Agent_Ids),
		Started = [begin
			PId = exoself:start(Agent_Id,self(),benchmark),
			{Agent_Id,PId,erlang:monitor(process,PId)}
		end || Agent_Id <- Batch],
		Results = [{Agent_Id,await_benchmark(PId,MRef)} || {Agent_Id,PId,MRef} <- Started],
		run_batches(Rest,Parallel,lists:reverse(Results)++Acc).

	await_benchmark(PId,MRef)->
		receive
			{PId,benchmark_complete,_Specie_Id,_Fitness,_Cycles,_Time}->
				erlang:demonitor(MRef,[flush]),
				case ets:take(eval_stats,{PId,validation}) of
					[{_Key,_F,EpStats}] -> {ok,EpStats};
					[] -> {error,no_statistics}
				end;
			{PId,benchmark_stalled,_Specie_Id}->
				erlang:demonitor(MRef,[flush]),
				{error,stalled};
			{'DOWN',MRef,process,PId,Reason}->
				{error,{exoself_down,Reason}}
		end.

%population_agents/1: every agent of a saved population, family by family.
population_agents(Population_Id)->
	P = genotype:dirty_read({population,Population_Id}),
	lists:append([(genotype:dirty_read({specie,Specie_Id}))#specie.agent_ids || Specie_Id <- P#population.specie_ids]).

%census/1 counts what the agents of a saved population are made of: the architecture their constraint allows, their
%recurrent links (a neuron's ro_ids), the encoding of their price lists (fx_PLT parameters [TF,N] = level, [TF,N,vol] =
%vol), and which sensors they carry and which of those feed a neuron. Prints one block and returns it as a map.
census(Population_Id)->
	Agents = [genotype:dirty_read({agent,Agent_Id}) || Agent_Id <- population_agents(Population_Id)],
	Rows = [begin
		Cx = genotype:dirty_read({cortex,A#agent.cx_id}),
		Ns = [genotype:dirty_read({neuron,N_Id}) || N_Id <- Cx#cortex.neuron_ids],
		Ss = [genotype:dirty_read({sensor,S_Id}) || S_Id <- Cx#cortex.sensor_ids],
		#{architecture=>(A#agent.constraint)#constraint.connection_architecture,
		  neurons=>length(Ns),
		  layers=>length(A#agent.pattern),
		  recurrent_links=>lists:sum([length(N#neuron.ro_ids) || N <- Ns]),
		  self_links=>length([1 || N <- Ns, lists:member(N#neuron.id,N#neuron.output_ids)]),
		  neuron_kinds=>lists:usort([{N#neuron.af,N#neuron.aggr_f,element(1,N#neuron.pf)} || N <- Ns]),
		  plt_encodings=>lists:usort([case S#sensor.parameters of [_,_,vol] -> vol; _ -> level end || S <- Ss, S#sensor.name == fx_PLT]),
		  sensors=>lists:usort([S#sensor.name || S <- Ss]),
		  connected=>lists:usort([S#sensor.name || S <- Ss, S#sensor.fanout_ids =/= []])}
	end || A <- Agents, is_record(A,agent)],
	Count = fun(Key)-> lists:foldl(fun(Row,M)-> lists:foldl(fun(V,M2)-> maps:update_with(V,fun(C)-> C+1 end,1,M2) end,M,listify(maps:get(Key,Row))) end,#{},Rows) end,
	Summary = #{
		agents=>length(Rows),
		architecture=>Count(architecture),
		with_recurrent_link=>length([1 || #{recurrent_links:=R} <- Rows, R > 0]),
		recurrent_links_mean=>mean_of(recurrent_links,Rows),
		self_links_mean=>mean_of(self_links,Rows),
		neurons_mean=>mean_of(neurons,Rows),
		neurons_max=>lists:max([0|[N || #{neurons:=N} <- Rows]]),
		layers=>Count(layers),
		plt_encoding=>Count(plt_encodings),
		sensors_present=>Count(sensors),
		sensors_connected=>Count(connected),
		neuron_kinds=>Count(neuron_kinds)
	},
	io:format("~nCENSUS ~p~n~p~n",[Population_Id,Summary]),
	Summary.

	listify(L) when is_list(L)-> L;
	listify(X)-> [X].

	mean_of(Key,Rows)->
		case Rows of
			[] -> undefined;
			_ -> functions:avg([maps:get(Key,Row) || Row <- Rows])
		end.

%order_effect/2,3 measures how much the score of saved agents depended on the order of their episodes while the network
%was not reset between episodes (config:episode_isolation/0 off: how every run before REVIEW.md 8.23 was scored).
%NEpisodes train episodes (a fixed draw) are run in the order drawn, a second time in the same order (what timing alone
%changes), reversed, and rotated by half; then in the order drawn and reversed with isolation on. Per agent and episode
%the money after cost is compared between runs. Prints a summary and returns it as a list of {Comparison,Map}.
order_effect(Population_Id,NEpisodes)->
	order_effect(population_agents(Population_Id),NEpisodes,[]).
order_effect(Agent_Ids,NEpisodes,Overrides)->
	Table = config:primary_currency_pair(),
	{[Drawn],_} = episodes:sample(Table,config:train_span(),[NEpisodes],config:episode_bars(),rand:seed_s(exsss,{61,67,71})),
	{Front,Back} = lists:split(NEpisodes div 2,Drawn),
	Runs = [{drawn,false,Drawn},{repeat,false,Drawn},{reversed,false,lists:reverse(Drawn)},{rotated,false,Back++Front},
		{drawn,true,Drawn},{reversed,true,lists:reverse(Drawn)}],
	Results = [begin
		T0 = erlang:monotonic_time(millisecond),
		R = by_episode(run_agents_on(Agent_Ids,Episodes,[{episode_isolation,Isolation}|Overrides])),
		io:format("  run ~p (isolation ~p): ~.1f s~n",[Name,Isolation,(erlang:monotonic_time(millisecond)-T0)/1000]),
		{{Name,Isolation},R}
	end || {Name,Isolation,Episodes} <- Runs],
	Get = fun(Key)-> element(2,lists:keyfind(Key,1,Results)) end,
	Comparisons = [
		{"same order twice, isolation off (timing alone)",{drawn,false},{repeat,false}},
		{"drawn against reversed, isolation off",{drawn,false},{reversed,false}},
		{"drawn against rotated by half, isolation off",{drawn,false},{rotated,false}},
		{"isolation off against on, order drawn",{drawn,false},{drawn,true}},
		{"drawn against reversed, isolation on",{drawn,true},{reversed,true}}
	],
	io:format("~nEpisode order effect: ~p agents, ~p train episodes of ~p bars, cost ~p per position~n",
		[length(Agent_Ids),NEpisodes,config:episode_bars(),config:account_spread()]),
	io:format("~-50s ~8s ~10s ~10s ~10s ~10s ~9s ~8s~n",["comparison","agents","ep_changed","mean|d|pip","max|d|pip","mean|dfit|","rho_fit","top_kept"]),
	[begin
		Summary = compare_runs(Get(K1),Get(K2)),
		io:format("~-50s ~8w ~10s ~10s ~10s ~10s ~9s ~8s~n",[Label,maps:get(agents,Summary),fmt(maps:get(episodes_changed,Summary)),
			fmt(maps:get(mean_abs_pips,Summary)),fmt(maps:get(max_abs_pips,Summary)),fmt(maps:get(mean_abs_fitness,Summary)),
			fmt(maps:get(rho_fitness,Summary)),fmt(maps:get(top_half_kept,Summary))]),
		{Label,Summary}
	end || {Label,K1,K2} <- Comparisons].

	%by_episode/1: per agent, its fitness and a map from episode start to its money after cost (pips) and z.
	by_episode(AgentResults)->
		[{Agent_Id,fitness_functions:aggregate_episodes(EpStats),
		  maps:from_list([{maps:get(start,E),{pips(E),maps:get(z,E)}} || E <- EpStats])}
			|| {Agent_Id,{ok,EpStats}} <- AgentResults].

	%compare_runs/2: what changed between two runs of the same agents on the same episodes.
	%	episodes_changed   share of agent-episodes whose money differs by more than 1e-6 pips
	%	mean_abs_pips      mean absolute change of an episode's money, in pips, over all agent-episodes
	%	mean_abs_fitness   mean absolute change of the agent's fitness (alpha_t), over the agents eligible in both runs
	%	rho_fitness        rank correlation of those fitnesses between the two runs
	%	top_half_kept      share of the better half by fitness in the first run that is also in the better half in the second
	compare_runs(R1,R2)->
		Pairs = [{F1,M1,F2,M2} || {Agent_Id,F1,M1} <- R1, {Agent_Id2,F2,M2} <- R2, Agent_Id == Agent_Id2],
		Deltas = lists:append([[abs(element(1,maps:get(Start,M1))-element(1,maps:get(Start,M2))) || Start <- maps:keys(M1), maps:is_key(Start,M2)] || {_,M1,_,M2} <- Pairs]),
		Floor = fitness_functions:episode_floor()/2,
		Eligible = [{F1,F2} || {F1,_,F2,_} <- Pairs, F1 > Floor, F2 > Floor],
		{Fs1,Fs2} = lists:unzip(Eligible),
		Half = length(Eligible) div 2,
		Top = fun(Fs)-> sets:from_list(lists:sublist([I || {_,I} <- lists:reverse(lists:sort(lists:zip(Fs,lists:seq(1,length(Fs)))))],Half)) end,
		#{
			agents=>length(Pairs),
			episodes_changed=>case Deltas of [] -> undefined; _ -> length([1 || D <- Deltas, D > 1.0e-6])/length(Deltas) end,
			mean_abs_pips=>case Deltas of [] -> undefined; _ -> functions:avg(Deltas) end,
			max_abs_pips=>case Deltas of [] -> undefined; _ -> lists:max(Deltas) end,
			mean_abs_fitness=>case Eligible of [] -> undefined; _ -> functions:avg([abs(A-B) || {A,B} <- Eligible]) end,
			rho_fitness=>spearman(Fs1,Fs2),
			top_half_kept=>case Half of 0 -> undefined; _ -> sets:size(sets:intersection(Top(Fs1),Top(Fs2)))/Half end,
			eligible_both=>length(Eligible)
		}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Agents on validation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%evaluate_agent/1 runs one agent once on the validation episodes and compares it with ?SHIFTS copies of itself whose
%exposure series has been rotated in time. Rotation keeps the number of trades, the holding times and the long/short
%split exactly, and destroys only the timing, so the p-value answers: "would the same activity with no information have
%done as well?". Returns a map; prints nothing.
evaluate_agent(Agent_Id)->
	evaluate_agent(Agent_Id,?SHIFTS).
evaluate_agent(Agent_Id,Shifts)->
	case ets:whereis(eval_stats) of
		undefined -> ets:new(eval_stats,[set,public,named_table]);
		_ -> ok
	end,
	%The shift test reads every episode's position at each bar. A population's own validation does not keep those
	%(config:validation_series/0: about 12 MB per agent, and nothing in a run reads them), so they are switched on for
	%this evaluation and put back afterwards.
	SeriesBefore = config:validation_series(),
	config:set(validation_series,true),
	Agent_PId = exoself:start(Agent_Id,self(),benchmark),
	%No time limit on the evaluation as a whole: the exoself reports a network that stops completing bars
	%(exoself:give_up/3), and the monitor covers an exoself that dies.
	MRef = erlang:monitor(process,Agent_PId),
	Result = receive
		{Agent_PId,benchmark_complete,_Specie_Id,Fitness,_Cycles,_Time}->
			EpStats = case ets:take(eval_stats,{Agent_PId,validation}) of
				[{_Key,_Fitness,Stats}] -> Stats;
				[] -> []
			end,
			Sum = fitness_functions:episode_summary(EpStats),
			{P,Percentile} = shift_test(EpStats,episodes:ensure_validation_set(),Shifts),
			Sum#{agent_id=>Agent_Id,validation_fitness=>Fitness,p_value=>P,beats_random_pct=>Percentile,t=>timing_t(Sum)};
		{Agent_PId,benchmark_stalled,_Specie_Id}->
			{error,stalled};
		{'DOWN',MRef,process,Agent_PId,Reason}->
			{error,{exoself_down,Reason}}
	end,
	erlang:demonitor(MRef,[flush]),
	config:set(validation_series,SeriesBefore),
	Result.

%shift_test/3 returns {PValue,Percentile} for the mean timing z of EpStats (which must carry u_series, i.e. come from a
%validation run) against Shifts rotated copies.
shift_test([],_Episodes,_Shifts)->
	{1.0,0.0};
shift_test(EpStats,Episodes,Shifts)->
	Table = config:primary_currency_pair(),
	Warmup = config:episode_warmup_bars(),
	Series = [begin
		Closes = episodes:closes(Table,Episode),
		Returns = lists:nthtail(Warmup,deltas(Closes)),
		U = maps:get(u_series,EpStat),
		N = length(U),
		{list_to_tuple(U),list_to_tuple(lists:sublist(Returns,N)),N,maps:get(cost,EpStat),maps:get(mu,EpStat,0.0)}
	end || {EpStat,Episode} <- lists:zip(EpStats,lists:sublist(Episodes,length(EpStats))), is_list(maps:get(u_series,EpStat))],
	shift_result(Series,Shifts).

%shift_result/2: no episode carried its positions (config:validation_series/0 was off when it ran), so nothing to test.
shift_result([],_Shifts)->
	{1.0,0.0};
shift_result(Series,Shifts)->
	Observed = functions:avg([series_z(U,R,N,Cost,Mu,0) || {U,R,N,Cost,Mu} <- Series]),
	Null = [functions:avg([series_z(U,R,N,Cost,Mu,rand:uniform(erlang:max(1,N-1))) || {U,R,N,Cost,Mu} <- Series]) || _ <- lists:seq(1,Shifts)],
	AtLeast = length([1 || X <- Null, X >= Observed]),
	{(1+AtLeast)/(1+Shifts),100*(Shifts-AtLeast)/Shifts}.

	deltas([A,B|Rest])-> [B-A|deltas([B|Rest])];
	deltas(_)-> [].

	%z of exposure series U rotated by Shift bars against returns R (same formula as fx:ep_stat/4). The statistic has to
	%be z, not alpha: an agent that is only in the market during volatile bars has a wide alpha, and its rotated copies,
	%which mostly land on quiet bars, have a narrow one, so comparing alphas would call volatility timing a directional
	%edge. z is rescaled by the variance each copy was actually exposed to.
	series_z(_U,_R,0,_Cost,_Mu,_Shift)->
		0.0;
	series_z(U,R,N,Cost,Mu,Shift)->
		{SumUR,SumU,SumR2,SumU2R2} = lists:foldl(fun(I,{AUR,AU,AR2,AU2R2})->
			Ui = element(((I-1+Shift) rem N)+1,U),
			Ri = element(I,R),
			{AUR+Ui*Ri,AU+Ui,AR2+Ri*Ri,AU2R2+Ui*Ui*Ri*Ri}
		end,{0.0,0,0.0,0.0},lists:seq(1,N)),
		ActiveVar = SumU2R2,
		case ActiveVar > 1.0e-9*SumR2 of
			true -> (SumUR - Mu*SumU - Cost)/math:sqrt(ActiveVar);
			false -> 0.0
		end.

%evaluate_population/1 prints the validation result of every agent of a population, best training fitness first.
evaluate_population(Population_Id)->
	P = genotype:dirty_read({population,Population_Id}),
	Agent_Ids = lists:append([(genotype:dirty_read({specie,Specie_Id}))#specie.agent_ids || Specie_Id <- P#population.specie_ids]),
	Agents = lists:reverse(lists:sort([{(genotype:dirty_read({agent,Agent_Id}))#agent.fitness,Agent_Id} || Agent_Id <- Agent_Ids])),
	io:format("~n~-12s ~-12s ~10s ~8s ~10s ~8s ~8s ~12s~n",["train_fit","val_fit","val_alpha","timing_t","entries/ep","active","p_value","beats_rand_%"]),
	Results = [begin
		R = evaluate_agent(Agent_Id),
		case R of
			{error,Why} ->
				io:format("~-12s not evaluated: ~p~n",[fmt(TrainFitness),Why]);
			_ ->
				io:format("~-12s ~-12s ~10.4f ~8.2f ~10.1f ~8.3f ~8.4f ~12.1f~n",
					[fmt(TrainFitness),fmt(maps:get(validation_fitness,R)),maps:get(mean_alpha,R),maps:get(t,R),
					 maps:get(entries,R)/erlang:max(1,maps:get(episodes,R)),maps:get(mean_active,R),maps:get(p_value,R),maps:get(beats_random_pct,R)])
		end,
		{TrainFitness,R}
	end || {TrainFitness,Agent_Id} <- Agents],
	%Clones and near-clones behave identically, so significance is counted over distinct behaviours, not agents.
	Distinct = maps:values(maps:from_list([{{maps:get(entries,R),maps:get(validation_fitness,R)},{T,R}} || {T,R} <- Results, is_map(R)])),
	Eligible = [{T,R} || {T,R} <- Distinct, fitness_functions:eligible(R)],
	io:format("agents=~p  distinct behaviours=~p  eligible on validation=~p  spearman(train fitness, validation fitness) over those=~s  with p<0.05: ~p (expected by chance: ~.1f)~n",
		[length(Results),length(Distinct),length(Eligible),
		 fmt(spearman([T || {T,_} <- Eligible],[maps:get(validation_fitness,R) || {_,R} <- Eligible])),
		 length([1 || {_,R} <- Eligible, maps:get(p_value,R) < 0.05]),0.05*length(Eligible)]),
	Results.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Generation summary %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%generation_summary/2 reads the ledger the exoselves filled during the generation and writes one line with:
%	fitness            best and median score fitness, and the best tuning fitness for comparison
%	eligible           agents that traded enough to be scored on alpha
%	never_trade, holders   the two attractors: no entry at all, or one side held >90% of the time in most episodes
%	entries_per_ep     median number of positions opened per episode
%	neurons            mean and largest network size
%	active             mean share of the market's variance the agents' changing positions were exposed to
%	rho_tune_score     rank agreement between tuning fitness and score fitness (how much tuning carries over to new data)
%	rho_score_val      rank agreement between score fitness and validation fitness (does selection generalise?)
%	val_t_all, val_t_top           mean validation timing t of the agents below and of the better half of them by score fitness
%	val_alpha_all, val_alpha_top   the same for validation alpha
%	stalls             agents whose network stopped completing bars (exoself:give_up/3), in any phase. An agent that
%	                   stalled before it was scored is not in agents=; one that stalled in validation is, with its score
%	runaways           agents ended because a neuron firing forever was flooding a mailbox; counted like stalls
%	gen_seconds        wall time of the generation
%	ms_per_bar         wall time per bar per agent in the score evaluation, while all agents run at once (grows with network size)
%	survivors          agents that were also scored in the previous generation, i.e. that selection kept
%	survivor_prev      their mean score in the previous generation (high by construction: that is why they were kept)
%	survivor_rescore   their mean score now, on this generation's fresh episodes. If selection keeps skill this stays well
%	                   above newcomer_score; if it keeps luck it falls back to it
%	newcomer_score     mean score of the agents created for this generation
%	rev60_score, rev240_score   fitness of the plain 60-bar and 240-bar reversal rules on this generation's score
%	                   episodes, at the same spread, for reference
%	above_rule         eligible agents whose score is above the better of the two rules
%	rev60_val, rev240_val, val_above_rule   the same on the validation episodes, in generations that validate
%	spread             config:account_spread() in force (round-trip cost in price units, charged on each new position);
%	                   with a config:cost_schedule() it changes every generation
%	positive           eligible agents whose score is above zero. With a cost this is the number that matters: zero means
%	                   "made nothing after cost", so it is the hurdle, not the rules
%	gross_pips_trade   money before cost per position opened, in pips, pooled over the eligible agents. It has to exceed the
%	                   cost per round trip in pips (spread/pip_size) for trading to pay
%	net_pips_ep        the median eligible agent's money after cost per episode, in pips
%	val_spread         cost per round trip on the validation episodes (config:validation_spread(), else the same as spread)
%	val_positive, val_net_pips_ep   the same two numbers on the validation episodes, in generations that validate
%	vm_mb, processes   memory of the Erlang VM in megabytes and its number of processes when the summary is written. Both
%	                   should be level from generation to generation; a steady rise over a long run is a leak
%	memory_evals_avg, memory_evals_max   with config:selection_memory() on: how many evaluations the agents of this generation
%	                   have behind the fitness selection ranks them by (selection_memory.erl), mean and largest. The largest
%	                   is the age in generations of the oldest agent; if it keeps pace with the generation counter, one
%	                   agent has never been displaced
%	rho_score_money, median_alpha_money   at the very end of the line, after the extra baselines (score_money_columns/1)
%Scores (fitness, the rule columns, val_t) have the average drift of their span taken out (fx:ep_stat/4); the pips columns
%are plain money with nothing taken out.
%The two rho values and the val_ columns only use agents that were eligible on both sides of the comparison, so that the
%large negative fitness of ineligible agents cannot create agreement by itself. The survivor columns use eligible agents only.
generation_summary(Population_Id,Generation)->
	Ledger = case ets:whereis(eval_ledger) of
		undefined -> [];
		_ -> ets:tab2list(eval_ledger)
	end,
	%Rows in an order fixed by what was measured, not by the agents' ids (which differ from run to run and decide the
	%order of the ledger table), so that sums of floats come out the same in two runs with the same seeds (8.23).
	IdRows = [IdRow || {_Key,IdRow} <- lists:keysort(1,[{{maps:get(score,Row,undefined),maps:get(score_summary,Row,undefined),
		maps:get(validation,Row,undefined),maps:get(validation_summary,Row,undefined),maps:get(neurons,Row,undefined)},{Agent_Id,Row}}
		|| {Agent_Id,Row} <- Ledger, is_map(Row)])],
	Rows = [Row || {_Agent_Id,Row} <- IdRows],
	Stalls = length([1 || {{stall,_Agent_Id},{stalled,_Phase}} <- Ledger]),
	Runaways = length([1 || {{stall,_Agent_Id},{runaway,_Phase}} <- Ledger]),
	GenSeconds = case config:get_val(episodes_generation_started_ms,undefined) of
		Started when is_integer(Started) -> (erlang:monotonic_time(millisecond)-Started)/1000;
		_ -> undefined
	end,
	case {Rows,Stalls+Runaways} of
		{[],0} ->
			ok;
		{[],_} ->
			%Every agent was given up on before it could be scored.
			config:set(episodes_prev_scores,#{}),
			write_summary(Population_Id,io_lib:format("GEN_SUMMARY | generation=~p | agents=0 | stalls=~p | runaways=~p | gen_seconds=~s",[Generation,Stalls,Runaways,fmt(GenSeconds)]));
		_ ->
			N = length(Rows),
			Scores = [maps:get(score,Row) || Row <- Rows],
			%Tune fitness is undefined when config:episode_tuning() is off.
			Tunes = [Tune || Row <- Rows, Tune <- [maps:get(tune,Row)], is_number(Tune)],
			BestTune = case Tunes of
				[] -> undefined;
				_ -> lists:max(Tunes)
			end,
			ScoreCycles = lists:sum([maps:get(score_cycles,Row,0) || Row <- Rows]),
			MsPerBar = case ScoreCycles of
				0 -> undefined;
				_ -> lists:sum([maps:get(score_time_us,Row,0) || Row <- Rows])/ScoreCycles/1000
			end,
			Prev = case Generation of
				0 -> #{};
				_ -> config:get_val(episodes_prev_scores,#{})
			end,
			EligibleScores = [{Agent_Id,maps:get(score,Row)} || {Agent_Id,Row} <- IdRows, fitness_functions:eligible(maps:get(score_summary,Row))],
			Survivors = [{maps:get(Agent_Id,Prev),Score} || {Agent_Id,Score} <- EligibleScores, maps:is_key(Agent_Id,Prev)],
			Newcomers = [Score || {Agent_Id,Score} <- EligibleScores, not maps:is_key(Agent_Id,Prev)],
			Avg = fun([])-> undefined; (Values)-> functions:avg(Values) end,
			config:set(episodes_prev_scores,maps:from_list(EligibleScores)),
			ScoreSums = [maps:get(score_summary,Row) || Row <- Rows],
			NeverTrade = length([1 || #{entries:=0} <- ScoreSums]),
			Holders = length([1 || #{holder_episodes:=H,episodes:=E,entries:=Entries} <- ScoreSums, Entries > 0, 2*H > E]),
			EntriesPerEp = [Entries/erlang:max(1,E) || #{entries:=Entries,episodes:=E} <- ScoreSums],
			Neurons = [maps:get(neurons,Row) || Row <- Rows],
			ScoreEligible = [Row || Row <- Rows, fitness_functions:eligible(maps:get(score_summary,Row))],
			%Tune fitness is on the same scale as score fitness, so an ineligible tuning result shows as a value near the floor.
			TuneFloor = fitness_functions:episode_floor()/2,
			TuneScore = [Row || Row <- ScoreEligible, is_number(maps:get(tune,Row)), maps:get(tune,Row) > TuneFloor],
			ScoreVal = [Row || Row <- ScoreEligible, maps:get(validation,Row) =/= undefined, fitness_functions:eligible(maps:get(validation_summary,Row))],
			ByScore = [Row || {_,Row} <- lists:reverse(lists:keysort(1,[{maps:get(score,Row),Row} || Row <- ScoreVal]))],
			TopHalf = lists:sublist(ByScore,erlang:max(1,length(ByScore) div 2)),
			ValMean = fun(_Key,[])-> undefined; (Key,Selected)-> functions:avg([maps:get(Key,maps:get(validation_summary,Row)) || Row <- Selected]) end,
			ValT = fun([])-> undefined; (Selected)-> functions:avg([timing_t(maps:get(validation_summary,Row)) || Row <- Selected]) end,
			%For reference: plain reversal rules on the very same episodes, under the same cost and fitness function.
			{Rule60,Rule240} = rule_scores(config:get_val(episodes_score_set,[]),train),
			BestRule = best_rule(Rule60,Rule240),
			AboveRule = case BestRule of
				undefined -> undefined;
				_ -> length([1 || {_Agent_Id,Score} <- EligibleScores, Score > BestRule])
			end,
			{RuleVal60,RuleVal240,ValAboveRule} = case ScoreVal of
				[] ->
					{undefined,undefined,undefined};
				_ ->
					{V60,V240} = validation_rule_scores(),
					case best_rule(V60,V240) of
						undefined -> {V60,V240,undefined};
						BestValRule -> {V60,V240,length([1 || Row <- ScoreVal, maps:get(validation,Row) > BestValRule])}
					end
			end,
			%New columns go at the end so that anything reading the earlier ones by position keeps working.
			Line = io_lib:format("GEN_SUMMARY | generation=~p | agents=~p | best_score=~s | median_score=~s | best_tune=~s | eligible=~p | never_trade=~p | holders=~p | entries_per_ep=~s | active=~s | neurons_avg=~s | neurons_max=~p | rho_tune_score=~s | rho_score_val=~s | val_agents=~p | val_t_all=~s | val_t_top=~s | val_alpha_all=~s | val_alpha_top=~s"
				" | stalls=~p | runaways=~p | gen_seconds=~s | ms_per_bar=~s | survivors=~p | survivor_prev=~s | survivor_rescore=~s | newcomer_score=~s"
				" | rev60_score=~s | rev240_score=~s | above_rule=~s | rev60_val=~s | rev240_val=~s | val_above_rule=~s | spread=~p"
				" | positive=~p | gross_pips_trade=~s | net_pips_ep=~s | val_spread=~p | val_positive=~s | val_net_pips_ep=~s"
				" | vm_mb=~p | processes=~p | memory_evals_avg=~s | memory_evals_max=~s",
				[Generation,N,fmt(lists:max(Scores)),fmt(median(Scores)),fmt(BestTune),
				 length(ScoreEligible),NeverTrade,Holders,fmt(median(EntriesPerEp)),
				 fmt(functions:avg([maps:get(mean_active,S) || S <- ScoreSums])),fmt(functions:avg(Neurons)),lists:max(Neurons),
				 fmt(spearman([maps:get(tune,Row) || Row <- TuneScore],[maps:get(score,Row) || Row <- TuneScore])),
				 fmt(spearman([maps:get(score,Row) || Row <- ScoreVal],[maps:get(validation,Row) || Row <- ScoreVal])),
				 length(ScoreVal),fmt(ValT(ByScore)),fmt(ValT(TopHalf)),fmt(ValMean(mean_alpha,ByScore)),fmt(ValMean(mean_alpha,TopHalf)),
				 Stalls,Runaways,fmt(GenSeconds),fmt(MsPerBar),length(Survivors),
				 fmt(Avg([PrevScore || {PrevScore,_} <- Survivors])),fmt(Avg([Score || {_,Score} <- Survivors])),fmt(Avg(Newcomers)),
				 fmt(Rule60),fmt(Rule240),fmt(AboveRule),fmt(RuleVal60),fmt(RuleVal240),fmt(ValAboveRule),config:account_spread(),
				 length([1 || {_Agent_Id,Score} <- EligibleScores, Score > 0]),
				 fmt(gross_pips_trade([maps:get(score_summary,Row) || Row <- ScoreEligible])),
				 fmt(median([net_pips_ep(maps:get(score_summary,Row)) || Row <- ScoreEligible])),
				 validation_spread(),
				 case ScoreVal of [] -> "na"; _ -> fmt(length([1 || Row <- ScoreVal, maps:get(validation,Row) > 0])) end,
				 fmt(median([net_pips_ep(maps:get(validation_summary,Row)) || Row <- ScoreVal])),
				 erlang:memory(total) div 1048576,erlang:system_info(process_count),
				 case selection_memory:stats() of undefined -> "na"; {MemAvg,_} -> fmt(MemAvg) end,
				 case selection_memory:stats() of undefined -> "na"; {_,MemMax} -> fmt(MemMax) end]),
			%config:extra_baselines/0: four more scripted rules on the same episodes, at the end of the line.
			Extra = case config:extra_baselines() of
				true ->
					extra_columns(extra_baselines(config:get_val(episodes_score_set,[]),train),
						case ScoreVal of [] -> undefined; _ -> validation_extra_baselines() end);
				_ ->
					""
			end,
			write_summary(Population_Id,lists:flatten([Line,Extra,score_money_columns(ScoreEligible)])),
			family_lines(Population_Id,Generation,IdRows,Prev)
	end.

%family_lines/4 writes one FAMILY_SUMMARY line per family when the population has more than one specie
%(config:families/0). family is the specie's place in the population's list, which cloning for the next run keeps. The
%columns are the GEN_SUMMARY quantities of the same names over that family's agents only. Read survivor_rescore first:
%it says whether what this family's selection keeps is still worth something on new episodes.
family_lines(Population_Id,Generation,IdRows,Prev)->
	Specie_Ids = case genotype:dirty_read({population,Population_Id}) of
		P when is_record(P,population) -> P#population.specie_ids;
		_ -> []
	end,
	case Specie_Ids of
		[_,_|_] ->
			[family_line(Population_Id,Generation,Family,[IdRow || IdRow={_Agent_Id,Row} <- IdRows, maps:get(specie,Row,undefined) == Specie_Id],Prev)
				|| {Family,Specie_Id} <- lists:zip(lists:seq(1,length(Specie_Ids)),Specie_Ids)],
			ok;
		_ ->
			ok
	end.

family_line(_Population_Id,_Generation,_Family,[],_Prev)->
	ok;
family_line(Population_Id,Generation,Family,IdRows,Prev)->
	Rows = [Row || {_Agent_Id,Row} <- IdRows],
	Eligible = [{Agent_Id,Row} || {Agent_Id,Row} <- IdRows, fitness_functions:eligible(maps:get(score_summary,Row))],
	EligibleRows = [Row || {_Agent_Id,Row} <- Eligible],
	Scores = [maps:get(score,Row) || Row <- Rows],
	Survivors = [{maps:get(Agent_Id,Prev),maps:get(score,Row)} || {Agent_Id,Row} <- Eligible, maps:is_key(Agent_Id,Prev)],
	Newcomers = [maps:get(score,Row) || {Agent_Id,Row} <- Eligible, not maps:is_key(Agent_Id,Prev)],
	Avg = fun([])-> undefined; (Values)-> functions:avg(Values) end,
	ScoreSums = [maps:get(score_summary,Row) || Row <- Rows],
	Val = [Row || Row <- EligibleRows, maps:get(validation,Row) =/= undefined, fitness_functions:eligible(maps:get(validation_summary,Row))],
	Line = io_lib:format("FAMILY_SUMMARY | generation=~p | family=~p | agents=~p | eligible=~p | best_score=~s | median_score=~s | holders=~p | entries_per_ep=~s | neurons_avg=~s"
		" | survivors=~p | survivor_prev=~s | survivor_rescore=~s | newcomer_score=~s | positive=~p | gross_pips_trade=~s | net_pips_ep=~s"
		" | val_agents=~p | val_positive=~s | val_net_pips_ep=~s",
		[Generation,Family,length(Rows),length(Eligible),fmt(lists:max(Scores)),fmt(median(Scores)),
		 length([1 || #{holder_episodes:=H,episodes:=E,entries:=Entries} <- ScoreSums, Entries > 0, 2*H > E]),
		 fmt(median([Entries/erlang:max(1,E) || #{entries:=Entries,episodes:=E} <- ScoreSums])),
		 fmt(functions:avg([maps:get(neurons,Row) || Row <- Rows])),
		 length(Survivors),fmt(Avg([PrevScore || {PrevScore,_} <- Survivors])),fmt(Avg([Score || {_,Score} <- Survivors])),fmt(Avg(Newcomers)),
		 length([1 || Row <- EligibleRows, maps:get(score,Row) > 0]),
		 fmt(gross_pips_trade([maps:get(score_summary,Row) || Row <- EligibleRows])),
		 fmt(median([net_pips_ep(maps:get(score_summary,Row)) || Row <- EligibleRows])),
		 length(Val),
		 case Val of [] -> "na"; _ -> fmt(length([1 || Row <- Val, maps:get(validation,Row) > 0])) end,
		 fmt(median([net_pips_ep(maps:get(validation_summary,Row)) || Row <- Val]))]),
	write_summary(Population_Id,lists:flatten([Line,score_money_columns(EligibleRows)])).

	%score_money_columns/1 (REVIEW.md 8.26), at the end of GEN_SUMMARY and FAMILY_SUMMARY, over the eligible agents of the
	%generation (or family) on that generation's own score episodes:
	%	rho_score_money     rank correlation between the score selection saw and the money per episode after cost (pips).
	%	                    Near zero or below while the score is high means the score has stopped meaning money (8.25)
	%	median_alpha_money  median over the agents of mean(alpha): money after cost with the drift taken out, in units of
	%	                    the market's movement, whatever the fitness function in force
	score_money_columns(EligibleRows)->
		io_lib:format(" | rho_score_money=~s | median_alpha_money=~s",
			[fmt(spearman([maps:get(score,Row) || Row <- EligibleRows],[net_pips_ep(maps:get(score_summary,Row)) || Row <- EligibleRows])),
			 fmt(median([maps:get(mean_alpha,maps:get(score_summary,Row)) || Row <- EligibleRows]))]).

	%rule_scores/1 runs the 60-bar and the 240-bar reversal rule on Episodes and returns their fitness, computed by the
	%configured fitness function at the configured spread, i.e. on exactly the scale of the agents' fitness.
	rule_scores([],_Span)->
		{undefined,undefined};
	rule_scores(Episodes,Span)->
		list_to_tuple([float(fitness_functions:aggregate_episodes(run({reversal,Bars},Episodes,Span))) || Bars <- [60,240]]).

	%The validation episodes never change within a run, so the rules are only run on them when the set, the spread or the
	%fitness function changes.
	validation_rule_scores()->
		Episodes = episodes:ensure_validation_set(),
		Params = {Episodes,validation_spread(),config:fitness_function(),config:min_trades_per_episode(),config:rollover_rule(),config:late_fill()},
		case config:get_val(episodes_validation_rules,undefined) of
			{Params,Result} ->
				Result;
			_ ->
				Result = rule_scores(Episodes,validation),
				config:set(episodes_validation_rules,{Params,Result}),
				Result
		end.

	%extra_baselines/2 (config:extra_baselines/0): never trading, always long, the 60-bar momentum rule (the mirror of the
	%60-bar reversal rule) and the time-of-day rule (policy(clock)) on Episodes, through the same simulator, fill rules
	%and cost as the agents. Returns [{Name,Fitness,NetPipsPerEpisode}]; flat and always long are ineligible under any
	%trade minimum above 1 and get the floor fitness, so read their pips.
	extra_baselines([],_Span)->
		[{Name,undefined,undefined} || {Name,_Policy} <- extra_policies()];
	extra_baselines(Episodes,Span)->
		[begin
			Stats = run(Policy,Episodes,Span),
			{Name,float(fitness_functions:aggregate_episodes(Stats)),net_pips_ep(fitness_functions:episode_summary(Stats))}
		end || {Name,Policy} <- extra_policies()].

	extra_policies()->
		[{flat,never},{long,always_long},{mom60,{momentum,60}},{clock,clock}].

	validation_extra_baselines()->
		Episodes = episodes:ensure_validation_set(),
		Params = {Episodes,validation_spread(),config:fitness_function(),config:min_trades_per_episode(),config:rollover_rule(),config:late_fill()},
		case config:get_val(episodes_validation_extra,undefined) of
			{Params,Result} ->
				Result;
			_ ->
				Result = extra_baselines(Episodes,validation),
				config:set(episodes_validation_extra,{Params,Result}),
				Result
		end.

	%extra_columns/2: " | flat_score=.. | flat_pips=.. | ..." for the train episodes, then flat_val, flat_val_pips, ...
	%for validation ("na" in generations that do not validate).
	extra_columns(Train,Val)->
		ValRows = case Val of
			undefined -> [{Name,undefined,undefined} || {Name,_,_} <- Train];
			_ -> Val
		end,
		lists:flatten(
			[io_lib:format(" | ~p_score=~s | ~p_pips=~s",[Name,fmt(Score),Name,fmt(Pips)]) || {Name,Score,Pips} <- Train] ++
			[io_lib:format(" | ~p_val=~s | ~p_val_pips=~s",[Name,fmt(Score),Name,fmt(Pips)]) || {Name,Score,Pips} <- ValRows]).

	%The cost per round trip charged on validation episodes (fx:span_spread/1).
	validation_spread()->
		case config:validation_spread() of
			undefined -> config:account_spread();
			Spread -> Spread
		end.

	%Money columns. gross and cost of a summary are in price units per unit of position, summed over its episodes.
	%net_pips_ep/1: money after cost per episode, in pips.
	net_pips_ep(#{episodes:=K,gross:=Gross,cost:=Cost}) when K > 0->
		(Gross-Cost)/K/config:pip_size();
	net_pips_ep(_)->
		0.0.

	%gross_pips_trade/1: money before cost per position opened, in pips, pooled over the given summaries.
	gross_pips_trade(Sums)->
		case lists:sum([maps:get(entries,S) || S <- Sums]) of
			0 -> undefined;
			Entries -> lists:sum([maps:get(gross,S,0.0) || S <- Sums])/Entries/config:pip_size()
		end.

	best_rule(A,B)->
		case [X || X <- [A,B], is_number(X)] of
			[] -> undefined;
			Values -> lists:max(Values)
		end.

	write_summary(Population_Id,Line)->
		qlog:benchmarker(Population_Id,Line),
		log("generation_summary.log","~s | ~p | ~s~n",[timestamp(),Population_Id,Line]),
		io:format("~s~n",[Line]),
		ets:delete_all_objects(eval_ledger),
		ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Trial counter %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%record_trial/2 appends one line per run configuration that will be compared against the validation span. The number of
%lines is the number of trials to correct for when a validation result is later judged (deflated Sharpe ratio, etc.).
record_trial(Label,Details)->
	log("validation_trials.log","~s | ~p | ~s~n",[timestamp(),Label,Details]).

trial_count()->
	case file:read_file(filename:join([log_root(),"Benchmarker","validation_trials.log"])) of
		{ok,Bin} -> length(binary:split(Bin,<<"\n">>,[global,trim_all]));
		_ -> 0
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Small statistics and logging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

median([])->
	undefined;
median(Values)->
	Sorted = lists:sort(Values),
	N = length(Sorted),
	case N rem 2 of
		1 -> lists:nth((N+1) div 2,Sorted);
		0 -> (lists:nth(N div 2,Sorted) + lists:nth(N div 2 + 1,Sorted))/2
	end.

%spearman/2: rank correlation with average ranks for ties; undefined for fewer than 3 points or a constant list.
spearman(Xs,Ys) when length(Xs) < 3; length(Xs) =/= length(Ys) ->
	undefined;
spearman(Xs,Ys)->
	RX = ranks(Xs),
	RY = ranks(Ys),
	MX = functions:avg(RX),
	MY = functions:avg(RY),
	SXY = lists:sum([(X-MX)*(Y-MY) || {X,Y} <- lists:zip(RX,RY)]),
	SXX = lists:sum([(X-MX)*(X-MX) || X <- RX]),
	SYY = lists:sum([(Y-MY)*(Y-MY) || Y <- RY]),
	case (SXX > 0) andalso (SYY > 0) of
		true -> SXY/math:sqrt(SXX*SYY);
		false -> undefined
	end.

	ranks(Values)->
		Indexed = lists:keysort(1,lists:zip(Values,lists:seq(1,length(Values)))),
		Ranked = assign_ranks(Indexed,1,[]),
		[Rank || {_Index,Rank} <- lists:keysort(1,Ranked)].

	assign_ranks([],_Position,Acc)->
		Acc;
	assign_ranks([{Value,_}|_]=Indexed,Position,Acc)->
		{Tied,Rest} = lists:splitwith(fun({V,_})-> V == Value end,Indexed),
		Count = length(Tied),
		Rank = Position + (Count-1)/2,
		assign_ranks(Rest,Position+Count,[{Index,Rank} || {_,Index} <- Tied]++Acc).

fmt(undefined)-> "na";
fmt(V) when is_integer(V)-> integer_to_list(V);
fmt(V) when is_float(V)-> lists:flatten(io_lib:format("~.4f",[V]));
fmt(V)-> lists:flatten(io_lib:format("~p",[V])).

log(File,Format,Args)->
	Path = filename:join([log_root(),"Benchmarker",File]),
	filelib:ensure_dir(Path),
	file:write_file(Path,io_lib:format(Format,Args),[append]).

	log_root()->
		case application:get_env(qlog,log_root_dir) of
			{ok,Dir} -> Dir;
			undefined ->
				case os:getenv("QLOG_ROOT") of
					false -> filename:absname("logs");
					EnvDir -> EnvDir
				end
		end.

	timestamp()->
		{{Y,Mo,D},{H,Mi,S}} = calendar:universal_time(),
		lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",[Y,Mo,D,H,Mi,S])).
