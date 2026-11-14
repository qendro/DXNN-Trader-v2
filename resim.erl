%% Re-simulation of saved agents under other trading conditions (REVIEW.md 8.23).
%%
%% The agents are run again, unchanged, through the simulator (eval_protocol:run_agents_on/4: an exoself in benchmark
%% mode on the named episodes), once per condition. Their stored positions are never re-scored: an agent that reads its
%% own position (fx_Position) or its open profit (fx_Internals) can trade differently when the rule that fills its
%% orders, or the cost it pays, is different.
%%
%%   resim:run(Agents,EpisodeSet,Label)              every condition of conditions/0
%%   resim:run(Agents,EpisodeSet,Label,Options)      Options: #{conditions=>[...], isolation=>true|false, parallel=>N}
%%
%%   Agents      {population,Population_Id} | [Agent_Id]
%%   EpisodeSet  validation                      every validation episode of the config in force (val_span, episodes_val)
%%               {tile,{From,To},MaxK}           up to MaxK non-overlapping episodes spread over the dates (episodes:tile/4)
%%               {sample,{From,To},K,Seed}       K episodes drawn over the dates with a fixed seed (episodes:sample/5)
%%               [{StartKey,Bars}]               these episodes
%%   Condition   #{cost=>Pips, rollover=>Bool, late=>Bool}: cost per position in pips (account_spread and
%%               validation_spread), config:rollover_rule/0, config:late_fill/0.
%%
%% Writes to <log root>/Resim/<Label>/ (logs/Resim/... unless QLOG_ROOT is set):
%%   episodes.csv     one row per condition, agent and episode: money after cost, positions opened, bars in the market,
%%                    bars per position, the largest drawdown inside the episode (all in pips), and checks that the
%%                    money rebuilt bar by bar from the positions equals the simulator's own figure
%%   agents.csv       one row per condition and agent: money per episode with its standard error from day blocks and
%%                    from week blocks (the money of each calendar day, or ISO week, of the file's clock is one
%%                    observation; consecutive episodes of the same days are not independent), t for each, positions
%%                    and bars in the market per episode, bars per position, largest and mean drawdown, eligibility
%%                    and fitness under the config in force
%%   conditions.csv   one row per condition: agents, eligible agents, and over the eligible ones the median money per
%%                    episode, how many make money and how many have a score above zero (GEN_SUMMARY reports the
%%                    median as val_net_pips_ep and the last count as val_positive, over the agents eligible both on
%%                    the generation's score episodes and on validation, when episodes, cost and fill rules are the same)
%% Returns the rows of conditions.csv as maps.
-module(resim).
-export([run/3, run/4, conditions/0, episodes/1, condition_name/1]).
-include("records.hrl").

%conditions/0: costs of 0.25, 0.5 and 1 pip per position, each with the rollover rule on and off and the fill now and
%one bar late: twelve conditions.
conditions()->
	[#{cost=>Cost,rollover=>Rollover,late=>Late} || Cost <- [0.25,0.5,1.0], Rollover <- [true,false], Late <- [false,true]].

condition_name(#{cost:=Cost,rollover:=Rollover,late:=Late})->
	lists:flatten(io_lib:format("cost~s_rollover_~s_fill_~s",[fmt_num(Cost),on_off(Rollover),case Late of true -> "late"; false -> "now" end])).

	on_off(true)-> "on";
	on_off(false)-> "off".

run(Agents,EpisodeSet,Label)->
	run(Agents,EpisodeSet,Label,#{}).
run(Agents,EpisodeSet,Label,Options)->
	Agent_Ids = case Agents of
		{population,Population_Id} -> eval_protocol:population_agents(Population_Id);
		List when is_list(List) -> List
	end,
	Episodes = episodes(EpisodeSet),
	Conditions = maps:get(conditions,Options,conditions()),
	Parallel = maps:get(parallel,Options,erlang:system_info(schedulers_online)),
	Isolation = case maps:get(isolation,Options,config) of
		config -> [];
		Iso -> [{episode_isolation,Iso}]
	end,
	Dir = filename:join([log_root(),"Resim",Label]),
	ok = filelib:ensure_dir(filename:join(Dir,"x")),
	Table = config:primary_currency_pair(),
	%The keys and closes of every episode, read once: the bar-by-bar money, drawdown and day and week blocks need them.
	Paths = maps:from_list([{Start,episode_path(Table,Episode)} || {Start,_Bars}=Episode <- Episodes]),
	EpFile = filename:join(Dir,"episodes.csv"),
	AgFile = filename:join(Dir,"agents.csv"),
	CoFile = filename:join(Dir,"conditions.csv"),
	ok = file:write_file(EpFile,"condition,agent,episode_start,money_pips,positions,bars_in_market,bars_per_position,max_drawdown_pips,gross_pips,cost_pips,rebuilt_matches\n"),
	ok = file:write_file(AgFile,"condition,agent,episodes,money_pips_per_episode,se_day_blocks,t_day,days,se_week_blocks,t_week,weeks,positions_per_episode,bars_in_market_per_episode,bars_per_position,max_drawdown_pips,mean_drawdown_pips,eligible,fitness,errors,alpha_t,alpha_money_t,mean_alpha,alpha_pips_t\n"),
	ok = file:write_file(CoFile,"condition,cost_pips,rollover,late_fill,agents,eligible,median_money_pips_per_episode_eligible,positive_eligible,positive_fitness_eligible,mean_money_pips_per_episode_all,episodes\n"),
	io:format("~nRESIM ~s: ~p agents, ~p episodes (~s .. ~s), ~p conditions, isolation ~p~n",
		[Label,length(Agent_Ids),length(Episodes),describe_first(Episodes),describe_last(Episodes),length(Conditions),
		 case Isolation of [] -> config:episode_isolation(); [{_,I}] -> I end]),
	[begin
		T0 = erlang:monotonic_time(millisecond),
		Name = condition_name(Condition),
		Spread = maps:get(cost,Condition)*config:pip_size(),
		Overrides = [{account_spread,Spread},{validation_spread,Spread},
			{rollover_rule,maps:get(rollover,Condition)},{late_fill,maps:get(late,Condition)}|Isolation],
		AgentRows = lists:append([begin
			Results = eval_protocol:run_agents_on(Batch,Episodes,Overrides,Parallel),
			[agent_row(Name,Agent_Id,Result,Paths,Spread,Overrides,EpFile) || {Agent_Id,Result} <- Results]
		end || Batch <- batches(Agent_Ids,Parallel)]),
		[append(AgFile,agent_csv(Row)) || Row <- AgentRows],
		Eligible = [maps:get(money,Row) || Row <- AgentRows, maps:get(eligible,Row) == true],
		Summary = #{condition=>Name,cost=>maps:get(cost,Condition),rollover=>maps:get(rollover,Condition),late=>maps:get(late,Condition),
			agents=>length(AgentRows),eligible=>length(Eligible),
			median_eligible=>eval_protocol:median(Eligible),
			positive_eligible=>length([1 || M <- Eligible, M > 0]),
			positive_fitness_eligible=>length([1 || Row <- AgentRows, maps:get(eligible,Row) == true, maps:get(fitness,Row) > 0]),
			mean_all=>case [maps:get(money,Row) || Row <- AgentRows, is_number(maps:get(money,Row))] of [] -> undefined; All -> functions:avg(All) end,
			episodes=>length(Episodes)},
		append(CoFile,io_lib:format("~s,~s,~s,~s,~p,~p,~s,~p,~p,~s,~p~n",[Name,fmt_num(maps:get(cost,Condition)),on_off(maps:get(rollover,Condition)),
			on_off(maps:get(late,Condition)),length(AgentRows),length(Eligible),fmt(maps:get(median_eligible,Summary)),
			maps:get(positive_eligible,Summary),maps:get(positive_fitness_eligible,Summary),fmt(maps:get(mean_all,Summary)),length(Episodes)])),
		io:format("  ~-36s agents ~p eligible ~p  median money per episode (eligible) ~s pips  money above zero ~p  score above zero ~p  mean (all) ~s  (~.1f s)~n",
			[Name,length(AgentRows),length(Eligible),fmt(maps:get(median_eligible,Summary)),maps:get(positive_eligible,Summary),maps:get(positive_fitness_eligible,Summary),
			 fmt(maps:get(mean_all,Summary)),(erlang:monotonic_time(millisecond)-T0)/1000]),
		Summary
	end || Condition <- Conditions].

%episodes/1: the episode list a named set stands for, under the config in force (episode_bars and the spans).
episodes(validation)->
	episodes:ensure_validation_set();
episodes({tile,Span,MaxK})->
	episodes:tile(config:primary_currency_pair(),Span,config:episode_bars(),MaxK);
episodes({sample,Span,K,Seed})->
	{[Set],_} = episodes:sample(config:primary_currency_pair(),Span,[K],config:episode_bars(),rand:seed_s(exsss,{Seed,Seed+1,Seed+2})),
	Set;
episodes(List) when is_list(List)->
	List.

	batches([],_N)-> [];
	batches(List,N)->
		{Batch,Rest} = lists:split(erlang:min(N,length(List)),List),
		[Batch|batches(Rest,N)].

%episode_path/2: {Keys,Closes} of the Bars+1 rows an episode spans, as tuples.
episode_path(Table,{StartKey,Bars})->
	{Keys,Closes} = lists:unzip(walk(Table,StartKey,Bars+1,[])),
	{list_to_tuple(Keys),list_to_tuple(Closes)}.

	walk(_Table,_Key,0,Acc)-> lists:reverse(Acc);
	walk(_Table,'$end_of_table',_N,Acc)-> lists:reverse(Acc);
	walk(Table,Key,N,Acc)->
		[Row] = ets:lookup(Table,Key),
		walk(Table,ets:next(Table,Key),N-1,[{Key,element(6,Row)}|Acc]).

%agent_row/7 turns one agent's statistics under one condition into its episode rows (written) and its agent row.
agent_row(Name,Agent_Id,{ok,EpStats},Paths,Spread,_Overrides,EpFile)->
	Pip = config:pip_size(),
	Warmup = config:episode_warmup_bars(),
	Episodes = [begin
		Start = maps:get(start,E),
		U = maps:get(u_series,E),
		{Keys,Closes} = maps:get(Start,Paths),
		{Bars,Entries,Equity,MaxDD,Blocks} = rebuild(U,Keys,Closes,Warmup,Spread),
		Money = (maps:get(gross,E)-maps:get(cost,E))/Pip,
		Matches = (Entries == maps:get(entries,E)) andalso (abs(Equity/Pip - Money) < 1.0e-6),
		Positions = maps:get(entries,E),
		append(EpFile,io_lib:format("~s,~s,~s,~s,~p,~p,~s,~s,~s,~s,~p~n",[Name,agent_str(Agent_Id),key_str(Start),fmt(Money),Positions,Bars,
			fmt(case Positions of 0 -> undefined; _ -> Bars/Positions end),fmt(MaxDD/Pip),fmt(maps:get(gross,E)/Pip),fmt(maps:get(cost,E)/Pip),Matches])),
		#{money=>Money,positions=>Positions,bars=>Bars,max_dd=>MaxDD/Pip,blocks=>Blocks,matches=>Matches}
	end || E <- EpStats],
	K = length(Episodes),
	TotalMoney = lists:sum([maps:get(money,E) || E <- Episodes]),
	Days = merge_blocks([maps:get(day,maps:get(blocks,E)) || E <- Episodes]),
	Weeks = merge_blocks([maps:get(week,maps:get(blocks,E)) || E <- Episodes]),
	Mean = case K of 0 -> undefined; _ -> TotalMoney/K end,
	SEDay = block_se(Days,K,Pip),
	SEWeek = block_se(Weeks,K,Pip),
	Summary = fitness_functions:episode_summary(EpStats),
	TotalPositions = lists:sum([maps:get(positions,E) || E <- Episodes]),
	TotalBars = lists:sum([maps:get(bars,E) || E <- Episodes]),
	#{condition=>Name,agent=>Agent_Id,episodes=>K,money=>Mean,se_day=>SEDay,days=>maps:size(Days),se_week=>SEWeek,weeks=>maps:size(Weeks),
	  positions=>TotalPositions/erlang:max(1,K),bars=>TotalBars/erlang:max(1,K),
	  bars_per_position=>case TotalPositions of 0 -> undefined; _ -> TotalBars/TotalPositions end,
	  max_dd=>lists:max([0.0|[maps:get(max_dd,E) || E <- Episodes]]),
	  mean_dd=>case K of 0 -> undefined; _ -> functions:avg([maps:get(max_dd,E) || E <- Episodes]) end,
	  eligible=>fitness_functions:eligible(Summary),
	  fitness=>fitness_functions:aggregate_episodes(EpStats),
	  %Both t-statistics whatever the fitness in force (REVIEW.md 8.26), and the mean alpha: money in units of the market.
	  alpha_t=>fitness_functions:aggregate_episodes(EpStats,alpha_t),
	  alpha_money_t=>fitness_functions:aggregate_episodes(EpStats,alpha_money_t),
	  mean_alpha=>maps:get(mean_alpha,Summary),
	  alpha_pips_t=>fitness_functions:aggregate_episodes(EpStats,alpha_pips_t),
	  errors=>length([1 || E <- Episodes, maps:get(matches,E) == false])};
agent_row(Name,Agent_Id,Error,_Paths,_Spread,_Overrides,_EpFile)->
	#{condition=>Name,agent=>Agent_Id,episodes=>0,money=>undefined,se_day=>undefined,days=>0,se_week=>undefined,weeks=>0,
	  positions=>undefined,bars=>undefined,bars_per_position=>undefined,max_dd=>undefined,mean_dd=>undefined,
	  eligible=>false,fitness=>undefined,alpha_t=>undefined,alpha_money_t=>undefined,mean_alpha=>undefined,alpha_pips_t=>undefined,errors=>{error,Error}}.

%rebuild/5 replays an episode's money bar by bar from the positions the simulator reported (fx:ep_step/4): the
%position held at scored bar I (row Warmup+I of the episode) earns close(Warmup+I+1) - close(Warmup+I), and a position
%opened there (non-zero and different from the one before; the warm-up is flat) pays Spread. Returns
%{BarsInMarket,Entries,FinalMoney,LargestDrawdown,#{day=>Blocks,week=>Blocks}}, money in price units, the blocks being
%the money of each calendar day and ISO week of the bar's own key.
rebuild(U,Keys,Closes,Warmup,Spread)->
	rebuild(U,Keys,Closes,Warmup,Spread,0,1,0,0,0.0,0.0,0.0,#{},#{}).
rebuild([],_Keys,_Closes,_Warmup,_Spread,_Prev,_I,Bars,Entries,Equity,_Peak,MaxDD,Days,Weeks)->
	{Bars,Entries,Equity,MaxDD,#{day=>Days,week=>Weeks}};
rebuild([P|U],Keys,Closes,Warmup,Spread,Prev,I,Bars,Entries,Equity,Peak,MaxDD,Days,Weeks)->
	Row = Warmup+I,%1-based tuple position of the bar
	R = element(Row+1,Closes) - element(Row,Closes),
	Entry = case (P =/= 0) andalso (P =/= Prev) of true -> 1; false -> 0 end,
	Money = P*R - Entry*Spread,
	U_Equity = Equity + Money,
	U_Peak = erlang:max(Peak,U_Equity),
	{Y,Mo,D,_H,_Mi,_S,_Rate} = element(Row,Keys),
	Day = {Y,Mo,D},
	Week = calendar:iso_week_number(Day),
	rebuild(U,Keys,Closes,Warmup,Spread,P,I+1,Bars+case P of 0 -> 0; _ -> 1 end,Entries+Entry,U_Equity,U_Peak,
		erlang:max(MaxDD,U_Peak-U_Equity),maps:update_with(Day,fun(X)-> X+Money end,Money,Days),maps:update_with(Week,fun(X)-> X+Money end,Money,Weeks)).

	merge_blocks(Maps)->
		lists:foldl(fun(M,Acc)-> maps:fold(fun(K,V,A)-> maps:update_with(K,fun(X)-> X+V end,V,A) end,Acc,M) end,#{},Maps).

	%block_se/3: the standard error of money per episode when each block (day or week) is one observation:
	%sqrt(Blocks) * sd(block money) / Episodes, in pips.
	block_se(Blocks,K,Pip)->
		Values = [V/Pip || V <- maps:values(Blocks)],
		case {length(Values),K} of
			{N,_} when N < 2 -> undefined;
			{_,0} -> undefined;
			{N,_} -> math:sqrt(N)*fitness_functions:sample_std(Values)/K
		end.

agent_csv(Row)->
	#{condition:=Name,agent:=Agent_Id,episodes:=K,money:=Money,se_day:=SEDay,days:=Days,se_week:=SEWeek,weeks:=Weeks,
	  positions:=Positions,bars:=Bars,bars_per_position:=BPP,max_dd:=MaxDD,mean_dd:=MeanDD,eligible:=Eligible,fitness:=Fitness,errors:=Errors,
	  alpha_t:=AlphaT,alpha_money_t:=AlphaMoneyT,mean_alpha:=MeanAlpha,alpha_pips_t:=AlphaPipsT} = Row,
	io_lib:format("~s,~s,~p,~s,~s,~s,~p,~s,~s,~p,~s,~s,~s,~s,~s,~p,~s,~s,~s,~s,~s,~s~n",
		[Name,agent_str(Agent_Id),K,fmt(Money),fmt(SEDay),fmt(ratio(Money,SEDay)),Days,fmt(SEWeek),fmt(ratio(Money,SEWeek)),Weeks,
		 fmt(Positions),fmt(Bars),fmt(BPP),fmt(MaxDD),fmt(MeanDD),Eligible,fmt(Fitness),fmt_errors(Errors),fmt(AlphaT),fmt(AlphaMoneyT),fmt(MeanAlpha),fmt(AlphaPipsT)]).

	ratio(A,B) when is_number(A), is_number(B), B > 0 -> A/B;
	ratio(_,_)-> undefined.

	fmt_errors(0)-> "0";
	fmt_errors(N) when is_integer(N)-> integer_to_list(N);
	fmt_errors(Other)-> "\"" ++ lists:flatten(io_lib:format("~w",[Other])) ++ "\"".

key_str({Y,Mo,D,H,Mi,_S,_Rate})->
	lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B",[Y,Mo,D,H,Mi])).

agent_str(Agent_Id)->
	"\"" ++ lists:flatten(io_lib:format("~w",[Agent_Id])) ++ "\"".

describe_first([])-> "none";
describe_first([E|_])-> episodes:describe(E).
describe_last([])-> "none";
describe_last(Es)-> episodes:describe(lists:last(Es)).

append(File,Data)->
	ok = file:write_file(File,Data,[append]).

fmt(undefined)-> "";
fmt(V) when is_integer(V)-> integer_to_list(V);
fmt(V) when is_float(V)-> lists:flatten(io_lib:format("~.6f",[V]));
fmt(V)-> lists:flatten(io_lib:format("~p",[V])).

fmt_num(V) when is_integer(V)-> integer_to_list(V);
fmt_num(V)-> float_to_list(V,[{decimals,2},compact]).

log_root()->
	case application:get_env(qlog,log_root_dir) of
		{ok,Dir} -> Dir;
		undefined ->
			case os:getenv("QLOG_ROOT") of
				false -> filename:absname("logs");
				EnvDir -> EnvDir
			end
	end.
