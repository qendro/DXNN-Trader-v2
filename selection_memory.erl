%% Selection that remembers earlier generations (episode mode, config:selection_memory() > 0).
%%
%% Why. An agent's score on one generation's episodes is its skill plus the luck of that draw, and the luck is the
%% larger part (fourth trial: the whole population was above zero in 23 generations of 40 and below it in 11). Agents
%% of one family share that luck, so a new family that is really better than the old one is still below it in two
%% generations of five, and while it is the smaller family one such generation removes all of it. Survivors are scored
%% again on fresh episodes every generation, so the information to do better is already there.
%%
%% What. Every generation each eligible agent gets a relative score: its score minus the median score of the eligible
%% agents of that generation (of its own family, when the population has several: config:families/0). All agents of a generation run the same episodes, so the luck they share cancels in that
%% difference. What selection ranks by is a decaying average of an agent's relative scores,
%%
%%     sum(g)    = W * sum(g-1) + relative(g)
%%     weight(g) = W * weight(g-1) + 1
%%     fitness   = sum(g) / (weight(g) + Prior)
%%
%% with W = config:selection_memory() (0.85: about the last 7 generations count) and Prior =
%% config:selection_memory_prior() (1: a newcomer's single result counts half, so it must be twice as far above the
%% median as a proven agent to rank beside it). The decay matters: a relative score is relative to the population of its
%% day, and old ones say little about today's.
%%
%% What it does not do. It does not use the level of the score (whether the population as a whole makes money): ranking
%% never did. The generation summary keeps reporting the raw scores; only #agent.fitness, which selection reads, holds
%% the remembered value. Agents that are not eligible keep the floor fitness fitness_functions gave them and lose their
%% history. The table belongs to the population monitor, so each run starts with an empty memory (agents get new ids at a
%% run boundary anyway).
-module(selection_memory).
-export([update/0, smooth/4, stats/0, lookup/1]).
-include("records.hrl").

-define(TABLE,selection_memory).

%update/0 is called by the population monitor when every agent of the generation has reported: after the exoselves
%have stored their score fitness, before the generation summary empties the ledger, and before selection.
update()->
	W = config:selection_memory(),
	case is_number(W) andalso (W > 0) andalso (ets:whereis(eval_ledger) =/= undefined) of
		false ->
			off;
		true ->
			case ets:whereis(?TABLE) of
				undefined -> ets:new(?TABLE,[set,public,named_table]);
				_ -> ok
			end,
			Rows = [{Agent_Id,maps:get(score,Row),maps:get(specie,Row,undefined)} || {Agent_Id,Row} <- ets:tab2list(eval_ledger), is_map(Row),
				is_number(maps:get(score,Row)), fitness_functions:eligible(maps:get(score_summary,Row))],
			Eligible = [{Agent_Id,Score} || {Agent_Id,Score,_Specie} <- Rows],
			Prior = config:selection_memory_prior(),
			%The reference is the median of the agent's own family (specie). Selection ranks inside a family, and against
			%a median of the whole population every agent of a strong family would sit above the reference and every
			%agent of a weak one below it. The prior then ranks proven agents over newcomers in the first and newcomers
			%over proven agents in the second, whatever their merit. With one family this is the median of the generation.
			References = maps:map(fun(_Specie,Scores)-> eval_protocol:median(Scores) end,
				lists:foldl(fun({_Agent_Id,Score,Specie},M)-> maps:update_with(Specie,fun(L)-> [Score|L] end,[Score],M) end,#{},Rows)),
			[begin
				{Sum0,Weight0,Count0} = lookup(Agent_Id),
				{Sum,Weight,Fitness} = smooth(Score-maps:get(Specie,References),{Sum0,Weight0},W,Prior),
				ets:insert(?TABLE,{Agent_Id,Sum,Weight,Count0+1}),
				case genotype:dirty_read({agent,Agent_Id}) of
					A when is_record(A,agent) -> genotype:write(A#agent{fitness=Fitness});
					_ -> ok
				end
			end || {Agent_Id,Score,Specie} <- Rows],
			%Everything else is forgotten: agents that no longer exist and agents that were not eligible this generation.
			Live = sets:from_list([Agent_Id || {Agent_Id,_Score} <- Eligible]),
			[ets:delete(?TABLE,Agent_Id) || {Agent_Id,_,_,_} <- ets:tab2list(?TABLE), not sets:is_element(Agent_Id,Live)],
			ok
	end.

%smooth/4 adds one relative score to a history {Sum,Weight} and returns {NewSum,NewWeight,Fitness}.
smooth(Relative,{Sum0,Weight0},W,Prior)->
	Sum = W*Sum0 + Relative,
	Weight = W*Weight0 + 1,
	{Sum,Weight,Sum/(Weight+Prior)}.

%lookup/1 returns {Sum,Weight,Evaluations} of an agent, zeros if it has no history.
lookup(Agent_Id)->
	case ets:whereis(?TABLE) of
		undefined ->
			{0.0,0.0,0};
		_ ->
			case ets:lookup(?TABLE,Agent_Id) of
				[{_,Sum,Weight,Count}] -> {Sum,Weight,Count};
				[] -> {0.0,0.0,0}
			end
	end.

%stats/0 returns {mean, largest} number of evaluations remembered per agent, or undefined when the memory is off or empty.
stats()->
	case ets:whereis(?TABLE) of
		undefined ->
			undefined;
		_ ->
			case [Count || {_,_,_,Count} <- ets:tab2list(?TABLE)] of
				[] -> undefined;
				Counts -> {lists:sum(Counts)/length(Counts),lists:max(Counts)}
			end
	end.
