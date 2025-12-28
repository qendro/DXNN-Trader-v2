-module(fitness_postprocessor).
-compile(export_all).
-include("records.hrl").
-define(EFF,0.01). %Efficiency.

none(Agent_Summaries)->
	lists:reverse(lists:sort(Agent_Summaries)).

size_proportional(Agent_Summaries)->
	SDX=lists:reverse(lists:sort([{Fitness/math:pow(TotN,?EFF),{Fitness,TotN,Agent_Id}}||{Fitness,TotN,Agent_Id}<-Agent_Summaries])),
	ProperlySorted_AgentSummaries = [Val || {_,Val}<-SDX],
	ProperlySorted_AgentSummaries.

% Minimal addition: Sort by neuron count first (for size-focused runs), then by fitness
size_first(Agent_Summaries)->
	% Sort by TotN descending, then by Fitness descending
	SDX=lists:reverse(lists:sort([{TotN*1000000.0+Fitness,{Fitness,TotN,Agent_Id}}||{Fitness,TotN,Agent_Id}<-Agent_Summaries])),
	ProperlySorted_AgentSummaries = [Val || {_,Val}<-SDX],
	ProperlySorted_AgentSummaries.