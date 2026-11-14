
-module(selection_algorithm).
-compile(export_all).
-include("records.hrl").

competition(ProperlySorted_AgentSummaries,NeuralEnergyCost,PopulationLimit,Survival_Percentage)->
	TotSurvivors = round(length(ProperlySorted_AgentSummaries)*Survival_Percentage),
	Valid_AgentSummaries = lists:sublist(ProperlySorted_AgentSummaries,TotSurvivors),
	Invalid_AgentSummaries = ProperlySorted_AgentSummaries -- Valid_AgentSummaries,
	{_,_,Invalid_AgentIds} = lists:unzip3(Invalid_AgentSummaries),
	[genotype:delete_Agent(Agent_Id) || Agent_Id <- Invalid_AgentIds],

	io:format("Valid_AgentSummaries:~p~n",[Valid_AgentSummaries]),
	io:format("Invalid_AgentSummaries:~p~n",[Invalid_AgentSummaries]),
	TopAgentSummaries = lists:sublist(Valid_AgentSummaries,3),
	{_TopFitnessList,_TopTotNs,TopAgent_Ids} = lists:unzip3(TopAgentSummaries),
	io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),

	%Offspring allotment uses fitness relative to the worst agent of the specie. Raw fitness values sit on a large
	%positive offset (e.g. equity ~300 +/- a few dollars), so without the shift every agent gets almost the same
	%allotment and the 1/TotNeurons term decides selection instead of P&L.
	{AlotmentsP,NextGenSize_Estimate} = calculate_alotments(shift_fitness(Valid_AgentSummaries,ProperlySorted_AgentSummaries),NeuralEnergyCost,[],0),
	Normalizer = case NextGenSize_Estimate of
		0 -> 1.0;  % Prevent division by zero
		_ -> abs(NextGenSize_Estimate)/PopulationLimit
	end,
	qlog:benchmarker(self,io_lib:format("Population size normalizer=~p",[Normalizer])),
	io:format("Population size normalizer:~p~n",[Normalizer]),
	NewGenAgent_Ids = gather_survivors(AlotmentsP,Normalizer,PopulationLimit,[]),
	{NewGenAgent_Ids,TopAgent_Ids}.
%The competition/4 is part of the selection algorithm dubbed "competition". The function accepts a Survival_Percentage parameter that determines what fraction of the population survives to the next generation. The function first executes calculate_alotments/4 to calculate the number of offspring alloted for each agent in the Sorted_AgentSummaries list. The function then calculates the Normalizer value, which is used then used to proportionalize the alloted number of offspring to each agent, to ensure that the final specie size is within PopulationLimit. The function then drops into the gather_survivors/3 function which, using the normalized offspring allotment values, creates the actual mutant offspring.

	%shift_fitness/2 rescales survivor fitness to (Fitness - MinFitness) + Eps, where MinFitness is taken over the whole
	%specie (survivors and culled), so the weakest survivor still gets a small positive share. If every agent has the
	%same fitness the summaries are returned unchanged. The overall scale cancels out in the normalizer of competition/4.
	shift_fitness(Valid_AgentSummaries,[])->
		Valid_AgentSummaries;
	shift_fitness(Valid_AgentSummaries,All_AgentSummaries)->
		Fitnesses = [F || {F,_,_} <- All_AgentSummaries],
		MinF = lists:min(Fitnesses),
		Range = lists:max(Fitnesses) - MinF,
		case Range > 0 of
			true ->
				Eps = 0.01*Range,
				[{F-MinF+Eps,TotN,Id} || {F,TotN,Id} <- Valid_AgentSummaries];
			false ->
				Valid_AgentSummaries
		end.

	calculate_alotments([{Fitness,TotNeurons,Agent_Id}|Sorted_AgentSummaries],NeuralEnergyCost,Acc,NewPopAcc)->
		NeuralAlotment = Fitness/NeuralEnergyCost,
		MutantAlotment = NeuralAlotment/TotNeurons,
		U_NewPopAcc = NewPopAcc+MutantAlotment,
		calculate_alotments(Sorted_AgentSummaries,NeuralEnergyCost,[{MutantAlotment,Fitness,TotNeurons,Agent_Id}|Acc],U_NewPopAcc);
	calculate_alotments([],_NeuralEnergyCost,Acc,NewPopAcc)->
		%io:format("NewPopAcc:~p~n",[NewPopAcc]),
		{Acc,NewPopAcc}.
%The calculate_alotments/4 function accepts the AgentSummaries list and for each agent, using the NeuralEnergyCost, calcualtes how many offspring that agent can produce by using the agent's Fitness, TotNEurons, and NeuralEnergyCost values. The function first calculates how many neurons the agent is alloted, based on the agent's fitness and the cost of each neuron (which itself was calculated based on the average performance of the population). From the number of neurons alloted to the agent, the function then calculates how many offspring the agent should be alloted, by deviding the agent's NN size by the number of neurons it is alloted. The function also keeps track of how many offspring will be created from all these agents in general, by adding up all the offspring alotements. The calcualte_alotments/4 function does this for each tuple in the AgentSummaries, and then returns the calculated alotment list and NewPopAcc to the caller.

	%With config:evolution_seed() set the offspring are made one after the other in this process, whose random generator
	%config:seed_rand/1 has seeded: in parallel each would draw from a generator of its own seeded from the clock, and
	%the order in which they finish (which decides the order of their ids and of the specie's list) would vary.
	parallel_create_mutants(Agent_Id,Count) when Count > 0 ->
		case config:evolution_seed() of
			undefined -> spawn_create_mutants(Agent_Id,Count);
			_ -> [population_monitor:create_MutantAgentCopy(Agent_Id) || _ <- lists:seq(1,Count)]
		end;
	parallel_create_mutants(_Agent_Id,_Count)->
		[].

	spawn_create_mutants(Agent_Id,Count)->
		Parent = self(),
		Pids = [spawn(fun()-> try MutantAgent_Id = population_monitor:create_MutantAgentCopy(Agent_Id), Parent ! {mutant_created,MutantAgent_Id} catch Error:Reason -> Parent ! {mutant_error,Error,Reason} end end) || _ <- lists:seq(1,Count)],
		MutantAgent_Ids = [receive {mutant_created,MutantAgent_Id} -> MutantAgent_Id; {mutant_error,Error,Reason} -> qlog:benchmarker(self,io_lib:format("Clone error Agent_Id=~p Error=~p Reason=~p",[Agent_Id,Error,Reason])), exit({clone_failed,Error,Reason}) end || _ <- Pids],
		MutantAgent_Ids.

	gather_survivors([{MutantAlotment,Fitness,TotNeurons,Agent_Id}|AlotmentsP],Normalizer,PopulationLimit,Acc)->
		SafeNormalizer = case Normalizer of
			0 -> 1.0;  % Prevent division by zero
			_ -> abs(Normalizer)
		end,
		Normalized_MutantAlotment = round(MutantAlotment/SafeNormalizer),
		% Cap to prevent excessive cloning
		CappedAlotment = min(Normalized_MutantAlotment, PopulationLimit),
		%io:format("Agent_Id:~p Normalized_MutantAlotment:~p~n",[Agent_Id,Normalized_MutantAlotment]),
		SurvivingAgent_Ids = case CappedAlotment >= 1 of
			true ->
				MutantAgent_Ids = case CappedAlotment >= 2 of true -> parallel_create_mutants(Agent_Id,CappedAlotment-1); false -> [] end,
				[Agent_Id|MutantAgent_Ids];
			false ->
				io:format("Deleting agent:~p~n",[Agent_Id]),
				qlog:benchmarker(self,io_lib:format("Deleting agent=~p",[Agent_Id])),
				genotype:delete_Agent(Agent_Id),
				[]
		end,
		gather_survivors(AlotmentsP,Normalizer,PopulationLimit,lists:append(SurvivingAgent_Ids,Acc));
	gather_survivors([],_Normalizer,_PopulationLimit,Acc)->
		io:format("New Population:~p PopSize:~p~n",[Acc,length(Acc)]),
		qlog:benchmarker(self,io_lib:format("New Population Size=~p",[length(Acc)])),
		Acc.
%The gather_survivors/3 function accepts the list composed of the alotment tuples and a population normalizer value calculated by the competition/3 function, and from those values calculates the actual number of offspring that each agent should produce, creating those mutant offspring and accumulating the new generation agent ids. FOr each Agent_Id the function first calculates the noramlized offspring alotment value, to ensure that the final nubmer of agents in the specie is within the popualtion limit of that specie. If the offspring alotment value is less than 0, the agent is killed. If the offspring alotment is 1, the parent agent is allowed to survive to the next generation, but is not allowed to create any new offspring. If the offspring alotment is greater than one, then the Normalized_MutantAlotment-1 offspring are created from this fit agent, by calling upon the create_MutantAgentCopy/1 function, which rerns the id of the new mutant offspring. Once all the offspring have been created, the function returns to the caller a list of ids, composed of the surviving parent agent ids, and their offspring.

top3(ProperlySorted_AgentSummaries,NeuralEnergyCost,PopulationLimit,_Survival_Percentage)->
	TotSurvivors = 3,
	Valid_AgentSummaries = lists:sublist(ProperlySorted_AgentSummaries,TotSurvivors),
	Invalid_AgentSummaries = ProperlySorted_AgentSummaries -- Valid_AgentSummaries,
	{_,_,Invalid_AgentIds} = lists:unzip3(Invalid_AgentSummaries),
	{_,_,Valid_AgentIds} = lists:unzip3(Valid_AgentSummaries),
	[genotype:delete_Agent(Agent_Id) || Agent_Id <- Invalid_AgentIds],
	io:format("Valid_AgentSummaries:~p~n",[Valid_AgentSummaries]),
	io:format("Invalid_AgentSummaries:~p~n",[Invalid_AgentSummaries]),
	TopAgentSummaries = lists:sublist(Valid_AgentSummaries,3),
	{_TopFitnessList,_TopTotNs,TopAgent_Ids} = lists:unzip3(TopAgentSummaries),
	io:format("NeuralEnergyCost:~p~n",[NeuralEnergyCost]),
	NewGenAgent_Ids = breed(Valid_AgentIds,PopulationLimit-TotSurvivors,[]),
	{NewGenAgent_Ids,TopAgent_Ids}.
		
	breed(_Valid_AgentIds,0,Acc)-> Acc;
	breed(Valid_AgentIds,OffspringIndex,Acc)->
		Parent_AgentId = lists:nth(rand:uniform(length(Valid_AgentIds)),Valid_AgentIds),
		BatchSize = min(OffspringIndex,10),
		MutantAgent_Ids = parallel_create_mutants(Parent_AgentId,BatchSize),
		Remaining = OffspringIndex - BatchSize,
		case Remaining > 0 of true -> breed(Valid_AgentIds,Remaining,lists:append(MutantAgent_Ids,Acc)); false -> lists:append(MutantAgent_Ids,Acc) end.
%The breed/3 function creates offspring from top 3 agents using parallel cloning for efficiency.

rank(AgentSummaries,_NeuralEnergyCost,PopulationLimit,Survival_Percentage)->
	%Best fitness first; between agents of equal fitness the one with fewer neurons comes first.
	Sorted_AgentSummaries = lists:sort(fun({F1,N1,_},{F2,N2,_})-> (F1 > F2) orelse ((F1 == F2) andalso (N1 =< N2)) end,AgentSummaries),
	TotSurvivors = erlang:max(1,erlang:min(PopulationLimit,round(length(Sorted_AgentSummaries)*Survival_Percentage))),
	{Valid_AgentSummaries,Invalid_AgentSummaries} = lists:split(TotSurvivors,Sorted_AgentSummaries),
	{_,_,Invalid_AgentIds} = lists:unzip3(Invalid_AgentSummaries),
	{_,_,Valid_AgentIds} = lists:unzip3(Valid_AgentSummaries),
	[genotype:delete_Agent(Agent_Id) || Agent_Id <- Invalid_AgentIds],
	io:format("Valid_AgentSummaries:~p~n",[Valid_AgentSummaries]),
	io:format("Invalid_AgentSummaries:~p~n",[Invalid_AgentSummaries]),
	TopAgent_Ids = lists:sublist(Valid_AgentIds,3),
	Alotments = apportion(lists:seq(TotSurvivors,1,-1),PopulationLimit-TotSurvivors),
	MutantAgent_Ids = lists:append([parallel_create_mutants(Agent_Id,Alotment) || {Agent_Id,Alotment} <- lists:zip(Valid_AgentIds,Alotments), Alotment > 0]),
	NewGenAgent_Ids = Valid_AgentIds++MutantAgent_Ids,
	qlog:benchmarker(self,io_lib:format("Rank selection | survivors=~p | offspring=~p | new population size=~p",[TotSurvivors,length(MutantAgent_Ids),length(NewGenAgent_Ids)])),
	{NewGenAgent_Ids,TopAgent_Ids}.
%rank/4 is a selection algorithm that looks only at the order of the agents, never at the size of their fitness values or of their networks. The best Survival_Percentage of the specie survive unchanged, and the remaining places up to PopulationLimit are filled with their mutant offspring. The survivor ranked first of S gets a share of the offspring proportional to S, the second to S-1, and so on down to 1 for the last survivor. It is the selection to use when fitness can be negative or sits on an arbitrary scale (alpha_lcb, and the large negative values given to ineligible agents): competition/4 turns fitness into offspring through sums and ratios, which a single outlier or a negative total distorts, and it divides every allotment by the neuron count.

	apportion(Weights,Total)->
		WeightSum = lists:sum(Weights),
		Shares = [Total*Weight/WeightSum || Weight <- Weights],
		Floors = [trunc(Share) || Share <- Shares],
		Leftover = Total - lists:sum(Floors),
		%Hand the places lost to rounding to the largest fractional parts, best rank first on ties.
		ByRemainder = lists:sort([{-(Share-Floor),Index} || {Index,Share,Floor} <- lists:zip3(lists:seq(1,length(Weights)),Shares,Floors)]),
		Extra = [Index || {_,Index} <- lists:sublist(ByRemainder,Leftover)],
		[Floor + case lists:member(Index,Extra) of true -> 1; false -> 0 end || {Index,Floor} <- lists:zip(lists:seq(1,length(Weights)),Floors)].
%apportion/2 splits Total whole offspring among parents in proportion to Weights (largest remainder method), so the result always sums to exactly Total.

competition(ProperlySorted_AgentSummaries)->
	TotEnergy = lists:sum([Fitness || {Fitness,_TotN,_Agent_Id}<-ProperlySorted_AgentSummaries]),
	TotNeurons = lists:sum([TotN || {_Fitness,TotN,_Agent_Id} <- ProperlySorted_AgentSummaries]),
	NeuralEnergyCost = TotEnergy/TotNeurons,
	{AlotmentsP,Normalizer} = calculate_alotments(ProperlySorted_AgentSummaries,NeuralEnergyCost,[],0),
	Choice = rand:uniform(),
	{WinnerFitness,WinnerTotN,WinnerAgent_Id}=choose_CompetitionWinner(AlotmentsP,Normalizer,Choice,0),
	{WinnerFitness,WinnerTotN,WinnerAgent_Id}.
		
	choose_CompetitionWinner([{MutantAlotment,Fitness,TotN,Agent_Id}|AlotmentsP],Normalizer,Choice,Range_From)->
		Range_To = Range_From+MutantAlotment/Normalizer,
		case (Choice >= Range_From) and (Choice =< Range_To) of
			true ->
				{Fitness,TotN,Agent_Id};
			false ->
				choose_CompetitionWinner(AlotmentsP,Normalizer,Choice,Range_To)
		end;
	choose_CompetitionWinner([],_Normalizer,_Choice,_Range_From)->
		exit("********ERROR:choose_CompetitionWinner:: reached [] without selecting a winner.").
