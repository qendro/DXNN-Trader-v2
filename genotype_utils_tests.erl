%% Unit tests for genotype_utils module
-module(genotype_utils_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

%% Test setup and teardown
setup() ->
    % Start Mnesia for testing
    mnesia:start(),
    
    % Force load the agent table if it exists
    case mnesia:force_load_table(agent) of
        yes -> ok;
        _ -> 
            % Create agent table if it doesn't exist
            case mnesia:create_table(agent, [
                {disc_copies, [node()]},
                {type, set},
                {attributes, record_info(fields, agent)}
            ]) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, agent}} -> ok;
                Error -> throw({table_creation_failed, Error})
            end
    end,
    
    % Clear any existing test data
    mnesia:clear_table(agent),
    ok.

cleanup(_) ->
    % Clean up test data
    mnesia:clear_table(agent),
    ok.

%% Test finding best agent with no agents in database
find_best_agent_no_agents_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         Result = genotype_utils:find_best_agent(),
         ?assertEqual({atomic, {error, no_agents}}, Result)
     end}.

%% Test finding best agent with single agent
find_best_agent_single_agent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         % Create a single test agent
         Agent_Id = {1.0, agent},
         Agent = #agent{
             id = Agent_Id,
             fitness = 0.75,
             generation = 1,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         % Insert agent into database
         mnesia:transaction(fun() ->
             mnesia:write(Agent)
         end),
         
         % Test finding best agent
         Result = genotype_utils:find_best_agent(),
         ?assertEqual({atomic, {ok, Agent_Id}}, Result)
     end}.

%% Test finding best agent with multiple agents
find_best_agent_multiple_agents_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         % Create multiple test agents with different fitness values
         Agent1_Id = {1.0, agent},
         Agent1 = #agent{
             id = Agent1_Id,
             fitness = 0.5,
             generation = 1,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         Agent2_Id = {2.0, agent},
         Agent2 = #agent{
             id = Agent2_Id,
             fitness = 0.9,  % This should be the best
             generation = 2,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         Agent3_Id = {3.0, agent},
         Agent3 = #agent{
             id = Agent3_Id,
             fitness = 0.3,
             generation = 1,
             population_id = test,
             specie_id = {2.0, specie}
         },
         
         % Insert agents into database
         mnesia:transaction(fun() ->
             mnesia:write(Agent1),
             mnesia:write(Agent2),
             mnesia:write(Agent3)
         end),
         
         % Test finding best agent
         Result = genotype_utils:find_best_agent(),
         ?assertEqual({atomic, {ok, Agent2_Id}}, Result)
     end}.

%% Test finding best agent with specific population
find_best_agent_specific_population_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         % Create agents in different populations
         Agent1_Id = {1.0, agent},
         Agent1 = #agent{
             id = Agent1_Id,
             fitness = 0.8,
             generation = 1,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         Agent2_Id = {2.0, agent},
         Agent2 = #agent{
             id = Agent2_Id,
             fitness = 0.9,  % Higher fitness but different population
             generation = 2,
             population_id = production,
             specie_id = {1.0, specie}
         },
         
         % Insert agents into database
         mnesia:transaction(fun() ->
             mnesia:write(Agent1),
             mnesia:write(Agent2)
         end),
         
         % Test finding best agent in specific population
         Result = genotype_utils:find_best_agent(test),
         ?assertEqual({atomic, {ok, Agent1_Id}}, Result),
         
         % Test finding best agent in production population
         Result2 = genotype_utils:find_best_agent(production),
         ?assertEqual({atomic, {ok, Agent2_Id}}, Result2)
     end}.

%% Test finding best agent with no agents in specified population
find_best_agent_empty_population_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         % Create agent in test population
         Agent_Id = {1.0, agent},
         Agent = #agent{
             id = Agent_Id,
             fitness = 0.75,
             generation = 1,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         % Insert agent into database
         mnesia:transaction(fun() ->
             mnesia:write(Agent)
         end),
         
         % Test finding best agent in non-existent population
         Result = genotype_utils:find_best_agent(nonexistent),
         ?assertEqual({atomic, {error, no_agents}}, Result)
     end}.

%% Test finding best agent with equal fitness values
find_best_agent_equal_fitness_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
         % Create agents with equal fitness
         Agent1_Id = {1.0, agent},
         Agent1 = #agent{
             id = Agent1_Id,
             fitness = 0.75,
             generation = 1,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         Agent2_Id = {2.0, agent},
         Agent2 = #agent{
             id = Agent2_Id,
             fitness = 0.75,  % Same fitness
             generation = 2,
             population_id = test,
             specie_id = {1.0, specie}
         },
         
         % Insert agents into database
         mnesia:transaction(fun() ->
             mnesia:write(Agent1),
             mnesia:write(Agent2)
         end),
         
         % Test finding best agent - should return one of them
         {atomic, {ok, Best_Agent_Id}} = genotype_utils:find_best_agent(),
         ?assert(Best_Agent_Id =:= Agent1_Id orelse Best_Agent_Id =:= Agent2_Id)
     end}.