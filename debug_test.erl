-module(debug_test).
-include("records.hrl").
-export([test/0]).

test() ->
    mnesia:start(),
    
    Agent_Id = {1.0, agent},
    Agent = #agent{
        id = Agent_Id,
        fitness = 0.75,
        generation = 1,
        population_id = test,
        specie_id = {1.0, specie}
    },
    
    mnesia:transaction(fun() -> mnesia:write(Agent) end),
    Keys = mnesia:dirty_all_keys(agent),
    io:format("Keys: ~p~n", [Keys]),
    
    Result = genotype_utils:find_best_agent(),
    io:format("Result: ~p~n", [Result]),
    
    % Also test reading the agent back
    ReadResult = mnesia:dirty_read({agent, Agent_Id}),
    io:format("Read result: ~p~n", [ReadResult]).