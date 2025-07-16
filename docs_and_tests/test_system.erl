%% Simple test module to verify the FX system works correctly
-module(test_system).
-compile(export_all).
-include("records.hrl").

%% Quick test of the system
quick_test() ->
    io:format("=== Quick System Test ===~n"),
    
    %% Test 1: Check if we can lookup price data
    io:format("Test 1: Price data lookup...~n"),
    case test_price_lookup() of
        ok -> io:format("✓ Price lookup: PASSED~n");
        error -> io:format("✗ Price lookup: FAILED~n")
    end,
    
    %% Test 2: Check if we can create a simple agent
    io:format("Test 2: Agent creation...~n"),
    case test_agent_creation() of
        ok -> io:format("✓ Agent creation: PASSED~n");
        error -> io:format("✗ Agent creation: FAILED~n")
    end,
    
    %% Test 3: Check if FX simulation works
    io:format("Test 3: FX simulation...~n"),
    case test_fx_simulation() of
        ok -> io:format("✓ FX simulation: PASSED~n");
        error -> io:format("✗ FX simulation: FAILED~n")
    end,
    
    io:format("=== Test Complete ===~n").

test_price_lookup() ->
    try
        %% Try to lookup some price data
        TableName = 'EURUSD15',
        case ets:info(TableName, name) of
            undefined -> 
                io:format("Table ~p not loaded~n", [TableName]),
                error;
            _ ->
                %% Get first key
                FirstKey = ets:first(TableName),
                case FirstKey of
                    '$end_of_table' -> 
                        io:format("Table ~p is empty~n", [TableName]),
                        error;
                    _ ->
                        %% Try lookup
                        Record = fx:lookup(TableName, FirstKey),
                        case Record of
                            undefined -> 
                                io:format("Lookup failed for key ~p~n", [FirstKey]),
                                error;
                            _ ->
                                io:format("Successfully looked up record: ~p~n", [Record]),
                                ok
                        end
                end
        end
    catch
        Error:Reason ->
            io:format("Price lookup test failed: ~p:~p~n", [Error, Reason]),
            error
    end.

test_agent_creation() ->
    try
        %% Try to create a simple agent genotype
        Constraint = #constraint{
            morphology = forex_trader,
            connection_architecture = recurrent,
            neural_afs = [tanh]
        },
        Agent_Id = genotype:construct_Agent(test_agent, Constraint, []),
        case Agent_Id of
            undefined -> 
                io:format("Failed to create agent~n"),
                error;
            _ ->
                io:format("Successfully created agent: ~p~n", [Agent_Id]),
                ok
        end
    catch
        Error:Reason ->
            io:format("Agent creation test failed: ~p:~p~n", [Error, Reason]),
            error
    end.

test_fx_simulation() ->
    try
        %% Try to start a simple FX simulation
        SimPid = spawn(fx, sim, [self()]),
        
        %% Send a sense message
        SimPid ! {self(), sense, 'EURUSD15', close, [5, list_sensor], 1000, 200},
        
        %% Wait for response
        receive
            {SimPid, Result} ->
                io:format("FX simulation returned: ~p~n", [Result]),
                ok
        after 5000 ->
            io:format("FX simulation timed out~n"),
            error
        end
    catch
        Error:Reason ->
            io:format("FX simulation test failed: ~p:~p~n", [Error, Reason]),
            error
    end.

%% Test with minimal parameters
test_minimal_benchmark() ->
    io:format("=== Minimal Benchmark Test ===~n"),
    
    %% Set minimal parameters for testing
    put(test_mode, true),
    
    try
        %% Start a very small benchmark
        benchmarker:start(test_minimal),
        io:format("✓ Minimal benchmark completed~n")
    catch
        Error:Reason ->
            io:format("✗ Minimal benchmark failed: ~p:~p~n", [Error, Reason])
    end,
    
    erase(test_mode),
    io:format("=== Minimal Test Complete ===~n").