%% Quick test script with minimal parameters
-module(quick_test).
-compile(export_all).

%% Test with minimal parameters
run_minimal_test() ->
    io:format("=== Starting Minimal Test ===~n"),
    
    %% Step 1: Compile modules
    io:format("1. Compiling modules...~n"),
    make:all(),
    
    %% Step 2: Initialize Mnesia
    io:format("2. Initializing Mnesia...~n"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Step 3: Initialize FX tables
    io:format("3. Initializing FX tables...~n"),
    fx:init(),
    
    %% Step 4: Start FX system
    io:format("4. Starting FX system...~n"),
    fx:start(),
    
    %% Step 5: Initialize price cache
    io:format("5. Initializing price cache...~n"),
    fx:init_price_cache('EURUSD15'),
    
    %% Step 6: Test a single FX simulation
    io:format("6. Testing single FX simulation...~n"),
    test_single_simulation(),
    
    %% Step 7: Create polis with test config
    io:format("7. Creating polis...~n"),
    polis:create(),
    polis:start(),
    polis:sync(),
    
    %% Step 8: Run minimal benchmark
    io:format("8. Starting minimal benchmark...~n"),
    io:format("This should complete in 1-2 minutes...~n"),
    
    %% Override config temporarily for this test
    put(test_mode, true),
    Result = benchmarker:start(test_minimal),
    erase(test_mode),
    
    io:format("=== Test Complete: ~p ===~n", [Result]).

%% Test a single FX simulation to make sure it works
test_single_simulation() ->
    try
        %% Spawn a single FX simulation
        SimPid = spawn(fx, sim, [self()]),
        
        %% Send a simple sense request
        SimPid ! {self(), sense, 'EURUSD15', close, [5, list_sensor], 100, 50},
        
        %% Wait for response
        receive
            {SimPid, SensorData} ->
                io:format("✓ FX simulation working - got sensor data: ~p~n", [length(SensorData)]);
            Other ->
                io:format("✗ Unexpected response: ~p~n", [Other])
        after 5000 ->
            io:format("✗ FX simulation timeout~n")
        end,
        
        %% Send a trade signal
        SimPid ! {self(), trade, 'EURUSD15', 1},
        
        %% Wait for trade response
        receive
            {SimPid, TradeResult, Status} ->
                io:format("✓ FX trading working - result: ~p, status: ~p~n", [TradeResult, Status]);
            Other2 ->
                io:format("✗ Unexpected trade response: ~p~n", [Other2])
        after 5000 ->
            io:format("✗ FX trading timeout~n")
        end
        
    catch
        Error:Reason ->
            io:format("✗ FX simulation test failed: ~p:~p~n", [Error, Reason])
    end.

%% Check system status
check_status() ->
    io:format("=== System Status ===~n"),
    
    %% Check tables
    Tables = ['EURUSD15', 'EURUSD1', 'EURUSD30', 'EURUSD60', metadata],
    [case ets:info(Table, name) of
        undefined -> io:format("✗ ~p: Not loaded~n", [Table]);
        _ -> io:format("✓ ~p: ~p records~n", [Table, ets:info(Table, size)])
    end || Table <- Tables],
    
    %% Check price cache
    case ets:info(fx_price_cache, name) of
        undefined -> io:format("✗ Price cache: Not initialized~n");
        _ -> io:format("✓ Price cache: ~p records~n", [ets:info(fx_price_cache, size)])
    end,
    
    %% Check processes
    case whereis(polis) of
        undefined -> io:format("✗ Polis: Not running~n");
        Pid -> io:format("✓ Polis: Running (~p)~n", [Pid])
    end.

%% Run just the FX system test without evolution
test_fx_only() ->
    io:format("=== FX System Only Test ===~n"),
    
    make:all(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    fx:init(),
    fx:start(),
    fx:init_price_cache('EURUSD15'),
    
    check_status(),
    test_single_simulation(),
    
    io:format("=== FX Test Complete ===~n").