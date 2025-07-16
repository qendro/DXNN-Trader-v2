%% Correct startup sequence for FX Trading System with performance optimizations
%% This file shows the proper order of initialization to avoid table creation errors

-module(startup_sequence).
-compile(export_all).

%% Correct startup sequence
start_system() ->
    io:format("Starting FX Trading System with optimizations...~n"),
    
    %% Step 1: Compile all modules
    io:format("1. Compiling modules...~n"),
    make:all(),
    
    %% Step 2: Initialize Mnesia database
    io:format("2. Initializing Mnesia...~n"),
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Step 3: Initialize FX tables (creates table files if they don't exist)
    io:format("3. Initializing FX tables...~n"),
    fx:init(),
    
    %% Step 4: Start FX system (loads tables into ETS)
    io:format("4. Starting FX system...~n"),
    fx:start(),
    
    %% Step 5: Initialize price cache (now that tables are loaded)
    io:format("5. Initializing price cache...~n"),
    fx:init_price_cache('EURUSD15'),
    
    %% Step 6: Create and start polis
    io:format("6. Creating polis...~n"),
    polis:create(),
    polis:start(),
    polis:sync(),
    
    %% Step 7: Start benchmarker
    io:format("7. Starting benchmarker...~n"),
    benchmarker:start(sliding_window_5),
    
    io:format("System startup complete!~n").

%% Alternative startup for testing with smaller parameters
start_system_test() ->
    io:format("Starting FX Trading System (test mode)...~n"),
    
    %% Step 1: Compile all modules
    make:all(),
    
    %% Step 2: Initialize Mnesia database
    mnesia:create_schema([node()]),
    mnesia:start(),
    
    %% Step 3: Initialize FX tables
    fx:init(),
    
    %% Step 4: Start FX system
    fx:start(),
    
    %% Step 5: Initialize price cache
    fx:init_price_cache('EURUSD15'),
    
    %% Step 6: Create and start polis with smaller parameters
    polis:create(),
    polis:start(),
    polis:sync(),
    
    %% Step 7: Start benchmarker with test parameters
    %% Use smaller population for testing
    benchmarker:start(test_small),
    
    io:format("Test system startup complete!~n").

%% Check system status
check_system_status() ->
    io:format("=== System Status ===~n"),
    
    %% Check Mnesia
    case mnesia:system_info(is_running) of
        yes -> io:format("✓ Mnesia: Running~n");
        _ -> io:format("✗ Mnesia: Not running~n")
    end,
    
    %% Check FX tables
    Tables = ['EURUSD1', 'EURUSD15', 'EURUSD30', 'EURUSD60', metadata],
    [case ets:info(Table, name) of
        undefined -> io:format("✗ Table ~p: Not loaded~n", [Table]);
        _ -> io:format("✓ Table ~p: Loaded (~p records)~n", [Table, ets:info(Table, size)])
    end || Table <- Tables],
    
    %% Check price cache
    case ets:info(fx_price_cache, name) of
        undefined -> io:format("✗ Price cache: Not initialized~n");
        _ -> io:format("✓ Price cache: Initialized (~p records)~n", [ets:info(fx_price_cache, size)])
    end,
    
    %% Check polis
    case whereis(polis) of
        undefined -> io:format("✗ Polis: Not running~n");
        Pid -> io:format("✓ Polis: Running (PID: ~p)~n", [Pid])
    end,
    
    io:format("=== End Status ===~n").