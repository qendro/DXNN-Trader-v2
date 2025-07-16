%% Simple test that bypasses the problematic functions
-module(simple_test).
-compile(export_all).

%% Include the records
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).
-record(account,{leverage,lot,spread,margin,balance,net_asset_value,realized_PL=0,unrealized_PL=0,order}).

%% Test FX simulation with manual state creation
test_fx_simulation_manual() ->
    io:format("=== Manual FX Simulation Test ===~n"),
    
    %% Get some keys manually
    FirstKey = ets:first('EURUSD15'),
    SecondKey = ets:next('EURUSD15', FirstKey),
    ThirdKey = ets:next('EURUSD15', SecondKey),
    
    io:format("Keys: ~p, ~p, ~p~n", [FirstKey, SecondKey, ThirdKey]),
    
    %% Create a manual state with valid keys
    TestState = #state{
        table_name = 'EURUSD15',
        feature = close,
        index_start = FirstKey,
        index_end = ThirdKey,
        index = FirstKey,
        price_list = []
    },
    
    io:format("Manual state: ~p~n", [TestState]),
    
    %% Test list sensor with this state
    try
        {Result, _UpdatedState} = fx:list_encoded(3, TestState),
        io:format("✓ List sensor worked: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("✗ List sensor failed: ~p:~p~n", [Error, Reason])
    end.

%% Test a complete simulation with manual setup
test_complete_simulation() ->
    io:format("=== Complete Simulation Test ===~n"),
    
    %% Start simulation process
    SimPid = spawn(fun() ->
        io:format("Sim process started~n"),
        
        %% Create manual state and account
        FirstKey = ets:first('EURUSD15'),
        Keys = get_next_keys(FirstKey, 'EURUSD15', 10),
        LastKey = lists:last(Keys),
        
        S = #state{
            table_name = 'EURUSD15',
            feature = close,
            index_start = FirstKey,
            index_end = LastKey,
            index = FirstKey,
            price_list = []
        },
        
        A = #account{
            leverage = 50,
            lot = 10000,
            spread = 0.000150,
            margin = 0,
            balance = 300,
            net_asset_value = 300,
            realized_PL = 0,
            unrealized_PL = 0,
            order = undefined
        },
        
        io:format("Manual sim setup complete~n"),
        manual_sim_loop(S, A)
    end),
    
    timer:sleep(100),
    
    %% Test sense message
    io:format("Sending sense message...~n"),
    SimPid ! {self(), sense, manual_test},
    
    receive
        {SimPid, Result} ->
            io:format("✓ Got result: ~p~n", [Result]);
        Other ->
            io:format("Got other: ~p~n", [Other])
    after 2000 ->
        io:format("✗ Timeout~n")
    end.

%% Simple simulation loop for testing
manual_sim_loop(S, A) ->
    receive
        {From, sense, manual_test} ->
            %% Simple sense operation
            try
                {Result, _U_S} = fx:list_encoded(3, S),
                From ! {self(), Result}
            catch
                Error:Reason ->
                    From ! {self(), {error, Error, Reason}}
            end,
            manual_sim_loop(S, A);
        Other ->
            io:format("Sim got: ~p~n", [Other]),
            manual_sim_loop(S, A)
    end.

%% Get next N keys from a starting key
get_next_keys(Key, _Table, 0) ->
    [Key];
get_next_keys(Key, Table, N) ->
    case ets:next(Table, Key) of
        '$end_of_table' -> [Key];
        NextKey -> [Key | get_next_keys(NextKey, Table, N-1)]
    end.

%% Run the tests
run_tests() ->
    test_fx_simulation_manual(),
    test_complete_simulation().