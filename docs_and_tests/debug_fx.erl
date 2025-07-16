%% Debug FX simulation issues
-module(debug_fx).
-compile(export_all).

%% Include the records from fx.erl
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).
-record(account,{leverage,lot,spread,margin,balance,net_asset_value,realized_PL=0,unrealized_PL=0,order}).
-record(order,{pair,position,entry,current,units,change,percentage_change,profit}).

%% Test basic FX operations
test_basic_operations() ->
    io:format("=== Testing Basic FX Operations ===~n"),
    
    %% Test 1: Basic table lookup
    io:format("1. Testing table lookup...~n"),
    FirstKey = ets:first('EURUSD15'),
    io:format("First key: ~p~n", [FirstKey]),
    
    case fx:lookup('EURUSD15', FirstKey) of
        undefined -> 
            io:format("✗ Lookup failed~n");
        Record ->
            io:format("✓ Lookup successful: ~p~n", [Record])
    end,
    
    %% Test 2: Next key
    io:format("2. Testing next key...~n"),
    NextKey = fx:next('EURUSD15', FirstKey),
    io:format("Next key: ~p~n", [NextKey]),
    
    %% Test 3: State initialization
    io:format("3. Testing state initialization...~n"),
    try
        TestState = fx:init_state(#state{}, 'EURUSD15', close, 100, 50),
        io:format("✓ State initialized: ~p~n", [TestState])
    catch
        Error:Reason ->
            io:format("✗ State initialization failed: ~p:~p~n", [Error, Reason])
    end,
    
    %% Test 4: Account creation
    io:format("4. Testing account creation...~n"),
    try
        TestAccount = fx:create_account(),
        io:format("✓ Account created: ~p~n", [TestAccount])
    catch
        Error2:Reason2 ->
            io:format("✗ Account creation failed: ~p:~p~n", [Error2, Reason2])
    end.

%% Test sensor operations
test_sensor_operations() ->
    io:format("=== Testing Sensor Operations ===~n"),
    
    try
        %% Create a test state
        FirstKey = ets:first('EURUSD15'),
        TestState = #state{
            table_name = 'EURUSD15',
            feature = close,
            index_start = FirstKey,
            index_end = fx:next('EURUSD15', fx:next('EURUSD15', FirstKey)),
            index = FirstKey,
            price_list = []
        },
        
        io:format("Test state: ~p~n", [TestState]),
        
        %% Test list sensor
        io:format("Testing list sensor...~n"),
        {Result1, UpdatedState1} = fx:list_encoded(3, TestState),
        io:format("✓ List sensor result: ~p~n", [Result1]),
        
        %% Test plane sensor
        io:format("Testing plane sensor...~n"),
        {Result2, UpdatedState2} = fx:plane_encoded(5, 3, TestState),
        io:format("✓ Plane sensor result length: ~p~n", [length(Result2)])
        
    catch
        Error:Reason ->
            io:format("✗ Sensor test failed: ~p:~p~n", [Error, Reason]),
            io:format("Stack trace: ~p~n", [erlang:get_stacktrace()])
    end.

%% Test a complete simulation step
test_simulation_step() ->
    io:format("=== Testing Complete Simulation Step ===~n"),
    
    try
        %% Start a simulation process
        SimPid = spawn(fun() -> 
            io:format("Simulation process started~n"),
            fx:sim(self())
        end),
        
        timer:sleep(100), %% Give it time to start
        
        %% Send a sense message with very simple parameters
        io:format("Sending sense message...~n"),
        SimPid ! {self(), sense, 'EURUSD15', close, [3, list_sensor], 100, 50},
        
        %% Wait for response with detailed timeout handling
        receive
            {SimPid, Result} ->
                io:format("✓ Got sensor result: ~p~n", [Result]);
            Other ->
                io:format("✗ Got unexpected message: ~p~n", [Other])
        after 2000 ->
            io:format("✗ Simulation timed out~n"),
            %% Check if process is still alive
            case is_process_alive(SimPid) of
                true -> 
                    io:format("Process still alive, checking message queue...~n"),
                    {message_queue_len, QLen} = process_info(SimPid, message_queue_len),
                    io:format("Message queue length: ~p~n", [QLen]);
                false ->
                    io:format("Process died~n")
            end
        end
        
    catch
        Error:Reason ->
            io:format("✗ Simulation step test failed: ~p:~p~n", [Error, Reason])
    end.

%% Run all tests
run_all_tests() ->
    test_basic_operations(),
    test_sensor_operations(),
    test_simulation_step().