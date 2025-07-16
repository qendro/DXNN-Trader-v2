%% Working test that bypasses fx_GetPriceList entirely
-module(working_test).
-compile(export_all).

%% Include the records
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).
-record(account,{leverage,lot,spread,margin,balance,net_asset_value,realized_PL=0,unrealized_PL=0,order}).
-record(technical,{id,open,high,low,close,volume}).

%% Create our own working price list function
get_price_list_working(Table, Key, Count) ->
    get_price_list_working(Table, Key, Count, []).

get_price_list_working(_Table, '$end_of_table', _Count, Acc) ->
    lists:reverse(Acc);
get_price_list_working(_Table, _Key, 0, Acc) ->
    lists:reverse(Acc);
get_price_list_working(Table, Key, Count, Acc) ->
    case fx:lookup(Table, Key) of
        undefined ->
            lists:reverse(Acc);
        Record ->
            PriceData = {Record#technical.open, Record#technical.close, 
                        Record#technical.high, Record#technical.low},
            NextKey = fx:next(Table, Key),
            get_price_list_working(Table, NextKey, Count-1, [PriceData|Acc])
    end.

%% Working list sensor function
list_sensor_working(HRes, TableName, StartKey) ->
    PriceList = get_price_list_working(TableName, StartKey, HRes),
    CloseList = [Close || {_Open, Close, _High, _Low} <- PriceList],
    CloseList.

%% Test the working sensor
test_working_sensor() ->
    io:format("=== Testing Working Sensor ===~n"),
    
    FirstKey = ets:first('EURUSD15'),
    io:format("Starting from key: ~p~n", [FirstKey]),
    
    %% Test with 5 price points
    try
        Result = list_sensor_working(5, 'EURUSD15', FirstKey),
        io:format("✓ Working sensor result: ~p~n", [Result])
    catch
        Error:Reason ->
            io:format("✗ Working sensor failed: ~p:~p~n", [Error, Reason])
    end.

%% Test a complete working simulation
test_working_simulation() ->
    io:format("=== Testing Working Simulation ===~n"),
    
    SimPid = spawn(fun() ->
        io:format("Working sim started~n"),
        working_sim_loop()
    end),
    
    timer:sleep(100),
    
    %% Send sense request
    SimPid ! {self(), sense, 5},
    
    receive
        {SimPid, Result} ->
            io:format("✓ Working sim result: ~p~n", [Result]);
        Other ->
            io:format("Got: ~p~n", [Other])
    after 2000 ->
        io:format("✗ Working sim timeout~n")
    end.

working_sim_loop() ->
    receive
        {From, sense, HRes} ->
            FirstKey = ets:first('EURUSD15'),
            Result = list_sensor_working(HRes, 'EURUSD15', FirstKey),
            From ! {self(), Result},
            working_sim_loop();
        Other ->
            io:format("Working sim got: ~p~n", [Other]),
            working_sim_loop()
    end.

%% Test trading operations
test_trading_operations() ->
    io:format("=== Testing Trading Operations ===~n"),
    
    %% Create test state and account
    FirstKey = ets:first('EURUSD15'),
    SecondKey = ets:next('EURUSD15', FirstKey),
    
    S = #state{
        table_name = 'EURUSD15',
        feature = close,
        index_start = FirstKey,
        index_end = SecondKey,
        index = FirstKey,
        price_list = []
    },
    
    A = #account{
        leverage = 50,
        lot = 10000,
        spread = 0.000150,
        margin = 0,
        balance = 300,
        net_asset_value = 300
    },
    
    %% Test make_trade function
    try
        UpdatedAccount = fx:make_trade(S, A, 1), %% Buy signal
        io:format("✓ Trade executed: ~p~n", [UpdatedAccount])
    catch
        Error:Reason ->
            io:format("✗ Trade failed: ~p:~p~n", [Error, Reason])
    end.

%% Run all working tests
run_all_tests() ->
    test_working_sensor(),
    test_working_simulation(),
    test_trading_operations().

%% Quick benchmark with working functions
quick_benchmark() ->
    io:format("=== Quick Benchmark (Working Version) ===~n"),
    
    %% Test 10 agents, 10 evaluations each
    StartTime = erlang:system_time(millisecond),
    
    Results = [test_single_agent(AgentId) || AgentId <- lists:seq(1, 10)],
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    io:format("✓ Completed 10 agents in ~p ms~n", [Duration]),
    io:format("Results: ~p~n", [Results]).

test_single_agent(AgentId) ->
    %% Simulate a single agent evaluation
    FirstKey = ets:first('EURUSD15'),
    
    %% Run 10 evaluations
    Profits = [simulate_single_evaluation(FirstKey) || _ <- lists:seq(1, 10)],
    
    AvgProfit = lists:sum(Profits) / length(Profits),
    {AgentId, AvgProfit}.

simulate_single_evaluation(StartKey) ->
    %% Simple simulation: get 5 price points and make a random trade
    PriceList = get_price_list_working('EURUSD15', StartKey, 5),
    
    case PriceList of
        [] -> 0.0;
        [_|_] ->
            %% Simple strategy: if last price > first price, profit = 1, else -1
            FirstPrice = element(2, hd(PriceList)), %% close price
            LastPrice = element(2, lists:last(PriceList)),
            if 
                LastPrice > FirstPrice -> 1.0;
                true -> -1.0
            end
    end.