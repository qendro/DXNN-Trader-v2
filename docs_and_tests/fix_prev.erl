%% Fix the prev function issue
-module(fix_prev).
-compile(export_all).

%% Test the prev function directly
test_prev_function() ->
    io:format("=== Testing prev function ===~n"),
    
    %% Get the last key from the table
    LastKey = ets:last('EURUSD15'),
    io:format("Last key in EURUSD15: ~p~n", [LastKey]),
    
    %% Test going back 1 step
    io:format("Testing prev with count 1...~n"),
    try
        Prev1 = fx:prev('EURUSD15', LastKey, prev, 1),
        io:format("✓ Prev 1: ~p~n", [Prev1])
    catch
        Error:Reason ->
            io:format("✗ Prev 1 failed: ~p:~p~n", [Error, Reason])
    end,
    
    %% Test going back 10 steps
    io:format("Testing prev with count 10...~n"),
    try
        Prev10 = fx:prev('EURUSD15', LastKey, prev, 10),
        io:format("✓ Prev 10: ~p~n", [Prev10])
    catch
        Error2:Reason2 ->
            io:format("✗ Prev 10 failed: ~p:~p~n", [Error2, Reason2])
    end,
    
    %% Test going back 100 steps
    io:format("Testing prev with count 100...~n"),
    try
        Prev100 = fx:prev('EURUSD15', LastKey, prev, 100),
        io:format("✓ Prev 100: ~p~n", [Prev100])
    catch
        Error3:Reason3 ->
            io:format("✗ Prev 100 failed: ~p:~p~n", [Error3, Reason3])
    end.

%% Test using original ETS prev directly
test_direct_prev() ->
    io:format("=== Testing direct ETS prev ===~n"),
    
    LastKey = ets:last('EURUSD15'),
    io:format("Last key: ~p~n", [LastKey]),
    
    %% Go back 10 steps using direct ETS
    Key = LastKey,
    Keys = go_back_direct(Key, 'EURUSD15', 10, []),
    io:format("Direct prev 10 steps: ~p~n", [Keys]).

go_back_direct(Key, _Table, 0, Acc) ->
    lists:reverse([Key|Acc]);
go_back_direct(Key, Table, Count, Acc) ->
    case ets:prev(Table, Key) of
        '$end_of_table' -> 
            lists:reverse([Key|Acc]);
        PrevKey ->
            go_back_direct(PrevKey, Table, Count-1, [Key|Acc])
    end.

%% Create a working prev function
working_prev(TableName, Key, Count) when Count > 0 ->
    working_prev_recursive(TableName, Key, Count).

working_prev_recursive(_TableName, Key, 0) ->
    Key;
working_prev_recursive(TableName, Key, Count) when Count > 0 ->
    case ets:prev(TableName, Key) of
        '$end_of_table' -> 
            Key; %% Return current key if we hit the end
        PrevKey -> 
            working_prev_recursive(TableName, PrevKey, Count - 1)
    end.

%% Test the working prev function
test_working_prev() ->
    io:format("=== Testing working prev function ===~n"),
    
    LastKey = ets:last('EURUSD15'),
    
    Prev10 = working_prev('EURUSD15', LastKey, 10),
    io:format("Working prev 10: ~p~n", [Prev10]),
    
    Prev100 = working_prev('EURUSD15', LastKey, 100),
    io:format("Working prev 100: ~p~n", [Prev100]).

%% Run all tests
run_tests() ->
    test_prev_function(),
    test_direct_prev(),
    test_working_prev().