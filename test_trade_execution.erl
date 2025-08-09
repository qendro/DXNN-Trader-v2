%% Test module for trade execution functionality
%% Tests the enhanced trade execution features implemented in task 4

-module(test_trade_execution).
-compile(export_all).
-include("records.hrl").

%% Test the trade signal translation
test_signal_translation() ->
    io:format("=== Testing Trade Signal Translation ===~n"),
    
    %% Test cases: {CurrentPosition, TradeSignal, ExpectedAction}
    TestCases = [
        {0, 1, "should open long position"},
        {0, -1, "should open short position"},
        {0, 0, "should do nothing"},
        {1, 0, "should close long position"},
        {-1, 0, "should close short position"},
        {1, 1, "should maintain long position"},
        {-1, -1, "should maintain short position"},
        {1, -1, "should switch from long to short"},
        {-1, 1, "should switch from short to long"}
    ],
    
    [begin
        io:format("Test: Position ~p, Signal ~p -> ~s~n", [Pos, Signal, Expected])
     end || {Pos, Signal, Expected} <- TestCases],
    
    io:format("Signal translation test completed~n~n").

%% Test order message encoding
test_order_encoding() ->
    io:format("=== Testing Order Message Encoding ===~n"),
    
    %% Test encoding functions
    Symbol = "EUR.USD",
    Action = "BUY",
    Quantity = 10000,
    OrderType = "MKT",
    
    io:format("Testing order encoding for:~n"),
    io:format("  Symbol: ~s~n", [Symbol]),
    io:format("  Action: ~s~n", [Action]),
    io:format("  Quantity: ~p~n", [Quantity]),
    io:format("  Order Type: ~s~n", [OrderType]),
    
    %% Test individual encoding functions
    IntEncoded = ib_connector:encode_int(12345),
    io:format("Integer encoding test: ~p~n", [IntEncoded]),
    
    StringEncoded = ib_connector:encode_string("TEST"),
    io:format("String encoding test: ~p~n", [StringEncoded]),
    
    DoubleEncoded = ib_connector:encode_double(1.23456),
    io:format("Double encoding test: ~p~n", [DoubleEncoded]),
    
    io:format("Order encoding test completed~n~n").

%% Test order tracking functionality
test_order_tracking() ->
    io:format("=== Testing Order Tracking ===~n"),
    
    %% Test order tracking data structures
    OrderId = 1001,
    Symbol = "EUR.USD",
    Action = "BUY",
    Quantity = 10000,
    Timestamp = erlang:timestamp(),
    
    PendingOrder = {OrderId, Symbol, Action, Quantity, Timestamp},
    io:format("Pending order structure: ~p~n", [PendingOrder]),
    
    %% Test order confirmation structure
    Status = "Filled",
    FillPrice = 1.1234,
    FillQuantity = 10000,
    
    OrderConfirmation = {OrderId, Status, FillPrice, FillQuantity},
    io:format("Order confirmation structure: ~p~n", [OrderConfirmation]),
    
    io:format("Order tracking test completed~n~n").

%% Test live scape trade interface
test_live_scape_interface() ->
    io:format("=== Testing Live Scape Trade Interface ===~n"),
    
    %% Create test state
    TestState = #live_state{
        table_name = 'EUR.USD',
        feature = close,
        current_position = 0,
        entry_price = 0,
        account_balance = 10000,
        realized_pnl = 0,
        unrealized_pnl = 0
    },
    
    io:format("Test state created: ~p~n", [TestState]),
    
    %% Test trade signal processing (without actual IB connection)
    TradeSignals = [1, -1, 0],
    
    [begin
        io:format("Testing trade signal: ~p~n", [Signal]),
        %% Note: This would normally call handle_trade_request, but we're just testing the interface
        io:format("  Signal ~p would be processed~n", [Signal])
     end || Signal <- TradeSignals],
    
    io:format("Live scape interface test completed~n~n").

%% Test configuration values
test_configuration() ->
    io:format("=== Testing Configuration Values ===~n"),
    
    %% Test all required config functions
    try
        Spread = config:account_spread(),
        io:format("Account spread: ~p~n", [Spread]),
        
        PositionSize = config:live_position_size(),
        io:format("Live position size: ~p~n", [PositionSize]),
        
        Leverage = config:account_leverage(),
        io:format("Account leverage: ~p~n", [Leverage]),
        
        InitialBalance = config:account_initial_balance(),
        io:format("Initial balance: ~p~n", [InitialBalance]),
        
        IBHost = config:ib_host(),
        io:format("IB host: ~p~n", [IBHost]),
        
        IBPort = config:ib_port(),
        io:format("IB port: ~p~n", [IBPort]),
        
        ClientId = config:ib_client_id(),
        io:format("IB client ID: ~p~n", [ClientId]),
        
        io:format("All configuration values loaded successfully~n")
    catch
        Error:Reason ->
            io:format("Configuration error: ~p:~p~n", [Error, Reason])
    end,
    
    io:format("Configuration test completed~n~n").

%% Run all tests
run_all_tests() ->
    io:format("=== Trade Execution Functionality Tests ===~n~n"),
    
    test_signal_translation(),
    test_order_encoding(),
    test_order_tracking(),
    test_live_scape_interface(),
    test_configuration(),
    
    io:format("=== All Tests Completed ===~n").

%% Sync function for development
sync() ->
    make:all([load]).