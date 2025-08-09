%% Demonstration of live_scape functionality
%% Shows the sensor/actuator interface working without IB connector dependency

-module(demo_live_scape).
-compile(export_all).
-include("records.hrl").

%% Demonstrate the live_scape interface
demo() ->
    io:format("=== Live Scape Interface Demonstration ===~n~n"),
    
    %% Initialize the price buffer
    live_scape:init_price_buffer(),
    
    %% Create a sample state
    State = #live_state{
        table_name = 'EURUSD15',
        feature = close,
        account_balance = 1000.0,
        current_position = 0,
        entry_price = 0,
        previous_pc = 0
    },
    
    %% Demonstrate sensor interface compatibility
    demo_sensor_interface(State),
    
    %% Demonstrate data processing
    demo_data_processing(),
    
    %% Cleanup
    live_scape:cleanup_price_buffer(),
    
    io:format("=== Demonstration Complete ===~n").

%% Demonstrate sensor interface
demo_sensor_interface(State) ->
    io:format("1. Sensor Interface Demonstration:~n"),
    
    %% Test internals sensor (this works without IB connector)
    InternalsResult = live_scape:handle_internals_request(State),
    io:format("   Internals sensor result: ~p~n", [InternalsResult]),
    io:format("   Format: [Position, Entry_Price, Previous_PC]~n"),
    
    %% Show what the sensor interface expects
    io:format("   Expected sensor calls:~n"),
    io:format("   - fx_PLI sensor: {From, sense, TableName, Feature, [HRes, list_sensor], Start, Finish}~n"),
    io:format("   - fx_PCI sensor: {From, sense, TableName, Feature, [HRes, VRes, graph_sensor], Start, Finish}~n"),
    io:format("   - fx_Internals: {From, sense, internals, Parameters}~n~n").

%% Demonstrate data processing capabilities
demo_data_processing() ->
    io:format("2. Data Processing Demonstration:~n"),
    
    %% Test vector normalization
    TestVector = [1.0, 2.0, 3.0, 4.0, 5.0],
    Normalized = live_scape:normalize_vector(TestVector),
    io:format("   Original vector: ~p~n", [TestVector]),
    io:format("   Normalized vector: ~p~n", [Normalized]),
    
    %% Test plane encoding
    SamplePrices = [
        {1.1000, 1.1010, 1.1015, 1.0995},
        {1.1010, 1.1005, 1.1020, 1.1000},
        {1.1005, 1.1012, 1.1018, 1.1002}
    ],
    
    VPos = 1.1005,
    VStep = 0.0005,
    PlaneData = live_scape:encode_to_plane(9, SamplePrices, VPos, VStep, []),
    io:format("   Sample OHLC data: ~p~n", [SamplePrices]),
    io:format("   Encoded plane (3x3): ~p~n", [PlaneData]),
    io:format("   Encoding: 1=body, 0=wick, -1=background~n"),
    
    %% Test position sizing
    Balances = [100, 500, 1000, 5000],
    io:format("   Position sizing examples:~n"),
    [io:format("     Balance: ~p -> Position size: ~p~n", 
               [Balance, live_scape:calculate_position_size(Balance)]) 
     || Balance <- Balances],
    
    io:format("~n").

%% Show the complete interface that would be used by the neural network
demo_complete_interface() ->
    io:format("3. Complete Neural Network Interface:~n"),
    io:format("   The live_scape module provides these entry points:~n"),
    io:format("   - gen/2: Creates scape process (matches existing pattern)~n"),
    io:format("   - prep/1: Initializes scape process~n"),
    io:format("   - live_sim/1: Main scape function (replaces fx_sim/1)~n"),
    io:format("~n"),
    io:format("   Message handling:~n"),
    io:format("   - Sensor requests: Returns live market data in expected format~n"),
    io:format("   - Trade requests: Executes trades through IB connector~n"),
    io:format("   - Internal requests: Returns position/P&L state~n"),
    io:format("~n"),
    io:format("   Data compatibility:~n"),
    io:format("   - fx_PLI sensor: Returns normalized price lists~n"),
    io:format("   - fx_PCI sensor: Returns plane-encoded price charts~n"),
    io:format("   - fx_Trade actuator: Accepts -1/0/1 trade signals~n"),
    io:format("~n").

%% Run complete demonstration
run_demo() ->
    demo(),
    demo_complete_interface().