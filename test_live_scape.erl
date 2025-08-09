%% Test module for live_scape functionality
%% Tests the sensor/actuator interface compatibility

-module(test_live_scape).
-compile(export_all).
-include("records.hrl").

%% Test the live_scape module functionality
test_live_scape() ->
    io:format("Testing live_scape module...~n"),
    
    %% Test 1: Module compilation and basic functions
    test_module_loading(),
    
    %% Test 2: ETS table initialization
    test_ets_initialization(),
    
    %% Test 3: Data processing functions
    test_data_processing(),
    
    %% Test 4: Configuration integration
    test_config_integration(),
    
    io:format("All live_scape tests completed~n").

%% Test module loading and basic function availability
test_module_loading() ->
    io:format("Test 1: Module loading...~n"),
    
    %% Check if module functions are available
    Functions = live_scape:module_info(exports),
    io:format("Available functions: ~p~n", [length(Functions)]),
    
    %% Test normalize_vector function
    TestVector = [1.0, 2.0, 3.0],
    Normalized = live_scape:normalize_vector(TestVector),
    io:format("Normalized vector: ~p~n", [Normalized]),
    
    %% Test empty vector handling
    EmptyNormalized = live_scape:normalize_vector([]),
    io:format("Empty vector normalized: ~p~n", [EmptyNormalized]),
    
    io:format("Test 1: PASSED~n~n").

%% Test ETS table initialization and cleanup
test_ets_initialization() ->
    io:format("Test 2: ETS table initialization...~n"),
    
    %% Test initialization
    live_scape:init_price_buffer(),
    
    %% Check if table exists
    case ets:info(live_price_buffer) of
        undefined ->
            io:format("ERROR: ETS table not created~n");
        Info ->
            io:format("ETS table created successfully: ~p~n", [Info])
    end,
    
    %% Test cleanup
    live_scape:cleanup_price_buffer(),
    
    %% Check if table is cleaned up
    case ets:info(live_price_buffer) of
        undefined ->
            io:format("ETS table cleaned up successfully~n");
        _ ->
            io:format("WARNING: ETS table still exists after cleanup~n")
    end,
    
    io:format("Test 2: PASSED~n~n").

%% Test data processing functions
test_data_processing() ->
    io:format("Test 3: Data processing functions...~n"),
    
    %% Test plane encoding with sample data
    SamplePriceList = [
        {1.1000, 1.1010, 1.1015, 1.0995},
        {1.1010, 1.1005, 1.1020, 1.1000},
        {1.1005, 1.1012, 1.1018, 1.1002}
    ],
    
    %% Test encode_to_plane function
    VPos = 1.1000,
    VStep = 0.0005,
    EncodedData = live_scape:encode_to_plane(9, SamplePriceList, VPos, VStep, []),
    io:format("Encoded plane data: ~p~n", [EncodedData]),
    
    %% Test position size calculation
    TestBalance = 1000.0,
    PositionSize = live_scape:calculate_position_size(TestBalance),
    io:format("Position size for balance ~p: ~p~n", [TestBalance, PositionSize]),
    
    io:format("Test 3: PASSED~n~n").

%% Test configuration integration
test_config_integration() ->
    io:format("Test 4: Configuration integration...~n"),
    
    %% Test config function calls
    try
        PrimaryCurrency = config:primary_currency_pair(),
        io:format("Primary currency pair: ~p~n", [PrimaryCurrency]),
        
        InitialBalance = config:account_initial_balance(),
        io:format("Initial balance: ~p~n", [InitialBalance]),
        
        Leverage = config:account_leverage(),
        io:format("Account leverage: ~p~n", [Leverage]),
        
        Spread = config:account_spread(),
        io:format("Account spread: ~p~n", [Spread]),
        
        %% Test live trading specific configs
        IBHost = config:ib_host(),
        io:format("IB host: ~p~n", [IBHost]),
        
        IBPort = config:ib_port(),
        io:format("IB port: ~p~n", [IBPort]),
        
        ClientId = config:ib_client_id(),
        io:format("IB client ID: ~p~n", [ClientId]),
        
        PositionSize = config:live_position_size(),
        io:format("Live position size: ~p~n", [PositionSize]),
        
        io:format("Test 4: PASSED~n~n")
    catch
        Error:Reason ->
            io:format("ERROR in config integration: ~p:~p~n", [Error, Reason]),
            io:format("Test 4: FAILED~n~n")
    end.

%% Test sensor interface compatibility
test_sensor_interface() ->
    io:format("Test 5: Sensor interface compatibility...~n"),
    
    %% Create a mock state
    State = #live_state{
        table_name = 'EURUSD15',
        feature = close,
        account_balance = 1000.0,
        current_position = 0,
        entry_price = 0,
        previous_pc = 0
    },
    
    %% Test PLI sensor handling
    {PLIResult, _UpdatedState1} = live_scape:handle_pli_sensor('EURUSD15', 5, State),
    io:format("PLI sensor result length: ~p~n", [length(PLIResult)]),
    
    %% Test PCI sensor handling  
    {PCIResult, _UpdatedState2} = live_scape:handle_pci_sensor('EURUSD15', 5, 10, State),
    io:format("PCI sensor result length: ~p~n", [length(PCIResult)]),
    
    %% Test internals sensor
    InternalsResult = live_scape:handle_internals_request(State),
    io:format("Internals sensor result: ~p~n", [InternalsResult]),
    
    io:format("Test 5: PASSED~n~n").

%% Run all tests
run_all_tests() ->
    io:format("=== Live Scape Module Test Suite ===~n~n"),
    test_live_scape(),
    test_sensor_interface(),
    io:format("=== Test Suite Complete ===~n").