%% Test module for market data functionality
-module(test_market_data).
-compile(export_all).
-include("records.hrl").

%% Test basic ETS table operations
test_ets_tables() ->
    io:format("Testing ETS table initialization...~n"),
    
    %% Test table creation directly (not through gen_server)
    case ib_connector:init_ets_tables() of
        ok ->
            io:format("ETS tables initialized successfully~n");
        {error, Reason} ->
            io:format("Failed to initialize ETS tables: ~p~n", [Reason])
    end,
    
    %% Test table cleanup directly
    case ib_connector:cleanup_ets_tables() of
        ok ->
            io:format("ETS tables cleaned up successfully~n");
        {error, Reason2} ->
            io:format("Failed to cleanup ETS tables: ~p~n", [Reason2])
    end.

%% Test market data processing functions
test_market_data_processing() ->
    io:format("Testing market data processing...~n"),
    
    %% Initialize tables first
    ib_connector:init_ets_tables(),
    
    %% Create a sample market tick
    SampleTick = #market_tick{
        symbol = "EUR.USD",
        timestamp = erlang:timestamp(),
        bid = 1.0850,
        ask = 1.0852,
        last = 1.0851,
        volume = 1000
    },
    
    %% Test data translation for sensors
    PLI_Data = ib_connector:translate_tick_to_sensor_format(SampleTick, fx_PLI),
    PCI_Data = ib_connector:translate_tick_to_sensor_format(SampleTick, fx_PCI),
    
    io:format("PLI sensor data: ~p~n", [PLI_Data]),
    io:format("PCI sensor data: ~p~n", [PCI_Data]),
    
    %% Cleanup
    ib_connector:cleanup_ets_tables().

%% Test OHLC data conversion
test_ohlc_conversion() ->
    io:format("Testing OHLC data conversion...~n"),
    
    %% Create sample OHLC data
    SampleOHLC = #live_ohlc{
        symbol = "EUR.USD",
        timestamp = erlang:timestamp(),
        open = 1.0850,
        high = 1.0855,
        low = 1.0848,
        close = 1.0852,
        volume = 5000,
        tick_count = 100
    },
    
    %% Test conversion to technical format
    TechnicalData = ib_connector:convert_ohlc_to_technical(SampleOHLC),
    io:format("Technical data format: ~p~n", [TechnicalData]).

%% Run all tests
run_all_tests() ->
    io:format("=== Running Market Data Tests ===~n"),
    test_ets_tables(),
    test_market_data_processing(),
    test_ohlc_conversion(),
    io:format("=== All tests completed ===~n").