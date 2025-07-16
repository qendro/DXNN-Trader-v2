%% Simple test script for data loading functionality
-module(test_data_loading).
-compile(export_all).

%% Include the technical record definition
-record(technical,{
    id,%%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open,
    high,
    low,
    close,
    volume}).

%% Quick test function that can be run in Erlang shell
quick_test() ->
    io:format("=== Testing FX Data Loading Functionality ===~n"),
    
    % Create a simple test CSV file
    TestData = "2023-01-01 00:00,1.0500,1.0520,1.0495,1.0510,1000\n" ++
               "2023-01-01 01:00,1.0510,1.0530,1.0505,1.0520,1200\n" ++
               "2023-01-01 02:00,1.0520,1.0540,1.0515,1.0535,1100\n",
    
    TestFile = "quick_test.csv",
    file:write_file(TestFile, TestData),
    
    % Test the loading function
    io:format("1. Testing load_data_file/1...~n"),
    case fx:load_data_file(TestFile) of
        {ok, TableName} ->
            io:format("   SUCCESS: Data loaded into table ~p~n", [TableName]),
            
            % Check table size
            Size = fx:table_size(TableName),
            io:format("   Table contains ~p records~n", [Size]),
            
            % Test a lookup
            FirstKey = ets:first(TableName),
            Record = fx:lookup(TableName, FirstKey),
            io:format("   First record: Open=~p, Close=~p~n", [Record#technical.open, Record#technical.close]),
            
            % Clean up
            fx:delete_temporary_table(TableName),
            io:format("   Table cleaned up successfully~n");
        {error, Reason} ->
            io:format("   ERROR: ~p~n", [Reason])
    end,
    
    % Test file not found
    io:format("2. Testing file not found error...~n"),
    case fx:load_data_file("nonexistent.csv") of
        {error, {file_not_found, _}} ->
            io:format("   SUCCESS: File not found error handled correctly~n");
        Other ->
            io:format("   ERROR: Expected file_not_found but got ~p~n", [Other])
    end,
    
    % Test invalid format
    io:format("3. Testing invalid format error...~n"),
    InvalidData = "invalid,data\n",
    InvalidFile = "invalid_test.csv",
    file:write_file(InvalidFile, InvalidData),
    
    case fx:load_data_file(InvalidFile) of
        {error, {invalid_format, _}} ->
            io:format("   SUCCESS: Invalid format error handled correctly~n");
        Other2 ->
            io:format("   ERROR: Expected invalid_format but got ~p~n", [Other2])
    end,
    
    % Clean up test files
    file:delete(TestFile),
    file:delete(InvalidFile),
    
    io:format("=== Test completed ===~n").

%% Test individual functions
test_validation() ->
    io:format("Testing validate_data_format/1...~n"),
    
    % Create a valid test file
    TestData = "2023-01-01 00:00,1.0500,1.0520,1.0495,1.0510,1000\n",
    TestFile = "validation_test.csv",
    file:write_file(TestFile, TestData),
    
    case fx:validate_data_format(TestFile) of
        ok ->
            io:format("   SUCCESS: Valid file format recognized~n");
        Error ->
            io:format("   ERROR: ~p~n", [Error])
    end,
    
    file:delete(TestFile).

test_table_management() ->
    io:format("Testing table management...~n"),
    
    % Test table creation
    TableName = fx:generate_temp_table_name(),
    io:format("   Generated table name: ~p~n", [TableName]),
    
    case fx:create_temporary_table(TableName) of
        {ok, TableName} ->
            io:format("   SUCCESS: Table created~n"),
            
            % Test table deletion
            fx:delete_temporary_table(TableName),
            io:format("   SUCCESS: Table deleted~n");
        Error ->
            io:format("   ERROR: ~p~n", [Error])
    end.

%% Run all tests
run_all() ->
    quick_test(),
    test_validation(),
    test_table_management(),
    io:format("All tests completed!~n").