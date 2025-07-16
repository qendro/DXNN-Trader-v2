%% Test module for fx data file loading functionality
-module(fx_data_loading_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% Test data file loading functionality
test_data_file_loading() ->
    % Test 1: Create a valid CSV file and test loading
    test_valid_csv_loading(),
    
    % Test 2: Test file not found error
    test_file_not_found(),
    
    % Test 3: Test invalid CSV format
    test_invalid_csv_format(),
    
    % Test 4: Test empty file
    test_empty_file(),
    
    % Test 5: Test table creation and deletion
    test_table_management(),
    
    io:format("All data file loading tests completed successfully!~n").

%% Test loading a valid CSV file
test_valid_csv_loading() ->
    io:format("Testing valid CSV loading...~n"),
    
    % Create a test CSV file
    TestData = "2023-01-01 00:00,1.0500,1.0520,1.0495,1.0510,1000\n" ++
               "2023-01-01 01:00,1.0510,1.0530,1.0505,1.0520,1200\n" ++
               "2023-01-01 02:00,1.0520,1.0540,1.0515,1.0535,1100\n",
    
    TestFile = "test_valid_data.csv",
    file:write_file(TestFile, TestData),
    
    % Test the loading function
    case fx:load_data_file(TestFile) of
        {ok, TableName} ->
            io:format("Successfully loaded data into table: ~p~n", [TableName]),
            
            % Verify table has data
            Size = fx:table_size(TableName),
            ?assert(Size == 3),
            io:format("Table size verified: ~p records~n", [Size]),
            
            % Test a lookup
            FirstKey = ets:first(TableName),
            Record = fx:lookup(TableName, FirstKey),
            ?assert(Record#technical.open == 1.0500),
            ?assert(Record#technical.close == 1.0510),
            io:format("Data integrity verified~n"),
            
            % Clean up
            fx:delete_temporary_table(TableName),
            file:delete(TestFile),
            io:format("Valid CSV loading test passed!~n");
        {error, Reason} ->
            file:delete(TestFile),
            ?assert(false, io_lib:format("Expected success but got error: ~p", [Reason]))
    end.

%% Test file not found error
test_file_not_found() ->
    io:format("Testing file not found error...~n"),
    
    case fx:load_data_file("nonexistent_file.csv") of
        {error, {file_not_found, _}} ->
            io:format("File not found error handled correctly~n");
        Other ->
            ?assert(false, io_lib:format("Expected file_not_found error but got: ~p", [Other]))
    end.

%% Test invalid CSV format
test_invalid_csv_format() ->
    io:format("Testing invalid CSV format...~n"),
    
    % Test with wrong number of fields
    InvalidData = "2023-01-01 00:00,1.0500,1.0520\n",  % Only 3 fields instead of 6
    TestFile = "test_invalid_format.csv",
    file:write_file(TestFile, InvalidData),
    
    case fx:load_data_file(TestFile) of
        {error, {invalid_format, _}} ->
            io:format("Invalid format error handled correctly~n");
        Other ->
            ?assert(false, io_lib:format("Expected invalid_format error but got: ~p", [Other]))
    end,
    
    file:delete(TestFile).

%% Test empty file
test_empty_file() ->
    io:format("Testing empty file...~n"),
    
    TestFile = "test_empty.csv",
    file:write_file(TestFile, ""),
    
    case fx:load_data_file(TestFile) of
        {error, {invalid_format, "Empty file"}} ->
            io:format("Empty file error handled correctly~n");
        Other ->
            ?assert(false, io_lib:format("Expected empty file error but got: ~p", [Other]))
    end,
    
    file:delete(TestFile).

%% Test table creation and deletion
test_table_management() ->
    io:format("Testing table management...~n"),
    
    % Test table creation
    TableName = fx:generate_temp_table_name(),
    case fx:create_temporary_table(TableName) of
        {ok, TableName} ->
            io:format("Table created successfully: ~p~n", [TableName]),
            
            % Verify table exists
            ?assert(ets:info(TableName) =/= undefined),
            
            % Test table deletion
            fx:delete_temporary_table(TableName),
            
            % Verify table is deleted
            ?assert(ets:info(TableName) == undefined),
            io:format("Table management test passed!~n");
        {error, Reason} ->
            ?assert(false, io_lib:format("Table creation failed: ~p", [Reason]))
    end.

%% Test CSV parsing edge cases
test_csv_parsing_edge_cases() ->
    io:format("Testing CSV parsing edge cases...~n"),
    
    % Test with different timestamp formats
    TestData1 = "2023-01-01 00:00:00,1.0500,1.0520,1.0495,1.0510,1000\n",
    TestFile1 = "test_timestamp_format.csv",
    file:write_file(TestFile1, TestData1),
    
    case fx:load_data_file(TestFile1) of
        {ok, TableName1} ->
            io:format("Timestamp with seconds parsed correctly~n"),
            fx:delete_temporary_table(TableName1);
        {error, Reason1} ->
            io:format("Warning: Timestamp with seconds failed: ~p~n", [Reason1])
    end,
    
    file:delete(TestFile1),
    
    % Test with whitespace
    TestData2 = " 2023-01-01 00:00 , 1.0500 , 1.0520 , 1.0495 , 1.0510 , 1000 \n",
    TestFile2 = "test_whitespace.csv",
    file:write_file(TestFile2, TestData2),
    
    case fx:load_data_file(TestFile2) of
        {ok, TableName2} ->
            io:format("Whitespace handling works correctly~n"),
            fx:delete_temporary_table(TableName2);
        {error, Reason2} ->
            io:format("Warning: Whitespace handling failed: ~p~n", [Reason2])
    end,
    
    file:delete(TestFile2).

%% Test data validation
test_data_validation() ->
    io:format("Testing data validation...~n"),
    
    % Test with invalid price values (too high)
    InvalidData = "2023-01-01 00:00,1000.0500,1000.0520,1000.0495,1000.0510,1000\n",
    TestFile = "test_invalid_prices.csv",
    file:write_file(TestFile, InvalidData),
    
    case fx:load_data_file(TestFile) of
        {error, {parse_error, _, _}} ->
            io:format("Price validation works correctly~n");
        {ok, TableName} ->
            % If it loads, clean up
            fx:delete_temporary_table(TableName),
            io:format("Note: High price values were accepted~n");
        Other ->
            io:format("Unexpected result for price validation: ~p~n", [Other])
    end,
    
    file:delete(TestFile).

%% Run all tests
run_all_tests() ->
    io:format("=== Starting FX Data Loading Tests ===~n"),
    
    test_data_file_loading(),
    test_csv_parsing_edge_cases(),
    test_data_validation(),
    
    io:format("=== All FX Data Loading Tests Completed ===~n").

%% Helper function to create sample data files for manual testing
create_sample_data_files() ->
    % Create a small sample file
    SmallData = "2023-01-01 00:00,1.0500,1.0520,1.0495,1.0510,1000\n" ++
                "2023-01-01 01:00,1.0510,1.0530,1.0505,1.0520,1200\n" ++
                "2023-01-01 02:00,1.0520,1.0540,1.0515,1.0535,1100\n" ++
                "2023-01-01 03:00,1.0535,1.0550,1.0530,1.0545,1300\n" ++
                "2023-01-01 04:00,1.0545,1.0560,1.0540,1.0555,1150\n",
    
    file:write_file("sample_small.csv", SmallData),
    
    % Create a larger sample file
    LargeData = lists:flatten([
        io_lib:format("2023-01-01 ~2..0w:00,~.5f,~.5f,~.5f,~.5f,~w~n", 
                     [Hour, 1.0500 + Hour*0.0001, 1.0520 + Hour*0.0001, 
                      1.0495 + Hour*0.0001, 1.0510 + Hour*0.0001, 1000 + Hour*10])
        || Hour <- lists:seq(0, 23)
    ]),
    
    file:write_file("sample_large.csv", LargeData),
    
    io:format("Sample data files created: sample_small.csv, sample_large.csv~n").