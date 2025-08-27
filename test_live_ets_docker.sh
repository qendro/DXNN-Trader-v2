#!/bin/bash
# Simple Docker test script for Live ETS implementation
# Run this to test the implementation in your Docker container

echo "=== Live ETS Docker Test ==="
echo "Starting Docker container..."

# Build and run the container
docker build -t erlang-dev .
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev bash -c "
echo 'Inside Docker container...'
echo 'Compiling all modules...'
erl -make
echo 'Starting Erlang shell...'
erl -eval '
    % Load all modules
    make:all([load]).
    
    % Test configuration
    io:format(\"Testing configuration...~n\").
    Enabled = config:live_trading_enabled(),
    io:format(\"Live trading enabled: ~p~n\", [Enabled]).
    
    % Test live table creation
    io:format(\"Testing live table creation...~n\").
    live_scape:init_live_tables().
    
    % Test data insertion
    io:format(\"Testing data insertion...~n\").
    TestRecord = #technical{id = {2024,1,1,12,0,0,60}, open = 1.1, high = 1.101, low = 1.099, close = 1.1005, volume = 1000},
    ets:insert(live_EURUSD1, TestRecord).
    
    % Test lookup
    io:format(\"Testing data lookup...~n\").
    case ets:lookup(live_EURUSD1, {2024,1,1,12,0,0,60}) of
        [TestRecord] -> io:format(\"✓ Data test successful~n\");
        Other -> io:format(\"✗ Data test failed: ~p~n\", [Other])
    end.
    
    % Test performance monitoring
    io:format(\"Testing performance monitoring...~n\").
    live_scape:monitor_live_tables().
    
    % Cleanup
    io:format(\"Cleaning up...~n\").
    ets:delete(live_EURUSD1).
    
    io:format(\"=== Live ETS test completed ===~n\").
    halt().
' -noshell
"

echo "=== Docker test completed ==="
