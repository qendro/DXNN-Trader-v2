%% Simple compilation test for the bridge
-module(test_bridge_compile).
-compile(export_all).

test_compile() ->
    io:format("Testing bridge compilation...~n"),
    
    %% Try to compile the bridge connector
    case compile:file(ib_bridge_connector) of
        {ok, _} ->
            io:format("✓ ib_bridge_connector compiled successfully~n"),
            test_json_functions();
        {error, Errors} ->
            io:format("✗ Compilation failed: ~p~n", [Errors])
    end.

test_json_functions() ->
    io:format("Testing JSON functions...~n"),
    
    %% Test JSON encoding
    TestMap = #{type => <<"connect">>, cid => 1, host => <<"127.0.0.1">>},
    try
        Json = ib_bridge_connector:encode_json(TestMap),
        io:format("✓ JSON encode works: ~p~n", [Json]),
        
        %% Test JSON decoding
        Decoded = ib_bridge_connector:decode_json(Json),
        io:format("✓ JSON decode works: ~p~n", [Decoded])
    catch
        _:Error ->
            io:format("✗ JSON functions failed: ~p~n", [Error])
    end.