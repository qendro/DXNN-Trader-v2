%% Phase 1 Test Suite - Minimal Viable Bridge
-module(test_phase1).
-compile(export_all).

%% Main test function
test_all() ->
    io:format("=== Phase 1: Minimal Viable Bridge Test ===~n"),
    
    %% Step 1: Test compilation
    test_compilation(),
    
    %% Step 2: Test Python dependencies
    test_python_deps(),
    
    %% Step 3: Test JSON functions
    test_json_functions(),
    
    %% Step 4: Test bridge startup (without IB connection)
    test_bridge_startup_basic(),
    
    io:format("=== Phase 1 Test Complete ===~n").

%% Test compilation
test_compilation() ->
    io:format("~n1. Testing Compilation:~n"),
    
    case compile:file(ib_bridge_connector) of
        {ok, _} ->
            io:format("   ✓ ib_bridge_connector compiled successfully~n");
        {error, Errors} ->
            io:format("   ✗ Compilation failed: ~p~n", [Errors])
    end.

%% Test Python dependencies
test_python_deps() ->
    io:format("~n2. Testing Python Dependencies:~n"),
    
    case os:find_executable("python3") of
        false ->
            io:format("   ✗ python3 not found~n");
        Python3 ->
            io:format("   ✓ python3 found: ~s~n", [Python3]),
            
            %% Test Python script
            TestScript = "./test_python_deps.py",
            case filelib:is_file(TestScript) of
                true ->
                    case os:cmd("python3 " ++ TestScript) of
                        Result ->
                            io:format("   Python deps test result:~n~s~n", [Result])
                    end;
                false ->
                    io:format("   ⚠ Python test script not found~n")
            end
    end.

%% Test JSON functions
test_json_functions() ->
    io:format("~n3. Testing JSON Functions:~n"),
    
    %% Test encoding
    TestMap = #{type => <<"connect">>, cid => 1, host => <<"127.0.0.1">>},
    try
        Json = ib_bridge_connector:encode_json(TestMap),
        io:format("   ✓ JSON encode works: ~s~n", [Json]),
        
        %% Test decoding
        try
            Decoded = ib_bridge_connector:decode_json(Json),
            io:format("   ✓ JSON decode works: ~p~n", [Decoded])
        catch
            _:DecodeError ->
                io:format("   ✗ JSON decode failed: ~p~n", [DecodeError])
        end
    catch
        _:EncodeError ->
            io:format("   ✗ JSON encode failed: ~p~n", [EncodeError])
    end.

%% Test basic bridge startup (without IB connection)
test_bridge_startup_basic() ->
    io:format("~n4. Testing Bridge Startup (Basic):~n"),
    
    %% Check if Python script exists
    Script = filename:join(code:priv_dir(dxnn), "ib_service.py"),
    case filelib:is_file(Script) of
        true ->
            io:format("   ✓ Python bridge script found: ~s~n", [Script]);
        false ->
            io:format("   ✗ Python bridge script not found: ~s~n", [Script])
    end,
    
    %% Test port creation (without actual connection)
    try
        Python3 = os:find_executable("python3"),
        case Python3 of
            false ->
                io:format("   ✗ Cannot test port - python3 not found~n");
            _ ->
                io:format("   ✓ Python executable available for port creation~n")
        end
    catch
        _:Error ->
            io:format("   ✗ Port test failed: ~p~n", [Error])
    end.

%% Quick validation test
quick_test() ->
    io:format("Quick Phase 1 validation...~n"),
    
    %% Check compilation
    case compile:file(ib_bridge_connector) of
        {ok, _} -> io:format("✓ Compilation OK~n");
        _ -> io:format("✗ Compilation failed~n")
    end,
    
    %% Check Python
    case os:find_executable("python3") of
        false -> io:format("✗ Python3 not found~n");
        _ -> io:format("✓ Python3 OK~n")
    end,
    
    %% Check script
    Script = filename:join(code:priv_dir(dxnn), "ib_service.py"),
    case filelib:is_file(Script) of
        true -> io:format("✓ Bridge script OK~n");
        false -> io:format("✗ Bridge script missing~n")
    end.