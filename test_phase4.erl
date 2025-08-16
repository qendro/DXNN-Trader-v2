-module(test_phase4).
-compile(export_all).

start() ->
    io:format("=== PHASE 4 TEST STARTING ===~n"),
    make:all([load]),
    io:format("Modules loaded successfully~n"),
    
    %% Try to compile our test modules
    case compile:file(test_phase4_comprehensive) of
        {ok, _} -> io:format("✓ test_phase4_comprehensive compiled~n");
        {error, E} -> io:format("✗ test_phase4_comprehensive failed: ~p~n", [E])
    end,
    
    case compile:file(run_phase4_tests) of
        {ok, _} -> io:format("✓ run_phase4_tests compiled~n");
        {error, E2} -> io:format("✗ run_phase4_tests failed: ~p~n", [E2])
    end,
    
    io:format("=== PHASE 4 TEST COMPLETE ===~n").
