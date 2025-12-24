%% Quick Start Script for System Profiling
%% 
%% Usage in Erlang shell:
%%   1. c(system_profiler). c(profile_quick_start).
%%   2. profile_quick_start:go(5).  % Profile for 5 seconds
%%   3. Run your experiment: exp_runner:start(new_evo)
%%   4. Wait for completion
%%   5. profile_quick_start:report().

-module(profile_quick_start).
-compile(export_all).

go(Seconds) ->
    io:format("~n========================================~n"),
    io:format("QUICK START PROFILING~n"),
    io:format("========================================~n~n"),
    io:format("Starting profiler for ~p seconds...~n", [Seconds]),
    system_profiler:start(Seconds),
    io:format("~nProfiler is running! Now run your experiment:~n"),
    io:format("   exp_runner:start(new_evo)~n~n"),
    io:format("Profiler will auto-stop after ~p seconds.~n", [Seconds]),
    io:format("Then run: profile_quick_start:report()~n~n"),
    ok.

%% Attach to already-running system
attach(Seconds) ->
    io:format("~n========================================~n"),
    io:format("ATTACH PROFILER TO RUNNING SYSTEM~n"),
    io:format("========================================~n~n"),
    io:format("Attaching profiler for ~p seconds...~n", [Seconds]),
    system_profiler:attach(Seconds),
    io:format("~nProfiler attached! Monitoring your running system.~n"),
    io:format("Profiler will auto-stop after ~p seconds.~n", [Seconds]),
    io:format("Then run: profile_quick_start:report()~n~n"),
    ok.

report() ->
    io:format("~nGenerating profiling report...~n~n"),
    system_profiler:report(),
    ok.

summary() ->
    system_profiler:summary().

stop() ->
    system_profiler:stop().



