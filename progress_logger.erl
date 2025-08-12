-module(progress_logger).
-compile(export_all).

-define(TAB, progress_logger_tab).
-define(DEFAULT_DIR, "logs").
-define(DEFAULT_FILE, "dxnn_run.log").

start() ->
    ensure_log_dir(),
    case ets:info(?TAB) of
        undefined -> ets:new(?TAB, [named_table, public, set]);
        _ -> ok
    end, ok.

mark_launch() ->
    start(),
    StartMs = erlang:monotonic_time(millisecond),
    ets:insert(?TAB, {start_time_ms, StartMs}),
    ets:insert(?TAB, {total_evals, undefined}),
    ets:insert(?TAB, {done_evals, 0}),
    ets:insert(?TAB, {iter, 0}),
    ets:insert(?TAB, {log_path, filename:join(?DEFAULT_DIR, ?DEFAULT_FILE)}),
    log("Program launched. start_ms=~p wall=~s", [StartMs, wallclock_str()]).

set_iteration(Iter) when is_integer(Iter), Iter >= 0 ->
    start(), ets:insert(?TAB, {iter, Iter}),
    log("Evolution iteration -> ~p", [Iter]), ok.
get_iteration() ->
    case ets:lookup(?TAB, iter) of [{iter,V}] -> V; _ -> 0 end.

set_total_evals(inf) ->
    start(), ets:insert(?TAB, {total_evals, inf}),
    ets:insert(?TAB, {done_evals, 0}), log("Total evaluations set: inf", []), ok;
set_total_evals(T) when is_integer(T), T >= 0 ->
    start(), ets:insert(?TAB, {total_evals, T}),
    ets:insert(?TAB, {done_evals, 0}), log("Total evaluations set: ~p", [T]), ok.

inc_done_eval() ->
    start(),
    D0 = case ets:lookup(?TAB, done_evals) of [{done_evals,D}] -> D; _ -> 0 end,
    D1 = D0 + 1, ets:insert(?TAB, {done_evals, D1}),
    maybe_progress_log(D1), ok.

get_progress() ->
    T = case ets:lookup(?TAB, total_evals) of [{total_evals,X}] -> X; _ -> undefined end,
    D = case ets:lookup(?TAB, done_evals)  of [{done_evals,Y}]  -> Y; _ -> 0 end, {D,T}.

eta() ->
    StartMs = case ets:lookup(?TAB, start_time_ms) of [{start_time_ms,S}] -> S; _ -> erlang:monotonic_time(millisecond) end,
    {D,T} = get_progress(), Now = erlang:monotonic_time(millisecond), Elap = Now - StartMs,
    case {D,T} of
        {_, undefined} -> {Elap div 1000, undefined};
        {_, inf}       -> {Elap div 1000, undefined};
        {0, _}         -> {Elap div 1000, undefined};
        {Dd, Tt} when is_integer(Tt), Tt > 0 ->
            Rate = Dd / (Elap / 1000.0),
            RemS = case Rate =:= 0.0 of true -> undefined; false -> (Tt - Dd) / Rate end,
            {Elap div 1000, RemS}
    end.

status_str() ->
    Iter = get_iteration(), {D,T} = get_progress(), {El,Et} = eta(),
    {PctStr, TotStr} = case T of
        undefined -> {"n/a","n/a"};
        inf       -> {"n/a","inf"};
        X when X>0 -> {io_lib:format("~.1f%%",[(D*100.0)/X]), integer_to_list(X)}
    end,
    io_lib:format("Iter=~p | done=~p/~s (~s) | elapsed=~ps | eta=~s",
                  [Iter, D, TotStr, PctStr, El, eta_str(Et)]).

%% helpers
maybe_progress_log(Done) ->
    {_, T} = get_progress(),
    case (Done rem 100 =:= 0) orelse (T =/= undefined andalso T =/= inf andalso Done >= T) of
        true -> log("Progress: ~s", [status_str()]);
        false -> ok
    end.
eta_str(undefined) -> "n/a";
eta_str(S) when is_number(S) ->
    S1=trunc(S), H=S1 div 3600, M=(S1 rem 3600) div 60, R=S1 rem 60,
    io_lib:format("~2..0B:~2..0B:~2..0B",[H,M,R]).
log(Fmt, Args) ->
    Path = case ets:lookup(?TAB, log_path) of [{log_path,P}] -> P; _ -> filename:join(?DEFAULT_DIR, ?DEFAULT_FILE) end,
    Ts = wallclock_str(), Line = io_lib:format("[~s] "++Fmt++"~n", [Ts|Args]),
    ok = append_file(Path, Line), io:format(Line), ok.
append_file(Path, Iolist) ->
    case file:open(Path, [append,{delayed_write,4096,1000}]) of
        {ok,Io} -> io:put_chars(Io, Iolist), file:close(Io), ok;
        _ -> io:format("[progress_logger] open failed ~s~n",[Path]), ok
    end.
ensure_log_dir() ->
    case file:read_file_info(?DEFAULT_DIR) of {ok,_}->ok; _-> file:make_dir(?DEFAULT_DIR), ok end.
wallclock_str() ->
    Sec=erlang:system_time(second),
    {{Y,Mo,D},{H,Mi,S}}=calendar:system_time_to_universal_time(Sec,second),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Y,Mo,D,H,Mi,S])).
