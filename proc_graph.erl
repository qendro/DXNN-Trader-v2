%% proc_graph.erl
%% Build a process graph starting from an entry function and write Graphviz DOT.

-module(proc_graph).
-compile(export_all).

-record(state, {
    nodes = #{},          %% Pid -> #{mfa => MFA | undefined}
    edges = #{},          %% {FromPid, ToPid, Type} -> true
    root  = undefined     %% Root Pid
}).

%% Public API
%% Run the entrypoint {Mod,Fun,Args}, trace its whole spawn tree, and write OutFile DOT.
run(Mod, Fun, Args, OutFile) ->
    Root = spawn(fun() ->
        apply(Mod, Fun, Args)
    end),
    State0 = #state{nodes = #{Root => #{mfa => {Mod,Fun,length(Args)}}}, root = Root},

    %% Start tracing Root (and descendants via set_on_spawn).
    _ = erlang:trace(Root, true, [procs, send, timestamp, set_on_spawn, set_on_link]),

    %% Collect events until root exits OR timeout (10s).
    State1 = loop(State0, erlang:monotonic_time(millisecond), 10000),
    timer:sleep(50),
    State2 = drain(State1),

    %% Turn tracing off. Return value is an int; ignore it.
    _ = erlang:trace(all, false, [procs, send, timestamp]),

    write_dot(OutFile, State2),
    OutFile.

%% Internal receive loop with timeout tracking.
loop(State = #state{root = Root}, StartMs, TimeoutMs) ->
    Now = erlang:monotonic_time(millisecond),
    Remaining = TimeoutMs - (Now - StartMs),
    if
        Remaining =< 0 ->
            State;
        true ->
            receive
                {trace, Pid, spawn, Child, {M,F,A}} ->
                    State1 = add_node_edge(State, Pid, Child, spawn, {M,F,A}),
                    _ = erlang:trace(Child, true,
                                     [procs, send, timestamp, set_on_spawn, set_on_link]),
                    loop(State1, StartMs, TimeoutMs);

                {trace, Pid, link, OtherPid} ->
                    loop(add_edge(State, Pid, OtherPid, link), StartMs, TimeoutMs);

                {trace, Pid, send, _Msg, Dest} ->
                    loop(add_edge(State, Pid, Dest, send), StartMs, TimeoutMs);

                {trace, Pid, exit, _Reason} when Pid =:= Root ->
                    State;

                {trace, _Pid, exit, _Reason} ->
                    loop(State, StartMs, TimeoutMs);

                _Other ->
                    loop(State, StartMs, TimeoutMs)
            after Remaining ->
                State
            end
    end.

%% Drain any remaining trace msgs quickly after root exit/timeout.
drain(State) ->
    receive
        {trace, Pid, spawn, Child, MFA} ->
            drain(add_node_edge(State, Pid, Child, spawn, MFA));
        {trace, Pid, link, OtherPid} ->
            drain(add_edge(State, Pid, OtherPid, link));
        {trace, Pid, send, _Msg, Dest} ->
            drain(add_edge(State, Pid, Dest, send));
        _ ->
            drain(State)
    after 0 ->
            State
    end.

%% State helpers
add_node_edge(State = #state{nodes = Ns}, From, To, Type, MFA) ->
    Ns1 = maps:put(From, maps:get(From, Ns, #{}), Ns),
    Ns2 = maps:put(To,   #{mfa => MFA}, Ns1),
    add_edge(State#state{nodes = Ns2}, From, To, Type).

add_edge(State = #state{edges = Es}, From, To, Type) ->
    State#state{edges = maps:put({From,To,Type}, true, Es)}.

%% Write Graphviz DOT
write_dot(OutFile, #state{nodes = Ns, edges = Es, root = Root}) ->
    Lines0 = [
      "digraph ProcGraph {",
      "  rankdir=LR;",
      "  node [shape=circle, fontsize=10];",
      io_lib:format("  \"~p\" [label=\"~s\\n~p\", style=filled, fillcolor=lightgoldenrod];~n",
                    [Root, "root", Root])
    ],
    NodeLines = [ node_line(Pid, Meta) || {Pid, Meta} <- maps:to_list(Ns), Pid =/= Root ],
    EdgeLines = [ edge_line(From, To, Type) || {{From,To,Type}, true} <- maps:to_list(Es) ],
    Lines = lists:flatten([Lines0, NodeLines, EdgeLines, "}\n"]),
    ok = write_file(OutFile, iolist_to_binary(Lines)).

node_line(Pid, Meta) ->
    Label =
        case Meta of
            #{mfa := {M,F,A}} ->
                io_lib:format("~p\\n~p:~p/~p", [Pid, M, F, A]);
            _ ->
                io_lib:format("~p", [Pid])
        end,
    io_lib:format("  \"~p\" [label=\"~s\"];~n", [Pid, Label]).

edge_line(From, To, spawn) ->
    io_lib:format("  \"~p\" -> \"~p\" [label=\"spawn\"];~n", [From, To]);
edge_line(From, To, link)  ->
    io_lib:format("  \"~p\" -> \"~p\" [style=dashed,label=\"link\"];~n", [From, To]);
edge_line(From, To, send)  ->
    io_lib:format("  \"~p\" -> \"~p\" [style=dotted,color=gray,label=\"send\"];~n", [From, To]).

write_file(Path, Bin) ->
    case file:write_file(Path, Bin) of
        ok -> ok;
        Error -> exit({write_dot_failed, Path, Error})
    end.
