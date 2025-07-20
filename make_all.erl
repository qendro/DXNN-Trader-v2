-module(make_all).
-export([all/0, core/0, evolution/0, substrate/0, utils/0]).

all() ->
    ensure_ebin_dir(),
    core(),
    evolution(), 
    substrate(),
    utils(),
    io:format("All modules compiled!~n").

core() ->
    compile_dir("src/core").

evolution() ->
    compile_dir("src/evolution").

substrate() ->
    compile_dir("src/substrate").

utils() ->
    compile_dir("src/utils").

compile_dir(Dir) ->
    Files = filelib:wildcard(Dir ++ "/*.erl"),
    Options = [{outdir, "build/ebin"}, {i, "src"}],
    io:format("Compiling ~s (~p files)...~n", [Dir, length(Files)]),
    [compile:file(File, Options) || File <- Files].

ensure_ebin_dir() ->
    filelib:ensure_dir("build/ebin/dummy").