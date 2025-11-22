-module(launcher).
-export([start/0, start/1]).

%% Public API
start() ->
    start(sliding_window_5).

start(BenchmarkId) ->
    fx:clear_log(),
    qlog:benchmarker(BenchmarkId, "Launcher starting"),
    
    % Compile all modules
    case make:all([load]) of
        up_to_date -> ok;
        ok -> ok;
        Error -> exit({make_failed, Error})
    end,
    
    % Initialize Mnesia
    maybe_create_schema(),
    qlog:benchmarker(BenchmarkId, "Schema created"),
    mnesia:start(),
    qlog:benchmarker(BenchmarkId, "Mnesia started"),
    
    % Initialize FX and Polis
    fx:init(),
    qlog:benchmarker(BenchmarkId, "FX initialized"),
    polis:create(),
    qlog:benchmarker(BenchmarkId, "Polis created"),
    polis:start(),
    qlog:benchmarker(BenchmarkId, "Polis started"),
    polis:sync(),
    qlog:benchmarker(BenchmarkId, "Polis synced"),
    
    ensure_fx_complete(BenchmarkId).

%% Wait until FX signals readiness, then kick off the benchmarker
ensure_fx_complete(BenchmarkId) ->
    case whereis(fx) of
        undefined ->
            qlog:benchmarker(BenchmarkId, "Starting FX process..."),
            register(launcher, self()),     % <<< register this process as the launcher
            fx:start(),
            wait_fx_ready(BenchmarkId);
        _Pid ->
            qlog:benchmarker(BenchmarkId, "FX process already running"),
            benchmarker:start(BenchmarkId)
    end.

%% Blocks until we see an fx_updated message
wait_fx_ready(BenchmarkId) ->
    receive
        fx_updated ->
            qlog:benchmarker(BenchmarkId, "FX ready"),
            benchmarker:start(BenchmarkId)
    end.

%% Create Mnesia schema if it doesn't exist
maybe_create_schema() ->
    case mnesia:create_schema([node()]) of
        ok -> 
            ok;
        {error, {_, {already_exists, _}}} -> 
            ok;
        {error, {already_exists, _}} -> 
            ok;
        {error, Reason} -> 
            exit({schema_create_failed, Reason})
    end.
