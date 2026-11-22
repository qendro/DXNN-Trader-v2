-module(launcher).
-export([start/0, start/1]).

%% Public API
start() ->
    start(sliding_window_5).

start(BenchmarkId) ->
    fx:clear_log(),
    fx:log("Launcher starting"),
    
    % Compile all modules
    case make:all([load]) of
        up_to_date -> ok;
        ok -> ok;
        Error -> exit({make_failed, Error})
    end,
    
    % Initialize Mnesia
    maybe_create_schema(),
    fx:log("Schema created"),
    mnesia:start(),
    fx:log("Mnesia started"),
    
    % Initialize FX and Polis
    fx:init(),
    fx:log("FX initialized"),
    polis:create(),
    fx:log("Polis created"),
    polis:start(),
    fx:log("Polis started"),
    polis:sync(),
    fx:log("Polis synced"),
    
    ensure_fx_complete(BenchmarkId).

%% Wait until FX signals readiness, then kick off the benchmarker
ensure_fx_complete(BenchmarkId) ->
    case whereis(fx) of
        undefined ->
            fx:log("Starting FX process..."),
            register(launcher, self()),     % <<< register this process as the launcher
            fx:start(),
            wait_fx_ready(BenchmarkId);
        _Pid ->
            fx:log("FX process already running"),
            benchmarker:start(BenchmarkId)
    end.

%% Blocks until we see an fx_updated message
wait_fx_ready(BenchmarkId) ->
    receive
        fx_updated ->
            fx:log("FX ready"),
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
