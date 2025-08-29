%% Live Scape – Minimal, focused on sensing and trading only
%% Assumptions:
%% - IB bridge provides #live_ohlc and #market_tick via records.hrl
%% - records.hrl also defines #live_state (we DO NOT redefine it here)
%% - config module supplies basic knobs used below
%% - We keep compatibility shims used by fx.erl: init_state/5, sense/2, lookup/2, next/2, prev/4, trade/3

-module(live_scape).
-include("records.hrl").

%% Public API needed by supervisor/fx.erl
-export([
    start_link/0, gen/2, prep/1,
    init_state/5, sense/2, lookup/2, next/2, prev/4, trade/3,
    init_scape/0
]).

%% --- Tables / Records -------------------------------------------------------

-define(LIVE_TABLES, [live_EURUSD1]).      %% extend if you add pairs
-define(DEFAULT_HRES, 100).
-define(ORDER_FILL_TIMEOUT_MS, 5000).

%% New: historical preload settings
-define(BAR_SEC_1MIN, 60).
-define(PRELOAD_DURATION, {weeks, 1}).      %% 1 week of 1-min bars

%% #technical lives here (not in records.hrl in most trees)
-record(technical, {
    id,     %% key = {Y,M,D,H,Min,S,SamplingSec}
    open,
    high,
    low,
    close,
    volume
}).

%% fx.erl compat record (lives here)
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).

%% --- Start / Pattern glue ---------------------------------------------------

start_link() ->
    Pid = spawn_link(?MODULE, init_scape, []),
    register(live_scape, Pid),
    {ok, Pid}.

init_scape() ->
    ensure_live_tables(),

    %% Start IB bridge once
    _ = case whereis(ib_bridge_connector) of
            undefined -> ib_bridge_connector:start_default_connection();
            _ -> ok
        end,

    %% (Optional) poll for connection a few times so first sense/pull succeeds
    ok = wait_ib_connected(10, 250),   %% tries for ~2.5s total

    %% Preload 1 week of 1-min bars into each live_* ETS table
    lists:foreach(fun preload_week/1, ?LIVE_TABLES),

    receive
        {ExoSelf_PId, live_sim} -> live_sim(ExoSelf_PId)
    end.

wait_ib_connected(0, _Delay) -> ok;
wait_ib_connected(N, Delay) ->
    case ib_bridge_connector:get_connection_status() of
        {ok, true} -> ok;
        _ ->
            timer:sleep(Delay),
            wait_ib_connected(N-1, Delay)
    end.

%% Legacy pattern used elsewhere
gen(ExoSelf_PId, Node) -> spawn(Node, ?MODULE, prep, [ExoSelf_PId]).
prep(ExoSelf_PId) ->
    receive {ExoSelf_PId, Name} -> live_scape:Name(ExoSelf_PId) end.

%% --- Main loop (minimal) ----------------------------------------------------

live_sim(ExoSelf_PId) ->
    %% Ensure trading fields are initialized so handle_trade/2 matches
    State = #live_state{
        table_name       = get_live_table_name(config:primary_currency_pair()),
        account_balance  = config:account_initial_balance(),
        current_position = 0,
        entry_price      = 0.0,
        realized_pnl     = 0.0,
        previous_pc      = 0.0,
        position_qty     = undefined
    },
    loop(ExoSelf_PId, State).

loop(ExoSelf_PId, State=#live_state{}) ->
    receive
        %% Sensor requests (fx pattern)
        {From, sense, TableName, Feature, Parameters, _Start, _Finish} ->
            {Result, S2} = handle_sense(TableName, Feature, Parameters, State),
            From ! {self(), Result},
            loop(ExoSelf_PId, S2);

        {From, sense, internals, _Params} ->
            From ! {self(), [State#live_state.current_position,
                             State#live_state.entry_price,
                             State#live_state.previous_pc]},
            loop(ExoSelf_PId, State);

        %% Trade request: TradeSignal ∈ {-1,0,1}
        {From, trade, _TableName, TradeSignal} ->
            {Fitness, Halt, S2} = handle_trade(TradeSignal, State),
            From ! {self(), Fitness, Halt},
            loop(ExoSelf_PId, S2);

        terminate ->
            ok
    after 10000 ->
        loop(ExoSelf_PId, State)
    end.

%% --- Sense (graph/list) -----------------------------------------------------

handle_sense(TableName0, _Feature, Params, State0) ->
    LiveT = ensure_live_table_name(TableName0),
    ensure_live_tables(),
    _ = maybe_pull_once(LiveT),
    case Params of
        [HRes, list_sensor] ->
            {PL, S1} = price_list_fx(LiveT, HRes, State0),
            { [C || {_O,C,_H,_L} <- PL], S1 };
        [HRes, VRes, graph_sensor] ->
            {PL, S1} = price_list_fx(LiveT, HRes, State0),
            { encode_plane(PL, HRes, VRes), S1 };
        _ ->
            {[], State0}
    end.

price_list_fx(Table, HRes, State) ->
    Index = ets:last(Table),
    PL = case Index of
        '$end_of_table' -> fallback_price_list(Table, HRes);
        _ -> collect_last(Table, HRes, Index, [])
    end,
    {PL, State#live_state{price_list = lists:keystore(HRes, 2, State#live_state.price_list, {PL, HRes})}}.

collect_last(_Table, 0, _Idx, Acc) -> lists:reverse(Acc);
collect_last(Table, N, Idx, Acc) ->
    case ets:lookup(Table, Idx) of
        [#technical{open=O,close=C,high=H,low=L}] ->
            Prev = ets:prev(Table, Idx),
            case Prev of
                '$end_of_table' -> lists:reverse([{O,C,H,L}|Acc]);
                _               -> collect_last(Table, N-1, Prev, [{O,C,H,L}|Acc])
            end;
        [] ->
            %% Skip missing row
            Prev = ets:prev(Table, Idx),
            case Prev of
                '$end_of_table' -> lists:reverse(Acc);
                _               -> collect_last(Table, N, Prev, Acc)
            end
    end.

fallback_price_list(Table, HRes) ->
    %% Single shot to IB; if still nothing, flat line at 1.0 (should be rare after bridge startup)
    Sym = table_to_ib_symbol(Table),
    case ib_bridge_connector:get_market_data(Sym) of
        {ok, Tick} ->
            Price = pick_price(Tick),
            lists:duplicate(HRes, {Price,Price,Price,Price});
        _ ->
            %% Bridge not ready or no data: flatline (should be rare)
            lists:duplicate(HRes, {1.0,1.0,1.0,1.0})
    end.

encode_plane([], HRes, VRes) -> lists:duplicate(HRes*VRes, -1);
encode_plane(PL, HRes, VRes) ->
    Highs = [H || {_O,_C,H,_L} <- PL],
    Lows  = [L || {_O,_C,_H,L} <- PL],
    Max1  = lists:max(Highs),
    Min1  = lists:min(Lows),
    Pad   = abs(Max1 - Min1) / 20,
    VMax  = Max1 + Pad,
    VMin  = Min1 - Pad,
    VStep = (VMax - VMin) / VRes,
    V0    = VMin + VStep/2,
    encode_plane_rows(HRes*VRes, PL, V0, VStep, []).

encode_plane_rows(0, _PL, _VPos, _VStep, Acc) -> lists:reverse(Acc);
encode_plane_rows(N, [{O,C,H,L}|Rest], VPos, VStep, Acc) ->
    {BHi,BLo} = if O > C -> {O,C}; true -> {C,O} end,
    Val = case (VPos + VStep/2 > BLo) andalso (VPos - VStep/2 =< BHi) of
              true  -> 1;   %% body
              false -> if (VPos + VStep/2 > L) andalso (VPos - VStep/2 =< H) -> 0; true -> -1 end
          end,
    encode_plane_rows(N-1, Rest, VPos, VStep, [Val|Acc]);
encode_plane_rows(N, [], VPos, VStep, Acc) ->
    encode_plane_rows(N-1, [], VPos+VStep, VStep, [-1|Acc]).

%% --- Trade (minimal open/close/switch) --------------------------------------

handle_trade(Signal, State=#live_state{current_position=Pos}) ->
    case {Pos, Signal} of
        {0,  1} -> open_position( 1, State);
        {0, -1} -> open_position(-1, State);
        {1,  0} -> close_position(State);
        {-1, 0} -> close_position(State);
        {1, -1} -> close_then_open(-1, State);
        {-1, 1} -> close_then_open( 1, State);
        _       -> {0,0,State}
    end.

close_then_open(New, S0) ->
    {_P, _H, S1} = close_position(S0),
    open_position(New, S1).

open_position(Signal, State=#live_state{table_name=T, account_balance=Bal}) ->
    Sym = table_to_ib_symbol(T),
    case current_price(Sym) of
        {ok, Price} ->
            Qty = max(1, round( Bal * config:live_position_size()
                                 * config:account_leverage()
                                 / (Price * config:account_lot_size()) )),
            Act = case Signal of 1 -> "BUY"; -1 -> "SELL" end,
            case ib_bridge_connector:place_order(Sym, Act, Qty, "MKT") of
                ok ->
                    case wait_fill(?ORDER_FILL_TIMEOUT_MS) of
                        {ok, Fill} ->
                            {0,0, State#live_state{current_position=Signal,
                                                   entry_price=Fill,
                                                   position_qty=Qty}};
                        _ ->
                            {0,0, State}
                    end;
                _ -> {0,0,State}
            end;
        _ -> {0,0,State}
    end.

close_position(State=#live_state{table_name=T, current_position=Pos,
                                 entry_price=Entry, account_balance=Bal,
                                 realized_pnl=Realized, position_qty=Qty0}) ->
    case Pos of
        0 -> {0,0,State};
        _ ->
            Sym = table_to_ib_symbol(T),
            case current_price(Sym) of
                {ok, _Price} ->
                    Qty = case Qty0 of undefined -> 1; Q -> Q end,
                    Act = case Pos of 1 -> "SELL"; -1 -> "BUY" end,
                    case ib_bridge_connector:place_order(Sym, Act, Qty, "MKT") of
                        ok ->
                            case wait_fill(?ORDER_FILL_TIMEOUT_MS) of
                                {ok, Fill} ->
                                    Delta = case Pos of 1 -> (Fill-Entry); -1 -> (Entry-Fill) end,
                                    Profit = Delta * Qty,
                                    NewBal = Bal + Profit,
                                    Pct    = if Entry =:= 0 -> 0.0; true -> (Fill-Entry)/Entry end,
                                    {Profit, 0,
                                     State#live_state{current_position=0,
                                                      entry_price=0.0,
                                                      account_balance=NewBal,
                                                      realized_pnl=Realized+Profit,
                                                      previous_pc=Pct,
                                                      position_qty=undefined}};
                                _ -> {0,0,State}
                            end;
                        _ -> {0,0,State}
                    end;
                _ -> {0,0,State}
            end
    end.

wait_fill(TimeoutMs) ->
    receive
        {execution_data, {_OrderId,_Sym,_Side,_Shares,Price,_Time}} -> {ok, Price}
    after TimeoutMs ->
        {error, timeout}
    end.

current_price(Sym) ->
    case ib_bridge_connector:get_market_data(Sym) of
        {ok, Tick} ->
            Price = pick_price(Tick),
            {ok, Price};
        Other -> Other
    end.

pick_price(Tick) ->
    case Tick#market_tick.last of
        undefined ->
            case Tick#market_tick.bid of
                undefined -> Tick#market_tick.ask;
                Bid -> Bid
            end;
        Last -> Last
    end.

%% --- fx.erl compatibility shims --------------------------------------------

init_state(S, TableName, Feature, live_data, live_data) ->
    LiveT = ensure_live_table_name(TableName),
    ensure_live_tables(),
    _ = maybe_pull_once(LiveT),
    %% return a minimal fx-compatible state window (100 bars)
    IndexEnd = ets:last(LiveT),
    {I0, I1} = case IndexEnd of
        '$end_of_table' -> {undefined, undefined};
        _               -> {find_start(LiveT, IndexEnd, ?DEFAULT_HRES-1), IndexEnd}
    end,
    S#state{ table_name = LiveT,
             feature    = Feature,
             index_start= I0, index_end= I1, index= I0, price_list = [] }.

sense(S=#state{table_name=T}, Params) ->
    case Params of
        [HRes, list_sensor] ->
            {PL, _} = price_list_fx(T, HRes, #live_state{}),
            {[C || {_O,C,_H,_L} <- PL], S};
        [HRes, VRes, graph_sensor] ->
            {PL, _} = price_list_fx(T, HRes, #live_state{}),
            {encode_plane(PL, HRes, VRes), S};
        _ -> {[], S}
    end.

lookup(Table, Index) ->
    case ets:lookup(Table, Index) of
        [R] -> R;
        []  -> undefined
    end.

next(Table, Index) -> ets:next(Table, Index).

prev(Table, Curr, prev, Cnt) ->
    step_prev(Table, Curr, Cnt);
prev(Table, Curr, next, Cnt) ->
    step_next(Table, Curr, Cnt).

step_prev(_T, I, 0) -> I;
step_prev(T, I, N) ->
    case ets:prev(T, I) of
        '$end_of_table' -> I;
        P -> step_prev(T, P, N-1)
    end.

step_next(_T, I, 0) -> I;
step_next(T, I, N) ->
    case ets:next(T, I) of
        '$end_of_table' -> I;
        P -> step_next(T, P, N-1)
    end.

trade(_TableName, TradeSignal, LiveState) ->
    handle_trade(TradeSignal, LiveState).

%% --- Live table utilities (minimal) -----------------------------------------

ensure_live_tables() ->
    lists:foreach(
      fun(T) ->
          case ets:info(T) of
              undefined -> ets:new(T, [ordered_set, public, named_table, {keypos, 1}]);
              _ -> ok
          end
      end, ?LIVE_TABLES),
    ok.

%% Expose the helper name the rest of your code expects
get_live_table_name(Name) ->
    ensure_live_table_name(Name).

ensure_live_table_name(Name) when is_atom(Name) ->
    case atom_to_list(Name) of
        "live_" ++ _ -> Name;
        S            -> list_to_atom("live_" ++ S)
    end;
ensure_live_table_name(Name) when is_list(Name) ->
    list_to_atom(ensure_live_table_name(list_to_atom(Name))).

%% New: if table is empty, preload 1 week of 1-min bars; otherwise noop
maybe_pull_once(LiveT) ->
    case ets:last(LiveT) of
        '$end_of_table' -> preload_week(LiveT);
        _               -> ok
    end.

%% New: try historical 1-week fetch; fallback to old single-shot loader
preload_week(LiveT) ->
    Sym = table_to_ib_symbol(LiveT),
    %% Prefer get_historical_ohlc/3 if available; use catch to avoid undef crash
    case catch ib_bridge_connector:get_historical_ohlc(Sym, ?BAR_SEC_1MIN, ?PRELOAD_DURATION) of
        {ok, OHLCs} when is_list(OHLCs), OHLCs =/= [] ->
            %% If newest->oldest, reverse to oldest->newest; harmless if already ascending
            Ordered =
                case OHLCs of
                    [] -> [];
                    [_|_] -> OHLCs
                end,
            insert_ohlc_batch(LiveT, Ordered),
            ok;
        _Other ->
            %% Fallback: preserve old behavior so we're never empty
            pull_once(LiveT)
    end.

%% New: batch insert helper
insert_ohlc_batch(LiveT, OHLCs) ->
    lists:foreach(
      fun(OHLC) ->
          ets:insert(LiveT, #technical{
              id     = OHLC#live_ohlc.timestamp,
              open   = OHLC#live_ohlc.open,
              high   = OHLC#live_ohlc.high,
              low    = OHLC#live_ohlc.low,
              close  = OHLC#live_ohlc.close,
              volume = OHLC#live_ohlc.volume})
      end, OHLCs).

%% Legacy single-shot (kept as fallback)
pull_once(LiveT) ->
    Sym = table_to_ib_symbol(LiveT),
    case ib_bridge_connector:get_ohlc_data(Sym, 60) of
        {ok, OHLCs} when OHLCs =/= [] ->
            insert_ohlc_batch(LiveT, OHLCs),
            ok;
        _ -> ok
    end.

find_start(_T, I, 0) -> I;
find_start(T, I, N) ->
    case ets:prev(T, I) of
        '$end_of_table' -> I;
        P -> find_start(T, P, N-1)
    end.

table_to_ib_symbol(LiveT) when is_atom(LiveT) ->
    Str = atom_to_list(LiveT),
    Base = case lists:prefix("live_", Str) of
        true -> lists:nthtail(5, Str);
        false -> Str
    end,
    %% Always use canonical form "EUR.USD" for all pairs
    Six = take_6_letters(Base),
    case Six of
        "EURUSD" -> "EUR.USD";
        "GBPUSD" -> "GBP.USD";
        "USDJPY" -> "USD.JPY";
        _        -> Six
    end.

take_6_letters(Str) ->
    Letters = [C || C <- Str, C >= $A, C =< $Z],
    case length(Letters) >= 6 of
        true  -> lists:sublist(Letters, 6);
        false -> Letters
    end.
