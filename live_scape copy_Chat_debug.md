%% Live Scape – Minimal, focused on sensing and trading only
%% Assumptions:
%% - IB bridge provides #live_ohlc and #market_tick via records.hrl
%% - records.hrl also defines #live_state (we DO NOT redefine it here)
%% - config module supplies basic knobs used below
%% - We keep compatibility shims used by fx.erl: init_state/5, sense/2, lookup/2, next/2, prev/4, trade/3

-module(live_scape).
-include("records.hrl").

%% ---------------------------------------------------------------------------
%% Debug helpers (terminal logging)
%% ---------------------------------------------------------------------------

-define(LOG(Level,Fmt,Args),
        io:format("~s [~p] live_scape(~p): " ++ Fmt ++ "~n",
                  [live_scape:ts(), Level, self()] ++ (Args))).

-define(TRACE(Fmt,Args), ?LOG(trace, Fmt, Args)).
-define(DEBUG(Fmt,Args), ?LOG(debug, Fmt, Args)).
-define(INFO(Fmt,Args),  ?LOG(info,  Fmt, Args)).
-define(WARN(Fmt,Args),  ?LOG(warn,  Fmt, Args)).
-define(ERROR(Fmt,Args), ?LOG(error, Fmt, Args)).

ts() ->
    {{Y,Mo,D},{H,Mi,S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", [Y,Mo,D,H,Mi,S]).

%% ---------------------------------------------------------------------------
%% Public API needed by supervisor/fx.erl
%% ---------------------------------------------------------------------------

-export([
    start_link/0, gen/2, prep/1,
    init_state/5, sense/2, lookup/2, next/2, prev/4, trade/3,
    init_scape/0
]).

%% --- Tables / Records -------------------------------------------------------

-define(LIVE_TABLES, [live_EURUSD1]).      %% extend if you add pairs
-define(DEFAULT_HRES, 100).
-define(ORDER_FILL_TIMEOUT_MS, 5000).

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
    ?INFO("start_link called", []),
    Pid = spawn_link(?MODULE, init_scape, []),
    register(live_scape, Pid),
    ?INFO("spawned and registered live_scape pid=~p", [Pid]),
    {ok, Pid}.

init_scape() ->
    ?INFO("init_scape: ensuring live tables", []),
    ensure_live_tables(),
    ?INFO("init_scape: waiting for {ExoSelf_PId, live_sim}", []),
    receive
        {ExoSelf_PId, live_sim} ->
            ?INFO("init_scape: received live_sim for ExoSelf_PId=~p", [ExoSelf_PId]),
            live_sim(ExoSelf_PId)
    end.

%% Legacy pattern used elsewhere
gen(ExoSelf_PId, Node) ->
    ?INFO("gen called with Node=~p ExoSelf_PId=~p", [Node, ExoSelf_PId]),
    spawn(Node, ?MODULE, prep, [ExoSelf_PId]).

prep(ExoSelf_PId) ->
    ?INFO("prep awaiting {ExoSelf_PId, Name}", []),
    receive
        {ExoSelf_PId, Name} ->
            ?INFO("prep received Name=~p; invoking live_scape:~p/1", [Name, Name]),
            live_scape:Name(ExoSelf_PId)
    end.

%% --- Main loop (minimal) ----------------------------------------------------

live_sim(ExoSelf_PId) ->
    %% #live_state is defined in records.hrl
    State = #live_state{
        table_name = get_live_table_name(config:primary_currency_pair()),
        account_balance = config:account_initial_balance()
    },
    ?INFO("live_sim start: table=~p, initial_balance=~p",
          [State#live_state.table_name, State#live_state.account_balance]),
    loop(ExoSelf_PId, State).

loop(ExoSelf_PId, State=#live_state{}) ->
    receive
        %% Sensor requests (fx pattern)
        {From, sense, TableName, Feature, Parameters, _Start, _Finish} ->
            ?DEBUG("loop: sense request From=~p Table=~p Feature=~p Params=~p",
                   [From, TableName, Feature, Parameters]),
            {Result, S2} = handle_sense(TableName, Feature, Parameters, State),
            ?TRACE("loop: sense reply -> ~p (len=~p)",
                   [case is_list(Result) of true -> list_to_tuple(lists:sublist(Result, min(3, length(Result)))); _ -> Result end,
                    case is_list(Result) of true -> length(Result); false -> 1 end]),
            From ! {self(), Result},
            loop(ExoSelf_PId, S2);

        {From, sense, internals, _Params} ->
            ?DEBUG("loop: sense internals request From=~p", [From]),
            From ! {self(), [State#live_state.current_position,
                             State#live_state.entry_price,
                             State#live_state.previous_pc]},
            loop(ExoSelf_PId, State);

        %% Trade request: TradeSignal ∈ {-1,0,1}
        {From, trade, _TableName, TradeSignal} ->
            ?INFO("loop: trade request From=~p Signal=~p Pos=~p Bal=~p",
                  [From, TradeSignal, State#live_state.current_position, State#live_state.account_balance]),
            {Fitness, Halt, S2} = handle_trade(TradeSignal, State),
            ?INFO("loop: trade result Fitness=~p Halt=~p NewPos=~p Bal=~p RealizedPnL=~p",
                  [Fitness, Halt, S2#live_state.current_position, S2#live_state.account_balance, S2#live_state.realized_pnl]),
            From ! {self(), Fitness, Halt},
            loop(ExoSelf_PId, S2);

        terminate ->
            ?WARN("loop: received terminate", []),
            ok
    after 10000 ->
        ?TRACE("loop: idle tick; position=~p bal=~p", [State#live_state.current_position, State#live_state.account_balance]),
        loop(ExoSelf_PId, State)
    end.

%% --- Sense (graph/list) -----------------------------------------------------

handle_sense(TableName0, Feature, Params, State0) ->
    ?DEBUG("handle_sense: TableName0=~p Feature=~p Params=~p", [TableName0, Feature, Params]),
    LiveT = ensure_live_table_name(TableName0),
    ensure_live_tables(),
    _ = maybe_pull_once(LiveT),
    case Params of
        [HRes, list_sensor] ->
            ?TRACE("handle_sense: list_sensor HRes=~p", [HRes]),
            {PL, S1} = price_list_fx(LiveT, HRes, State0),
            ?TRACE("handle_sense: list_sensor produced ~p bars", [length(PL)]),
            { [C || {_O,C,_H,_L} <- PL], S1 };
        [HRes, VRes, graph_sensor] ->
            ?TRACE("handle_sense: graph_sensor HRes=~p VRes=~p", [HRes, VRes]),
            {PL, S1} = price_list_fx(LiveT, HRes, State0),
            { encode_plane(PL, HRes, VRes), S1 };
        _ ->
            ?WARN("handle_sense: unknown params -> ~p", [Params]),
            {[], State0}
    end.

price_list_fx(Table, HRes, State) ->
    Idx = ets:last(Table),
    ?TRACE("price_list_fx: Table=~p HRes=~p last=~p", [Table, HRes, Idx]),
    PL = case Idx of
        '$end_of_table' -> fallback_price_list(Table, HRes);
        _ -> collect_last(Table, HRes, Idx, [])
    end,
    ?TRACE("price_list_fx: collected ~p bars", [length(PL)]),
    {PL, State#live_state{price_list = lists:keystore(HRes, 2, State#live_state.price_list, {PL, HRes})}}.

collect_last(_Table, 0, _Idx, Acc) ->
    lists:reverse(Acc);
collect_last(Table, N, Idx, Acc) ->
    case ets:lookup(Table, Idx) of
        [#technical{open=O,close=C,high=H,low=L}] ->
            Prev = ets:prev(Table, Idx),
            ?TRACE("collect_last: idx=~p O=~p C=~p H=~p L=~p prev=~p left=~p",
                   [Idx,O,C,H,L,Prev,N]),
            case Prev of
                '$end_of_table' -> lists:reverse([{O,C,H,L}|Acc]);
                _               -> collect_last(Table, N-1, Prev, [{O,C,H,L}|Acc])
            end;
        [] ->
            %% Skip missing row
            Prev = ets:prev(Table, Idx),
            ?WARN("collect_last: missing row at idx=~p; prev=~p left=~p", [Idx,Prev,N]),
            case Prev of
                '$end_of_table' -> lists:reverse(Acc);
                _               -> collect_last(Table, N, Prev, Acc)
            end
    end.

fallback_price_list(Table, HRes) ->
    %% Single shot to IB; if still nothing, flat line at 1.0
    Sym = table_to_ib_symbol(Table),
    ?INFO("fallback_price_list: Table=~p HRes=~p Symbol=~p", [Table,HRes,Sym]),
    case ib_bridge_connector:get_market_data(Sym) of
        {ok, Tick} ->
            Price = pick_price(Tick),
            ?INFO("fallback_price_list: got tick -> price=~p (duplicating ~p bars)", [Price,HRes]),
            lists:duplicate(HRes, {Price,Price,Price,Price});
        _ ->
            ?WARN("fallback_price_list: no tick; using flat 1.0 (~p bars)", [HRes]),
            lists:duplicate(HRes, {1.0,1.0,1.0,1.0})
    end.

encode_plane([], HRes, VRes) ->
    ?WARN("encode_plane: empty PL; returning -1 plane HRes=~p VRes=~p", [HRes,VRes]),
    lists:duplicate(HRes*VRes, -1);
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
    ?TRACE("encode_plane: Max=~p Min=~p Pad=~p VMin=~p VMax=~p VStep=~p", [Max1,Min1,Pad,VMin,VMax,VStep]),
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
    ?INFO("handle_trade: Pos=~p Signal=~p", [Pos, Signal]),
    case {Pos, Signal} of
        {0,  1} -> open_position( 1, State);
        {0, -1} -> open_position(-1, State);
        {1,  0} -> close_position(State);
        {-1, 0} -> close_position(State);
        {1, -1} -> close_then_open(-1, State);
        {-1, 1} -> close_then_open( 1, State);
        _       -> ?TRACE("handle_trade: no-op for {~p,~p}", [Pos,Signal]),
                   {0,0,State}
    end.

close_then_open(New, S0) ->
    ?INFO("close_then_open: switching to ~p", [New]),
    {_P, _H, S1} = close_position(S0),
    open_position(New, S1).

open_position(Signal, State=#live_state{table_name=T, account_balance=Bal}) ->
    Sym = table_to_ib_symbol(T),
    ?INFO("open_position: Sym=~p Signal=~p Bal=~p", [Sym, Signal, Bal]),
    case current_price(Sym) of
        {ok, Price} ->
            Qty = max(1, round( Bal * config:live_position_size()
                                 * config:account_leverage()
                                 / (Price * config:account_lot_size()) )),
            Act = case Signal of 1 -> "BUY"; -1 -> "SELL" end,
            ?INFO("open_position: action=~s qty=~p price=~p", [Act, Qty, Price]),
            case ib_bridge_connector:place_order(Sym, Act, Qty, "MKT") of
                ok ->
                    ?DEBUG("open_position: order placed; waiting fill ~p ms", [?ORDER_FILL_TIMEOUT_MS]),
                    case wait_fill(?ORDER_FILL_TIMEOUT_MS) of
                        {ok, Fill} ->
                            ?INFO("open_position: fill=~p -> position OPENED", [Fill]),
                            {0,0, State#live_state{current_position=Signal,
                                                   entry_price=Fill}};
                        _ ->
                            ?WARN("open_position: fill timeout or error", []),
                            {0,0, State}
                    end;
                Err ->
                    ?ERROR("open_position: place_order error -> ~p", [Err]),
                    {0,0,State}
            end;
        Other ->
            ?WARN("open_position: could not get current price -> ~p", [Other]),
            {0,0,State}
    end.

close_position(State=#live_state{table_name=T, current_position=Pos,
                                 entry_price=Entry, account_balance=Bal,
                                 realized_pnl=Realized}) ->
    case Pos of
        0 ->
            ?TRACE("close_position: already flat", []),
            {0,0,State};
        _ ->
            Sym = table_to_ib_symbol(T),
            ?INFO("close_position: Sym=~p Pos=~p Entry=~p", [Sym, Pos, Entry]),
            case current_price(Sym) of
                {ok, _Price} ->
                    Qty = 1, %% minimalist; track/restore actual qty if you store it
                    Act = case Pos of 1 -> "SELL"; -1 -> "BUY" end,
                    ?INFO("close_position: action=~s qty=~p", [Act, Qty]),
                    case ib_bridge_connector:place_order(Sym, Act, Qty, "MKT") of
                        ok ->
                            ?DEBUG("close_position: order placed; waiting fill ~p ms", [?ORDER_FILL_TIMEOUT_MS]),
                            case wait_fill(?ORDER_FILL_TIMEOUT_MS) of
                                {ok, Fill} ->
                                    Delta = case Pos of 1 -> (Fill-Entry); -1 -> (Entry-Fill) end,
                                    Profit = Delta * Qty,
                                    NewBal = Bal + Profit,
                                    Pct    = if Entry =:= 0 -> 0.0; true -> (Fill-Entry)/Entry end,
                                    ?INFO("close_position: filled at ~p profit=~p newBal=~p pct=~p",
                                          [Fill, Profit, NewBal, Pct]),
                                    {Profit, 0,
                                     State#live_state{current_position=0,
                                                      entry_price=0.0,
                                                      account_balance=NewBal,
                                                      realized_pnl=Realized+Profit,
                                                      previous_pc=Pct}};
                                _ ->
                                    ?WARN("close_position: fill timeout or error", []),
                                    {0,0,State}
                            end;
                        Err ->
                            ?ERROR("close_position: place_order error -> ~p", [Err]),
                            {0,0,State}
                    end;
                Other ->
                    ?WARN("close_position: current_price error -> ~p", [Other]),
                    {0,0,State}
            end
    end.

wait_fill(TimeoutMs) ->
    ?TRACE("wait_fill: awaiting {execution_data,...} for up to ~p ms", [TimeoutMs]),
    receive
        {execution_data, {_OrderId,Sym,Side,Shares,Price,Time}} ->
            ?INFO("wait_fill: got execution_data orderId=~p sym=~p side=~p qty=~p price=~p time=~p",
                  [_OrderId,Sym,Side,Shares,Price,Time]),
            {ok, Price}
    after TimeoutMs ->
        ?WARN("wait_fill: timed out after ~p ms", [TimeoutMs]),
        {error, timeout}
    end.

current_price(Sym) ->
    ?TRACE("current_price: requesting market data for ~p", [Sym]),
    case ib_bridge_connector:get_market_data(Sym) of
        {ok, Tick} ->
            Price = pick_price(Tick),
            ?TRACE("current_price: got price=~p", [Price]),
            {ok, Price};
        Other ->
            ?WARN("current_price: get_market_data error -> ~p", [Other]),
            Other
    end.

pick_price(Tick) ->
    P = case Tick#market_tick.last of
        undefined ->
            case Tick#market_tick.bid of
                undefined -> Tick#market_tick.ask;
                Bid -> Bid
            end;
        Last -> Last
    end,
    ?TRACE("pick_price: resolved ~p", [P]),
    P.

%% --- fx.erl compatibility shims --------------------------------------------

init_state(S, TableName, Feature, live_data, live_data) ->
    LiveT = ensure_live_table_name(TableName),
    ?INFO("init_state: Table=~p Feature=~p -> ensured live table ~p", [TableName,Feature,LiveT]),
    ensure_live_tables(),
    _ = maybe_pull_once(LiveT),
    %% return a minimal fx-compatible state window (100 bars)
    IndexEnd = ets:last(LiveT),
    {I0, I1} = case IndexEnd of
        '$end_of_table' ->
            ?WARN("init_state: live table ~p is empty", [LiveT]),
            {undefined, undefined};
        _ ->
            Start = find_start(LiveT, IndexEnd, ?DEFAULT_HRES-1),
            ?TRACE("init_state: live window start=~p end=~p", [Start, IndexEnd]),
            {Start, IndexEnd}
    end,
    S#state{ table_name = LiveT,
             feature    = Feature,
             index_start= I0, index_end= I1, index= I0, price_list = [] }.

sense(S=#state{table_name=T}, Params) ->
    ?DEBUG("sense(state): T=~p Params=~p", [T, Params]),
    case Params of
        [HRes, list_sensor] ->
            {PL, _} = price_list_fx(T, HRes, #live_state{}),
            ?TRACE("sense(state): list len=~p", [length(PL)]),
            {[C || {_O,C,_H,_L} <- PL], S};
        [HRes, VRes, graph_sensor] ->
            {PL, _} = price_list_fx(T, HRes, #live_state{}),
            {encode_plane(PL, HRes, VRes), S};
        _ ->
            ?WARN("sense(state): unknown params -> ~p", [Params]),
            {[], S}
    end.

lookup(Table, Index) ->
    R = case ets:lookup(Table, Index) of
        [Rec] -> Rec;
        []    -> undefined
    end,
    ?TRACE("lookup: Table=~p Index=~p -> ~p", [Table,Index,R]),
    R.

next(Table, Index) ->
    N = ets:next(Table, Index),
    ?TRACE("next: Table=~p Index=~p -> ~p", [Table,Index,N]),
    N.

prev(Table, Curr, prev, Cnt) ->
    ?TRACE("prev/prev: Table=~p Curr=~p Cnt=~p", [Table,Curr,Cnt]),
    step_prev(Table, Curr, Cnt);
prev(Table, Curr, next, Cnt) ->
    ?TRACE("prev/next: Table=~p Curr=~p Cnt=~p", [Table,Curr,Cnt]),
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
    ?DEBUG("trade shim: signal=~p", [TradeSignal]),
    handle_trade(TradeSignal, LiveState).

%% --- Live table utilities (minimal) -----------------------------------------

ensure_live_tables() ->
    lists:foreach(
      fun(T) ->
          case ets:info(T) of
              undefined ->
                  ?INFO("ensure_live_tables: creating ETS table ~p", [T]),
                  ets:new(T, [ordered_set, public, named_table, {keypos, 2}]);
              _ ->
                  ?TRACE("ensure_live_tables: table ~p already exists", [T]),
                  ok
          end
      end, ?LIVE_TABLES),
    ok.

%% Expose the helper name the rest of your code expects
get_live_table_name(Name) ->
    ensure_live_table_name(Name).

ensure_live_table_name(Name) when is_atom(Name) ->
    Out = case atom_to_list(Name) of
        "live_" ++ _ -> Name;
        S            -> list_to_atom("live_" ++ S)
    end,
    ?TRACE("ensure_live_table_name(atom): ~p -> ~p", [Name,Out]),
    Out;
ensure_live_table_name(Name) when is_list(Name) ->
    Out = list_to_atom(ensure_live_table_name(list_to_atom(Name))),
    ?TRACE("ensure_live_table_name(list): ~p -> ~p", [Name,Out]),
    Out.

maybe_pull_once(LiveT) ->
    case ets:last(LiveT) of
        '$end_of_table' ->
            ?INFO("maybe_pull_once: ~p empty; pulling once", [LiveT]),
            pull_once(LiveT);
        _ ->
            ?TRACE("maybe_pull_once: ~p already has data", [LiveT]),
            ok
    end.

pull_once(LiveT) ->
    Sym = table_to_ib_symbol(LiveT),
    ?INFO("pull_once: requesting initial OHLC for ~p (~p)", [LiveT, Sym]),
    case ib_bridge_connector:get_ohlc_data(Sym, 60) of
        {ok, OHLCs} when OHLCs =/= [] ->
            ?INFO("pull_once: received ~p bars; inserting", [length(OHLCs)]),
            lists:foreach(fun(OHLC) ->
                ets:insert(LiveT, #technical{
                    id = OHLC#live_ohlc.timestamp,
                    open = OHLC#live_ohlc.open,
                    high = OHLC#live_ohlc.high,
                    low  = OHLC#live_ohlc.low,
                    close= OHLC#live_ohlc.close,
                    volume = OHLC#live_ohlc.volume})
            end, OHLCs),
            ok;
        Other ->
            ?WARN("pull_once: no OHLC inserted (resp=~p)", [Other]),
            ok
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
    %% Expect forms like "EURUSD1" -> take 6 letters
    Six = take_6_letters(Base),
    Out = case Six of
        "EURUSD" -> "EUR.USD";
        "GBPUSD" -> "GBP.USD";
        "USDJPY" -> "USD.JPY";
        _        -> Six
    end,
    ?TRACE("table_to_ib_symbol: ~p -> ~p", [LiveT,Out]),
    Out.

take_6_letters(Str) ->
    Letters = [C || C <- Str, C >= $A, C =< $Z],
    case length(Letters) >= 6 of
        true  -> lists:sublist(Letters, 6);
        false -> Letters
    end.
