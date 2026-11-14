%% Episode sampling for multi-window evaluation (config:evaluation_mode() == episodes).
%%
%% An episode is {StartKey, Bars}: the simulator starts at the row StartKey of the price table and steps Bars rows.
%% Every generation the population monitor calls begin_generation/2, which draws a fresh "tune" set and a fresh "score"
%% set from config:train_span() and publishes them through config:set/2, so every agent of that generation is evaluated
%% on exactly the same data. The validation set is fixed for the whole run and tiles config:val_span().
%%
%% Episode start points are drawn one per time stratum: the span is cut into as many equal slices as episodes are needed
%% and one start is picked uniformly inside each slice, so a draw always covers the whole span instead of clustering.

-module(episodes).
-export([begin_generation/2, current_set/1, ensure_validation_set/0, sample/5, tile/4,
	index/1, span_positions/2, describe/1, closes/2, set_key/1, drift/1, span_drift/2, set_drift/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Per-generation sets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

begin_generation(Population_Id,Generation)->
	Table = config:primary_currency_pair(),
	Bars = config:episode_bars(),
	{[Tune,Score],_} = sample(Table,config:train_span(),[config:episodes_tune(),config:episodes_score()],Bars,rand_state(Generation)),
	config:set(set_key(tune),Tune),
	config:set(set_key(score),Score),
	config:set(episodes_generation,Generation),
	Val = ensure_validation_set(),
	qlog:benchmarker(Population_Id,io_lib:format("EPISODES | generation=~p | tune=~s | score=~s | validation=~p episodes",
		[Generation,describe(Tune),describe(Score),length(Val)])),
	{Tune,Score}.

%current_set/1 returns the episode list an evaluation of the given kind must use. The sets are normally published by
%begin_generation/2 before any agent is spawned; if they are missing (an agent started by hand) they are drawn on demand.
current_set(Mode)->
	case config:get_val(set_key(Mode),undefined) of
		undefined ->
			case Mode of
				validation ->
					ensure_validation_set();
				_ ->
					begin_generation(manual,0),
					config:get_val(set_key(Mode),[])
			end;
		Set ->
			Set
	end.

ensure_validation_set()->
	Table = config:primary_currency_pair(),
	Params = {Table,config:val_span(),config:episode_bars(),config:episodes_val(),ets:info(Table,size)},
	case config:get_val(episodes_validation_params,undefined) of
		Params ->
			config:get_val(set_key(validation),[]);
		_ ->
			Set = tile(Table,config:val_span(),config:episode_bars(),config:episodes_val()),
			config:set(set_key(validation),Set),
			config:set(episodes_validation_params,Params),
			Set
	end.

set_key(tune)-> episodes_tune_set;
set_key(score)-> episodes_score_set;
set_key(validation)-> episodes_validation_set.

%With a plain integer seed generation G draws the same episodes in every chained run; with {Seed,RunIndex} (what
%DXNN_EPISODE_SEED sets, exp_runner:get_run_configs/0) each run of the chain draws its own.
rand_state(Generation)->
	case config:episode_seed() of
		undefined -> rand:seed_s(exsss);
		{Seed,Run} -> rand:seed_s(exsss,{Seed,Generation,7919+Run});
		Seed -> rand:seed_s(exsss,{Seed,Generation,7919})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Sampling %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%sample/5 draws lists:sum(SetSizes) episode starts from Span, one per time stratum, then deals the strata out at random
%to the requested sets. Returns {[Set1,Set2,...],RandState}. Sets drawn in one call never share a stratum.
sample(Table,Span,SetSizes,Bars,R0)->
	{_Size,_Step,Anchors} = index(Table),
	{Lo,Hi} = candidate_range(Table,Span,Bars),
	K = lists:sum(SetSizes),
	{Starts,R1} = draw_strata(Lo,Hi,K,0,R0,[]),
	{Shuffled,R2} = shuffle(Starts,R1),
	Episodes = [{element(I,Anchors),Bars} || I <- Shuffled],
	{deal(Episodes,SetSizes),R2}.

	draw_strata(_Lo,_Hi,K,K,R,Acc)->
		{lists:reverse(Acc),R};
	draw_strata(Lo,Hi,K,J,R,Acc)->
		N = Hi-Lo+1,
		From = Lo + (J*N) div K,
		To = erlang:max(From,Lo + ((J+1)*N) div K - 1),
		{U,R1} = rand:uniform_s(To-From+1,R),
		draw_strata(Lo,Hi,K,J+1,R1,[From+U-1|Acc]).

	shuffle(List,R)->
		{Tagged,R1} = lists:mapfoldl(fun(E,RA)-> {U,RB} = rand:uniform_s(RA), {{U,E},RB} end,R,List),
		{[E || {_,E} <- lists:sort(Tagged)],R1}.

	deal(_Episodes,[])->
		[];
	deal(Episodes,[Size|Sizes])->
		{Set,Rest} = lists:split(Size,Episodes),
		[Set|deal(Rest,Sizes)].

%tile/4 returns up to MaxK non-overlapping episodes spread evenly over Span, in time order. Used for validation, where the
%set must be the same every time it is asked for.
tile(Table,Span,Bars,MaxK)->
	{_Size,Step,Anchors} = index(Table),
	{Lo,Hi} = candidate_range(Table,Span,Bars),
	PerEpisode = erlang:max(1,(Bars+Step-1) div Step),
	Fit = (Hi-Lo) div PerEpisode + 1,
	Slots = case Fit =< MaxK of
		true ->
			lists:seq(0,Fit-1);
		false when MaxK == 1 ->
			[0];
		false ->
			[round(J*(Fit-1)/(MaxK-1)) || J <- lists:seq(0,MaxK-1)]
	end,
	[{element(Lo+Slot*PerEpisode,Anchors),Bars} || Slot <- Slots].

%candidate_range/3 returns the first and last anchor numbers at which an episode of Bars rows fits inside Span and has
%config:episode_min_history() rows of history before it for the sensors.
candidate_range(Table,Span,Bars)->
	{_Size,Step,Anchors} = index(Table),
	{LoPos,HiPos} = span_positions(Table,Span),
	FirstPos = erlang:max(LoPos,config:episode_min_history()),
	Lo = (FirstPos+Step-1) div Step + 1,
	Hi = erlang:min((HiPos-Bars) div Step + 1,tuple_size(Anchors)),
	case Hi >= Lo of
		true ->
			{Lo,Hi};
		false ->
			exit({episodes_span_too_small,[{span,Span},{bars,Bars},{rows_in_span,HiPos-LoPos+1},{table,Table}]})
	end.

%span_positions/2 converts a span into {FirstRow,LastRow}, counted from 0 at the oldest row of the table.
span_positions(Table,all)->
	{Size,_Step,_Anchors} = index(Table),
	{0,Size-1};
span_positions(Table,{bars_back,StartBack,EndBack})->
	{Size,_Step,_Anchors} = index(Table),
	{erlang:max(0,Size-1-StartBack),erlang:max(0,Size-1-EndBack)};
span_positions(Table,{From={_,_,_},To={_,_,_}})->
	{Size,Step,Anchors} = index(Table),
	N = tuple_size(Anchors),
	First = first_anchor(fun(Key)-> key_date(Key) >= From end,Anchors,1,N+1),
	AfterLast = first_anchor(fun(Key)-> key_date(Key) > To end,Anchors,1,N+1),
	case AfterLast > First of
		true ->
			{(First-1)*Step,erlang:min((AfterLast-1)*Step-1,Size-1)};
		false ->
			exit({episodes_span_has_no_rows,[{span,{From,To}},{table_first,key_date(element(1,Anchors))},{table_last,key_date(element(N,Anchors))}]})
	end.

	%first_anchor/4: binary search for the lowest anchor number in [Lo,Hi) for which Pred holds (Hi if none does).
	first_anchor(_Pred,_Anchors,Lo,Hi) when Lo >= Hi ->
		Hi;
	first_anchor(Pred,Anchors,Lo,Hi)->
		Mid = (Lo+Hi) div 2,
		case Pred(element(Mid,Anchors)) of
			true -> first_anchor(Pred,Anchors,Lo,Mid);
			false -> first_anchor(Pred,Anchors,Mid+1,Hi)
		end.

	key_date(Key)->
		{element(1,Key),element(2,Key),element(3,Key)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Drift of a span %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%drift/1 returns the average change of the close per bar that the simulator takes out of every bar's return
%(fx:ep_stat/4), so that being long in a period that happened to rise is not scored as skill. It is one number, known
%before any agent runs, and nothing an agent does can move it.
%	train        the average over the whole train span. An episode is about one part in several hundred of it.
%	validation   the average over the scored bars of the validation episodes themselves. That set never changes, so the
%	             number is just as fixed, and a constant position scores exactly zero over the set. With the average of
%	             the whole validation span instead, the 60 episodes (about a third of the span) would keep whatever drift
%	             they happen to have beyond it, and every agent that leans long or short would carry that into each of
%	             its validation results (measured: +1.6 on the timing t of an always-long position, +0.5 this way; the rest
%	             is because quiet and busy episodes weigh the same in t).
drift(validation)->
	Table = config:primary_currency_pair(),
	Params = {Table,config:val_span(),ets:info(Table,size),config:episode_bars(),config:episodes_val(),config:episode_warmup_bars(),config:episode_anchor_step()},
	case config:get_val({episodes_drift,validation},undefined) of
		{Params,Mu} ->
			Mu;
		_ ->
			Mu = set_drift(Table,ensure_validation_set()),
			config:set({episodes_drift,validation},{Params,Mu}),
			Mu
	end;
drift(_Train)->
	Table = config:primary_currency_pair(),
	Span = config:train_span(),
	Params = {Table,Span,ets:info(Table,size),config:episode_anchor_step()},
	case config:get_val({episodes_drift,train},undefined) of
		{Params,Mu} ->
			Mu;
		_ ->
			Mu = span_drift(Table,Span),
			config:set({episodes_drift,train},{Params,Mu}),
			Mu
	end.

%set_drift/2: the move of the close over the scored bars of Episodes divided by the number of those bars. The simulator
%scores the bars from row Warmup to row Bars-1 of an episode (fx:ep_step/4 stops when it reaches the last row), which
%eval_protocol:g0/0 checks: a constant position must score exactly zero over the validation set.
set_drift(Table,Episodes)->
	Warmup = config:episode_warmup_bars(),
	{Move,Bars} = lists:foldl(fun(Episode,{MoveAcc,BarsAcc})->
		Closes = closes(Table,Episode),
		Rows = length(Closes),
		case Rows > Warmup+2 of
			true -> {MoveAcc + lists:nth(Rows-1,Closes) - lists:nth(Warmup+1,Closes),BarsAcc + Rows-2-Warmup};
			false -> {MoveAcc,BarsAcc}
		end
	end,{0.0,0},Episodes),
	case Bars of
		0 -> 0.0;
		_ -> Move/Bars
	end.

%span_drift/2: (close at the last anchor row of Span - close at its first anchor row) / bars between the two.
span_drift(Table,Span)->
	{_Size,Step,Anchors} = index(Table),
	{LoPos,HiPos} = span_positions(Table,Span),
	First = (LoPos+Step-1) div Step + 1,
	Last = erlang:min(HiPos div Step + 1,tuple_size(Anchors)),
	case Last > First of
		true ->
			[FirstRow] = ets:lookup(Table,element(First,Anchors)),
			[LastRow] = ets:lookup(Table,element(Last,Anchors)),
			(element(6,LastRow) - element(6,FirstRow)) / ((Last-First)*Step);
		false ->
			0.0
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Row index %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%index/1 returns {Rows,Step,Anchors}, where Anchors is a tuple holding the key of every Step-th row (anchor number I is row
%(I-1)*Step). An ordered_set ETS table has no positional access, so the table is walked once and the result is cached
%in a persistent_term; it is rebuilt when the table has grown or the step has changed.
index(Table)->
	Step = config:episode_anchor_step(),
	Size = ets:info(Table,size),
	case persistent_term:get({?MODULE,index,Table},undefined) of
		{Size,Step,Anchors} ->
			{Size,Step,Anchors};
		_ ->
			Anchors = list_to_tuple(collect_anchors(Table,ets:first(Table),0,Step,[])),
			persistent_term:put({?MODULE,index,Table},{Size,Step,Anchors}),
			{Size,Step,Anchors}
	end.

	collect_anchors(_Table,'$end_of_table',_Pos,_Step,Acc)->
		lists:reverse(Acc);
	collect_anchors(Table,Key,Pos,Step,Acc)->
		U_Acc = case Pos rem Step of
			0 -> [Key|Acc];
			_ -> Acc
		end,
		collect_anchors(Table,ets:next(Table,Key),Pos+1,Step,U_Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%closes/2 returns the Bars+1 closing prices an episode spans, oldest first.
closes(Table,{StartKey,Bars})->
	closes(Table,StartKey,Bars+1,[]).

	closes(_Table,_Key,0,Acc)->
		lists:reverse(Acc);
	closes(_Table,'$end_of_table',_N,Acc)->
		lists:reverse(Acc);
	closes(Table,Key,N,Acc)->
		[Row] = ets:lookup(Table,Key),
		closes(Table,ets:next(Table,Key),N-1,[element(6,Row)|Acc]).

describe(Episodes) when is_list(Episodes)->
	lists:flatten(lists:join(",",[describe(E) || E <- Episodes]));
describe({StartKey,Bars})->
	{Y,Mo,D,H,Mi,_S,_Rate} = StartKey,
	lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B+~p",[Y,Mo,D,H,Mi,Bars])).
