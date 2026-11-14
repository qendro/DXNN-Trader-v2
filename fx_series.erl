%% Read-only price series shared by every agent.
%%
%% The price table is walked once and packed into a few binaries (closing prices, running sums, and 15-minute, hourly,
%% 4-hour and daily bars). Sensors that look further back than a few hundred minutes are answered from these arrays in a
%% handful of reads, instead of walking the table row by row for every agent on every bar:
%%
%%   {plt,TF,N}   price list by timeframe: the closes of the last N-1 completed TF-minute bars, then the current close
%%   {plt,TF,N,vol}  the same list scaled by the trailing volatility instead of the price level (answer_at/3)
%%   ind          indicators: change over six ranges, distance from three moving averages, two volatility ratios, and the
%%                position inside the last day's range (see ind/2)
%%   clock        time of day and day of week, each as a sine and a cosine
%%
%% Every answer for the bar at position P uses rows 0..P only (test/2 checks this by rebuilding the series from a table
%% cut off at P). The arrays cost about 45 MB for a million rows and are shared by all processes through persistent_term.
%% Not available on the live-trading path yet.

-module(fx_series).
-export([ensure/1, answer/3, answer_at/3, position/2, vl/1, build/1, test/2]).

-define(TIMEFRAMES,[15,60,240,1440]).
-define(VOL_FLOOR,1.0e-5).%smallest one-minute move the volatility-scaled price lists divide by, as a share of the price (0.1 pip at 1.1)
-define(F,64/float).
-define(U,32/unsigned).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Building %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%ensure/1 returns the packed series of Table, building them on first use and again if the table has changed size.
ensure(Table)->
	Size = ets:info(Table,size),
	case persistent_term:get({?MODULE,Table},undefined) of
		{Size,Series} ->
			Series;
		_ ->
			%One builder at a time. Without this, every simulator whose first request arrives while the arrays are
			%missing builds its own copy (a full pass over the table each, all at once at the start of a run).
			global:trans({{?MODULE,Table},self()},fun()->
				case persistent_term:get({?MODULE,Table},undefined) of
					{Size,Built} ->
						Built;
					_ ->
						Built = build(Table),
						persistent_term:put({?MODULE,Table},{Size,Built}),
						Built
				end
			end)
	end.

%build/1 walks Table (an ordered ETS table of {technical,Key,Open,High,Low,Close,...} rows, Key = {Y,M,D,H,Mi,S,Rate}).
build(Table)->
	Bars0 = maps:from_list([{TF,{undefined,-1,0.0,0.0,0.0,<<>>,<<>>,<<>>,<<>>}} || TF <- ?TIMEFRAMES]),
	build(Table,ets:first(Table),0,undefined,<<>>,<<0.0:?F>>,<<0.0:?F>>,0.0,0.0,Bars0).

build(_Table,'$end_of_table',N,_PrevClose,Closes,AbsSum,CloseSum,_AbsAcc,_CloseAcc,Bars)->
	TFs = maps:map(fun(_TF,{_Bucket,_Index,High,Low,Close,BarOf,BarCloses,BarHighs,BarLows})->
		%the bar still open at the end of the table is stored as it stands
		#{bar_of=>BarOf,closes=><<BarCloses/binary,Close:?F>>,highs=><<BarHighs/binary,High:?F>>,lows=><<BarLows/binary,Low:?F>>}
	end,Bars),
	#{size=>N,closes=>Closes,abs_sum=>AbsSum,close_sum=>CloseSum,tf=>TFs};
build(Table,Key,N,PrevClose,Closes,AbsSum,CloseSum,AbsAcc,CloseAcc,Bars)->
	[Row] = ets:lookup(Table,Key),
	High = float(element(4,Row)),
	Low = float(element(5,Row)),
	Close = float(element(6,Row)),
	Move = case PrevClose of
		undefined -> 0.0;
		_ -> abs(Close-PrevClose)
	end,
	U_AbsAcc = AbsAcc+Move,
	U_CloseAcc = CloseAcc+Close,
	U_Bars = maps:map(fun(TF,{Bucket,Index,BHigh,BLow,BClose,BarOf,BarCloses,BarHighs,BarLows})->
		case bucket(TF,Key) of
			Bucket ->
				{Bucket,Index,erlang:max(BHigh,High),erlang:min(BLow,Low),Close,<<BarOf/binary,Index:?U>>,BarCloses,BarHighs,BarLows};
			NewBucket when Bucket == undefined ->
				{NewBucket,0,High,Low,Close,<<BarOf/binary,0:?U>>,BarCloses,BarHighs,BarLows};
			NewBucket ->
				{NewBucket,Index+1,High,Low,Close,<<BarOf/binary,(Index+1):?U>>,
				 <<BarCloses/binary,BClose:?F>>,<<BarHighs/binary,BHigh:?F>>,<<BarLows/binary,BLow:?F>>}
		end
	end,Bars),
	build(Table,ets:next(Table,Key),N+1,Close,<<Closes/binary,Close:?F>>,<<AbsSum/binary,U_AbsAcc:?F>>,<<CloseSum/binary,U_CloseAcc:?F>>,U_AbsAcc,U_CloseAcc,U_Bars).

	bucket(15,{Y,M,D,H,Mi,_S,_Rate})-> {Y,M,D,H,Mi div 15};
	bucket(60,{Y,M,D,H,_Mi,_S,_Rate})-> {Y,M,D,H};
	bucket(240,{Y,M,D,H,_Mi,_S,_Rate})-> {Y,M,D,H div 4};
	bucket(1440,{Y,M,D,_H,_Mi,_S,_Rate})-> {Y,M,D}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Positions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%position/2 returns the row number (0-based) of Key in Table. A simulator asks for consecutive bars, so the last answer
%is kept in the process dictionary: the same bar costs nothing, the next bar one ets:next. Anything else is found from
%the episode index (every Nth key with its position) and a short walk.
position(Table,Key)->
	case get({?MODULE,position,Table}) of
		{Key,Pos} ->
			Pos;
		{PrevKey,PrevPos} ->
			case ets:next(Table,PrevKey) of
				Key -> remember(Table,Key,PrevPos+1);
				_ -> remember(Table,Key,search(Table,Key))
			end;
		undefined ->
			remember(Table,Key,search(Table,Key))
	end.

	remember(Table,Key,Pos)->
		put({?MODULE,position,Table},{Key,Pos}),
		Pos.

	search(Table,Key)->
		{_Size,Step,Anchors} = episodes:index(Table),
		I = last_anchor_at_or_before(Key,Anchors,1,tuple_size(Anchors)),
		walk(Table,element(I,Anchors),Key,(I-1)*Step).

	%largest I in [Lo,Hi] with element(I,Anchors) =< Key (the first anchor is the first row, so one always exists)
	last_anchor_at_or_before(_Key,_Anchors,Lo,Hi) when Lo >= Hi ->
		Lo;
	last_anchor_at_or_before(Key,Anchors,Lo,Hi)->
		Mid = (Lo+Hi+1) div 2,
		case element(Mid,Anchors) =< Key of
			true -> last_anchor_at_or_before(Key,Anchors,Mid,Hi);
			false -> last_anchor_at_or_before(Key,Anchors,Lo,Mid-1)
		end.

	walk(_Table,Key,Key,Pos)->
		Pos;
	walk(Table,At,Key,Pos) when At < Key ->
		walk(Table,ets:next(Table,At),Key,Pos+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Answers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%vl/1: the length of the vector a spec produces.
vl({plt,_TF,N})-> N;
vl({plt,_TF,N,vol})-> N;
vl(ind)-> 12;
vl(clock)-> 4;
vl({clock,Harmonics})-> 2*Harmonics+2.

%answer/3: the sensor vector for the bar with key Key of Table.
answer(_Table,Key,clock)->
	clock(Key);
answer(_Table,Key,{clock,Harmonics})->
	clock(Key,Harmonics);
answer(Table,Key,Spec)->
	answer_at(ensure(Table),position(Table,Key),Spec).

%answer_at/3: the same from a series and a row number. Uses rows 0..P only.
%Two encodings of the price list, the same N closes (plt_closes/4) and the same difference from their mean:
%	{plt,TF,N}       scaled by the price level: in units of 0.1% of price (as sensor:normalize/1), divided by the square
%	                 root of the window's length relative to the 270-minute list, so every timeframe arrives in the same
%	                 range. A quiet year and a busy one differ by the factor their volatility differs by.
%	{plt,TF,N,vol}   scaled by the trailing volatility, on the scale of fx_IND (ind/2): divided by what a random walk
%	                 with the last day's typical one-minute move (mean absolute move of the last 1440 bars) deviates
%	                 from its mean over a window of TF*N minutes (that move times sqrt(TF*N/6)), and clipped to +/- 5.
%	                 The move is floored at ?VOL_FLOOR of the price, so a flat stretch or the first bars of the table
%	                 cannot blow the values up. Uses rows 0..P only, like everything here (test/2 checks it).
answer_at(Series,P,{plt,TF,N})->
	Values = plt_closes(Series,P,TF,N),
	Mean = lists:sum(Values)/N,
	Scale = 1000/(Mean*math:sqrt(TF*N/270)),
	[(V-Mean)*Scale || V <- Values];
answer_at(Series,P,{plt,TF,N,vol})->
	Values = plt_closes(Series,P,TF,N),
	Mean = lists:sum(Values)/N,
	Close = close(Series,P),
	Sigma = erlang:max(mean_move(Series,P,1440),?VOL_FLOOR*Close),
	Scale = 1/(Sigma*math:sqrt(TF*N/6)),
	[clip((V-Mean)*Scale) || V <- Values];
answer_at(Series,P,ind)->
	ind(Series,P).

	%plt_closes/4: the closes of the last N-1 completed TF-minute bars and the current close, oldest first. With less
	%history than that the list is padded with the oldest close available.
	plt_closes(Series,P,TF,N)->
		#{bar_of:=BarOf,closes:=BarCloses} = maps:get(TF,maps:get(tf,Series)),
		Bar = u32(BarOf,P),
		Have = erlang:min(N-1,Bar),%completed bars available
		Completed = floats(BarCloses,Bar-Have,Have),
		Now = close(Series,P),
		Pad = case Completed of
			[] -> lists:duplicate(N-1,Now);
			[Oldest|_] -> lists:duplicate(N-1-Have,Oldest)
		end,
		Pad ++ Completed ++ [Now].

%ind/2: twelve numbers, each about 0 +/- 1 in ordinary conditions and clipped to +/- 5.
%	1-6    price change over the last 5, 15, 60, 240, 1440 and 5760 bars, in units of what is usual for that range
%	7-9    distance of the price from its 60-, 240- and 1440-bar moving average, in the same units
%	10     recent activity: mean 1-bar move of the last 60 bars against the last 1440, minus 1
%	11     longer activity: the last 1440 bars against the last 14400, minus 1
%	12     where the price sits in the range of the last 24 completed hours: -1 at the low, +1 at the high
ind(Series,P)->
	Close = close(Series,P),
	Sigma = erlang:max(mean_move(Series,P,1440),1.0e-9),%what a 1-bar move usually is, over the last day
	Change = [clip((Close-close(Series,erlang:max(0,P-H)))/(Sigma*math:sqrt(H))) || H <- [5,15,60,240,1440,5760]],
	Distance = [clip((Close-mean_close(Series,P,W))/(Sigma*math:sqrt(W))) || W <- [60,240,1440]],
	Activity = [clip(mean_move(Series,P,60)/Sigma-1),
		clip(Sigma/erlang:max(mean_move(Series,P,14400),1.0e-9)-1)],
	#{bar_of:=BarOf,highs:=Highs,lows:=Lows} = maps:get(60,maps:get(tf,Series)),
	Bar = u32(BarOf,P),
	Have = erlang:min(24,Bar),
	High = lists:max([Close|floats(Highs,Bar-Have,Have)]),
	Low = lists:min([Close|floats(Lows,Bar-Have,Have)]),
	Range = case High > Low of
		true -> 2*(Close-Low)/(High-Low)-1;
		false -> 0.0
	end,
	Change ++ Distance ++ Activity ++ [Range].

%clock/2: the time of day as Harmonics sine and cosine pairs (once a day, twice a day, ...), then the place in the week
%as one pair. One pair lets a neuron pick out about half a day; the second lets it pick out a few hours, which is the
%width of the time-of-day pattern in the prices (REVIEW.md 8.22). Computed from the bar's own time: no table is read.
clock({Y,M,D,H,Mi,_S,_Rate},Harmonics)->
	Day = (H*60+Mi)/1440,
	Week = (calendar:day_of_the_week(Y,M,D)-1+Day)/7,
	lists:append([[math:sin(2*math:pi()*K*Day),math:cos(2*math:pi()*K*Day)] || K <- lists:seq(1,Harmonics)]) ++
		[math:sin(2*math:pi()*Week),math:cos(2*math:pi()*Week)].

clock({Y,M,D,H,Mi,_S,_Rate})->
	Day = (H*60+Mi)/1440,
	Week = (calendar:day_of_the_week(Y,M,D)-1+Day)/7,
	[math:sin(2*math:pi()*Day),math:cos(2*math:pi()*Day),math:sin(2*math:pi()*Week),math:cos(2*math:pi()*Week)].

	clip(X) when X > 5.0 -> 5.0;
	clip(X) when X < -5.0 -> -5.0;
	clip(X)-> X.

	close(#{closes:=Closes},P)->
		f64(Closes,P).

	%mean absolute 1-bar move over the W bars ending at P (fewer if the table starts later)
	mean_move(#{abs_sum:=AbsSum},P,W)->
		From = erlang:max(0,P-W),
		case P-From of
			0 -> 0.0;
			Count -> (f64(AbsSum,P+1)-f64(AbsSum,From+1))/Count
		end.

	%mean close over the W bars ending at P
	mean_close(#{close_sum:=CloseSum},P,W)->
		From = erlang:max(0,P+1-W),
		(f64(CloseSum,P+1)-f64(CloseSum,From))/(P+1-From).

	f64(Bin,I)->
		Skip = I*8,
		<<_:Skip/binary,X:?F,_/binary>> = Bin,
		X.

	u32(Bin,I)->
		Skip = I*4,
		<<_:Skip/binary,X:?U,_/binary>> = Bin,
		X.

	floats(_Bin,_From,0)->
		[];
	floats(Bin,From,Count)->
		[X || <<X:?F>> <= binary:part(Bin,From*8,Count*8)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% No-look-ahead test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%test/2 checks, at Count random rows of Table, that every answer is identical when the series are rebuilt from a copy of
%the table that ends at that row. Returns {ok,Checked} or {failed,[{Position,Spec}]}.
test(Table,Count)->
	Full = ensure(Table),
	Size = maps:get(size,Full),
	Specs = [{plt,TF,N} || {TF,N} <- [{15,24},{60,24},{240,30},{1440,20}]] ++
		[{plt,TF,N,vol} || {TF,N} <- [{15,32},{60,24},{240,30},{1440,20}]] ++ [ind],
	Keys = keys(Table,ets:first(Table),[]),
	Positions = lists:usort([rand:uniform(Size)-1 || _ <- lists:seq(1,Count)] ++ [0,1,Size-1]),
	Failed = lists:append([begin
		Cut = ets:new(fx_series_cut,[ordered_set,{keypos,2}]),
		[ets:insert(Cut,ets:lookup(Table,Key)) || Key <- lists:sublist(Keys,P+1)],
		Short = build(Cut),
		ets:delete(Cut),
		Key = lists:nth(P+1,Keys),
		[{P,Spec} || Spec <- Specs, answer_at(Full,P,Spec) =/= answer_at(Short,P,Spec)] ++
		[{P,position} || position(Table,Key) =/= P]
	end || P <- Positions]),
	case Failed of
		[] -> {ok,length(Positions)*(length(Specs)+1)};
		_ -> {failed,Failed}
	end.

	keys(_Table,'$end_of_table',Acc)->
		lists:reverse(Acc);
	keys(Table,Key,Acc)->
		keys(Table,ets:next(Table,Key),[Key|Acc]).
