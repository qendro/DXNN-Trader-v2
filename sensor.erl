
-module(sensor).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) ->
		%qlog:xLog(pid_to_list(ExoSelf_PId), "Sensor: ~p in Prep waiting init message from ExoSelf_Id: ~p", [self(), ExoSelf_PId]),
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds,OpMode}} ->
			put(opmode,OpMode),
			%qlog:xLog(pid_to_list(ExoSelf_PId), "Sensor: ~p recieved Init message from Exoself: ~p", [self(), ExoSelf_PId]),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)
	end.
%When gen/2 is executed it spawns the sensor element and immediately begins to wait for its initial state message.

loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds)->
	receive
		{Cx_PId,sync}-> 
			%qlog:xLog(pid_to_list(ExoSelf_PId), "Sensor: ~p received sync from Cortex: ~p. ExoSelf_Id: ~p", [self(), Cx_PId, ExoSelf_PId]),
			SensoryVector = sensor:SensorName(ExoSelf_PId,VL,Parameters,Scape),
			[Pid ! {self(),forward,SensoryVector} || Pid <- Fanout_PIds],
			%qlog:xLog(pid_to_list(ExoSelf_PId), "Sensor: ~p sent forward to Neurons: ~p. ExoSelf_Id: ~p, SensoryVector: ~p", [self(), Fanout_PIds, ExoSelf_PId, SensoryVector]),
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds);
		{ExoSelf_PId,reset_prep} ->
			%First step of the full network reset (exoself:full_reset/2). Messages are taken in order, so a sync that came
			%first has been answered and its vector sent before this reply: after it, this sensor sends nothing more
			%until the cortex starts the next bar.
			ExoSelf_PId ! {self(),ready},
			loop(Id,ExoSelf_PId,Cx_PId,Scape,SensorName,VL,Parameters,Fanout_PIds);
		{ExoSelf_PId,terminate} ->
			ok
	end.
%The sensor process accepts only 2 types of messages, both from the cortex. The sensor can either be triggered to begin gathering sensory data based on its sensory role, or terminate if the cortex requests so.

rng(ExoSelf_PId,VL,_Scape)->
	rng1(VL,[]).
rng1(0,Acc)->
	Acc;
rng1(VL,Acc)-> 
	rng1(VL-1,[rand:uniform()|Acc]).
%rng/2 is a simple random number generator that produces a vector of random values, each between 0 and 1. The length of the vector is defined by the VL, which itself is specified within the sensor record.

%This function encodes the Price Chart Input (PCI) sensor data.
% A reconstructed visual representation of price data, similar to a candlestick chart.
% Encoded as a reduced-resolution grid (e.g., 100x20 or 10x10) to preserve geometric patterns.
% The grid uses trinary encoding:
% -1 → Background.
% 0 → Body of candlestick.
% 1 → Key features (e.g., candle wicks, highlights).
fx_PCI(Exoself_Id,VL,Parameters,Scape)->
	[HRes,VRes] = Parameters,
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 200
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:gt_start(),config:gt_end()};
		benchmark ->
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:bench_start(),config:bench_end()};
		live_trading ->
			% In live trading, use offsets relative to the latest bar: start=0 (last), end=1000 (1000 bars window)
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],0,config:bench_end()}
	end,
	receive 
		{Scape,Result}->
			Result
	end.

%This function encodes the Price List Input (PLI) sensor data.
% It retrieves the last 200 rows of the EURUSD1 table and outputs the close prices as a vector.
% The function takes the Exoself_Id, VL (vector length), Parameters (which includes the resolution and type of data), and Scape (the process that handles the data
fx_PLI(Exoself_Id,VL,Parameters,Scape)->
	[HRes,Type] = Parameters,%HRes= ; Type=open|close|high|low
	case get(opmode) of
		gt	->
			%Normal, assuming we have 10000 rows, we start from 1000 to 200
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],config:gt_start(),config:gt_end()};
		benchmark ->
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],config:bench_start(),config:bench_end()};
		live_trading ->
			% In live trading, use offsets relative to the latest bar: start=0 (last), end=1000 (1000 bars window)
			Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,list_sensor],0,config:bench_end()}
	end,
	receive 
		{Scape,Result}->
			normalize(Result)
	end.

%normalize/1 expresses each price relative to the window mean, in units of 0.1% of price (~11.7 pips at 1.17).
%The previous version divided raw prices (~1.17) by the vector's L2 norm, so every input was ~1/sqrt(N) and the
%actual price movement only showed up around the 5th decimal place, i.e. the network saw a near-constant vector.
%Using the mean (not the newest bar) as reference keeps the encoding independent of list order, which differs
%between fx.erl and live_scape.erl.
normalize([])->
	[];
normalize(Vector)->
	Mean = lists:sum(Vector)/length(Vector),
	case Mean == 0 of
		true -> [0.0 || _ <- Vector];
		false -> [(Val-Mean)/Mean*1000 || Val <- Vector]
	end.

%Sensors answered from the shared price arrays (fx_series.erl). They can look back days at the cost of a few reads:
%	fx_PLT    price list by timeframe: parameters [TF,N], the closes of the last N bars of TF minutes (15, 60, 240 or 1440)
%	fx_IND    twelve indicators: change over several ranges, distance from moving averages, activity, place in the day's range
%	fx_Clock  time of day and day of week
%The values arrive already scaled (about 0 +/- a few), so nothing is normalised here.
%Parameters [TF,N] is the list scaled by the price level, [TF,N,vol] the one scaled by the trailing volatility
%(config:plt_encoding/0, fx_series:answer_at/3). The choice is stored with the sensor, so saved agents keep theirs.
fx_PLT(_Exoself_Id,VL,[TF,N],Scape)->
	series_request(Scape,{plt,TF,N},VL);
fx_PLT(_Exoself_Id,VL,[TF,N,vol],Scape)->
	series_request(Scape,{plt,TF,N,vol},VL).

fx_IND(_Exoself_Id,VL,_Parameters,Scape)->
	series_request(Scape,ind,VL).

%Parameters [clock] is the four-value clock; [clock,Harmonics] the one with that many daily pairs (fx_series:clock/2).
fx_Clock(_Exoself_Id,VL,[clock,Harmonics],Scape)->
	series_request(Scape,{clock,Harmonics},VL);
fx_Clock(_Exoself_Id,VL,_Parameters,Scape)->
	series_request(Scape,clock,VL).

	series_request(Scape,Spec,VL)->
		case get(opmode) of
			live_trading ->
				%The live path has no packed arrays yet. An agent that uses these sensors is not ready to trade live.
				lists:duplicate(VL,0.0);
			OpMode ->
				{Start,Finish} = case OpMode of
					gt -> {config:gt_start(),config:gt_end()};
					_ -> {config:bench_start(),config:bench_end()}
				end,
				Scape ! {self(),sense,config:primary_currency_pair(),close,[Spec,series_sensor],Start,Finish},
				receive
					{Scape,Result}->
						Result
				end
		end.

fx_Internals(Exoself_Id,VL,Parameters,Scape)->
	Scape ! {self(),sense,internals,Parameters},
	receive
		{Scape,Result}->
			Result
	end.

%fx_Position: the position held now, -1, 0 or 1, and nothing about open profit (config:position_sensor/0). The
%simulator answers [Position] (fx:internals/2); a scape that answers the three-value account state (live_scape) gives
%the position first, so only that is kept.
fx_Position(_Exoself_Id,_VL,_Parameters,Scape)->
	Scape ! {self(),sense,internals,position},
	receive
		{Scape,[Position|_]}->
			[Position]
	end.
