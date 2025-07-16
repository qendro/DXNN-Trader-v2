%% This source code and work is provided and developed by DXNN Research Group WWW.DXNNResearch.COM
%%
%Copyright (C) 2012 by Gene Sher, DXNN Research Group, CorticalComputer@gmail.com
%All rights reserved.
%
%This code is licensed under the version 3 of the GNU General Public License. Please see the LICENSE file that accompanies this project for the terms of use.

-module(fx).
-compile(export_all).
-define(ALL_TABLES,[metadata,'EURUSD1','EURUSD15','EURUSD30','EURUSD60']).
%-define(T2D,[{'EURUSD1',1},{'EURUSD15',15},{'EURUSD30',30},{'EURUSD60',60}]).
%-define(SOURCE_DIR,"/home/puter/.wine/dosdevices/c:/Program Files/MetaTrader - Alpari (US)/experts/files/").
-define(FX_TABLES_DIR,"fx_tables/").
-define(SOURCE_DIR,"fx_tables/").
-record(technical,{
	id,%%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate}
	open,
	high,
	low,
	close,
	volume}).
-record(metadata,{
	feature, %P={Currency,Feature}
	first,
	last,
	avg,
	dev,
	stdev,
	quantile25,
	quantile50,
	quantile75,
	max,
	min}).
-define(FEATURES,[
	open,
	high,
	low,
	close,
	volume]).

-record(technical_e,{
	id,	%%%key={Year,Month,Day,Hour,Minute,Second}
	open,
	high,
	low,
	close,
	volume,
	diff,
	up_diff,
	down_diff,
	ud_ema27,
	dd_ema27,
	ema2,
	ema3,
	ema6,
	ema9,
	ema14,
	ema26,
	ema50,
	ema100,
	sma2,
	sma3,
	sma6,
	sma9,
	sma14,
	sma26,
	sma50,
	sma100,
	rsi,
	macd,
	macd_signal,
	adi,
	sts_kfast,
	sts_dfast,
	sts_dslow,
	sts_dfull,
	bix9,
	trix9
}).

-define(ACTUATOR_CA_TAG,false).
-define(SENSE_CA_TAG,false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FX SIMULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{table_name,feature,index_start,index_end,index,price_list=[]}).
-record(account,{leverage=50,lot=10000,spread=0.000150,margin=0,balance=300,net_asset_value=300,realized_PL=0,unrealized_PL=0,order}).
-record(order,{pair,position,entry,current,units,change,percentage_change,profit}).

go()->
	Signals =[-1,-1,-1,-1,0,0,0,1,1,1,-1,-1,1,1,0,0,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,1,1,
 1,1,1,-1,-1,-1,-1,-1,0,0,0,0,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1,
 -1,-1,-1,-1,-1,-1,0,0,-1,-1,-1,0,0,-1,-1,-1,-1,-1,-1,-1,-1,0,0,-1,-1,-1,-1,
 -1,1,1,1,1,1,0,0,1,1,1,-1,-1,-1,1,1,1,1,-1,-1,-1,1,1,1,1,0,0,1,1,1,1,1,1,-1,
 -1,1,1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,-1,-1,
 1,1,1,1,-1,-1,-1,1,1,1,1,0,0,-1,-1,-1,-1,1,1,0,0,1,1,-1,-1,-1,-1,-1,-1,-1,1,
 1,1,1,1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,-1,-1,0,0,0,0,0,0,
 -1,-1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,
 -1,-1,-1,0,0,-1,-1,1,1,1,1,1,-1,-1,-1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
 1,1,1,1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,-1,1,1,-1,-1,-1,-1,-1,1,1,
 1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,-1,-1,-1,-1,
 -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,-1,-1,-1,1,1,0,0,0,-1,-1,-1,-1,-1,-1,0,
 0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,-1,-1,-1,1,1,1,-1,-1,
 -1,-1,-1,-1,-1,1,1,1,-1,-1,1,1,-1,-1,-1,-1,-1,0,0,-1,-1,-1,-1,-1,1,1,0,0,-1,
 -1,-1,-1,-1,-1,-1,1,1,1,1,0,0,-1,-1,-1,-1,-1,-1,-1,-1,0,0,1,1,1,1,1,1,1,1,1,
 1,0,0,-1,-1,-1,-1,1,1,1,1,1,-1,-1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,0,1,1,1,1,
 1,1,1,1,1,0,0,0,0,1,1,1,1,-1,-1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,1,1,1,-1,
 -1,-1,-1,-1,-1,-1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,-1,
 -1,-1,1,1,1,1,0,0,-1,-1,1,1,1,1,1,0,0,-1,-1,1,1,1,1,-1,-1,1,1,1,1,1,1,1,1,0,
 0,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,1,1,1,1,
 -1,-1,-1,-1,-1,-1,-1,1,1,1,-1,-1,-1,-1,0,0,-1,-1,-1,1,1,1,1,1,1,1,-1,-1,-1,
 -1,-1,1,1,1,0,-1,-1,-1,-1,-1,-1,1,1,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 -1,-1,-1,1,1,1,1,1,1,-1,-1,-1,-1,-1,1,1,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,-1,
 -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,1,0,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
 1,1,-1,-1,-1,1,1,-1,-1,1,1,1,1,1,1,1,1,1,1,-1,-1,-1,-1,1,1,1,1,1,1,1,1,-1,-1,
 -1,-1,-1,0,0,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1],
 	put(opmode,gt),
 	sensors:fx_ListSensor(10,void,[10,list_sensor]),
	[{actuators:fx_Trade(void,[TradeSignal],void,void),io:format("TradeSignal:~p~n",[TradeSignal])} || TradeSignal <- Signals],
	erase().
	
market_properties()->
	random:seed(now()),
	market_properties('EURUSD15',close,[3,list_sensor],800,200).
market_properties(CurrencyPair,Feature,Parameters,StartIndex,EndIndex)->
	max(CurrencyPair,Feature,Parameters,StartIndex,EndIndex),
	erase().

	random(CurrencyPair,Feature,Parameters,Start,Finish)->
		S = #state{},
		A = #account{},
		InitS = init_state(S,CurrencyPair,Feature,Start,Finish),
		AvgRandomProfit=lists:sum([random_profit(CurrencyPair,InitS,A) || _<-lists:seq(1,1000)])/1000,
		erase().
	random_profit(CurrencyPair,S,A)->
		Trade_Signal = random:uniform(3)-2,
		case profit_trade(S,A,Trade_Signal) of
			{U_S,U_A}->
				random_profit(CurrencyPair,U_S,U_A);
			Profit ->
				Profit
		end.
		
	max(CurrencyPair,Feature,Parameters,Start,Finish)->
		S = #state{},
		A = #account{},
		InitS = init_state(S,CurrencyPair,Feature,Start,Finish),
		max_profit(CurrencyPair,InitS,A).
	max_profit(CurrencyPair,S,A)->
		Index=S#state.index,
		EndIndex=S#state.index_end,
		T = fx:lookup(CurrencyPair,Index),
		Close = T#technical.close,
		{FlipIndex,FlipClose} = find_next_flip(CurrencyPair,Index,Close,EndIndex),
		TradeSignal=if 
			FlipClose > (Close+ 0.000150*Close) -> 1;
			FlipClose < (Close- 0.000150*Close) -> -1;
			true -> 0
		end,
		case forward(CurrencyPair,S,A,TradeSignal,Index,FlipIndex) of
			{U_S,U_A} ->
				max_profit(CurrencyPair,U_S,U_A);
			_ ->
				ok
		end.
		
		find_next_flip(CurrencyPair,Index,Close,EndIndex)->
			NextIndex = fx:next(CurrencyPair,Index),
			NextT = fx:lookup(CurrencyPair,NextIndex),
			NextClose = NextT#technical.close,
			case NextClose > Close of
				true ->
					find_next_flip(CurrencyPair,Index,Close,EndIndex,long);
				false ->
					find_next_flip(CurrencyPair,Index,Close,EndIndex,short)
			end.
		
			find_next_flip(CurrencyPair,EndIndex,Close,EndIndex,long)->
				{EndIndex,Close};
			find_next_flip(CurrencyPair,Index,Close,EndIndex,long)->
				%move the index forward until a change in sign occurs. Then return index of the last closing price of the same sign.
				NextIndex = fx:next(CurrencyPair,Index),
				NextT = fx:lookup(CurrencyPair,NextIndex),
				NextClose = NextT#technical.close,
%				io:format("Close:~p NextClose:~p~n",[Close,NextClose]),
				case (NextClose - Close) > 0 of
					true ->
						find_next_flip(CurrencyPair,NextIndex,NextClose,EndIndex,long);
					false ->
						{Index,Close}
				end;
			find_next_flip(CurrencyPair,EndIndex,Close,EndIndex,short)->
				{EndIndex,Close};
			find_next_flip(CurrencyPair,Index,Close,EndIndex,short)->
				%move the index forward until a change in sign occurs. Then return index of the last closing price of the same sign.
				NextIndex = fx:next(CurrencyPair,Index),
				NextT = fx:lookup(CurrencyPair,NextIndex),
				NextClose = NextT#technical.close,
%				io:format("Close:~p NextClose:~p Close*NextClose:~p~n",[Close,NextClose,Close*NextClose]),
				case (NextClose - Close) < 0 of
					true ->
						find_next_flip(CurrencyPair,NextIndex,NextClose,EndIndex,short);
					false ->
						%Is it a true flip, or a 0 flip (no profit) and then another slide towards the same direction?
						%To check this, check if the next slide is 0 trade signal one, and then if afterwards, it goes in the same direction again. And if it does go in the same direciton again,
						%check if at the end of that slide, there is a profit... if at the end of that slide there is no profit again from the orignal, then check if therei safter that...
						{Index,Close}
				end.
		
		forward(CurrencyPair,S,A,TradeSignal,FlipIndex,FlipIndex)->
			profit_trade(S,A,TradeSignal);
		forward(CurrencyPair,S,A,TradeSignal,Index,FlipIndex)->
			{U_S,U_A}=profit_trade(S,A,TradeSignal),
			NextIndex = U_S#state.index,
			forward(CurrencyPair,U_S,U_A,TradeSignal,NextIndex,FlipIndex).
			
		max_profit_sense(S,A,Parameters)->
			{Result,U_S} = sense(S,Parameters),
			U_S.
		
		profit_trade(S,A,TradeSignal)->	
%		io:format("******************************STARTING TO PROCESS TRADE SIGNAL******************************~n"),
			case get(trade_signal) of
				undefined -> put(trade_signal,[TradeSignal]);
				TSL -> put(trade_signal,[TradeSignal|TSL])
			end,
			U_A = make_trade(S,A,TradeSignal),
			Total_Profit = A#account.balance + A#account.unrealized_PL,
%			io:format("TP:~p~n",[Total_Profit]),
			case (U_A#account.balance + U_A#account.unrealized_PL) =< 100 of
				true ->
					io:format("Lost all money~n"),
					(U_A#account.balance + U_A#account.unrealized_PL);
%					io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n");
				false ->
					case update_state(S) of
						sim_over ->
							Total_Profit = A#account.balance + A#account.unrealized_PL,
							io:format("Sim Over:~p~n",[Total_Profit]),
							Total_Profit;
%							io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n");
						U_S ->
							U_A2 = update_account(U_S,U_A),
%							io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n"),
							{U_S,U_A2}
					end
			end.

ts(PId)->
	PId ! {self(),sense,'EURUSD15',close,[HRes=5,VRes=5,graph_sensor]},
	receive 
		{From,Result}->
			io:format("From:~p Result:~p~n",[From,Result])
	end.
tt(PId,TradeSignal)->
	PId ! {self(),trade,'EURUSD15',TradeSignal},
	receive 
		{From,Result}->
			io:format("Trade_Signal:~p~n Result:~p~n",[TradeSignal,Result])
	end.

%Entry Point for Starting the Forex Simulation
% Recieves a message from the ExoSelf process to start the simulation.
% Message is of the form spawn(fx,sim,[ExoSelf_PID]).
% Defines the initial state and account, and starts the simulation loop for this PID
% Calls the recursive sim/3 function to handle the simulation logic.
sim(ExoSelf)->io:format("Started~n"),
	put(prev_PC,0),
	S = #state{},
	A = #account{},
	sim(ExoSelf,S,A).

% Recursive function that handles the simulation logic.
% It processes different messages received from the ExoSelf process.
% Handles the following messages:
% - {From,sense,TableName,Feature,Parameters,Start,Finish} - Starts sensing the market data.
% 	Parameters is a list that can contain the resolution and type of sensor (e.g., graph_sensor, list_sensor).
% - {From,sense,internals,Parameters} - Returns the current state of the account and order.
% - {From,trade,TableName,TradeSignal} - Processes a trade signal and updates the account state.
% - restart - Resets the simulation state and account.
% - terminate - Terminates the simulation.
% The function also handles a timeout of 10 seconds to recheck the state.
% If the simulation is over, it returns the total profit and resets the state and account.
sim(ExoSelf,S,A)-> 
	receive
		{From,sense,TableName,Feature,Parameters,Start,Finish}->%Parameters:{VL,SignalEncoding}
			% This is the main sensing signal that starts the simulation.
			% It receives the table name, feature, parameters, start and finish indices.
			% Parameters is a list that can contain the resolution and type of sensor (e.g., graph_sensor, list_sensor).
			% Start and Finish are the indices to start and finish sensing.
			% The function initializes the state with the given parameters and starts sensing the market data.
			% It returns the result of the sensing operation to the From process.
			% The result is a tuple of the form {Result,U_S}, where Result is the sensed data and U_S is the updated state.
			% If the table name is undefined, it initializes the state with the given parameters.
			%io:format("******************************STARTING TO PROCESS SENSE SIGNAL******************************~n"),
			{Result,U_S}=case S#state.table_name of
				undefined ->
					sense(init_state(S,TableName,Feature,Start,Finish),Parameters);
				TableName ->
					sense(S,Parameters)
			end,
			From ! {self(),Result},
			%io:format("State:~p~n",[U_S]),
			%io:format("******************************FINISHED PROCESSING SENSE SIGNAL******************************~n"),
			case ?SENSE_CA_TAG of
				true ->
					timer:sleep(10000),
					IndexT = U_S#state.index,
					NextIndexT = fx:next(TableName,IndexT),
					RowT = fx:lookup(TableName,IndexT),
					NextRowT = fx:lookup(TableName,NextIndexT),
					QuoteT = RowT#technical.close,
					NextQuoteT = NextRowT#technical.close,
					io:format("Sense~n Index:~p~n Quote:~p~n Next Index:~p~n Next Quote:~p~n NetWorth:~p~n",[IndexT,QuoteT,NextIndexT,NextQuoteT,A#account.balance + A#account.unrealized_PL]);
				false ->
					ok
			end,
			fx:sim(ExoSelf,U_S,A);
		{From,sense,internals,Parameters}->
			%Internals are the current long/short/nothing position, the buy price (or -1 if in do nothing state)
			%io:format("A#account.order: ~p~n",[A#account.order]),
			% This is the current state of the account
			% It returns the current position, entry price, and previous percentage change.
			% Parameters is a list that can contain the resolution and type of sensor (e.g., graph_sensor, list_sensor).
			% The result is a list of three elements: [Position, Entry, Previous_Percentage_Change].
			% If there is no order, it returns [0, 0, 0].
			Result = case A#account.order of
				undefined ->
					[0,0,0];
				O ->
					%io:format("O:~p~n",[O]),
					Position = O#order.position,
					Entry = O#order.entry,
					Percentage_Change = O#order.percentage_change,
					%io:format("self():~p CurPC:~p PrevPC:~p~n",[self(),Percentage_Change,get(prev_PC)]),
					[Position,Entry,get(prev_PC)]
			end,
			From ! {self(),Result},
			fx:sim(ExoSelf,S,A);
		{From,trade,TableName,TradeSignal}->
			% This is the main trading signal that executes a trade.
			% It receives the table name, trade signal, and the current state and account.
			% The function processes the trade signal and updates the account accordingly.
			% It returns the result of the trading operation to the From process.
			% The result is a tuple of the form {Result,U_A}, where Result is the trade result and U_A is the updated account.
			% If the table name is undefined, it initializes the state with the given parameters.
			%io:format("******************************STARTING TO PROCESS TRADE SIGNAL******************************~n"),
			U_A = make_trade(S,A,TradeSignal),
			Total_Profit = A#account.balance + A#account.unrealized_PL,
			
			case ?ACTUATOR_CA_TAG of
				true ->
					timer:sleep(10000),
					IndexT = S#state.index,
					NextIndexT = fx:next(TableName,IndexT),
					RowT = fx:lookup(TableName,IndexT),
					NextRowT = fx:lookup(TableName,NextIndexT),
					QuoteT = RowT#technical.close,
					NextQuoteT = NextRowT#technical.close,
					io:format("Trade~n Index:~p~n Quote:~p~n Next Index:~p~n Next Quote:~p~n TradeSignal:~p~n NetWorth:~p~n",[IndexT,QuoteT,NextIndexT,NextQuoteT,TradeSignal,Total_Profit]);
				false ->
					ok
			end,
			case (U_A#account.balance + U_A#account.unrealized_PL) =< 100 of
				true ->
					From ! {self(),0,1},
					io:format("Lost all money~n"),
					%io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n"),
					put(prev_PC,0),
					fx:sim(ExoSelf,#state{},#account{});
				false ->
					case update_state(S) of
						sim_over ->
							Total_Profit = A#account.balance + A#account.unrealized_PL,
							From ! {self(),Total_Profit,1},
							%io:format("Sim Over:~p~n",[Total_Profit]),
							%io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n"),
							put(prev_PC,0),
							fx:sim(ExoSelf,#state{},#account{});
						U_S ->
							From ! {self(),0,0},
							U_A2 = update_account(U_S,U_A),
							%io:format("******************************FINISHED PROCESSING TRADE SIGNAL******************************~n"),
							fx:sim(ExoSelf,U_S,U_A2)
					end
			end;
		restart ->
			fx:sim(ExoSelf,#state{},#account{});
		terminate ->
			ok
		after 10000 ->
			fx:sim(ExoSelf,S,A)
	end.

%This function initializes the state for the simulation.
% It takes the current state S, the table name TableName, the feature to be used Feature,
% the start index StartBL, and the end index EndBL.
% It sets the initial index to the last index of the table and the end index to the last index or the previous index based on the EndBL parameter.
% The function returns a new state record with the initialized values.
% The StartBL and EndBL parameters can be used to specify the range of indices to be used in the simulation.
% StartBL can be a specific index or 'last' to indicate the last index.
% EndBL can also be a specific index or 'last' to indicate the last index.
% The function uses the ets module to retrieve the last index of the table and the previous index based on the StartBL and EndBL parameters.
% The ets module is used to access the table data stored in the Erlang Term Storage (ETS).
% The function initializes the state with the table name, feature, start index, end index, and the current index.
% The current index is set to the start index, which is the last index of the table.
% The function returns the initialized state record.
% The state record contains the table name, feature, start index, end index, and current index.
% The state record is used to keep track of the current state of the simulation.
init_state(S,TableName,Feature,StartBL,EndBL)->
	Index_End = case EndBL of
		last ->
			ets:last(TableName);
		_ ->
			prev(TableName,ets:last(TableName),prev,EndBL)
	end,
	Index_Start = prev(TableName,ets:last(TableName),prev,StartBL),
	S#state{
		table_name = TableName,
		feature = Feature,
		index_start = Index_Start,
		index_end = Index_End,
		index = Index_Start
	}.	


% This function retrieves the previous index based on the current index and the specified direction.
% It takes the table name TableName, the current index CurrentIndex, the direction Direction (prev or next), and the specified index SpecifiedIndex.
update_state(S)->
	NextIndex = fx:next(S#state.table_name,S#state.index),
	case NextIndex == S#state.index_end of
		true ->
			sim_over;
		false ->
			S#state{index=NextIndex}
	end.

% This function updates the account based on the current state and order.
% It takes the current state S and the account A as parameters.
% It checks if there is an order in the account.
% If there is an order, it retrieves the current row from the table based on the state's table name and index.
% It calculates the close price, balance, position, entry price, and units from the order.
% It then calculates the change in price, percentage change, profit, unrealized profit/loss, and net asset value.
% It updates the order with the current price
update_account(S,A)->
%	io:format("update_account(S:~p,A:~p)~n",[S,A]),
	case A#account.order of
		undefined ->
			nothing_to_update,
			A;
		O ->
%			io:format("Current Account & Order:~n"),
%				r(A),r(O),
			TableName = S#state.table_name,
			Index = S#state.index,
			Row = fx:lookup(TableName,Index),
			Close = Row#technical.close,
			Balance = A#account.balance,
			Position = O#order.position,
			
			Entry = O#order.entry,
			Units = O#order.units,
			Change = Close - Entry,
			Percentage_Change = (Change/Entry)*100,
			Profit = Position*Change*Units,
			Unrealized_PL = Profit,
			Net_Asset_Value = Balance + Unrealized_PL,
			U_O = O#order{current=Close,change=Change,percentage_change=Percentage_Change,profit=Profit},
			U_A = A#account{unrealized_PL=Unrealized_PL,net_asset_value=Net_Asset_Value,order=U_O},
%			io:format("Updated Account & Order: Close:~p Units:~p ~n",[Close,Units]),
%				r(U_A),r(U_O),
			put(prev_PC,O#order.percentage_change),
			U_A
	end.

% This function retrieves the fields of a record.
% It takes a record R as a parameter and prints the field names and values.
% It uses the record_info function to get the field names based on the record type.
r()->r(#account{}).
r(R)->
	[RName|Element_Values] = A=tuple_to_list(R),
	Element_Names = case RName of
		account ->
			record_info(fields, account);
		order ->
			record_info(fields, order);
		state ->
			record_info(fields, state)
	end,
	io:format("~p:~n",[RName]),
	r(Element_Names,Element_Values).
	
	r([EName|ENames],[EValue|EValues])->
		io:format(" ~p = ~p~n",[EName,EValue]),
		r(ENames,EValues);
	r([],[])->
		ok.
		
%-record(state,{table_name,feature,index_start,index_end,index,price_list}).
%-record(account,{leverage=50,lot=10000,spread=0.000150,margin=0,balance=300,net_asset_value=300,realized_PL=0,unrealized_PL=0,order}).
%-record(order,{pair,position,entry,units,change,percentage_change,profit}).
determine_profit(A)->
	U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL.

make_trade(S,A,Action)->
	case A#account.order of
		undefined ->
			case Action == 0 of
				true ->%Do nothing
					A;
				false ->%Open new position
					open_order(S,A,Action)
			end;
		O ->
			case Action == 0 of
				true ->%Close Order
					close_order(S,A);
				false ->%Modify Order
					Current_Position = O#order.position,
					case Current_Position == Action of
						true ->
							A;
						false ->
							U_A=close_order(S,A),
							open_order(S,U_A,Action)
					end
			end
	end.
	
open_order(S,A,Action)->
	Order_Size = 0.2,% Open order size is 20% of the current account balance.
	BuyMoney = 100,
	Spread=A#account.spread,
	Leverage = A#account.leverage,
	Balance = A#account.balance,
	TableName = S#state.table_name,
	Index = S#state.index,
	Row = fx:lookup(TableName,Index),
	Quote = Row#technical.close,
	Entry = Quote + Spread*Action,
	Units = round((BuyMoney*Leverage)/Entry),
	Change= Quote-Entry,
	PChange = (Change/Entry)*100,
	Profit=Action*Change*Units,
	Unrealized_PL = Profit,
	New_Order = #order{pair=TableName,position=Action,entry=Entry,current=Quote,units=Units,change=Change,percentage_change=PChange,profit=Profit},
	A#account{unrealized_PL = Unrealized_PL,order=New_Order}.

close_order(S,A)->
	U_Balance = A#account.balance + A#account.unrealized_PL,
	U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL,
	A#account{balance=U_Balance,realized_PL=U_Realized_PL,unrealized_PL = 0,order=undefined}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FX SENSORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense(S,Parameters)->
	case Parameters of
		[HRes,VRes,graph_sensor]->
			{Result,U_S}=plane_encoded(HRes,VRes,S);
		[HRes,list_sensor]->
			{Result,U_S}=list_encoded(HRes,S)
	end.

% This function encodes the list sensor data.
% It takes the resolution HRes and the state S as parameters.
% It retrieves the current index and currency pair from the state.
% It returns a tuple containing the list of closing prices and the updated state.
% The list of closing prices is extracted from the price list.
list_encoded(HRes,S)->
	Index = S#state.index,
	CurrencyPair = S#state.table_name,
	PriceListPs = S#state.price_list,
	case lists:keyfind(HRes, 2,PriceListPs) of
		false ->
			Trailing_Index = prev(CurrencyPair,Index,prev,HRes-1),
			U_PList = fx_GetPriceList(CurrencyPair,Trailing_Index,HRes,[]),
			U_PriceListPs = [{U_PList,HRes}|PriceListPs];
		{PList,HRes} ->
			R = fx:lookup(CurrencyPair,Index),
			U_PList = [{R#technical.open,R#technical.close,R#technical.high,R#technical.low}|lists:sublist(PList,HRes-1)],
			U_PriceListPs = lists:keyreplace(HRes, 2, PriceListPs, {U_PList,HRes})
	end,
	%io:format("PriceList:~p~n",[U_PList]),
	%io:format("List:~p~n",[[Close||{_Open,Close,_High,_Low}<-U_PList]]),
	U_S=S#state{price_list=U_PriceListPs},
	{[Close||{_Open,Close,_High,_Low}<-U_PList],U_S}.

% This function encodes the plane sensor data.
% It takes the resolution HRes, VRes, and the state S as parameters.
% It retrieves the current index and currency pair from the state.
% It returns a tuple containing the encoded plane data and the updated state.
% The encoded plane data is a list of values representing the open, close, high, and low prices.
% The function calculates the maximum and minimum values for the high and low prices,
% and determines the vertical step and starting position for the plane encoding.
plane_encoded(HRes,VRes,S)->
	Index = S#state.index,
	CurrencyPair = S#state.table_name,
	PriceListPs = S#state.price_list,
	case lists:keyfind(HRes, 2,PriceListPs) of
		false ->
			Trailing_Index = prev(CurrencyPair,Index,prev,HRes-1),
			U_PList = fx_GetPriceList(CurrencyPair,Trailing_Index,HRes,[]),
			U_PriceListPs = [{U_PList,HRes}|PriceListPs];
		{PList,HRes} ->
			R = fx:lookup(CurrencyPair,Index),
			U_PList = [{R#technical.open,R#technical.close,R#technical.high,R#technical.low}|lists:sublist(PList,HRes-1)],
			U_PriceListPs = lists:keyreplace(HRes, 2, PriceListPs, {U_PList,HRes})
			
	end,
	LVMax1 = lists:max([High||{_Open,_Close,High,_Low}<-U_PList]),
	LVMin1 = lists:min([Low||{_Open,_Close,_High,Low}<-U_PList]),
	LVMax =LVMax1+abs(LVMax1-LVMin1)/20,
	LVMin =LVMin1-abs(LVMax1-LVMin1)/20,
	VStep = (LVMax-LVMin)/VRes,
	V_StartPos = LVMin + VStep/2,
	U_S=S#state{price_list=U_PriceListPs},
	{l2fx(HRes*VRes,{U_PList,U_PList},V_StartPos,VStep,[]),U_S}.
	
	fx_GetPriceList(_Table,EndKey,0,Acc)->
%		io:format("EndKey:~p~n",[EndKey]),
		Acc;
	fx_GetPriceList(_Table,'end_of_table',_Index,Acc)->
		exit("fx_GetPriceList, reached end_of_table");
	fx_GetPriceList(Table,Key,Index,Acc) ->
		R = fx:lookup(Table,Key),
		%io:format("R:~p~n",[R]),
		fx_GetPriceList(Table,fx:next(Table,Key),Index-1,[{R#technical.open,R#technical.close,R#technical.high,R#technical.low}|Acc]).
		
	l2fx(Index,{[{Open,Close,High,Low}|VList],MemList},VPos,VStep,Acc)->
%		io:format("Index:~p {Open,Close,High,Low}:~p VPos:~p VStep:~p~n",[Index,{Open,Close,High,Low},VPos,VStep]),
		{BHigh,BLow} = case Open > Close of
			true ->
				{Open,Close};
			false ->
				{Close,Open}
		end,
		O = case (VPos+VStep/2 > BLow) and (VPos-VStep/2 =< BHigh) of %(VPos+VStep)/2 > Open	(Close =< VPos+VStep/2) and (Close > VPos-VStep/2) of
			true ->
				1;
			false ->
				case (VPos+VStep/2 > Low) and (VPos-VStep/2 =< High) of
					true ->
						0;
					false ->
						-1
				end
		end,
		%io:format("Val:~p VPos:~p VStep:~p O:~p~n",[O,VPos,VStep,O]),
		l2fx(Index-1,{VList,MemList},VPos,VStep,[O|Acc]);
	l2fx(0,{[],_MemList},_VPos,_VStep,Acc)->
		%io:format("~p~n",[Acc]),
		Acc;
	l2fx(Index,{[],MemList},VPos,VStep,Acc)->
		%io:format("Acc:~p~n",[Acc]),
		l2fx(Index,{MemList,MemList},VPos+VStep,VStep,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FX ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Init %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init()->
	io:format("Initializing FX currency tables:~p~n",[?ALL_TABLES]),
	TableNames = [init_table(TableName) || TableName <- ?ALL_TABLES],
	[ets:tab2file(TableName,?FX_TABLES_DIR++atom_to_list(TableName)) || TableName <- ?ALL_TABLES],
	[delete_table(TableName) || TableName <- TableNames],
	io:format("FX metadata & currency tables initialized and written to file.~n").

init_table(metadata)->
	ets:new(metadata,[set,public,named_table,{keypos,2}]);
init_table(TableName)->
	Table = ets:new(TableName,[ordered_set,public,named_table,{keypos,2}]),
	[insert(metadata,#metadata{feature = {TableName,Feature}}) || Feature <- ?FEATURES],
	Table.

erase_all()->
	TableNames = ?ALL_TABLES,
	[file:delete(?FX_TABLES_DIR++atom_to_list(TableName)) || TableName <- TableNames].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DB Commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start()->
	register(fx,spawn(fx,loop,[])).
loop()->
	TableNames = ?ALL_TABLES,
	TableTuples = summon_tables(TableNames,[]),
	io:format("******** FX Tables:~p started~n",[TableTuples]),
	HeartBeat_PId = spawn(fx,heartbeat,[self(),TableNames,5000]),
	loop(TableNames,HeartBeat_PId).

	summon_tables([TableName|TableNames],TableTupleAcc)->
		case ets:file2tab(?FX_TABLES_DIR++atom_to_list(TableName)) of
			{ok,TableId} ->
				fx:summon_tables(TableNames,[{TableName,TableId}|TableTupleAcc]);
			{error,Reason}->
				io:format("Reason:~p~n",[Reason]),
				exit(Reason)
		end;
	summon_tables([],TableTupleAcc)->
		TableTupleAcc.

loop(TableNames,HeartBeat_PId)->
	receive
		{new_time,NewTime}->
			HeartBeat_PId ! {self(),new_time,NewTime},
			fx:loop(TableNames,HeartBeat_PId);
		{HeartBeat_PId,tables_updated,NewestKey} ->
			void,
			fx:loop(TableNames,HeartBeat_PId);
		backup ->
			backup(TableNames,[]),
			fx:loop(TableNames,HeartBeat_PId);
		stop ->
			backup(TableNames,[]),
			terminate(TableNames),
			HeartBeat_PId ! {self(),terminate},
			ok;
		terminate ->
			terminate(TableNames),
			HeartBeat_PId ! {self(),terminate},
			ok
		after 10000 ->
			fx:loop(TableNames,HeartBeat_PId)
	end.
	
	terminate(TableNames)->
		[delete_table(TableName) || TableName<-TableNames],
		io:format("******** Database:~p terminated~n",[TableNames]).
	
		backup([TableName|TableNames],ErrAcc)->	
			try first(TableName) of
				_->
					ets:tab2file(TableName,?FX_TABLES_DIR++atom_to_list(TableName)),
					backup(TableNames,ErrAcc)
			catch 
				_:Why ->
					io:format("******** FOREX_DB backup of table:~p faled due to:~p~n",[TableName,Why]),
					backup(TableNames,[TableName|ErrAcc])
			end;
		backup([],ErrAcc)->
			case ErrAcc of
				[] ->
					io:format("******** All tables within FOREX_DB have been backed up~n");
				_ ->
					io:format("******** The following tables within FOREX_DB could not be backed up:~n~p~n",[ErrAcc])
			end.
stop()->
	fx ! stop.
terminate() ->
	fx ! terminate.
backup_tables()->
	fx ! backup.
	
heartbeat(FXTables_PId,TableNames,Time)->
	receive
		{FXTables_PId,new_time,NewTime}->
			io:format("Heartbeat timer changed from:~p to:~p~n",[Time,NewTime]),
			fx:heartbeat(FXTables_PId,TableNames,NewTime);
		{FXTables_PID,terminate} ->
			io:format("******** Heartbeat terminated~n")
	after Time ->
		updater(TableNames),
		fx:heartbeat(FXTables_PId,TableNames,Time)
	end.

	updater([TN|TableNames])->
		%io:format("Updating.~n"),
		insert_ForexRaw(?SOURCE_DIR++atom_to_list(TN)++".txt",update),
		updater(TableNames);
	updater([])->
		ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Table Commands %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lookup(TableName,Key)->
	[R] = ets:lookup(TableName,Key),
	R.
	
insert(TableName,Record)->
	ets:insert(TableName,Record).
		
first(TableName)->
	ets:first(TableName).
	
last(TableName)->
	ets:last(TableName).

delete_table(TableName)->
	ets:delete(TableName).

next(TableName,Key)->
	ets:next(TableName,Key).
	
prev(TableName,Key)->
	ets:prev(TableName,Key).

prev(TableName,'end_of_table',prev,_Index)->
	ets:first(TableName);
prev(_TableName,Key,prev,0)->
	Key;
prev(TableName,Key,prev,Index)->
	prev(TableName,ets:prev(TableName,Key),prev,Index-1).
	
member(TableName,Key)->
	Result = ets:member(TableName,Key),
	Result.

%%=============================== Forex Data Insertion =====================================
%Description: -record(forex_raw,{key,open,high,low,close,volume}). %%%key={currency_pair,Year,Month,Day,Hour,Minute,Second}
%Input: textfile/cvsfile
%2009.05.15,00:00,0.88880,0.89060,0.88880,0.88950,362 :: date/time/open/high/low/close/volume
%URL= ???/????/????/File  File= FileName.FileExtension, FileName= [CPair][TimeFrame]
insert_ForexRaw(URL,Flag)->
	{Dir,File} = extract_dir(URL),
	{FileName,_FileExtension} = extract_filename(File),
	{CurrencyPair,TimeFrame} = extract_cpair(FileName), 
	TableName = CurrencyPair++TimeFrame,
%	io:format("Inserting into table:~p~n",[TableName]),
	case lists:member(TableName,[atom_to_list(TN) || TN<-?ALL_TABLES]) of
		true ->
			case file:read_file(URL) of
					{ok,Data} ->
						file:close(URL),
						List = binary_to_list(Data),
						case Flag of
							init ->
								exit("Can not yet recognize Flag: init~n");
							update ->
								case update_ForexDB(list_to_atom(TableName),CurrencyPair,list_to_integer(TimeFrame),List) of
									undefined ->
										done;
									NewestKey ->
										io:format("New FOREX_DB update starting with:~p~n",[NewestKey]),
										%calculate_ForexTechnicals(TableName,CurrencyPair,NewestKey),
										%calculate_ForexMetaData(TableName,CurrencyPair,?FOREX_TECHNICAL--[key]),
										fx ! {self(),fx_updated,NewestKey},
										done
								end
						end;
					{error,Error} ->
						%io:format("******** Error reading file:~p in insert_ForexRaw(URL,Flag):~p~n",[URL,Error]),
						cant_read
			end;
		false ->
			io:format("******** TableName:~p is unknown, file rejected.~n",[TableName])
	end.
	
	extract_dir(URL)-> extract_dir(URL,[]).
	extract_dir(List,DirAcc)->
		case split_with(47,List) of % 47 == '/'
			{File,[]}->
				Dir = lists:concat(DirAcc),
				{Dir,File};
			{DirPart,Remainder}->
				extract_dir(Remainder,lists:merge([DirPart,'/'],DirAcc))
		end.
	
	extract_filename(File)->
		split_with(46,File,[]).		% . 46
			
	extract_cpair(FileName)->
		lists:split(6,FileName).

update_ForexDB(_TableName,_CurrencyPair,_SamplingRate,[])->
	Key = get(new_id),
	erase(new_id),
	Key;

update_ForexDB(TableName,CurrencyPair,SamplingRate,List)->
%	io:format("TableName:~p CurrencyPair:~p SamplingRate:~p~n",[TableName,CurrencyPair,SamplingRate]),
	{YearL,Remainder1} = split_with(46,List),		% . 46
	{MonthL,Remainder2} = split_with(46,Remainder1),	% . 46
	{DayL,Remainder3} = split_with(44,Remainder2),		% , 44
	{HourL,Remainder4} = split_with(58,Remainder3),		% : 58
	{MinuteL,Remainder5} = split_with(44,Remainder4),	% , 44 (changed from :58 to ,44 for HH:MM format)
	{OpenL,Remainder6} = split_with(44,Remainder5),		% , 44
	{HighL,Remainder7} = split_with(44,Remainder6),		% , 44
	{LowL,Remainder8} = split_with(44,Remainder7),		% , 44
	{CloseL,Remainder9} = split_with(44,Remainder8),	% , 44
	{VolumeL,Remainder10} = split_with(13,Remainder9),	%\r 13
	[_|Remainder] = Remainder10,				%gets rid of (\n 10)
%	io:format("CloseL:~p~n",[CloseL]),
%	io:format("here~p~n",[{YearL,MonthL,DayL,HourL,MinuteL,OpenL,HighL,LowL,CloseL,VolumeL}]),
	Year = list_to_integer(YearL),
	Month = list_to_integer(MonthL),
	Day = list_to_integer(DayL),
	Hour = list_to_integer(HourL),
	Minute = list_to_integer(MinuteL),
	%Second = list_to_integer(SecondL),
	Second = 0,
	Open = list_to_number(OpenL),
	High = list_to_number(HighL),
	Low = list_to_number(LowL),
	Close = list_to_number(CloseL),
	Volume = list_to_integer(VolumeL),
	Id = {Year,Month,Day,Hour,Minute,0,SamplingRate},
	case (Second == 0) and ((Open+High+Low+Close) < 1000) and ((Open+High+Low+Close) > -1000) of
		true ->
			case member(TableName,Id) of
				false ->%{key,%%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate},open,high,low,close,volume}).
					Record = #technical{id=Id,open=Open,high=High,low=Low,close=Close,volume=Volume},
					insert(TableName,Record),
					%io:format("New record inserted into table:~p~n",[TableName]),
					case get(new_id) of
						undefined ->
							put(new_id,Id);
						_ ->
							done
					end;
				true ->
					%io:format("******** ERROR during FX data insertion.~n"),
					done
			end;
		false ->
			done
	end,			
	update_ForexDB(TableName,CurrencyPair,SamplingRate,Remainder).
	
		split_with(Seperator,List)->
			split_with(Seperator,List,[]).
		
			split_with(Seperator,[Char|List],ValAcc)->
				case Char of
					Seperator->
						{lists:reverse(ValAcc),List};
					_ ->
						split_with(Seperator,List,[Char|ValAcc])
				end;
			split_with(_Seperator,[],ValAcc)->
				{lists:reverse(ValAcc),[]}.	
			
list_to_number(List)->
	try list_to_float(List) of
		Float ->
			Float
	catch 
		_:_ ->
			list_to_integer(List)
	end.
	
%%===============================Table Size=====================================
%Gets: The total number of elements in the table.	
%Input: TableName::ets_table_name
%Output: TableSize::int
table_size(TableName)->
	case ets:info(TableName, size) of
		undefined ->
			0;
		Size ->
			Size
	end.

%%=============================== Data File Loading =====================================
%Description: Load arbitrary CSV files into temporary ETS tables for agent testing
%Input: CSV file path with format: Timestamp,Open,High,Low,Close,Volume
%Output: {ok, TableName} | {error, Reason}

% Main function to load a data file and create a temporary ETS table
load_data_file(FilePath) ->
    case validate_data_format(FilePath) of
        ok ->
            case file:read_file(FilePath) of
                {ok, Data} ->
                    TableName = generate_temp_table_name(),
                    case create_temporary_table(TableName) of
                        {ok, TableName} ->
                            case parse_and_load_csv_data(TableName, binary_to_list(Data)) of
                                ok ->
                                    {ok, TableName};
                                {error, Reason} ->
                                    delete_temporary_table(TableName),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Validate that the data file exists and has the correct format
validate_data_format(FilePath) ->
    case filelib:is_file(FilePath) of
        false ->
            {error, {file_not_found, FilePath}};
        true ->
            case file:read_file(FilePath) of
                {ok, Data} ->
                    Lines = string:tokens(binary_to_list(Data), "\n\r"),
                    case Lines of
                        [] ->
                            {error, {invalid_format, "Empty file"}};
                        [FirstLine|_] ->
                            validate_csv_line_format(FirstLine)
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end
    end.

% Validate a single CSV line format
validate_csv_line_format(Line) ->
    Fields = string:tokens(Line, ","),
    case length(Fields) of
        6 ->
            try
                [TimestampStr, OpenStr, HighStr, LowStr, CloseStr, VolumeStr] = Fields,
                % Try to parse timestamp and numeric fields
                case parse_timestamp(string:strip(TimestampStr)) of
                    {ok, _} ->
                        % Try to parse numeric fields
                        _ = list_to_number(string:strip(OpenStr)),
                        _ = list_to_number(string:strip(HighStr)),
                        _ = list_to_number(string:strip(LowStr)),
                        _ = list_to_number(string:strip(CloseStr)),
                        _ = list_to_integer(string:strip(VolumeStr)),
                        ok;
                    {error, _} ->
                        {error, {invalid_format, "Invalid timestamp format"}}
                end
            catch
                _:_ ->
                    {error, {invalid_format, "Invalid numeric values in CSV"}}
            end;
        _ ->
            {error, {invalid_format, "Expected 6 fields: Timestamp,Open,High,Low,Close,Volume"}}
    end.

% Parse timestamp from various formats
parse_timestamp(TimestampStr) ->
    % Try different timestamp formats
    case string:tokens(TimestampStr, " ") of
        [DateStr, TimeStr] ->
            case string:tokens(DateStr, "-") of
                [YearStr, MonthStr, DayStr] ->
                    case string:tokens(TimeStr, ":") of
                        [HourStr, MinuteStr] ->
                            try
                                Year = list_to_integer(YearStr),
                                Month = list_to_integer(MonthStr),
                                Day = list_to_integer(DayStr),
                                Hour = list_to_integer(HourStr),
                                Minute = list_to_integer(MinuteStr),
                                {ok, {Year, Month, Day, Hour, Minute, 0, 1}}
                            catch
                                _:_ ->
                                    {error, invalid_timestamp}
                            end;
                        [HourStr, MinuteStr, SecondStr] ->
                            try
                                Year = list_to_integer(YearStr),
                                Month = list_to_integer(MonthStr),
                                Day = list_to_integer(DayStr),
                                Hour = list_to_integer(HourStr),
                                Minute = list_to_integer(MinuteStr),
                                Second = list_to_integer(SecondStr),
                                {ok, {Year, Month, Day, Hour, Minute, Second, 1}}
                            catch
                                _:_ ->
                                    {error, invalid_timestamp}
                            end;
                        _ ->
                            {error, invalid_timestamp}
                    end;
                _ ->
                    {error, invalid_timestamp}
            end;
        _ ->
            {error, invalid_timestamp}
    end.

% Create a temporary ETS table for data loading
create_temporary_table(TableName) ->
    try
        Table = ets:new(TableName, [ordered_set, public, named_table, {keypos, 2}]),
        {ok, TableName}
    catch
        _:Reason ->
            {error, {table_creation_failed, Reason}}
    end.

% Delete a temporary ETS table
delete_temporary_table(TableName) ->
    try
        ets:delete(TableName),
        ok
    catch
        _:_ ->
            ok  % Table might not exist, that's fine
    end.

% Generate a unique temporary table name
generate_temp_table_name() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    list_to_atom("temp_fx_" ++ integer_to_list(MegaSecs) ++ "_" ++ 
                 integer_to_list(Secs) ++ "_" ++ integer_to_list(MicroSecs)).

% Parse CSV data and load into ETS table
parse_and_load_csv_data(TableName, Data) ->
    Lines = string:tokens(Data, "\n\r"),
    case Lines of
        [] ->
            {error, {invalid_format, "No data lines found"}};
        _ ->
            try
                load_csv_lines(TableName, Lines, 1),
                ok
            catch
                throw:{parse_error, LineNum, Reason} ->
                    {error, {parse_error, LineNum, Reason}};
                _:Reason ->
                    {error, {unexpected_error, Reason}}
            end
    end.

% Load individual CSV lines into the table
load_csv_lines(TableName, [Line|Lines], LineNum) ->
    case string:strip(Line) of
        "" ->
            % Skip empty lines
            load_csv_lines(TableName, Lines, LineNum + 1);
        _ ->
            case parse_csv_line(Line, LineNum) of
                {ok, Record} ->
                    insert(TableName, Record),
                    load_csv_lines(TableName, Lines, LineNum + 1);
                {error, Reason} ->
                    throw({parse_error, LineNum, Reason})
            end
    end;
load_csv_lines(_TableName, [], _LineNum) ->
    ok.

% Parse a single CSV line into a technical record
parse_csv_line(Line, LineNum) ->
    try
        Fields = string:tokens(Line, ","),
        case length(Fields) of
            6 ->
                [TimestampStr, OpenStr, HighStr, LowStr, CloseStr, VolumeStr] = Fields,
                case parse_timestamp(string:strip(TimestampStr)) of
                    {ok, Id} ->
                        Open = list_to_number(string:strip(OpenStr)),
                        High = list_to_number(string:strip(HighStr)),
                        Low = list_to_number(string:strip(LowStr)),
                        Close = list_to_number(string:strip(CloseStr)),
                        Volume = list_to_integer(string:strip(VolumeStr)),
                        
                        % Validate price data
                        case (Open + High + Low + Close) < 1000 andalso (Open + High + Low + Close) > -1000 of
                            true ->
                                Record = #technical{id=Id, open=Open, high=High, low=Low, close=Close, volume=Volume},
                                {ok, Record};
                            false ->
                                {error, "Price values out of reasonable range"}
                        end;
                    {error, _} ->
                        {error, "Invalid timestamp format"}
                end;
            _ ->
                {error, "Expected 6 fields: Timestamp,Open,High,Low,Close,Volume"}
        end
    catch
        _:Reason ->
            {error, lists:flatten(io_lib:format("Parse error: ~p", [Reason]))}
    end.	
