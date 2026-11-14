
-module(fx).
-compile(export_all).
-define(ALL_TABLES,[metadata, 'EURUSD1','EURUSD1_LIVE']).
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



% Debug tags now use direct config function calls (no macros needed)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FX SIMULATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{table_name,feature,index_start,index_end,index,price_list=[],cycle=0,realized_pl_by_cycle=[]}).
-record(account,{leverage,lot,spread,margin,balance,net_asset_value,realized_PL=0,unrealized_PL=0,order}).
-record(order,{pair,position,entry,current,units,change,percentage_change,profit}).

%% Helper function to create account with config values
create_account() ->
    Balance = config:account_initial_balance(),
    #account{
        leverage = config:account_leverage(),
        lot = config:account_lot_size(),
        spread = config:account_spread(),
        margin = config:account_margin(),
        balance = Balance,
        net_asset_value = Balance
    }.

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
	rand:seed(exsss),
	market_properties(config:primary_currency_pair(),close,[config:internal_sensor_dimensions(),list_sensor],config:gt_start(),config:gt_end()).
market_properties(CurrencyPair,Feature,Parameters,StartIndex,EndIndex)->
	max(CurrencyPair,Feature,Parameters,StartIndex,EndIndex),
	erase().

	random(CurrencyPair,Feature,_Parameters,Start,Finish)->
		S = #state{},
		A = create_account(),
		InitS = init_state(S,CurrencyPair,Feature,Start,Finish),
		_AvgRandomProfit=lists:sum([random_profit(CurrencyPair,InitS,A) || _<-lists:seq(1,1000)])/1000,
		erase().
	random_profit(CurrencyPair,S,A)->
		Trade_Signal = rand:uniform(3)-2,
		case profit_trade(S,A,Trade_Signal) of
			{U_S,U_A}->
				random_profit(CurrencyPair,U_S,U_A);
			Profit ->
				Profit
		end.
		
	max(CurrencyPair,Feature,_Parameters,Start,Finish)->
		S = #state{},
		A = create_account(),
		InitS = init_state(S,CurrencyPair,Feature,Start,Finish),
		max_profit(CurrencyPair,InitS,A).
	max_profit(CurrencyPair,S,A)->
		Index=S#state.index,
		EndIndex=S#state.index_end,
		T = fx:lookup(CurrencyPair,Index),
		Close = T#technical.close,
		{FlipIndex,FlipClose} = find_next_flip(CurrencyPair,Index,Close,EndIndex),
		Threshold = config:min_profit_threshold(),
		TradeSignal=if 
			FlipClose > (Close+ Threshold*Close) -> 1;
			FlipClose < (Close- Threshold*Close) -> -1;
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
		
			find_next_flip(_CurrencyPair,EndIndex,Close,EndIndex,long)->
				{EndIndex,Close};
			find_next_flip(CurrencyPair,Index,Close,EndIndex,long)->
				%move the index forward until a change in sign occurs. Then return index of the last closing price of the same sign.
				NextIndex = fx:next(CurrencyPair,Index),
				NextT = fx:lookup(CurrencyPair,NextIndex),
				NextClose = NextT#technical.close,
				case (NextClose - Close) > 0 of
					true ->
						find_next_flip(CurrencyPair,NextIndex,NextClose,EndIndex,long);
					false ->
						{Index,Close}
				end;
			find_next_flip(_CurrencyPair,EndIndex,Close,EndIndex,short)->
				{EndIndex,Close};
			find_next_flip(CurrencyPair,Index,Close,EndIndex,short)->
				%move the index forward until a change in sign occurs. Then return index of the last closing price of the same sign.
				NextIndex = fx:next(CurrencyPair,Index),
				NextT = fx:lookup(CurrencyPair,NextIndex),
				NextClose = NextT#technical.close,
				case (NextClose - Close) < 0 of
					true ->
						find_next_flip(CurrencyPair,NextIndex,NextClose,EndIndex,short);
					false ->
						%Is it a true flip, or a 0 flip (no profit) and then another slide towards the same direction?
						%To check this, check if the next slide is 0 trade signal one, and then if afterwards, it goes in the same direction again. And if it does go in the same direciton again,
						%check if at the end of that slide, there is a profit... if at the end of that slide there is no profit again from the orignal, then check if therei safter that...
						{Index,Close}
				end.
		
		forward(_CurrencyPair,S,A,TradeSignal,FlipIndex,FlipIndex)->
			profit_trade(S,A,TradeSignal);
		forward(CurrencyPair,S,A,TradeSignal,_Index,FlipIndex)->
			{U_S,U_A}=profit_trade(S,A,TradeSignal),
			NextIndex = U_S#state.index,
			forward(CurrencyPair,U_S,U_A,TradeSignal,NextIndex,FlipIndex).
			
		max_profit_sense(S,_A,Parameters)->
			{_Result,U_S} = sense(S,Parameters),
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
			ExtinctionThreshold = config:account_extinction_threshold(),
			case (U_A#account.balance + U_A#account.unrealized_PL) =< ExtinctionThreshold of
				true ->
					io:format("Lost all money (threshold: ~p)~n", [ExtinctionThreshold]),
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
	HRes=5,VRes=5,
	PId ! {self(),sense,'EURUSD1',close,[HRes,VRes,graph_sensor]},
	receive 
		{_From,Result}->
			io:format("Result:~p~n",[Result])
	end.
tt(PId,TradeSignal)->
	PId ! {self(),trade,'EURUSD1',TradeSignal},
	receive
		{_From, Profit, IsOver} ->
			io:format("Trade_Signal:~p~n Profit:~p IsOver:~p~n", [TradeSignal, Profit, IsOver])
	end.

%Entry Point for Starting the Forex Simulation
% Recieves a message from the ExoSelf process to start the simulation.
% Message is of the form spawn(fx,sim,[ExoSelf_PID]).
% Defines the initial state and account, and starts the simulation loop for this PID
% Calls the recursive sim/3 function to handle the simulation logic.
sim(ExoSelf)->io:format("Started~n"),
	put(prev_PC,0),
	S = #state{},
	A = create_account(),
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
%With config:sense_before_trade/0 on, the exoself tells the simulator how many sensors read it ({_,sensors,N}), and a
%bar's trade is not taken off the mailbox until N senses have been answered for that bar (sim_senses/3). Without it a
%sensor whose neurons reach the actuator only through recurrent links can be served after the trade, that is, from the
%next bar, depending on how the processes happen to be scheduled.
sim(ExoSelf,S,A)->
	case get(fx_sensors) of
		N when is_integer(N) ->
			case senses_served() < N of
				true -> sim_senses(ExoSelf,S,A);
				false -> sim_main(ExoSelf,S,A)
			end;
		_ ->
			sim_main(ExoSelf,S,A)
	end.

	senses_served()->
		case get(fx_senses) of
			undefined -> 0;
			K -> K
		end.

	count_sense()->
		put(fx_senses,senses_served()+1).

%sim_senses/3: some sensors have not read this bar yet. Everything but a trade is handled; a trade waits in the mailbox.
sim_senses(ExoSelf,S,A)->
	receive
		{_From,eval_mode,Mode}->
			on_eval_mode(ExoSelf,S,A,Mode);
		{_From,sensors,N}->
			on_sensors(ExoSelf,S,A,N);
		{From,sense,TableName,Feature,Parameters,Start,Finish}->
			on_sense(ExoSelf,From,TableName,Feature,Parameters,Start,Finish,S,A);
		{From,sense,internals,Parameters}->
			on_internals(ExoSelf,From,Parameters,S,A);
		restart ->
			fx:sim(ExoSelf,#state{},create_account());
		{_From,terminate} ->
			ok;
		terminate ->
			ok
		after 10000 ->
			fx:sim(ExoSelf,S,A)
	end.

	%eval_mode is sent by the exoself before an evaluation: tune | score | validation select the episode set used when
	%config:evaluation_mode() is episodes. Any other value (or no message at all) keeps the legacy single window.
	%With config:episode_isolation/0 on it also puts the simulator back to idle. Normally it is idle already; it is not
	%when a sensor read it after the previous evaluation ended, which started an evaluation with the old episode set.
	on_eval_mode(ExoSelf,S,A,Mode)->
		put(eval_mode,Mode),
		put(fx_senses,0),
		case config:episode_isolation() of
			true ->
				erase(ep_ctx),
				put(prev_PC,0),
				fx:sim(ExoSelf,#state{},create_account());
			_ ->
				fx:sim(ExoSelf,S,A)
		end.

	on_sensors(ExoSelf,S,A,N)->
		put(fx_sensors,N),
		put(fx_senses,0),
		fx:sim(ExoSelf,S,A).

	on_sense(ExoSelf,From,TableName,Feature,Parameters,Start,Finish,S,A)->
		U_S = sense_reply(From,TableName,Feature,Parameters,Start,Finish,S,A),
		count_sense(),
		fx:sim(ExoSelf,U_S,A).

	on_internals(ExoSelf,From,Parameters,S,A)->
		From ! {self(),internals(A,Parameters)},
		count_sense(),
		%This sense does not carry the table that starts an evaluation (the price senses do), so an agent whose only
		%sensor is this one never started: every trade was answered "nothing happened" for ever, the cortex kept cycling,
		%and the evaluation never ended (overnight run of 2026-11-11, an agent that had lost its price sensor to
		%remove_sensor). In episode mode everything needed to start is in the config, so start here too.
		fx:sim(ExoSelf,start_if_idle(S),A).

	%sense_reply/8 answers a price sense and returns the simulator state, which a sense starts if it is idle.
	% Parameters is a list that can contain the resolution and type of sensor (e.g., graph_sensor, list_sensor).
	% Start and Finish are the indices to start and finish sensing (legacy mode only).
	sense_reply(From,TableName,Feature,Parameters,Start,Finish,S,A)->
		{Result,U_S}=case S#state.table_name of
			undefined ->
				sense(eval_init_state(S,TableName,Feature,Start,Finish),Parameters);
			CurrentTableName when CurrentTableName =:= TableName ->
				sense(S,Parameters);
			_OtherTableName ->
				%% Table name mismatch, reinitialize state
				sense(eval_init_state(S,TableName,Feature,Start,Finish),Parameters)
		end,
		From ! {self(),Result},
		case config:sensor_debug_tag() of
			true ->
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
		U_S.

	%internals/2: what the account-state sensors read.
	%	position   [Position]: -1, 0 or 1, the position held now (fx_Position)
	%	otherwise  [Position, OpenPnL, PrevOpenPnL] (fx_Internals). P&L terms are signed by position (positive = winning)
	%	           and expressed in units of 0.1% of price (~11.7 pips at 1.17) so they sit in a range neurons can use.
	internals(A,position)->
		[position(A)];
	internals(A,_Parameters)->
		case A#account.order of
			undefined ->
				[0,0,0];
			O ->
				Position = O#order.position,
				CurPC = O#order.percentage_change,
				PrevPC = case get(prev_PC) of undefined -> CurPC; V -> V end,
				[Position, Position*CurPC*10, Position*PrevPC*10]
		end.

sim_main(ExoSelf,S,A)->
	receive
		{_From,eval_mode,Mode}->
			on_eval_mode(ExoSelf,S,A,Mode);
		{_From,sensors,N}->
			on_sensors(ExoSelf,S,A,N);
		{From,sense,TableName,Feature,Parameters,Start,Finish}->%Parameters:{VL,SignalEncoding}
			on_sense(ExoSelf,From,TableName,Feature,Parameters,Start,Finish,S,A);
		{From,sense,internals,Parameters}->
			on_internals(ExoSelf,From,Parameters,S,A);
		{From,trade,TableName,TradeSignal}->
			%A trade ends the bar: the next bar's senses are counted from zero.
			put(fx_senses,0),
			case {S#state.table_name,get(ep_ctx)} of
				{undefined,_} ->
					% State not initialized - ignore early trade (likely from recurrent network)
					From ! {self(),0,0},
					fx:sim(ExoSelf,S,A);
				{_,Ctx} when is_map(Ctx) ->
					% Episode mode: several episodes per evaluation, fitness aggregated at the end
					episode_trade(ExoSelf,From,S,A,Ctx,TradeSignal);
				_ ->
					% Process trade normally
					{U_S, U_A} = make_trade(S,A,TradeSignal),
					Total_Profit = A#account.balance + A#account.unrealized_PL,
					case config:actuator_debug_tag() of
						true ->
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
					ExtinctionThreshold = config:account_extinction_threshold(),
					case (U_A#account.balance + U_A#account.unrealized_PL) =< ExtinctionThreshold of
						true ->
							From ! {self(),0,1},
							io:format("Lost all money (threshold: ~p)~n", [ExtinctionThreshold]),
							put(prev_PC,0),
							fx:sim(ExoSelf,#state{},create_account());
						false ->
							case update_state(U_S) of
								sim_over ->
									Fitness = fitness_functions:calculate_fitness(U_S, U_A),
									RawTotalProfit = U_A#account.balance + U_A#account.unrealized_PL,
									TotalRealizedPL = lists:sum([PL || {_,PL} <- U_S#state.realized_pl_by_cycle]),
									TradesStr = case U_S#state.realized_pl_by_cycle of [] -> "none"; Trades -> string:join([io_lib:format("cycle:~p,pl:~.2f",[Cycle,PL]) || {Cycle,PL} <- Trades], " | ") end,
									FitnessFunctionName = config:fitness_function(),
									qlog:agent_trades(ExoSelf,io_lib:format("FITNESS_EVAL | fitness_function=~p | fitness=~p | raw_total_profit=~p | balance=~p | realized_pl=~p | unrealized_pl=~p | realized_trades=~p | trades=[~s]",[FitnessFunctionName,Fitness,RawTotalProfit,U_A#account.balance,TotalRealizedPL,U_A#account.unrealized_PL,length(U_S#state.realized_pl_by_cycle),TradesStr])),
									From ! {self(),Fitness,1},
									put(prev_PC,0),
									fx:sim(ExoSelf,#state{},create_account());
								U_S2 ->
									From ! {self(),0,0},
									U_A2 = update_account(U_S2,U_A),
									fx:sim(ExoSelf,U_S2,U_A2)
							end
					end
			end;
		restart ->
			fx:sim(ExoSelf,#state{},create_account());
		{From,terminate} ->
			ok;
		terminate ->
			ok
		after 10000 ->
			fx:sim(ExoSelf,S,A)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EPISODE MODE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%In episode mode (config:evaluation_mode() == episodes) one evaluation runs the agent through a list of episodes
%{StartKey,Bars} published by episodes:begin_generation/2. The account is reset at the start of every episode. While an
%evaluation is in progress its context lives in the process dictionary under ep_ctx:
%	#{mode => tune|score|validation, queue => episodes still to run, done => stats of finished episodes, acc => accumulator}
%The halt flag is only raised after the last episode, together with the fitness aggregated over all of them by
%fitness_functions:aggregate_episodes/1.
%
%What is measured per episode (see ep_stat/4): with u_t the position held from bar t to t+1 and r_t = close[t+1]-close[t],
%	gross  = sum(u_t*r_t)		P&L per unit of position, before cost
%	timing = gross - mu*sum(u_t) - spread*entries
%	alpha  = timing / sqrt(sum(r_t^2))
%	z      = timing / sqrt(sum(u_t^2 * r_t^2))
%timing is the money the agent made per unit of position, after cost, on a market whose average drift has been taken
%out. mu is ONE number for all train episodes (the average change of the close per bar over the whole train span) and
%one for all validation episodes (the same average over the validation set), see episodes:drift/1. It is fixed before
%any agent runs, so being long in a period that happened to rise earns nothing, and the score cannot be moved by
%anything the agent does.
%alpha expresses timing in units of the market's movement over the episode, so it grows with how much the agent trades:
%a position that knows nothing about the next bar gives about 0 +/- sqrt(active), where active (also reported) is the
%share of the market's variance the agent was exposed to.
%z expresses it in units of its own standard deviation under "the position knows nothing about the next bar": about
%0 +/- 1 whether the agent is in the market all the time or for a few bars, so z does not favour either.
%
%Until November 2026 timing was gross - mean(u)*sum(r_t), with mean(u) taken over the same episode. That looks like the
%same correction but is not: a rule that sells after a rise is short, on average, in exactly the episodes that rose, so
%-mean(u)*sum(r_t) is positive for it whether or not it makes money. On a random walk that formula gave a 240-bar
%reversal rule about +0.3 z per episode and the matching momentum rule -0.3 (measured on the train span: 11.7 pips of
%credit per episode against 1.1 pips of money). eval_protocol:random_walk_check/0 guards against this.
%
%The cost per round trip also depends on the span: config:account_spread() on train episodes and
%config:validation_spread() on validation episodes (the same unless validation_spread is set).

%start_if_idle/1 starts an episode evaluation on a simulator that has not been started yet (see the internals clause of
%sim/3). It does nothing in legacy mode, where the window comes with the price sense.
start_if_idle(S) when S#state.table_name =/= undefined->
	S;
start_if_idle(S)->
	case episode_mode() of
		false ->
			S;
		Mode ->
			case episodes:current_set(Mode) of
				[] -> S;
				_ -> eval_init_state(S,config:primary_currency_pair(),close,undefined,undefined)
			end
	end.

%eval_init_state/5 is called on the first sense of an evaluation. It decides between episode mode and the legacy window.
eval_init_state(S,TableName,Feature,Start,Finish)->
	Episodes = case episode_mode() of
		false -> [];
		Mode -> episodes:current_set(Mode)
	end,
	case Episodes of
		[] ->
			erase(ep_ctx),
			init_state(S,TableName,Feature,Start,Finish);
		[Episode|Queue] ->
			put(ep_ctx,#{mode=>get(eval_mode),queue=>Queue,done=>[],acc=>ep_acc_new(get(eval_mode),span_of(get(eval_mode)))}),
			init_episode_state(S,TableName,Feature,Episode)
	end.

	episode_mode()->
		case {config:evaluation_mode(),get(eval_mode)} of
			{episodes,Mode} when Mode == tune; Mode == score; Mode == validation -> Mode;
			_ -> false
		end.

init_episode_state(S,TableName,Feature,{StartKey,Bars})->
	S#state{
		table_name = TableName,
		feature = Feature,
		index_start = StartKey,
		index_end = walk(TableName,StartKey,Bars),
		index = StartKey,
		price_list = [],
		cycle = 0,
		realized_pl_by_cycle = []
	}.

	walk(_TableName,Key,0)->
		Key;
	walk(TableName,Key,N)->
		case ets:next(TableName,Key) of
			'$end_of_table' -> Key;
			NextKey -> walk(TableName,NextKey,N-1)
		end.

%The accumulator. The exposure series is only kept for validation runs, where eval_protocol uses it for the shift test.
%Span (train | validation) says which span the episode belongs to; it decides the drift taken out of the score and the
%cost charged per round trip. Agents run tune and score evaluations on the train span and validation ones on the
%validation span (span_of/1); scripted policies name the span themselves (run_policy/4).
%u_series, the position at every bar, is kept only for a validation evaluation with config:validation_series/0 on:
%eval_protocol:evaluate_agent/2 and replay tools switch it on. A population's own validation does not keep it. Kept for
%48 agents on 369 episodes it took 0.6 GB at the peak of the generation, unread (REVIEW.md 8.18).
ep_acc_new(Mode,Span)->
	#{
		step=>0,n=>0,entries=>0,sum_u=>0,sum_abs_u=>0,sum_ur=>0.0,sum_r=>0.0,sum_r2=>0.0,sum_u2r2=>0.0,
		warmup=>config:episode_warmup_bars(),
		floor=>config:account_extinction_threshold(),
		%Fill rules (ep_step/4), read once per episode.
		rollover=>config:rollover_rule() == true,
		late=>config:late_fill() == true,
		pending=>0,
		span=>Span,
		mu=>episodes:drift(Span),
		spread=>span_spread(Span),
		u_series=>case (Mode == validation) andalso (config:validation_series() == true) of true -> []; false -> off end
	}.

	span_of(validation)-> validation;
	span_of(_)-> train.

	span_spread(validation)->
		case config:validation_spread() of
			undefined -> config:account_spread();
			Spread -> Spread
		end;
	span_spread(_)->
		config:account_spread().

%ep_step/4 applies one trade signal and advances one bar. It is a pure function of its arguments (apart from prev_PC in
%the process dictionary, as in the legacy path) so the agent simulator and the scripted policies of eval_protocol share it.
%Returns {continue,U_S,U_A,U_Acc} | {episode_over,EpStat} | {extinct,EpStat}.
%Two fill rules apply here, so that the agents and the scripted policies (run_policy/4) are filled the same way:
%	late_fill      (config:late_fill/0) the signal given on a bar is filled at the next bar's close, one minute later;
%	               the signal filled on this bar is the one given on the previous bar (pending in Acc). During the
%	               warm-up the signal is 0, so the first scored bar is always flat.
%	rollover_rule  (config:rollover_rule/0) on a bar whose hour on the file's clock is 21, 22 or 23 no position is
%	               opened, closed or reversed: the position held is kept whatever the signal.
ep_step(S,A0,Acc,TradeSignal)->
	#{step:=Step,warmup:=Warmup,floor:=Floor,spread:=Spread} = Acc,
	%The account charges what the episode's span says, whatever it was created with.
	A = A0#account{spread=Spread},
	Scored = Step >= Warmup,
	Signal = case Scored of true -> TradeSignal; false -> 0 end,
	PrevPosition = position(A),
	Wanted = case maps:get(late,Acc,false) of
		true -> maps:get(pending,Acc,0);
		false -> Signal
	end,
	Filled = case maps:get(rollover,Acc,false) andalso rollover_hour(S#state.index) of
		true -> PrevPosition;
		false -> Wanted
	end,
	{U_S,U_A} = make_trade(S,A,Filled),
	Position = position(U_A),
	case (U_A#account.balance + U_A#account.unrealized_PL) =< Floor of
		true ->
			{extinct,ep_stat(U_S,U_A,Acc,extinct)};
		false ->
			case update_state(U_S) of
				sim_over ->
					{episode_over,ep_stat(U_S,U_A,Acc,complete)};
				U_S2 ->
					U_Acc = case Scored of
						false ->
							Acc#{step:=Step+1,pending=>Signal};
						true ->
							TableName = S#state.table_name,
							R = (lookup(TableName,U_S2#state.index))#technical.close - (lookup(TableName,S#state.index))#technical.close,
							Entry = case (Position =/= 0) and (Position =/= PrevPosition) of true -> 1; false -> 0 end,
							#{n:=N,entries:=Entries,sum_u:=SumU,sum_abs_u:=SumAbsU,sum_ur:=SumUR,sum_r:=SumR,sum_r2:=SumR2,sum_u2r2:=SumU2R2,u_series:=USeries} = Acc,
							Acc#{
								step:=Step+1,n:=N+1,entries:=Entries+Entry,pending=>Signal,
								sum_u:=SumU+Position,sum_abs_u:=SumAbsU+abs(Position),
								sum_ur:=SumUR+Position*R,sum_r:=SumR+R,sum_r2:=SumR2+R*R,
								sum_u2r2:=SumU2R2+Position*Position*R*R,
								u_series:=case USeries of off -> off; _ -> [Position|USeries] end
							}
					end,
					{continue,U_S2,update_account(U_S2,U_A),U_Acc}
			end
	end.

	position(A)->
		case A#account.order of
			undefined -> 0;
			O -> O#order.position
		end.

	%rollover_hour/1: the bar with this key is in hours 21 to 23 of the file's clock (local market time).
	rollover_hour(Key)->
		element(4,Key) >= 21.

%ep_stat/4 turns the accumulator of a finished episode into its statistics.
ep_stat(S,A,Acc,Status)->
	#{n:=N,entries:=Entries,sum_u:=SumU,sum_abs_u:=SumAbsU,sum_ur:=SumUR,sum_r:=SumR,sum_r2:=SumR2,sum_u2r2:=SumU2R2,u_series:=USeries,
		span:=Span,mu:=Mu,spread:=Spread} = Acc,
	Vol = math:sqrt(SumR2),
	Cost = Entries*Spread,
	MeanU = case N of 0 -> 0.0; _ -> SumU/N end,
	Timing = SumUR - Mu*SumU - Cost,
	Alpha = case (N > 0) and (Vol > 0) of
		true -> Timing/Vol;
		false -> 0.0
	end,
	%sum(u^2 * r^2): the variance of the gross P&L if the position knew nothing about the next bar
	ActiveVar = float(SumU2R2),
	Z = case ActiveVar > 1.0e-9*SumR2 of
		true -> Timing/math:sqrt(ActiveVar);
		false -> 0.0
	end,
	#{
		z=>Z,
		active=>case SumR2 > 0 of true -> ActiveVar/SumR2; false -> 0.0 end,
		span=>Span,
		mu=>Mu,
		spread=>Spread,
		status=>Status,
		start=>S#state.index_start,
		bars=>N,
		entries=>Entries,
		mean_u=>MeanU,
		mean_abs_u=>case N of 0 -> 0.0; _ -> SumAbsU/N end,
		gross=>SumUR,
		drift=>SumR,
		vol=>Vol,
		cost=>Cost,
		%money after cost with the span's drift taken out, in price units (alpha_pips_t, REVIEW.md 8.26)
		timing=>Timing,
		alpha=>Alpha,
		pnl=>float(A#account.balance + A#account.unrealized_PL - config:account_initial_balance()),
		legacy=>case fitness_functions:is_episode_fitness(config:fitness_function()) of
			true -> undefined;
			false -> fitness_functions:calculate_fitness(S,A)
		end,
		u_series=>case USeries of off -> off; _ -> lists:reverse(USeries) end
	}.

%episode_trade/6 is the trade handler of sim/3 in episode mode.
episode_trade(ExoSelf,From,S,A,Ctx,TradeSignal)->
	#{mode:=Mode,queue:=Queue,done:=Done,acc:=Acc} = Ctx,
	case ep_step(S,A,Acc,TradeSignal) of
		{continue,U_S,U_A,U_Acc} ->
			put(ep_ctx,Ctx#{acc:=U_Acc}),
			From ! {self(),0,0},
			fx:sim(ExoSelf,U_S,U_A);
		{episode_over,EpStat} when Queue =/= [] ->
			[NextEpisode|U_Queue] = Queue,
			put(ep_ctx,Ctx#{queue:=U_Queue,done:=[EpStat|Done],acc:=ep_acc_new(Mode,span_of(Mode))}),
			NextS = init_episode_state(#state{},S#state.table_name,S#state.feature,NextEpisode),
			NextA = create_account(),
			case config:episode_isolation() of
				true ->
					%The actuator waits for this answer and the cortex for the actuator, so no new bar starts until the
					%exoself has reset the network (exoself:episode_boundary/3) and says go.
					ExoSelf ! {self(),episode_boundary},
					boundary_wait(ExoSelf,From,S,A,NextS,NextA);
				_ ->
					put(prev_PC,0),
					From ! {self(),0,0},
					fx:sim(ExoSelf,NextS,NextA)
			end;
		{_LastEpisodeOverOrExtinct,EpStat} ->
			EpStats = lists:reverse([EpStat|Done]),
			Fitness = fitness_functions:aggregate_episodes(EpStats),
			publish_evaluation(ExoSelf,Mode,Fitness,EpStats),
			From ! {self(),Fitness,1},
			erase(ep_ctx),
			put(prev_PC,0),
			fx:sim(ExoSelf,#state{},create_account())
	end.

	%boundary_wait/6: an episode has ended and the next one is ready (NextS, NextA), but the bar that would start it is
	%held until the exoself has reset the network. A sense that arrives meanwhile was sent for the bar just finished (a
	%sensor off the path to the actuator, without config:sense_before_trade/0) and is answered from that bar; the
	%exoself waits for every sensor to finish before it resets the neurons.
	boundary_wait(ExoSelf,From,S,A,NextS,NextA)->
		receive
			{ExoSelf,episode_go} ->
				put(prev_PC,0),
				put(fx_senses,0),
				From ! {self(),0,0},
				fx:sim(ExoSelf,NextS,NextA);
			{SFrom,sense,TableName,Feature,Parameters,Start,Finish} ->
				_ = sense_reply(SFrom,TableName,Feature,Parameters,Start,Finish,S,A),
				boundary_wait(ExoSelf,From,S,A,NextS,NextA);
			{SFrom,sense,internals,Parameters} ->
				SFrom ! {self(),internals(A,Parameters)},
				boundary_wait(ExoSelf,From,S,A,NextS,NextA);
			{_From,terminate} ->
				ok;
			terminate ->
				ok
		end.

	%The exoself reads the statistics of its latest evaluation from the eval_stats table (created by population_monitor
	%or eval_protocol). The scape only knows the exoself's pid, so that is the key.
	publish_evaluation(ExoSelf,Mode,Fitness,EpStats)->
		%Tuning evaluations are not published: nobody reads them, and there are many per agent.
		case (Mode =/= tune) andalso (ets:whereis(eval_stats) =/= undefined) of
			true -> ets:insert(eval_stats,{{ExoSelf,Mode},Fitness,EpStats});
			false -> ok
		end,
		Sum = fitness_functions:episode_summary(EpStats),
		qlog:agent_trades(ExoSelf,io_lib:format("FITNESS_EVAL | mode=~p | fitness_function=~p | fitness=~.4f | episodes=~p | mean_z=~.4f | mean_alpha=~.4f | std_alpha=~.4f | entries=~p | mean_abs_exposure=~.3f | active=~.3f | holder_episodes=~p | pnl=~.2f",
			[Mode,config:fitness_function(),float(Fitness),maps:get(episodes,Sum),maps:get(mean_z,Sum),maps:get(mean_alpha,Sum),maps:get(std_alpha,Sum),maps:get(entries,Sum),maps:get(mean_abs_u,Sum),maps:get(mean_active,Sum),maps:get(holder_episodes,Sum),maps:get(pnl,Sum)])).

%run_policy/3 runs a scripted policy through the same episode simulator, without spawning an agent. Used by eval_protocol
%for baselines and integrity checks. Policy = fun(StepNo,Close,Position) -> -1 | 0 | 1. Returns one stat map per episode.
%run_policy/4 also says which span the episodes come from (train | validation), which decides drift and cost; Mode only
%decides whether the exposure series is kept.
run_policy(Policy,Episodes,Mode)->
	run_policy(Policy,Episodes,Mode,span_of(Mode)).
run_policy(Policy,Episodes,Mode,Span)->
	TableName = config:primary_currency_pair(),
	[begin
		put(prev_PC,0),
		policy_loop(Policy,init_episode_state(#state{},TableName,close,Episode),create_account(),ep_acc_new(Mode,Span),0)
	end || Episode <- Episodes].

	%A policy of arity 4 is also given the bar's key {Y,M,D,H,Mi,S,Rate} (eval_protocol:policy(clock)).
	policy_loop(Policy,S,A,Acc,StepNo)->
		Close = (lookup(S#state.table_name,S#state.index))#technical.close,
		Signal = case is_function(Policy,4) of
			true -> Policy(StepNo,Close,position(A),S#state.index);
			false -> Policy(StepNo,Close,position(A))
		end,
		case ep_step(S,A,Acc,Signal) of
			{continue,U_S,U_A,U_Acc} -> policy_loop(Policy,U_S,U_A,U_Acc,StepNo+1);
			{_Over,EpStat} -> EpStat
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
		index = Index_Start,
		cycle = 0,
		realized_pl_by_cycle = []
	}.	


% This function retrieves the previous index based on the current index and the specified direction.
% It takes the table name TableName, the current index CurrentIndex, the direction Direction (prev or next), and the specified index SpecifiedIndex.
update_state(S)->
	NextIndex = fx:next(S#state.table_name,S#state.index),
	case NextIndex == S#state.index_end of
		true ->
			sim_over;
		false ->
			case config:fitness_time_weighted_enabled() of
				true -> S#state{index=NextIndex,cycle=S#state.cycle+1};
				false -> S#state{index=NextIndex}
			end
	end.

% This function updates the account based on the current state and order.
% It takes the current state S and the account A as parameters.
% It checks if there is an order in the account.
% If there is an order, it retrieves the current row from the table based on the state's table name and index.
% It calculates the close price, balance, position, entry price, and units from the order.
% It then calculates the change in price, percentage change, profit, unrealized profit/loss, and net asset value.
% It updates the order with the current price
update_account(S,A)->
	case A#account.order of
		undefined ->
			nothing_to_update,
			A;
		O ->
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
			put(prev_PC,O#order.percentage_change),
			U_A
	end.

% This function retrieves the fields of a record.
% It takes a record R as a parameter and prints the field names and values.
% It uses the record_info function to get the field names based on the record type.
r()->r(#account{}).
r(R)->
	[RName|Element_Values] = tuple_to_list(R),
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
%-record(order,{pair,position,entry,units,change,percentage_change,profit}).
determine_profit(A)->
	_U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL.

%% NOTE: calculate_time_weighted_fitness/2 has been moved to fitness_functions.erl
%% Use fitness_functions:calculate_fitness/2 instead, which dispatches to the
%% function specified in config:fitness_function()

make_trade(S,A,Action)->
	case A#account.order of
		undefined ->
			case Action == 0 of
				true -> {S, A};
				false -> open_order(S,A,Action)
			end;
		O ->
			case Action == 0 of
				true -> close_order(S,A);
				false ->
					Current_Position = O#order.position,
					case Current_Position == Action of
						true -> {S, A};
						false ->
							{U_S, U_A} = close_order(S,A),
							open_order(U_S,U_A,Action)
					end
			end
	end.
	
open_order(S,A,Action)->
	_Order_Size = config:order_size_percentage(),% Open order size is 20% of the current account balance.
	BuyMoney = config:buy_money_fixed(),
	Spread=A#account.spread,
	Leverage = A#account.leverage,
	_Balance = A#account.balance,
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
	U_A = A#account{unrealized_PL = Unrealized_PL,order=New_Order},
	{S, U_A}.

close_order(S,A)->
	U_Balance = A#account.balance + A#account.unrealized_PL,
	U_Realized_PL = A#account.realized_PL + A#account.unrealized_PL,
	RealizedPL_Delta = A#account.unrealized_PL,
	U_S = case config:fitness_time_weighted_enabled() of
		true -> S#state{realized_pl_by_cycle = [{S#state.cycle, RealizedPL_Delta} | S#state.realized_pl_by_cycle]};
		false -> S
	end,
	{U_S, A#account{balance=U_Balance,realized_PL=U_Realized_PL,unrealized_PL = 0,order=undefined}}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% FX SENSORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sense(S,Parameters)->
	case Parameters of
		[HRes,VRes,graph_sensor]->
			{Result,U_S}=plane_encoded(HRes,VRes,S);
		[HRes,list_sensor]->
			{Result,U_S}=list_encoded(HRes,S);
		[Spec,series_sensor]->
			%Answered from the packed arrays every agent shares (fx_series.erl); the simulator keeps no window for these.
			{fx_series:answer(S#state.table_name,S#state.index,Spec),S}
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
		Acc;
	fx_GetPriceList(_Table,'end_of_table',_Index,Acc)->
		exit("fx_GetPriceList, reached end_of_table");
	fx_GetPriceList(Table,Key,Index,Acc) ->
		R = fx:lookup(Table,Key),
		fx_GetPriceList(Table,fx:next(Table,Key),Index-1,[{R#technical.open,R#technical.close,R#technical.high,R#technical.low}|Acc]).
		
	l2fx(Index,{[{Open,Close,High,Low}|VList],MemList},VPos,VStep,Acc)->
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
		l2fx(Index-1,{VList,MemList},VPos,VStep,[O|Acc]);
	l2fx(0,{[],_MemList},_VPos,_VStep,Acc)->
		Acc;
	l2fx(Index,{[],MemList},VPos,VStep,Acc)->
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
	%read_concurrency: every simulator reads this table several times per bar and nothing writes to it during a run.
	Table = ets:new(TableName,[ordered_set,public,named_table,{keypos,2},{read_concurrency,true}]),
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

	updater([metadata|TableNames])->
		% Skip non-price metadata table
		updater(TableNames);
	updater([TN|TableNames])->
		%io:format("Updating.~n"),
		insert_ForexRaw(?SOURCE_DIR++atom_to_list(TN)++".txt",update),
		updater(TableNames);
	updater([])->
		case whereis(launcher) of
			undefined -> 
				ok;
			Pid when is_pid(Pid) -> 
				launcher ! fx_updated
		end,
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
	%The heartbeat calls this every few seconds. A file that has not changed since it was last parsed (same size and
	%modification time) is skipped, so the price file is parsed once at startup and again only when new rows arrive.
	Signature = {filelib:file_size(URL),filelib:last_modified(URL)},
	case lists:member(TableName,[atom_to_list(TN) || TN<-?ALL_TABLES]) of
		true when Signature == {0,0} ->
			cant_read;
		true ->
			case get({fx_parsed,URL}) == Signature of
				true ->
					unchanged;
				false ->
					case file:read_file(URL) of
						{ok,Data} ->
							case Flag of
								init ->
									exit("Can not yet recognize Flag: init~n");
								update ->
									% Live trading files ("_LIVE" suffix) carry the sampling rate as leading digits; training files are just the number
									SamplingRate = case string:str(TimeFrame, "_LIVE") > 0 of
										true -> timeframe_to_integer(TimeFrame);
										false -> list_to_integer(TimeFrame)
									end,
									Result = update_ForexDB_lines(list_to_atom(TableName),CurrencyPair,SamplingRate,Data,undefined),
									put({fx_parsed,URL},Signature),
									erlang:garbage_collect(),
									case Result of
										undefined ->
											done;
										NewestKey ->
											io:format("New FOREX_DB update starting with:~p~n",[NewestKey]),
											fx ! {self(),fx_updated,NewestKey},
											done
									end
							end;
						{error,_Error} ->
							%io:format("******** Error reading file:~p in insert_ForexRaw(URL,Flag):~p~n",[URL,Error]),
							cant_read
					end
			end;
		false ->
			io:format("******** TableName:~p is unknown, file rejected.~n",[TableName])
	end.

	%update_ForexDB_lines/5 feeds the file to update_ForexDB/4 one line at a time and returns the key of the first row that
	%was new (undefined if none was). The previous version converted the whole file to one character list first, which
	%costs 16 bytes per byte of file (about 1 GB for the 63 MB EURUSD1.txt, more during garbage collection) on every pass.
	update_ForexDB_lines(TableName,CurrencyPair,SamplingRate,Data,FirstNewKey)->
		case binary:split(Data,<<"\n">>) of
			[Line,Rest] ->
				update_ForexDB_lines(TableName,CurrencyPair,SamplingRate,Rest,update_ForexDB_line(TableName,CurrencyPair,SamplingRate,Line,FirstNewKey));
			[Line] ->
				update_ForexDB_line(TableName,CurrencyPair,SamplingRate,Line,FirstNewKey)
		end.

		update_ForexDB_line(TableName,CurrencyPair,SamplingRate,Line,FirstNewKey)->
			Row = case byte_size(Line) of
				0 -> <<>>;
				Size ->
					case binary:last(Line) of
						$\r -> binary:part(Line,0,Size-1);
						_ -> Line
					end
			end,
			case Row of
				<<>> ->
					FirstNewKey;
				_ ->
					%update_ForexDB/4 expects every row to end in "\r\n"; it returns the row's key if the row was new
					NewKey = update_ForexDB(TableName,CurrencyPair,SamplingRate,binary_to_list(Row)++"\r\n"),
					case FirstNewKey of
						undefined -> NewKey;
						_ -> FirstNewKey
					end
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

%% Convert timeframe suffix to integer by taking leading digits only
%% Examples: "1" -> 1, "1_LIVE" -> 1, "15_TEST" -> 15
timeframe_to_integer(TimeframeRaw) ->
	Digits = take_digits(TimeframeRaw, []),
	case Digits of
		[] -> erlang:error({bad_timeframe, TimeframeRaw});
		_  -> list_to_integer(Digits)
	end.

take_digits([C|Rest], Acc) when C >= $0, C =< $9 ->
	take_digits(Rest, [C|Acc]);
take_digits(_Other, Acc) ->
	lists:reverse(Acc).

update_ForexDB(_TableName,_CurrencyPair,_SamplingRate,[])->
	Key = get(new_id),
	erase(new_id),
	Key;


update_ForexDB(TableName,CurrencyPair,SamplingRate,List)->
	% Expected CSV format per line (new):
	% YYYY-MM-DD<space>HH:MM:SS,Open,High,Low,Close,Volume
	% Example: 2024-09-02 21:15:00,1.10727,1.107275,1.1072,1.10725,-1.0\r\n
	% Parse date components
	{YearL,Remainder1} = split_with(45,List),		% - 45
	{MonthL,Remainder2} = split_with(45,Remainder1),	% - 45
	{DayL,Remainder3} = split_with(32,Remainder2),		% " " 32

	% Parse time components
	{HourL,Remainder4} = split_with(58,Remainder3),		% : 58
	{MinuteL,Remainder5} = split_with(58,Remainder4),	% : 58
	{SecondL,Remainder6} = split_with(44,Remainder5),	% , 44

	% Parse OHLCV fields
	{OpenL,Remainder7} = split_with(44,Remainder6),		% , 44
	{HighL,Remainder8} = split_with(44,Remainder7),		% , 44
	{LowL,Remainder9} = split_with(44,Remainder8),		% , 44
	{CloseL,Remainder10} = split_with(44,Remainder9),	% , 44
	{VolumeL,Remainder11} = split_with(13,Remainder10),	% \r 13
	[_|Remainder] = Remainder11,				% gets rid of (\n 10)

%	io:format("here~p~n",[{YearL,MonthL,DayL,HourL,MinuteL,SecondL,OpenL,HighL,LowL,CloseL,VolumeL}]),
	Year = list_to_integer(YearL),
	Month = list_to_integer(MonthL),
	Day = list_to_integer(DayL),
	Hour = list_to_integer(HourL),
	Minute = list_to_integer(MinuteL),
	Second = list_to_integer(SecondL),
	Open = list_to_number(OpenL),
	High = list_to_number(HighL),
	Low = list_to_number(LowL),
	Close = list_to_number(CloseL),
	Volume = list_to_number(VolumeL),

	% Keep the key structure consistent: {Y,Mo,D,H,Mi,Sec,SamplingRate}
	Id = {Year,Month,Day,Hour,Minute,Second,SamplingRate},
	case ((Open+High+Low+Close) < 1000) and ((Open+High+Low+Close) > -1000) of
		true ->
			case member(TableName,Id) of
				false ->%{key,%%%key={Year,Month,Day,Hour,Minute,Second,sampling_rate},open,high,low,close,volume}).
					Record = #technical{id=Id,open=Open,high=High,low=Low,close=Close,volume=Volume},
					insert(TableName,Record),
					case get(new_id) of
						undefined ->
							put(new_id,Id);
						_ ->
							done
					end;
				true ->
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
	[_,_,_,{size,Size},_,_,_,_,_] = ets:info(TableName),
	Size.	
