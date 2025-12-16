
-module(actuator).
-compile(export_all).
-include("records.hrl").

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf_PId) -> 
	receive 
		{ExoSelf_PId,{Id,Cx_PId,Scape,ActuatorName,Parameters,Fanin_PIds,OpMode}} ->
			put(opmode,OpMode),
			connectivity_check_loop(Id,ExoSelf_PId,Cx_PId,Scape,ActuatorName,Parameters,{Fanin_PIds,Fanin_PIds})
	end.
%When gen/2 is executed it spawns the actuator element and immediately begins to wait for its initial state message.

connectivity_check_loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{Fanin_PIds,MFanin_PIds})->
	receive
		{From,connectivity_check}-> 
			case lists:member(From, Fanin_PIds) of
				true ->
					Cx_PId ! {self(),connectivity_check_complete}, 
					loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{Fanin_PIds,MFanin_PIds},[]);
				false ->
					connectivity_check_loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{Fanin_PIds,MFanin_PIds}) % Discard from unknown sources
			end;
		{ExoSelf_PId,terminate} -> 
			ok
	end.

loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{[From_PId|Fanin_PIds],MFanin_PIds},Acc) ->
	receive
		{From_PId,forward,Input} -> 
			loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{Fanin_PIds,MFanin_PIds},lists:append(Input,Acc));
		{_From,connectivity_check}-> loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{[From_PId|Fanin_PIds],MFanin_PIds},Acc);
		{ExoSelf_PId,terminate} -> 
			ok
	end;
loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{[],MFanin_PIds},Acc)->
	{Fitness,EndFlag} = actuator:AName(ExoSelf_PId,lists:reverse(Acc),Parameters,Scape),
	Cx_PId ! {self(),sync,Fitness,EndFlag},
	loop(Id,ExoSelf_PId,Cx_PId,Scape,AName,Parameters,{MFanin_PIds,MFanin_PIds},[]).
%The actuator process gathers the control signals from the neurons, appending them to the accumulator. The order in which the signals are accumulated into a vector is in the same order as the neuron ids are stored within NIds. Once all the signals have been gathered, the actuator sends cortex the sync signal, executes its function, and then again begins to wait for the neural signals from the output layer by reseting the Fanin_PIds from the second copy of the list.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ACTUATORS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pts(ExoSelf_PId,Result,_Scape)->
	io:format("actuator:pts(Result): ~p~n",[Result]),
	{1,0}.
%The pts/2 actuation function simply prints to screen the vector passed to it.


% Actuator allows the agent to send a trade commmand to the Scape and receive the fitness and halt flag in return.
% TradeSignal will be:
% -1 → Short (sell).
% 0 → Hold or close position.
% 1 → Long (buy).
fx_Trade(ExoSelf_PId,Output,Parameters,Scape)->
	[TradeSignal] = Output,
	Scape ! {self(),trade,config:primary_currency_pair(),functions:trinary(TradeSignal)},
	receive 
		{Scape,Fitness,HaltFlag}->
			{Fitness,HaltFlag}
	end.
