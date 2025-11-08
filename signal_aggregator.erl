
-module(signal_aggregator).
-compile(export_all).
-include("records.hrl").

dot_product(IAcc,IPIdPs)->
	%qlog:l1msg(self(), "DEBUG: dot_product called with " ++ integer_to_list(length(IAcc)) ++ " inputs, " ++ integer_to_list(length(IPIdPs)) ++ " weight pairs"),
	dot_product(IAcc,IPIdPs,0).
dot_product([{IPId,Input}|IAcc],[{IPId,WeightsP}|IPIdPs],Acc)->
	%qlog:l1msg(self(), "DEBUG: Processing input from " ++ lists:flatten(io_lib:format("~p", [IPId])) ++ " with input length=" ++ integer_to_list(length(Input)) ++ ", weights length=" ++ integer_to_list(length(WeightsP))),
	Dot = dot(Input,WeightsP,0),
	%qlog:l1msg(self(), "DEBUG: Dot product result=" ++ lists:flatten(io_lib:format("~p", [Dot]))),
	dot_product(IAcc,IPIdPs,Dot+Acc);
dot_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	Acc + Bias;
dot_product([],[],Acc)->
	Acc.
		
		dot([I|Input],[{W,_LPs}|WeightsP],Acc) ->
			%qlog:l1msg(self(), "DEBUG: dot processing I=" ++ lists:flatten(io_lib:format("~p", [I])) ++ ", W=" ++ lists:flatten(io_lib:format("~p", [W])) ++ ", Acc=" ++ lists:flatten(io_lib:format("~p", [Acc]))),
			dot(Input,WeightsP,I*W+Acc);
		dot([],[],Acc)->
			%qlog:l1msg(self(), "DEBUG: dot completed with final Acc=" ++ lists:flatten(io_lib:format("~p", [Acc]))),
			Acc;
		dot(Input,WeightsP,Acc) ->
			%qlog:l1msg(self(), "DEBUG: dot function_clause error - Input=" ++ lists:flatten(io_lib:format("~p", [Input])) ++ ", WeightsP=" ++ lists:flatten(io_lib:format("~p", [WeightsP])) ++ ", Acc=" ++ lists:flatten(io_lib:format("~p", [Acc]))),
			exit("Error in dot function - function_clause with Input=" ++ lists:flatten(io_lib:format("~p", [Input])) ++ ", WeightsP=" ++ lists:flatten(io_lib:format("~p", [WeightsP]))).
%The dot/3 function accepts an input vector and a weight list, and computes the dot product of the two vectors.

diff_product(IAcc,IPIdPs)->
	case get(diff_product) of
		undefined ->
			put(diff_product,IAcc),
			dot_product(IAcc,IPIdPs,0);
		Prev_IAcc ->
			put(diff_product,IAcc),
			Diff_IAcc = input_diff(IAcc,Prev_IAcc,[]),
			dot_product(Diff_IAcc,IPIdPs,0)
	end.
	
	input_diff([{IPId,Input}|IAcc],[{IPId,Prev_Input}|Prev_IAcc],Acc)->
		Vector_Diff = diff(Input,Prev_Input,[]),
		input_diff(IAcc,Prev_IAcc,[{IPId,Vector_Diff}|Acc]);
	input_diff([],[],Acc)->
		lists:reverse(Acc).
	
		diff([A|Input],[B|Prev_Input],Acc)->
			diff(Input,Prev_Input,[A-B|Acc]);
		diff([],[],Acc)->
			lists:reverse(Acc).
			
mult_product(IAcc,IPIdPs)->
	mult_product(IAcc,IPIdPs,1).
mult_product([{IPId,Input}|IAcc],[{IPId,WeightsP}|IPIdPs],Acc)->
	Dot = mult(Input,WeightsP,1),
	mult_product(IAcc,IPIdPs,Dot*Acc);
mult_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	Acc * Bias;
mult_product([],[],Acc)->
	Acc.

	mult([I|Input],[{W,_LPs}|Weights],Acc) ->
		mult(Input,Weights,I*W*Acc);
	mult([],[],Acc)->
		Acc.
