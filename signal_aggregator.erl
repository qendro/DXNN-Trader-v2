
-module(signal_aggregator).
-compile(export_all).
-include("records.hrl").

dot_product(IAcc,IPIdPs)->
	dot_product(IAcc,IPIdPs,0).
dot_product([{IPId,Input}|IAcc],[{IPId,WeightsP}|IPIdPs],Acc)->
	case {WeightsP, length(Input), length(WeightsP)} of
		{[], InLen, _} -> qlog:xLog(qStatus, "dot_input EMPTY_WEIGHTS IPId=~p InputLen=~p", [IPId, InLen]);
		{_WPs, InLen, WLen} when InLen =/= WLen -> qlog:xLog(qStatus, "dot_input LEN_MISMATCH IPId=~p InputLen=~p WLen=~p", [IPId, InLen, WLen]);
		_ -> ok
	end,
	Dot = dot(Input,WeightsP,0),
	dot_product(IAcc,IPIdPs,Dot+Acc);
dot_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	Acc + Bias;
dot_product([],[],Acc)->
	Acc.
		
		dot([I|Input],[{W,_LPs}|WeightsP],Acc) ->
			dot(Input,WeightsP,I*W+Acc);
		dot([],[],Acc)->
			Acc;
		dot(Input,WeightsP,Acc) ->
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
			
% Safe multiplication limits to prevent arithmetic overflow
-define(MULT_MAX, 1.0e150).
-define(MULT_MIN, -1.0e150).

mult_product(IAcc,IPIdPs)->
	mult_product(IAcc,IPIdPs,1).
mult_product([{IPId,Input}|IAcc],[{IPId,WeightsP}|IPIdPs],Acc)->
	Dot = mult(Input,WeightsP,1),
	SafeProduct = safe_mult(Dot, Acc),
	mult_product(IAcc,IPIdPs,SafeProduct);
mult_product([],[{bias,[{Bias,_LPs}]}],Acc)->
	safe_mult(Acc, Bias);
mult_product([],[],Acc)->
	Acc.

	mult([I|Input],[{W,_LPs}|Weights],Acc) ->
		% Multiply I*W first, then multiply result with Acc (both protected)
		IW = safe_mult(I, W),
		SafeAcc = safe_mult(IW, Acc),
		mult(Input,Weights,SafeAcc);
	mult([],[],Acc)->
		Acc.

% Helper: Safe multiplication that clamps to prevent overflow
% Fast path: magnitude check first, then multiply with catch fallback
safe_mult(A, B) when abs(A) > 1.0e75; abs(B) > 1.0e75 ->
	% Quick exit for huge values - clamp based on signs
	if (A > 0) == (B > 0) -> ?MULT_MAX; true -> ?MULT_MIN end;
safe_mult(A, B) ->
	% Normal path: try multiply, catch overflow, clamp result
	try
		Product = A * B,
		if Product > ?MULT_MAX -> ?MULT_MAX;
		   Product < ?MULT_MIN -> ?MULT_MIN;
		   true -> Product
		end
	catch error:badarith ->
		% Overflow occurred - clamp based on signs
		if (A > 0) == (B > 0) -> ?MULT_MAX; true -> ?MULT_MIN end
	end.
