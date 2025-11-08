
-module(substrate).
-compile(export_all).
-include("records.hrl").
-define(SAT_LIMIT,math:pi()).
-record(state,{
	type,
	plasticity=none,
	morphology,
	specie_id,
	sensors,
	actuators,
	spids=[],
	apids=[],
	cpp_pids=[],
	cep_pids=[],
	densities,
	substrate_state_flag,
	old_substrate,
	cur_substrate,
	link_form
}).

gen(ExoSelf_PId,Node)->
	spawn(Node,?MODULE,prep,[ExoSelf_PId]).

prep(ExoSelf)->
	random:seed(now()),
	receive
		{ExoSelf,init,InitState}->
			{Sensors,Actuators,SPIds,APIds,CPP_PIds,CEP_PIds,Densities,Plasticity,LinkForm}=InitState,
			%io:format("InitState:~p~n",[InitState]),
			%qlog:agent(ExoSelf, io_lib:format("Substrate initialized | SPIds: ~p | APIds: ~p | CPP_PIds: ~p | CEP_PIds: ~p | Sensors: ~p | Actuators: ~p", [SPIds, APIds, CPP_PIds, CEP_PIds, length(Sensors), length(Actuators)])),
			S = #state{
				sensors=Sensors,
				actuators=Actuators,
				spids=SPIds,
				apids=APIds,
				cpp_pids=CPP_PIds,
				cep_pids=CEP_PIds,
				densities = Densities,
				substrate_state_flag=reset,
				old_substrate=void,
				cur_substrate=init,
				plasticity=Plasticity,
				link_form = LinkForm
			},
			substrate:loop(ExoSelf,S,SPIds,[])
	end.

loop(ExoSelf,S,[SPId|SPIds],SAcc)->
	%io:format("~p~n",[S]),
	receive
		{SPId,forward,Sensory_Signal}->
			%qlog:l1msg(self(), "DEBUG: Substrate received data from sensor " ++ lists:flatten(io_lib:format("~p", [SPId])) ++ " with length=" ++ integer_to_list(length(Sensory_Signal)) ++ ", data=" ++ lists:flatten(io_lib:format("~p", [lists:sublist(Sensory_Signal, 5)])) ++ " (showing first 5 elements)"),
			loop(ExoSelf,S,SPIds,[Sensory_Signal|SAcc]);
		{ExoSelf,reset_substrate}->
			U_S = S#state{
				old_substrate=S#state.cur_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SPId|SPIds],SAcc);
		{ExoSelf,backup_substrate} ->
%			io:format("reseting:~n"),
			U_S = S#state{
				old_substrate=S#state.cur_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SPId|SPIds],SAcc);
		{ExoSelf,revert_substrate} ->
%			io:format("reverting:~n"),
			U_S = S#state{
				cur_substrate = S#state.old_substrate,
				substrate_state_flag=reset
			},
			ExoSelf ! {self(),ready},
			loop(ExoSelf,U_S,[SPId|SPIds],SAcc);
		{ExoSelf,terminate}->
%			io:format("Resulting substrate:~p~n",[Substrate]),
			void;
		{UnexpectedSPId,forward,Sensory_Signal} ->
			%qlog:l1msg(ExoSelf, io_lib:format("Substrate unexpected sensor order | Expected: ~p | Got: ~p | InList: ~p", [SPId, UnexpectedSPId, lists:member(UnexpectedSPId,[SPId|SPIds])])),
			case lists:member(UnexpectedSPId,[SPId|SPIds]) of
				true ->
					%qlog:l2msg(ExoSelf, io_lib:format("Substrate accepting out-of-order sensor | SPId: ~p | Remaining: ~p", [UnexpectedSPId, lists:delete(UnexpectedSPId,[SPId|SPIds])])),
					loop(ExoSelf,S,lists:delete(UnexpectedSPId,[SPId|SPIds]),[Sensory_Signal|SAcc]);
				false ->
					%qlog:l1msg(ExoSelf, io_lib:format("Substrate ignoring unknown sensor | SPId: ~p | Expected list: ~p", [UnexpectedSPId, [SPId|SPIds]])),
					loop(ExoSelf,S,[SPId|SPIds],SAcc)
			end;
		Msg ->
			%qlog:l1msg(ExoSelf, io_lib:format("Substrate unknown message | Msg: ~p | Expected SPId: ~p | Remaining: ~p", [Msg, SPId, SPIds])),
			loop(ExoSelf,S,[SPId|SPIds],SAcc)
%		after 20000 ->
%			io:format("********ERROR: Substrate Crashed:~p~n",[S])
	end;
loop(ExoSelf,S,[],SAcc)->%All sensory signals received
	{U_Substrate,U_SMode,OAcc} = reason(SAcc,S),
	%qlog:l1msg(self(), "DEBUG: Substrate processed " ++ integer_to_list(length(SAcc)) ++ " sensor inputs, sending " ++ integer_to_list(length(OAcc)) ++ " outputs to neuron"),
	%%qlog:l1msg(self(), "DEBUG: Substrate output data: " ++ lists:flatten(io_lib:format("~p", [lists:sublist(OAcc, 5)])) ++ " (showing first 5 elements)"),
	advanced_fanout(OAcc,S#state.actuators,S#state.apids),
	U_S = S#state{
		cur_substrate=U_Substrate,
		substrate_state_flag=U_SMode
	},
	loop(ExoSelf,U_S,S#state.spids,[]).

reason(Input,S)->
	Densities = S#state.densities,
	Substrate = S#state.cur_substrate,
	SMode = S#state.substrate_state_flag,
	CPP_PIds = S#state.cpp_pids,
	CEP_PIds = S#state.cep_pids,
	Plasticity = S#state.plasticity,
	case SMode of
		reset ->%io:format("reset~n"),
			Sensors=S#state.sensors,
			Actuators=S#state.actuators,
			New_Substrate = create_substrate(Sensors,Densities,Actuators,S#state.link_form),
			U_SMode=case Plasticity of
				iterative ->
					{Output,Populated_Substrate} = calculate_ResetOutput(Densities,New_Substrate,Input,CPP_PIds,CEP_PIds,Plasticity,S#state.link_form),
					iterative;
				_ ->
					{Output,Populated_Substrate} = calculate_ResetOutput(Densities,New_Substrate,Input,CPP_PIds,CEP_PIds,Plasticity,S#state.link_form),
					hold
			end,
			{Populated_Substrate,U_SMode,Output};
		iterative ->%io:format("Iterative~n"),
			{Output,U_Substrate} = calculate_ResetOutput(Densities,Substrate,Input,CPP_PIds,CEP_PIds,Plasticity,S#state.link_form),
			{U_Substrate,SMode,Output};
		hold ->%io:format("hold~n"),
			{Output,U_Substrate} = calculate_HoldOutput(Densities,Substrate,Input,S#state.link_form,Plasticity,CPP_PIds,CEP_PIds),
			{U_Substrate,SMode,Output}
	end.

advanced_fanout(OAcc,[Actuator|Actuators],[APId|APIds])->
	{Output,OAccRem}=lists:split(Actuator#actuator.vl,OAcc),
	APId ! {self(),forward,Output},
	advanced_fanout(OAccRem,Actuators,APIds);
advanced_fanout([],[],[])->
	ok.
%%==================================================================== Internal Functions
fanout([Pid|Pids],Msg)->
	Pid ! Msg,
	fanout(Pids,Msg);
fanout([],_Msg)->
	true.

flush_buffer()->
	receive 
		ANY -> %io:format("ANY:~p~n",[ANY]),
		flush_buffer()
	after 0 ->
		done
end.

%	no_geo
%	{symmetric,[R1,R2...Rk],[Val1...Valn]} where n == R1*R2*...Dk and k = dimension
%	{asymmetric,[[R1..Rp],[R1..Rt]],[Val1...Valn]} where lists:sum(lists:flatten([[R1...Rp],[R1..Rt]])) == n, and depth = Dimension.
%	coorded, every val comes with its own coord tuple: {Coord,Val}. The coord is a list, thus specifying the dimensionality.
test_cs()->
	Sensors = [
		#sensor{format=no_geo,vl=3},
		#sensor{format={symmetric,lists:reverse([2,3])},vl=6}
	],
	Actuators = [
		#actuator{format=no_geo,vl=2},
		#actuator{format={symmetric,lists:reverse([3,2])},vl=6}
	],
	create_substrate(Sensors,[3,2,3,2],Actuators,l2l_feedforward).
	
test_IS(SubstrateDimension)->
	Sensors = [
		#sensor{format=no_geo,vl=10},
		#sensor{format={symmetric,lists:reverse([3,4])},vl=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_ISubstrate(Sensors,SubstrateDimension).

test_OS(SubstrateDimension)->
	Actuators = [
		#actuator{format=no_geo,vl=10},
		#actuator{format={symmetric,lists:reverse([3,4])},vl=[
		1,-1,-1,-1,
		1,-1,-1,-1,
		1,1,1,1]}
	],
	compose_OSubstrate(Actuators,SubstrateDimension,[w1,w2,w3]).
	
create_substrate(Sensors,Densities,Actuators,LinkForm)->
	%qlog:l1msg(self(), "DEBUG: create_substrate called with " ++ integer_to_list(length(Sensors)) ++ " sensors, densities=" ++ lists:flatten(io_lib:format("~p", [Densities])) ++ ", " ++ integer_to_list(length(Actuators)) ++ " actuators"),
	[Depth|SubDensities] = Densities,
	Substrate_I = compose_ISubstrate(Sensors,length(Densities)),
	I_VL = length(Substrate_I),
	case LinkForm of
		l2l_feedforward ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL,Weight),
			HWeights = lists:duplicate(H,Weight);
		fully_interconnected ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			Tot_HiddenNeurodes = mult([Depth-1|SubDensities]),
			Tot_Weights = Tot_HiddenNeurodes + I_VL + Output_Neurodes,
			IWeights = lists:duplicate(Tot_Weights,Weight),
			HWeights = lists:duplicate(Tot_Weights,Weight);
		jordan_recurrent ->
			Output_Neurodes = tot_ONeurodes(Actuators,0),
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL+Output_Neurodes,Weight),
			HWeights = lists:duplicate(H,Weight);
		neuronself_recurrent ->
			Weight = 0,
			H = mult(SubDensities),
			IWeights = lists:duplicate(I_VL+1,Weight),
			HWeights = lists:duplicate(H+1,Weight)
	end,	
	case Depth of
		0 ->
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),IWeights),
			[Substrate_I,Substrate_O];
		1 ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
			[Substrate_I,extrude(0,Substrate_R),Substrate_O];
		_ ->
			Substrate_R = cs(SubDensities,IWeights),
			Substrate_H = cs(SubDensities,HWeights),
			Substrate_O=compose_OSubstrate(Actuators,length(Densities),HWeights),
			[_,RCoord|C1] = build_CoordList(Depth+1),
			[_|C2] = lists:reverse(C1),
			HCoords = lists:reverse(C2),
			ESubstrate_R = extrude(RCoord,Substrate_R),
			ESubstrates_H = [extrude(HCoord,Substrate_H) || HCoord<-HCoords],
			lists:append([[Substrate_I,ESubstrate_R],ESubstrates_H,[Substrate_O]])
	end.

	compose_ISubstrate(Sensors,SubstrateDimension)->
		compose_ISubstrate(Sensors,[],1,SubstrateDimension-2).
	compose_ISubstrate([S|Sensors],Acc,Max_Dim,Required_Dim)->
		%qlog:l1msg(self(), "DEBUG: Processing sensor " ++ atom_to_list(S#sensor.name) ++ " with format=" ++ lists:flatten(io_lib:format("~p", [S#sensor.format])) ++ ", vl=" ++ integer_to_list(S#sensor.vl)),
		case S#sensor.format of
			undefined ->
				Dim=1,
				%qlog:l1msg(self(), "DEBUG: Creating coord lists for undefined format sensor with vl=" ++ integer_to_list(S#sensor.vl)),
				CoordLists = create_CoordLists([S#sensor.vl]),
				%qlog:l1msg(self(), "DEBUG: Created " ++ integer_to_list(length(CoordLists)) ++ " coordinate lists for undefined format sensor"),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				{Dim,ISubstrate_Part};
			no_geo ->
				Dim=1,
				%qlog:l1msg(self(), "DEBUG: Creating coord lists for no_geo format sensor with vl=" ++ integer_to_list(S#sensor.vl)),
				CoordLists = create_CoordLists([S#sensor.vl]),
				%qlog:l1msg(self(), "DEBUG: Created " ++ integer_to_list(length(CoordLists)) ++ " coordinate lists for no_geo format sensor"),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				{Dim,ISubstrate_Part};
			{symmetric,Resolutions}->
				Dim = length(Resolutions),
				Signal_Length = mult(Resolutions),
				%qlog:l1msg(self(), "DEBUG: Creating coord lists for symmetric format sensor with resolutions=" ++ lists:flatten(io_lib:format("~p", [Resolutions])) ++ ", signal_length=" ++ integer_to_list(Signal_Length)),
				CoordLists = create_CoordLists(Resolutions),
				%qlog:l1msg(self(), "DEBUG: create_CoordLists returned, creating ISubstrate_Part"),
				ISubstrate_Part=[{Coord,0,void}|| Coord<-CoordLists],
				%qlog:l1msg(self(), "DEBUG: Created " ++ integer_to_list(length(CoordLists)) ++ " coordinate lists for symmetric format sensor, ISubstrate_Part length=" ++ integer_to_list(length(ISubstrate_Part))),
				{Dim,ISubstrate_Part};
			{coorded,Dim,Resolutions,ISubstrate_Part} ->
				{Dim,ISubstrate_Part}
		end,
		U_Dim = case Max_Dim > Dim of
			true ->
				Max_Dim;
			false ->
				Dim
		end,
		compose_ISubstrate(Sensors,[ISubstrate_Part|Acc],U_Dim,Required_Dim);
	compose_ISubstrate([],Acc,ISubstratePart_MaxDim,Required_Dim)->
		%qlog:l1msg(self(), "DEBUG: compose_ISubstrate finished processing all sensors, Acc length=" ++ integer_to_list(length(Acc)) ++ ", MaxDim=" ++ integer_to_list(ISubstratePart_MaxDim) ++ ", RequiredDim=" ++ integer_to_list(Required_Dim)),
		%qlog:l1msg(self(), "DEBUG: Acc contains substrate parts with lengths: " ++ lists:flatten(io_lib:format("~p", [lists:map(fun(Part) -> length(Part) end, Acc)]))),
		%qlog:l1msg(self(), "DEBUG: Checking if Required_Dim >= ISubstratePart_MaxDim: " ++ integer_to_list(Required_Dim) ++ " >= " ++ integer_to_list(ISubstratePart_MaxDim)),
		case Required_Dim >= ISubstratePart_MaxDim of
			true ->
				%qlog:l1msg(self(), "DEBUG: Condition true, proceeding with adv_extrude"),
				ISubstrate_Depth = length(Acc),
				%qlog:l1msg(self(), "DEBUG: ISubstrate_Depth = " ++ integer_to_list(ISubstrate_Depth)),
				ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
				%qlog:l1msg(self(), "DEBUG: About to call adv_extrude with depth=" ++ integer_to_list(ISubstrate_Depth) ++ ", coords=" ++ integer_to_list(length(ISubstrate_DepthCoords))),
				adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),-1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coords.
			false ->
				%qlog:l1msg(self(), "DEBUG 294: Condition false, exiting with error"),
				exit("Error in adv_extrude, Required_Depth < ISubstratePart_MaxDepth~n")
		end.

		adv_extrude([ISubstrate_Part|ISubstrate],Required_Dim,[IDepthCoord|ISubstrate_DepthCoords],LeadCoord,Acc)->
			%qlog:l1msg(self(), "DEBUG: adv_extrude processing substrate part with " ++ integer_to_list(length(ISubstrate_Part)) ++ " elements"),
			%qlog:l1msg(self(), "DEBUG: Sample coordinates before extrusion: " ++ lists:flatten(io_lib:format("~p", [lists:sublist([Coord || {Coord,_,_} <- ISubstrate_Part], 3)]))),
			Extruded_ISP = [{[LeadCoord,IDepthCoord|lists:append(lists:duplicate(Required_Dim - length(Coord),0),Coord)],O,W} || {Coord,O,W}<-ISubstrate_Part],
			%qlog:l1msg(self(), "DEBUG: Sample coordinates after extrusion: " ++ lists:flatten(io_lib:format("~p", [lists:sublist([Coord || {Coord,_,_} <- Extruded_ISP], 3)]))),
			extrude(ISubstrate_Part,Required_Dim,IDepthCoord,[]),
			adv_extrude(ISubstrate,Required_Dim,ISubstrate_DepthCoords,LeadCoord,lists:append(Extruded_ISP,Acc));
		adv_extrude([],_Required_Dim,[],_LeadCoord,Acc)->
			%qlog:l1msg(self(), "DEBUG: adv_extrude completed, returning " ++ integer_to_list(length(Acc)) ++ " elements"),
			Acc.
			
			extrude([{Coord,O,W}|ISubstrate_Part],Required_Dim,DepthCoord,Acc)->
				Dim_Dif = Required_Dim - length(Coord),
				U_Coord= [1,DepthCoord|lists:append(lists:duplicate(Dim_Dif,0),Coord)],
				extrude(ISubstrate_Part,Required_Dim,DepthCoord,[{U_Coord,O,W}|Acc]);
			extrude([],_Required_Dim,_DepthCoord,Acc)->
				Acc.

	compose_OSubstrate(Actuators,SubstrateDimension,Weights)->
		compose_OSubstrate(Actuators,[],1,SubstrateDimension-2,Weights).
	compose_OSubstrate([A|Actuators],Acc,Max_Dim,Required_Dim,Weights)->
		case A#actuator.format of
			undefined ->%Dim=void,OSubstrate_Part=void,
				Dim=1,
				CoordLists = create_CoordLists([A#actuator.vl]),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			no_geo ->%Dim=void,OSubstrate_Part=void,
				Dim=1,
				CoordLists = create_CoordLists([A#actuator.vl]),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			{symmetric,Resolutions}->%Dim=void,OSubstrate_Part=void,
				Dim = length(Resolutions),
				Signal_Length = mult(Resolutions),
				CoordLists = create_CoordLists(Resolutions),
				OSubstrate_Part=[{Coord,0,Weights}|| Coord<-CoordLists],
				{Dim,OSubstrate_Part};
			{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
				OSubstrate_Part=[{Coord,O,Weights}|| {Coord,O,_}<-Unadjusted_OSubstrate_Part],
				{Dim,OSubstrate_Part}
		end,
		U_Dim = case Max_Dim > Dim of
			true ->
				Max_Dim;
			false ->
				Dim
		end,
		compose_OSubstrate(Actuators,[OSubstrate_Part|Acc],U_Dim,Required_Dim,Weights);
	compose_OSubstrate([],Acc,OSubstratePart_MaxDim,Required_Dim,_Weights)->
		case Required_Dim >= OSubstratePart_MaxDim of
			true ->%done;
				ISubstrate_Depth = length(Acc),
				ISubstrate_DepthCoords = build_CoordList(ISubstrate_Depth),
				adv_extrude(Acc,Required_Dim,lists:reverse(ISubstrate_DepthCoords),1,[]);%Passed in inverted,reversed inside adv_extrude, same for depth coord
			false ->
				%qlog:l1msg(self(), "DEBUG 344: Condition false, exiting with error"),
				exit("Error in adv_extrude, Required_Depth < OSubstratePart_MaxDepth~n")
		end.

		find_depth(Resolutions)->find_depth(Resolutions,0).
		find_depth(Resolutions,Acc)->
			case is_list(Resolutions) of
				true ->
					[_Head|Tail] = Resolutions,
					find_depth(Tail,Acc+1);
				false ->
					Acc
			end.

%Substrate encoding: X density = n, Y density = k, Z density = p, T density = l
%Weights = [W1,W2...WI],
%[[{[Z1,Y,X],o,[W1...Wn]}...{[Z1,Yn,Xk],o,[W1...Wn]}]...[{[Zs,Y,X],o,[W1...Wn]}...]],
		build_CoordList(Density)->
			%qlog:l1msg(self(), "DEBUG: build_CoordList called with density=" ++ integer_to_list(Density)),
			case Density == 1 of
				true ->
					%qlog:l1msg(self(), "DEBUG: Density=1, returning [0.0]"),
					[0.0];
				false ->
					DensityDividers = Density - 1,
					Resolution = 2/DensityDividers,
					%qlog:l1msg(self(), "DEBUG: Density=" ++ integer_to_list(Density) ++ ", dividers=" ++ integer_to_list(DensityDividers) ++ ", resolution=" ++ float_to_list(Resolution)),
					build_CoordList(Resolution,DensityDividers,1,[])
			end.

			extend(I,DI,D,Substrate)->
				void.
				
			mult(List)->
				mult(List,1).
			mult([Val|List],Acc)->
				mult(List,Val*Acc);
			mult([],Acc)->
				Acc.

tot_ONeurodes([A|Actuators],Acc)->
	Tot_ANeurodes=case A#actuator.format of
		undefined ->
			A#actuator.vl;
		no_geo ->
			A#actuator.vl;
		{symmetric,Resolutions}->
			mult(Resolutions);
		{coorded,Dim,Resolutions,Unadjusted_OSubstrate_Part} ->
			length(Unadjusted_OSubstrate_Part)
	end,
	tot_ONeurodes(Actuators,Tot_ANeurodes+Acc);
tot_ONeurodes([],Acc)->
	Acc.


%[{[D3,D2,D1],o,[W1,W2,W3...]}...]
	cs(Densities,Weights)->
		RDensities = lists:reverse(Densities),
		Substrate = create_CoordLists(RDensities,[]),
		attach(Substrate,0,Weights).
	
		create_CoordLists(Densities)->
			%qlog:l1msg(self(), "DEBUG: create_CoordLists called with densities=" ++ lists:flatten(io_lib:format("~p", [Densities]))),
			create_CoordLists(Densities,[]).	
		create_CoordLists([Density|RDensities],[])->
			%qlog:l1msg(self(), "DEBUG: Building coord list for density=" ++ integer_to_list(Density)),
			CoordList = build_CoordList(Density),
			%qlog:l1msg(self(), "DEBUG: Built coord list with " ++ integer_to_list(length(CoordList)) ++ " elements"),
			XtendedCoordList = [[Coord]||Coord <- CoordList],
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([Density|RDensities],Acc)->
			%qlog:l1msg(self(), "DEBUG: create_CoordLists recursive call with density=" ++ integer_to_list(Density) ++ ", Acc length=" ++ integer_to_list(length(Acc))),
			CoordList = build_CoordList(Density),
			%qlog:l1msg(self(), "DEBUG: Built coord list with " ++ integer_to_list(length(CoordList)) ++ " elements, about to create extended list"),
			XtendedCoordList = [[Coord|Sub_Coord]||Coord <- CoordList,Sub_Coord <- Acc],
			%qlog:l1msg(self(), "DEBUG: Created extended coord list with " ++ integer_to_list(length(XtendedCoordList)) ++ " elements"),
			create_CoordLists(RDensities,XtendedCoordList);
		create_CoordLists([],Acc)->
			%qlog:l1msg(self(), "DEBUG: create_CoordLists completed, returning " ++ integer_to_list(length(Acc)) ++ " coordinate lists"),
			Acc.
			
			build_CoordList(Resolution,0,Coord,Acc)->
				[-1|Acc];
			build_CoordList(Resolution,DensityDividers,Coord,Acc)->
				build_CoordList(Resolution,DensityDividers-1,Coord-Resolution,[Coord|Acc]).
		
attach(List,E1,E2)->
	attach(List,E1,E2,[]).
attach([Val|List],E1,E2,Acc)->
	attach(List,E1,E2,[{Val,E1,E2}|Acc]);
attach([],_E1,_E2,Acc)->
	lists:reverse(Acc).
	
extrude(NewDimension_Coord,Substrate)->
	extrude(NewDimension_Coord,Substrate,[]).
extrude(NewDimension_Coord,[{Coord,O,W}|Substrate],Acc)->
	extrude(NewDimension_Coord,Substrate,[{[NewDimension_Coord|Coord],O,W}|Acc]);
extrude(_Coord,[],Acc)->
	lists:reverse(Acc).

calculate_HoldOutput(Densities,Substrate,Input,LinkForm,Plasticity,CPP_PIds,CEP_PIds)->
	[IHyperlayer|Populated_PHyperlayers] = Substrate,
	Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer,lists:flatten(Input),[]),
	{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,Populated_PHyperlayers,LinkForm,Plasticity,CPP_PIds,CEP_PIds),
	{Output,[IHyperlayer|U_PHyperlayers]}.

calculate_ResetOutput(Densities,Substrate,Input,CPP_PIds,CEP_PIds,Plasticity,LinkForm)->
	[IHyperlayer|PHyperlayers] = Substrate,
	Populated_IHyperlayer = populate_InputHyperlayer(IHyperlayer,lists:flatten(Input),[]),
	case Plasticity of
		iterative ->
			{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,PHyperlayers,LinkForm,Plasticity,CPP_PIds,CEP_PIds),
			{Output,[IHyperlayer|U_PHyperlayers]};
		_->
			Populated_PHyperlayers = populate_PHyperlayers(Substrate,CPP_PIds,CEP_PIds,LinkForm,Plasticity),
			{Output,U_PHyperlayers}=calculate_substrate_output(Populated_IHyperlayer,Populated_PHyperlayers,LinkForm,Plasticity,CPP_PIds,CEP_PIds),
			{Output,[IHyperlayer|U_PHyperlayers]}
	end.

	populate_InputHyperlayer([{Coord,PrevO,void}|Substrate],[I|Input],Acc)->
		populate_InputHyperlayer(Substrate,Input,[{Coord,I,void}|Acc]);
	populate_InputHyperlayer([],[],Acc)->
		lists:reverse(Acc).
		
	populate_PHyperlayers(Substrate,CPP_PIds,CEP_PIds,LinkForm,Plasticity)->
		case LinkForm of
			l2l_feedforward ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				populate_PHyperlayers_l2l(IHyperlayer,PHyperlayer,RemSubstrate,CPP_PIds,CEP_PIds,Plasticity,[],[]);
			fully_interconnected ->
				[_IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				I_Neurodes = lists:flatten(Substrate),
				populate_PHyperlayers_fi(I_Neurodes,PHyperlayer,RemSubstrate,CPP_PIds,CEP_PIds,Plasticity,[],[]);
			jordan_recurrent ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				[OHyperlayer|_]=lists:reverse(Substrate),
				I_Neurodes=lists:flatten([IHyperlayer,OHyperlayer]),
				populate_PHyperlayers_l2l(I_Neurodes,PHyperlayer,RemSubstrate,CPP_PIds,CEP_PIds,Plasticity,[],[]);
			neuronself_recurrent ->
				[IHyperlayer,PHyperlayer|RemSubstrate] = Substrate,
				populate_PHyperlayers_nsr(IHyperlayer,PHyperlayer,RemSubstrate,CPP_PIds,CEP_PIds,Plasticity,[],[])
		end.
	
		populate_PHyperlayers_l2l(PrevHyperlayer,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights(PrevHyperlayer,Coord,CPP_PIds,CEP_PIds,[]);
				_ ->
					get_weights(PrevHyperlayer,Coord,CPP_PIds,CEP_PIds,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_l2l(PrevHyperlayer,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_l2l(_PrevHyperlayer,[],[CurHyperlayer|Substrate],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			PrevHyperlayer = lists:reverse(Acc1),
			populate_PHyperlayers_l2l(PrevHyperlayer,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[],[PrevHyperlayer|Acc2]);
		populate_PHyperlayers_l2l(_PrevHyperlayer,[],[],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_PHyperlayers_fi(FlatSubstrate,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights(FlatSubstrate,Coord,CPP_PIds,CEP_PIds,[]);
				_ ->
					get_weights(FlatSubstrate,Coord,CPP_PIds,CEP_PIds,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_fi(FlatSubstrate,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_fi(FlatSubstrate,[],[CurHyperlayer|Substrate],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			populate_PHyperlayers_fi(FlatSubstrate,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[],[lists:reverse(Acc1)|Acc2]);
		populate_PHyperlayers_fi(_FlatSubstrate,[],[],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).

		populate_PHyperlayers_nsr(PrevHyperlayer,[{Coord,PrevO,PrevWeights}|CurHyperlayer],Substrate,CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			NewWeights = case Plasticity of
				none -> 
					get_weights([{Coord,PrevO,PrevWeights}|PrevHyperlayer],Coord,CPP_PIds,CEP_PIds,[]);
				_ ->
					get_weights([{Coord,PrevO,PrevWeights}|PrevHyperlayer],Coord,CPP_PIds,CEP_PIds,[],PrevWeights,PrevO)
			end,
			populate_PHyperlayers_nsr(PrevHyperlayer,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[{Coord,PrevO,NewWeights}|Acc1],Acc2);
		populate_PHyperlayers_nsr(_PrevHyperlayer,[],[CurHyperlayer|Substrate],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			PrevHyperlayer = lists:reverse(Acc1),
			populate_PHyperlayers_nsr(PrevHyperlayer,CurHyperlayer,Substrate,CPP_PIds,CEP_PIds,Plasticity,[],[PrevHyperlayer|Acc2]);
		populate_PHyperlayers_nsr(_PrevHyperlayer,[],[],CPP_PIds,CEP_PIds,Plasticity,Acc1,Acc2)->
			lists:reverse([lists:reverse(Acc1)|Acc2]).
						
			get_weights([{I_Coord,I,_I_Weights}|I_Neurodes],Coord,CPP_PIds,CEP_PIds,Acc)->
				static_fanout(CPP_PIds,I_Coord,Coord),
				U_W=fanin(CEP_PIds,void),
				get_weights(I_Neurodes,Coord,CPP_PIds,CEP_PIds,[U_W|Acc]);
			get_weights([],_Coord,_CPP_PIds,_CEP_PIds,Acc)->
				lists:reverse(Acc).

				static_fanout([CPP_PId|CPP_PIds],I_Coord,Coord)->
					CPP_PId ! {self(),I_Coord,Coord},
					static_fanout(CPP_PIds,I_Coord,Coord);
				static_fanout([],_I_Coord,_Coord)->
					done.
					
				fanin([CEP_PId|CEP_PIds],W)->
					receive
						{CEP_PId,Command,Signal}->
							U_W=substrate:Command(Signal,W)
					end,
					fanin(CEP_PIds,U_W);
				fanin([],W)->
					W.
				
			get_weights([{I_Coord,I,_I_Weights}|I_Neurodes],Coord,CPP_PIds,CEP_PIds,Acc,[W|Weights],O)->
				plasticity_fanout(CPP_PIds,I_Coord,Coord,[I,O,W]),
				U_W=fanin(CEP_PIds,W),
				get_weights(I_Neurodes,Coord,CPP_PIds,CEP_PIds,[U_W|Acc],Weights,O);
			get_weights([],_Coord,CPP_PIds,CEP_PIds,Acc,[],_O)->
				lists:reverse(Acc).

				plasticity_fanout([CPP_PId|CPP_PIds],I_Coord,Coord,IOW)->
					CPP_PId ! {self(),I_Coord,Coord,IOW},
					plasticity_fanout(CPP_PIds,I_Coord,Coord,IOW);
				plasticity_fanout([],_I_Coord,_Coord,_IOW)->
					done.
							
					set_weight(Signal,_WP)->
						[U_W] = Signal,
						functions:sat(U_W,3.1415,-3.1415).
						
					weight_expression(Signal,_WP) ->
						[U_W,Expression]=Signal,
						case Expression > 0 of
							true ->
								functions:sat(U_W,3.1415,-3.1415);
							false ->
								0
						end.
					
					set_abcn(Signal,_WP)->
						[U_W,A,B,C,N] = Signal,
						{functions:sat(U_W,3.1415,-3.1415),abcn,[A,B,C,N]}.
						
					set_iterative(Signal,W)->
						[Delta_Weight] = Signal,
						functions:sat(W + Delta_Weight,3.1415,-3.1415).
						
		calculate_substrate_output(IHyperlayer,PHyperlayer,LinkForm,Plasticity,CPP_PIds,CEP_PIds)->
			case LinkForm of
				l2l_feedforward ->
					calculate_output_std(IHyperlayer,PHyperlayer,Plasticity,CPP_PIds,CEP_PIds,[]);
				fully_interconnected ->
					calculate_output_fi(lists:flatten([IHyperlayer|PHyperlayer]),PHyperlayer,Plasticity,CPP_PIds,CEP_PIds,[]);
				jordan_recurrent ->
					[OHyperlayer|_] = lists:reverse(PHyperlayer,Plasticity),
					calculate_output_std(lists:flatten([IHyperlayer|OHyperlayer]),PHyperlayer,Plasticity,CPP_PIds,CEP_PIds,[]);
				neuronself_recurrent ->
					calculate_output_nsr(IHyperlayer,PHyperlayer,Plasticity,CPP_PIds,CEP_PIds,[])
			end.
			
		calculate_output_std(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPP_PIds,CEP_PIds,Acc)->
			U_CurHyperlayer = [calculate_output(I_Neurodes,Neurode,Plasticity,CPP_PIds,CEP_PIds) || Neurode <- Cur_Hyperlayer],
			calculate_output_std(U_CurHyperlayer,Substrate,Plasticity,CPP_PIds,CEP_PIds,[U_CurHyperlayer|Acc]);
		calculate_output_std(Output_Hyperlayer,[],_Plasticity,CPP_PIds,CEP_PIds,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
			
			calculate_output(I_Neurodes,Neurode,Plasticity,CPP_PIds,CEP_PIds)->
				{Coord,_Prev_O,Weights} = Neurode,
				case Plasticity of
					none ->
						Output=calculate_neurode_output_std(I_Neurodes,Neurode,0),
						{Coord,Output,Weights};
					iterative ->
						Output=calculate_neurode_output_std(I_Neurodes,Neurode,0),
						U_Weights = get_weights(I_Neurodes,Coord,CPP_PIds,CEP_PIds,[],Weights,Output),
						{Coord,Output,U_Weights};
					abcn ->
						Output=calculate_neurode_output_plast(I_Neurodes,Neurode,0),
						update_neurode(I_Neurodes,{Coord,Output,Weights},[])
				end.
			
					calculate_neurode_output_std([{_I_Coord,O,_I_Weights}|I_Neurodes],{Coord,Prev_O,[Weight|Weights]},Acc)->
						calculate_neurode_output_std(I_Neurodes,{Coord,Prev_O,Weights},O*Weight+Acc);
					calculate_neurode_output_std([],{Coord,Prev_O,[]},Acc)->
						functions:tanh(Acc).
					
					calculate_neurode_output_plast([{_I_Coord,O,_I_Weights}|I_Neurodes],{Coord,Prev_O,[{W,_LF,_Parameters}|WPs]},Acc)->
						calculate_neurode_output_plast(I_Neurodes,{Coord,Prev_O,WPs},O*W+Acc);
					calculate_neurode_output_plast([],{Coord,Prev_O,[]},Acc)->
						functions:tanh(Acc).
						
					update_neurode([{_I_Coord,I_O,_I_Weights}|I_Neurodes],{Coord,O,[{W,LF,Parameters}|WPs]},Acc)->
						U_W = substrate:LF(I_O,O,W,Parameters),
						update_neurode(I_Neurodes,{Coord,O,WPs},[{U_W,LF,Parameters}|Acc]);
					update_neurode([],{Coord,O,[]},Acc)->
						{Coord,O,lists:reverse(Acc)}.
			
						abcn(Input,Output,W,[A,B,C,N])->
							Delta_Weight = N*(A*Input*Output + B*Input + C*Output),
							W+Delta_Weight.
			
		calculate_output_fi(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPP_PIds,CEP_PIds,Acc)->
			U_CurHyperlayer = [calculate_output(I_Neurodes,Neurode,Plasticity,CPP_PIds,CEP_PIds) || Neurode <- Cur_Hyperlayer],
			calculate_output_fi([I_Neurodes|U_CurHyperlayer],Substrate,Plasticity,CPP_PIds,CEP_PIds,[U_CurHyperlayer|Acc]);
		calculate_output_fi(Output_Hyperlayer,[],_Plasticity,CPP_PIds,CEP_PIds,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
			
		calculate_output_nsr(I_Neurodes,[Cur_Hyperlayer|Substrate],Plasticity,CPP_PIds,CEP_PIds,Acc)->
			U_CurHyperlayer = [calculate_output([Neurode|I_Neurodes],Neurode,Plasticity,CPP_PIds,CEP_PIds) || Neurode <- Cur_Hyperlayer],
			calculate_output_nsr(U_CurHyperlayer,Substrate,Plasticity,CPP_PIds,CEP_PIds,[U_CurHyperlayer|Acc]);
		calculate_output_nsr(Output_Hyperlayer,[],_Plasticity,CPP_PIds,CEP_PIds,Acc)->
			{[Output || {_Coord,Output,_Weights} <- Output_Hyperlayer],lists:reverse(Acc)}.
