
-module(morphology).
-compile(export_all).
-include("records.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Standard Actuators/Sensors %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSensors(Morphology)->
	% Check if agent_gen has custom sensors stored in ETS
	case ets:whereis(agent_gen_overrides) of
		undefined ->
			% No ETS table, use default behavior
			Sensors = morphology:Morphology(sensors),
			first_sensors(Sensors);
		_ ->
			% Check for custom sensors
			case ets:lookup(agent_gen_overrides, {custom_sensors, Morphology}) of
				[] ->
					% No custom sensors, use default
					Sensors = morphology:Morphology(sensors),
					first_sensors(Sensors);
				[{_, Custom_Sensors}] ->
					% Use custom sensors from agent_gen
					Custom_Sensors
			end
	end.
	%[lists:nth(1,Sensors)].

%first_sensors/1 chooses the sensors a new agent starts with (config:init_sensors/0). A first sensor must carry price
%information, so the account-state and clock sensors are never among them.
%	one     One sensor drawn at random: the original behaviour. A population becomes one family within its first 30
%	        generations, so from then on the whole experiment reads whichever sensor that family's founder drew, and
%	        another sensor arrives only when add_sensor happens to pick it (about one mutation in 400 for a given
%	        sensor). The forward run of 2026-11-12 read four-hour bars and nothing else, and learned nothing; the one
%	        run that did learn was the one where the indicators reached every agent (REVIEW.md 8.17).
%	series  Every sensor answered from the shared price arrays: each fx_PLT list and fx_IND. The first neuron reads
%	        one of them or all of them (genotype:construct_InitialNeurons/6), and add_sensorlink connects the others.
%	        The raw sensors (fx_PLI, fx_PCI), the clock and the account state still arrive through add_sensor.
%	        Falls back to one when no series sensor is switched on.
%	series_clock  The same and the clock (REVIEW.md 8.22: EUR/USD has a time-of-day pattern, and the clock reached only a
%	        third of the agents through add_sensor).
%	With config:position_sensor/0 on, the position sensor (fx_Position) is added to whichever of these is chosen: an agent
%	that reads its own position has it from the start (REVIEW.md 8.23).
first_sensors(Sensors)->
	Price = [S || S <- Sensors, not lists:member(S#sensor.name,[fx_Internals,fx_Clock,fx_Position])],
	Series = [S || S <- Price, lists:member(S#sensor.name,[fx_PLT,fx_IND])],
	Position = [S || S <- Sensors, S#sensor.name == fx_Position],
	First = case {config:init_sensors(),Series} of
		{series,[_|_]} -> Series;
		{series_clock,[_|_]} -> Series ++ [S || S <- Sensors, S#sensor.name == fx_Clock];
		_ -> [lists:nth(rand:uniform(length(Price)),Price)]
	end,
	First ++ Position.

get_InitActuators(Morphology)->
	% Check if agent_gen has custom actuators stored in ETS
	case ets:whereis(agent_gen_overrides) of
		undefined ->
			% No ETS table, use default behavior
			Actuators = morphology:Morphology(actuators),
			[lists:nth(rand:uniform(length(Actuators)),Actuators)];
		_ ->
			% Check for custom actuators
			case ets:lookup(agent_gen_overrides, {custom_actuators, Morphology}) of
				[] ->
					% No custom actuators, use default
					Actuators = morphology:Morphology(actuators),
					[lists:nth(rand:uniform(length(Actuators)),Actuators)];
				[{_, Custom_Actuators}] ->
					% Use custom actuators from agent_gen
					Custom_Actuators
			end
	end.
	%[lists:nth(1,Actuators)].

get_Sensors(Morphology)->
	morphology:Morphology(sensors).

get_Actuators(Morphology)->
	morphology:Morphology(actuators).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Get Init Substrate_CPPs/Substrate_CEPs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_InitSubstrateCPPs(Dimensions,Plasticity)->
	Substrate_CPPs = get_SubstrateCPPs(Dimensions,Plasticity),
	[lists:nth(rand:uniform(length(Substrate_CPPs)),Substrate_CPPs)].
	%[lists:nth(1,Substrate_CPPs)].

get_InitSubstrateCEPs(Dimensions,Plasticity)->
	Substrate_CEPs = get_SubstrateCEPs(Dimensions,Plasticity),
	[lists:nth(rand:uniform(length(Substrate_CEPs)),Substrate_CEPs)].
	%[lists:nth(1,Substrate_CEPs)].

get_SubstrateCPPs(Dimensions,Plasticity)->
	%io:format("Dimensions:~p, Plasticity:~p~n",[Dimensions,Plasticity]),
	if
		(Plasticity == iterative) or (Plasticity == abcn) or (Plasticity == modular_none) or (Plasticity == hebbian) or (Plasticity == ojas) ->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2+3},%{cartesian,Dimensions*2+3},
				#sensor{name=centripital_distances,type=substrate,vl=2+3},%{centripital_distances,2+3},
				#sensor{name=cartesian_distance,type=substrate,vl=1+3},%{cartesian_distance,1+3},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions+3},%{cartesian_GaussedCoordDiffs,Dimensions+3}
				#sensor{name=iow,type=substrate,vl=3}%{iow,3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2+3}];%[{polar,Dimensions*2+3}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2+3}];%[{spherical,Dimensions*2+3}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt);
		(Plasticity == none) ->
			Std=[
				#sensor{name=cartesian,type=substrate,vl=Dimensions*2},%{cartesian,Dimensions*2},
				#sensor{name=centripital_distances,type=substrate,vl=2},%{centripital_distances,2},
				#sensor{name=cartesian_distance,type=substrate,vl=1},%{cartesian_distance,1},
				#sensor{name=cartesian_CoordDiffs,type=substrate,vl=Dimensions},%{cartesian_CoordDiffs,Dimensions+3}
				#sensor{name=cartesian_GaussedCoordDiffs,type=substrate,vl=Dimensions}%{cartesian_GaussedCoordDiffs,Dimensions+3}
			],
			Adt=case Dimensions of
				2 ->
					[#sensor{name=polar,type=substrate,vl=Dimensions*2}];%[{polar,Dimensions*2}];
				3 ->
					[#sensor{name=spherical,type=substrate,vl=Dimensions*2}];%[{spherical,Dimensions*2}]
				_ -> 
					[]
			end,
			lists:append(Std,Adt)
	end.

get_SubstrateCEPs(Dimensions,Plasticity)->
	case Plasticity of
		iterative ->
			[#actuator{name=delta_weight,type=substrate,vl=1}]; %[{delta_weight,1}];
		abcn ->
			[#actuator{name=set_abcn,type=substrate,vl=5}]; %[{abcn,4}];
		none ->
			[#actuator{name=set_weight,type=substrate,vl=1}]; %[{weight,1}]
		modular_none ->
			[#actuator{name=weight_expression,type=substrate,vl=2}]; %[{weight_conexpr,2}]
		hebbian ->
			[#actuator{name=set_weight,type=substrate,vl=1}]; %[{weight,1}]
		ojas ->
			[#actuator{name=set_weight,type=substrate,vl=1}] %[{weight,1}]
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MORPHOLOGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	This sets up the link so that when the neural network outputs a value, it calls fx_Trade to send that trade to the Forex simulator.
forex_trader(actuators)->
	[
		#actuator{name=fx_Trade,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[]}
	];
% Defines the sensors, which provide input data to the agent's neural network.
% 1. PLI Sensor — Price List Input
% 	Provides a sliding window of raw price data with resolution HRes = 10
% 	Format is simple numerical (no geometric structure)
% 	Parameters: window size (HRes) and which price to use (close)
% 2. PCI Sensor — Price Chart Input
% 	Provides reconstructed chart images with horizontal resolution HRes = 50 and vertical VRes = 20
% 	format = symmetric indicates spatial (geometric) structure for substrate encoding
% 	vl = 50 * 20 = 1000 total inputs for this chart
forex_trader(sensors)->
	PLI_Sensors=  [#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-config:pli_resolutions()],
	PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-config:pci_horizontal_resolutions(), VRes<-config:pci_vertical_resolutions()],
	%The account state: position and open profit (fx_Internals), or with config:position_sensor/0 the position only
	%(fx_Position), which a new agent then also starts with (first_sensors/1).
	InternalSensors = case config:position_sensor() of
		true -> [#sensor{name=fx_Position,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[position]}];
		_ -> [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=config:internal_sensor_dimensions(),parameters=[config:internal_sensor_dimensions()]}]%[Long|Short|Void],Value
	end,
	%Sensors answered from the shared price arrays (fx_series.erl): price lists on coarser bars, indicators, a clock.
	%They are offered next to the raw ones; add_sensor lets evolution pick. Empty unless switched on in config.
	%The price lists carry their encoding (config:plt_encoding/0) in their parameters: [TF,N] or [TF,N,vol].
	SeriesSensors =
		[#sensor{name=fx_PLT,type=standard,scape={private,fx_sim},format=no_geo,vl=N,parameters=plt_parameters(TF,N)} || {TF,N}<-config:plt_specs()] ++
		[#sensor{name=fx_IND,type=standard,scape={private,fx_sim},format=no_geo,vl=fx_series:vl(ind),parameters=[ind]} || config:indicator_sensor() == true] ++
		[#sensor{name=fx_Clock,type=standard,scape={private,fx_sim},format=no_geo,vl=fx_series:vl(ClockSpec),parameters=ClockParameters} || config:clock_sensor() == true, {ClockSpec,ClockParameters} <- [clock_spec()]],
	%PLI_Sensors.
	%PCI_Sensors. %Inital state [BASE]
	PLI_Sensors++PCI_Sensors++SeriesSensors++InternalSensors.
	%PCI_Sensors++PLI_Sensors++InternalSensors.
	%PLI_Sensors.%++InternalSensors. %qq
	%PLI_Sensors++InternalSensors. % qq - Enable internal sensors for more mutation options
	%InternalSensors.

% New morphology specifically optimized for 1-minute forsex trading
% Uses smaller time windows and higher resolution for quick decision making
forex_trader_1m(actuators)->
	[
		#actuator{name=fx_Trade,type=standard,scape={private,fx_sim},format=no_geo,vl=1,parameters=[]}
	];

forex_trader_1m(sensors)->
	% Smaller time windows for 1-minute data
	PLI_Sensors=[#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-config:pli_1m_resolutions()], % Multiple resolutions
	PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-config:pci_1m_horizontal_resolutions(), VRes<-config:pci_1m_vertical_resolutions()], % Smaller charts for faster processing
	InternalSensors = [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=config:internal_sensor_dimensions(),parameters=[config:internal_sensor_dimensions()]}],%[Long|Short|Void],Value
	PLI_Sensors++PCI_Sensors++InternalSensors. % Include all sensor types for 1-minute trading
	
generate_id() ->
	{MegaSeconds,Seconds,MicroSeconds} = now(), 
	1/(MegaSeconds*1000000 + Seconds + MicroSeconds/1000000).

plt_parameters(TF,N)->
	case config:plt_encoding() of
		vol -> [TF,N,vol];
		_ -> [TF,N]
	end.

%clock_spec/0: what the clock sensor asks the simulator for, and the parameters stored with the sensor.
clock_spec()->
	case config:clock_harmonics() of
		H when is_integer(H), H > 1 -> {{clock,H},[clock,H]};
		_ -> {clock,[clock]}
	end.
