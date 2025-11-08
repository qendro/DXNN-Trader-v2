# Dimensionality Fix Implementation Plan

## Problem Statement

When adding sensors to substrate-encoded agents, dimensional conflicts occur:
- **PLI Agent**: Starts with 3D substrate (`[1, 1, 5]`) for 1D sensors
- **PCI Agent**: Starts with 4D substrate (`[1, 1, 5, 5]`) for 2D sensors
- **Adding PCI to PLI**: Fails because 2D sensor can't fit in 3D substrate
- **Adding PLI to PCI**: Works because 1D sensor can be extruded to fit 4D substrate

## Solution: Dynamic Dimension Recalculation

Recalculate substrate dimensions every time a sensor is added, expanding the substrate only when necessary.

## Implementation Plan

### Phase 1: Configuration Updates

#### 1.1 Add Configurable Parameters
**File**: `config.erl`

```erlang
%% === Substrate Configuration ===
substrate_depth() -> 1.           % Number of hidden layers
substrate_density() -> 5.        % Resolution per spatial dimension
```

#### 1.2 Update Existing Hardcoded Values
**File**: `genotype.erl` (line 83-84)

```erlang
% Replace:
Density = 5,
Depth = 1,

% With:
Density = config:substrate_density(),
Depth = config:substrate_depth(),
```

### Phase 2: Helper Functions

#### 2.1 Create Helper Functions
**File**: `genotype.erl`

```erlang
%% Helper function to get current sensors
get_current_sensors(S_Ids) ->
    [read({sensor,S_Id}) || S_Id <- S_Ids].

%% Helper function to get current actuators  
get_current_actuators(A_Ids) ->
    [read({actuator,A_Id}) || A_Id <- A_Ids].

%% Helper function to extract density from substrate
extract_substrate_density(Densities) ->
    case Densities of
        [_Depth, 1|Rest] ->
            case Rest of
                [] -> config:substrate_density();
                [D|_] -> D
            end;
        _ -> config:substrate_density()
    end.

%% Helper function to extract depth from substrate
extract_substrate_depth(Densities) ->
    case Densities of
        [Depth|_] -> Depth;
        _ -> config:substrate_depth()
    end.
```

### Phase 3: Substrate Dimension Update Functions

#### 3.1 Create Substrate Expansion Function
**File**: `genotype.erl`

```erlang
%% Expand substrate dimensions when needed
expand_substrate_dimensions(Agent_Id, New_Dimensions) ->
    Agent = read({agent,Agent_Id}),
    Substrate_Id = Agent#agent.substrate_id,
    Substrate = read({substrate,Substrate_Id}),
    
    % Extract existing parameters
    Current_Densities = Substrate#substrate.densities,
    Current_Depth = extract_substrate_depth(Current_Densities),
    Current_Density = extract_substrate_density(Current_Densities),
    
    % Calculate new densities
    New_Densities = [Current_Depth,1|lists:duplicate(New_Dimensions-2,Current_Density)],
    
    % Get additional CPPs/CEPs needed for new dimensions
    SPlasticity = Substrate#substrate.plasticity,
    Morphology = (Agent#agent.constraint)#constraint.morphology,
    Cx_Id = Agent#agent.cx_id,
    Generation = Agent#agent.generation,
    
    % Calculate what additional CPPs/CEPs are needed
    Current_Dimensions = length(Current_Densities),
    Additional_Dimensions = New_Dimensions - Current_Dimensions,
    
    if Additional_Dimensions > 0 ->
        % Create additional CPPs/CEPs for new dimensions
        Additional_CPPs = morphology:get_InitSubstrateCPPs(New_Dimensions,SPlasticity) -- 
                         morphology:get_InitSubstrateCPPs(Current_Dimensions,SPlasticity),
        Additional_CEPs = morphology:get_InitSubstrateCEPs(New_Dimensions,SPlasticity) -- 
                         morphology:get_InitSubstrateCEPs(Current_Dimensions,SPlasticity),
        
        % Create and write new CPPs/CEPs
        New_CPPs = [CPP#sensor{id={{-1,generate_UniqueId()},sensor},cx_id=Cx_Id,generation=Generation}|| CPP<- Additional_CPPs],
        New_CEPs = [CEP#actuator{id={{1,generate_UniqueId()},actuator},cx_id=Cx_Id,generation=Generation}||CEP<- Additional_CEPs],
        
        [write(CPP) || CPP <- New_CPPs],
        [write(CEP) || CEP <- New_CEPs],
        
        % Update substrate record
        Updated_Substrate = Substrate#substrate{
            densities = New_Densities,
            cpp_ids = Substrate#substrate.cpp_ids ++ [CPP#sensor.id || CPP<-New_CPPs],
            cep_ids = Substrate#substrate.cep_ids ++ [CEP#actuator.id || CEP<-New_CEPs]
        },
        write(Updated_Substrate),
        
        % Update cortex neuron_ids to include new CPPs/CEPs
        Cx = read({cortex,Cx_Id}),
        N_Ids = construct_InitialNeuroLayer(Cx_Id,Generation,Agent#agent.constraint,New_CPPs,New_CEPs,[],[]),
        Updated_Cx = Cx#cortex{neuron_ids=N_Ids},
        write(Updated_Cx);
    true ->
        % No expansion needed
        ok
    end.
```

### Phase 4: Modify add_sensor Function

#### 4.1 Update add_sensor in genome_mutator.erl
**File**: `genome_mutator.erl` (around line 890)

```erlang
add_sensor(Agent_Id)->
    Agent = genotype:read({agent,Agent_Id}),
    Cx_Id = Agent#agent.cx_id,
    Cx = genotype:read({cortex,Cx_Id}),
    S_Ids = Cx#cortex.sensor_ids,
    SpeCon = Agent#agent.constraint,
    Morphology = SpeCon#constraint.morphology,
    
    case morphology:get_Sensors(Morphology)--[(genotype:read({sensor,S_Id}))#sensor{id=undefined,cx_id=undefined,fanout_ids=[],generation=undefined} || S_Id<-S_Ids] of
        [] ->
            exit("********ERROR:add_sensor(Agent_Id):: NN system is already using all available sensors");
        Available_Sensors ->
            NewS_Id = {{-1,genotype:generate_UniqueId()},sensor},
            NewSensor=(lists:nth(random:uniform(length(Available_Sensors)),Available_Sensors))#sensor{id=NewS_Id,cx_id=Cx_Id},
            EvoHist = Agent#agent.evo_hist,
            
            case Agent#agent.encoding_type of
                neural->
                    genotype:write(NewSensor),
                    N_Ids = Cx#cortex.neuron_ids,
                    N_Id = lists:nth(random:uniform(length(N_Ids)),N_Ids),
                    link_FromElementToElement(Agent_Id,NewS_Id,N_Id),
                    U_EvoHist = [{add_sensor,NewS_Id,N_Id}|EvoHist];
                substrate ->
                    % NEW: Check if we need to expand substrate dimensions
                    Updated_Sensors = [NewSensor|genotype:get_current_sensors(S_Ids)],
                    Updated_Actuators = genotype:get_current_actuators(Cx#cortex.actuator_ids),
                    Required_Dimensions = genotype:calculate_OptimalSubstrateDimension(Updated_Sensors, Updated_Actuators),
                    
                    % Expand substrate if needed
                    genotype:expand_substrate_dimensions(Agent_Id, Required_Dimensions),
                    
                    Substrate_Id = Agent#agent.substrate_id,
                    genotype:write(NewSensor#sensor{fanout_ids=[Substrate_Id]}),
                    U_EvoHist = [{add_sensor,NewS_Id,Substrate_Id}|EvoHist]
            end,
            
            U_Cx = Cx#cortex{sensor_ids=[NewS_Id|S_Ids]},
            genotype:write(U_Cx),
            genotype:write(Agent#agent{evo_hist=U_EvoHist})
    end.
```

### Phase 5: Testing Strategy

#### 5.1 Test Cases
1. **PLI Agent → Add PCI**: Should expand from 3D to 4D substrate
2. **PCI Agent → Add PLI**: Should remain 4D substrate (no expansion needed)
3. **Mixed Agent**: Should handle multiple sensor additions correctly
4. **Edge Cases**: Test with maximum sensor combinations

#### 5.2 Test Implementation
**File**: `test_dimensionality_fix.erl`

```erlang
-module(test_dimensionality_fix).
-compile(export_all).

test_pli_to_pci() ->
    % Create PLI agent
    % Add PCI sensor
    % Verify substrate expanded to 4D
    ok.

test_pci_to_pli() ->
    % Create PCI agent  
    % Add PLI sensor
    % Verify substrate remains 4D
    ok.

test_multiple_additions() ->
    % Create agent with one sensor type
    % Add multiple sensors of different types
    % Verify substrate handles all additions
    ok.
```

### Phase 6: Rollback Plan

#### 6.1 Backup Strategy
- Create backup of original `genome_mutator.erl` and `genotype.erl`
- Test changes in isolated environment first
- Implement feature flag to enable/disable new behavior

#### 6.2 Rollback Implementation
```erlang
%% In config.erl
enable_dynamic_dimensions() -> true.  % Set to false to disable

%% In add_sensor function
case config:enable_dynamic_dimensions() of
    true ->
        % Use new dynamic dimension calculation
        genotype:expand_substrate_dimensions(Agent_Id, Required_Dimensions);
    false ->
        % Use original behavior
        ok
end.
```

## Implementation Order

1. **Phase 1**: Configuration updates (low risk)
2. **Phase 2**: Helper functions (low risk, isolated)
3. **Phase 3**: Substrate expansion functions (medium risk)
4. **Phase 4**: Modify add_sensor (high risk, core functionality)
5. **Phase 5**: Testing (validation)
6. **Phase 6**: Rollback mechanisms (safety)

## Benefits

1. **Solves Core Problem**: Eliminates dimensional conflicts
2. **Efficient**: Only expands when necessary
3. **Evolutionary**: Allows natural growth from simple to complex
4. **Maintainable**: Uses configurable parameters
5. **Backward Compatible**: Doesn't break existing functionality

## Risks

1. **Complexity**: Adds complexity to sensor addition process
2. **Performance**: Substrate expansion has computational cost
3. **Testing**: Requires comprehensive testing of edge cases
4. **Memory**: Expanded substrates use more memory

## Success Criteria

- [ ] PLI agents can successfully add PCI sensors
- [ ] PCI agents can successfully add PLI sensors  
- [ ] No dimensional conflicts during evolution
- [ ] Performance impact is acceptable
- [ ] All existing functionality preserved
- [ ] Comprehensive test coverage

## Future Enhancements

1. **Smart Expansion**: Only expand specific dimensions that are needed
2. **Compression**: Shrink substrate when sensors are removed
3. **Adaptive Density**: Adjust density based on sensor complexity
4. **Caching**: Cache dimension calculations for performance
