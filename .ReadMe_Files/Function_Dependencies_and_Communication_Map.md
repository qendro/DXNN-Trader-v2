# DXNN Function Dependencies and Communication Map

This document provides a comprehensive analysis of function dependencies and inter-module communication patterns in the DXNN (Distributed eXtended Neural Network) system.

## Module Overview

The DXNN system consists of 28 Erlang modules that implement a neuroevolutionary trading system for forex markets. Each module has specific responsibilities and communication patterns.

## Function Dependencies by Module

### actuator.erl
**Functions:**
- `gen/2` - Spawns actuator process
- `prep/1` - Initializes actuator state
- `loop/8` - Main actuator loop
- `pts/3` - Print to screen actuator
- `fx_Trade/4` - Forex trading actuator

**Dependencies:**
- **Calls:** `config:primary_currency_pair/0`, `functions:trinary/1`
- **Receives messages from:** ExoSelf process, neurons
- **Sends messages to:** Scape process, cortex process
- **Message patterns:** `{From_PId,forward,Input}`, `{ExoSelf_PId,terminate}`, `{self(),sync,Fitness,EndFlag}`

### benchmarker.erl
**Functions:**
- `start/1` - Starts benchmarking experiment
- `continue/1` - Continues existing experiment
- `prep/1` - Prepares benchmarker state
- `loop/2` - Main benchmarker loop
- `report/2` - Generates experiment reports
- `prepare_Graphs/1` - Prepares graph data
- `write_Graphs/2` - Writes graph files
- `checkpoint_and_exit/0` - AWS spot instance checkpoint
- `maybe_restore/0` - Restores from checkpoint
- `completion_signal/0` - Signals training completion

**Dependencies:**
- **Calls:** `config:*` functions, `population_monitor:prep_PopState/2`, `genotype:write/1`, `genotype:dirty_read/1`, `functions:avg/1`, `functions:std/1`
- **Receives messages from:** Population monitor
- **Sends messages to:** Population monitor
- **Message patterns:** `{P_Id,completed,Trace}`, `terminate`

### config.erl
**Functions:**
- All configuration getter functions (no dependencies, pure configuration)

**Dependencies:**
- **Calls:** None (configuration module)
- **Receives messages from:** None
- **Sends messages to:** None

### cortex.erl
**Functions:**
- `gen/2` - Spawns cortex process
- `prep/1` - Initializes cortex state
- `loop/9` - Main cortex loop

**Dependencies:**
- **Calls:** `random:seed/3`, `now/0`
- **Receives messages from:** Actuators, ExoSelf
- **Sends messages to:** Sensors, neurons, actuators, ExoSelf
- **Message patterns:** `{APId,sync,Fitness,EndFlag}`, `{ExoSelf_PId,reactivate}`, `{ExoSelf_PId,terminate}`

### exoself.erl
**Functions:**
- `start/3` - Starts agent process
- `prep/3` - Prepares agent state
- `loop/2` - Main agent loop
- `spawn_CerebralUnits/3` - Spawns neural components
- `spawn_Scapes/4` - Spawns private scapes
- `link_Sensors/3`, `link_Actuators/3`, `link_Neurons/3` - Links components
- `backup_genotype/2` - Backs up neural weights
- `terminate_phenotype/8` - Terminates neural network

**Dependencies:**
- **Calls:** `genotype:dirty_read/1`, `tuning_duration:*`, `tuning_selection:*`, `scape:gen/2`, `cortex:gen/2`, `sensor:gen/2`, `actuator:gen/2`, `neuron:gen/2`, `substrate:gen/2`, `substrate_cpp:gen/2`, `substrate_cep:gen/2`
- **Receives messages from:** Cortex, population monitor
- **Sends messages to:** All neural components, population monitor
- **Message patterns:** `{Cx_PId,evaluation_completed,Fitness,Cycles,Time,GoalReachedFlag}`, `{self(),weight_backup}`, `{self(),weight_restore}`, `{self(),weight_perturb,Spread}`

### fitness_postprocessor.erl
**Functions:**
- `none/1` - No fitness postprocessing
- `size_proportional/1` - Size-proportional fitness adjustment

**Dependencies:**
- **Calls:** `math:pow/2`
- **Receives messages from:** None (pure function module)
- **Sends messages to:** None

### functions.erl
**Functions:**
- Mathematical and utility functions (saturation, scaling, activation functions, etc.)

**Dependencies:**
- **Calls:** `math:*` functions, `random:uniform/0`
- **Receives messages from:** None (pure function module)
- **Sends messages to:** None

### fx.erl
**Functions:**
- `init/0` - Initializes FX tables
- `start/0` - Starts FX system
- `loop/2` - Main FX loop
- `sim/1` - FX simulation entry point
- `sim/3` - FX simulation loop
- `init_state/5` - Initializes simulation state
- `sense/2` - Sensor data processing
- `make_trade/3` - Executes trades
- `update_account/2` - Updates account state
- Various data processing functions

**Dependencies:**
- **Calls:** `config:*` functions, `ets:*` functions, `progress_logger:*`, `file:*`
- **Receives messages from:** Sensors, actuators, ExoSelf
- **Sends messages to:** Sensors, actuators
- **Message patterns:** `{From,sense,TableName,Feature,Parameters,Start,Finish}`, `{From,trade,TableName,TradeSignal}`, `{From,sense,internals,Parameters}`

### genome_mutator.erl
**Functions:**
- `mutate/1` - Main mutation function
- `mutate_weights/1` - Weight mutation
- `add_bias/1`, `remove_bias/1` - Bias manipulation
- `mutate_af/1` - Activation function mutation
- `mutate_pf/1` - Plasticity function mutation
- Various topological mutation operators

**Dependencies:**
- **Calls:** `genotype:read/1`, `genotype:write/1`, `random:uniform/0`, `plasticity:*`, `tot_topological_mutations:*`
- **Receives messages from:** None (transaction-based)
- **Sends messages to:** None
- **Database operations:** Mnesia transactions

### genotype.erl
**Functions:**
- `construct_Agent/3` - Creates new agent
- `construct_Cortex/6` - Creates cortex
- `construct_InitialNeuroLayer/7` - Creates initial neurons
- `update_fingerprint/1` - Updates agent fingerprint
- `clone_Agent/2` - Clones existing agent
- `delete_Agent/1` - Deletes agent
- Database I/O functions

**Dependencies:**
- **Calls:** `morphology:*`, `plasticity:*`, `random:uniform/0`, `mnesia:*`
- **Receives messages from:** None (database module)
- **Sends messages to:** None
- **Database operations:** Mnesia read/write operations

### genotype_utils.erl
**Functions:**
- `print_best_genotype/1` - Prints best agent
- `list_all_agents/1` - Lists all agents
- `get_agent_stats/1` - Gets population statistics

**Dependencies:**
- **Calls:** `genotype:*`, `mnesia:*`
- **Receives messages from:** None (utility module)
- **Sends messages to:** None

### morphology.erl
**Functions:**
- `get_InitSensors/1`, `get_InitActuators/1` - Gets initial components
- `forex_trader/1` - Forex trader morphology
- `forex_trader_1m/1` - 1-minute forex trader morphology
- Substrate component functions

**Dependencies:**
- **Calls:** `config:*` functions
- **Receives messages from:** None (configuration module)
- **Sends messages to:** None

### neuron.erl
**Functions:**
- `gen/2` - Spawns neuron process
- `prep/1` - Initializes neuron state
- `loop/6` - Main neuron loop
- `perturb_IPIdPs/2` - Weight perturbation
- `fanout/2` - Signal fanout
- `flush_buffer/0` - Buffer flushing

**Dependencies:**
- **Calls:** `signal_aggregator:*`, `functions:*`, `plasticity:*`, `random:uniform/0`
- **Receives messages from:** Sensors, other neurons, ExoSelf
- **Sends messages to:** Other neurons, actuators, ExoSelf
- **Message patterns:** `{SI_PId,forward,Input}`, `{ExoSelf_PId,weight_backup}`, `{ExoSelf_PId,weight_restore}`, `{ExoSelf_PId,weight_perturb,Spread}`

### plasticity.erl
**Functions:**
- Various plasticity functions (`none/3`, `hebbian/4`, `ojas/4`, `self_modulation*/4`, `neuromodulation/4`)
- Parameter generation functions

**Dependencies:**
- **Calls:** `genotype:read/1`, `functions:*`, `random:uniform/0`, `math:*`
- **Receives messages from:** None (function module)
- **Sends messages to:** None

### polis.erl
**Functions:**
- `start/0` - Starts polis system
- `create/0` - Creates database schema
- `reset/0` - Resets database
- Gen_server callbacks

**Dependencies:**
- **Calls:** `mnesia:*`, `scape:start_link/1`
- **Receives messages from:** Clients via gen_server
- **Sends messages to:** Scapes
- **Message patterns:** `{get_scape,Type}`, `{stop,normal}`, `{stop,shutdown}`

### population_monitor.erl
**Functions:**
- `start/1` - Starts population monitor
- `init_population/2` - Initializes population
- `mutate_population/4` - Mutates population
- `create_MutantAgentCopy/1` - Creates mutant copies
- Gen_server callbacks

**Dependencies:**
- **Calls:** `genotype:*`, `exoself:start/3`, `selection_algorithm:*`, `fitness_postprocessor:*`, `genome_mutator:*`, `progress_logger:*`
- **Receives messages from:** Agents, benchmarker
- **Sends messages to:** Agents, benchmarker
- **Message patterns:** `{Agent_Id,terminated,Fitness}`, `{From,evaluations,Specie_Id,AEA,AgentCycleAcc,AgentTimeAcc}`

### progress_logger.erl
**Functions:**
- `start/0` - Starts logger
- `mark_launch/0` - Marks program launch
- `set_iteration/1` - Sets current iteration
- `inc_done_eval/0` - Increments evaluation counter
- Various status functions

**Dependencies:**
- **Calls:** `ets:*`, `file:*`, `calendar:*`
- **Receives messages from:** None (ETS-based)
- **Sends messages to:** None

### scape.erl
**Functions:**
- `gen/2` - Spawns scape process
- `prep/1` - Initializes scape
- `fx_sim/1` - FX simulation scape

**Dependencies:**
- **Calls:** `fx:sim/1`
- **Receives messages from:** ExoSelf
- **Sends messages to:** FX simulator
- **Message patterns:** `{ExoSelf_PId,Name}`

### selection_algorithm.erl
**Functions:**
- `competition/3` - Competition selection
- `top3/3` - Top 3 selection
- Various helper functions

**Dependencies:**
- **Calls:** `genotype:delete_Agent/1`, `population_monitor:create_MutantAgentCopy/1`, `random:uniform/0`
- **Receives messages from:** None (function module)
- **Sends messages to:** None

### sensor.erl
**Functions:**
- `gen/2` - Spawns sensor process
- `prep/1` - Initializes sensor state
- `loop/8` - Main sensor loop
- `fx_PCI/4` - Price chart input sensor
- `fx_PLI/4` - Price list input sensor
- `fx_Internals/4` - Internal state sensor

**Dependencies:**
- **Calls:** `config:*` functions, `random:uniform/0`
- **Receives messages from:** Cortex, Scape
- **Sends messages to:** Neurons, Scape
- **Message patterns:** `{Cx_PId,sync}`, `{ExoSelf_PId,terminate}`, `{_From,Result}`

### signal_aggregator.erl
**Functions:**
- `dot_product/2` - Dot product aggregation
- `diff_product/2` - Difference product aggregation
- `mult_product/2` - Multiplication product aggregation

**Dependencies:**
- **Calls:** None (pure function module)
- **Receives messages from:** None
- **Sends messages to:** None

### substrate.erl
**Functions:**
- `gen/2` - Spawns substrate process
- `prep/1` - Initializes substrate
- `loop/4` - Main substrate loop
- `create_substrate/4` - Creates substrate structure
- Various substrate calculation functions

**Dependencies:**
- **Calls:** `functions:*`, `random:uniform/0`
- **Receives messages from:** Sensors, CPPs, CEPs, ExoSelf
- **Sends messages to:** Actuators, CPPs, CEPs, ExoSelf
- **Message patterns:** `{SPId,forward,Sensory_Signal}`, `{ExoSelf,reset_substrate}`, `{ExoSelf,backup_substrate}`

### substrate_cpp.erl
**Functions:**
- `gen/2` - Spawns substrate CPP process
- `prep/1` - Initializes CPP state
- `loop/7` - Main CPP loop

**Dependencies:**
- **Calls:** `functions:*`
- **Receives messages from:** Substrate, ExoSelf
- **Sends messages to:** Neurons
- **Message patterns:** `{Substrate_PId,Presynaptic_Coords,Postsynaptic_Coords}`, `{ExoSelf_PId,terminate}`

### substrate_cep.erl
**Functions:**
- `gen/2` - Spawns substrate CEP process
- `prep/1` - Initializes CEP state
- `loop/8` - Main CEP loop
- `set_weight/3`, `set_abcn/3`, `delta_weight/3` - Weight setting functions

**Dependencies:**
- **Calls:** `functions:*`
- **Receives messages from:** Neurons, ExoSelf
- **Sends messages to:** Substrate
- **Message patterns:** `{From_PId,forward,Input}`, `{ExoSelf_PId,terminate}`

### tot_topological_mutations.erl
**Functions:**
- `ncount_exponential/2` - Exponential mutation count
- `ncount_linear/2` - Linear mutation count

**Dependencies:**
- **Calls:** `genotype:read/1`, `math:pow/2`, `random:uniform/1`
- **Receives messages from:** None (function module)
- **Sends messages to:** None

### tuning_duration.erl
**Functions:**
- `const/3` - Constant duration
- `wsize_proportional/3` - Weight-size proportional duration
- `nsize_proportional/3` - Neuron-size proportional duration

**Dependencies:**
- **Calls:** `genotype:dirty_read/1`, `functions:sat/3`, `math:pow/2`
- **Receives messages from:** None (function module)
- **Sends messages to:** None

### tuning_selection.erl
**Functions:**
- `dynamic/4` - Dynamic neuron selection
- `dynamic_random/4` - Random dynamic selection
- `active/4`, `current/4`, `all/4` - Various selection strategies
- `extract_CurGenNIdPs/6` - Extracts current generation neurons

**Dependencies:**
- **Calls:** `genotype:dirty_read/1`, `math:sqrt/1`, `math:pow/2`, `random:uniform/0`
- **Receives messages from:** None (function module)
- **Sends messages to:** None

## Communication Patterns

### Process Hierarchy
```
ExoSelf (Agent)
├── Cortex
├── Sensors
├── Neurons
├── Actuators
├── Substrate (optional)
│   ├── Substrate_CPPs
│   └── Substrate_CEPs
└── Private Scapes
```

### Message Flow Patterns

#### Training Cycle
1. **ExoSelf** → **Cortex**: `{self(),reactivate}`
2. **Cortex** → **Sensors**: `{self(),sync}`
3. **Sensors** → **Scape**: `{self(),sense,...}`
4. **Scape** → **Sensors**: `{self(),Result}`
5. **Sensors** → **Neurons**: `{self(),forward,SensoryVector}`
6. **Neurons** → **Neurons**: `{self(),forward,Output}`
7. **Neurons** → **Actuators**: `{self(),forward,Output}`
8. **Actuators** → **Scape**: `{self(),trade,...}`
9. **Scape** → **Actuators**: `{self(),Fitness,HaltFlag}`
10. **Actuators** → **Cortex**: `{self(),sync,Fitness,EndFlag}`
11. **Cortex** → **ExoSelf**: `{self(),evaluation_completed,...}`

#### Evolution Cycle
1. **Population Monitor** spawns **ExoSelf** processes
2. **ExoSelf** processes complete evaluations
3. **ExoSelf** → **Population Monitor**: `{Agent_Id,terminated,Fitness}`
4. **Population Monitor** calls **Selection Algorithm**
5. **Population Monitor** calls **Genome Mutator**
6. **Population Monitor** spawns new generation

#### Weight Tuning
1. **ExoSelf** → **Neurons**: `{self(),weight_perturb,Spread}`
2. **ExoSelf** → **Neurons**: `{self(),weight_backup}`
3. **ExoSelf** → **Neurons**: `{self(),weight_restore}`

### Database Communication
- **Genotype** module handles all Mnesia database operations
- **Population Monitor**, **Genome Mutator**, and **ExoSelf** use genotype functions
- All database operations are transactional

### Configuration Dependencies
- **Config** module provides centralized configuration
- Most modules call config functions for parameters
- No circular dependencies in configuration

## Key Communication Interfaces

### ExoSelf ↔ Neural Components
- Initialization messages with component parameters
- Control messages (backup, restore, perturb, terminate)
- Status messages (ready, evaluation_completed)

### Neural Network Signal Flow
- Forward propagation through sensors → neurons → actuators
- Synchronous message passing between components
- Cortex coordinates the sense-think-act cycle

### Population ↔ Agents
- Population monitor spawns and manages agent lifecycles
- Agents report fitness and termination to population monitor
- Population monitor triggers evolution and mutation

### Database ↔ System
- Genotype module abstracts all database operations
- Transactional consistency for mutations and evolution
- Persistent storage of neural network structures and parameters

This comprehensive map shows how the DXNN system implements a distributed neural network with evolutionary learning, where each component has clearly defined responsibilities and communication patterns that enable the system to evolve trading strategies through genetic algorithms.