# System Architecture Analysis: Memetic TWEANN with HyperNEAT

## Overview

This document provides a comprehensive analysis of the system's architecture, capabilities, and implementation details based on extensive investigation of the codebase. The system is a sophisticated **Memetic Algorithm-based Topology and Weight Evolving Artificial Neural Network (TWEANN)** that incorporates **HyperNEAT** for spatial pattern recognition in forex trading.

## System Classification

### Primary Architecture
- **Memetic Algorithm**: Population-based evolution with local search optimization
- **TWEANN**: Topology and Weight Evolving Artificial Neural Networks
- **HyperNEAT**: Spatial neural network architecture for pattern recognition
- **Hybrid Approach**: Combines NEAT evolution with HyperNEAT spatial processing

### Key Characteristics
- **Population Evolution**: NEAT-based agent evolution
- **Cultural Knowledge Transfer**: Species formation and fitness sharing
- **Spatial Processing**: HyperNEAT substrate for forex chart analysis
- **Dynamic Structure**: Evolving neural network topologies
- **Local Optimization**: Weight tuning and plasticity mechanisms

## Example Configuration Analysis

### System Parameters
```erlang
Density = 2          % Spatial resolution (2×2 = 4 processing points)
Depth = 1            % Number of hidden layers (1 hidden layer)
PLI Sensor = 6       % Price List Input with 6 data points
Actuators = 1        % Single trading decision output
Agents = 3           % Population size for evolution
```

### Resulting HyperNEAT Structure
```
INPUT LAYER (Depth=0)          HIDDEN LAYER (Depth=1)        OUTPUT LAYER (Depth=2)
┌─────────────────────────┐    ┌─────────────────────────┐    ┌─────────────────────────┐
│ PLI (6 data points):   │    │ [1,0,0] → Processor 1   │    │ [2,0,0] → Trading      │
│ [0,0,0] → Price 1      │    │ [1,0,1] → Processor 2   │    │         Decision        │
│ [0,0,1] → Price 2      │    │                         │    │         (BUY/SELL/HOLD)│
│ [0,0,2] → Price 3      │    │                         │    │                         │
│ [0,0,3] → Price 4      │    │                         │    │                         │
│ [0,0,4] → Price 5      │    │                         │    │                         │
│ [0,0,5] → Price 6      │    │                         │    │                         │
└─────────────────────────┘    └─────────────────────────┘    └─────────────────────────┘
```

## Core Components

### 1. HyperNEAT Substrate (Fixed Structure, Dynamic Weights)

The HyperNEAT substrate provides a **fixed spatial processing framework** that is shared across agents, but each agent has its **own substrate instance** with weights set by its evolving NEAT networks.

#### Substrate Structure (Fixed Framework)
- **Spatial Coordinates**: Fixed coordinate system based on densities and dimensions
  - **Input Layer**: `[0,0,0]` to `[0,0,5]` (6 price data points)
  - **Hidden Layer**: `[1,0,0]` to `[1,0,1]` (2 processing units)
  - **Output Layer**: `[2,0,0]` (1 trading decision)
- **Structure**: Never changes during evolution (densities, dimensions, coordinates)
- **Weights**: Dynamically calculated by each agent's NEAT CPP/CEP neurons

#### How It Works
1. **Substrate Framework**: All agents share the same spatial coordinate structure
2. **Per-Agent Instance**: Each agent has its own substrate record with its own CPP/CEP neuron IDs
3. **Weight Calculation**: For each substrate connection:
   - **CPP Neurons** (NEAT networks): Receive spatial coordinates `[x1,y1,z1,x2,y2,z2]` → Output weight factors
   - **CEP Neurons** (NEAT networks): Process weight factors → Final synaptic weights
   - **Substrate**: Uses these weights for pattern recognition

#### Key Point
- **Fixed**: Substrate structure (coordinates, dimensions, densities)
- **Evolving**: NEAT CPP/CEP networks that determine the weights
- **Unique per agent**: Each agent's substrate instance has different weights set by its unique NEAT networks

### 2. NEAT Neural Networks (Evolving Components)

Each agent contains NEAT neural networks that evolve:

#### Neuron Structure
```erlang
-record(neuron, {
    id,                    % Unique identifier
    generation,            % Evolution generation
    cx_id,                % Cortex identifier
    af,                   % Activation function (tanh, cos, gaussian, etc.)
    pf,                   % Plasticity function (hebbian, ojas, etc.)
    aggr_f,               % Aggregation function (dot_product, etc.)
    input_idps=[],        % Input connections and weights
    output_ids=[],        % Output connections
    ro_ids=[]             % Recurrent output connections
}).
```

#### Evolution Mechanisms
- **Weight mutations**: `{mutate_weights,1}`
- **Topology changes**: `{add_neuron,1}`, `{add_outlink,40}`, `{add_inlink,40}`
- **Function evolution**: `{mutate_af,1}`, `{mutate_aggrf,1}`
- **Bias modifications**: `{add_bias,1}`, `{remove_bias,1}`

### 3. CPP/CEP System (Spatial Processing Functions)

**CPP and CEP are NOT neurons themselves - they are special sensor/actuator interfaces that connect the HyperNEAT substrate to the NEAT networks!**

#### CPP (Cartesian Product Point) - Special "Sensor" for NEAT Networks

**What it is**: A special sensor that converts spatial coordinates into a format the NEAT network can understand.

**What it does**:
1. Receives two spatial coordinates from the substrate: `Presynaptic_Coords` (source) and `Postsynaptic_Coords` (target)
2. Converts these coordinates into a "sensory vector" using a coordinate transformation function
3. Sends this vector to the NEAT neurons as if it were sensor input

**Example CPP Functions** (from `functions.erl`):
- `cartesian(I_Coord, Coord)` → `[X1,Y1,Z1,X2,Y2,Z2]` (just concatenates coordinates)
- `centripital_distances(I_Coord, Coord)` → `[distance_from_origin1, distance_from_origin2]`
- `cartesian_distance(I_Coord, Coord)` → `[distance_between_points]`
- `cartesian_CoordDiffs(I_Coord, Coord)` → `[X2-X1, Y2-Y1, Z2-Z1]` (difference vector)

**Why it's needed**: The NEAT network needs to understand spatial relationships. Instead of giving it raw coordinates, CPP transforms them into meaningful features (distance, direction, etc.) that the network can learn from.

#### CEP (Cartesian End Point) - Special "Actuator" for NEAT Networks

**What it is**: A special actuator that converts NEAT network output into a final weight value for the substrate.

**What it does**:
1. Receives output signals from NEAT neurons
2. Processes these signals using a weight-setting function
3. Sends the final weight value back to the substrate

**Example CEP Functions** (from `substrate_cep.erl`):
- `set_weight(Output)` → Converts NEAT output into a weight value (with threshold, scaling)
- `set_abcn(Output)` → Sets weight with ABCN plasticity parameters
- `delta_weight(Output)` → Sets weight change (delta) for iterative learning

**Why it's needed**: The NEAT network outputs signals (like -1 to 1), but the substrate needs actual weight values. CEP converts the network's output into the appropriate weight format.

#### The Complete Flow

```
Substrate needs weight for connection from Neurode A [0,0,0] to Neurode B [1,0,0]
    ↓
Substrate sends coordinates to CPP: {[0,0,0], [1,0,0]}
    ↓
CPP converts coordinates using function (e.g., cartesian):
    → SensoryVector = [0,0,0,1,0,0] (concatenated coordinates)
    ↓
CPP sends SensoryVector to NEAT neurons (as if it were sensor input)
    ↓
NEAT neurons process the vector through their evolved network
    → Output = [0.7] (network's decision)
    ↓
NEAT neurons send output to CEP
    ↓
CEP processes output using function (e.g., set_weight):
    → Weight = 0.85 (converted and scaled)
    ↓
CEP sends weight back to substrate: {set_weight, [0.85]}
    ↓
Substrate uses weight = 0.85 for that connection
```

**Key Insight**: CPP and CEP are the "translators" between the spatial coordinate world (substrate) and the neural network world (NEAT). They allow the NEAT network to "understand" spatial relationships and "output" weight values.

#### Visual Diagram

```
┌─────────────────────────────────────────────────────────────┐
│ HyperNEAT Substrate (Spatial World)                        │
│                                                              │
│  Neurode A [0,0,0] ──(needs weight)──> Neurode B [1,0,0]    │
│         │                                                      │
│         └── sends coordinates ────┐                          │
└───────────────────────────────────┼──────────────────────────┘
                                     ↓
                        ┌────────────────────────┐
                        │ CPP (Sensor Interface) │
                        │ Function: cartesian    │
                        │ Input: [0,0,0], [1,0,0]│
                        │ Output: [0,0,0,1,0,0] │
                        └────────────────────────┘
                                     ↓
                        ┌────────────────────────┐
                        │ NEAT Neural Network    │
                        │ (Evolving neurons)     │
                        │ Processes: [0,0,0,1,0,0]│
                        │ Output: [0.7]          │
                        └────────────────────────┘
                                     ↓
                        ┌────────────────────────┐
                        │ CEP (Actuator Interface)│
                        │ Function: set_weight   │
                        │ Input: [0.7]           │
                        │ Output: Weight = 0.85  │
                        └────────────────────────┘
                                     ↓
┌───────────────────────────────────┼──────────────────────────┐
│ HyperNEAT Substrate (Spatial World)                        │
│                                                              │
│  Neurode A [0,0,0] ──(weight=0.85)──> Neurode B [1,0,0]    │
└─────────────────────────────────────────────────────────────┘
```

**Important**: CPP and CEP are implemented as special sensor/actuator types in the code, but conceptually they're **interfaces** that translate between two different representation systems:
- **Substrate**: Uses spatial coordinates `[x,y,z]`
- **NEAT Network**: Uses neural signals and vectors

### 4. Cultural Knowledge Transfer System

#### Species Formation
```erlang
speciate(Agent_Id)->
    update_fingerprint(Agent_Id),  % Create cultural "fingerprint"
    % Group agents by neural structure similarity
    % Create new species or add to existing species
```

#### Fitness Sharing
- **Within species**: Knowledge transfer between similar agents
- **Cultural competition**: Resource allocation based on species performance
- **Champion selection**: Preservation of best cultural knowledge

#### Innovation Tracking
```erlang
U_InnovationFactor = case TopFitness > Fitness of
    true ->  {0,TopFitness};      % Cultural breakthrough
    false -> {Factor-1,Fitness}   % Cultural stagnation
end
```

## Data Flow Architecture

### 1. Input Processing
```
Forex Price Data → PLI Sensor → HyperNEAT Substrate
```

### 2. Spatial Weight Calculation
```
Coordinates → CPP Neurons → NEAT Networks → CEP Neurons → Synaptic Weights
```

### 3. Pattern Recognition
```
Price Data + Spatial Weights → Substrate Processing → Trading Decision
```

### 4. Evolution Cycle
```
Fitness Evaluation → Species Formation → Cultural Competition → NEAT Mutation → Next Generation
```

## Evolution Process

### Generation 1: Initialization
```erlang
# Random NEAT networks with random weights:
Agent 1: Neuron Weights = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6] → Fitness: 0.3
Agent 2: Neuron Weights = [0.4, 0.5, 0.6, 0.7, 0.8, 0.9] → Fitness: 0.7
Agent 3: Neuron Weights = [0.7, 0.8, 0.9, 0.1, 0.2, 0.3] → Fitness: 0.5
```

### Generation N: Evolved Networks
```erlang
# NEAT-evolved networks with optimized weights:
Agent 1: Neuron Weights = [0.8, 0.7, 0.6, 0.5, 0.4, 0.3] → Fitness: 0.9
Agent 2: Neuron Weights = [0.2, 0.3, 0.4, 0.5, 0.6, 0.7] → Fitness: 0.8
Agent 3: Neuron Weights = [0.5, 0.6, 0.7, 0.8, 0.9, 1.0] → Fitness: 0.85
```

## Key Insights

### 1. Hybrid Architecture
The system combines three powerful approaches:
- **NEAT**: Evolving neural network topologies and weights
- **HyperNEAT**: Spatial pattern recognition for forex charts
- **Memetic Algorithm**: Cultural knowledge transfer and local optimization

### 2. Fixed vs. Evolving Components
- **Fixed**: HyperNEAT substrate **structure** (coordinates, spatial framework, dimensions)
  - All agents share the same coordinate system template
- **Evolving**: NEAT neural networks (CPP/CEP neurons) that:
  - Determine weights for substrate connections
  - Evolve topology, functions, and weights
- **Per-Agent**: Each agent has its own:
  - Substrate instance (with unique CPP/CEP neuron IDs)
  - NEAT networks that set unique weights for that substrate

### 3. Cultural Evolution
- **Species formation**: Groups agents by neural structure similarity
- **Fitness sharing**: Knowledge transfer within cultural groups
- **Cultural competition**: Resource allocation based on group performance
- **Innovation tracking**: Detection of cultural breakthroughs

### 4. Spatial Processing
- **Coordinate-based**: Each processor sees all inputs with spatial weighting
- **Pattern recognition**: Spatial relationships between price points
- **Efficient processing**: No need for complex topologies

## Performance Characteristics

### Advantages
1. **Adaptive Structure**: Networks can grow/shrink based on complexity needs
2. **Spatial Efficiency**: HyperNEAT provides effective pattern recognition
3. **Cultural Learning**: Species formation enables knowledge specialization
4. **Local Optimization**: Weight tuning provides fine-grained improvement
5. **Scalability**: Can handle complex multi-sensor inputs

### Limitations
1. **Fixed Spatial Structure**: HyperNEAT substrate cannot evolve
2. **Computational Cost**: Multiple neural networks per agent
3. **Cultural Isolation**: Limited knowledge transfer between species
4. **Parameter Sensitivity**: Requires careful tuning of density/depth

## Configuration Recommendations

### For Simple Patterns (Trend Detection)
```erlang
Density = 5,  Depth = 1   % 25 processing points, 1 hidden layer
```

### For Medium Patterns (Trends + Reversals)
```erlang
Density = 8,  Depth = 2   % 64 processing points per layer, 2 hidden layers
```

### For Complex Patterns (All Chart Patterns)
```erlang
Density = 12, Depth = 2   % 144 processing points per layer, 2 hidden layers
```

## Conclusion

This system represents a sophisticated implementation of evolutionary computation that combines:

1. **Memetic Algorithm**: Cultural knowledge transfer and local optimization
2. **TWEANN**: Dynamic neural network evolution
3. **HyperNEAT**: Efficient spatial pattern recognition

The result is a powerful forex trading system that can:
- **Evolve** neural network structures for optimal performance
- **Recognize** complex spatial patterns in forex charts
- **Transfer** knowledge through cultural evolution
- **Adapt** to changing market conditions

This hybrid approach provides significant advantages over traditional neural networks or pure evolutionary approaches, making it well-suited for complex pattern recognition tasks in financial markets.

## Technical Implementation Notes

### Database Schema
- **neuron**: Stores NEAT neural network genotypes
- **sensor**: Stores CPP neurons (spatial processors)
- **actuator**: Stores CEP neurons (weight processors)
- **substrate**: Stores HyperNEAT structure and connections
- **specie**: Stores cultural group information
- **agent**: Stores individual agent genotypes

### Key Files
- `genotype.erl`: Core genotype management
- `population_monitor.erl`: Cultural evolution and species management
- `substrate.erl`: HyperNEAT spatial processing
- `neuron.erl`: NEAT neural network processing
- `genome_mutator.erl`: Evolution operators
- `selection_algorithm.erl`: Cultural competition mechanisms

This architecture represents a state-of-the-art approach to evolutionary neural networks for financial pattern recognition.

---

## High-Level Process Walkthrough: NEAT Neural Network Lifecycle

This section provides a step-by-step walkthrough of how NEAT neural networks are created, assessed, mutated, and evolved in your system.

**Note**: This walkthrough is based on the actual codebase implementation and aligns with concepts described in Gene Sher's "Handbook of Neuroevolution Through Erlang" (specifically Chapters 8, 10, 11, 16, and 17).

### Phase 1: NEAT Neural Network Creation

#### Step 1: Population Initialization
When you run `benchmarker:start(sliding_window_10)`, the system:
- Creates a population of agents (e.g., 10 agents)
- Each agent will contain a NEAT neural network
- Agents are grouped into species based on their structure similarity

#### Step 2: Agent Construction (`genotype:construct_Agent/3`)
For each new agent:
1. **Generation set to 0** (first generation)
2. **Encoding type chosen** (neural or substrate - your system uses substrate for HyperNEAT)
3. **Cortex creation** (`construct_Cortex/6`):
   - Creates a "brain" (cortex) that coordinates all components
   - Sets up sensors (input) and actuators (output)
   - For substrate encoding:
     - Creates substrate record with dimensions and densities (lines 82-85, 94-102)
     - Creates CPP sensors and CEP actuators (lines 86-87)
     - **Note**: The actual HyperNEAT substrate structure (hypercube with neurodes at coordinates) is created later at runtime, not here

#### Step 3: Initial Neural Layer Creation (`construct_InitialNeuroLayer/7`)
**This creates the NEAT networks (the neurons that connect to CPP/CEP), NOT the HyperNEAT substrate structure!**

For **substrate encoding** (your system uses this):
- The function receives `Substrate_CPPs` and `Substrate_CEPs` (which are special sensor/actuator **interfaces**, not neurons)
- CPP = Special sensor that converts spatial coordinates to vectors
- CEP = Special actuator that converts NEAT output to weight values
- It creates **NEAT neurons** that connect to these CPP/CEP interfaces
- These NEAT neurons will evolve to process coordinate information and calculate weights for the substrate

1. **For each CEP actuator** (CEP = Cartesian End Point):
   - Creates a set of NEAT neurons (one neuron per CEP output dimension)
   - Each neuron gets a unique ID like `{{0,Unique_Id},neuron}`
   - These are the NEAT network neurons that will process substrate coordinates

2. **NEAT neuron connections** (`construct_InitialNeurons/6`):
   - Each NEAT neuron randomly connects to:
     - **Either** all CPP sensors (50% chance)
     - **Or** a random subset of CPP sensors (50% chance)
   - Each NEAT neuron connects to its target CEP actuator
   - **Note**: CPP = Cartesian Product Point (receives spatial coordinates)

3. **NEAT neuron initialization** (`construct_Neuron/6`):
   - Creates the neuron record with:
     - **Random activation function** (tanh, cos, gaussian, etc.)
     - **Random plasticity function** (hebbian, ojas, etc.)
     - **Random aggregation function** (dot_product, etc.)
     - **Random weights** for each input connection (range: -0.5 to 0.5)
     - **Plasticity parameters** based on the plasticity function

**Important Distinction**:
- **Step 3 creates**: NEAT networks (CPP/CEP neurons) that will set substrate weights
- **HyperNEAT substrate structure**: Created later at runtime when the agent is activated:
  - The substrate record (created in Step 2) stores dimensions and densities
  - When `exoself:start` runs, it spawns the substrate process
  - The substrate process creates the actual hypercube structure with neurodes at specific coordinates
  - This happens in `substrate.erl` in functions like `create_substrate/4` and `populate_InputHyperlayer`

#### Step 4: Weight Initialization
- Each weight is initialized as `random:uniform() - 0.5` (between -0.5 and 0.5)
- Each weight has associated plasticity parameters for learning during evaluation

**Result**: You now have a population of agents, each with:
- **Its own HyperNEAT substrate instance** (fixed spatial structure framework, but unique per agent)
- **NEAT neural networks (CPP/CEP neurons)** that will evolve and set weights for the substrate
- Random initial weights and connections in the NEAT networks

---

### Phase 2: Assessment and Evaluation

#### Step 1: Agent Activation (`exoself:start/3`)
When an agent needs to be evaluated:
- The exoself process reads the agent's genotype from the database
- Spawns all neural network processes:
  - Sensors (read market data)
  - Neurons (process information)
  - Actuators (make trading decisions)
  - Cortex (coordinates everything)

#### Step 2: Evaluation Cycle (Sense-Think-Act)
The agent runs through multiple cycles:

1. **Sense Phase**:
   - Sensors request market data from `live_scape` or `fx_scape`
   - Data flows: `Price Data → Sensor → Substrate → CPP Neurons`

2. **Think Phase**:
   - **Cortex** sends `sync` signal to all sensors
   - Sensors send data to substrate
   - **Substrate weight calculation** (for each connection):
     - Substrate sends spatial coordinates to **CPP neurons** (NEAT networks)
     - CPP neurons calculate weight factors based on spatial relationships
     - **CEP neurons** (NEAT networks) process CPP output → final synaptic weights
     - Substrate uses these weights to process price data
   - **Neurons process** (if any regular NEAT neurons exist):
     - Receive inputs from sensors/other neurons
     - Apply aggregation function (e.g., dot product)
     - Apply activation function (e.g., tanh)
     - Apply plasticity function (weights update during processing)
     - Send outputs to connected neurons/actuators

3. **Act Phase**:
   - Actuators receive signals from neurons
   - Make trading decision (BUY=1, SELL=-1, HOLD=0)
   - Send decision to `live_scape` or `fx_scape`
   - Receive fitness feedback (P&L from trading)

#### Step 3: Fitness Accumulation
- **Cortex** collects fitness from actuators after each cycle
- Fitness accumulates across multiple trading cycles
- When evaluation completes:
  - Total fitness = accumulated P&L from all trades
  - Number of cycles = how many Sense-Think-Act loops completed
  - Time taken = execution duration

#### Step 4: Weight Tuning (Local Optimization)
During evaluation in "gt" (genetic training) mode:
- Agent tries to improve by **weight perturbation**:
  - Selects neurons for weight adjustment
  - Perturbs weights slightly
  - Re-evaluates to see if fitness improves
  - If better: keeps new weights
  - If worse: restores previous weights
- This is **memetic learning** - local optimization within the agent

**Result**: Each agent has a fitness score representing its trading performance.

---

### Phase 3: Mutation and Evolution

#### Step 1: Generation Completion
When all agents in a generation finish evaluation:
- Population monitor receives `terminated` messages from all agents
- Calculates species fitness statistics (avg, max, min)
- Triggers `mutate_population/4`

#### Step 2: Species Mutation (`mutate_Specie/5`)
For each species:

1. **Fitness Calculation**:
   - Sorts agents by fitness
   - Calculates average, max, min fitness
   - Tracks innovation factor (cultural breakthroughs)

2. **Selection**:
   - Applies fitness postprocessor (normalizes/transforms fitness)
   - Runs selection algorithm (chooses survivors)
   - Top performers survive, others are replaced

3. **Offspring Creation**:
   - Selected agents become parents
   - New agents created via mutation of parent agents
   - Maintains species size (e.g., keep 10 agents per species)

#### Step 3: Individual Agent Mutation (`genome_mutator:mutate/1`)
For each new offspring agent:

1. **Generation Increment**: `generation = parent_generation + 1`

2. **Search Parameter Mutation** (10% chance):
   - May mutate tuning selection function
   - May mutate annealing parameters
   - May mutate mutation rate functions

3. **Calculate Mutation Count**:
   - Determines how many mutations to apply
   - Based on `tot_topological_mutations_f` function
   - Typically: 1 to sqrt(total_neurons) mutations

4. **Apply Neural Mutations** (`apply_Mutators/2`):
   Randomly selects from available mutation operators:

   **Weight Mutations**:
   - `mutate_weights`: Perturbs existing weights
     - Probability: `1/sqrt(total_weights)`
     - Each weight has a chance to be slightly changed

   **Topology Mutations**:
   - `add_neuron`: Inserts new neuron between two existing neurons
   - `add_outlink`: Adds new connection from neuron to another
   - `add_inlink`: Adds new connection to neuron from another
   - `remove_link`: Removes an existing connection
   - `remove_neuron`: Removes a neuron (if it becomes disconnected)

   **Function Mutations**:
   - `mutate_af`: Changes activation function (tanh → cos, etc.)
   - `mutate_aggrf`: Changes aggregation function
   - `mutate_pf`: Changes plasticity function

   **Bias Mutations**:
   - `add_bias`: Adds bias input to neuron
   - `remove_bias`: Removes bias from neuron

5. **Fingerprint Update**:
   - Updates agent's "fingerprint" (structural signature)
   - Used for species classification

**Result**: New generation of agents with:
- Evolved topologies (more/less neurons, different connections)
- Evolved weights (optimized for trading)
- Evolved functions (different activation/plasticity functions)

---

### Phase 4: Evolution Cycle (Complete Loop)

```
┌─────────────────────────────────────────────────────────────┐
│ GENERATION N                                                 │
├─────────────────────────────────────────────────────────────┤
│ 1. Agents evaluated (trading performance)                   │
│ 2. Fitness scores calculated                                │
│ 3. Species formed/updated (structure similarity)            │
│ 4. Selection: Best agents survive                           │
│ 5. Mutation: New agents created from survivors              │
│ 6. Generation N+1 created                                    │
└─────────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────────┐
│ GENERATION N+1                                               │
├─────────────────────────────────────────────────────────────┤
│ (Repeat process with evolved agents)                        │
└─────────────────────────────────────────────────────────────┘
```

#### Key Evolution Mechanisms:

1. **Species Formation**:
   - Agents grouped by neural structure similarity (fingerprint)
   - Each species competes independently
   - Prevents one dominant solution from taking over

2. **Fitness Sharing**:
   - Within species: agents share fitness "resources"
   - Encourages diversity within species

3. **Innovation Tracking**:
   - Tracks when species improves
   - Detects cultural breakthroughs vs stagnation
   - Influences selection pressure

4. **Cultural Evolution**:
   - Best agents preserved as "champions"
   - Knowledge transferred through mutation
   - Species adapt or die based on performance

---

### Summary: What Your System Does

1. **Creates** NEAT networks with random structures and weights
2. **Evaluates** them by having them trade forex and measuring P&L
3. **Mutates** them by:
   - Changing weights
   - Adding/removing neurons and connections
   - Changing activation/plasticity functions
4. **Evolves** them through:
   - Selection (survival of the fittest)
   - Speciation (diversity through structure grouping)
   - Cultural knowledge transfer (best solutions preserved)

The system continues this cycle until:
- Maximum generations reached
- Fitness goal achieved
- Evaluation limit reached

**Result**: Evolved NEAT neural networks optimized for forex trading pattern recognition!

---

### Key Files Reference

- **`genotype.erl`**: Creates initial NEAT networks (`construct_Neuron`, `construct_Agent`)
- **`exoself.erl`**: Activates agents and manages evaluation
- **`cortex.erl`**: Coordinates Sense-Think-Act cycles
- **`neuron.erl`**: Processes neural signals during evaluation
- **`genome_mutator.erl`**: Applies mutations (`mutate`, `mutate_weights`, `add_neuron`, etc.)
- **`population_monitor.erl`**: Manages evolution (`mutate_population`, `mutate_Specie`)
- **`selection_algorithm.erl`**: Chooses survivors for next generation
- **`substrate.erl`**: Manages HyperNEAT substrate and calls CPP/CEP neurons for weights
- **`substrate_cpp.erl`**: CPP neurons that calculate spatial weight factors
- **`substrate_cep.erl`**: CEP neurons that convert weight factors to synaptic weights

---

## Clarification: Substrate Framework vs. NEAT Networks

### The Architecture (Corrected Understanding - Verified Against Handbook)

You're absolutely right! Here's the correct picture, verified against Gene Sher's "Handbook of Neuroevolution Through Erlang":

```
┌─────────────────────────────────────────────────────────────┐
│ SHARED SUBSTRATE FRAMEWORK (Fixed Structure)                │
│ - Coordinate system: [0,0,0] to [2,0,0]                     │
│ - Dimensions: Based on sensors/actuators                    │
│ - Densities: [Depth, 1, Density, Density, ...]               │
│ - Structure: NEVER changes                                  │
└─────────────────────────────────────────────────────────────┘
            ↓
┌─────────────────────────────────────────────────────────────┐
│ AGENT 1                                                      │
│ - Substrate Instance: Unique ID, links to CPP/CEP neurons   │
│ - NEAT CPP Neurons: Evolve to set weights                    │
│ - NEAT CEP Neurons: Evolve to process weights               │
│ - Result: Unique weight pattern for Agent 1's substrate      │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ AGENT 2                                                      │
│ - Substrate Instance: Unique ID, links to CPP/CEP neurons   │
│ - NEAT CPP Neurons: Evolve to set weights (different!)      │
│ - NEAT CEP Neurons: Evolve to process weights (different!)   │
│ - Result: Unique weight pattern for Agent 2's substrate      │
└─────────────────────────────────────────────────────────────┘
```

### How Weight Setting Works

When the substrate needs weights for a connection between two coordinates:

1. **Substrate calls** `get_weights()` with coordinates `[x1,y1,z1,x2,y2,z2]`
2. **CPP neurons** (NEAT networks) receive coordinates:
   - Process through their evolved NEAT network structure
   - Output: Weight factors based on spatial relationship
3. **CEP neurons** (NEAT networks) receive CPP output:
   - Process through their evolved NEAT network structure  
   - Output: Final synaptic weight for that connection
4. **Substrate uses** this weight to process price data

### Key Insight (From Handbook Chapter 16)

According to the Handbook of Neuroevolution (Chapter 16: Substrate Encoding):

- **Substrate Structure**: A hypercube structure with neurodes (neurons) embedded at specific coordinates
  - Each neurode has a coordinate based on its position in the substrate
  - Coordinates are fixed based on density and dimensions
  - The substrate structure itself is shared conceptually, but each agent has its own instance

- **Indirect Encoding**: The NEAT networks (CPP/CEP neurons) act as a "secondary NN" that:
  - Receives coordinates of connected neurodes in the substrate
  - Outputs the synaptic weights for those connections
  - This allows dense substrates (thousands of neurons, millions of connections) to be specified with relatively small NEAT networks

- **Per-Agent**: Each agent has:
  - Its own substrate instance with its own CPP/CEP neuron IDs
  - Unique NEAT CPP/CEP networks that evolve to set weights
  - Different weight patterns for the same substrate structure

- **Result**: Same substrate framework/structure, but each agent interprets spatial patterns differently through its evolved NEAT networks that set unique weights!

### Handbook Reference

From Chapter 16.1: "The directly encoded NN is fed the coordinates of these substrate-embedded neurodes... and the output signal produced by the direct encoded NN is the synaptic weight between the connected neurodes in the substrate. Thus the synaptic weights of the neurodes are defined by this secondary NN."

This confirms: **One substrate framework, many instances, evolving weight controllers (NEAT networks)**
