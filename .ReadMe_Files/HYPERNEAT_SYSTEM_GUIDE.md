# HyperNEAT-Based Neuroevolutionary Trading System
## A Comprehensive Guide to the Distributed Extended Neural Network (DXNN) Platform

---

## Table of Contents

1. [30,000 Feet: System Overview](#30000-feet-system-overview)
2. [How It Works: Operational Flow](#how-it-works-operational-flow)
3. [Technical Deep Dive](#technical-deep-dive)
4. [Practical Usage](#practical-usage)
5. [System Architecture](#system-architecture)
6. [References](#references)

---

## 30,000 Feet: System Overview

### What Is This System?

This is a **HyperNEAT-based neuroevolutionary trading platform** implemented in Erlang. It combines three powerful computational approaches to create intelligent forex trading agents:

1. **NEAT (NeuroEvolution of Augmenting Topologies)**: Evolves the structure and weights of neural networks
2. **HyperNEAT (Hypercube-based NEAT)**: Enables spatial pattern recognition through substrate encoding
3. **Memetic Algorithms**: Combines evolutionary search with local weight optimization

### The Core Concept: What is HyperNEAT?

Traditional neural networks directly encode every neuron and connection. For a network with 1000 neurons, you need to store thousands of connections individually. HyperNEAT takes a revolutionary approach:

**Instead of evolving the large neural network directly, HyperNEAT evolves a small "pattern-generating network" that describes HOW to build the large network.**

Think of it like this:
- **Traditional approach**: Drawing every pixel of an image by hand
- **HyperNEAT approach**: Writing a mathematical function that generates the image

This allows the system to:
- Encode very large networks efficiently
- Exploit geometric regularities in data (like patterns in forex charts)
- Discover and utilize spatial relationships between inputs

### How This System Implements HyperNEAT

Your system consists of two interconnected neural network types:

#### 1. The Substrate (The Large, Fixed-Structure Network)

The **substrate** is a spatial neural network organized as a multi-dimensional grid (hypercube) with coordinates:

```
INPUT LAYER              HIDDEN LAYER(S)          OUTPUT LAYER
[0,0,0] → Price 1        [1,0,0] → Processor 1    [2,0,0] → Trading Decision
[0,0,1] → Price 2        [1,0,1] → Processor 2    
[0,0,2] → Price 3        [1,0,2] → Processor 3
[0,0,3] → Price 4        ...
```

- **Fixed structure**: The coordinate system and layout never change
- **Spatial organization**: Each neuron (neurode) has a position in space
- **Purpose**: Processes forex price data to make trading decisions

#### 2. The NEAT Networks (The Evolving Weight-Generators)

Small **NEAT neural networks** that evolve to determine the connection weights in the substrate:

```
CPP Neurons → Process spatial coordinates
     ↓
NEAT Network Processing (EVOLVES)
     ↓
CEP Neurons → Output connection weights
     ↓
Substrate uses these weights
```

- **Evolving structure**: These networks grow, shrink, and mutate
- **Purpose**: Calculate optimal connection strengths based on spatial relationships
- **Key insight**: By evolving these small networks, the system efficiently searches for patterns across the entire substrate

### The Trading Application

The system applies HyperNEAT to **forex trading**:

1. **Sensors** gather price data (historical candlesticks, technical indicators)
2. **Substrate** processes spatial patterns in price charts
3. **NEAT networks** determine which spatial patterns are important
4. **Actuators** make trading decisions (BUY, SELL, HOLD)
5. **Evolution** improves the NEAT networks based on trading profit/loss

### Why This Approach Works for Trading

**Forex charts have spatial structure**: 
- Support and resistance levels span horizontally
- Trends move diagonally
- Chart patterns (head-and-shoulders, triangles) have geometric shapes

**HyperNEAT exploits this structure**:
- The substrate's spatial organization matches the chart's spatial structure
- NEAT networks learn which geometric patterns predict price movements
- The system discovers patterns like "when prices form a triangle at these coordinates, typically a breakout follows"

### The Evolutionary Process

The system doesn't just run neural networks—it **evolves** them:

1. **Population**: Starts with 10-100 random trading agents
2. **Evaluation**: Each agent trades on historical forex data
3. **Selection**: Profitable agents survive; unprofitable ones are removed
4. **Mutation**: Survivors create offspring with modified NEAT networks
5. **Repeat**: Over hundreds of generations, agents improve their trading strategies

The evolution happens at the **NEAT network level** (the weight-generators), which indirectly evolves the trading behavior of the substrate.

### Key Benefits of This Hybrid Approach

1. **Scalability**: Can handle large substrates (thousands of neurons) with small NEAT networks
2. **Pattern Recognition**: Naturally discovers geometric patterns in data
3. **Efficiency**: Evolves compact network descriptions rather than massive networks
4. **Adaptability**: Both structure (topology) and parameters (weights) evolve
5. **Diversity**: Multiple species explore different trading strategies simultaneously

---

## How It Works: Operational Flow

### The Complete System Flow

Let's trace what happens when you run an evolutionary experiment for forex trading:

```
User Command: benchmarker:start(chart_plane_10x20)
                    ↓
         [INITIALIZATION PHASE]
                    ↓
         Population Monitor creates 20 agents
                    ↓
         Each agent gets:
         • Random NEAT network (CPP/CEP neurons)
         • Substrate template (10x20 grid)
         • Forex trading morphology
                    ↓
         [EVALUATION PHASE]
                    ↓
         For each agent:
         1. ExoSelf spawns neural processes
         2. Substrate creates spatial grid
         3. Agent trades on historical data
         4. Fitness = Profit/Loss accumulated
                    ↓
         [EVOLUTION PHASE]
                    ↓
         Population Monitor:
         • Ranks agents by fitness
         • Selects top performers
         • Mutates NEAT networks
         • Creates next generation
                    ↓
         [REPEAT FOR GENERATIONS]
                    ↓
         Final result: Best evolved trading strategy
```

### Detailed Operational Walkthrough

#### Phase 1: Initialization (Creating the First Generation)

**Step 1: Population Creation**

When you run `benchmarker:start(chart_plane_10x20)`:

```erlang
% The system creates:
- Population ID: "test"
- Species: 1-3 species based on structural similarity
- Agents per species: 10-20 agents
- Morphology: forex_trader (trading strategy)
- Encoding: substrate (HyperNEAT)
```

**Step 2: Agent Genotype Construction**

For each agent, the system constructs a genotype (genetic blueprint) in the Mnesia database:

```erlang
Agent Genotype:
├── Cortex (Brain coordinator)
├── Sensors (Price data inputs)
│   ├── fx_PCI (Price Chart Input - 2D spatial grid)
│   └── fx_PLI (Price List Input - 1D time series)
├── Substrate (HyperNEAT spatial processor)
│   ├── Dimensions: [10, 20] = 200 neurodes per layer
│   ├── Depth: 1 hidden layer
│   ├── CPP_Ids: [Sensor IDs that provide coordinates]
│   └── CEP_Ids: [Actuator IDs that receive weights]
├── NEAT Neurons (Weight-generating network)
│   ├── Random topology: 5-15 neurons initially
│   ├── Random connections to CPP/CEP
│   └── Random weights: -0.5 to +0.5
└── Actuators (Trading outputs)
    └── fx_Trade (BUY/SELL/HOLD decisions)
```

**Step 3: NEAT Network Initial Structure**

The initial NEAT network is small and random:

```
CPP (Coordinate Input)
  ↓
Neuron 1 (random activation: tanh)
  ↓
Neuron 2 (random activation: gaussian)
  ↓
CEP (Weight Output)
```

This network will be asked questions like: *"What should the connection weight be between position [0,0,1] and [1,0,5]?"*

#### Phase 2: Evaluation (Testing Trading Performance)

**Step 1: Agent Activation**

The ExoSelf process spawns the agent's neural network as distributed Erlang processes:

```
ExoSelf (Agent Manager)
├── Cortex_PId (Coordinator)
├── Sensor_PIds (Data inputs)
├── Neuron_PIds (NEAT neurons)
├── Actuator_PIds (Trading outputs)
├── Substrate_PId (Spatial processor)
├── CPP_PIds (Coordinate providers)
└── CEP_PIds (Weight receivers)
```

Each component runs as an independent Erlang process communicating via message passing.

**Step 2: Substrate Construction**

When the substrate process starts, it builds the spatial grid:

```erlang
% For chart_plane_10x20 (Depth=1, Resolution=10x20)

Input Layer (Depth 0):
  - 10x20 = 200 neurodes at coordinates [0, x, y]
  - Each receives one price data point
  - Coordinates: [0,0,0] to [0,9,19]

Hidden Layer (Depth 1):
  - 10x20 = 200 neurodes at coordinates [1, x, y]
  - Process spatial patterns
  - Coordinates: [1,0,0] to [1,9,19]

Output Layer (Depth 2):
  - 1 neurode at coordinate [2,0,0]
  - Final trading decision
```

**Step 3: The Sense-Think-Act Cycle**

The agent runs through multiple trading cycles on historical forex data:

```
CYCLE START (t=0)
    ↓
1. SENSE: Sensors request price data
   sensor ! {cortex, sync}
   fx_scape ! {sensor, sense, EURUSD, close, [10,20,graph_sensor], 1000, 2000}
    ↓
2. SUBSTRATE WEIGHT CALCULATION:
   For each connection in substrate:
   
   a. Substrate needs weight for [0,3,7] → [1,5,12]
   
   b. CPP receives coordinates [0,3,7,1,5,12]
      - Converts to sensory vector using cartesian function
      - Result: [0,3,7,1,5,12] (or transformed coordinates)
   
   c. NEAT Neurons process the coordinate vector:
      - Input aggregation (weighted sum)
      - Activation function (tanh, gaussian, etc.)
      - Plasticity adjustments (Hebbian, Oja's rule)
      - Output: [0.73]
   
   d. CEP receives NEAT output [0.73]
      - Applies set_weight function
      - Scales/thresholds the value
      - Result: Weight = 0.85
   
   e. Substrate stores this weight for connection [0,3,7]→[1,5,12]
   
   Repeat for ALL substrate connections (thousands of connections!)
    ↓
3. SUBSTRATE PROCESSING:
   - Input layer receives price data (200 values)
   - Each neurode has weighted connections (calculated above)
   - Hidden layer processes: Output = activation(sum(inputs × weights))
   - Output layer produces trading signal: -1 to +1
    ↓
4. ACT: Actuator executes trade
   actuator receives signal: 0.73
   converts to discrete action:
     > 0.5  → BUY  (1)
     < -0.5 → SELL (-1)
     else   → HOLD (0)
   
   Sends to scape: fx_scape ! {actuator, trade, EURUSD, 1}
    ↓
5. FITNESS CALCULATION:
   scape calculates P&L from trade
   - Entry price: 1.0850
   - Exit price: 1.0875
   - Profit: +25 pips
   - Fitness: +0.0025
   
   Returns: {scape, fitness, 0.0025, continue}
    ↓
6. ACCUMULATE:
   cortex accumulates fitness
   - Previous total: 0.0100
   - This cycle: +0.0025
   - New total: 0.0125
    ↓
NEXT CYCLE (t=1) or END if data exhausted
```

**Step 4: Evaluation Completion**

After trading through the entire historical dataset (e.g., 7000 price bars):

```erlang
Agent Evaluation Results:
- Total Fitness: 0.2347 (23.47% profit)
- Cycles Completed: 7000
- Time Taken: 45,231,000 microseconds
- Status: terminated
```

The ExoSelf backs up the final state to the database and reports to the Population Monitor.

#### Phase 3: Evolution (Creating the Next Generation)

**Step 1: Selection**

Population Monitor receives fitness scores from all agents:

```erlang
Generation 0 Results:
Agent_1: Fitness = -0.1234 (loss)
Agent_2: Fitness =  0.2347 (profit!)
Agent_3: Fitness =  0.0432 (profit)
Agent_4: Fitness = -0.0821 (loss)
...
Agent_20: Fitness = 0.1876 (profit)

Sorted by fitness:
1. Agent_2:  0.2347  ← Champion (survives)
2. Agent_20: 0.1876  ← Top performer (survives)
3. Agent_3:  0.0432  ← Survives
...
(Rest are eliminated)
```

**Step 2: Mutation**

Survivors create offspring through mutation:

```erlang
Parent: Agent_2 (Fitness 0.2347)
    ↓
Mutation Operators Applied:
1. mutate_weights: Perturb 3 random weights
   Weight [0.73] → [0.81]
   
2. add_neuron: Insert new neuron
   Before: N1 → N2
   After:  N1 → N_new → N2
   
3. mutate_af: Change activation function
   Neuron 5: tanh → cos
   
4. add_outlink: Add new connection
   N3 → N7 (weight: 0.34)
    ↓
Offspring: Agent_21 (Generation 1)
- Inherits most structure from Agent_2
- Has 4 structural changes
- Ready for evaluation
```

**Step 3: Next Generation Launch**

```erlang
Generation 1:
- Agent_2 (Champion, unchanged)
- Agent_20 (Elite survivor)
- Agent_21 (Mutated offspring of Agent_2)
- Agent_22 (Mutated offspring of Agent_2)
...
- Agent_40 (Mutated offspring of Agent_20)

Total: 20 agents for evaluation
```

The cycle repeats: Evaluation → Selection → Mutation → Evaluation...

#### Phase 4: Convergence (Finding Optimal Strategies)

Over many generations (10-200+), the population improves:

```
Generation 0:  Best Fitness = 0.2347, Avg = -0.0123
Generation 10: Best Fitness = 0.4521, Avg = 0.1234
Generation 50: Best Fitness = 0.8934, Avg = 0.5432
Generation 100: Best Fitness = 1.2341, Avg = 0.8765
```

**What's evolving?**

1. **NEAT Network Topology**: Number of neurons, connections, architecture
2. **NEAT Network Weights**: Synaptic strengths in the weight-generating network
3. **Activation Functions**: tanh, gaussian, sin, cos, abs, etc.
4. **Plasticity Functions**: Hebbian, Oja's, neuromodulation rules
5. **Indirectly, Trading Behavior**: Through the substrate weights generated by NEAT

### The Beautiful Interaction: NEAT ↔ HyperNEAT

Here's the key insight of how NEAT and HyperNEAT work together:

```
USER'S QUESTION: "How do NEAT and HyperNEAT interact?"

ANSWER: The NEAT networks GENERATE the HyperNEAT substrate's behavior!

1. NEAT Network = "Weight Calculator"
   - Small network (10-50 neurons)
   - Receives coordinate pairs as input
   - Outputs connection weights
   - EVOLVES over generations

2. HyperNEAT Substrate = "Pattern Processor"
   - Large network (200-10,000 neurodes)
   - Fixed spatial structure
   - Uses weights from NEAT
   - Processes trading data

INTERACTION:
┌──────────────────────────────────────────────────────────┐
│ For each substrate connection:                           │
│                                                           │
│ Substrate needs: Weight for [Input_A] → [Hidden_B]      │
│                                                           │
│ 1. Substrate sends coordinates to CPP:                   │
│    CPP ! {substrate, [0,3,7], [1,5,12]}                 │
│                                                           │
│ 2. CPP converts to neural input:                         │
│    Vector = [0,3,7,1,5,12]                              │
│    Sends to NEAT neurons                                 │
│                                                           │
│ 3. NEAT Network processes:                               │
│    - Neuron_1: dot_product(Vector, Weights_1) → tanh    │
│    - Neuron_2: input from Neuron_1 → gaussian           │
│    - Neuron_3: input from Neuron_2 → linear             │
│    Output: [0.73]                                        │
│                                                           │
│ 4. CEP receives and converts:                            │
│    set_weight([0.73]) → Weight = 0.85                   │
│                                                           │
│ 5. Substrate uses weight:                                │
│    Connection [Input_A] → [Hidden_B] has weight 0.85    │
│                                                           │
│ This happens for EVERY connection in the substrate!     │
└──────────────────────────────────────────────────────────┘

EVOLUTION:
- Generation 0: NEAT network generates poor weights → Bad trading → Low fitness
- Selection: Poor traders eliminated
- Mutation: NEAT networks modified (add neurons, adjust weights, change functions)
- Generation 1: NEAT networks generate better weights → Better trading → Higher fitness
- Repeat...
- Generation 100: Highly evolved NEAT networks generate optimal weights → Excellent trading!
```

### Memetic Learning (Local Optimization)

Beyond evolution, the system also performs **memetic learning** during evaluation:

```
Agent starts with evolved NEAT weights
    ↓
Evaluation Cycle 1: Tests on market data → Fitness: 0.1234
    ↓
Weight Perturbation: Slightly adjusts NEAT weights
    ↓
Evaluation Cycle 2: Tests with perturbed weights → Fitness: 0.1456 (better!)
    ↓
Keep new weights
    ↓
Repeat perturbation...
    ↓
After max_attempts (e.g., 10 cycles):
    Return best fitness achieved
```

This combines:
- **Global Search** (Evolution across generations)
- **Local Search** (Weight tuning within evaluation)

---

## Technical Deep Dive

### System Architecture Components

#### 1. Core Erlang Modules (Neural Runtime)

**exoself.erl** - Agent Lifecycle Manager
```erlang
% Responsibilities:
- Spawns all neural processes for an agent
- Manages agent evaluation lifecycle
- Coordinates weight backup/restore
- Reports fitness to population monitor

% Key Functions:
start(Agent_Id, PM_PId, OpMode) → spawns agent
prep(Agent_Id, PM_PId, OpMode) → initializes agent
loop(State, [exoself|IdsNPIds]) → manages evaluation
```

**cortex.erl** - Neural Network Coordinator
```erlang
% Responsibilities:
- Synchronizes Sense-Think-Act cycles
- Sends sync signals to sensors
- Receives fitness from actuators
- Accumulates total fitness

% Key Functions:
gen(ExoSelf_PId, Node) → spawns cortex
loop(ExoSelf_PId, SPIds, NPIds, APIds, ...) → coordination loop
```

**sensor.erl** - Input Processors
```erlang
% Sensor Types:
fx_PCI: Price Chart Input (2D spatial grid of prices)
fx_PLI: Price List Input (1D time series)
fx_Internals: Account state (position, P&L)

% Key Functions:
gen(ExoSelf_PId, Node) → spawns sensor
loop(ExoSelf_PId, Cx_PId, ..., SName, VL, Parameters, Fanout_PIds) → sense loop
```

**neuron.erl** - NEAT Neural Processing Units
```erlang
% NEAT Neuron Structure:
- Input connections with weights
- Aggregation function (dot_product, mult_product)
- Activation function (tanh, gaussian, sin, cos, abs, etc.)
- Plasticity function (hebbian, ojas, neuromodulation)
- Output connections

% Key Functions:
gen(ExoSelf_PId, Node) → spawns neuron
loop(ExoSelf_PId, Input_PIdPs, ..., AF, PF, AggrF) → neural processing
```

**actuator.erl** - Output Executors
```erlang
% Actuator Types:
fx_Trade: Executes forex trades (BUY/SELL/HOLD)
pts: Print to screen (debugging)

% Key Functions:
gen(ExoSelf_PId, Node) → spawns actuator
loop(ExoSelf_PId, Cx_PId, ..., Fanin_PIds, ..., Parameters) → act loop
fx_Trade(ExoSelf_PId, ..., Parameters, Scape_PId) → trading logic
```

#### 2. Substrate Components (HyperNEAT)

**substrate.erl** - Spatial Neural Network
```erlang
% Structure:
- Hypercube of neurodes at fixed coordinates
- Multiple layers (input, hidden, output)
- Densities define resolution per dimension
- Connections determined by linkform

% Key Functions:
gen(ExoSelf_PId, Node) → spawns substrate
prep(ExoSelf) → initializes substrate
loop(ExoSelf, State, SPIds, SAcc) → substrate processing
reason(SAcc, State) → calculates substrate forward pass

% Coordinate System:
Input Layer:   [0, X, Y, Z, ...] (Depth 0)
Hidden Layer:  [1, X, Y, Z, ...] (Depth 1)
Output Layer:  [2, X, Y, Z, ...] (Depth 2)
```

**substrate_cpp.erl** - Coordinate Pattern Producer
```erlang
% Purpose: Converts spatial coordinates to neural inputs for NEAT

% CPP Functions (from functions.erl):
cartesian(I_Coord, Coord) → [X1,Y1,Z1,X2,Y2,Z2]
centripital_distances(I_Coord, Coord) → [Dist1, Dist2]
cartesian_distance(I_Coord, Coord) → [Distance_between]
cartesian_CoordDiffs(I_Coord, Coord) → [X2-X1, Y2-Y1, Z2-Z1]

% Process:
1. Substrate requests weight for connection
2. CPP receives coordinate pair
3. CPP converts to sensory vector
4. CPP sends vector to NEAT neurons
```

**substrate_cep.erl** - Connection Expression Producer
```erlang
% Purpose: Converts NEAT outputs to substrate connection weights

% CEP Functions:
set_weight(Output, ...) → converts NEAT signal to weight value
set_abcn(Output, ...) → sets weight with plasticity parameters
delta_weight(Output, ...) → sets weight change (iterative learning)

% Process:
1. CEP receives output from NEAT neurons
2. CEP applies weight function
3. CEP sends weight to substrate
```

#### 3. Evolution Modules (Genetic Operators)

**population_monitor.erl** - Evolutionary Orchestrator
```erlang
% Responsibilities:
- Manages population lifecycle
- Spawns agents for evaluation
- Collects fitness results
- Triggers evolution
- Tracks generations and statistics

% Key Functions:
init(State) → initializes population
handle_cast({Agent_Id, terminated, Fitness}, State) → collects results
mutate_population(Specie_Ids, Pop_Id, ...) → evolves population
mutate_Specie(S, OpTag, ...) → evolves one species
```

**genome_mutator.erl** - Mutation Operators
```erlang
% Mutation Types:

% Weight Mutations:
mutate_weights(Agent_Id) → perturbs synaptic weights

% Topology Mutations:
add_neuron(Agent_Id) → inserts new neuron
add_outlink(Agent_Id) → adds new connection
add_inlink(Agent_Id) → adds input connection
remove_inlink(Agent_Id) → removes connection
remove_neuron(Agent_Id) → removes neuron

% Function Mutations:
mutate_af(Agent_Id) → changes activation function
mutate_pf(Agent_Id) → changes plasticity function
mutate_aggrf(Agent_Id) → changes aggregation function

% Bias Mutations:
add_bias(Agent_Id) → adds bias input
remove_bias(Agent_Id) → removes bias
```

**selection_algorithm.erl** - Survivor Selection
```erlang
% Selection Methods:
competition(Sorted_AgentIds, NeuralEnergyCost, FitnessGoal) →
    - Fitness-based tournament selection
    - Top performers survive
    - Bottom performers eliminated

top3(Sorted_AgentIds, NeuralEnergyCost, FitnessGoal) →
    - Top 3 agents survive
    - Rest are offspring of top 3
```

#### 4. Genotype Management (Database Layer)

**genotype.erl** - Genetic Blueprint Storage
```erlang
% Genotype Records (stored in Mnesia):
#agent{} - Complete agent blueprint
#cortex{} - Brain coordinator
#sensor{} - Input specification
#actuator{} - Output specification
#neuron{} - NEAT neuron genotype
#substrate{} - HyperNEAT substrate structure

% Key Functions:
construct_Agent(Specie_Id, Agent_Id, SpecCon) → builds new genotype
construct_Cortex(...) → creates neural architecture
construct_InitialNeuroLayer(...) → creates initial NEAT neurons
sync() → synchronizes database
dirty_read({Type, Id}) → fast database read
write(Record) → database write
```

**records.hrl** - Data Structure Definitions
```erlang
% Core Agent Record:
-record(agent,{
    id, encoding_type, generation, population_id,
    specie_id, cx_id, fingerprint, constraint,
    evo_hist=[], fitness, innovation_factor,
    pattern=[], tuning_selection_f, annealing_parameter,
    tuning_duration_f, perturbation_range,
    mutation_operators, tot_topological_mutations_f,
    heredity_type, substrate_id
}).

% Neuron Record:
-record(neuron,{
    id, generation, cx_id, af, pf, aggr_f,
    input_idps=[], input_idps_modulation=[],
    output_ids=[], ro_ids=[], si_output_ids=[]
}).

% Substrate Record:
-record(substrate,{
    id, agent_id, cpp_ids, cep_ids,
    densities, plasticity=none, linkform
}).
```

#### 5. Trading Environment (Scape Layer)

**fx.erl** - Forex Simulator
```erlang
% Responsibilities:
- Loads historical forex data from files
- Simulates trading account
- Executes trades (market simulation)
- Calculates P&L and fitness
- Manages account state

% Key Functions:
init() → loads forex data into ETS tables
start() → starts forex system
sim(ExoSelf_PId) → starts trading simulation
sense(Sensor_PId, Parameters) → provides price data
make_trade(Signal, State, TechData) → executes trade

% Data Files:
fx_tables/EURUSD1.txt - 1-minute bars
fx_tables/EURUSD15.txt - 15-minute bars
```

**scape.erl** - Environment Interface
```erlang
% Purpose: Abstracts environment interaction

% Functions:
gen(ExoSelf_PId, {private, ScapeName}) → spawns private scape
fx_sim(ExoSelf_PId) → forex simulation environment
```

**live_scape.erl** - Live Trading Interface
```erlang
% Purpose: Connects agents to live Interactive Brokers

% Responsibilities:
- Receives real-time OHLC bars from Python IB service
- Stores bars in ETS tables
- Provides data to neural network sensors
- Sends trade signals to Python IB service
- Manages live trading state
```

#### 6. Utility Modules

**config.erl** - System Configuration
```erlang
% Configuration Categories:
- Account parameters (leverage, initial balance, lot size)
- Data parameters (currency pairs, date ranges, indices)
- Neural parameters (activation functions, plasticity functions)
- Evolution parameters (population size, mutation rates)
- Live trading parameters (IB connection, risk management)
```

**functions.erl** - Mathematical Functions
```erlang
% Activation Functions:
tanh, cos, sin, gaussian, absolute, linear, quadratic,
cubic, sqrt, sigmoid, etc.

% Utility Functions:
saturation, scale, etc.

% CPP Coordinate Functions:
cartesian, centripital_distances, cartesian_distance,
cartesian_CoordDiffs, iow, tor_difference, etc.
```

**plasticity.erl** - Learning Rules
```erlang
% Plasticity Functions:
none(Input, Output, Weights) → no learning
hebbian(Input, Output, Weights, Parameters) → Hebbian rule
ojas(Input, Output, Weights, Parameters) → Oja's rule
self_modulation_v1/v2/v3/v4 → self-modulating plasticity
neuromodulation → modulated learning
```

**benchmarker.erl** - Experiment Manager
```erlang
% Responsibilities:
- Runs multiple experimental runs
- Collects statistics
- Generates reports
- Handles checkpointing (for AWS spot instances)

% Key Functions:
start(Experiment_Id) → starts benchmark
continue(Experiment_Id) → resumes benchmark
report(Traces, Graph_MorphList) → generates report
```

### Data Structures and Flow

#### Agent Genotype Structure

```
Agent_Id: {timestamp, agent}
├── encoding_type: substrate
├── generation: 0 (increments with evolution)
├── cx_id: {timestamp, cortex}
├── substrate_id: {timestamp, substrate}
├── constraint: #constraint{}
├── mutation_operators: [
│   {mutate_weights, 1},
│   {add_neuron, 1},
│   {add_outlink, 40},
│   {add_inlink, 40},
│   {mutate_af, 1},
│   ...
│   ]
├── fitness: 0.2347
└── evo_hist: [mutation history]

Cortex_Id: {timestamp, cortex}
├── sensor_ids: [{s1, sensor}, {s2, sensor}, ...]
├── actuator_ids: [{a1, actuator}, ...]
├── neuron_ids: [{n1, neuron}, {n2, neuron}, ...]

Substrate_Id: {timestamp, substrate}
├── cpp_ids: [{cpp1, sensor}, ...]  % Coordinate producers
├── cep_ids: [{cep1, actuator}, ...] % Weight expressors
├── densities: [1, 10, 20]  % [Depth, Dim1, Dim2]
├── linkform: l2l_feedforward % Connection pattern
├── plasticity: none

Neuron_Id: {timestamp, neuron}
├── af: tanh  % Activation function
├── pf: none  % Plasticity function
├── aggr_f: dot_product
├── input_idps: [
│   {{sensor, s1}, [{w1, Weights1}]},
│   {{neuron, n2}, [{w2, Weights2}]},
│   ...
│   ]
└── output_ids: [{neuron, n3}, {actuator, a1}, ...]
```

#### Message Flow During Evaluation

```
Time T: Cortex sends sync

Cortex → Sensors: {Cx_PId, sync}

Sensors → Scape: {Sensor_PId, sense, TableName, Feature, Parameters, Start, End}

Scape → Sensors: {Scape_PId, [Price1, Price2, ..., PriceN]}

Sensors → Substrate: {Sensor_PId, forward, PriceVector}

Substrate internally:
  For each connection:
    Substrate → CPP: {Sub_PId, Coord1, Coord2}
    CPP → NEAT_Neurons: {CPP_PId, forward, CoordVector}
    NEAT_Neurons → NEAT_Neurons: {Neuron_PId, forward, Signal}
    NEAT_Neurons → CEP: {Neuron_PId, forward, Output}
    CEP → Substrate: {CEP_PId, set_weight, Weight}
  
  Substrate calculates forward pass with all weights
  
Substrate → Actuators: {Sub_PId, forward, [TradeSignal]}

Actuators → Scape: {Act_PId, trade, TableName, Signal}

Scape → Actuators: {Scape_PId, sync, Fitness, HaltFlag}

Actuators → Cortex: {Act_PId, sync, Fitness, HaltFlag}

Cortex accumulates fitness, checks HaltFlag

If not halted: Repeat cycle
If halted: Cortex → ExoSelf: {Cx_PId, evaluation_completed, TotalFitness, Cycles, Time}
```

### Evolution Cycle Details

#### Generation Flow

```
GENERATION N:

1. Population Monitor State:
   - Active Agents: 20
   - Agents Left: 20
   - Generation: N

2. Spawn all agents via exoself:start(Agent_Id, PM_PId, gt)

3. Each agent evaluates independently:
   - Trades on historical data
   - Accumulates fitness
   - May perform local weight tuning (memetic)

4. Agents report termination:
   ExoSelf → PopMonitor: {Agent_Id, terminated, Fitness, Evals, Cycles, Time}

5. Population Monitor collects all results:
   Agent_1: Fitness = -0.1234, Cycles = 7000, Time = 45s
   Agent_2: Fitness =  0.2347, Cycles = 7000, Time = 43s
   ...

6. When all agents finished:
   mutate_population(Specie_Ids, Population_Id, EvolutionaryAlgorithm)

7. For each species:
   a. Sort agents by fitness
   b. Calculate statistics (avg, max, min, std)
   c. Apply fitness postprocessor
   d. Run selection algorithm
   e. Keep top performers (champions)
   f. Eliminate bottom performers
   g. Create mutant offspring from survivors

8. Mutation Process for each offspring:
   - Clone parent agent genotype
   - Calculate mutation count: sqrt(TotalNeurons)
   - Apply random mutation operators:
     * mutate_weights (adjust synaptic strengths)
     * add_neuron (insert new neuron)
     * add_outlink (new connection)
     * mutate_af (change activation function)
     * etc.
   - Update fingerprint (structural signature)
   - Write mutated genotype to database

9. New generation ready:
   - Champions preserved unchanged
   - Offspring with mutations
   - Total population size maintained

10. Check termination conditions:
    - Generation limit reached?
    - Fitness goal achieved?
    - Evaluation limit reached?
    
    If not terminated: Start Generation N+1 (go to step 2)
    If terminated: Report results to benchmarker
```

#### Species and Speciation

```
Species Formation (Cultural Evolution):

1. Agent Fingerprint:
   - Encodes neural network structure
   - Based on: # sensors, # actuators, # neurons
   - Updated after each mutation

2. Species Creation:
   - When no species exists: Create new species
   - When species exist: Compare fingerprints
   - If similar enough: Add to existing species
   - If too different: Create new species

3. Species Properties:
   #specie{
       id,
       population_id,
       fingerprint,
       constraint,
       agent_ids=[...],
       innovation_factor={0, 0},
       stats=[...]
   }

4. Fitness Sharing:
   - Agents compete within species
   - Species compete for resources
   - Prevents single strategy dominance
   - Encourages diversity

5. Innovation Tracking:
   - Tracks species improvement
   - {InnovationFactor, TopFitness}
   - If TopFitness improves: Innovation!
   - If stagnant: InnovationFactor decreases
   - Species with low innovation may be eliminated
```

### HyperNEAT Substrate Mathematics

#### Coordinate System

For a substrate with configuration:
- Depth = 1 (number of hidden layers)
- Dimensions = [10, 20] (10 rows, 20 columns)

```
Layer Structure:

Input Layer (Depth 0):
Coordinates: [0, Row, Col] where Row ∈ [0,9], Col ∈ [0,19]
Total neurodes: 10 × 20 = 200
Example: [0,0,0], [0,0,1], ..., [0,9,19]

Hidden Layer (Depth 1):
Coordinates: [1, Row, Col]
Total neurodes: 10 × 20 = 200
Example: [1,0,0], [1,0,1], ..., [1,9,19]

Output Layer (Depth 2):
Coordinates: [2, 0, 0]
Total neurodes: 1 (single trading decision)
```

#### Weight Calculation Process

For connection from Input[0,3,7] to Hidden[1,5,12]:

```erlang
% Step 1: CPP receives coordinates
Presynaptic = [0,3,7]
Postsynaptic = [1,5,12]

% Step 2: CPP applies coordinate transformation
% Using cartesian function:
CoordVector = [0,3,7,1,5,12]

% Or using centripital_distances:
Dist1 = sqrt(0² + 3² + 7²) = 7.62
Dist2 = sqrt(1² + 5² + 12²) = 13.04
CoordVector = [7.62, 13.04]

% Or using cartesian_CoordDiffs:
Diffs = [1-0, 5-3, 12-7] = [1, 2, 5]
CoordVector = [1, 2, 5]

% Step 3: NEAT network processes vector
% Assume 3 neurons: N1 → N2 → N3

N1_Inputs = CoordVector
N1_Weights = [0.5, 0.3, 0.2, 0.7, 0.1, 0.4]
N1_Aggregation = dot_product(N1_Inputs, N1_Weights) = sum(Inputs[i] × Weights[i])
N1_Activation = tanh(N1_Aggregation) = 0.73

N2_Inputs = [0.73]
N2_Weights = [0.6, 0.2]
N2_Aggregation = dot_product([0.73, 1], [0.6, 0.2]) = 0.638
N2_Activation = gaussian(0.638) = 0.867

N3_Inputs = [0.867]
N3_Weights = [0.8]
N3_Aggregation = 0.867 × 0.8 = 0.6936
N3_Activation = linear(0.6936) = 0.6936

% Step 4: CEP receives NEAT output
CEP_Input = [0.6936]

% CEP applies set_weight function:
set_weight(Output) ->
    Threshold = 0.5,
    if
        abs(Output) < Threshold -> Weight = 0;
        true -> Weight = Output
    end.

Final_Weight = 0.6936

% Step 5: Substrate uses weight
Connection [[0,3,7] → [1,5,12]] has weight = 0.6936
```

This process repeats for **every connection** in the substrate (potentially thousands of connections).

#### Substrate Forward Pass

Once all weights are calculated:

```erlang
% Input Layer receives price data
Input_Layer = [Price_0_0, Price_0_1, ..., Price_9_19]  % 200 values

% Hidden Layer computation
For each hidden neurode H at [1, row, col]:
    H_Input = sum over all input neurodes I at [0, r, c]:
        Weight[I→H] × Input[I]
    H_Output = activation(H_Input)

% Output Layer computation
O_Input = sum over all hidden neurodes H:
    Weight[H→O] × Hidden[H]
O_Output = activation(O_Input)

% Final result: Trading signal
TradeSignal = O_Output  % e.g., 0.73 → BUY
```

### Plasticity and Learning

#### Hebbian Learning

Neurons can adapt weights during evaluation using Hebbian learning:

```erlang
% Hebbian Rule: "Neurons that fire together, wire together"

hebbian(Input, Output, {Weights, Parameters}, Acc) ->
    Learning_Rate = parameters:get(learning_rate, Parameters, 0.01),
    
    Updated_Weights = [
        W + Learning_Rate × I × Output
        || {W, I} <- lists:zip(Weights, Input)
    ],
    
    {Updated_Weights, Acc}.

% Example:
Input = [0.5, 0.8, 0.3]
Output = 0.7
Weights = [0.2, 0.3, 0.5]
Learning_Rate = 0.01

New_Weights = [
    0.2 + 0.01 × 0.5 × 0.7 = 0.2035,
    0.3 + 0.01 × 0.8 × 0.7 = 0.3056,
    0.5 + 0.01 × 0.3 × 0.7 = 0.5021
]
```

#### Oja's Rule

A normalized version of Hebbian learning that prevents weight explosion:

```erlang
ojas(Input, Output, {Weights, Parameters}, Acc) ->
    Learning_Rate = parameters:get(learning_rate, Parameters, 0.01),
    
    Updated_Weights = [
        W + Learning_Rate × Output × (I - Output × W)
        || {W, I} <- lists:zip(Weights, Input)
    ],
    
    {Updated_Weights, Acc}.
```

### Memetic Algorithm (Local Search)

During evaluation, agents perform local weight optimization:

```erlang
% Exoself evaluation loop (gt mode):

1. Initial Evaluation:
   - Run neural network on data
   - Record fitness: F_initial = 0.1234

2. Weight Perturbation:
   - Select neurons via tuning_selection function
   - Send weight_perturb message
   - Neurons adjust weights randomly: W_new = W_old + random(-Spread, +Spread)

3. Re-evaluation:
   - Run neural network with perturbed weights
   - Record fitness: F_perturbed = 0.1456

4. Hill Climbing Decision:
   If F_perturbed > F_initial:
       Keep perturbed weights (send weight_backup)
       F_initial = F_perturbed
   Else:
       Restore old weights (send weight_restore)

5. Repeat:
   - Try up to max_attempts times (e.g., 10 attempts)
   - Keep best weights found

6. Termination:
   - Save best weights to database
   - Report best fitness to population monitor
```

This combines:
- **Global Search**: Evolution across generations (population-level)
- **Local Search**: Weight tuning within evaluation (individual-level)

Result: Memetic algorithm, more powerful than pure evolution.

---

## Practical Usage

### Installation and Setup

#### Prerequisites

```bash
# Docker (recommended for consistent environment)
docker --version

# Or Erlang/OTP 26+ installed locally
erl -version
```

#### Build and Compile

```bash
# Using Docker:
docker build -t erlang-dev .
docker run -it --rm -v ${PWD}:/workspace -w /workspace erlang-dev

# Inside container or with local Erlang:
make:all([load]).
```

#### Initialize Database and Data

```erlang
% Inside Erlang shell:
mnesia:create_schema([node()]).
mnesia:start().

% Load forex data:
fx:init().
fx:start().

% Create polis (infrastructure):
polis:create().
polis:start().
polis:sync().
```

### Running Evolution Experiments

#### Simple Experiments

```erlang
% Price List Input (1D time series) experiments:
benchmarker:start(sliding_window_5).    % 5 data points
benchmarker:start(sliding_window_10).   % 10 data points
benchmarker:start(sliding_window_20).   % 20 data points
benchmarker:start(sliding_window_50).   % 50 data points
benchmarker:start(sliding_window_100).  % 100 data points
```

#### HyperNEAT Experiments (2D Spatial)

```erlang
% Price Chart Input (2D grid) experiments:
benchmarker:start(chart_plane_5x10).    % 5×10 grid = 50 neurodes/layer
benchmarker:start(chart_plane_5x20).    % 5×20 grid = 100 neurodes/layer
benchmarker:start(chart_plane_10x10).   % 10×10 grid = 100 neurodes/layer
benchmarker:start(chart_plane_10x20).   % 10×20 grid = 200 neurodes/layer
benchmarker:start(chart_plane_20x20).   % 20×20 grid = 400 neurodes/layer
```

#### Monitoring Progress

```erlang
% View current generation logs:
file:read_file("logs/Population/generation_0.log").
file:read_file("logs/Population/generation_1.log").

% View evolution milestones:
file:read_file("logs/Population/evolution_milestones.log").

% Check population stats:
genotype_utils:get_agent_stats().
```

### Analyzing Results

#### Finding the Best Agent

```erlang
% Load record definitions:
rr("records.hrl").

% Find best agent:
genotype_utils:print_best_genotype().

% Or from specific population:
genotype_utils:print_best_genotype(test).

% List top agents:
genotype_utils:print_top_agents(10).

% Find best agent ID:
{atomic, BestAgentId} = genotype_utils:find_best_agent(all).
% Returns: {5.693207755943648e-10, agent}
```

#### Testing an Evolved Agent

```erlang
% Test on training data:
BestAgentId = {5.693207755943648e-10, agent}.
exoself:start(BestAgentId, self(), gt).

% Test on benchmark data (next 200 bars after training):
exoself:start(BestAgentId, self(), benchmark).

% Wait for result:
receive
    {_Pid, benchmark_complete, _SpecieId, Fitness, Cycles, Time} ->
        io:format("Fitness: ~p | Cycles: ~p | Time: ~p μs~n", [Fitness, Cycles, Time])
after 60000 ->
    io:format("Timeout~n")
end.
```

#### Inspecting Agent Structure

```erlang
% Read agent genotype:
Agent = genotype:dirty_read({agent, BestAgentId}).

% Print agent details:
io:format("Generation: ~p~n", [Agent#agent.generation]).
io:format("Fitness: ~p~n", [Agent#agent.fitness]).
io:format("Encoding: ~p~n", [Agent#agent.encoding_type]).
io:format("Mutations: ~p~n", [Agent#agent.evo_hist]).

% If substrate-encoded:
SubstrateId = Agent#agent.substrate_id.
Substrate = genotype:dirty_read({substrate, SubstrateId}).
io:format("Densities: ~p~n", [Substrate#substrate.densities]).

% Get cortex:
CortexId = Agent#agent.cx_id.
Cortex = genotype:dirty_read({cortex, CortexId}).
io:format("Neurons: ~p~n", [length(Cortex#cortex.neuron_ids)]).
```

### Live Trading

#### Setup Interactive Brokers Connection

```bash
# Set environment variables:
export IB_HOST=host.docker.internal
export IB_PORT=7497  # Paper trading (7496 for live)
export IB_CLIENT_ID=101
export ALLOW_LIVE_ORDERS=1

# Start Python IB service:
python3 priv/ib_service.py
```

#### Deploy Evolved Agent to Live Trading

```erlang
% Start live scape:
{ok, Pid} = live_scape:start_link().

% Wait for historical data to load:
timer:sleep(10000).

% Check data availability:
ets:info(ohlc_data, size).

% Deploy best agent:
{atomic, BestAgentId} = genotype_utils:find_best_agent(all).
AgentPid = exoself:start(BestAgentId, self(), live_trading).

% Monitor agent:
is_process_alive(AgentPid).

% Check position:
live_scape ! {self(), sense, internals, []}.
receive
    {_From, [Position, EntryPrice, PreviousPC]} ->
        io:format("Position: ~p, Entry: ~p, P&L: ~p~n", 
                  [Position, EntryPrice, PreviousPC])
after 5000 ->
    io:format("No response~n")
end.

% Stop agent:
AgentPid ! {self(), terminate}.
```

### Configuration

#### Key Configuration Functions (config.erl)

```erlang
% Data configuration:
config:primary_currency_pair().  % 'EURUSD1'
config:data_start_index().       % 1000
config:data_end_index().         % 8000
config:benchmark_end_index().    % 8200

% Account configuration:
config:account_initial_balance(). % 10000.0
config:account_leverage().        % 100
config:account_lot_size().        % 10000

% Neural configuration:
config:neural_activation_functions().  % [tanh, cos, gaussian, ...]
config:neural_plasticity_functions().  % [none, hebbian, ojas, ...]

% Evolution configuration:
config:init_specie_size().        % 10
config:survival_percentage().     % 0.5
config:generation_limit().        % 100
config:evaluations_limit().       % 100000

% Mutation configuration:
config:neural_pfns().             % Plasticity functions
config:agent_encoding_types().    % [neural, substrate]
```

#### Modifying Configurations

```erlang
% Edit config.erl to change defaults

% Example: Increase population size
init_specie_size() -> 20.  % Default was 10

% Example: Change activation functions
neural_activation_functions() -> 
    [tanh, relu, gaussian, sin, cos].

% Recompile:
make:all([load]).

% Changes take effect for new experiments
```

### Troubleshooting

#### Common Issues

**1. No data in ETS tables**
```erlang
% Solution: Initialize and start fx system
fx:init().
fx:start().
```

**2. Mnesia schema issues**
```erlang
% Solution: Reset Mnesia
mnesia:stop().
mnesia:delete_schema([node()]).
mnesia:create_schema([node()]).
mnesia:start().
```

**3. Agent evaluation hangs**
```erlang
% Check for crashed processes:
process_info(AgentPid).

% Kill hung agent:
exit(AgentPid, kill).

% Restart population monitor:
population_monitor:stop().
```

**4. Cannot read agent genotype**
```erlang
% Solution: Sync Mnesia
polis:sync().

% Or reload from disk:
mnesia:stop().
mnesia:start().
```

#### Debugging Tips

```erlang
% Enable debug logging in config.erl:
actuator_debug_tag() -> true.

% View logs:
qlog:read_agent_logs(AgentId).

% Check ETS tables:
ets:info(ohlc_data).
ets:tab2list(ohlc_data).

% Monitor processes:
observer:start().  % Graphical process monitor
```

---

## System Architecture

### High-Level Architecture Diagram

```
┌──────────────────────────────────────────────────────────────────────────┐
│                          DXNN HYPERNEAT SYSTEM                            │
├──────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  ┌─────────────────────────────────────────────────────────────────────┐ │
│  │                      BENCHMARKER LAYER                              │ │
│  │  - Experiment Management                                             │ │
│  │  - Multi-run Coordination                                            │ │
│  │  - Statistics & Reporting                                            │ │
│  └────────────────────────────┬─────────────────────────────────────────┘ │
│                               │                                           │
│  ┌────────────────────────────▼─────────────────────────────────────────┐ │
│  │                  POPULATION MONITOR LAYER                            │ │
│  │  - Population Management                                             │ │
│  │  - Generation Evolution                                              │ │
│  │  - Species Formation                                                 │ │
│  │  - Selection & Mutation                                              │ │
│  └────────────────────────────┬─────────────────────────────────────────┘ │
│                               │                                           │
│                    ┌──────────┴──────────┐                               │
│                    │                     │                                │
│  ┌─────────────────▼────────┐  ┌────────▼─────────────────────────────┐ │
│  │   EVOLUTION MODULES      │  │   AGENT EVALUATION (Multiple)        │ │
│  │  - genome_mutator        │  │                                      │ │
│  │  - selection_algorithm   │  │  ┌────────────────────────────────┐  │ │
│  │  - fitness_postprocessor │  │  │ EXOSELF (Agent Manager)        │  │ │
│  └──────────────────────────┘  │  │  - Process Spawning            │  │ │
│                                 │  │  - Lifecycle Management        │  │ │
│                                 │  │  - Memetic Local Search        │  │ │
│                                 │  └───────────┬────────────────────┘  │ │
│                                 │              │                        │ │
│  ┌──────────────────────────┐  │  ┌───────────▼────────────────────┐  │ │
│  │   DATABASE LAYER         │  │  │ CORTEX (Coordinator)           │  │ │
│  │  - Mnesia Database       │◀─┼──│  - Sense-Think-Act Cycles      │  │ │
│  │  - genotype.erl          │  │  │  - Synchronization             │  │ │
│  │  - Agent Storage         │  │  └───────────┬────────────────────┘  │ │
│  └──────────────────────────┘  │              │                        │ │
│                                 │      ┌───────┴────────┐              │ │
│  ┌──────────────────────────┐  │      │                │              │ │
│  │   SCAPE LAYER            │  │  ┌───▼─────┐   ┌──────▼────────┐   │ │
│  │  - fx.erl (Simulator)    │◀─┼──│ SENSORS │   │   ACTUATORS   │   │ │
│  │  - live_scape.erl (Live) │  │  │ fx_PCI  │   │   fx_Trade    │   │ │
│  │  - Market Data           │  │  │ fx_PLI  │   │   pts         │   │ │
│  │  - Trade Execution       │  │  └───┬─────┘   └──────┬────────┘   │ │
│  └──────────────────────────┘  │      │                │              │ │
│                                 │      └────────┬───────┘              │ │
│                                 │               │                      │ │
│                                 │  ┌────────────▼─────────────────┐   │ │
│                                 │  │  HYPERNEAT SUBSTRATE         │   │ │
│                                 │  │  - Spatial Grid (Hypercube)  │   │ │
│                                 │  │  - Fixed Structure           │   │ │
│                                 │  │  - Dynamic Weights from NEAT │   │ │
│                                 │  └────────────┬─────────────────┘   │ │
│                                 │               │                      │ │
│                                 │      ┌────────┴───────┐             │ │
│                                 │      │                │              │ │
│                                 │  ┌───▼──────┐   ┌────▼─────────┐   │ │
│                                 │  │   CPP    │   │     CEP      │   │ │
│                                 │  │ (Coord   │   │  (Weight     │   │ │
│                                 │  │ Producer)│   │  Expresser)  │   │ │
│                                 │  └───┬──────┘   └────▲─────────┘   │ │
│                                 │      │               │              │ │
│                                 │      └───────┬───────┘              │ │
│                                 │              │                      │ │
│                                 │  ┌───────────▼────────────────┐    │ │
│                                 │  │  NEAT NEURONS              │    │ │
│                                 │  │  - Evolving Topology       │    │ │
│                                 │  │  - Evolving Weights        │    │ │
│                                 │  │  - Activation Functions    │    │ │
│                                 │  │  - Plasticity Functions    │    │ │
│                                 │  └────────────────────────────┘    │ │
│                                 └─────────────────────────────────────┘ │
│                                                                           │
└──────────────────────────────────────────────────────────────────────────┘
```

### Process Architecture

```
Population Monitor Process (gen_server)
├── Spawns → ExoSelf_1 (Agent 1)
│            ├── Cortex_PId
│            ├── Sensor_PIDs (3-5 sensors)
│            ├── Neuron_PIDs (10-100 NEAT neurons)
│            ├── Actuator_PIDs (1-2 actuators)
│            ├── Substrate_PId
│            ├── CPP_PIDs (2-5 coordinate producers)
│            └── CEP_PIDs (1-3 weight expressers)
│
├── Spawns → ExoSelf_2 (Agent 2)
│            ├── ... (same structure)
│            └── ...
│
└── Spawns → ExoSelf_N (Agent N)
             ├── ... (same structure)
             └── ...

All processes communicate via Erlang message passing
Each agent evaluates independently in parallel
```

### Data Flow Architecture

```
┌─────────────┐
│ Forex Data  │
│ (ETS Table) │
└──────┬──────┘
       │
       ▼
┌──────────────────────────────────────────────────────────┐
│                    AGENT RUNTIME                          │
│                                                            │
│  Sense Phase:                                             │
│  Sensor → Scape → Sensor → Substrate                     │
│                                                            │
│  Weight Generation Phase (for each substrate connection): │
│  Substrate → CPP → NEAT Neurons → CEP → Substrate        │
│                                                            │
│  Think Phase:                                             │
│  Substrate: Forward pass with generated weights           │
│                                                            │
│  Act Phase:                                               │
│  Substrate → Actuator → Scape                            │
│                                                            │
│  Fitness Phase:                                           │
│  Scape → Actuator → Cortex                               │
│                                                            │
│  Accumulation:                                            │
│  Cortex: Total_Fitness += Cycle_Fitness                  │
│                                                            │
└──────────────────────────┬───────────────────────────────┘
                           │
                           ▼
                  ┌─────────────────┐
                  │ Population      │
                  │ Monitor         │
                  │ (Collects       │
                  │  Fitness)       │
                  └────────┬────────┘
                           │
                           ▼
                  ┌─────────────────┐
                  │ Evolution       │
                  │ (Mutation &     │
                  │  Selection)     │
                  └────────┬────────┘
                           │
                           ▼
                  ┌─────────────────┐
                  │ Mnesia Database │
                  │ (Stores Next    │
                  │  Generation)    │
                  └─────────────────┘
```

### Module Dependencies

```
Application Layer:
├── benchmarker.erl → Experiment orchestration
└── launcher.erl → System startup

Evolution Layer:
├── population_monitor.erl → Population management
├── genome_mutator.erl → Genetic operators
├── selection_algorithm.erl → Survivor selection
└── fitness_postprocessor.erl → Fitness transformation

Agent Layer:
├── exoself.erl → Agent lifecycle
├── cortex.erl → Neural coordination
├── sensor.erl → Input processing
├── neuron.erl → NEAT neural processing
├── actuator.erl → Output execution
├── substrate.erl → HyperNEAT spatial network
├── substrate_cpp.erl → Coordinate encoding
└── substrate_cep.erl → Weight decoding

Genotype Layer:
├── genotype.erl → Genetic blueprint management
└── genotype_utils.erl → Inspection utilities

Environment Layer:
├── fx.erl → Forex simulator
├── scape.erl → Environment interface
└── live_scape.erl → Live trading interface

Support Layer:
├── config.erl → System configuration
├── functions.erl → Mathematical functions
├── plasticity.erl → Learning rules
├── signal_aggregator.erl → Neural aggregation
├── tuning_duration.erl → Memetic duration
└── tuning_selection.erl → Neuron selection
```

---

## References

### Source Inspiration

This system is based on the architecture described in:

**"Handbook of Neuroevolution Through Erlang"** by Gene I. Sher  
- Chapter 10: DXNN Case Study
- Chapter 16: Substrate Encoding (HyperNEAT)
- Chapter 19: Evolving Currency Trading Agents

### Key Papers

1. **NEAT**: Stanley, K. O., & Miikkulainen, R. (2002). "Evolving Neural Networks through Augmenting Topologies"

2. **HyperNEAT**: Stanley, K. O., D'Ambrosio, D. B., & Gauci, J. (2009). "A Hypercube-Based Encoding for Evolving Large-Scale Neural Networks"

3. **Memetic Algorithms**: Moscato, P. (1989). "On Evolution, Search, Optimization, Genetic Algorithms and Martial Arts: Towards Memetic Algorithms"

### System Components

- **Language**: Erlang/OTP 26+
- **Database**: Mnesia (distributed database)
- **Trading Interface**: Interactive Brokers API (via Python ib_insync)
- **Data Storage**: ETS (Erlang Term Storage)

### Additional Documentation

See the `.ReadMe_Files/` directory for detailed documentation:
- `CONSOLIDATED_ARCHITECTURE.md` - Live trading architecture
- `System_Architecture_Analysis.md` - Technical analysis
- `Function_Dependencies_and_Communication_Map.md` - Module communication
- `Process_Steps.md` - Detailed execution flow
- `Neurevolution_Handbook_Overview.md` - Handbook chapter summaries

---

## Conclusion

This HyperNEAT-based neuroevolutionary trading system represents a sophisticated application of computational intelligence to financial markets. By combining:

- **NEAT's evolutionary topology search**
- **HyperNEAT's spatial pattern recognition**
- **Memetic algorithms' local optimization**
- **Erlang's distributed, fault-tolerant architecture**

The system can evolve highly specialized forex trading strategies that exploit geometric patterns in price charts.

The modular, extensible design allows for:
- Easy addition of new sensors (data inputs)
- New actuators (trading strategies)
- New activation/plasticity functions
- New mutation operators
- New benchmarks and environments

Whether used for research, trading, or as a platform for exploring neuroevolution, this system provides a powerful foundation for developing intelligent agents in complex, dynamic environments.

---

**Version**: 1.0  
**Last Updated**: November 2025  
**Repository**: /workspace  
**License**: See repository for license information

