# Process Steps: Neural Network Evolution System

## Starting Point: `benchmarker:start(sliding_window_10)`

This document traces the complete execution flow of the neural network evolution system starting from the benchmarker initialization.

---

## Phase 1: Benchmarker Initialization

### Step 1: Benchmarker Setup
- **Function**: `benchmarker:start(sliding_window_10)`
- **Process**: Creates experiment record with parameters:
  - Population ID: `test`
  - Survival percentage: from `config:survival_percentage()`
  - Species size limit: from `config:specie_size_limit()`
  - Initial species size: from `config:init_specie_size()`
  - Polis ID: `mathema`
  - Generation limit: `inf`
  - Evaluations limit: from `config:evaluations_limit()`
  - Fitness goal: `inf`
  - Total runs: from `config:tot_runs()`

### Step 2: Experiment Record Creation
- Creates `#experiment{}` record with:
  - ID: `sliding_window_10`
  - Backup flag: `true`
  - PM parameters: Population Monitor parameters
  - Initial constraints: `?INIT_CONSTRAINTS` (forex_trader morphology with feedforward architecture)
  - Progress flag: `in_progress`
  - Run index: `1`
  - Start timestamp

### Step 3: Benchmarker Process Spawn
- Writes experiment to database via `genotype:write(E)`
- Registers benchmarker process
- Spawns `benchmarker:prep/1` process

---

## Phase 2: Population Monitor Preparation

### Step 4: Population State Preparation
- **Function**: `population_monitor:prep_PopState/2`
- Sets up population monitor state with benchmarker parameters
- Calls `init_population/2` with state and species constraints

### Step 5: Population Creation/Validation
- **Function**: `init_population/2`
- Seeds random number generator
- Checks if population with ID `test` exists
- If exists: deletes old population via `delete_population/1`
- Creates fresh population via `create_Population/3`

### Step 6: Species and Agent Construction
- **Function**: `create_Population/3`
- Creates species based on `Specie_Constraints`
- For each species constraint:
  - Calls `create_specie/6` with specified species size
  - Constructs agents via `genotype:construct_Agent/3`

### Step 7: Agent Genotype Construction
- **Function**: `genotype:construct_Agent/3`
- For each agent:
  - Sets generation to 0
  - Determines encoding type (neural vs substrate)
  - Creates cortex via `construct_Cortex/6`
  - Sets up agent parameters:
    - Tuning selection function
    - Annealing parameter
    - Perturbation range
    - Mutation operators
  - Writes agent record to database

### Step 8: Neural Network Architecture Creation
- **Function**: `construct_Cortex/6`
- Based on morphology (forex_trader):
  - Creates sensors via `morphology:get_InitSensors/1`
  - Creates actuators via `morphology:get_InitActuators/1`
  - For substrate encoding:
    - Creates substrate with CPPs (Cartesian Pole Placement) and CEPs (Cartesian End Points)
    - Sets up substrate dimensions and densities
  - Constructs initial neural layer via `construct_InitialNeuroLayer/6`
  - Creates cortex record linking all components

---

## Phase 3: Population Monitor Startup

### Step 9: Population Monitor Process Launch
- **Function**: `population_monitor:start/1`
- Spawns gen_server process
- Registers as `monitor`
- Calls `init/1` with population state

### Step 10: Population Monitor Initialization
- **Function**: `population_monitor:init/1`
- Sets process flag `trap_exit`
- Extracts all agent IDs from population
- Initializes evaluation counters for each species
- Reads population trace for total evaluations
- Sets up state record with:
  - Active agent ID/PID pairs
  - Total agents count
  - Evolutionary algorithm (generational)
  - Fitness postprocessor
  - Selection algorithm

### Step 11: Agent Summoning
- **Function**: `summon_agents/2`
- For each agent ID:
  - Spawns exoself process via `exoself:start/3`
  - Creates {Agent_Id, Agent_PId} tuple
- Returns list of active agent processes

---

## Phase 4: Exoself (Agent) Initialization

### Step 12: Exoself Process Setup
- **Function**: `exoself:prep/3`
- Seeds random number generator
- Creates ETS table for ID/PID mappings
- Reads agent and cortex records from database
- Extracts sensor, actuator, and neuron IDs

### Step 13: Scape Spawning
- **Function**: `spawn_Scapes/4`
- Identifies unique scapes from sensors and actuators
- Spawns private scapes (fx_sim for forex trading)
- Creates scape PID mappings in ETS table

### Step 14: Cerebral Unit Spawning
- **Function**: `spawn_CerebralUnits/3`
- Spawns processes for each component type:
  - Cortex process
  - Sensor processes
  - Actuator processes  
  - Neuron processes
  - Substrate components (if substrate encoding)

### Step 15: Neural Network Linking
- Links all spawned processes:
  - **Sensors**: Links to cortex, scape, and fanout neurons
  - **Actuators**: Links to cortex, scape, and fanin neurons
  - **Neurons**: Links input/output connections with weights
  - **Cortex**: Links to all sensors, neurons, and actuators

---

## Phase 5: Neural Network Execution Loop

### Step 16: Cortex Activation
- **Function**: `cortex:loop/9`
- Cortex sends sync signals to all sensors
- Enters main Sense-Think-Act cycle

### Step 17: Sensor Processing
- **Function**: `sensor:loop/8`
- Receives sync from cortex
- Executes sensor function (e.g., `fx_PCI`, `fx_PLI` for forex data)
- Sends sensory data to connected neurons

### Step 18: Neural Processing
- **Function**: `neuron:loop/6`
- Neurons receive inputs from sensors/other neurons
- Apply aggregation function to weighted inputs
- Apply activation function
- Send output to connected neurons/actuators
- Handle plasticity updates if enabled

### Step 19: Actuator Processing
- **Function**: `actuator:loop/8`
- Collects signals from all fanin neurons
- Executes actuator function (e.g., `fx_Trade` for trading decisions)
- Sends sync signal back to cortex with fitness and end flag

### Step 20: Cycle Completion
- Cortex receives sync from all actuators
- Accumulates fitness and cycle count
- Checks for end conditions
- If not ended: triggers next Sense-Think-Act cycle
- If ended: sends evaluation_completed to exoself

---

## Phase 6: Agent Evolution and Tuning

### Step 21: Fitness Evaluation
- **Function**: `exoself:loop/2` (gt mode)
- Receives evaluation_completed from cortex
- Compares current fitness to highest achieved
- If better: backs up neural weights
- If worse: restores previous best weights

### Step 22: Weight Perturbation
- If max attempts not reached:
  - Selects neurons for perturbation via tuning selection function
  - Sends weight_perturb messages to selected neurons
  - Reactivates cortex for next evaluation cycle

### Step 23: Agent Termination
- If max attempts reached or goal achieved:
  - Backs up final genotype to database
  - Terminates all neural network processes
  - Sends termination message to population monitor

---

## Phase 7: Population Evolution

### Step 24: Generation Processing
- **Function**: `population_monitor:handle_cast/2`
- Receives terminated messages from all agents
- When all agents complete:
  - Calls `mutate_population/4` for generational evolution
  - Increments population generation

### Step 25: Species Mutation
- **Function**: `mutate_Specie/5`
- Calculates species fitness statistics
- Sorts agents by fitness
- Applies fitness postprocessor
- Runs selection algorithm to choose survivors
- Creates mutant offspring from survivors
- Updates species records

### Step 26: Next Generation Launch
- Checks termination conditions:
  - Generation limit reached
  - Evaluation limit reached  
  - Fitness goal achieved
- If continuing: spawns new generation of agents
- If terminating: proceeds to completion

---

## Phase 8: Experiment Completion

### Step 27: Population Monitor Termination
- **Function**: `population_monitor:terminate/2`
- Gathers final statistics
- Updates population trace with total evaluations
- Writes final population state to database
- Sends completion message to benchmarker

### Step 28: Benchmarker Completion
- **Function**: `benchmarker:loop/2`
- Receives completion message from population monitor
- Accumulates trace data
- Checks if all runs completed
- If more runs needed: starts next run
- If all runs done: generates final report

### Step 29: Report Generation
- **Function**: `benchmarker:report/2`
- Writes trace data to file: `benchmarks/report_Trace_Acc`
- Prepares performance graphs
- Writes graph data to file: `benchmarks/graph_forex_trader_report_Graphs`
- Calculates and displays final statistics

---

## Key Data Flows

### Forex Trading Data Flow:
1. **Sensor** (`fx_PCI`/`fx_PLI`) → Requests forex data from scape
2. **Scape** (`fx_sim`) → Retrieves price data from ETS tables
3. **Sensor** → Processes and normalizes data → Sends to neurons
4. **Neurons** → Process signals → Send to actuator
5. **Actuator** (`fx_Trade`) → Makes trading decision → Sends to scape
6. **Scape** → Executes trade → Returns fitness and halt flag

### Evolution Data Flow:
1. **Agent fitness** → Population monitor
2. **Population statistics** → Species mutation
3. **Survivor selection** → Offspring creation
4. **Mutated genotypes** → Next generation agents
5. **Generation statistics** → Benchmarker
6. **Final results** → Report files

This completes the full execution cycle from benchmarker initialization through neural network evolution to final report generation.
---


## Phase 9: Adjacent Data Range Testing (Benchmark Mode)

### Step 30: Benchmark Mode Activation
- **Context**: After evolution completes, the system can test evolved agents on adjacent data
- **Function**: Agents spawned with `OpMode = benchmark` instead of `gt`
- **Purpose**: Evaluate performance on data immediately following the training period

### Step 31: Data Range Switching
- **Function**: `sensor:fx_PCI/4` and `sensor:fx_PLI/4`
- **Training Data Range**: `config:data_start_index()` to `config:data_end_index()`
- **Testing Data Range**: `config:data_end_index()` to `config:benchmark_end_index()`
- **Key Point**: Testing uses the **next 200 data points** after training data ends

### Step 32: Sensor Benchmark Behavior
```erlang
fx_PCI(Exoself_Id,VL,Parameters,Scape)->
    case get(opmode) of
        gt ->
            % Training: e.g., rows 1000-8000
            Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:data_start_index(),config:data_end_index()};
        benchmark ->
            % Testing: e.g., rows 8000-8200 (adjacent unseen data)
            Scape ! {self(),sense,config:primary_currency_pair(),close,[HRes,VRes,graph_sensor],config:data_end_index(),config:benchmark_end_index()}
    end
```

### Step 33: Benchmark Execution Flow
- **Exoself Mode**: `exoself:loop/2` with `benchmark` parameter
- **Single Evaluation**: Agent runs once through the test data (no weight tuning)
- **Immediate Termination**: After one evaluation cycle:
  - Terminates all neural network processes
  - Reports final fitness, cycles, and time
  - Sends `benchmark_complete` message to population monitor

### Step 34: Benchmark vs Training Differences

| Aspect              | Training (gt mode)                        | Benchmark (benchmark mode)           |
|---------------------|-------------------------------------------|---------------------------------------|
| **Data Range**      | `data_start_index` → `data_end_index`     | `data_end_index` → `benchmark_end_index` |
| **Weight Updates**  | Yes (tuning/perturbation)                 | No (frozen weights)                   |
| **Evaluation Cycles** | Multiple attempts (up to `max_attempts`) | Single evaluation only                |
| **Purpose**         | Optimize neural weights                   | Test generalization                   |
| **Duration**        | Extended training period                  | Quick validation run                  |

### Step 35: Adjacent Data Testing Rationale
- **Temporal Continuity**: Tests on immediately following time periods
- **Market Conditions**: Evaluates performance on recent but unseen market data
- **Overfitting Detection**: Reveals if agent learned patterns vs. memorized specific data points
- **Realistic Validation**: Simulates forward-testing in live trading scenarios

### Step 36: Benchmark Results Integration
- **Performance Comparison**: Benchmark fitness vs. training fitness
- **Generalization Measure**: How well evolved strategies perform on fresh data
- **Model Selection**: Could be used to choose best performing agents across different data periods
- **Reporting**: Benchmark results included in final performance analysis

### Limitations of Current Testing Approach:
1. **Adjacent Data Bias**: Test data immediately follows training data (temporal leakage)
2. **Limited Scope**: Only 200 data points for testing vs. thousands for training
3. **Same Market Conditions**: Test period may have similar characteristics to training period
4. **No Cross-Validation**: Single test period rather than multiple validation sets

### Note on Data Leakage:
While this provides some validation, using immediately adjacent data can still suffer from temporal correlation and doesn't constitute true out-of-sample testing. For robust validation, the test data should be from a completely different time period or market conditions.
--
-

## Independent Benchmark Testing of Saved Agents

### Running Benchmark Tests on Existing Mnesia Agents

After your evolution process completes, you can independently test any saved agent from the Mnesia database without modifying code. Here's how:

### Prerequisites:
1. **Mnesia Database**: Must contain evolved agents from previous runs
2. **FX Data**: ETS tables must be loaded with forex data (`fx:init()` and `fx:start()`)
3. **Polis**: System must be running (`polis:start()`)

### Method 1: Direct Exoself Spawning
```erlang
% 1. Find an agent ID from your population
Agent_Id = {your_agent_id, agent}.

% 2. Spawn the agent directly in benchmark mode
Agent_PId = exoself:start(Agent_Id, self(), benchmark).

% 3. Wait for the benchmark result
receive
    {Agent_PId, benchmark_complete, Specie_Id, Fitness, Cycles, Time} ->
        io:format("Benchmark Results:~n"),
        io:format("  Fitness: ~p~n", [Fitness]),
        io:format("  Cycles: ~p~n", [Cycles]),
        io:format("  Time: ~p microseconds~n", [Time])
end.
```

### Method 2: Using Population Monitor Test Mode
```erlang
% 1. Create a minimal population state for testing
TestState = #state{
    op_mode = benchmark,
    population_id = test,  % or your existing population ID
    agents_left = 1
}.

% 2. Start population monitor with single agent
population_monitor:start(TestState).

% 3. The system will automatically run benchmark on all agents in the population
```

### Method 3: Champion Agent Testing
```erlang
% 1. Extract champion agents from existing species
Population_Id = test,  % your population ID
Champion_Ids = population_monitor:extract_AgentIds(Population_Id, champions).

% 2. Test each champion individually
lists:foreach(fun(Agent_Id) ->
    Agent_PId = exoself:start(Agent_Id, self(), benchmark),
    receive
        {Agent_PId, benchmark_complete, Specie_Id, Fitness, Cycles, Time} ->
            io:format("Champion ~p: Fitness=~p, Cycles=~p~n", [Agent_Id, Fitness, Cycles])
    end
end, Champion_Ids).
```

### Step-by-Step Manual Process:

#### Step 1: System Preparation
```erlang
% Start the required systems
mnesia:start().
fx:init().
fx:start().
polis:start().
```

#### Step 2: Find Available Agents
```erlang
% List all populations
mnesia:dirty_all_keys(population).

% Get agents from a specific population
P = genotype:dirty_read({population, test}),
Specie_Ids = P#population.specie_ids,

% Get agents from first species
[First_Specie|_] = Specie_Ids,
S = genotype:dirty_read({specie, First_Specie}),
Agent_Ids = S#specie.agent_ids.
```

#### Step 3: Run Benchmark Test
```erlang
% Pick the first agent
[Test_Agent|_] = Agent_Ids,

% Spawn in benchmark mode
Test_PId = exoself:start(Test_Agent, self(), benchmark),

% Wait for results
receive
    {Test_PId, benchmark_complete, Specie_Id, Fitness, Cycles, Time} ->
        io:format("=== Benchmark Results ===~n"),
        io:format("Agent: ~p~n", [Test_Agent]),
        io:format("Fitness: ~p~n", [Fitness]),
        io:format("Cycles: ~p~n", [Cycles]),
        io:format("Time: ~p microseconds~n", [Time]),
        io:format("========================~n")
after 30000 ->
    io:format("Benchmark test timed out~n")
end.
```

### What Happens During Independent Benchmark:

1. **Agent Reconstruction**: The exoself reads the saved genotype from Mnesia and reconstructs the neural network
2. **Benchmark Mode**: Sensors automatically switch to benchmark data range (`data_end_index` to `benchmark_end_index`)
3. **Single Run**: Agent performs one complete evaluation cycle with frozen weights
4. **Results**: Returns fitness, cycle count, and execution time
5. **Cleanup**: All processes terminate automatically after reporting

### Key Advantages:
- **No Code Changes**: Uses existing benchmark infrastructure
- **Preserved Weights**: Tests the exact evolved neural network weights
- **Isolated Testing**: Each test runs independently without affecting saved genotypes
- **Multiple Agents**: Can test any number of saved agents sequentially or in parallel

### Data Range Used:
- **Training Data**: `config:data_start_index()` to `config:data_end_index()`
- **Benchmark Data**: `config:data_end_index()` to `config:benchmark_end_index()`
- **Test Period**: Typically the next 200 data points after training

This approach allows you to validate your evolved agents on the benchmark dataset anytime after evolution completes, providing a quick way to assess generalization performance without running the full evolution process again.---


## Quick Benchmark: Testing Your Best Agent

After evolution completes, you can quickly test any saved agent on benchmark data. Here's the streamlined process:

### Manual Benchmark Steps:
```erlang
% 1. System startup
mnesia:start().                              % Start database
fx:init().                                   % Initialize forex data  
fx:start().                                  % Start forex tables
polis:start().                               % Start polis system

% 2. Find and test agent
All_Agent_Keys = mnesia:dirty_all_keys(agent).  % Get all saved agents
[Test_Agent|_] = All_Agent_Keys.                % Pick first agent
Test_PId = exoself:start(Test_Agent, self(), benchmark),  % Spawn in benchmark mode
receive
    {Test_PId, benchmark_complete, _, Fitness, Cycles, Time} ->
        io:format("Fitness: ~p | Cycles: ~p | Time: ~p μs~n", [Fitness, Cycles, Time])
after 30000 ->
    io:format("Benchmark timeout~n")
end.
```

### Optional: Enable Debug Mode
```erlang
% Edit config.erl: actuator_debug_tag() -> true.
make:all().                                  % Recompile with debug on
% Run benchmark - will show each trade decision with 1-second delays
```