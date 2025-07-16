# Instructions on how to build and run this Dockerfile and the Erlang environment:

### 0. Git Workflow
    git add .       #Saves a new version to local git
    git commit -m "Describe what changed"   #with comments
    git push        #Saves it to Github

    3. Optional - Use Branches (For New Features or Experiments):

    git checkout -b new-feature
    #Work on your new feature, then:
        git add .
        git commit -m "Added new feature"
        git push -u origin new-feature

        git log --oneline   # Log of all versions
        git log --stat      # Log with changes



### 1. Build the Docker image (ONLY ONCE):
    # a. Open terminal/PowerShell.
    # b. Navigate to the directory containing this Dockerfile.
    # c. Run: docker build -t erlang-dev .
# **********************************************************************
    ### 2. Run the Docker container:
    # Run: docker run -it --rm -v ${PWD}:/app -w /app erlang-dev

## Windows (other shells):
    # Command Prompt: docker run -it --rm -v %cd%:/app -w /app erlang-dev
    # Git Bash: docker run -it --rm -v $(pwd):/app -w /app erlang-dev

## Experiment Configuration

This system now supports configurable experiment parameters via the `config.erl` module. You can easily switch between different datasets, population sizes, and other experiment settings without modifying the core code.

### Configuration Parameters

Edit `config.erl` to change experiment settings:

```erlang
% In config.erl, modify these values:
get(fx_table, _Default) -> 'EURUSD15';        % Data source to use
get(specie_size_limit, _Default) -> 10;       % Population size per species  
get(agent_encoding, _Default) -> neural;      % Neural network encoding
get(selection_algorithm, _Default) -> top3;   % Selection method
get(op_mode, _Default) -> benchmark;          % Operation mode
```

### Available Data Sources

The system includes multiple EURUSD timeframes:
- **EURUSD1** - 1-minute intervals (for high-frequency trading)
- **EURUSD15** - 15-minute intervals (DEFAULT)
- **EURUSD30** - 30-minute intervals  
- **EURUSD60** - 60-minute intervals

### Quick Start Workflow

**Step 1: Start Docker and Erlang Shell**
```bash
docker run -it --rm -v $(pwd):/app -w /app erlang-dev
```
q
**Step 2: Initialize System (in Erlang shell)**
```erlang
mnesia:create_schema([node()]).
mnesia:start().
make:all().
fx:init().
fx:start().
polis:create().
polis:start().
```

**Step 3: Run Experiment**
```erlang
benchmarker:start(sliding_window_5).
```

**Step 4: Check Results**
```erlang
genotype_utils:print_best_genotype(all).
```

### How to Use Different Configurations

1. **Change the data source**: Edit `fx_table` in `config.erl`
2. **Recompile**: Run `make:all().` in the Erlang shell
3. **Run experiment**: `benchmarker:start(sliding_window_5).`

**Example - Switch to 1-minute data:**
```erlang
% Edit config.erl: 
get(fx_table, _Default) -> 'EURUSD1';

% Then in Erlang shell:
make:all().
benchmarker:start(sliding_window_10).

## Initialize and start fx.erl
    
c(actuator).
c(benchmarker).
c(cortex).
c(exoself).
c(fitness_postprocessor).
c(functions).
c(fx).
c(genome_mutator).
c(genotype_utils).
c(genotype).
c(morphology).
c(neuron).
c(plasticity).
c(polis).
c(population_monitor).
c(scape).
c(selection_algorithm).
c(sensor).
c(signal_aggregator).
c(substrate_cep).
c(substrate_cpp).
c(substrate).
c(tot_topological_mutations).
c(tuning_duration).
c(tuning_selection).

    make:all().
    mnesia:create_schema([node()]).
    mnesia:start().
    fx:init().
    fx:start().
    polis:create().
    polis:start().
    polis:sync().
    benchmarker:start(sliding_window_5).
    
    benchmarker:start(chart_plane_5x10).

    benchmarker:start(sliding_window_5).
    .


    benchmarker:start(sliding_window_10).
    benchmarker:start(sliding_window_20).
    benchmarker:start(sliding_window_50).
    benchmarker:start(sliding_window_100).
    benchmarker:start(chart_plane_5x10).
    benchmarker:start(chart_plane_5x20).
    benchmarker:start(chart_plane_10x10).
    benchmarker:start(chart_plane_10x20).
    benchmarker:start(chart_plane_20x10).
    benchmarker:start(chart_plane_20x20).
    benchmarker:start(chart_plane_50x10).
    benchmarker:start(chart_plane_50x20).
    benchmarker:start(chart_plane_100x10).


# Reseting Mnesia
   #bash find . -name "*.beam" -delete
    # Stop mnesia
    mnesia:stop().
    mnesia:delete_schema([node()]).
    q().

    # Delete the entire database
    mnesia:delete_schema([node()]).
    q().

    % Restart and recreate (you'll need to reinitialize your system)
    docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
    mnesia:create_schema([node()]).
    mnesia:start().
    make:all().
    fx:init().    % This will recreate the FX ETS tables
    fx:start().
    polis:create().
    polis:start().
    polis:sync().         % Recompiles and reloads updated modules
    benchmarker:start(sliding_window_10).

## How to print the best genotype from Mnesia:

1. **Start Mnesia:**
   ```erlang
   mnesia:start().
   ```

4. **Print the best genotype:**
   ```erlang
   % Print the best genotype from the default 'test' population
   genotype_utils:print_best_genotype().
   
   % Or specify a population ID
   genotype_utils:print_best_genotype(your_population_id).
   
   % List all agents with their fitness scores
   genotype_utils:list_all_agents().
   
   % Print top N agents
   genotype_utils:print_top_agents(5).
   
   % Get agent statistics
   genotype_utils:get_agent_stats().
   ```

## Alternative method using existing genotype module:

If you know the specific agent ID, you can use the existing print function:
```erlang
genotype:print(Agent_Id).
```

## Finding all populations:
```erlang
% Get all population keys
mnesia:dirty_all_keys(population).
genotype_utils:print_best_genotype(test).
% Check all agents regardless of population
genotype_utils:print_best_genotype(all).

% Read a specific population
genotype:dirty_read({population, Population_Id}).
```



# My Erlang Project

## Description
Brief overview of what the project does.

## Folder Structure
- `src/` - Erlang source files
- `ebin/` - Compiled beam files
- `include/` - Header files
- `test/` - Test files


## How to Build

#Speed Test
# 1. Mac (old Docker settings): 7min 27sec
# 2. Mac (max Docker settings): 8min 16sec
# 3. Win :                      13mn 28sec


## How to Run Your Best Agent:

```erlang
% 1. Load and start your best agent
Best_Agent_Id = {5.709358053012848e-10,agent}.  % Update with your latest
Agent_PId = exoself:start(Best_Agent_Id, self()).

% 2. Monitor the agent
is_process_alive(Agent_PId).

% 3. Simple test run
run_best_agent() ->
    {atomic, Best_Agent_Id} = genotype_utils:find_best_agent(all),
    Agent_PId = exoself:start(Best_Agent_Id, self()),
    timer:sleep(30000),  % Run for 30 seconds
    Agent_PId ! {self(), terminate}.
```

## Improving Evolution Performance:

**Current Setting** (very limited evolution):
```erlang
evaluations_limit = 10,  % Only 10 evaluations
```

**Recommended Settings** for better agents:
```erlang
% In benchmarker.erl, change to:
evaluations_limit = 100,   % Quick test (5 min)
evaluations_limit = 1000,  % Good evolution (30 min)  
evaluations_limit = 10000, % Full evolution (3-5 hours)
```

**Why longer runs help:**
- Current runs only reach Generation 0-1
- Longer runs = more generations = better evolved agents
- Your 510.91 fitness could improve to 600-800+ with more evolution

## Quick Workflow for Testing Changes:

**Step 1: Recompile and restart**
```erlang
make:all().
benchmarker:start(sliding_window_10).
```

**Step 2: Check results**
```erlang
genotype_utils:print_best_genotype(all).
```

**Step 3: Run your best agent**
```erlang
Best_Agent_Id = {YOUR_LATEST_AGENT_ID,agent}.
Agent_PId = exoself:start(Best_Agent_Id, self()).
```

## Alternative method using existing genotype module:

If you know the specific agent ID, you can use the existing print function:
```erlang
genotype:print(Agent_Id).
```

## Finding all populations:
```erlang
% Get all population keys
mnesia:dirty_all_keys(population).

% Check all agents regardless of population  
genotype_utils:print_best_genotype(all).

% Read a specific population
genotype:dirty_read({population, Population_Id}).
```