# Instructions on how to build and run this Dockerfile and the Erlang environment:

### 1. Build the Docker image (ONLY ONCE):
    # a. Open terminal/PowerShell.
    # b. Navigate to the directory containing this Dockerfile.
    # c. Run: docker build -t erlang-dev .
# **********************************************************************
    ### 2. Run the Docker container:
    # Run: docker run -it --rm -v ${PWD}:/app -w /app erlang-dev

## Windows (other shells):
    # Command Prompt: docker run -it --rm -v %cd%:/app -w /app erlang-dev
    # Git Bash: docker run -it --rmdocker build -t erlang-dev . -v $(pwd):/app -w /app erlang-dev

## Data Configuration

This system supports multiple EURUSD timeframes for forex trading evolution:
- **EURUSD1** - 1-minute intervals (for high-frequency trading)
- **EURUSD15** - 15-minute intervals (CURRENTLY ACTIVE)
- **EURUSD30** - 30-minute intervals
- **EURUSD60** - 60-minute intervals

The neural networks are currently configured to use **EURUSD15** (15-minute data) for:
- **fx_PCI sensor** - Price Chart Input (50x20 resolution chart encoding)
- **fx_PLI sensor** - Price List Input (10 data points sliding window)

To switch to other timeframes, modify the sensor configurations in `sensor.erl`.

## Initialize and start fx.erl
    make:all().
    fx:init().
    fx:start().
    polis:start().
    benchmarker:start(sliding_window_5).
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
    # Stop mnesia
    mnesia:stop().

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