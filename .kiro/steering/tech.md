# Technology Stack

## Core Technologies
- **Language**: Erlang/OTP - Functional programming with actor model concurrency
- **Database**: Mnesia - Distributed Erlang database for persistent storage of genotypes, populations, and experiments
- **Build System**: Native Erlang compilation with `make:all()` and Makefiles
- **Containerization**: Docker with Erlang 26 base image

## Key Libraries & Frameworks
- **OTP Behaviors**: `gen_server` for stateful processes (population_monitor, polis)
- **ETS Tables**: In-memory storage for forex data and process mappings
- **Random Module**: Erlang's built-in random number generation for genetic operations
- **Records**: Extensive use of Erlang records for data structures (defined in `records.hrl`)

## Development Environment
- **Container**: `docker build -t erlang-dev . && docker run -it --rm -v ${PWD}:/app -w /app erlang-dev`
- **Erlang Shell**: Primary development interface with hot code reloading
- **Language Server**: Erlang LS included in Docker image for IDE support

## Common Commands

### Build & Compilation
```erlang
make:all([load]).          % Compile and hot-reload all modules
sync().                    % Available in most modules for quick recompilation
```

### System Startup
```erlang
mnesia:create_schema([node()]).  % Initialize database schema
mnesia:start().                  % Start Mnesia database
fx:init().                       % Initialize forex data tables
fx:start().                      % Load forex data into ETS
polis:create().                  % Create polis configuration
polis:start().                   % Start coordination system
polis:sync().                    % Sync and reload all modules
```

### Running Experiments
```erlang
benchmarker:start(sliding_window_5).     % Start evolution experiment
benchmarker:start(chart_plane_5x10).     % Alternative morphology
```

### Database Management
```erlang
% Reset Mnesia database
mnesia:stop().
mnesia:delete_schema([node()]).
find . -name "*.beam" -delete    % Clean compiled files
```

### Debugging & Analysis
```erlang
rr("records.hrl").                    % Load record definitions
genotype_utils:print_best_genotype(). % Display best evolved agent
genotype_utils:list_all_agents().     % Show all agents with fitness
```

## Architecture Patterns
- **Actor Model**: Each neural component (neuron, sensor, actuator) runs as separate Erlang process
- **Message Passing**: Synchronous communication between neural network components
- **Hot Code Reloading**: Live system updates without stopping processes
- **Fault Tolerance**: OTP supervision trees for process management
- **Distributed Computing**: Designed for multi-node Erlang clusters