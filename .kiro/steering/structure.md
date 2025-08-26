# Project Structure

## Root Directory Layout
```
├── *.erl                    # Core Erlang modules (main system components)
├── *.beam                   # Compiled Erlang bytecode files
├── records.hrl              # Central record definitions for all data structures
├── Dockerfile               # Container setup for development environment
├── Process_Steps.md         # Detailed system execution flow documentation
└── README.md               # Basic setup and usage instructions
```

## Core System Modules

### Neural Network Components
- **`neuron.erl`** - Individual neural processing units with weights and activation functions
- **`sensor.erl`** - Input components that interface with external data sources (forex)
- **`actuator.erl`** - Output components that execute trading decisions
- **`cortex.erl`** - Central coordinator for neural network execution cycles

### Genetic Algorithm Engine
- **`genotype.erl`** - Agent construction and genotype manipulation
- **`genome_mutator.erl`** - Genetic mutation operations for neural networks
- **`population_monitor.erl`** - Manages evolution across generations
- **`selection_algorithm.erl`** - Fitness-based selection for breeding

### System Architecture
- **`polis.erl`** - Central platform coordinator and process registry
- **`exoself.erl`** - Agent lifecycle management and neural network spawning
- **`scape.erl`** - Environment interface for sensors and actuators

### Specialized Components
- **`substrate.erl`** - Alternative neural architecture using spatial coordinates
- **`substrate_cpp.erl`** / **`substrate_cep.erl`** - Substrate connection endpoints
- **`morphology.erl`** - Defines neural network topologies (forex_trader, etc.)
- **`plasticity.erl`** - Synaptic weight adaptation algorithms

### Forex Trading System
- **`fx.erl`** - Forex data management and ETS table operations
- **`benchmarker.erl`** - Experiment orchestration and performance evaluation
- **`fitness_postprocessor.erl`** - Fitness score normalization and adjustment

### Utility Modules
- **`functions.erl`** - Mathematical functions for neural processing
- **`config.erl`** - System configuration parameters
- **`genotype_utils.erl`** - Helper functions for genotype analysis
- **`signal_aggregator.erl`** - Neural signal processing utilities

## Data Directories

### Database Storage
```
├── Mnesia.nonode@nohost/    # Active Mnesia database files
│   ├── *.DCD               # Data files for each record type
│   ├── *.LOG               # Transaction logs
│   └── schema.DAT          # Database schema
└── data/Mnesia.nonode@nohost/  # Backup database location
```

### Forex Data
```
└── fx_tables/
    ├── EURUSD1             # 1-minute EUR/USD price data
    ├── EURUSD15            # 15-minute aggregated data
    ├── EURUSD30            # 30-minute aggregated data
    ├── EURUSD60            # 1-hour aggregated data
    └── metadata            # Data range and indexing information
```

### Experiment Results
```
└── benchmarks/
    ├── report_Trace_Acc                    # Evolution trace data
    ├── graph_forex_trader_report_Graphs    # Performance visualization data
    └── graph_forex_trader_1m_report_Graphs # 1-minute timeframe results
```

## Key Architectural Patterns

### Module Naming Conventions
- **Core Components**: Single word (neuron, sensor, actuator, cortex)
- **System Services**: Compound names (population_monitor, genome_mutator)
- **Utilities**: Descriptive names (genotype_utils, signal_aggregator)
- **Domain-Specific**: Prefixed (fx.erl for forex, substrate_*.erl for substrates)

### File Organization Principles
- **Flat Structure**: All core modules in root directory for easy access
- **Data Separation**: Database and forex data in dedicated subdirectories
- **Results Isolation**: Benchmark outputs in separate benchmarks/ folder
- **Configuration Centralization**: System parameters in config.erl and records.hrl

### Process Architecture
- **One Module Per Process Type**: Each .erl file typically represents a distinct process type
- **Shared Records**: All data structures defined in central records.hrl
- **Hot Reloading**: All modules support sync() for development iteration
- **Distributed Design**: Modules designed to work across Erlang node clusters

### Development Workflow
1. **Edit** Erlang source files (.erl)
2. **Compile** with `make:all([load])` or module-specific `sync()`
3. **Test** in Erlang shell with live system
4. **Persist** results automatically saved to Mnesia database
5. **Analyze** using genotype_utils and benchmark reports