# DXNN-Trader-v2: Distributed Neural Network Trading Platform

## 🎯 System Overview

DXNN-Trader-v2 is a sophisticated distributed neural network trading platform written in Erlang that combines neuro-evolution, dynamic substrates, and live trading capabilities. The system evolves trading agents through genetic algorithms and deploys them in both simulated and live market environments.

### Core Architecture Components

**Neuro-Evolution Engine:**
- **Genotype Management**: `genotype.erl` - Agent blueprints stored as genomes
- **Mutation Engine**: `genome_mutator.erl` - Evolutionary operators for genome modification
- **Population Control**: `population_monitor.erl` - Manages agent populations and speciation
- **Selection**: `selection_algorithm.erl` - Survival strategies (competition, top3)

**Neural Network Runtime:**
- **Agent Controller**: `exoself.erl` - Maps genotype→phenotype, manages agent lifecycle
- **Central Coordinator**: `cortex.erl` - Synchronizes Sense-Think-Act cycles
- **Neural Components**: `sensor.erl`, `neuron.erl`, `actuator.erl` - Distributed neural processes
- **Substrate Layers**: `substrate.erl`, `substrate_cep.erl`, `substrate_cpp.erl` - Advanced neural structures

**Market Environment:**
- **Simulation**: `fx.erl`, `scape.erl` - Historical data and account simulation
- **Live Trading**: `ib_connector.erl`, `live_trader.erl`, `live_scape.erl` - Interactive Brokers integration
- **Data Management**: `fx_tables/` - Historical FX data (EURUSD 1m, 15m intervals)

**Analysis & Monitoring:**
- **Benchmarking**: `benchmarker.erl` - Performance testing and reporting
- **Fitness Processing**: `fitness_postprocessor.erl` - Agent evaluation and ranking
- **Utilities**: `genotype_utils.erl` - Agent inspection and statistics

## 🚀 Quick Start Guide

### Prerequisites
- Docker
- Erlang/OTP 26+ (provided in Docker image)

### Build & Run
```bash
# Compile all modules
make all

# Build and run Docker container
docker build -t erlang-dev .
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
```

### Initialize System
```erlang
% Inside Erlang shell
make:all().
mnesia:create_schema([node()]).
mnesia:start().
fx:init().
fx:start().
polis:create().
polis:start().
polis:sync().
```

### Common Operations
```erlang
% Start benchmark experiment
benchmarker:start(sliding_window_5).

% View best performing agent
genotype_utils:print_best_genotype().

% Run specific agent
exoself:start(BestAgentId, self(), benchmark).

% Start live trading
live_trading_main:start().
```

## 📁 Repository Structure

### Core Neural Components
| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `exoself.erl` | Agent lifecycle manager | `start/1`, `prep/3`, `loop/2` |
| `cortex.erl` | Neural network coordinator | `gen/2`, `prep/1`, `loop/9` |
| `sensor.erl` | Data input processes | `gen/2`, `prep/1`, `loop/8` |
| `neuron.erl` | Neural computation units | `gen/2`, `prep/1`, `loop/8` |
| `actuator.erl` | Output/trading processes | `gen/2`, `prep/1`, `loop/8` |

### Evolution & Genetics
| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `genotype.erl` | Genome persistence & construction | `construct_Agent/3`, `sync/0` |
| `genome_mutator.erl` | Mutation operators | `mutate/1`, `apply_ESMutators/2` |
| `population_monitor.erl` | Population management | `create_MutantAgentCopy/1`, `continue/1` |
| `selection_algorithm.erl` | Survival strategies | `competition/3`, `top3/3` |

### Market & Trading
| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `fx.erl` | FX data & simulation | `create_account/0`, `go/0` |
| `ib_connector.erl` | Interactive Brokers gateway | `start_connection/3`, `place_order/4` |
| `live_trader.erl` | Live trading supervisor | `start_link/0` |
| `live_scape.erl` | Live market environment | `start_link/0` |

### Analysis & Utilities
| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `benchmarker.erl` | Performance testing | `start/1`, `report/2` |
| `genotype_utils.erl` | Agent inspection | `print_best_genotype/0`, `get_agent_stats/0` |
| `fitness_postprocessor.erl` | Fitness evaluation | `none/1`, `size_proportional/1` |

## 🔧 Configuration System

### Configuration Categories (`config.erl`)

**Account Settings:**
- `account_leverage/0`, `account_initial_balance/0`, `account_lot_size/0`
- `account_spread/0`, `account_margin/0`, `order_size_percentage/0`

**Data Parameters:**
- `primary_currency_pair/0`, `data_start_index/0`, `data_end_index/0`
- `benchmark_end_index/0`, `market_props_start/0`, `market_props_end/0`

**Neural Architecture:**
- `neural_activation_functions/0`, `neural_plasticity_functions/0`
- `connection_architecture/0`, `agent_encoding_types/0`

**Live Trading:**
- `ib_host/0`, `ib_port/0`, `ib_client_id/0`
- `live_position_size/0`, `live_max_daily_loss/0`
- `live_currency_pairs/0`, `live_max_drawdown_limit/0`

**Validation Functions:**
- `validate_ib_connection_config/0`, `validate_risk_parameters/0`
- `validate_live_trading_config/0`

## 🔄 System Workflows

### 1. Agent Evolution Cycle
```
1. genotype:construct_Agent/3 → Create initial agent
2. population_monitor:create_specie/3 → Group similar agents
3. exoself:start/1 → Execute agent in simulation
4. benchmarker:start/1 → Run performance tests
5. fitness_postprocessor:none/1 → Rank agents by fitness
6. selection_algorithm:competition/3 → Select survivors
7. genome_mutator:mutate/1 → Create new variants
8. Repeat from step 2
```

### 2. Neural Network Execution
```
1. exoself:start/1 → Spawn agent processes
2. cortex:gen/2 → Create central coordinator
3. sensor:gen/2 → Spawn input processes
4. neuron:gen/2 → Spawn computation units
5. actuator:gen/2 → Spawn output processes
6. cortex:loop/9 → Coordinate Sense-Think-Act cycles
7. sensor:loop/8 → Gather market data
8. neuron:loop/8 → Process neural signals
9. actuator:loop/8 → Execute trading decisions
10. cortex:loop/9 → Accumulate fitness
```

### 3. Live Trading Pipeline
```
1. live_trading_main:start/0 → Initialize live system
2. live_trading_integration:start_live_trading/1 → Start components
3. ib_connector:start_connection/3 → Connect to Interactive Brokers
4. live_scape:start_link/0 → Create live market environment
5. live_trader:start_link/0 → Spawn trading agent
6. Real-time data flow: IB → live_scape → sensor → neuron → actuator → IB
```

## 📊 Data Flow Patterns

### Input Data Sources
- **Historical FX Data**: `fx_tables/EURUSD1.txt`, `fx_tables/EURUSD15.txt`
- **Live Market Data**: Interactive Brokers via `ib_connector`
- **Internal Sensors**: Account balance, position status via `fx_Internals`

### Output Actions
- **Trading Signals**: Buy/sell orders sent to `scape` (simulation) or `ib_connector` (live)
- **Fitness Metrics**: Performance scores returned to `cortex`
- **Logging**: Traces and statistics via `benchmarker`

### Message Patterns
```
% Sensor → Neuron
{forward, InputId, Input, NeuronPid}

% Neuron → Actuator  
{forward, OutputId, Output, ActuatorPid}

% Actuator → Scape
{actuator, ExoSelfPid, Action, ScapePid}

% Scape → Actuator
{result, Fitness, EndFlag}
```

## 🧪 Testing & Validation

### Test Suites
```erlang
% Quick system test
test_live_trading_integration:quick_test().

% Comprehensive test suite
test_live_trading_integration:full_test().

% Component-specific tests
test_live_trading_integration:run_test_suite(configuration_validation).
test_live_trading_integration:run_test_suite(startup_sequence).
```

### Performance Monitoring
```erlang
% View system status
live_trading_main:status().

% Get agent statistics
genotype_utils:get_agent_stats().

% Monitor population
population_monitor:get_stats().
```

## 🔍 Key Data Structures

### Records (`records.hrl`)
- `agent` - Complete agent definition
- `cortex` - Neural network coordinator state
- `neuron` - Individual neuron configuration
- `sensor` - Input sensor specification
- `actuator` - Output actuator specification
- `substrate` - Substrate layer configuration

### Configuration Records
- `constraint` - Evolution constraints
- `mutation_operator` - Mutation parameters
- `tuning_selection` - Neuron selection strategies

## ⚠️ Important Notes

### Safety Considerations
- Live trading defaults to paper trading (port 7497)
- Risk parameters validated before live execution
- Emergency shutdown available via `live_trading_integration:emergency_shutdown/0`

### Performance Considerations
- Mnesia database for persistent storage
- Distributed Erlang processes for scalability
- Circuit breakers in `ib_connector` for fault tolerance

### Development Workflow
- Use `make all` to recompile after changes
- Reset Mnesia with `mnesia:delete_schema([node()])` for clean state
- Check `logs/` directory for runtime information

## 🎯 Common Use Cases

### Research & Development
```erlang
% Run evolutionary experiment
benchmarker:start(chart_plane_5x10).

% Analyze results
genotype_utils:print_top_agents(10).

% Test specific agent
exoself:start(AgentId, self(), test).
```

### Live Trading
```erlang
% Start live system
live_trading_main:start().

% Monitor status
live_trading_main:status().

% Emergency stop
live_trading_main:stop().
```

### System Maintenance
```erlang
% Reset database
mnesia:stop().
mnesia:delete_schema([node()]).

% Clean compiled files
find . -name "*.beam" -delete
```

This comprehensive guide provides AI systems with complete understanding of the DXNN-Trader-v2 architecture, workflows, and operational patterns for effective code analysis and modification.