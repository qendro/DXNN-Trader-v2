# Configuration Parameter Analysis

## List 1: Parameters Currently Hard-Coded in Your Codebase

These parameters are actually hardcoded somewhere in your code and should be moved to config:

### **Trading Parameters (fx.erl)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `account_leverage` | 50 | fx.erl | 86 |
| `account_initial_balance` | 300 | fx.erl | 86 |
| `account_lot_size` | 10000 | fx.erl | 86 |
| `account_spread` | 0.000150 | fx.erl | 86 |
| `account_margin` | 0 | fx.erl | 86 |
| `order_size_percentage` | 0.2 | fx.erl | 530 |
| `buy_money_fixed` | 100 | fx.erl | 531 |
| `min_profit_threshold` | 0.000150 | fx.erl | 155-156 |

### **Data Parameters (sensor.erl, fx.erl)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `primary_currency_pair` | 'EURUSD15' | sensor.erl | 55, 72 |
| `data_start_index` | 1000 | sensor.erl | 55, 72 |
| `data_end_index` | 200 | sensor.erl | 55, 72 |
| `benchmark_end_index` | last | sensor.erl | 58, 75 |
| `market_props_start` | 800 | fx.erl | 123 |
| `market_props_end` | 200 | fx.erl | 123 |

### **Sensor Configuration (morphology.erl)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `pli_resolutions` | [10] | morphology.erl | 105 |
| `pci_horizontal_resolutions` | [50] | morphology.erl | 106 |
| `pci_vertical_resolutions` | [20] | morphology.erl | 106 |
| `pli_1m_resolutions` | [5,10,20] | morphology.erl | 120 |
| `pci_1m_horizontal_resolutions` | [20,30] | morphology.erl | 121 |
| `pci_1m_vertical_resolutions` | [10,15] | morphology.erl | 121 |
| `internal_sensor_dimensions` | 3 | morphology.erl | 108, 122 |

### **Evolution Parameters (benchmarker.erl)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `specie_size_limit` | 100 | benchmarker.erl | 26 |
| `init_specie_size` | 100 | benchmarker.erl | 27 |
| `evaluations_limit` | 10000 | benchmarker.erl | 30 |
| `survival_percentage` | 0.5 | benchmarker.erl | 26 |

### **Neural Network Parameters (records.hrl)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `tuning_duration` | {const,10} | records.hrl | 31 |
| `annealing_parameters` | [1] | records.hrl | 32 |
| `perturbation_ranges` | [1] | records.hrl | 34 |
| `neural_activation_functions` | [tanh,cos,gaussian,absolute] | records.hrl | 25 |
| `neural_plasticity_functions` | [none] | records.hrl | 26 |
| `agent_encoding_types` | [neural] | records.hrl | 35 |

### **System Constants (various files)**
| Parameter | Current Hard-Coded Value | Location | Line # |
|-----------|-------------------------|----------|--------|
| `fx_tables_directory` | "fx_tables/" | fx.erl | 13 |
| `source_directory` | "fx_tables/" | fx.erl | 14 |
| `actuator_debug_tag` | false | fx.erl | 81 |
| `sensor_debug_tag` | false | fx.erl | 82 |

---

## List 2: Parameters NOT Currently Hard-Coded (Added by Me)

These parameters don't exist in your current codebase but would be useful additions:

### **Risk Management Parameters (Not Currently Implemented)**
| Parameter | Suggested Value | Purpose |
|-----------|----------------|---------|
| `max_open_positions` | 1 | Limit concurrent trades |
| `max_daily_trades` | 10 | Daily trade limit |
| `max_drawdown_percentage` | 0.20 | Maximum portfolio drawdown |
| `risk_per_trade` | 0.02 | Risk percentage per trade |
| `stop_loss_percentage` | 0.05 | Automatic stop loss |
| `take_profit_percentage` | 0.10 | Automatic take profit |

### **Extended Data Parameters (Not Currently Used)**
| Parameter | Suggested Value | Purpose |
|-----------|----------------|---------|
| `available_pairs` | ['EURUSD1', 'EURUSD15', 'EURUSD30', 'EURUSD60'] | Multi-pair support |
| `default_timeframe` | 15 | Default timeframe in minutes |
| `total_data_rows` | 10000 | Total available data rows |
| `enable_price_normalization` | true | Price data normalization |
| `normalization_method` | l2_norm | Normalization algorithm |

### **Advanced Neural Network Parameters (Not Currently Used)**
| Parameter | Suggested Value | Purpose |
|-----------|----------------|---------|
| `connection_architecture` | feedforward | Network topology |
| `substrate_plasticities` | [none] | Substrate learning |
| `substrate_linkforms` | [l2l_feedforward] | Substrate connections |
| `mutation_probability` | 0.1 | Mutation rate |
| `delta_multiplier` | 6.28318 | Weight change multiplier |
| `saturation_limit` | 6.28318 | Activation saturation |

### **System Performance Parameters (Not Currently Used)**
| Parameter | Suggested Value | Purpose |
|-----------|----------------|---------|
| `heartbeat_interval` | 5000 | System heartbeat (ms) |
| `backup_interval` | 300000 | Auto-backup interval |
| `max_simulation_time` | 600000 | Simulation timeout |
| `max_concurrent_agents` | 10 | Concurrency limit |
| `worker_pool_size` | 4 | Worker thread pool |

### **Logging & Debug Parameters (Not Currently Used)**
| Parameter | Suggested Value | Purpose |
|-----------|----------------|---------|
| `enable_trade_logging` | true | Trade logging flag |
| `log_level` | info | Logging verbosity |
| `benchmark_directory` | "benchmarks/" | Benchmark output path |

---

## Summary

**Actually Hard-Coded (Priority 1):** 25 parameters
- These are currently hardcoded in your files and should be moved to config immediately

**Nice-to-Have Additions (Priority 2):** 23 parameters  
- These would add new functionality but require code changes to implement

## Recommendation

Start with **List 1** parameters only. These will give you immediate benefits without requiring new feature development. You can add **List 2** parameters later as you enhance your trading system with new features like risk management, multi-pair trading, etc.

The **List 1** parameters alone will make your system much more configurable and professional!