# Implementation Plan

- [x] 1. Create core best agent identification function
  - Implement `find_best_agent/0` function in `genotype_utils.erl` that returns the agent ID instead of just printing
  - Add error handling for cases where no agents exist in the database
  - Write unit tests to verify the function correctly identifies the highest fitness agent
  - _Requirements: 1.1, 1.5_

- [x] 2. Create best agent runner module structure
  - Create `best_agent_runner.erl` module with basic module structure and exports
  - Define record structures for `#agent_run_results{}`, `#trading_decision{}`, and `#run_options{}`
  - Implement module initialization and basic utility functions
  - _Requirements: 1.1, 3.1, 3.2_

- [x] 3. Implement data file loading functionality
  - Add `load_data_file/1` function to `fx.erl` that reads CSV files and creates temporary ETS tables
  - Implement `validate_data_format/1` function to check CSV file structure and content
  - Add `create_temporary_table/1` and `delete_temporary_table/1` functions for table management
  - Write tests for various CSV formats and error conditions
  - _Requirements: 2.1, 2.2, 2.3, 2.4_

- [x] 4. Implement standalone agent execution controller
  - Create `run_agent_standalone/2` function that starts an agent and manages its lifecycle
  - Implement message collection system to capture trading decisions from the agent
  - Add `monitor_agent_execution/2` function to track agent performance and handle timeouts
  - Write tests to verify agent startup, execution, and clean termination
  - _Requirements: 1.2, 1.4, 3.1_

- [x] 5. Implement trading decision collection and tracking
  - Create `collect_trading_decisions/1` function that monitors agent messages and records decisions
  - Implement decision recording with timestamps, prices, and signals
  - Add profit/loss calculation for each trading decision
  - Write tests to verify accurate decision capture and calculation
  - _Requirements: 3.1, 3.2, 3.3_

- [x] 6. Create main interface function
  - Implement `run_best_agent_on_data/1` function that orchestrates the entire workflow
  - Add `run_best_agent_on_data/2` function with options parameter for configuration
  - Integrate agent identification, data loading, execution, and result collection
  - Write integration tests for the complete workflow
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 7. Implement result formatting and reporting
  - Create `format_results/1` function that generates comprehensive trading reports
  - Add summary statistics calculation (total trades, profit/loss, success rate)
  - Implement optional file output for results
  - Write tests to verify result accuracy and formatting
  - _Requirements: 3.3, 3.4, 3.5_

- [ ] 8. Add comprehensive error handling
  - Implement error handling for file not found, invalid format, and parsing errors
  - Add error handling for agent execution failures and timeouts
  - Ensure proper cleanup of resources (ETS tables, agent processes) in all error scenarios
  - Write tests for all error conditions and recovery scenarios
  - _Requirements: 1.5, 2.4, 4.3_

- [x] 9. Create integration tests and documentation
  - Write end-to-end integration tests using sample forex data files
  - Test with different data sizes, formats, and edge cases
  - Create usage documentation with examples and common use cases
  - Verify backward compatibility with existing training workflows
  - _Requirements: 1.1, 1.2, 1.3, 4.4_