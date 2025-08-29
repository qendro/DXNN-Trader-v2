# Implementation Plan - Python-Centric Architecture

## Overview

This implementation plan transforms the live trading system into a revolutionary Python-centric architecture where `ib_service.py` handles ALL Interactive Brokers operations (connection, data streaming, historical loading, tick aggregation, trade execution) while Erlang focuses purely on neural network coordination and ETS data storage.

**Key Architectural Changes**:
- **Python handles**: IB connection, live data streaming, historical data loading, tick-to-OHLC aggregation, trade execution
- **Erlang handles**: Neural network coordination, ETS storage of OHLC bars, system orchestration
- **Communication**: Only processed OHLC bars flow from Python to Erlang
- **Simplification**: 60% reduction in Erlang code complexity

**Simplification Principles**:
- **One Erlang module for data path**: Keep everything data-plane in live_scape.erl (port/socket, decode/encode, acks, ETS insert, sense/2, trade signal out)
- **One Python entrypoint**: ib_service.py hosts all classes; no extra micro-modules
- **One OHLC schema**: Canonical {symbol, t_open, o,h,l,c,vol,source}. Key ETS by {Symbol, TOpen}. Idempotent upsert
- **One readiness gate**: live_trading checks "≥M bars per symbol AND last_bar_age < Xs"
- **One kill switch**: Env flag + runtime command to flip to PAPER and block new orders in Python

**Note on Temporary Code**: Throughout this implementation, any temporary test modules, mock utilities, or one-time code should be created in separate modules with the label "delete" in their names or comments. These modules can be safely removed after the consolidation is complete.

## Implementation Tasks

### Phase 0: Python-Centric Architecture Planning

- [x] 0. Create comprehensive Python-centric architecture documentation
  - Create `PYTHON_CENTRIC_ARCHITECTURE.md` documenting the revolutionary Python-first approach
  - Define complete `ib_service.py` architecture with all IB operations
  - Document simplified Erlang modules (live_trading.erl, live_scape.erl only)
  - Map data flow: Python (all IB ops) → Erlang (OHLC bars only) → Neural Networks
  - Create dependency analysis showing 60% Erlang code reduction
  - _Requirements: 10.4, 10.6_

- [x] 0.1 Define comprehensive ib_service.py architecture (single Python entrypoint)
  - Document IB connection management in Python
  - Define historical data loading system (x weeks, x bar size)
  - Document live tick streaming and 1-minute OHLC aggregation
  - Define trade execution system with PAPER mode kill switch
  - Document canonical OHLC schema: {symbol, t_open, o,h,l,c,vol,source}
  - Map all Python classes in single ib_service.py file (no micro-modules)
  - _Requirements: 1.1, 2.1, 8.1_

- [x] 0.2 Define simplified live_trading.erl architecture
  - Document system coordination functions only (no IB operations)
  - Define Python service management (start/stop/monitor)
  - Document neural network deployment and monitoring
  - Define simplified state management (no IB connection state)
  - Map user interface functions (start, stop, status, performance)
  - Remove all IB-related functions (moved to Python)
  - _Requirements: 1.1, 7.1, 7.2_

- [x] 0.3 Define simplified live_scape.erl architecture (single data path module)
  - Document complete data path in live_scape.erl: port/socket, decode/encode, acks, ETS insert, sense/2, trade signal out
  - Define canonical OHLC ETS schema: Key by {Symbol, TOpen}, idempotent upsert
  - Document neural network sensor interface (unchanged for compatibility)
  - Define readiness gate: "≥M bars per symbol AND last_bar_age < Xs"
  - Remove all IB connection and tick processing functions
  - No separate bridge modules - everything in live_scape.erl
  - _Requirements: 7.1, 8.1, 8.3_

- [x] 0.4 Comprehensive workspace dependency analysis for Python-centric approach
  - Analyze impact of removing ib_bridge_connector.erl entirely
  - Verify config.erl integration points remain unchanged
  - Confirm fx.erl delegation to live_scape.erl still works
  - Document modules to delete: live_trading_main.erl, live_trading_integration.erl, live_trader.erl, ib_bridge_connector.erl
  - Map all external references that need updating
  - Create cleanup plan for 60% code reduction
  - Document Python dependencies (ib_insync, asyncio, datetime, json)
  - _Requirements: 1.3, 8.2, 10.4_

- [ ] 0.5 Review Python-centric architecture and finalize implementation plan
  - Review all Python-centric architecture documentation
  - Validate that Python handles ALL IB operations comprehensively
  - Confirm Erlang simplification achieves 60% code reduction
  - Verify neural network compatibility is maintained 100%
  - Update implementation phases to reflect Python-first approach
  - Ensure all external integrations (config.erl, fx.erl) remain functional
  - Validate cleanup plan covers all obsolete modules
  - Create final implementation roadmap with Python and Erlang phases
  - _Requirements: 1.1, 1.3, 10.4_

### Phase 1: Enhanced Python Service Development

- [ ] 1. Create comprehensive ib_service.py (single Python entrypoint)
  - Create single `ib_service.py` file with all IB management classes
  - Implement IBConnectionManager class for IB TWS connection
  - Implement HistoricalDataLoader class for loading x weeks of x bar size data
  - Implement LiveTickAggregator class for real-time tick-to-OHLC conversion
  - Implement TradeExecutor class with PAPER mode kill switch (env flag + runtime command)
  - Implement canonical OHLC schema: {symbol, t_open, o,h,l,c,vol,source}
  - Create `python_service_tests.py` module (LABEL: delete) for Python testing
  - _Requirements: 1.1, 2.1, 8.1_

- [ ] 1.1 Implement IB connection management in Python
  - Create IBConnectionManager with async IB TWS connection
  - Implement market data subscription setup for configured symbols
  - Add connection monitoring and auto-reconnection logic
  - Implement heartbeat monitoring and connection health checks
  - Add error handling and connection status reporting
  - Create connection configuration loading from environment/config
  - _Requirements: 2.1, 2.2, 4.1_

- [ ] 1.2 Implement historical data loading system
  - Create HistoricalDataLoader with configurable weeks and bar size
  - Implement IB historical data API integration using ib_insync
  - Add data validation and quality checks for historical bars
  - Implement streaming delivery using canonical OHLC schema: {symbol, t_open, o,h,l,c,vol,source='historical'}
  - Add progress reporting and error handling for large data loads
  - Create data format conversion from IB bars to canonical OHLC format
  - _Requirements: 8.1, 8.2, 8.6_

- [ ] 1.3 Implement live tick aggregation system
  - Create LiveTickAggregator for real-time tick processing
  - Implement 1-minute OHLC bar creation using canonical schema: {symbol, t_open, o,h,l,c,vol,source='live'}
  - Add time-based bar completion and delivery to Erlang
  - Implement tick validation and data quality monitoring
  - Add buffer management for incomplete bars
  - Create tick-to-price conversion logic (bid/ask/last selection)
  - _Requirements: 8.3, 8.4, 10.1_

### Phase 2: Python Trade Execution and Communication

- [ ] 2. Implement trade execution system with kill switch
  - Create TradeExecutor class for direct IB trade placement
  - Implement PAPER mode kill switch: env flag + runtime command to block new orders
  - Implement market order, limit order, and stop order execution
  - Add order tracking and status monitoring
  - Implement position management and tracking in Python
  - Add trade confirmation and error handling
  - Create order validation and risk checks before execution
  - _Requirements: 7.3, 7.4, 9.1, 9.5_

- [ ] 2.1 Implement Erlang communication bridge
  - Create ErlangBridge class for Python-Erlang communication
  - Implement OHLC bar streaming to Erlang (send_ohlc_bar method)
  - Add trade signal reception from Erlang neural networks
  - Implement trade confirmation reporting back to Erlang
  - Add error reporting and status updates to Erlang
  - Create message framing and JSON serialization
  - _Requirements: 1.4, 7.3, 10.2_

- [ ] 2.2 Implement Python service configuration and startup
  - Create configuration loading from config.erl integration
  - Implement Python service startup and shutdown procedures
  - Add service health monitoring and status reporting
  - Create logging and error tracking in Python service
  - Implement graceful shutdown and cleanup procedures
  - Add service restart and recovery mechanisms
  - _Requirements: 2.1, 2.6, 6.1_

### Phase 3: Simplified Erlang Data Interface (Single Data Path Module)

- [ ] 3. Create simplified live_scape.erl (complete data path in one module)
  - Create new `live_scape.erl` module with complete data path: port/socket, decode/encode, acks, ETS insert, sense/2, trade signal out
  - Implement canonical OHLC ETS schema: Key by {Symbol, TOpen}, idempotent upsert
  - Add direct ETS insertion using canonical schema (no processing needed)
  - Maintain existing neural network sensor interface for 100% compatibility
  - Remove all IB connection and tick processing functions
  - No separate bridge modules - everything in live_scape.erl
  - _Requirements: 7.1, 8.1, 8.3_

- [ ] 3.1 Implement canonical OHLC bar reception and storage
  - Create message handling for OHLC bars using canonical schema: {symbol, t_open, o,h,l,c,vol,source}
  - Implement ETS keying by {Symbol, TOpen} with idempotent upsert
  - Add data validation for incoming OHLC bars
  - Create table management for multiple currency pairs
  - Implement data archiving and cleanup procedures
  - Add data quality monitoring and error reporting
  - _Requirements: 8.1, 8.2, 8.6_

- [ ] 3.2 Maintain neural network sensor compatibility
  - Preserve existing `sense/2` function interface exactly
  - Ensure `handle_sense_request/4` works with ETS data
  - Maintain price list and graph sensor data formats
  - Verify neural network data access patterns work unchanged
  - Test with existing neural network agents
  - Document 100% backward compatibility guarantee
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [ ] 3.3 Implement trade signal transmission to Python
  - Create `handle_trade_signal/2` function for neural network decisions
  - Implement message formatting for Python service communication
  - Add trade signal validation and error handling
  - Create trade confirmation reception from Python
  - Implement position tracking and synchronization
  - Add trade history logging and performance tracking
  - _Requirements: 7.3, 7.4, 9.1_

### Phase 4: Python-Erlang Integration and Communication

- [ ] 4. Implement Python-Erlang communication protocol
  - Create message framing and JSON serialization between Python and Erlang
  - Implement bidirectional communication channel (port-based or socket-based)
  - Add message acknowledgment and error handling
  - Create message queuing and buffering for reliability
  - Implement heartbeat and connection monitoring
  - Add communication performance monitoring and optimization
  - _Requirements: 1.4, 4.1, 10.2_

- [ ] 4.1 Implement OHLC bar streaming from Python to Erlang
  - Create `send_ohlc_bar/1` function in Python ErlangBridge
  - Implement message formatting for historical and live OHLC bars (same format)
  - Stream historical bars one-by-one via send_ohlc_bar, with flow control and idempotent upserts
  - Create flow control to prevent Erlang message queue overflow
  - Implement progress reporting for historical data streaming
  - Add streaming error recovery and resume capability
  - _Requirements: 8.1, 8.2, 10.1_

- [ ] 4.2 Implement trade signal communication from Erlang to Python
  - Create trade signal message format (symbol, signal, timestamp)
  - Implement signal validation and error handling in Python
  - Add trade execution confirmation back to Erlang
  - Create order status updates and position synchronization
  - Implement trade rejection handling and error reporting
  - Add performance tracking for trade execution latency
  - _Requirements: 7.3, 7.4, 9.1_

- [ ] 4.3 Create system status and monitoring integration
  - Implement Python service status reporting to Erlang
  - Add IB connection status monitoring and alerts
  - Create data quality monitoring and reporting
  - Implement performance metrics sharing between Python and Erlang
  - Add error reporting and diagnostic information sharing
  - Create system health dashboard and monitoring
  - _Requirements: 10.1, 10.3, 10.5, 10.6_

### Phase 5: Simplified Erlang System Coordinator

- [ ] 5. Create simplified live_trading.erl system coordinator
  - Create new `live_trading.erl` module with Python service management
  - Implement `start/0` function that starts Python ib_service.py
  - Add `stop/0` and `emergency_stop/0` functions with Python service control
  - Implement system status monitoring (Python service + neural networks)
  - Create simplified state management (no IB connection state)
  - Remove all IB-related functions (delegated to Python)
  - _Requirements: 1.1, 2.1, 2.6_

- [ ] 5.1 Implement Python service lifecycle management
  - Create `start_python_service/0` function to launch ib_service.py
  - Implement Python process monitoring and health checking
  - Add Python service restart logic for failures
  - Create communication channel setup with Python service
  - Implement graceful Python service shutdown
  - Add Python service status reporting and diagnostics
  - _Requirements: 2.1, 2.2, 4.2_

- [ ] 5.2 Implement neural network coordination
  - Create `deploy_agent/1` function for neural network deployment
  - Implement neural network process monitoring
  - Add agent performance tracking and reporting
  - Create neural network restart logic for failures
  - Implement agent switching and hot-swapping
  - Add neural network status and diagnostics
  - _Requirements: 7.1, 7.2, 4.3_

- [ ] 5.3 Create user interface and command system
  - Implement user API functions (start, stop, status, performance)
  - Add quick commands (go, halt, st, ms, perf)
  - Create comprehensive help system
  - Implement response formatting and user-friendly output
  - Add command validation and error handling
  - Create system diagnostics and troubleshooting commands
  - _Requirements: 1.1, 10.4, 10.6_

### Phase 6: Testing and Validation

- [ ] 6. Create comprehensive Python service testing
  - Create `test_ib_service.py` module (LABEL: delete) for Python testing
  - Test IB connection management and error handling
  - Validate historical data loading with various timeframes
  - Test live tick aggregation and OHLC bar creation
  - Validate trade execution and order management
  - Test Python-Erlang communication protocol
  - _Requirements: 5.1, 5.2, 5.4_

- [ ] 6.1 Create Erlang module testing
  - Create `test_live_trading.erl` module (LABEL: delete) for Erlang testing
  - Test simplified live_trading.erl system coordination
  - Validate live_scape.erl data serving and neural network interface
  - Test OHLC bar reception and ETS storage
  - Validate trade signal transmission to Python
  - Test system startup and shutdown procedures
  - _Requirements: 5.3, 5.4, 7.1_

- [ ] 6.2 Create integration testing framework
  - Create `integration_test_suite.py` module (LABEL: delete) for end-to-end testing
  - Test complete Python-Erlang integration
  - Validate neural network compatibility with new architecture
  - Test historical data loading and live trading workflow
  - Validate error handling and recovery scenarios
  - Test performance and latency requirements
  - _Requirements: 5.4, 7.1, 10.1_

### Phase 7: Neural Network Compatibility and Performance

- [ ] 7. Validate neural network compatibility with new architecture
  - Test existing neural network agents with new live_scape.erl interface
  - Verify sensor data formats remain unchanged (price lists, OHLC)
  - Validate actuator trade signal processing works correctly
  - Test neural network deployment and monitoring
  - Ensure 100% backward compatibility with existing agents
  - Document any compatibility issues and solutions
  - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [ ] 7.1 Implement performance optimization and monitoring
  - Optimize Python OHLC bar aggregation performance
  - Tune Erlang ETS table operations for efficiency
  - Implement performance monitoring and metrics collection
  - Optimize Python-Erlang communication throughput
  - Test system performance under various load conditions
  - Create performance benchmarks and comparison with old system
  - _Requirements: 5.5, 10.1, 10.3_

- [ ] 7.2 Create comprehensive error handling and recovery testing
  - Test Python service failures and automatic restart
  - Simulate IB connection failures and recovery procedures
  - Test Erlang process failures and supervision
  - Validate data loss prevention during failures
  - Test emergency stop and risk management scenarios
  - Create failure recovery documentation and procedures
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 4.6_

### Phase 8: Migration and Cleanup

- [ ] 8. Create migration utilities and procedures for Python-centric architecture
  - Create `migration_utils.py` module (LABEL: delete) for Python migration tools
  - Create `backup_restore.erl` module (LABEL: delete) for Erlang backup procedures
  - Implement data migration from old ETS tables to new format
  - Create configuration migration for Python service integration
  - Add rollback procedures in case of migration issues
  - Document step-by-step migration procedures
  - _Requirements: 2.6, 3.5_

- [ ] 8.1 Implement parallel testing framework for architecture comparison
  - Create `architecture_comparison.py` module (LABEL: delete) for testing both systems
  - Create `performance_benchmark.erl` module (LABEL: delete) for Erlang performance testing
  - Implement side-by-side testing with old 5-module and new Python-centric systems
  - Add performance comparison tools measuring latency and throughput
  - Create validation reports comparing neural network performance
  - Test with multiple existing neural network agents
  - _Requirements: 5.4, 7.1, 7.5_

- [ ] 8.2 Execute comprehensive workspace cleanup for Python-centric architecture
  - Remove `live_trading_main.erl` (400 lines) after migration
  - Remove `live_trading_integration.erl` (1070+ lines) after consolidation
  - Remove `live_trader.erl` (1724+ lines) after neural network migration
  - Remove `ib_bridge_connector.erl` (600 lines) - replaced by Python service
  - Remove all temporary modules with "delete" labels created during implementation
  - Execute cleanup plan achieving 60% Erlang code reduction
  - Update external references in config.erl and fx.erl if needed
  - Clean up obsolete ETS tables and registered processes
  - Update documentation and comments referencing deleted modules
  - _Requirements: 1.1, 1.3_

- [ ] 8.3 Create comprehensive documentation for Python-centric architecture
  - Document new Python-centric architecture with ib_service.py as IB handler
  - Create API documentation for simplified live_trading.erl and live_scape.erl
  - Document Python service configuration and deployment procedures
  - Create troubleshooting guides for Python-Erlang integration
  - Document migration procedures and rollback steps
  - Create performance optimization guides for Python and Erlang components
  - _Requirements: 10.4, 10.6_

### Phase 9: Final Validation and Deployment

- [ ] 9. Perform final system validation for Python-centric architecture
  - Run complete test suite covering Python service and Erlang modules
  - Validate 100% neural network compatibility with existing agents
  - Test system performance and verify 60% Erlang code reduction benefits
  - Verify all configuration options work with Python service integration
  - Validate error handling and recovery procedures across Python-Erlang boundary
  - Test all external integration points (config.erl, fx.erl) work correctly
  - Validate that no broken references exist after 4-module cleanup
  - Confirm Python service handles all IB operations correctly
  - _Requirements: 5.4, 7.1, 7.5, 9.6_

- [ ] 9.1 Create deployment and monitoring procedures for hybrid system
  - Create deployment scripts for Python service and Erlang modules
  - Implement monitoring for both Python service and Erlang processes
  - Add health check endpoints for Python-Erlang communication
  - Create operational runbooks for hybrid system management
  - Set up logging and metrics collection across Python and Erlang
  - Create alerting for Python service failures and Erlang process issues
  - _Requirements: 10.1, 10.3, 10.5, 10.6_

- [ ] 9.2 Conduct final performance optimization and validation
  - Profile Python service performance for IB operations and data processing
  - Optimize Erlang ETS operations and neural network coordination
  - Tune Python-Erlang communication for optimal throughput
  - Validate performance improvements from architecture simplification
  - Confirm system meets all latency and reliability requirements
  - Document performance benchmarks and optimization recommendations
  - _Requirements: 5.5, 8.1, 8.2_

## Success Criteria

### Functional Requirements - Python-Centric Architecture
- [ ] Python service handles ALL IB operations (connection, data, trades) correctly
- [ ] Historical data loading (x weeks, x bar size) works reliably
- [ ] Live tick aggregation to 1-minute OHLC bars functions properly
- [ ] Neural network agents deploy and operate with 100% compatibility
- [ ] OHLC bar streaming from Python to Erlang ETS works seamlessly
- [ ] Trade signal transmission from Erlang to Python executes correctly
- [ ] Error handling and recovery work across Python-Erlang boundary

### Performance Requirements - Hybrid System
- [ ] System startup completes within 30 seconds (Python + Erlang)
- [ ] Historical data loading completes within 60 seconds for 4 weeks
- [ ] OHLC bar processing latency < 5ms per bar in Python
- [ ] Trade signal processing latency < 50ms from neural network to Python
- [ ] Order placement latency < 100ms via Python to IB
- [ ] Erlang memory usage reduced by 60% due to simplified architecture
- [ ] Python-Erlang communication throughput > 1000 messages/second

### Quality Requirements - Architecture Transformation
- [ ] 60% reduction in Erlang code complexity achieved
- [ ] 100% neural network backward compatibility maintained
- [ ] Python service code coverage > 80% for all IB operations
- [ ] Erlang module code coverage > 80% for simplified functions
- [ ] All integration tests pass consistently across Python-Erlang boundary
- [ ] No memory leaks during extended operation in either Python or Erlang
- [ ] System recovers gracefully from Python service failures
- [ ] System recovers gracefully from Erlang process failures
- [ ] Documentation is complete for both Python and Erlang components

### Architecture Success Metrics
- [ ] **Code Reduction**: 4 modules eliminated (live_trading_main.erl, live_trading_integration.erl, live_trader.erl, ib_bridge_connector.erl)
- [ ] **Simplification**: Only 2 Erlang modules remain (live_trading.erl, live_scape.erl)
- [ ] **Separation of Concerns**: Python handles 100% of IB operations, Erlang handles 100% of neural network operations
- [ ] **Performance**: Improved system performance due to optimized data processing in Python
- [ ] **Maintainability**: Clear boundaries between Python IB service and Erlang neural network system

This implementation plan provides a systematic approach to transforming your live trading system into a revolutionary Python-centric architecture while maintaining full compatibility with your existing neural network infrastructure and achieving dramatic simplification.