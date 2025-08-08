# Requirements Document

## Introduction

This feature extends the existing neuroevolutionary trading system to support live paper trading through Interactive Brokers API. The system will take evolved neural network models that have been trained and validated on historical forex data and deploy them to make real-time trading decisions in a paper trading environment. This bridges the gap between backtesting performance and live market execution, allowing for validation of evolved strategies under real market conditions with live data feeds.

**Design Constraints:**
- All new functionality must be implemented in separate modules without modifying existing codebase
- Minimize code footprint - implement only essential functionality required for live trading
- Avoid redundant features or "nice-to-have" additions that don't directly support core live trading capability
- Maintain clear separation between existing backtesting system and new live trading components

## Requirements

### Requirement 1

**User Story:** As a trader, I want to deploy my best evolved neural network model to a live paper trading account, so that I can validate its performance with real market conditions without risking actual capital.

#### Acceptance Criteria

1. WHEN a user selects a saved neural network model THEN the system SHALL load the model's genotype and neural network configuration
2. WHEN the model is loaded THEN the system SHALL establish connection to Interactive Brokers paper trading account
3. WHEN connected to IB THEN the system SHALL begin receiving live market data for the configured forex pairs
4. WHEN live data is received THEN the neural network SHALL process the data and generate trading signals
5. WHEN a trading signal is generated THEN the system SHALL execute the corresponding trade through the IB API
6. IF the IB connection fails THEN the system SHALL log the error and attempt reconnection with exponential backoff

### Requirement 2

**User Story:** As a system administrator, I want to configure and manage the Interactive Brokers API connection, so that the system can securely connect to paper trading accounts with proper authentication.

#### Acceptance Criteria

1. WHEN configuring IB connection THEN the system SHALL support TWS API connection parameters (host, port, client ID)
2. WHEN authenticating THEN the system SHALL handle IB authentication requirements securely
3. WHEN connection is established THEN the system SHALL verify paper trading mode is active
4. IF production trading mode is detected THEN the system SHALL refuse connection and log security warning
5. WHEN connection parameters change THEN the system SHALL support hot reconfiguration without system restart

### Requirement 3

**User Story:** As a researcher, I want to monitor live trading performance and compare it with backtesting results, so that I can evaluate model generalization and identify potential overfitting.

#### Acceptance Criteria

1. WHEN live trades are executed THEN the system SHALL record all trading decisions with timestamps and market context
2. WHEN recording trades THEN the system SHALL track P&L, win rate, and other performance metrics in real-time
3. WHEN performance data is available THEN the system SHALL provide comparison with historical backtesting results
4. WHEN significant performance deviation occurs THEN the system SHALL alert the user
5. WHEN requested THEN the system SHALL generate performance reports comparing live vs backtested results

### Requirement 4

**User Story:** As a developer, I want a minimal architecture that can deploy a single best-performing model, so that I can validate live trading functionality with the simplest possible implementation.

#### Acceptance Criteria

1. WHEN deploying a model THEN the system SHALL support running one model at a time with a single IB connection
2. WHEN a model is active THEN the system SHALL manage position sizing based on simple fixed allocation rules
3. WHEN switching models THEN the system SHALL require manual intervention to stop current model and start new one
4. WHEN a model is running THEN the system SHALL provide basic status monitoring showing connection and trading state
5. WHEN model selection is needed THEN the system SHALL load models from existing Mnesia database without duplication

### Requirement 5

**User Story:** As a risk manager, I want comprehensive risk controls and position management, so that paper trading operations remain within safe parameters and don't exceed account limits.

#### Acceptance Criteria

1. WHEN placing orders THEN the system SHALL enforce maximum position size limits per currency pair
2. WHEN calculating position sizes THEN the system SHALL consider current account balance and available margin
3. WHEN total exposure exceeds limits THEN the system SHALL reject new orders and log risk violations
4. WHEN market conditions are volatile THEN the system SHALL implement emergency stop mechanisms
5. WHEN daily loss limits are reached THEN the system SHALL halt trading and notify administrators

### Requirement 6

**User Story:** As a system operator, I want robust error handling and recovery mechanisms, so that temporary market disruptions or API issues don't compromise the trading system's stability.

#### Acceptance Criteria

1. WHEN IB API errors occur THEN the system SHALL categorize errors and implement appropriate recovery strategies
2. WHEN market data feed is interrupted THEN the system SHALL detect the interruption and attempt reconnection
3. WHEN order execution fails THEN the system SHALL retry with exponential backoff and log failure details
4. WHEN system components crash THEN the system SHALL restart affected processes automatically using OTP supervision
5. WHEN critical errors occur THEN the system SHALL maintain system state and provide graceful degradation