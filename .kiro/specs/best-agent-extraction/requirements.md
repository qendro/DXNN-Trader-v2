# Requirements Document

## Introduction

This feature enables running the best-performing trading agent on arbitrary new forex datasets after training completion. While the DXXN program can identify and start the best agent using `genotype_utils:print_best_agent(all)` and `exoself:start(Agent_Id, self())`, there's currently no streamlined way to run that agent on different forex datasets of the user's choosing. This enhancement will provide a simple interface to load any forex data file and run the best agent against it to get trading decisions.

## Requirements

### Requirement 1

**User Story:** As a trader, I want to easily run the best agent on a new forex dataset, so that I can get trading decisions for different market conditions or time periods without manually managing the agent lifecycle.

#### Acceptance Criteria

1. WHEN a user specifies a forex data file and calls the new function THEN the system SHALL automatically identify the best agent from the database
2. WHEN the best agent is identified THEN the system SHALL start the agent using the existing `exoself:start()` mechanism
3. WHEN the agent is running THEN the system SHALL load and process the specified forex data file through the agent
4. WHEN processing is complete THEN the system SHALL terminate the agent cleanly and return the trading results
5. IF no agents exist in the database THEN the system SHALL return an error message indicating no trained agents are available

### Requirement 2

**User Story:** As a trader, I want to specify different forex data files for agent evaluation, so that I can test the agent's performance across various market conditions and currency pairs.

#### Acceptance Criteria

1. WHEN a user specifies a forex data file path THEN the system SHALL validate the file exists and is readable
2. WHEN the data file is valid THEN the system SHALL load the data in the same format as the existing fx tables (EURUSD1, EURUSD15, etc.)
3. WHEN processing different currency pairs THEN the system SHALL handle the data appropriately regardless of the specific pair format
4. IF the data file is missing, malformed, or incompatible THEN the system SHALL provide specific error messages indicating the issue
5. WHEN the data is loaded THEN the system SHALL make it available to the agent using the same interface as the training data

### Requirement 3

**User Story:** As a trader, I want to get clear trading decisions and performance metrics from the agent run, so that I can evaluate the agent's effectiveness on the new data.

#### Acceptance Criteria

1. WHEN the agent processes the forex data THEN the system SHALL capture all trading decisions (buy/hold/sell) with timestamps
2. WHEN the agent makes trading decisions THEN the system SHALL record the associated confidence or fitness metrics
3. WHEN processing is complete THEN the system SHALL provide a summary report including total trades, profit/loss, and decision accuracy
4. WHEN displaying results THEN the system SHALL format the output in a readable format similar to existing benchmarker reports
5. WHEN the run completes THEN the system SHALL save the results to a file for later analysis

### Requirement 4

**User Story:** As a developer, I want the agent extraction and execution to integrate seamlessly with the existing DXXN architecture, so that the new functionality doesn't disrupt current training workflows.

#### Acceptance Criteria

1. WHEN the benchmarker completes training THEN the system SHALL automatically trigger best agent extraction without requiring manual intervention
2. WHEN extracting the best agent THEN the system SHALL not interfere with the normal cleanup and result reporting process
3. WHEN running a saved agent THEN the system SHALL use the same Docker container environment as the training process
4. WHEN the new functionality is added THEN all existing benchmarker and population_monitor functions SHALL continue to work unchanged