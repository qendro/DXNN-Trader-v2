# Product Overview

This is a **Neuroevolutionary Trading System** - an Erlang-based artificial neural network platform that evolves trading strategies for forex markets using genetic algorithms.

## Core Functionality
- **Neural Network Evolution**: Automatically evolves neural network topologies and weights to optimize trading performance
- **Forex Trading Simulation**: Tests evolved strategies on historical EUR/USD forex data with realistic trading mechanics
- **Population-Based Learning**: Uses species-based genetic algorithms with mutation, selection, and fitness evaluation
- **Substrate Computing**: Supports both traditional neural networks and substrate-based (CPP/CEP) architectures

## Key Components
- **Agents**: Individual neural networks that make trading decisions
- **Population Monitor**: Manages evolution across generations of agents
- **Benchmarker**: Orchestrates experiments and performance evaluation
- **Forex Simulator**: Provides realistic trading environment with historical data
- **Polis**: Central coordination system for the neuroevolutionary platform

## Primary Use Case
Developing and testing automated forex trading strategies through evolutionary computation, with built-in benchmarking on adjacent time periods to validate generalization performance.