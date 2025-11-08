%% Configuration file for FX Trading System
%% Centralized configuration for all system parameters
%% Options listed in comments show available alternatives

-module(config).
-compile(export_all).

%% ===================================================================
%% Trading & Account Setup
%% ===================================================================
account_leverage() -> 50.                     % Options: 1-500 (typical: 10, 50, 100, 200)
account_initial_balance() -> 300.             % Options: any positive number (typical: 100-10000)
account_lot_size() -> 10000.                  % Options: 1000, 10000, 100000 (micro, mini, standard lots)
account_margin() -> 0.                        % Options: 0-1.0 (percentage, 0=no margin requirement)
account_spread() -> 0.000150.                 % Options: 0.00001-0.001 (1-100 pips, typical: 0.00015)
buy_money_fixed() -> 100.                     % Options: any positive number (fixed trade size)
min_profit_threshold() -> 0.000150.           % Options: 0.00001-0.01 (minimum profit in pips)
order_size_percentage() -> 0.2.               % Options: 0.01-1.0 (1%-100% of balance per trade)

%% ===================================================================
%% Market Data Windows
%% ===================================================================
primary_currency_pair() -> 'EURUSD1'.         % Options: 'EURUSD1', 'EURUSD1_LIVE', 'EURUSD15', 'EURUSD30', 'EURUSD60'
data_start_index() -> 1000.                   % Options: 1-N (starting row for training data)
data_end_index() -> 200.                      % Options: data_start_index+1 to N (ending row for training)
benchmark_end_index() -> last.                % Options: last, or specific number (ending row for benchmark)
market_props_start() -> 800.                  % Options: 1-N (market analysis start point)
market_props_end() -> 200.                    % Options: market_props_start+1 to N (market analysis end)

%% ===================================================================
%% Sensor Profiles
%% ===================================================================
%% --- Standard Forex Trader ---
pli_resolutions() -> [20].                    % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], etc.
pci_horizontal_resolutions() -> [90].         % Options: [10], [20], [50], [10,20], [20,50], etc.
pci_vertical_resolutions() -> [20].           % Options: [10], [15], [20], [10,15], [15,20], etc.

%% --- 1-Minute Optimized Trader ---
pli_1m_resolutions() -> [5,10,20].            % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], [3,5,10,20]
pci_1m_horizontal_resolutions() -> [20,30].   % Options: [10], [20], [30], [10,20], [20,30], [10,20,30]
pci_1m_vertical_resolutions() -> [10,15].     % Options: [5], [10], [15], [5,10], [10,15], [5,10,15]
internal_sensor_dimensions() -> 3.            % Options: 1-10 (trading state dimensions: position, profit, time)

%% ===================================================================
%% Evolution Strategy
%% ===================================================================
specie_size_limit() -> 100.                     % Options: 1-100 (max agents per species)
init_specie_size() -> 100.                      % Options: 1-50 (initial agents per species)
evaluations_limit() -> 10000.                    % Options: 10-100000 (max evaluations per run)
generation_limit() -> 100.                     % Options: 1-1000 (max generations per run)
survival_percentage() -> 0.5.                  % Options: 0.1-0.9 (percentage of agents that survive)
tot_runs() -> 1.                               % Options: 1-100 (number of benchmark runs)

%% --- Termination Logic Reference ---
%% Generation = evaluations_limit() / (agents_per_generation * tuning_duration())
%% Example: 100000 evaluations / (100 agents * 10 attempts) = 100 generations

population_evo_alg_f() -> generational.        % Options: generational, steady_state
population_selection_f() -> competition.       % Options: competition, top3
population_fitness_postprocessor_f() -> size_proportional. % Options: none, size_proportional
tot_topological_mutations_functions() -> [{ncount_exponential,0.5}]. % Options: {ncount_exponential,0.5}, {ncount_linear,1}

%% ===================================================================
%% Substrate Configuration
%% ===================================================================
substrate_plasticities() -> [abcn].            % Options: none, hebbian, ojas, iterative, abcn
substrate_linkforms() -> [l2l_feedforward].    % Options: l2l_feedforward, jordan_recurrent, fully_connected
neural_aggregation_functions() -> [dot_product]. % Options: dot_product, mult_product, diff

%% ===================================================================
%% NEAT Agent Configuration
%% ===================================================================
morphology() -> forex_trader.                  % Options: forex_trader, forex_trader_1m
connection_architecture() -> recurrent.        % Options: feedforward, recurrent
agent_encoding_types() -> [substrate].         % Options: neural, substrate
tuning_duration() -> {const,10}.               % Options: {const,N}, {linear,N}, {exponential,N}
tuning_selection_functions() -> [dynamic_random]. % Options: all, all_random, recent, recent_random, lastgen, lastgen_random
annealing_parameters() -> [0.1,0.5,1].                 % Options: [0.1], [0.5], [1], [0.5,1], [0.1,0.5,1]
perturbation_ranges() -> [0.5,1].                  % Options: [0.1], [0.5], [1], [2], [0.5,1], [1,2]
heredity_types() -> [darwinian].               % Options: darwinian, lamarckian
neural_activation_functions() -> [tanh,cos,gaussian,absolute]. % Options: tanh, cos, sin, gaussian, absolute, sigmoid, sqrt
neural_plasticity_functions() -> [neuromodulation]. % Options: none, hebbian, hebbian_w, ojas, ojas_w, self_modulationV1-V6, neuromodulation

%% --- Mutation Operators ---
%% CPP (compositional pattern producing) and CEP (compositional encoding pattern) operators are listed explicitly.
mutation_operators() -> [
    %% Weight & Bias Adaptation
    {mutate_weights,10},
    {mutate_af,5},
    {mutate_plasticity_parameters,1}, 
    {add_bias,5},
    {remove_bias,5},

    %% Topology Growth
    {add_neuron,80},
    {add_inlink,80},
    {add_outlink,80},
    {outsplice,5},

    %% Sensor & Actuator Expansion
    {add_sensor,10},
    {add_actuator,1},

    %% CPP / CEP Extensions
    {add_cpp,30},
    {add_cep,10}
]. % Options: adjust weights and probabilities as needed


%% ===================================================================
%% System & Diagnostics
%% ===================================================================
fx_tables_directory() -> "fx_tables/".         % Options: any valid directory path
source_directory() -> "fx_tables/".            % Options: any valid directory path
actuator_debug_tag() -> false.                 % Options: true, false (enables trade-by-trade debug output)
sensor_debug_tag() -> false.                   % Options: true, false (enables sensor debug output)
