%% Test configuration with minimal parameters for quick testing
-module(config_test).
-compile(export_all).

%% === Trading Parameters (fx.erl) ===
account_leverage() -> 50.
account_initial_balance() -> 300.
account_lot_size() -> 10000.
account_spread() -> 0.000150.
account_margin() -> 0.
order_size_percentage() -> 0.2.
buy_money_fixed() -> 100.
min_profit_threshold() -> 0.000150.

%% === Data Parameters (sensor.erl, fx.erl) ===
primary_currency_pair() -> 'EURUSD15'.
data_start_index() -> 100.                   % Reduced from 1000
data_end_index() -> 50.                      % Reduced from 200
benchmark_end_index() -> last.
market_props_start() -> 80.                  % Reduced from 800
market_props_end() -> 50.                    % Reduced from 200

%% === Sensor Configuration (morphology.erl) - MUCH SMALLER ===
pli_resolutions() -> [5].                    % Reduced from [10]
pci_horizontal_resolutions() -> [10].        % Reduced from [50]
pci_vertical_resolutions() -> [5].           % Reduced from [20]
pli_1m_resolutions() -> [3].                 % Reduced from [5,10,20]
pci_1m_horizontal_resolutions() -> [5].      % Reduced from [20,30]
pci_1m_vertical_resolutions() -> [3].        % Reduced from [10,15]
internal_sensor_dimensions() -> 3.

%% === Evolution Parameters (benchmarker.erl) - MUCH SMALLER ===
specie_size_limit() -> 5.                    % Reduced from 100
init_specie_size() -> 5.                     % Reduced from 100
evaluations_limit() -> 100.                  % Reduced from 10000
survival_percentage() -> 0.5.

%% === Neural Network Parameters (records.hrl) ===
tuning_duration() -> {const,5}.              % Reduced from 10
annealing_parameters() -> [1].
perturbation_ranges() -> [1].
neural_activation_functions() -> [tanh].     % Reduced to just tanh
neural_plasticity_functions() -> [none].
agent_encoding_types() -> [neural].

%% === System Constants (various files) ===
fx_tables_directory() -> "fx_tables/".
source_directory() -> "fx_tables/".
actuator_debug_tag() -> false.
sensor_debug_tag() -> false.

%% === Backward Compatibility ===
fx_table() -> 'EURUSD15'.
morphology() -> forex_trader.
tot_runs() -> 1.                             % Reduced from 3