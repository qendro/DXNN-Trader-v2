%% Configuration file for FX Trading System
%% Replaces hardcoded parameters throughout the codebase
%% Based on config_parameter_analysis.md List 1 - Actually Hard-Coded Parameters

-module(config).
-compile(export_all).

%% === Trading Parameters (fx.erl) ===
account_leverage() -> 50.                    % fx.erl:86
account_initial_balance() -> 300.            % fx.erl:86  
account_lot_size() -> 10000.                 % fx.erl:86
account_spread() -> 0.000150.                % fx.erl:86
account_margin() -> 0.                       % fx.erl:86
order_size_percentage() -> 0.2.              % fx.erl:530
buy_money_fixed() -> 100.                    % fx.erl:531
min_profit_threshold() -> 0.000150.          % fx.erl:155-156

%% === Data Parameters (sensor.erl, fx.erl) ===
primary_currency_pair() -> 'EURUSD1'.       % sensor.erl:55,72
data_start_index() -> 1000.                  % sensor.erl:55,72
data_end_index() -> 200.                     % sensor.erl:55,72
benchmark_end_index() -> last.               % sensor.erl:58,75
market_props_start() -> 800.                 % fx.erl:123
market_props_end() -> 200.                   % fx.erl:123

%% === Sensor Configuration (morphology.erl) ===
pli_resolutions() -> [10].                   % morphology.erl:105
pci_horizontal_resolutions() -> [50].        % morphology.erl:106
pci_vertical_resolutions() -> [20].          % morphology.erl:106
pli_1m_resolutions() -> [5,10,20].           % morphology.erl:120
pci_1m_horizontal_resolutions() -> [20,30].  % morphology.erl:121
pci_1m_vertical_resolutions() -> [10,15].    % morphology.erl:121
internal_sensor_dimensions() -> 3.           % morphology.erl:108,122

%% === Evolution Parameters (benchmarker.erl) ===
specie_size_limit() -> 100.                  % benchmarker.erl:26
init_specie_size() -> 100.                   % benchmarker.erl:27
evaluations_limit() -> 10000.                % benchmarker.erl:30
survival_percentage() -> 0.5.                % benchmarker.erl:26
tot_runs() -> 3.                             % benchmarker.erl:40

%% === Neural Network Parameters (records.hrl) ===
tuning_duration() -> {const,10}.             % records.hrl:31
annealing_parameters() -> [1].               % records.hrl:32
perturbation_ranges() -> [1].                % records.hrl:34
neural_activation_functions() -> [tanh,cos,gaussian,absolute]. % records.hrl:25
neural_plasticity_functions() -> [none].    % records.hrl:26
agent_encoding_types() -> [neural].         % records.hrl:35

%% === System Constants (various files) ===
fx_tables_directory() -> "fx_tables/".       % fx.erl:13
source_directory() -> "fx_tables/".          % fx.erl:14
actuator_debug_tag() -> true.               % fx.erl:81  //Set to false
sensor_debug_tag() -> true.                 % fx.erl:82  //Set to false
