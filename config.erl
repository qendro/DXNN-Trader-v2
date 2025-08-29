%% Configuration file for FX Trading System
%% Centralized configuration for all system parameters
%% Options listed in comments show available alternatives

-module(config).
-compile(export_all).

%% === Trading Parameters ===
account_leverage() -> 50.                    % Options: 1-500 (typical: 10, 50, 100, 200)
account_initial_balance() -> 300.            % Options: any positive number (typical: 100-10000)
account_lot_size() -> 10000.                 % Options: 1000, 10000, 100000 (micro, mini, standard lots)
account_spread() -> 0.000150.                % Options: 0.00001-0.001 (1-100 pips, typical: 0.00015)
account_margin() -> 0.                       % Options: 0-1.0 (percentage, 0=no margin requirement)
order_size_percentage() -> 0.2.              % Options: 0.01-1.0 (1%-100% of balance per trade)
buy_money_fixed() -> 100.                    % Options: any positive number (fixed trade size)
min_profit_threshold() -> 0.000150.          % Options: 0.00001-0.01 (minimum profit in pips)

%% === Data Parameters ===
primary_currency_pair() -> 'EURUSD1'.         % Options: 'EURUSD1', 'EURUSD15', 'EURUSD30', 'EURUSD60'
data_start_index() -> 1000.                  % Options: 1-N (starting row for training data)
data_end_index() -> 200.                     % Options: data_start_index+1 to N (ending row for training)
benchmark_end_index() -> last.               % Options: last, or specific number (ending row for benchmark)
market_props_start() -> 800.                 % Options: 1-N (market analysis start point)
market_props_end() -> 200.                   % Options: market_props_start+1 to N (market analysis end)

%% === Sensor Configuration - Standard Forex Trader ===
pli_resolutions() -> [5].                   % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], etc.
pci_horizontal_resolutions() -> [50].        % Options: [10], [20], [50], [10,20], [20,50], etc.
pci_vertical_resolutions() -> [20].          % Options: [10], [15], [20], [10,15], [15,20], etc.

%% === Sensor Configuration - 1-Minute Optimized ===
pli_1m_resolutions() -> [5,10,20].           % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], [3,5,10,20]
pci_1m_horizontal_resolutions() -> [20,30].  % Options: [10], [20], [30], [10,20], [20,30], [10,20,30]
pci_1m_vertical_resolutions() -> [10,15].    % Options: [5], [10], [15], [5,10], [10,15], [5,10,15]
internal_sensor_dimensions() -> 3.           % Options: 1-10 (trading state dimensions: position, profit, time)

%% === Evolution Parameters ===
specie_size_limit() -> 1.                    % Options: 1-100 (max agents per species)
init_specie_size() -> 1.                     % Options: 1-50 (initial agents per species)
evaluations_limit() -> 10.                    % Options: 10-100000 (max evaluations per run)
survival_percentage() -> 0.5.                % Options: 0.1-0.9 (percentage of agents that survive)
tot_runs() -> 1.                             % Options: 1-100 (number of benchmark runs)

%% === Neural Network Parameters ===
morphology() -> forex_trader.             % Options: forex_trader, forex_trader_1m
tuning_duration() -> {const,10}.             % Options: {const,N}, {linear,N}, {exponential,N}
annealing_parameters() -> [1].               % Options: [0.1], [0.5], [1], [0.5,1], [0.1,0.5,1]
perturbation_ranges() -> [1].                % Options: [0.1], [0.5], [1], [2], [0.5,1], [1,2]
neural_activation_functions() -> [tanh,cos,gaussian,absolute]. % Options: tanh, cos, sin, gaussian, absolute, sigmoid, sqrt
neural_plasticity_functions() -> [none].     % Options: none, hebbian, ojas, self_modulation
agent_encoding_types() -> [substrate].          % Options: neural, substrate

%% === Benchmarker Constraint Parameters ===
population_evo_alg_f() -> generational.      % Options: generational, steady_state
connection_architecture() -> recurrent.    % Options: feedforward, recurrent

%% === System Configuration ===
fx_tables_directory() -> "fx_tables/".  % Options: any valid directory path
source_directory() -> "fx_tables/".     % Options: any valid directory path  
actuator_debug_tag() -> false.               % Options: true, false (enables trade-by-trade debug output)
sensor_debug_tag() -> false.                 % Options: true, false (enables sensor debug output)
%% === Live Trading Parameters ===
ib_host() -> 
    os:getenv("IB_HOST", "host.docker.internal").

ib_port() -> 
    list_to_integer(os:getenv("IB_PORT", "7497")).

ib_client_id() -> 
    list_to_integer(os:getenv("IB_CLIENT_ID", "101")).

%% Add logging at startup
log_ib_config() ->
    Host = ib_host(),
    Port = ib_port(),
    ClientId = ib_client_id(),
    io:format("IB Config: host=~s port=~p client_id=~p~n", [Host, Port, ClientId]).
live_position_size() -> 0.1.                 % Options: 0.01-1.0 (10% of account per trade)
live_max_daily_loss() -> 0.05.               % Options: 0.01-0.5 (5% max daily loss)
live_currency_pairs() -> ['EUR.USD'].        % Options: IB format currency pairs

%% === Risk Management Parameters ===
live_max_position_per_pair() -> 0.2.         % Options: 0.01-1.0 (20% max exposure per currency pair)
live_max_total_exposure() -> 0.5.            % Options: 0.1-1.0 (50% max total portfolio exposure)
live_min_account_balance() -> 100.           % Options: any positive number (minimum balance to continue trading)
live_margin_requirement() -> 0.02.           % Options: 0.01-0.1 (2% margin requirement for forex)
live_max_drawdown_limit() -> 0.15.           % Options: 0.05-0.5 (15% max drawdown before halt)
live_daily_trade_limit() -> 50.              % Options: 1-1000 (max trades per day)
live_position_timeout() -> 3600.             % Options: 300-86400 (1 hour max position hold time in seconds)

%% === Currency Pair Mapping ===
%% Maps internal system format to Interactive Brokers symbol format
internal_to_ib_symbol('EURUSD') -> 'EUR.USD';
internal_to_ib_symbol('GBPUSD') -> 'GBP.USD';
internal_to_ib_symbol('USDJPY') -> 'USD.JPY';
internal_to_ib_symbol('USDCHF') -> 'USD.CHF';
internal_to_ib_symbol('AUDUSD') -> 'AUD.USD';
internal_to_ib_symbol('USDCAD') -> 'USD.CAD';
internal_to_ib_symbol('NZDUSD') -> 'NZD.USD';
internal_to_ib_symbol(Symbol) -> Symbol.  % Default passthrough

%% Maps Interactive Brokers symbol format to internal system format
ib_to_internal_symbol('EUR.USD') -> 'EURUSD';
ib_to_internal_symbol('GBP.USD') -> 'GBPUSD';
ib_to_internal_symbol('USD.JPY') -> 'USDJPY';
ib_to_internal_symbol('USD.CHF') -> 'USDCHF';
ib_to_internal_symbol('AUD.USD') -> 'AUDUSD';
ib_to_internal_symbol('USD.CAD') -> 'USDCAD';
ib_to_internal_symbol('NZD.USD') -> 'NZDUSD';
ib_to_internal_symbol(Symbol) -> Symbol.  % Default passthrough

%% === Configuration Validation Functions ===

%% Validates IB connection parameters
validate_ib_connection_config() ->
    Host = ib_host(),
    Port = ib_port(),
    ClientId = ib_client_id(),
    
    % Validate host format (allow both IP addresses and hostnames)
    case inet:parse_address(Host) of
        {ok, _} -> ok;  % Valid IP address
        {error, _} ->
            % Check if it's a valid hostname (like host.docker.internal)
            case Host of
                "host.docker.internal" -> ok;  % Docker hostname
                "localhost" -> ok;             % Localhost
                "127.0.0.1" -> ok;             % Localhost IP
                _ ->
                    % Try to resolve the hostname
                    case inet:gethostbyname(Host) of
                        {ok, _} -> ok;  % Valid hostname
                        {error, _} -> throw({invalid_ib_host, Host})
                    end
            end
    end,
    
    % Validate port range
    if 
        Port >= 1024 andalso Port =< 65535 -> ok;
        true -> throw({invalid_ib_port, Port})
    end,
    
    % Validate client ID range (IB allows 1-32 for paper trading, but we'll be more flexible)
    if 
        ClientId >= 1 andalso ClientId =< 999 -> ok;
        true -> throw({invalid_ib_client_id, ClientId})
    end,
    
    % Ensure paper trading port (security check)
    if 
        Port =:= 7497 -> ok;  % Paper trading port
        Port =:= 7496 -> throw({production_port_detected, "Live trading port 7496 is not allowed"});
        true -> ok  % Allow other ports for testing
    end,
    
    ok.

%% Validates live trading risk parameters
validate_risk_parameters() ->
    PositionSize = live_position_size(),
    MaxDailyLoss = live_max_daily_loss(),
    MaxPositionPerPair = live_max_position_per_pair(),
    MaxTotalExposure = live_max_total_exposure(),
    MinAccountBalance = live_min_account_balance(),
    MarginRequirement = live_margin_requirement(),
    MaxDrawdownLimit = live_max_drawdown_limit(),
    DailyTradeLimit = live_daily_trade_limit(),
    PositionTimeout = live_position_timeout(),
    
    % Validate position size (0.01 to 1.0)
    if 
        PositionSize >= 0.01 andalso PositionSize =< 1.0 -> ok;
        true -> throw({invalid_position_size, PositionSize})
    end,
    
    % Validate max daily loss (0.01 to 0.5)
    if 
        MaxDailyLoss >= 0.01 andalso MaxDailyLoss =< 0.5 -> ok;
        true -> throw({invalid_max_daily_loss, MaxDailyLoss})
    end,
    
    % Validate max position per pair (0.01 to 1.0)
    if 
        MaxPositionPerPair >= 0.01 andalso MaxPositionPerPair =< 1.0 -> ok;
        true -> throw({invalid_max_position_per_pair, MaxPositionPerPair})
    end,
    
    % Validate max total exposure (0.1 to 1.0)
    if 
        MaxTotalExposure >= 0.1 andalso MaxTotalExposure =< 1.0 -> ok;
        true -> throw({invalid_max_total_exposure, MaxTotalExposure})
    end,
    
    % Validate minimum account balance (positive number)
    if 
        MinAccountBalance > 0 -> ok;
        true -> throw({invalid_min_account_balance, MinAccountBalance})
    end,
    
    % Validate margin requirement (0.01 to 0.1)
    if 
        MarginRequirement >= 0.01 andalso MarginRequirement =< 0.1 -> ok;
        true -> throw({invalid_margin_requirement, MarginRequirement})
    end,
    
    % Validate max drawdown limit (0.05 to 0.5)
    if 
        MaxDrawdownLimit >= 0.05 andalso MaxDrawdownLimit =< 0.5 -> ok;
        true -> throw({invalid_max_drawdown_limit, MaxDrawdownLimit})
    end,
    
    % Validate daily trade limit (1 to 1000)
    if 
        DailyTradeLimit >= 1 andalso DailyTradeLimit =< 1000 -> ok;
        true -> throw({invalid_daily_trade_limit, DailyTradeLimit})
    end,
    
    % Validate position timeout (300 to 86400 seconds)
    if 
        PositionTimeout >= 300 andalso PositionTimeout =< 86400 -> ok;
        true -> throw({invalid_position_timeout, PositionTimeout})
    end,
    
    ok.

%% Validates currency pair configuration
validate_currency_pairs() ->
    CurrencyPairs = live_currency_pairs(),
    
    % Ensure currency pairs list is not empty
    if 
        length(CurrencyPairs) > 0 -> ok;
        true -> throw({empty_currency_pairs_list})
    end,
    
    % Validate each currency pair format
    lists:foreach(fun(Pair) ->
        case atom_to_list(Pair) of
            [C1,C2,C3,$.,C4,C5,C6] when 
                C1 >= $A, C1 =< $Z, C2 >= $A, C2 =< $Z, C3 >= $A, C3 =< $Z,
                C4 >= $A, C4 =< $Z, C5 >= $A, C5 =< $Z, C6 >= $A, C6 =< $Z -> ok;
            _ -> throw({invalid_currency_pair_format, Pair})
        end
    end, CurrencyPairs),
    
    ok.

%% Validates all live trading configuration parameters
validate_live_trading_config() ->
    try
        validate_ib_connection_config(),
        validate_risk_parameters(),
        validate_currency_pairs(),
        ok
    catch
        throw:Error -> {error, Error};
        error:Reason -> {error, {validation_error, Reason}}
    end.

%% Helper function to get all live trading configuration as a proplist
get_live_trading_config() ->
    [
        {ib_host, ib_host()},
        {ib_port, ib_port()},
        {ib_client_id, ib_client_id()},
        {live_position_size, live_position_size()},
        {live_max_daily_loss, live_max_daily_loss()},
        {live_currency_pairs, live_currency_pairs()},
        {live_max_position_per_pair, live_max_position_per_pair()},
        {live_max_total_exposure, live_max_total_exposure()},
        {live_min_account_balance, live_min_account_balance()},
        {live_margin_requirement, live_margin_requirement()},
        {live_max_drawdown_limit, live_max_drawdown_limit()},
        {live_daily_trade_limit, live_daily_trade_limit()},
        {live_position_timeout, live_position_timeout()}
    ].

%% ============================================================================
%% Live ETS Tables Configuration
%% ============================================================================

%% Live trading configuration
live_trading_enabled() -> false.  % Set to false to disable live trading
live_data_update_interval() -> 30000.  % 30 seconds (more frequent updates)
live_data_max_records() -> 1000.  % Max records per live table

%% Pull-on-demand strategy configuration
live_data_pull_timeout() -> 10000.  % 10 seconds timeout for IB data requests
live_data_freshness_threshold() -> 300.  % 5 minutes - data considered stale after this
live_data_pull_range_minutes() -> 20.  % Pull 20 minutes of data around requested time
live_data_fallback_strategy() -> historical.  % Options: historical, latest_available, fail

%% ============================================================================
%% Neural Network Initialization Timeouts
%% ============================================================================

%% Neural network deployment timeouts (in milliseconds)
neural_network_init_timeout() -> 60000.      % 60 seconds for neural network initialization
neural_network_deployment_timeout() -> 45000. % 45 seconds for model deployment
neural_network_startup_timeout() -> 120000.   % 2 minutes for complete startup process
neural_network_sensor_init_timeout() -> 30000. % 30 seconds for sensor initialization
neural_network_actuator_init_timeout() -> 15000. % 15 seconds for actuator initialization

%% Live trading startup timeouts (in milliseconds)
live_trading_startup_timeout() -> 180000.    % 3 minutes for complete live trading startup
live_scape_init_timeout() -> 45000.          % 45 seconds for live scape initialization
ib_connection_timeout() -> 30000.            % 30 seconds for IB connection establishment
ib_handshake_timeout() -> 20000.             % 20 seconds for IB handshake completion
live_data_initialization_timeout() -> 60000. % 60 seconds for live data table initialization

%% Process supervision timeouts
exoself_startup_timeout() -> 90000.          % 90 seconds for exoself process startup
cortex_initialization_timeout() -> 45000.    % 45 seconds for cortex initialization
neuron_spawn_timeout() -> 30000.             % 30 seconds for neuron spawning
sensor_actuator_link_timeout() -> 25000.     % 25 seconds for sensor/actuator linking

%% Retry and backoff configuration
neural_network_init_retries() -> 3.          % Number of retries for neural network initialization
neural_network_retry_delay() -> 5000.        % 5 seconds delay between retries
live_data_init_retries() -> 2.               % Number of retries for live data initialization
ib_connection_retries() -> 5.                % Number of retries for IB connection

