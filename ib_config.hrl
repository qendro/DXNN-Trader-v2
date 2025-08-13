%% IB Configuration Constants
%% Runtime configurable values for IB TWS API connection

%% Connection defaults
-define(IB_HOST, "127.0.0.1").
-define(IB_PORT, 7497).                    % Paper trading TWS; Gateway often 4002/4001
-define(IB_CONNECT_TIMEOUT, 5000).
-define(IB_HANDSHAKE_TIMEOUT, 5000).

%% Client protocol version (integer), NOT marketing "9.76.1"
%% Keep configurable; do not hardcode stale values
-define(IB_CLIENT_VERSION, 38).            % Conservative version
-define(IB_CLIENT_DATE, <<"">>).           % Empty string, NOT date

%% TCP Options
-define(IB_TCP_OPTS, [
    binary, 
    {active, false}, 
    {packet, 0}, 
    {nodelay, true},
    {keepalive, true}, 
    {send_timeout, 5000},
    {send_timeout_close, true}
]).

%% Options
-define(IB_EXTRA_AUTH, false).             % Two-step API auth flags if needed

%% Server version thresholds for feature gating
-define(IB_MIN_SERVER_VER, 38).
-define(IB_SERVER_VER_PNL, 142).           % PnL support
-define(IB_SERVER_VER_TICK_BY_TICK, 100).  % Tick-by-tick data
-define(IB_SERVER_VER_MARKET_DEPTH, 50).   % Market depth