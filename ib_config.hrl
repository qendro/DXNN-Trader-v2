%% IB Configuration Constants
%% Runtime configurable values for IB TWS API connection

%% Connection defaults
-define(IB_HOST, "127.0.0.1").
-define(IB_PORT, 7497).                    % Paper trading TWS; Gateway often 4002/4001
-define(IB_CONNECT_TIMEOUT, 5000).
-define(IB_HANDSHAKE_TIMEOUT, 5000).

%% Client protocol version (integer), NOT marketing "9.76.1"
%% Keep configurable; do not hardcode stale values
-define(IB_CLIENT_VERSION, 151).           % Modern version - make runtime configurable
-define(IB_CLIENT_DATE, <<"20250101">>).   % YYYYMMDD format - runtime configurable

%% TCP Options
-define(IB_TCP_OPTS, [
    binary, 
    {active, false}, 
    {packet, raw}, 
    {nodelay, true}, 
    {keepalive, true}
]).

%% Options
-define(IB_EXTRA_AUTH, false).             % Two-step API auth flags if needed

%% Server version thresholds for feature gating
-define(IB_MIN_SERVER_VER, 38).
-define(IB_SERVER_VER_PNL, 142).           % PnL support
-define(IB_SERVER_VER_TICK_BY_TICK, 100).  % Tick-by-tick data
-define(IB_SERVER_VER_MARKET_DEPTH, 50).   % Market depth