%% Function Analysis for Phase 5 Cleanup - TEMPORARY FILE
%% Analysis of functions to remove, replace, or keep

%% File: ib_connector.erl
%% Status: REPLACE with ib_bridge_connector.erl
%% Size: 1982 lines → ~400 lines (75% reduction)
%% Functions to replace:
%% - start_connection/3 (replaced by bridge)
%% - stop_connection/0 (replaced by bridge)
%% - subscribe_market_data/2 (replaced by bridge)
%% - place_order/4 (replaced by bridge)
%% - get_connection_status/0 (replaced by bridge)
%% - get_market_data/1 (replaced by bridge)
%% - init_market_data_tables/0 (replaced by bridge)
%% - cleanup_market_data_tables/0 (replaced by bridge)
%% - test_connectivity/0 (replaced by bridge)
%% - test_handshake_detailed/0 (replaced by bridge)

%% File: ib_proto.erl
%% Status: REMOVE (completely obsolete)
%% Functions to remove:
%% - z/1 (null terminator - not needed with JSON)
%% - i2b/1 (integer to binary - not needed with JSON)
%% - read_cstring/1 (C-string reading - not needed with JSON)
%% - read_cstring/2 (C-string reading - not needed with JSON)

%% File: ib_diag.erl
%% Status: REMOVE (replaced by Python bridge diagnostics)
%% Functions to remove:
%% - test_env/0 (environment testing)
%% - test_tcp/0 (TCP connectivity testing)
%% - test_handshake/1 (handshake testing)
%% - test_comprehensive/0 (comprehensive testing)
%% - log_hex/2 (hex logging utility)

%% File: debug_tws_trust.erl
%% Status: REMOVE (TWS-specific debugging)
%% Functions to remove:
%% - perform_debug_handshake/3 (debug handshake)
%% - All debugging utilities

%% File: test_ib_fixes.erl
%% Status: REMOVE (IB-specific tests replaced by bridge tests)
%% Functions to remove:
%% - test_all/0 (IB-specific tests)
%% - test_configuration/0 (IB configuration tests)
%% - test_basic_connectivity/0 (basic connectivity tests)
%% - test_full_connection/0 (full connection tests)

%% File: ib_config.hrl
%% Status: REVIEW (keep useful constants, remove IB-specific)
%% Keep:
%% - IB_HOST, IB_PORT (connection parameters)
%% - IB_CONNECT_TIMEOUT (useful timeout)
%% Remove:
%% - IB_CLIENT_VERSION (protocol-specific)
%% - IB_TCP_OPTS (TCP-specific options)
%% - Protocol version constants

%% File: live_trading_integration.erl
%% Status: REVIEW (remove IB-specific logic)
%% Keep: Generic supervision and orchestration
%% Review: Any hardcoded ib_connector references

%% File: live_scape.erl
%% Status: REVIEW (check for IB-specific functions)
%% Keep: Generic sensor/actuator interface
%% Review: Any direct IB API calls

%% File: live_trader.erl
%% Status: REVIEW (check for IB-specific functions)
%% Keep: Generic trading orchestration
%% Review: Any hardcoded ib_connector references