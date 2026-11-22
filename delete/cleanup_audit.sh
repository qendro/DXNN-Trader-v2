#!/bin/bash
# cleanup_audit.sh - Identify IB-related files and functions
# TEMPORARY FILE FOR DELETION

echo "=== IB Code Cleanup Audit ==="
echo "Scanning workspace for IB-related code..."
echo

# Find all Erlang files with IB-related content
echo "Files with IB-related code:"
find . -name "*.erl" -exec grep -l -i "ib_\|interactive\|tws\|gateway" {} \;

echo
echo "Files with IB API calls:"
find . -name "*.erl" -exec grep -l "reqMktData\|placeOrder\|cancelOrder\|reqHistoricalData" {} \;

echo
echo "Files with IB connection logic:"
find . -name "*.erl" -exec grep -l "connect\|disconnect\|connection" {} \;

echo
echo "=== Function Analysis ==="
echo "IB-related functions to review:"
grep -r "ib_" --include="*.erl" . | grep "^-" | sort | uniq

echo
echo "=== File Size Analysis ==="
echo "Current file sizes:"
wc -l *.erl | sort -nr | head -10

echo
echo "=== Cleanup Recommendations ==="
echo "Files to REMOVE (completely obsolete):"
echo "  - ib_proto.erl (protocol handling replaced by JSON)"
echo "  - ib_diag.erl (diagnostics replaced by Python bridge)"
echo "  - debug_tws_trust.erl (TWS-specific debugging)"
echo "  - test_ib_fixes.erl (IB-specific tests)"
echo
echo "Files to REPLACE:"
echo "  - ib_connector.erl → ib_bridge_connector.erl"
echo
echo "Files to REVIEW:"
echo "  - ib_config.hrl (keep useful constants)"
echo "  - live_trading_integration.erl (remove IB-specific logic)"
echo "  - live_scape.erl (check for IB-specific functions)"
echo "  - live_trader.erl (check for IB-specific functions)"