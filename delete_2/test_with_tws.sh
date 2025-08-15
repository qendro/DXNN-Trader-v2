#!/bin/bash
# Interactive test script for Phase 2 with live TWS

echo "=== Phase 2 Live TWS Test ==="
echo "Starting interactive Erlang shell with TWS connection..."
echo ""
echo "Make sure TWS is running with:"
echo "  - Port 7497 (paper trading)"
echo "  - API enabled"
echo "  - Client ID 0 or higher"
echo ""
echo "Starting Docker container..."

docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev