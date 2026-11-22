#!/bin/bash
# Docker test script for Phase 1 - Minimal Viable Bridge
# Connects to local Interactive Brokers TWS

echo "=== Phase 1 Docker Test - Local TWS Connection ==="
echo "Building Docker image with Python bridge..."

# Build the Docker image
docker build -t erlang-dev . || {
    echo "✗ Docker build failed"
    exit 1
}

echo "✓ Docker image built successfully"

echo ""
echo "Testing Python dependencies in container..."

# Test Python dependencies
docker run --rm -v ${PWD}:/app -w /app erlang-dev python3 test_python_deps.py || {
    echo "⚠ Python dependencies test had issues"
}

echo ""
echo "Testing Erlang compilation in container..."

# Test Erlang compilation and basic functionality
docker run --rm -v ${PWD}:/app -w /app erlang-dev erl -noshell -eval "
    test_phase1:test_all(),
    init:stop().
" || {
    echo "✗ Erlang tests failed"
    exit 1
}

echo ""
echo "✓ Phase 1 basic tests completed"
echo ""
echo "=== Ready to Connect to Local TWS ==="
echo ""
echo "1. Make sure TWS is running on your local machine (port 7497 for paper trading)"
echo "2. In TWS: Configure -> API -> Settings:"
echo "   - Enable ActiveX and Socket Clients ✓"
echo "   - Socket port: 7497"
echo "   - Master API client ID: 0"
echo "   - Read-Only API: ✗ (unchecked)"
echo ""
echo "3. Run the container with host networking:"
echo "   docker run -it --rm --network host -v \${PWD}:/app -w /app erlang-dev"
echo ""
echo "4. In Erlang shell, test the bridge:"
echo "   config:log_ib_config().                    % Check connection settings"
echo "   test_phase1:quick_test().                  % Basic validation"
echo "   test_ib_fixes:test_bridge_all().           % Full bridge tests"
echo ""
echo "5. If tests pass, start the full system:"
echo "   make:all([load])."
echo "   {ok, _} = ib_bridge_connector:start_connection(\"host.docker.internal\", 7497, 101)."
echo "   ib_bridge_connector:subscribe_market_data(\"EUR.USD\", 1)."