#!/bin/bash
# Docker test script for Phase 2 - Essential Reliability
# Tests enhanced error handling, reconnection, and symbol normalization

echo "=== Phase 2 Docker Test - Essential Reliability ==="
echo "Building Docker image with Phase 2 enhancements..."

# Build the Docker image
docker build -t erlang-dev . || {
    echo "✗ Docker build failed"
    exit 1
}

echo "✓ Docker image built successfully"

echo ""
echo "Testing Phase 2 compilation in container..."

# Test Phase 2 compilation and functionality
docker run --rm -v ${PWD}:/app -w /app erlang-dev erl -noshell -eval "
    make:all([load]),
    test_phase2:test_all(),
    init:stop().
" || {
    echo "✗ Phase 2 tests failed"
    exit 1
}

echo ""
echo "✓ Phase 2 basic tests completed"
echo ""
echo "=== Phase 2 Ready for Integration Testing ==="
echo ""
echo "Enhanced Features Available:"
echo "  ✓ Enhanced error handling with proper error codes"
echo "  ✓ Automatic reconnection with connection monitoring"
echo "  ✓ Symbol normalization for multiple currency pairs"
echo "  ✓ Clean shutdown with graceful termination"
echo ""
echo "To test with TWS:"
echo "  1. Start TWS on your local machine (port 7497)"
echo "  2. Run: docker run -it --rm --network host -v \${PWD}:/app -w /app erlang-dev"
echo "  3. Test: test_phase2:quick_test()."
echo ""
echo "Multiple symbol test:"
echo "  ib_bridge_connector:subscribe_market_data(\"EUR.USD\", 1)."
echo "  ib_bridge_connector:subscribe_market_data(\"GBP.USD\", 2)."
echo "  ib_bridge_connector:subscribe_market_data(\"USD.JPY\", 3)."