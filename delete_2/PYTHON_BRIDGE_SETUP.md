# Python Bridge Setup - Phase 1

## Quick Start

1. **Start TWS on your local machine:**
   - Open Interactive Brokers TWS
   - Go to Configure → API → Settings
   - Enable "ActiveX and Socket Clients" ✓
   - Set Socket port: 7497 (paper trading)
   - Set Master API client ID: 0
   - Uncheck "Read-Only API"

2. **Build and test the Docker container:**
   ```bash
   ./docker_test_phase1.sh
   ```

3. **Run the container with host networking:**
   ```bash
   docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev
   ```

4. **Test the bridge in Erlang shell:**
   ```erlang
   % Check configuration
   config:log_ib_config().
   
   % Quick validation
   test_phase1:quick_test().
   
   % Full bridge tests
   test_ib_fixes:test_bridge_all().
   
   % Manual bridge test
   make:all([load]).
   {ok, Pid} = ib_bridge_connector:start_connection("host.docker.internal", 7497, 101).
   ib_bridge_connector:get_connection_status().
   ib_bridge_connector:subscribe_market_data("EUR.USD", 1).
   ```

## Phase 1 Success Criteria

- [ ] Docker builds successfully with Python and ib_insync
- [ ] Bridge compiles without errors
- [ ] Python script starts and connects to TWS
- [ ] Heartbeat messages flow every 3 seconds
- [ ] Market data subscription works for EUR.USD
- [ ] Connection status tracking works
- [ ] Clean shutdown works

## Troubleshooting

### Connection Issues
- **"Connection refused"**: Make sure TWS is running and API is enabled
- **"Paper only"**: Bridge only connects to port 7497 (paper trading) for safety
- **"Host not found"**: Use `host.docker.internal` from Docker to reach host machine

### Python Issues
- **"ib_insync not found"**: Docker should install it automatically
- **"Python3 not found"**: Check Dockerfile Python installation

### Erlang Issues
- **"JSON encode/decode failed"**: Simple JSON implementation for basic cases only
- **"Bridge startup failed"**: Check Python script permissions and path

## Files Created

- `ib_bridge_connector.erl` - Drop-in replacement for ib_connector.erl
- `priv/ib_service.py` - Python bridge service using ib_insync
- `priv/requirements.txt` - Python dependencies
- Test files for validation

## Next Steps (Phase 2)

After Phase 1 works:
- Enhanced error handling
- Reconnection logic
- Multiple symbol support
- Order placement functionality