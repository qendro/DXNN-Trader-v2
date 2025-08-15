```bash
make all
make shell
```
``` erlang
make_all:all().
```

# Build once
```bash
docker build -t erlang-dev .

# Run your neural network system
docker run -it --rm -v ${PWD}:/app -w /app erlang-dev
docker run -it --rm --network host -v ${PWD}:/app -w /app erlang-dev
```
``` erlang
# Inside container:
make:all().
mnesia:create_schema([node()]).
mnesia:start().
fx:init().
fx:start().
polis:create().
polis:start().
polis:sync().
benchmarker:start(sliding_window_5).

benchmarker:start(chart_plane_5x10).
# ... your neural network commands

```
## Reseting Mnesia
```bash
find . -name "*.beam" -delete
```
```erlang
mnesia:stop().
mnesia:delete_schema([node()]).
q().
```

4. **Print the best genotype:**
   ```erlang
  
   rr("records.hrl").
   % Print the best genotype from the default 'test' population
   genotype_utils:print_best_genotype().

   genotype_utils:print_best_genotype(all).
   
   % Or specify a population ID
   genotype_utils:print_best_genotype(your_population_id).
   
   % List all agents with their fitness scores
   genotype_utils:list_all_agents().
   
   % Print top N agents
   genotype_utils:print_top_agents(5).
   
   % Get agent statistics
   genotype_utils:get_agent_stats().

   BestAgentId = {5.699247180669372e-10,agent}.
   exoself:start(Best_Agent_Id, self(),benchmark).




    benchmarker:start(sliding_window_10).
    benchmarker:start(sliding_window_20).
    benchmarker:start(sliding_window_50).
    benchmarker:start(sliding_window_100).
    benchmarker:start(chart_plane_5x10).
    benchmarker:start(chart_plane_5x20).
    benchmarker:start(chart_plane_10x10).
    benchmarker:start(chart_plane_10x20).
    benchmarker:start(chart_plane_20x10).
    benchmarker:start(chart_plane_20x20).
    benchmarker:start(chart_plane_50x10).
    benchmarker:start(chart_plane_50x20).
    benchmarker:start(chart_plane_100x10).
   ```


   # Live Trader

   ## Initialization 

   ```
```erlang
make:all([load]).
config:validate_ib_connection_config().  
ib_bridge_connector:start_default_connection().
ib_bridge_connector:stop_connection().

live_trading_integration:get_system_status().
live_trading_integration:startup_step_ib_connection().
live_trading_integration:subscribe_to_all_pairs([]).
live_trading_integration:initialize_performance_monitoring().
live_trading_integration:get_default_risk_parameters().
live_trading_integration:test_system_integration().
live_trading_integration:
live_trading_integration:

Setup Commands
First, start the system:

% Compile all modules
make:all([load]).

% Start the live trading system
live_trading_integration:start().
% Or start with specific configuration
live_trading_integration:start([{host, "127.0.0.1"}, {port, 7497}, {client_id, 1}]).

Market Data Commands
Get live market data for EUR/USD and USD/JPY:

live_trading_integration:subscribe_market_data("EUR", "USD").
live_trading_integration:subscribe_market_data("USD", "JPY").
live_trading_integration:get_market_data("EUR", "USD").
live_trading_integration:get_market_data("USD", "JPY").

Order Placement Commands
Place orders for each currency pair:

live_trading_integration:place_order("EUR", "USD", buy, 0.01).
live_trading_integration:place_order("EUR", "USD", sell, 0.01).
live_trading_integration:place_order("USD", "JPY", buy, 0.01).
live_trading_integration:place_order("USD", "JPY", sell, 0.01).

Useful Testing Commands
Connection and status checks:

live_trading_integration:get_connection_status().
live_trading_integration:test_connection().
live_trading_integration:get_account_info().
live_trading_integration:get_positions().
live_trading_integration:get_orders().

System monitoring:

% Check if live trading process is running
whereis(live_trading_integration).
% Get system state
sys:get_state(live_trading_integration).
live_trading_integration:stop().
live_trading_integration:restart().

Debug and logging:

% Enable debug mode
live_trading_integration:set_debug(true).
% Get recent logs
live_trading_integration:get_logs().
% Clear logs
live_trading_integration:clear_logs().

Interactive Broker Specific Commands
If using the IB bridge:

ib_bridge_connector:start().
ib_bridge_connector:ping().
ib_bridge_connector:get_account_summary().

```