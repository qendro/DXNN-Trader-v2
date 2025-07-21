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
   % Print the best genotype from the default 'test' population
   genotype_utils:print_best_genotype().
   
   % Or specify a population ID
   genotype_utils:print_best_genotype(your_population_id).
   
   % List all agents with their fitness scores
   genotype_utils:list_all_agents().
   
   % Print top N agents
   genotype_utils:print_top_agents(5).
   
   % Get agent statistics
   genotype_utils:get_agent_stats().




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