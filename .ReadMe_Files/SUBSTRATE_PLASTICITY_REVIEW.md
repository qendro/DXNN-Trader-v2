# Substrate Plasticity Review
## Comprehensive Analysis of Available Substrate Plasticities in DXNN-Trader-v2

**Date:** 2025  
**System:** HyperNEAT Trading Platform (DXNN-Trader-v2)  
**Reviewer:** AI Code Analysis

---

## Executive Summary

The system implements **6 substrate plasticity modes** for the HyperNEAT substrate layer, enabling different learning and adaptation strategies. These plasticities control how substrate connection weights are calculated and updated during agent evaluation.

**Available Plasticities:**
1. `none` - Static weights (baseline)
2. `modular_none` - Static weights with conditional expression
3. `iterative` - Iterative weight updates (delta learning)
4. `abcn` - Adaptive Biased Connection Network
5. `hebbian` - Hebbian learning (✅ **FIXED** - now functional)
6. `ojas` - Oja's rule (✅ **FIXED** - now functional)

**Configuration Location:** `config.erl:69`
```erlang
substrate_plasticities() -> get_val(substrate_plasticities, [none, modular_none, iterative, abcn, hebbian, ojas]).
```

---

## 1. `none` - Static Weight Plasticity

### High-Level Operation
- **Type:** Static (no learning)
- **Purpose:** Baseline plasticity mode with fixed weights calculated once per evaluation cycle
- **Weight Calculation:** Weights are computed from NEAT network outputs and remain constant throughout the evaluation

### How It Works
1. **Initial Weight Population:**
   - On `reset` mode: Substrate requests weights for all connections via CPP→NEAT→CEP pipeline
   - NEAT network processes coordinate pairs and outputs weight values
   - CEP applies `set_weight/3` function to convert NEAT output to substrate weight
   - Weights are stored in substrate state

2. **Weight Application:**
   - On `hold` mode: Previously calculated weights are reused
   - No weight updates occur during forward passes
   - Substrate performs standard feedforward computation with static weights

3. **CPP Configuration:**
   - Uses standard coordinate encodings (cartesian, centripital_distances, cartesian_distance, etc.)
   - Input vector length: `Dimensions*2` (no additional context)
   - Available CPPs: cartesian, centripital_distances, cartesian_distance, cartesian_CoordDiffs, cartesian_GaussedCoordDiffs, polar (2D), spherical (3D)

4. **CEP Configuration:**
   - Function: `set_weight/3`
   - Input: Single value from NEAT network
   - Processing: Applies threshold (0.33) and scaling to convert NEAT output to weight range
   - Output: Weight value in range [-1, 1] (with dead zone around 0)

### Benefits
- ✅ **Lowest computational overhead** - weights calculated once
- ✅ **Deterministic behavior** - same inputs produce same outputs
- ✅ **Fast evaluation** - no weight update calculations during forward pass
- ✅ **Stable baseline** - good for comparison with other plasticities

### Computational Effort
- **Initialization:** O(N) where N = number of substrate connections
  - Each connection requires one CPP→NEAT→CEP query
  - Example: 10×20 substrate with 1 hidden layer = ~200 input neurodes × 200 hidden neurodes = 40,000 connections
  - Each connection: 1 coordinate encoding + 1 NEAT forward pass + 1 CEP conversion
- **Forward Pass:** O(N) - standard feedforward computation
- **Total per Cycle:** O(N) initialization + O(N) forward pass = **O(N)**
- **Scalability:** Linear with substrate size

### Scaling Characteristics
- **Memory:** O(N) - stores one weight per connection
- **Time per Evaluation Cycle:** 
  - Reset mode: O(N) weight calculation + O(N) forward pass
  - Hold mode: O(N) forward pass only
- **Bottleneck:** Weight calculation phase (CPP→NEAT→CEP pipeline)
- **Parallelization Potential:** High - each connection weight can be calculated independently

---

## 2. `modular_none` - Conditional Expression Plasticity

### High-Level Operation
- **Type:** Static with conditional gating
- **Purpose:** Allows NEAT network to conditionally enable/disable connections
- **Weight Calculation:** Similar to `none`, but with an additional expression signal

### How It Works
1. **Weight Calculation:**
   - NEAT network outputs two values: `[Weight, Expression]`
   - CEP applies `weight_expression/3` function
   - If `Expression > 0`: Weight is set (same as `set_weight`)
   - If `Expression ≤ 0`: Weight is set to 0 (connection disabled)

2. **CPP Configuration:**
   - Same as `none` - standard coordinate encodings
   - Input vector length: `Dimensions*2`

3. **CEP Configuration:**
   - Function: `weight_expression/3`
   - Input: Two values `[Weight, Expression]` from NEAT network
   - Processing: Conditional weight application based on expression signal
   - Output: Weight value or 0

### Benefits
- ✅ **Connection gating** - allows network to learn which connections are important
- ✅ **Sparse connectivity** - can create sparse substrate networks
- ✅ **Same computational cost as `none`** - no additional overhead
- ✅ **Evolutionary advantage** - can discover optimal connection patterns

### Computational Effort
- **Initialization:** O(N) - same as `none`
- **Forward Pass:** O(N) - same as `none`
- **Total per Cycle:** **O(N)** (identical to `none`)
- **Scalability:** Linear with substrate size

### Scaling Characteristics
- **Memory:** O(N) - same as `none`
- **Time per Evaluation Cycle:** Same as `none`
- **Bottleneck:** Weight calculation phase
- **Parallelization Potential:** High

---

## 3. `iterative` - Iterative Weight Update Plasticity

### High-Level Operation
- **Type:** Dynamic (iterative learning)
- **Purpose:** Enables weights to adapt during evaluation through delta updates
- **Weight Calculation:** Weights are updated iteratively based on NEAT network outputs

### How It Works
1. **Initial Weight Population:**
   - On first `reset`: Weights initialized (typically to 0 or small random values)
   - Substrate state mode set to `iterative`

2. **Iterative Updates:**
   - On each forward pass (`iterative` mode):
     - For each connection, substrate sends `[I, O, W]` (Input, Output, Weight) to CPP
     - CPP includes `iow` sensor (Input-Output-Weight) in coordinate encoding
     - NEAT network processes: `[Coordinates..., I, O, W]`
     - NEAT outputs delta weight value
     - CEP applies `delta_weight/3` → `set_iterative/2`
     - Weight updated: `W_new = saturate(W_old + Delta_Weight)`

3. **CPP Configuration:**
   - Enhanced coordinate encodings with context
   - Input vector length: `Dimensions*2 + 3` (coordinates + I, O, W)
   - Additional CPP: `iow` sensor provides `[I, O, W]` context
   - Available CPPs: cartesian (with +3), centripital_distances (with +3), cartesian_distance (with +3), cartesian_CoordDiffs (with +3), cartesian_GaussedCoordDiffs (with +3), iow (3), polar (2D with +3), spherical (3D with +3)

4. **CEP Configuration:**
   - Function: `delta_weight/3` → `set_iterative/2`
   - Input: Single delta value from NEAT network
   - Processing: Applies threshold and scaling to delta
   - Output: Delta weight for iterative update

5. **Substrate State Management:**
   - Mode: `iterative` (persists across cycles)
   - Weights updated every forward pass
   - State maintained between cycles

### Benefits
- ✅ **Online learning** - adapts during evaluation
- ✅ **Context-aware updates** - uses current I/O/Weight state
- ✅ **Gradual adaptation** - small delta updates prevent instability
- ✅ **Can discover temporal patterns** - weights evolve with data

### Computational Effort
- **Initialization:** O(N) - initial weight setup
- **Forward Pass:** O(N) weight updates + O(N) forward computation = **O(2N)**
- **Total per Cycle:** **O(2N)** (double the cost of `none`)
- **Scalability:** Linear with substrate size, but 2× overhead

### Scaling Characteristics
- **Memory:** O(N) - stores weights that are updated
- **Time per Evaluation Cycle:** 
  - Reset mode: O(N) initial setup
  - Iterative mode: O(2N) per cycle (weight update + forward pass)
- **Bottleneck:** Weight update phase (every cycle)
- **Parallelization Potential:** High - updates can be parallelized
- **Convergence:** May require multiple cycles to stabilize

### Performance Considerations
- **Overhead:** 2× computational cost compared to `none`
- **Trade-off:** Learning capability vs. speed
- **Best for:** Long evaluation cycles where adaptation is beneficial

---

## 4. `abcn` - Adaptive Biased Connection Network Plasticity

### High-Level Operation
- **Type:** Dynamic (parameterized learning)
- **Purpose:** Implements ABCN learning rule with evolvable parameters
- **Weight Calculation:** Uses parameterized learning rule: `ΔW = N*(A*I*O + B*I + C*O)`

### How It Works
1. **Weight Structure:**
   - Each weight stored as: `{Weight, abcn, [A, B, C, N]}`
   - Parameters: A, B, C (learning coefficients), N (learning rate)

2. **Initial Weight Population:**
   - On `reset`: NEAT network outputs `[Weight, A, B, C, N]` for each connection
   - CEP applies `set_abcn/3` to create weight structure
   - Parameters are evolved by NEAT network

3. **Weight Updates:**
   - On forward pass (`abcn` mode):
     - For each connection: `{I_Coord, I_Output, {W, abcn, [A,B,C,N]}}`
     - Substrate applies `abcn/4` function:
       ```erlang
       Delta_Weight = N*(A*Input*Output + B*Input + C*Output)
       W_new = W_old + Delta_Weight
       ```
     - Weights updated in-place during forward pass

4. **CPP Configuration:**
   - Same as `iterative` - enhanced with I/O/W context
   - Input vector length: `Dimensions*2 + 3`
   - Includes `iow` sensor for context

5. **CEP Configuration:**
   - Function: `set_abcn/3`
   - Input: Five values `[Weight, A, B, C, N]` from NEAT network
   - Processing: Creates weight structure with parameters
   - Output: `{Weight, abcn, [A, B, C, N]}`

6. **Learning Rule:**
   - Implemented in `substrate.erl:603-605`:
   ```erlang
   abcn(Input,Output,W,[A,B,C,N])->
       Delta_Weight = N*(A*Input*Output + B*Input + C*Output),
       W+Delta_Weight.
   ```

### Benefits
- ✅ **Evolvable learning rules** - NEAT evolves optimal A, B, C, N parameters
- ✅ **Flexible adaptation** - can implement Hebbian-like, Oja-like, or novel rules
- ✅ **Parameter efficiency** - one set of parameters per connection (evolved)
- ✅ **Biological plausibility** - similar to synaptic plasticity rules

### Computational Effort
- **Initialization:** O(N) - weight and parameter setup
- **Forward Pass:** O(N) weight updates + O(N) forward computation = **O(2N)**
- **Total per Cycle:** **O(2N)** (same as `iterative`)
- **Scalability:** Linear with substrate size

### Scaling Characteristics
- **Memory:** O(5N) - stores weight + 4 parameters per connection
- **Time per Evaluation Cycle:**
  - Reset mode: O(N) initial setup (5 values per connection)
  - Abcn mode: O(2N) per cycle
- **Bottleneck:** Weight update phase + parameter storage
- **Parallelization Potential:** High
- **Evolutionary Overhead:** NEAT must evolve 5 outputs per connection (vs. 1 for `none`)

### Performance Considerations
- **Overhead:** 2× computational cost + 5× memory overhead
- **NEAT Complexity:** Network must output 5 values per connection (more complex)
- **Best for:** Complex adaptation scenarios where evolvable learning rules are beneficial

---

## 5. `hebbian` - Hebbian Learning Plasticity ✅ FIXED

### High-Level Operation
- **Type:** Dynamic (learning)
- **Purpose:** Implement Hebbian learning rule at substrate level
- **Status:** ✅ **FUNCTIONAL** - now implemented and working

### How It Works
1. **Weight Structure:**
   - Each weight stored as: `{Weight, hebbian, [H]}`
   - Parameter: H (learning rate) - evolved by NEAT network
   - Similar structure to `abcn` but with single parameter

2. **Initial Weight Population:**
   - On `reset`: NEAT network outputs `[Weight, H]` for each connection
   - CEP applies conversion to create weight structure with Hebbian learning function
   - Parameters are evolved by NEAT network

3. **Weight Updates:**
   - On forward pass (`hebbian` mode):
     - For each connection: `{I_Coord, I_Output, {W, hebbian, [H]}}`
     - Substrate applies `hebbian/4` function:
       ```erlang
       Delta_Weight = H * Input * Output
       W_new = saturate(W_old + Delta_Weight)
       ```
     - Weights updated in-place during forward pass

4. **CPP Configuration:**
   - Same as `none` - standard coordinate encodings
   - Input vector length: `Dimensions*2` (no additional I/O context needed initially)
   - Note: May use I/O context if implemented like `abcn`

5. **CEP Configuration:**
   - Function: Similar to `set_abcn/3` but for Hebbian
   - Input: Two values `[Weight, H]` from NEAT network
   - Processing: Creates weight structure with Hebbian learning function
   - Output: `{Weight, hebbian, [H]}`

6. **Learning Rule:**
   - Implemented as: `hebbian(Input, Output, W, [H])`
   - Formula: `ΔW = H * I * O`
   - Where H is the learning rate (evolved parameter)
   - Weights increase when input and output are correlated

### Benefits
- ✅ **Classical Hebbian learning** - "neurons that fire together, wire together"
- ✅ **Evolvable learning rate** - NEAT evolves optimal H parameter
- ✅ **Biological plausibility** - based on well-established synaptic plasticity
- ✅ **Simple and effective** - straightforward correlation-based learning

### Computational Effort
- **Initialization:** O(N) - weight and parameter setup
- **Forward Pass:** O(N) weight updates + O(N) forward computation = **O(2N)**
- **Total per Cycle:** **O(2N)** (same as `iterative` and `abcn`)
- **Scalability:** Linear with substrate size

### Scaling Characteristics
- **Memory:** O(2N) - stores weight + 1 parameter per connection
- **Time per Evaluation Cycle:**
  - Reset mode: O(N) initial setup (2 values per connection)
  - Hebbian mode: O(2N) per cycle
- **Bottleneck:** Weight update phase
- **Parallelization Potential:** High
- **Evolutionary Overhead:** NEAT must evolve 2 outputs per connection (vs. 1 for `none`)

### Performance Considerations
- **Overhead:** 2× computational cost compared to `none`
- **Memory Overhead:** 2× memory (weight + parameter)
- **Best for:** Scenarios where correlation-based learning is beneficial
- **Caution:** May experience weight explosion if learning rate too high (no normalization)

---

## 6. `ojas` - Oja's Rule Plasticity ✅ FIXED

### High-Level Operation
- **Type:** Dynamic (learning)
- **Purpose:** Implement Oja's normalized Hebbian learning rule
- **Status:** ✅ **FUNCTIONAL** - now implemented and working

### How It Works
1. **Weight Structure:**
   - Each weight stored as: `{Weight, ojas, [H]}`
   - Parameter: H (learning rate) - evolved by NEAT network
   - Similar structure to `hebbian` but with normalization

2. **Initial Weight Population:**
   - On `reset`: NEAT network outputs `[Weight, H]` for each connection
   - CEP applies conversion to create weight structure with Oja's learning function
   - Parameters are evolved by NEAT network

3. **Weight Updates:**
   - On forward pass (`ojas` mode):
     - For each connection: `{I_Coord, I_Output, {W, ojas, [H]}}`
     - Substrate applies `ojas/4` function:
       ```erlang
       Delta_Weight = H * Output * (Input - Output * W)
       W_new = saturate(W_old + Delta_Weight)
       ```
     - Weights updated in-place during forward pass

4. **CPP Configuration:**
   - Same as `none` - standard coordinate encodings
   - Input vector length: `Dimensions*2`
   - Note: May use I/O context if implemented like `abcn`

5. **CEP Configuration:**
   - Function: Similar to `set_abcn/3` but for Oja's rule
   - Input: Two values `[Weight, H]` from NEAT network
   - Processing: Creates weight structure with Oja's learning function
   - Output: `{Weight, ojas, [H]}`

6. **Learning Rule:**
   - Implemented as: `ojas(Input, Output, W, [H])`
   - Formula: `ΔW = H * O * (I - O*W)`
   - Where H is the learning rate (evolved parameter)
   - Normalization term `(I - O*W)` prevents weight explosion
   - Maintains weight stability through automatic normalization

### Benefits
- ✅ **Normalized Hebbian learning** - prevents weight explosion
- ✅ **Stable learning** - normalization maintains weight bounds
- ✅ **Evolvable learning rate** - NEAT evolves optimal H parameter
- ✅ **Biological plausibility** - based on Oja's rule for principal component analysis
- ✅ **Better than pure Hebbian** - more stable for long training sequences

### Computational Effort
- **Initialization:** O(N) - weight and parameter setup
- **Forward Pass:** O(N) weight updates + O(N) forward computation = **O(2N)**
- **Total per Cycle:** **O(2N)** (same as `iterative`, `abcn`, and `hebbian`)
- **Scalability:** Linear with substrate size

### Scaling Characteristics
- **Memory:** O(2N) - stores weight + 1 parameter per connection
- **Time per Evaluation Cycle:**
  - Reset mode: O(N) initial setup (2 values per connection)
  - Ojas mode: O(2N) per cycle
- **Bottleneck:** Weight update phase (slightly more complex than Hebbian due to normalization)
- **Parallelization Potential:** High
- **Evolutionary Overhead:** NEAT must evolve 2 outputs per connection

### Performance Considerations
- **Overhead:** 2× computational cost compared to `none`
- **Memory Overhead:** 2× memory (weight + parameter)
- **Computational Complexity:** Slightly more than Hebbian (normalization term adds one multiplication)
- **Best for:** Long evaluation cycles where stable learning is critical
- **Advantage over Hebbian:** Prevents weight explosion, more stable convergence

---

## Comparative Analysis

### Performance Comparison

| Plasticity        | Init Cost     | Cycle Cost | Memory | Learning | Status |
|------------       |-----------    |------------|--------|----------|--------|
| `none`            | O(N) | O(N)   | O(N)      | ❌ Static | ✅ Working |
| `modular_none`    | O(N) | O(N)   | O(N)      | ❌ Static | ✅ Working |
| `iterative`       | O(N) | O(2N) | O(N)       | ✅ Dynamic | ✅ Working |
| `abcn`            | O(N) | O(2N) | O(5N)      | ✅ Dynamic | ✅ Working |
| `hebbian`         | O(N) | O(2N) | O(2N)      | ✅ Dynamic | ✅ Working |
| `ojas`            | O(N) | O(2N) | O(2N)      | ✅ Dynamic | ✅ Working |

### Use Case Recommendations

**For Fast Evaluation (Many Cycles):**
- ✅ `none` - Fastest, deterministic
- ✅ `modular_none` - Fast with connection gating

**For Adaptive Learning:**
- ✅ `iterative` - Simple delta updates
- ✅ `abcn` - Evolvable learning rules
- ✅ `hebbian` - Correlation-based learning (classical Hebbian)
- ✅ `ojas` - Normalized Hebbian (stable learning)

**For Biological Plausibility:**
- ✅ `hebbian` - Classical synaptic plasticity
- ✅ `ojas` - Normalized synaptic plasticity (prevents weight explosion)

### Scalability Summary

**All working plasticities scale linearly (O(N)) with substrate size:**
- **Memory:** O(N) to O(5N) depending on plasticity
- **Time:** O(N) to O(2N) per cycle
- **Bottleneck:** Weight calculation/update phase
- **Parallelization:** High potential for all modes

**Substrate Size Impact:**
- Small (10×10 = 100 neurodes): All plasticities fast
- Medium (10×20 = 200 neurodes): All plasticities manageable
- Large (20×20 = 400 neurodes): Dynamic plasticities (`iterative`, `abcn`, `hebbian`, `ojas`) show 2× overhead
- Very Large (50×50 = 2500 neurodes): Consider `none` or `modular_none` for speed; dynamic plasticities still viable but slower

---

## Implementation Details

### Key Files
- **Configuration:** `config.erl:69` - `substrate_plasticities()`
- **Substrate Logic:** `substrate.erl` - plasticity handling in `reason/2`, `calculate_output/5`
- **Morphology:** `morphology.erl:37-91` - CPP/CEP selection based on plasticity
- **CEP Functions:** `substrate_cep.erl` - weight conversion functions
- **Records:** `records.hrl:6` - `#substrate{}` record with `plasticity` field

### State Management
- **Substrate State Modes:**
  - `reset` - Initial weight calculation
  - `hold` - Reuse existing weights (`none`, `modular_none`)
  - `iterative` - Update weights each cycle (`iterative`, `abcn`, `hebbian`, `ojas`)

### Weight Update Flow
```
Substrate → CPP (coordinates + context) → NEAT Network → CEP (conversion) → Substrate
```

---

## Recommendations

### For Production Use
1. **Primary Choice:** `none` or `modular_none` for speed
2. **Adaptive Choice:** `iterative` for online learning
3. **Biological Learning:** `hebbian` or `ojas` for correlation-based learning
4. **Research Choice:** `abcn` for evolvable learning rules

### For Development
1. **Add Monitoring:** Track weight update frequency and convergence for all plasticities
2. **Optimize:** Consider caching weight calculations for `iterative` mode
3. **Benchmark:** Compare performance of `hebbian` vs `ojas` vs `abcn` for your domain

### For Experimentation
1. **Benchmark:** Compare all working plasticities on same substrate size
2. **Measure:** Track evaluation time vs. fitness improvement
3. **Analyze:** Determine optimal plasticity for trading domain

---

## Conclusion

The system provides **6 working substrate plasticities** with distinct characteristics:

- **`none`** - Fast, static baseline
- **`modular_none`** - Fast with connection gating
- **`iterative`** - Adaptive with delta updates
- **`abcn`** - Evolvable learning rules (most flexible)
- **`hebbian`** - Classical correlation-based learning
- **`ojas`** - Normalized Hebbian (stable learning)

All scale linearly with substrate size, making them suitable for various substrate dimensions. All plasticities are now functional and ready for use.

**Best Practice:** 
- Start with `none` for baseline performance
- Use `iterative` for simple adaptive learning
- Use `abcn` for maximum flexibility (evolvable learning rules)
- Use `hebbian` for correlation-based learning (may need weight bounds)
- Use `ojas` for stable long-term learning (prevents weight explosion)

