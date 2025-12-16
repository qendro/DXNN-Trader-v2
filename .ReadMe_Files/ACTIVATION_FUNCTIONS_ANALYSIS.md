# Neural Activation Functions Analysis
## DXNN-Trader-v2 HyperNEAT Trading System

**Date:** 2025  
**System:** Distributed Extended Neural Network (DXNN) with HyperNEAT substrate encoding  
**Purpose:** Comprehensive review of available activation functions, their characteristics, computational costs, and scalability

---

## Executive Summary

The DXNN-Trader-v2 system implements **9 actively used activation functions** (with 6 additional functions available but disabled due to stability issues). These functions are applied in two contexts:

1. **NEAT Neurons**: Small evolving networks that generate weights for the HyperNEAT substrate
2. **Substrate Neurodes**: Large spatial networks that process forex price data (currently hardcoded to `tanh`)

The system tracks activation function distribution across the population, allowing evolutionary selection of optimal function combinations for trading performance.

---

## Activation Function Inventory

### ✅ **Actively Used Functions** (9 total)

These functions are enabled in `config.erl` and tracked in `genotype.erl`:

1. **tanh** - Hyperbolic tangent
2. **sin** - Sine function
3. **cos** - Cosine function
4. **gaussian** - Gaussian (radial basis) function
5. **absolute** - Absolute value
6. **sgn** - Sign function
7. **log** - Logarithmic function
8. **sqrt** - Square root
9. **linear** - Identity function

### ⚠️ **Implemented but Disabled** (6 total)

These functions exist in `functions.erl` but are **not enabled** in configuration due to reported crashes:

1. **quadratic** - Quadratic function
2. **sigmoid** - Sigmoid function (-1 to 1)
3. **sigmoid1** - Alternative sigmoid
4. **multiquadric** - Multiquadric radial basis function
5. **bin** - Binary threshold
6. **trinary** - Three-state threshold

---

## Detailed Function Analysis

### 1. **tanh** (Hyperbolic Tangent)

**Implementation:**
```erlang
tanh(Val) -> math:tanh(Val).
```

**High-Level Operation:**
- Maps input to range [-1, 1]
- S-shaped curve (sigmoid-like)
- Smooth, differentiable everywhere
- Output saturates for large |input|

**Benefits:**
- ✅ **Bounded output**: Prevents signal explosion in deep networks
- ✅ **Smooth gradients**: Excellent for backpropagation (if used)
- ✅ **Zero-centered**: Helps with weight initialization
- ✅ **Universal approximation**: Can approximate any continuous function
- ✅ **Proven in trading**: Common in financial neural networks

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Medium (requires exponential computation: `(e^x - e^-x) / (e^x + e^-x)`)
- **Erlang Implementation**: Uses `math:tanh/1` (native C implementation)
- **Performance**: ~50-100ns per call (typical)

**Scaling Characteristics:**
- **Memory**: O(1) - no state
- **Parallelization**: Perfect (no dependencies)
- **Vectorization**: Excellent (element-wise operation)
- **Network Depth**: Handles deep networks well (bounded output)
- **Network Width**: Scales linearly with neuron count

**Usage in System:**
- Default activation function (fallback when no functions specified)
- Used in substrate neurodes (hardcoded in `substrate.erl`)
- Used in plasticity modulation (hardcoded in `neuron.erl` line 84)
- Most common function in evolved networks

---

### 2. **sin** (Sine)

**Implementation:**
```erlang
sin(Val) -> math:sin(Val).
```

**High-Level Operation:**
- Periodic function: output ∈ [-1, 1]
- Oscillates with period 2π
- Zero at multiples of π

**Benefits:**
- ✅ **Periodic patterns**: Excellent for detecting cyclical patterns in time series
- ✅ **Oscillatory behavior**: Can model market cycles, seasonal patterns
- ✅ **Frequency analysis**: Natural for Fourier-like decompositions
- ✅ **Diversity**: Provides different behavior than monotonic functions

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Low-Medium (trigonometric computation)
- **Erlang Implementation**: Uses `math:sin/1` (native C, optimized)
- **Performance**: ~30-60ns per call (typically faster than tanh)

**Scaling Characteristics:**
- **Memory**: O(1) - no state
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Can cause instability (unbounded input → bounded output, but periodic)
- **Network Width**: Scales linearly

**Trading Application:**
- Useful for detecting periodic patterns in price movements
- Can model intraday cycles, weekly patterns
- May help with oscillating indicators (RSI-like behavior)

---

### 3. **cos** (Cosine)

**Implementation:**
```erlang
cos(Val) -> math:cos(Val).
```

**High-Level Operation:**
- Periodic function: output ∈ [-1, 1]
- Phase-shifted sine: cos(x) = sin(x + π/2)
- Maximum at x = 0

**Benefits:**
- ✅ **Phase diversity**: Complements sin with 90° phase shift
- ✅ **Symmetry**: Even function (cos(-x) = cos(x))
- ✅ **Pattern detection**: Can detect different phase relationships than sin
- ✅ **Fourier components**: Natural for frequency domain analysis

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Low-Medium (trigonometric computation)
- **Performance**: ~30-60ns per call (similar to sin)

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Similar to sin (periodic behavior)
- **Network Width**: Scales linearly

**Trading Application:**
- Complements sin for detecting phase-shifted patterns
- Useful for cross-correlation analysis
- Can model lag relationships in price data

---

### 4. **gaussian** (Gaussian / Radial Basis Function)

**Implementation:**
```erlang
gaussian(Val) -> gaussian(2.71828183, Val).

gaussian(Const, Val) ->
    V = case Val > 10 of
        true -> 10;
        false -> case Val < -10 of
            true -> -10;
            false -> Val
        end
    end,
    math:pow(Const, -V*V).
```

**High-Level Operation:**
- Radial basis function: `e^(-x²)`
- Bell-shaped curve centered at 0
- Output ∈ [0, 1] (maximum at x=0)
- Input clamped to [-10, 10] to prevent overflow

**Benefits:**
- ✅ **Localized response**: Strong output only near x=0
- ✅ **Smooth decay**: Gradual falloff (not hard threshold)
- ✅ **Pattern matching**: Excellent for detecting specific input ranges
- ✅ **RBF networks**: Natural for radial basis function networks
- ✅ **Noise tolerance**: Smooth response reduces sensitivity to noise

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Medium-High (exponentiation: `e^(-x²)`)
- **Input clamping**: Additional overhead (3 comparisons + potential assignment)
- **Performance**: ~80-150ns per call (slower due to pow operation)

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Excellent (bounded output)
- **Network Width**: Scales linearly
- **Input Range**: Requires careful weight initialization (works best for inputs near 0)

**Trading Application:**
- Excellent for detecting specific price levels (support/resistance)
- Can model "relevance zones" in price space
- Useful for pattern matching (e.g., "price near this level")
- May help with mean reversion strategies

---

### 5. **absolute** (Absolute Value)

**Implementation:**
```erlang
absolute(Val) -> abs(Val).
```

**High-Level Operation:**
- Returns |x| (magnitude, always non-negative)
- V-shaped function (linear on each side of 0)
- Non-differentiable at x=0

**Benefits:**
- ✅ **Magnitude encoding**: Captures signal strength regardless of sign
- ✅ **Symmetry**: Even function
- ✅ **Simple**: Minimal computation
- ✅ **Feature extraction**: Useful for extracting magnitude features

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Very Low (single conditional or bit operation)
- **Erlang Implementation**: Uses `abs/1` (highly optimized)
- **Performance**: ~5-10ns per call (fastest function)

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Can cause issues (unbounded output for large inputs)
- **Network Width**: Scales linearly
- **Gradient Issues**: Non-differentiable at 0 (problematic for gradient-based learning, but not relevant for evolutionary approach)

**Trading Application:**
- Useful for magnitude-based features (volatility, price change magnitude)
- Can extract absolute price movements
- May help with risk assessment (position sizing based on signal strength)

---

### 6. **sgn** (Sign Function)

**Implementation:**
```erlang
sgn(0) -> 0;
sgn(Val) -> case Val > 0 of
    true -> 1;
    false -> -1
end.
```

**High-Level Operation:**
- Returns sign of input: -1, 0, or +1
- Hard threshold function
- Discretizes continuous input

**Benefits:**
- ✅ **Discretization**: Converts continuous signals to discrete decisions
- ✅ **Decision making**: Natural for binary/ternary decisions
- ✅ **Noise reduction**: Eliminates small fluctuations
- ✅ **Trading signals**: Direct mapping to BUY/SELL/HOLD

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Very Low (2 comparisons maximum)
- **Performance**: ~5-10ns per call (very fast)

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Can cause information loss (compression to 3 values)
- **Network Width**: Scales linearly
- **Information Loss**: Significant (continuous → discrete)

**Trading Application:**
- Direct decision making (BUY=1, SELL=-1, HOLD=0)
- Final layer activation for discrete actions
- May cause premature discretization if used in hidden layers

---

### 7. **log** (Logarithm)

**Implementation:**
```erlang
log(Val) -> case Val == 0 of
    true -> 0;
    false -> sgn(Val) * math:log(abs(Val))
end.
```

**High-Level Operation:**
- Natural logarithm with sign preservation
- Handles negative inputs: log(-x) = -log(|x|)
- Output unbounded (grows slowly)

**Benefits:**
- ✅ **Compression**: Compresses large values, expands small values
- ✅ **Multiplicative relationships**: Transforms products to sums
- ✅ **Skew handling**: Useful for skewed distributions
- ✅ **Range transformation**: Maps [0,∞) to (-∞,∞)

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Medium (logarithm computation + sign handling)
- **Special case**: Zero check adds overhead
- **Performance**: ~40-80ns per call

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Can cause issues (unbounded output, slow growth)
- **Network Width**: Scales linearly
- **Input Range**: Requires positive inputs for meaningful output (handled by abs)

**Trading Application:**
- Useful for log-returns (common in finance)
- Can handle price ratios, percentage changes
- May help with multiplicative relationships in market data

---

### 8. **sqrt** (Square Root)

**Implementation:**
```erlang
sqrt(Val) -> sgn(Val) * math:sqrt(abs(Val)).
```

**High-Level Operation:**
- Square root with sign preservation
- Handles negative inputs: sqrt(-x) = -sqrt(|x|)
- Output grows slower than input (compression)

**Benefits:**
- ✅ **Compression**: Reduces large values more than small ones
- ✅ **Smooth**: Differentiable (except at 0)
- ✅ **Sub-linear growth**: Output grows slower than input
- ✅ **Variance normalization**: Common in feature scaling

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Medium (square root computation + sign handling)
- **Performance**: ~30-60ns per call

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Moderate (bounded growth for positive inputs)
- **Network Width**: Scales linearly

**Trading Application:**
- Useful for variance normalization
- Can compress volatility measures
- May help with feature scaling in preprocessing

---

### 9. **linear** (Identity Function)

**Implementation:**
```erlang
linear(Val) -> Val.
```

**High-Level Operation:**
- Identity function: f(x) = x
- No transformation applied
- Passes input through unchanged

**Benefits:**
- ✅ **No information loss**: Preserves all input information
- ✅ **Minimal computation**: Fastest possible function
- ✅ **Linear combinations**: Allows linear relationships
- ✅ **Baseline**: Useful as baseline/reference

**Computational Effort:**
- **Complexity**: O(1) per evaluation
- **Cost**: Minimal (no computation, just pass-through)
- **Performance**: ~1-2ns per call (essentially free)

**Scaling Characteristics:**
- **Memory**: O(1)
- **Parallelization**: Perfect
- **Vectorization**: Excellent
- **Network Depth**: Can cause issues (unbounded output)
- **Network Width**: Scales linearly
- **Non-linearity**: Provides no non-linearity (may limit expressiveness)

**Trading Application:**
- Useful in final layers for unbounded outputs
- Can be combined with other functions for hybrid behavior
- May limit network expressiveness if overused

---

## Disabled Functions Analysis

### **quadratic**
```erlang
quadratic(Val) -> sgn(Val) * Val * Val.
```
- **Issue**: Reported crashes (not tracked in AF_Distribution)
- **Likely Problem**: Unbounded growth, potential overflow
- **Would Provide**: Polynomial non-linearity, symmetry

### **sigmoid** & **sigmoid1**
```erlang
sigmoid(Val) -> 2/(1+math:pow(2.71828183,-V)) - 1.  % Clamped to [-10,10]
sigmoid1(Val) -> Val/(1+abs(Val)).
```
- **Issue**: Reported crashes
- **Likely Problem**: Numerical instability, division by zero edge cases
- **Would Provide**: S-shaped bounded output (similar to tanh)

### **multiquadric**
```erlang
multiquadric(Val) -> math:pow(Val*Val + 0.01, 0.5).
```
- **Issue**: Reported crashes
- **Likely Problem**: Potential overflow, numerical issues
- **Would Provide**: Alternative radial basis function

### **bin** & **trinary**
```erlang
bin(Val) -> case Val > 0 of true -> 1; false -> 0 end.
trinary(Val) -> if (Val < 0.33) and (Val > -0.33) -> 0;
                   Val >= 0.33 -> 1;
                   Val =< -0.33 -> -1 end.
```
- **Issue**: Reported crashes
- **Likely Problem**: Hard thresholds may cause gradient issues (though evolution doesn't use gradients)
- **Would Provide**: Hard discretization (similar to sgn but different thresholds)

---

## Computational Effort Summary

| Function | Complexity | Relative Cost | Typical Time | Notes |
|----------|-----------|---------------|--------------|-------|
| **linear** | O(1) | Minimal | ~1-2ns | Identity, no computation |
| **absolute** | O(1) | Very Low | ~5-10ns | Single abs operation |
| **sgn** | O(1) | Very Low | ~5-10ns | 2 comparisons max |
| **sqrt** | O(1) | Low-Medium | ~30-60ns | Square root + sign |
| **sin** | O(1) | Low-Medium | ~30-60ns | Trigonometric |
| **cos** | O(1) | Low-Medium | ~30-60ns | Trigonometric |
| **log** | O(1) | Medium | ~40-80ns | Logarithm + sign |
| **tanh** | O(1) | Medium | ~50-100ns | Hyperbolic tangent |
| **gaussian** | O(1) | Medium-High | ~80-150ns | Exponentiation + clamping |

**Key Observations:**
- All functions are O(1) - constant time per evaluation
- Cost differences are relatively small (microseconds)
- For a network with 1000 neurons, total activation cost: ~50-150μs (negligible compared to weight calculations)
- **Bottleneck**: Weight calculations and message passing, NOT activation functions

---

## Scaling Analysis

### **Per-Neuron Scaling**
- All functions scale **linearly** with number of neurons
- No cross-neuron dependencies
- Perfect parallelization possible

### **Network Depth Scaling**
- **Bounded functions** (tanh, sin, cos, gaussian, sgn): Handle deep networks well
- **Unbounded functions** (linear, absolute, log, sqrt): Can cause signal explosion in deep networks
- **Recommendation**: Use bounded functions in hidden layers, unbounded in output layers

### **Network Width Scaling**
- All functions scale **linearly** with network width
- No quadratic or exponential dependencies
- Memory usage: O(1) per neuron (no state)

### **HyperNEAT Substrate Scaling**
- Substrate uses **hardcoded tanh** (line 590 in `substrate.erl`)
- For a 10×20 substrate (200 neurodes per layer):
  - Activation cost per forward pass: ~10-20μs (negligible)
  - Weight calculation dominates (thousands of NEAT network queries)

### **Evolutionary Scaling**
- Activation function selection evolves over generations
- System tracks distribution: `{TotTanh, TotSin, TotCos, TotGaussian, TotAbsolute, TotSgn, TotLog, TotSqrt, TotLin}`
- Mutation operator `mutate_af` (weight: 5) changes activation functions
- **Observation**: Evolution can discover optimal function combinations

---

## Benefits Summary by Function Category

### **Bounded Functions** (tanh, sin, cos, gaussian, sgn)
- ✅ Prevent signal explosion
- ✅ Stable in deep networks
- ✅ Well-suited for hidden layers
- ⚠️ May limit expressiveness if overused

### **Unbounded Functions** (linear, absolute, log, sqrt)
- ✅ Preserve information
- ✅ Useful for output layers
- ✅ Can model unbounded relationships
- ⚠️ Risk of signal explosion in deep networks

### **Periodic Functions** (sin, cos)
- ✅ Detect cyclical patterns
- ✅ Model temporal relationships
- ✅ Frequency domain analysis
- ⚠️ Can cause instability if phase relationships are not learned

### **Localized Functions** (gaussian)
- ✅ Pattern matching
- ✅ Noise tolerance
- ✅ Relevance zones
- ⚠️ Requires careful weight initialization

### **Discretization Functions** (sgn)
- ✅ Direct decision making
- ✅ Noise reduction
- ✅ Trading signal generation
- ⚠️ Information loss (continuous → discrete)

---

## Recommendations

### **For Trading Applications**

1. **Hidden Layers**: Prefer bounded functions (tanh, gaussian, sin/cos)
   - Prevents signal explosion
   - Stable gradients (if used)
   - Good for pattern detection

2. **Output Layers**: Consider linear or bounded functions
   - Linear: Unbounded trading signals
   - tanh: Bounded signals (-1 to 1)
   - sgn: Discrete decisions (BUY/SELL/HOLD)

3. **Pattern Detection**: Use periodic functions (sin, cos)
   - Detect cyclical patterns
   - Model temporal relationships
   - Complement bounded functions

4. **Feature Extraction**: Use gaussian for localized patterns
   - Support/resistance levels
   - Pattern matching
   - Relevance zones

### **For System Optimization**

1. **Enable Disabled Functions** (with fixes):
   - Fix numerical stability issues in sigmoid/sigmoid1
   - Add input clamping to quadratic
   - Test multiquadric with proper bounds
   - Consider enabling bin/trinary for final decision layers

2. **Substrate Activation Diversity**:
   - Currently hardcoded to tanh
   - Consider allowing evolution to select substrate activation
   - Or use different functions per layer

3. **Performance Monitoring**:
   - Activation function costs are negligible
   - Focus optimization on weight calculations and message passing
   - Profile actual bottlenecks (likely in substrate weight generation)

### **For Evolution**

1. **Function Diversity**:
   - Current set (9 functions) provides good diversity
   - Monitor AF_Distribution to see which functions evolve
   - Consider adjusting mutation rates if certain functions dominate

2. **Function Combinations**:
   - Evolution can discover optimal combinations
   - Different functions may work better in different network regions
   - Trust the evolutionary process to find good combinations

---

## Conclusion

The DXNN-Trader-v2 system implements a **diverse set of 9 activation functions** that provide:

- ✅ **Computational Efficiency**: All O(1), costs range from 1ns to 150ns (negligible)
- ✅ **Functional Diversity**: Bounded, unbounded, periodic, localized, discrete
- ✅ **Scalability**: Linear scaling with network size, perfect parallelization
- ✅ **Evolutionary Flexibility**: Functions can evolve and combine optimally

**Key Findings:**
1. Activation function computation is **not a bottleneck** (weight calculations dominate)
2. **Bounded functions** (tanh, gaussian) are safest for deep networks
3. **Periodic functions** (sin, cos) provide unique pattern detection capabilities
4. **Evolution** can discover optimal function combinations automatically
5. **Substrate** currently limited to tanh (opportunity for enhancement)

The system is well-designed for evolutionary neural networks, with activation functions that scale efficiently and provide diverse computational behaviors for trading applications.

---

**Document Version:** 1.0  
**Last Updated:** 2025  
**System:** DXNN-Trader-v2 HyperNEAT Trading Platform












