# NEAT Weight Tracking: Log File Counts and Sample Logs

## Number of Log Files Created

### Log File Structure
**One log file per agent**: `logs/Agents/{Agent_Id}.log`

All weight tracking entries for a specific agent are written to that agent's individual log file.

### Total Log Files Calculation

Based on your configuration (`config.erl`):
- **Initial Population**: `init_specie_size() = 100` agents
- **Species Size Limit**: `specie_size_limit() = 1000` agents (maximum)
- **Survival Rate**: `survival_percentage() = 0.5` (50% survive, 50% are clones)
- **Generation Limit**: `generation_limit() = 100` generations
- **Evaluation Limit**: `evaluations_limit() = 100000` evaluations

#### Scenario 1: Early Termination (Typical)
- **Generation 0**: 100 agents → **100 log files**
- **Generation 1**: ~50 survivors + ~50 clones = ~100 agents → **100 log files** (total: 200)
- **Generation 2**: ~50 survivors + ~50 clones = ~100 agents → **100 log files** (total: 300)
- **Early termination** (evaluations_limit reached) at ~3-5 generations
- **Total**: ~300-500 log files

#### Scenario 2: Full Evolution (Maximum)
- If evolution runs for full 100 generations
- **Maximum**: Up to `specie_size_limit = 1000` agents per species
- **Total**: Up to **1,000 log files** (one per agent)

#### Scenario 3: Multiple Species
- If multiple species exist, multiply by number of species
- Typically 1-5 species per population
- **Total**: 300-5,000 log files (depending on species count)

### Log File Size Estimates

**Per Agent Log File Size** (with recommended weight tracking):

| Event Type | Frequency per Agent | Entries per Event | Estimated Size |
|------------|---------------------|-------------------|----------------|
| Initial Weight Creation | 1 (at creation) | 1 | ~200 bytes |
| Weight Mutations | 1-5 per generation | 2 (before/after) | ~400 bytes each |
| Weight Inheritance | 1 per clone | 1 | ~300 bytes |
| Plasticity Updates | 10-50 per evaluation | 1 (summary) | ~150 bytes each |
| Weight Backup/Restore | 10 per evaluation | 1 | ~200 bytes each |
| **Total per Generation** | | | **~5-15 KB** |
| **Total per Agent (100 gens)** | | | **~500 KB - 1.5 MB** |

**With threshold-based logging** (only significant changes):
- **Total per Agent**: ~100-500 KB (much more manageable)

## Sample Log Entries

### Current Log Format (from qlog.erl)
```
Timestamp | [WEIGHT_UPDATE] Gen: X | Details
```

### Sample 1: Initial Weight Creation
**Location**: `genotype.erl:create_NeuralWeightsP/3` (after neuron creation)

```
2025-10-29 14:23:15 | [WEIGHT_UPDATE] Gen: 0 | INITIAL_WEIGHTS | Neuron: {{0,5.676318257887454e-10},neuron} | TotalWeights: 6 | WeightRange: [-0.48,0.42] | Mean: -0.03 | StdDev: 0.31
```

**Alternative (Summary Format)**:
```
2025-10-29 14:23:15 | [WEIGHT_UPDATE] Gen: 0 | INITIAL_WEIGHTS | Neuron: {{0,5.676318257887454e-10},neuron} | Summary: Count=6 Min=-0.48 Max=0.42 Mean=-0.03
```

### Sample 2: Evolutionary Weight Mutation (Before)
**Location**: `genome_mutator.erl:mutate_weights/1` (line 191)

```
2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_MUTATION_BEFORE | Neuron: {{0,5.676318257887454e-10},neuron} | TotalWeights: 6 | Inputs: 1 | MutationProb: 0.408 | Weights: [2.44,1.80,0.54,0.36,1.33,0.31]
```

### Sample 3: Evolutionary Weight Mutation (After)
**Location**: `genome_mutator.erl:mutate_weights/1` (line 196)

```
2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_MUTATION_AFTER | Neuron: {{0,5.676318257887454e-10},neuron} | Changed: 2 | Weights: [2.44,1.95,0.54,0.42,1.33,0.31] | AvgDelta: 0.03 | MaxDelta: 0.15
```

**Alternative (Compact Format)**:
```
2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_MUTATION | Neuron: {{0,5.676318257887454e-10},neuron} | Changed: 2/6 weights | AvgDelta: 0.03 | MaxDelta: 0.15
```

### Sample 4: Weight Perturbation Summary
**Location**: `genome_mutator.erl:perturb_IdPs/3` (after perturbation)

```
2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_PERTURBATION | Neuron: {{0,5.676318257887454e-10},neuron} | Inputs: 1 | TotalWeights: 6 | Mutated: 2 | MutationProb: 0.408 | DeltaRange: [-0.15,0.15]
```

### Sample 5: Weight Inheritance During Cloning
**Location**: `genotype.erl:clone_Agent/2` (after cloning)

```
2025-10-29 14:24:10 | [WEIGHT_UPDATE] Gen: 0 | CLONED_FROM | Parent: {5.6763182579052e-10,agent} | NeuronCount: 1 | WeightSummary: Total=6 Min=-0.48 Max=2.44 Mean=1.12 | Identical: 6/6 weights
```

**Alternative (When weights differ)**:
```
2025-10-29 14:24:10 | [WEIGHT_UPDATE] Gen: 0 | CLONED_FROM | Parent: {5.6763182579052e-10,agent} | NeuronCount: 1 | WeightSummary: Total=6 Min=-0.48 Max=2.44 Mean=1.12 | Similarity: 100%
```

### Sample 6: New Neuron Weight Initialization
**Location**: `genome_mutator.erl:add_neuron/1` (after line 723)

```
2025-10-29 14:24:35 | [WEIGHT_UPDATE] Gen: 2 | NEW_NEURON_ADDED | Neuron: {{1,5.676318842018895e-10},neuron} | Layer: 1 | InitialWeights: Count=3 Min=-0.45 Max=0.38 Mean=-0.05
```

### Sample 7: Runtime Plasticity Weight Update
**Location**: `neuron.erl:loop/6` (after plasticity update, line 89)

```
2025-10-29 14:25:20 | [WEIGHT_UPDATE] Gen: 2 | PLASTICITY_UPDATE | PF: hebbian | Neuron: {{0,5.676318257887454e-10},neuron} | AvgChange: 0.012 | MaxChange: 0.045 | UpdatedWeights: 4/6
```

**Alternative (Threshold-based, only significant changes)**:
```
2025-10-29 14:25:20 | [WEIGHT_UPDATE] Gen: 2 | PLASTICITY_UPDATE | PF: hebbian | Neuron: {{0,5.676318257887454e-10},neuron} | SignificantChange: true | AvgDelta: 0.012 | Threshold: 0.01
```

### Sample 8: Weight Backup (Fitness Improved)
**Location**: `exoself.erl:loop/2` (line 156)

```
2025-10-29 14:26:10 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_BACKUP | Fitness: 350.5 | Attempt: 3 | Neurons: 1 | Reason: Fitness improved from 313.2
```

### Sample 9: Weight Restore (Fitness Degraded)
**Location**: `exoself.erl:loop/2` (line 161)

```
2025-10-29 14:26:15 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_RESTORE | BadFitness: 298.1 | GoodFitness: 350.5 | PerturbedNeurons: 1 | Reason: Fitness degraded, restoring backup
```

### Sample 10: Complete Weight Snapshot (Optional)
**Location**: After significant events (generation boundaries, fitness milestones)

```
2025-10-29 14:27:00 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_SNAPSHOT | Context: GENERATION_BOUNDARY | TotalNeurons: 1 | TotalWeights: 6 | WeightStats: Min=-0.48 Max=2.44 Mean=1.12 StdDev=0.89 | Distribution: [-2,-1]:0 [-1,0]:2 [0,1]:2 [1,2]:1 [2,3]:1
```

## Complete Sample Log File

Here's what a typical agent log file would look like with all weight tracking enabled:

```
2025-10-29 14:23:15 | [GENOTYPE_SNAPSHOT] CREATION | Agent: {5.6763182579052e-10,agent} | Gen: 0 | Fitness: undefined
[Full genotype dump...]

2025-10-29 14:23:15 | [WEIGHT_UPDATE] Gen: 0 | INITIAL_WEIGHTS | Neuron: {{0,5.676318257887454e-10},neuron} | Summary: Count=6 Min=-0.48 Max=0.42 Mean=-0.03

2025-10-29 14:23:45 | [MUTATION] APPLYING | Agent: {5.6763182579052e-10,agent} | Gen: 1 | Details: Operator: mutate_weights

2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_MUTATION_BEFORE | Neuron: {{0,5.676318257887454e-10},neuron} | TotalWeights: 6 | MutationProb: 0.408

2025-10-29 14:23:45 | [WEIGHT_UPDATE] Gen: 1 | WEIGHT_MUTATION_AFTER | Neuron: {{0,5.676318257887454e-10},neuron} | Changed: 2/6 | AvgDelta: 0.03 | MaxDelta: 0.15

2025-10-29 14:24:10 | [WEIGHT_UPDATE] Gen: 1 | CLONED_FROM | Parent: {5.6763182579052e-10,agent} | NeuronCount: 1 | WeightSummary: Total=6 Mean=1.12 | Identical: 6/6

2025-10-29 14:25:20 | [WEIGHT_UPDATE] Gen: 2 | PLASTICITY_UPDATE | PF: hebbian | Neuron: {{0,5.676318257887454e-10},neuron} | AvgChange: 0.012 | UpdatedWeights: 4/6

2025-10-29 14:26:10 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_BACKUP | Fitness: 350.5 | Attempt: 3 | Neurons: 1 | Reason: Fitness improved

2025-10-29 14:26:15 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_RESTORE | BadFitness: 298.1 | GoodFitness: 350.5 | PerturbedNeurons: 1

2025-10-29 14:27:00 | [WEIGHT_UPDATE] Gen: 2 | WEIGHT_SNAPSHOT | Context: GENERATION_BOUNDARY | TotalNeurons: 1 | TotalWeights: 6 | WeightStats: Mean=1.12 StdDev=0.89
```

## Log Volume Management Recommendations

### Option 1: Summary-Based Logging (Recommended)
- Log weight statistics (min, max, mean, count) instead of individual weights
- Log only mutation summaries, not every weight change
- **Result**: ~100-500 KB per agent log file

### Option 2: Threshold-Based Logging
- Only log when weight changes exceed a threshold (e.g., > 0.01)
- Skip logging for insignificant updates
- **Result**: ~50-200 KB per agent log file

### Option 3: Sampling-Based Logging
- Log every Nth weight update (e.g., every 10th)
- Or log probabilistically (e.g., 10% of updates)
- **Result**: ~50-150 KB per agent log file

### Option 4: Event-Based Logging (Most Efficient)
- Log only at key events:
  - Initial creation
  - Major mutations (before/after)
  - Generation boundaries
  - Significant fitness changes
- **Result**: ~20-100 KB per agent log file

## Log File Naming Convention

Current format: `logs/Agents/{Agent_Id}.log`

Example filenames:
- `logs/Agents/5.6763182579052e-10agent.log`
- `logs/Agents/5.676318842018895e-10agent.log`

The Agent_Id is cleaned to remove unsafe characters (brackets, spaces, etc.) for filename safety.

## Summary

- **Total Log Files**: 300-5,000 files (depending on evolution duration and species count)
- **Log File Size**: 100-500 KB per agent (with threshold-based logging)
- **Total Storage**: ~30 MB - 2.5 GB (depending on evolution scale)
- **Log Format**: Single-line entries, easily parseable, timestamped
- **All entries in one file**: Each agent's complete weight evolution history in `logs/Agents/{Agent_Id}.log`


