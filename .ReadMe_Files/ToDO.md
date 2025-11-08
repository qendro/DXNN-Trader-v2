Substrate: 
- mutate_resolution / increase_density / decrease_density
    - No operators modify the densities field of the substrate record
    - The densities field is set during creation (line 84-85 in genotype.erl) but never mutated
- mutate_dimensionality / increase_dimensionality / decrease_dimensionality
    - No operators change substrate dimensionality
    - Dimensionality is calculated during creation (calculate_OptimalSubstrateDimension) but not mutated afterward
- delete CPP and/or CEP
    - Ability to add multiple CPP or CEP but no ability to delete 


    