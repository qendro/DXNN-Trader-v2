-----
- reformat qlog:delete_all(). so it deletes the folder instead of each item one by one
- Agent connectivity fix
- Agent RAM and CPU utlization and message queue
- Distributed system across multple PC's or AWS instances
- Improve fitness function
- Improve CEP and weights 
-----





GPU: 
- Fixed Agent: Build 1 agent that has all the sensors, set substrate and actuator
    - Get weights for all connections from Neat
    - Run through 1 run 
    - Do Neat mutation
    - No internal sensor (no sensor that gets data from the next time-step)
    > Allows you to do very large sensors, substrate (maybe very fast)


Sensors: 
- Interval Sensor: Gets historical data on incment basis (i.e 15 min interval over 1 year)
    - Option 1 (Ideal): gets actual 15 min interval over 1 year
        - Alot of memory need to cache full 1 year at 1 min ~400K timesteps.
    - Option 2: static 15 min internval over 1 year 
        - doesn't change for 15 mins
        - ~27k timesteps (1/15)
        - Update sensors to use different datasets 
- Sensors able to use different data-sets
    - Need to align on what row to use
    -- Likely need to do a date check or manually provide its starting point
- Fix different dimension sensors. 


Benchmarker
- Each run is a build from the last
    - Each run is a new set of data 
    - saves agents from last run and runs them on new data  
    - need to address forgetting 

Fitness function
- Modify fitness function

FX Tables [Done]
-Need flexibility to configure such as adding RSI and Dates etc. 
- Does column location matter?



Substrate: 
- mutate_resolution / increase_density / decrease_density
    - No operators modify the densities field of the substrate record
    - The densities field is set during creation (line 84-85 in genotype.erl) but never mutated
- mutate_dimensionality / increase_dimensionality / decrease_dimensionality
    - No operators change substrate dimensionality
    - Dimensionality is calculated during creation (calculate_OptimalSubstrateDimension) but not mutated afterward
- delete CPP and/or CEP
    - Ability to add multiple CPP or CEP but no ability to delete 


Config Fixes
- Recurrent, jordan recurrent
- abcn 

**There is an issue with 1 remaining agent indicating that something is crashing silently somewhere. 