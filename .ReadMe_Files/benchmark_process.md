Caller process (you)
    |
    | (calls) exoself:start(Best, self(), benchmark)
    v
exoself:start/3
    |
    | -- spawns --> ExoSelf process (ExoSelfPid)
    v
ExoSelf process
    |
    | -- reads --> genotype: agent, cortex, sensors, actuators, neurons
    | -- calls --> spawn_Scapes(IdsNPIds, SIds, AIds, Agent_Id, benchmark)
    v
spawn_Scapes/5
    |
    | -- spawns --> Scape process (for each {private, fx_sim})
    | -- sends --> {ExoSelfPid, fx_sim}
    v
Scape process
    |
    | -- calls --> scape:fx_sim(ExoSelfPid) -> fx:sim(ExoSelfPid)
    v
fx:sim loop (inside Scape)
    |
    | (ready to receive sense/trade messages)
    v
ExoSelf process
    |
    | -- spawns --> Cortex process (CxPid)
    | -- spawns --> Sensor processes (SPidᵢ)
    | -- spawns --> Actuator processes (APidⱼ)
    | -- spawns --> Neuron processes (NPidₖ)
    v
ExoSelf process
    |
    | -- links sensors --> SPidᵢ ! {ExoSelfPid, {SId, CxPid, ScapePid, SName, VL, Params, FanoutPids, benchmark}}
    | -- links actuators -> APidⱼ ! {ExoSelfPid, {AId, CxPid, ScapePid, AName, Params, FaninPids, benchmark}}
    | -- links cortex ----> CxPid ! {ExoSelfPid, CxId, SPids, NPids, APids}
    v
Cortex process
    |
    | -- sends --> {self(), sync} to each SPid (start cycle 1)
    v
Sensor process (general)
    |
    | (on {CxPid, sync} calls sensor:SName/4)
    v

--- Example: Sensor fx_PCI (benchmark) ---
Sensor process (fx_PCI)
    |
    | (calls) sensor:fx_PCI(ExoSelf_Id, VL, [HRes,VRes], ScapePid)
    | -- sends --> {SensorPid, sense,
    |               config:primary_currency_pair(), close,
    |               [HRes,VRes,graph_sensor],
    |               config:data_end_index(),
    |               config:benchmark_end_index()}
    v
Scape process (fx:sim)
    |
    | -- replies --> {ScapePid, ResultPlane}
    v
Sensor process (fx_PCI)
    |
    | (logs, returns ResultPlane)
    | -- forwards --> {SensorPid, forward, ResultPlane} to FanoutPids (neurons)
    v
Neuron processes
    |
    | (accumulate inputs, compute outputs, forward onward)
    v

--- Example: Sensor fx_PLI (benchmark) ---
Sensor process (fx_PLI)
    |
    | (calls) sensor:fx_PLI(ExoSelf_Id, VL, [HRes,list_sensor], ScapePid)
    | -- sends --> {SensorPid, sense,
    |               config:primary_currency_pair(), close,
    |               [HRes,list_sensor],
    |               config:data_end_index(),
    |               config:benchmark_end_index()}
    v
Scape process (fx:sim)
    |
    | -- replies --> {ScapePid, ResultList}
    v
Sensor process (fx_PLI)
    |
    | (normalizes ResultList -> SensoryVector)
    | -- forwards --> {SensorPid, forward, SensoryVector} to FanoutPids (neurons)
    v
Neuron processes
    |
    | (accumulate inputs, compute outputs, forward onward)
    v

--- Example: Sensor fx_Internals ---
Sensor process (fx_Internals)
    |
    | (calls) sensor:fx_Internals(ExoSelf_Id, VL, Params, ScapePid)
    | -- sends --> {SensorPid, sense, internals, Params}
    v
Scape process (fx:sim)
    |
    | -- replies --> {ScapePid, [Position, EntryPrice, PrevPC]}
    v
Sensor process (fx_Internals)
    |
    | (returns internals vector to loop)
    v

--- Example: Actuator fx_Trade ---
Actuator process (fx_Trade)
    |
    | (after receiving fanin inputs)
    | (calls) actuator:fx_Trade(ExoSelf_Id, Output, Params, ScapePid)
    | -- sends --> {ActPid, trade,
    |               config:primary_currency_pair(),
    |               functions:trinary(TradeSignal)}
    v
Scape process (fx:sim)
    |
    | -- replies --> {ScapePid, Fitness, HaltFlag}
    v
Actuator process
    |
    | -- sends --> {self(), sync, Fitness, HaltFlag} to CxPid
    v
Cortex process
    |
    | (collects sync from all actuators)
    | if any HaltFlag > 0:
    |   -- sends --> {self(), evaluation_completed,
    |                 FitnessAcc, CycleAcc, TimeDiff, GoalReached} to ExoSelfPid
    | else:
    |   -- sends --> {self(), sync} to each SPid (next cycle)
    v

--- Benchmark Completion ---
ExoSelf process
    |
    | (on evaluation_completed)
    | -- terminate_phenotype() → sends {terminate} to all SPids, APids, NPids, ScapePids, CxPid
    | -- sends --> {self(), benchmark_complete,
    |               SpecieId, Fitness, Cycles, Time} to PM_Pid (caller)
    v
Caller process (PM_Pid = you)
    |
    | (receives) {ExoSelfPid, benchmark_complete, SpecieId, Fitness, Cycles, Time}
    v
Done
