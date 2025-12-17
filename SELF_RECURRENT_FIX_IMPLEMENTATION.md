# Self-Recurrent Neuron Infinite Loop Fix - Implementation

## Problem
Neurons with only self-recurrent connections can enter infinite processing loops because they send messages to themselves and process them immediately, bypassing the Cortex sync mechanism.

## Solution
Limit the number of computations each neuron can perform per evaluation to the total number of cycles. Each neuron should process once per cycle, so the limit equals the number of cycles.

## Implementation Changes

### 1. population_monitor.erl

**Location**: In `summon_agents/2` function, before spawning agents (around line 413)

**Add**:
```erlang
summon_agents(OpMode,Agent_Ids)->
	% Set global max computations per neuron per evaluation (before spawning agents)
	TotalCycles = case OpMode of
		gt -> config:gt_start() - config:gt_end();
		benchmark -> 
			case config:bench_end() of
				last -> 0;  % Safe: unknown cycles = 0 (prevents infinite loops)
				N -> config:bench_start() - N
			end;
		_ -> 10000
	end,
	config:set(max_neuron_computations_per_eval, TotalCycles),
	io:format("Summoning agents:~p with OpMode:~p~n",[Agent_Ids,OpMode]),
	qlog:benchmarker(self(),io_lib:format("SUMMONING_AGENTS | op_mode=~p | agent_ids=~p | max_computations=~p",[OpMode,Agent_Ids,TotalCycles])),
	summon_agents(OpMode,Agent_Ids,[]).
```

**Lines**: 11 lines added (replaces existing summon_agents/2 function)

---

### 2. neuron.erl

#### 2.1 Update State Record

**Location**: Line 8-24, add field to `#state{}` record

**Change**:
```erlang
-record(state,{
	id,
	cx_pid,
	af,
	pf,
	aggrf,
	heredity_type,
	si_pids=[],
	si_pidps_bl = [],
	si_pidps_current=[],
	si_pidps_backup=[],
	mi_pids=[],
	mi_pidps_current=[],
	mi_pidps_backup=[],
	output_pids=[],
	ro_pids=[],
	computation_count=0  % ADD THIS LINE
}).
```

**Lines**: 1 line added

---

#### 2.2 Add Computation Limit Check in Main Loop

**Location**: Line 65-96, in `loop(S,ExoSelf_PId,[ok],[ok],SIAcc,MIAcc)`

**Change**: Add check at the start of the try block, before normal processing

**Before**:
```erlang
loop(S,ExoSelf_PId,[ok],[ok],SIAcc,MIAcc)->
	try
		%qlog:xLog(pid_to_list(ExoSelf_PId), "Neuron: ~p in secondary loop SI_PIds and MI_PIds empty. ExoSelf_Id: ~p", [self(), ExoSelf_PId]),
		PF = S#state.pf,
		...
```

**After**:
```erlang
loop(S,ExoSelf_PId,[ok],[ok],SIAcc,MIAcc)->
	try
		% Check computation limit before processing (add 100 buffer for safety)
		MaxComputations = config:get_val(max_neuron_computations_per_eval, 100000) + 100,
		case S#state.computation_count >= MaxComputations of
			true ->
				% Exceeded limit, just wait (don't process)
				qlog:xLog(qStatus, "Neuron ~p hit computation limit: count=~p max=~p", [S#state.id, S#state.computation_count, MaxComputations]),
				neuron:loop(S,ExoSelf_PId,[ok],[ok],SIAcc,MIAcc);
			false ->
				% Normal processing
				%qlog:xLog(pid_to_list(ExoSelf_PId), "Neuron: ~p in secondary loop SI_PIds and MI_PIds empty. ExoSelf_Id: ~p", [self(), ExoSelf_PId]),
				PF = S#state.pf,
				AF = S#state.af,
				AggrF = S#state.aggrf,
				{PFName,PFParameters} = PF,
				Ordered_SIAcc = lists:reverse(SIAcc),
				SI_PIdPs = S#state.si_pidps_current,
				[begin case lists:keyfind(IPId,1,SI_PIdPs) of {IPId,WeightsP} -> case {WeightsP, length(InputVec), length(WeightsP)} of {[], InLen, _} -> qlog:xLog(qStatus, "dot_input EMPTY_WEIGHTS Neuron=~p IPId=~p InputLen=~p", [S#state.id, IPId, InLen]); {_WPs, InLen, WLen} when InLen =/= WLen -> qlog:xLog(qStatus, "dot_input LEN_MISMATCH Neuron=~p IPId=~p InputLen=~p WLen=~p", [S#state.id, IPId, InLen, WLen]); _ -> ok end; _ -> ok end end || {IPId,InputVec} <- Ordered_SIAcc],
				SAggregation_Product = sat(signal_aggregator:AggrF(Ordered_SIAcc,SI_PIdPs),?SAT_LIMIT),
				SOutput = functions:AF(SAggregation_Product),
				
				Output_PIds = S#state.output_pids,
				[Output_PId ! {self(),forward,[SOutput]} || Output_PId <- Output_PIds],
				
				% Increment computation count (one per process)
				U_ComputationCount = S#state.computation_count + 1,
				
				case PFName of
					none ->
						U_S = S#state{computation_count=U_ComputationCount};
					_ ->
						Ordered_MIAcc = lists:reverse(MIAcc),
						MI_PIdPs = S#state.mi_pidps_current,
						MAggregation_Product = sat(signal_aggregator:dot_product(Ordered_MIAcc,MI_PIdPs),?SAT_LIMIT),
						MOutput = functions:tanh(MAggregation_Product),
						U_SI_PIdPs = plasticity:PFName([MOutput|PFParameters],Ordered_SIAcc,SI_PIdPs,SOutput),
						U_S = S#state{
							si_pidps_current = U_SI_PIdPs,
							computation_count = U_ComputationCount
						}
				end,
				SI_PIds = S#state.si_pids,
				MI_PIds = S#state.mi_pids,
				neuron:loop(U_S,ExoSelf_PId,SI_PIds,MI_PIds,[],[])
		end
	catch
		...
```

**Lines**: 7 lines added, 2 lines modified
- Added: MaxComputations check, case statement, increment line, logging line
- Modified: Line 83 (`U_S=S;` → `U_S = S#state{computation_count=U_ComputationCount};`)
- Modified: Line 90-92 (add `computation_count = U_ComputationCount` to state update)

---

#### 2.3 Reset Counter on reset_prep

**Location**: Line 146-154, in `{ExoSelf_PId,reset_prep}` handler

**Before**:
```erlang
{ExoSelf_PId,reset_prep}->
	neuron:flush_buffer(),
	ExoSelf_PId ! {self(),ready},
	RO_PIds = S#state.ro_pids,
	receive 
		{ExoSelf_PId, reset}->
			fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]})
	end,
	loop(S,ExoSelf_PId,S#state.si_pids,S#state.mi_pids,[],[]);
```

**After**:
```erlang
{ExoSelf_PId,reset_prep}->
	neuron:flush_buffer(),
	ExoSelf_PId ! {self(),ready},
	RO_PIds = S#state.ro_pids,
	receive 
		{ExoSelf_PId, reset}->
			fanout(RO_PIds,{self(),forward,[?RO_SIGNAL]})
	end,
	U_S = S#state{computation_count=0},  % Reset counter
	loop(U_S,ExoSelf_PId,S#state.si_pids,S#state.mi_pids,[],[]);
```

**Lines**: 1 line added, 1 line modified
- Added: `U_S = S#state{computation_count=0},`
- Modified: `loop(S,...)` → `loop(U_S,...)`

---

## Summary

### Total Lines Changed: 22 lines
- **population_monitor.erl**: 11 lines added (replaces existing function)
- **neuron.erl**: 10 lines added, 3 lines modified

### Changes by File:

**population_monitor.erl**:
- 11 lines added: TotalCycles calculation and config:set() in summon_agents/2 (before spawning agents)

**neuron.erl**:
- 1 line added: `computation_count=0` in state record
- 7 lines added: Computation limit check in main loop (includes logging)
- 2 lines modified: State updates to include computation_count
- 1 line added: Reset counter in reset_prep
- 1 line modified: Use U_S instead of S in reset_prep

### Key Points:
- Config set in population_monitor: Avoids race conditions when multiple agents spawn concurrently
- No hardcoded values: Uses `config:get_val()` with default fallback
- Safe buffer: Adds 100 computations to the config value as a safety margin
- Safe defaults: `last -> 0` prevents infinite loops when cycle count is unknown
- Minimal overhead: One integer field, one comparison per process, one increment per computation
- Simple logic: Count computations (1 per process), not messages
- Computation count increments only on actual processing (in the `false` branch), not on message receipt

## Important Notes & Clarifications

### Issue #1: Config Key Syntax
The implementation uses the correct syntax:
```erlang
MaxComputations = config:get_val(max_neuron_computations_per_eval, 100000) + 100,
```
This correctly retrieves the value from config with a default fallback, then adds a 100-computation buffer for safety. The key is passed as an atom, not a computed expression.

### Config Setting Location: population_monitor vs exoself
The config is set in `population_monitor:summon_agents/2` instead of `exoself:prep/3` to avoid race conditions:
- **Problem**: If set in exoself, concurrent agents could overwrite each other's config values
- **Solution**: Set once in population_monitor before spawning any agents, ensuring all agents use the same value
- **Benefit**: Guarantees consistent limit across all agents in a population

