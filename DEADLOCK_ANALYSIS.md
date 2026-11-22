# DXNN Agent Deadlock Analysis

**Date:** 2025-10-04  
**Issue:** 6 agents frozen during training, system hangs indefinitely

---

## Executive Summary

6 agents from an old population remain stuck after the population_monitor crashed and restarted. A new population of 50 agents was spawned, but the old 6 agents are still alive and consuming resources. Investigation reveals a complex deadlock scenario.

---

## Evidence Collection

### 1. Initial Symptoms

**From crash report (lines 3119-3215 in terminal):**
- 6 agents still linked to population_monitor when it crashed:
  - `<0.8479666.0>` 
  - `<0.8479664.0>`
  - `<0.8479658.0>`
  - `<0.8479654.0>`
  - `<0.8479642.0>` (analyzed in detail)
  - `<0.8479624.0>`

**All 6 agents showing identical state:**
```erlang
current_function: {exoself,loop,2}
status: waiting
message_queue_len: 0
reductions: ~10,950-10,962
current_stacktrace: [{exoself,loop,2,[{file,"exoself.erl"},{line,233}]}]
```

**Line 233 in exoself.erl corresponds to:** `benchmark` mode loop, waiting for `{Cx_PId,evaluation_completed,...}` message from cortex.

---

### 2. Agent Log Analysis

**All 6 agent logs end with identical pattern:**

```erlang
[2025-10-04 16:11:10] cortex(...) sent: {<SPId>, sync}
[2025-10-04 16:11:10] sensor(...) received: {<Cx_PId>, sync}
[2025-10-04 16:11:10] scape_interaction(sensor_fx_PCI) sent_to_scape: {<Scape>, sense, graph_sensor}
[2025-10-04 16:11:10] scape_interaction(sensor_fx_PCI) received_from_scape: {<Scape>, data_received}
[2025-10-04 16:11:10] scape_interaction(sensor_fx_PLI) sent_to_scape: {<Scape>, sense, list_sensor}
[2025-10-04 16:11:10] scape_interaction(sensor_fx_PLI) received_from_scape: {<Scape>, data_size, 20}
[2025-10-04 16:11:10] sensor(...) sent: {<Substrate_PId>, forward}
[2025-10-04 16:11:10] substrate(<Substrate_PId>) received: {<SPId>, forward}
[2025-10-04 16:11:10] substrate(<Substrate_PId>) all_sensors_received: 2
```

**Key observation:** Logs stop at "all_sensors_received: 2"
- **Expected next log:** "sending_to_actuators: N"
- **This log never appears**

---

### 3. Process State Deep Dive

#### Substrate Processes (50 total across new population)

**Sample data from line 3445-3566:**
```erlang
{<0.14703369.0>,
  [{current_function,{substrate,loop,4}},
   {status,waiting},
   {reductions,15593558},        ← 15.5 MILLION reductions!
   {message_queue_len,0}]},
```

**Analysis:**
- ✅ All substrates completed their work (15M+ reductions = expensive computation finished)
- ✅ Status: `waiting` (not stuck in infinite loop)
- ✅ No messages queued
- ✅ Substrates are in `loop/4` waiting for next sensor cycle

**Looking at substrate.erl lines 96-105:**
```erlang
loop(ExoSelf,S,[],SAcc)->  % All sensory signals received
    agent_logger:log_message(ExoSelf, substrate, self(), all_sensors_received, length(SAcc)),
    {U_Substrate,U_SMode,OAcc} = reason(SAcc,S),  ← Line 98: expensive computation
    agent_logger:log_message(ExoSelf, substrate, self(), sending_to_actuators, length(S#state.apids)),
    advanced_fanout(OAcc,S#state.actuators,S#state.apids),
    U_S = S#state{...},
    loop(ExoSelf,U_S,S#state.spids,[]).
```

**Hypothesis 1 (DISPROVEN):** Substrates hanging in `reason/2`
- **Evidence against:** 15M reductions completed, status=waiting (not running)
- **Evidence against:** Substrates are in loop/4, not in reason/2

**Revised understanding:** 
- Substrates COMPLETED `reason/2` (took 15M operations)
- Substrates sent output to actuators via `advanced_fanout`
- Substrates are now waiting for next sensor cycle
- **BUT** they never logged "sending_to_actuators" - this suggests the log line was never reached OR logging failed

---

#### Actuator Processes (50 total)

**Sample data from lines 3584-3682:**
```erlang
{<0.14703365.0>,
  [{current_function,{gen,do_call,4}},    ← CRITICAL CLUE
   {status,waiting},
   {message_queue_len,0}]},

{<0.14703395.0>,
  [{current_function,{actuator,loop,8}},
   {status,waiting},
   {message_queue_len,0}]},
```

**Analysis:**
- ⚠️ **MIXED STATE:** Some actuators in `{gen,do_call,4}`, others in `{actuator,loop,8}`
- **actuator,loop,8** = waiting for next forward message from substrate (normal)
- **gen,do_call,4** = blocked making a gen_server:call (ABNORMAL)

**Looking at actuator.erl lines 50-59:**
```erlang
fx_Trade(ExoSelf_PId,Output,Parameters,Scape)->
    [TradeSignal] = Output,
    agent_logger:log_message(...),
    Scape ! {self(),trade,config:primary_currency_pair(),functions:trinary(TradeSignal)},
    receive 
        {Scape,Fitness,HaltFlag}->
            agent_logger:log_message(...),
            {Fitness,HaltFlag}
    end.
```

**CRITICAL OBSERVATION:**
- Actuator code uses **regular message passing** (`Scape !` and `receive`)
- It does NOT use `gen_server:call` 
- **Yet actuators are stuck in `{gen,do_call,4}`**

**Possible explanations:**
1. Actuators are stuck in a DIFFERENT function that calls gen_server
2. There's scape interaction code that uses gen_server we haven't seen
3. The stack trace is misleading

---

#### Cortex Processes

**Sample data from lines 3697-3795:**
```erlang
{<0.14703361.0>,
  [{current_function,{cortex,loop,9}},
   {status,waiting},
   {message_queue_len,0}]},
```

**Analysis:**
- ✅ All cortexes in normal waiting state
- Cortex loop/9 = waiting for actuator sync messages
- This is the **expected** state when actuators haven't responded yet

---

#### Scape Processes (fx_sim)

**Sample data from lines 3809-3930:**
```erlang
{<0.477.0>,
  [{current_function,{fx,sim,3}},
   {status,waiting},
   {message_queue_len,1},      ← ONE MESSAGE QUEUED
   {reductions,1290098}]},
```

**CRITICAL FINDING:** All scapes have `message_queue_len: 1`

**Checking the message content (line 4043):**
```erlang
process_info(hd(ScapePids), messages).
{messages,[{<0.458.0>,terminate}]}
```

**Analysis:**
- ⚠️ Scape has `{<0.458.0>, terminate}` stuck in its queue
- `<0.458.0>` is from the OLD population (dead agent)
- This terminate message was never processed

**Looking at fx.erl line 368-377 (receive pattern):**
```erlang
sim(ExoSelf,S,A)-> 
    receive
        {From,sense,TableName,Feature,Parameters,Start,Finish}-> ...
        {From,sense,internals,_Parameters}-> ...
        {From,trade,TableName,TradeSignal}-> ...
        restart -> ...
        terminate ->        ← MATCHES ATOM ONLY
            ok
        after 10000 ->
            fx:sim(ExoSelf,S,A)
    end.
```

**BUG IDENTIFIED:**
- Scape receives: `{<0.458.0>, terminate}` (2-tuple)
- Pattern matches: `terminate` (atom)
- **PATTERN DOESN'T MATCH!**
- Message stays in queue, blocking all subsequent messages

---

## Deadlock Chain Analysis

### Timeline Reconstruction

1. **Old population running** (agents 0.8479...)
2. **Population monitor crashes** (line 3040)
3. **Old agents attempt to terminate:**
   - Exoself sends `{self(), terminate}` to scapes
   - Scapes DON'T match this pattern (expecting `terminate` atom)
   - Message stuck in queue
4. **New population spawned** (agents 0.147...)
5. **New agents start:**
   - Sensors query scapes for data ✅ (works)
   - Substrates process ✅ (15M reductions)
   - Substrates send to actuators ⚠️ (unclear if successful)
   - Actuators try to trade with scapes ❌ (blocked)
6. **Scapes frozen:**
   - Old terminate message at front of queue
   - Can't process ANY new messages
   - Actuators waiting indefinitely for response

---

## Current Hypothesis

### Primary Theory: Scape Message Queue Poisoning

**The Bug:**
```erlang
% What exoself SENDS:
Scape ! {self(), terminate}

% What fx:sim/3 EXPECTS:
terminate ->  % Just the atom
    ok
```

**Impact:**
- Unmatched `{PId, terminate}` message stuck at head of queue
- Erlang processes messages in FIFO order
- All subsequent messages (trade requests) never processed
- Actuators wait forever for responses

**Evidence Supporting:**
- ✅ All scapes have `message_queue_len: 1`
- ✅ The message is `{<0.458.0>, terminate}` 
- ✅ Pattern in fx.erl doesn't match
- ✅ Scapes are `waiting` (in receive, not processing)

**Evidence Questioning:**
- ❓ Why are some actuators in `{gen,do_call,4}`? This doesn't match our code
- ❓ If substrates completed, why no "sending_to_actuators" log?
- ❓ Are the NEW actuators actually sending trade messages?

---

## Proposed Fix

**Change in fx.erl line 370:**
```erlang
% ADD THIS PATTERN:
{_From, terminate} ->
    ok;
% KEEP EXISTING:
terminate ->
    ok
```

---

## Outstanding Questions

### 1. Why are actuators in `{gen,do_call,4}`?

**Possibilities:**
- A. Actuators are calling some gen_server we haven't identified
- B. The stack trace shows a parent frame, not current operation
- C. There's indirect gen_server use in scape interaction

**Action needed:** Trace actuator code path more carefully

### 2. Did substrates actually send to actuators?

**Evidence for YES:**
- 15M reductions = computation completed
- Status = waiting (moved past reason/2)

**Evidence for NO:**
- No "sending_to_actuators" log
- Log appears immediately after reason/2 in code

**Possible explanations:**
- A. Logging failed but send succeeded
- B. Exception/crash between reason and logging
- C. Process killed/restarted between operations

**Action needed:** Add more defensive logging

### 3. Are NEW agents affected or just OLD agents?

**Current state:**
- OLD agents (0.8479...) are the 6 stuck ones
- NEW agents (0.147...) were spawned
- Process info shows NEW population processes

**Need to verify:** Are the stuck substrates/actuators from OLD or NEW population?

---

## Next Steps

### Immediate Actions

1. **Verify the fix:**
   - Apply terminate message pattern fix
   - Recompile fx.erl
   - Test if system progresses

2. **If fix works:**
   - System should complete
   - Document root cause
   - Add regression test

3. **If fix doesn't work:**
   - Investigate gen,do_call mystery
   - Add more logging around substrate fanout
   - Check if old/new agent confusion is the real issue

### Additional Investigation Needed

- [ ] Examine exoself terminate code to confirm message format
- [ ] Check if there are other places sending `{PId, terminate}` 
- [ ] Verify substrate fanout actually sends messages
- [ ] Understand why actuators show gen:do_call stack frame
- [ ] Confirm new vs old agent process identity

---

## Confidence Level

**Scape Terminate Bug:** 🔴 HIGH (80%)
- Clear evidence of unmatched message
- Pattern mismatch is obvious
- Explains scape freezing

**Complete Fix:** 🟡 MEDIUM (50%)
- Scape bug is real BUT
- Actuator gen:do_call mystery unresolved
- Substrate logging gap unexplained
- May be multiple issues compounding

**Recommendation:** Apply fix but continue monitoring for other issues.


