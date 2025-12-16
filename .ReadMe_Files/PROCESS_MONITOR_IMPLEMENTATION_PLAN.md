# Process Monitor Module Implementation Plan

## Overview
Create a new Erlang module `process_monitor.erl` that provides manual process monitoring capabilities, logging process memory usage and message queue information to the Benchmarker folder using `qlog.erl`.

## Objectives
1. Create `process_monitor.erl` module with functions to log process information
2. Extend `qlog.erl` with new logging function for process monitoring
3. Support memory usage tracking, message queue tracking, and combined tracking
4. Support configurable output formats (per-line vs single-line)
5. Support limiting output to top N processes
6. Identify process types (Cortex, Sensor, Neuron, Actuator, Substrate, etc.)
7. Include additional valuable metrics: reductions, status, priority, heap_size, total_heap_size

---

## Phase 1: Extend qlog.erl

### Task 1.1: Add process_monitor/1 function
**File:** `qlog.erl`

**Implementation:**
- Add `process_monitor/1` to export list
- Create function following pattern of `benchmarker/2`:
  ```erlang
  process_monitor(Msg) ->
      Dir = filename:join(get_log_root_dir(), "Benchmarker"),
      ensure_directory_exists(Dir),
      Filename = filename:join(Dir, "process_monitor.log"),
      {ok, File} = file:open(Filename, [append]),
      Timestamp = format_timestamp(),
      io:format(File, "~s | ~s~n", [Timestamp, Msg]),
      file:close(File).
  ```

**Location:** After `benchmarker/2` function (around line 185)

**Dependencies:** None

**Testing:** Verify function writes to `logs/Benchmarker/process_monitor.log`

---

## Phase 2: Create process_monitor.erl Module

### Task 2.1: Module Structure and Exports
**File:** `process_monitor.erl`

**Implementation:**
- Create module with proper header
- Export functions:
  - `log_memory_usage/1`
  - `log_message_queue/1`
  - `log_process_info/1`
  - Helper functions (internal)

**Dependencies:** `qlog.erl`, `records.hrl` (if needed for type definitions)

---

### Task 2.2: Process Type Identification Function
**File:** `process_monitor.erl`

**Function:** `identify_process_type/3`

**Implementation:**
- Replicate logic from `genotype_utils:identify_process_type/3`
- Check registered_name first (if registered, return atom_to_list)
- Check initial_call for patterns:
  - `{cortex, prep, _}` → "Cortex"
  - `{sensor, prep, _}` → "Sensor"
  - `{neuron, prep, _}` → "Neuron"
  - `{actuator, prep, _}` → "Actuator"
  - `{substrate, prep, _}` → "Substrate"
  - `{substrate_cpp, prep, _}` → "Substrate_CPP"
  - `{substrate_cep, prep, _}` → "Substrate_CEP"
  - `{exoself, prep, _}` → "ExoSelf"
  - `{scape, gen, _}` → "Scape"
  - `{M, F, _}` when is_atom(M), is_atom(F) → atom_to_list(M)
- Check current_function for same patterns (with `loop` instead of `prep`)
- Default to "Unknown" if cannot identify

**Input:** `{Fun, Init, Reg}` where:
- `Fun` = current_function from process_info
- `Init` = initial_call from process_info
- `Reg` = registered_name from process_info

**Output:** String identifying process type

**Dependencies:** None

---

### Task 2.3: Format Current Function Helper
**File:** `process_monitor.erl`

**Function:** `format_current_function/1`

**Implementation:**
- Take current_function tuple: `{Module, Function, Arity}`
- Format as: `"Module:Function/Arity"`
- Handle undefined case: return `"undefined"`

**Dependencies:** None

---

### Task 2.4: Collect Process Information Helper
**File:** `process_monitor.erl`

**Function:** `collect_process_info/2`

**Implementation:**
- Input: List of Pids, Options proplist
- For each Pid:
  - Call `erlang:process_info(Pid, [memory, message_queue_len, current_function, initial_call, registered_name, reductions, status, priority, heap_size, total_heap_size])`
  - Handle `undefined` (dead process) - skip
  - Extract: memory, message_queue_len, current_function, initial_call, registered_name, reductions, status, priority, heap_size, total_heap_size
  - Call `identify_process_type/3` to get type
  - Call `format_current_function/1` to format function
  - Return list of tuples: `{Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize}`

**Error Handling:**
- Skip processes that return `undefined`
- Continue processing remaining processes
- Handle missing optional fields gracefully (use defaults: 0 for numbers, "unknown" for atoms)

**Dependencies:** `identify_process_type/3`, `format_current_function/1`

---

### Task 2.5: Format Process Entry Helper
**File:** `process_monitor.erl`

**Function:** `format_process_entry/10`

**Implementation:**
- Input: `Pid, Type, Function, Memory, QueueLen, Reductions, Status, Priority, HeapSize, TotalHeapSize`
- Format as: `"Process ID: <pid> | Type: <type> | Function: <function> | Memory: <bytes> | Heap: <bytes> | TotalHeap: <bytes> | Queue: <length> | Reductions: <count> | Status: <status> | Priority: <priority>"`
- Handle optional fields (if any field not provided, use default or omit that part)
- Format numbers with appropriate units/suffixes if needed (e.g., bytes, KB for memory)

**Dependencies:** None

---

### Task 2.6: Sort Processes Helper
**File:** `process_monitor.erl`

**Function:** `sort_processes/2`

**Implementation:**
- Input: List of process info tuples, SortBy atom (`memory`, `message_queue`, `reductions`, `heap_size`, `total_heap_size`)
- Sort by specified metric (high to low)
- Return sorted list
- Default to `memory` if invalid sort_by specified

**Dependencies:** None

---

### Task 2.7: Limit Processes Helper
**File:** `process_monitor.erl`

**Function:** `limit_processes/2`

**Implementation:**
- Input: List of process info tuples, Limit (integer or `all`)
- If Limit is integer, take first N processes
- If Limit is `all` or undefined, return all processes

**Dependencies:** None

---

### Task 2.8: Format Output Helper
**File:** `process_monitor.erl`

**Function:** `format_output/2`

**Implementation:**
- Input: List of formatted process entry strings, Format (`per_line` or `single_line`)
- If `per_line`: Join with newlines, each entry on separate line
- If `single_line`: Join with ` | ` separator, all on one line

**Dependencies:** None

---

### Task 2.9: log_memory_usage/1 Function
**File:** `process_monitor.erl`

**Function:** `log_memory_usage(Options)`

**Implementation:**
1. Get all processes: `erlang:processes()`
2. Collect process info (memory, heap_size, total_heap_size, reductions, status, priority, current_function, initial_call, registered_name, message_queue_len)
3. Identify process types
4. Sort by memory (high to low)
5. Apply limit if specified in Options
6. Format entries (include memory, heap_size, total_heap_size, reductions, status, priority; queue length optional)
7. Format output according to Options
8. Log using `qlog:process_monitor/1`

**Options:**
- `{limit, N}` - Optional, default: all
- `{format, per_line | single_line}` - Optional, default: per_line

**Example:**
```erlang
process_monitor:log_memory_usage([{limit, 10}, {format, per_line}]).
```

**Dependencies:** All helper functions above

---

### Task 2.10: log_message_queue/1 Function
**File:** `process_monitor.erl`

**Function:** `log_message_queue(Options)`

**Implementation:**
1. Get all processes: `erlang:processes()`
2. Collect process info (message_queue_len, reductions, status, priority, current_function, initial_call, registered_name, memory, heap_size)
3. Identify process types
4. Sort by message_queue_len (high to low)
5. Apply limit if specified in Options
6. Format entries (include queue length, reductions, status, priority; memory/heap optional)
7. Format output according to Options
8. Log using `qlog:process_monitor/1`

**Options:**
- `{limit, N}` - Optional, default: all
- `{format, per_line | single_line}` - Optional, default: per_line

**Example:**
```erlang
process_monitor:log_message_queue([{format, single_line}]).
```

**Dependencies:** All helper functions above

---

### Task 2.11: log_process_info/1 Function
**File:** `process_monitor.erl`

**Function:** `log_process_info(Options)`

**Implementation:**
1. Get all processes: `erlang:processes()`
2. Collect process info (memory, message_queue_len, reductions, status, priority, heap_size, total_heap_size, current_function, initial_call, registered_name)
3. Identify process types
4. Sort by specified metric (memory, message_queue, reductions, heap_size, or total_heap_size from Options)
5. Apply limit if specified in Options
6. Format entries (include all fields: memory, heap_size, total_heap_size, queue length, reductions, status, priority)
7. Format output according to Options
8. Log using `qlog:process_monitor/1`

**Options:**
- `{limit, N}` - Optional, default: all
- `{format, per_line | single_line}` - Optional, default: per_line
- `{sort_by, memory | message_queue | reductions | heap_size | total_heap_size}` - Optional, default: memory

**Example:**
```erlang
process_monitor:log_process_info([{limit, 20}, {sort_by, reductions}, {format, per_line}]).
```

**Dependencies:** All helper functions above

---

## Phase 3: Testing and Validation

### Task 3.1: Unit Testing
**Actions:**
1. Test `identify_process_type/3` with various process types
2. Test `format_current_function/1` with valid and undefined inputs
3. Test `collect_process_info/2` with live processes
4. Test sorting functions
5. Test limiting functions
6. Test formatting functions

**Test Cases:**
- Identify Cortex, Sensor, Neuron, Actuator, Substrate processes
- Handle dead processes gracefully
- Handle processes without registered names
- Test with empty process list
- Test with large number of processes

---

### Task 3.2: Integration Testing
**Actions:**
1. Call `log_memory_usage/1` during system execution
2. Verify log file creation in `logs/Benchmarker/process_monitor.log`
3. Verify log format matches specification
4. Test all three main functions
5. Test with different option combinations

**Test Scenarios:**
- Log top 10 processes by memory
- Log all processes by message queue
- Log combined info sorted by memory
- Log combined info sorted by message queue
- Test per_line format
- Test single_line format

---

### Task 3.3: Performance Testing
**Actions:**
1. Measure execution time with large number of processes
2. Verify no significant performance impact
3. Test with system under load

**Metrics:**
- Time to collect process info for 1000+ processes
- Time to sort and format output
- Memory overhead

---

## Phase 4: Documentation

### Task 4.1: Code Comments
**Actions:**
- Add concise one-line comments where needed
- Document function purposes
- Document option parameters

---

### Task 4.2: Usage Examples
**Actions:**
- Document example usage in module header
- Provide examples for each main function
- Document option combinations

---

## Implementation Order

1. **Phase 1** - Extend qlog.erl (Task 1.1)
2. **Phase 2** - Create process_monitor.erl:
   - Task 2.1: Module structure
   - Task 2.2: Process type identification
   - Task 2.3: Format current function
   - Task 2.4: Collect process info
   - Task 2.5: Format process entry
   - Task 2.6: Sort processes
   - Task 2.7: Limit processes
   - Task 2.8: Format output
   - Task 2.9: log_memory_usage
   - Task 2.10: log_message_queue
   - Task 2.11: log_process_info
3. **Phase 3** - Testing
4. **Phase 4** - Documentation

---

## File Structure

```
process_monitor.erl
├── Module header and exports
├── identify_process_type/3
├── format_current_function/1
├── collect_process_info/2
├── format_process_entry/10
├── sort_processes/2
├── limit_processes/2
├── format_output/2
├── log_memory_usage/1
├── log_message_queue/1
└── log_process_info/1
```

---

## Dependencies

- `qlog.erl` - For logging functionality
- `erlang:processes/0` - Built-in function
- `erlang:process_info/2` - Built-in function

---

## Error Handling Strategy

1. **Dead Processes:** Skip processes that return `undefined` from `process_info`
2. **Invalid Options:** Use sensible defaults (all processes, per_line format)
3. **File I/O Errors:** Let qlog.erl handle (it already has error handling)
4. **Large Process Lists:** Efficiently handle 1000+ processes

---

## Log File Format Examples

### Per-line format (full info):
```
[2025-01-15 10:30:45] | Process ID: <0.123.0> | Type: Cortex | Function: cortex:loop/9 | Memory: 45678 | Heap: 23456 | TotalHeap: 32768 | Queue: 0 | Reductions: 1234567 | Status: running | Priority: normal
[2025-01-15 10:30:45] | Process ID: <0.124.0> | Type: Sensor | Function: sensor:loop/8 | Memory: 34567 | Heap: 18923 | TotalHeap: 24576 | Queue: 2 | Reductions: 987654 | Status: waiting | Priority: normal
```

### Per-line format (memory-focused):
```
[2025-01-15 10:30:45] | Process ID: <0.123.0> | Type: Cortex | Function: cortex:loop/9 | Memory: 45678 | Heap: 23456 | TotalHeap: 32768 | Reductions: 1234567 | Status: running | Priority: normal
```

### Per-line format (queue-focused):
```
[2025-01-15 10:30:45] | Process ID: <0.124.0> | Type: Sensor | Function: sensor:loop/8 | Queue: 2 | Reductions: 987654 | Status: waiting | Priority: normal | Memory: 34567
```

### Single-line format:
```
[2025-01-15 10:30:45] | Process ID: <0.123.0> | Type: Cortex | Function: cortex:loop/9 | Memory: 45678 | Heap: 23456 | TotalHeap: 32768 | Queue: 0 | Reductions: 1234567 | Status: running | Priority: normal | Process ID: <0.124.0> | Type: Sensor | Function: sensor:loop/8 | Memory: 34567 | Heap: 18923 | TotalHeap: 24576 | Queue: 2 | Reductions: 987654 | Status: waiting | Priority: normal
```

---

## Success Criteria

1. ✅ Module compiles without errors
2. ✅ All three main functions work correctly
3. ✅ Process types correctly identified
4. ✅ Logging works to correct file location
5. ✅ Both output formats work correctly
6. ✅ Limiting works correctly
7. ✅ Sorting works correctly
8. ✅ Handles edge cases (dead processes, empty lists, etc.)
9. ✅ Code follows existing codebase style
10. ✅ All logging calls are one-liners for easy commenting

---

## Notes

- All `qlog:` calls should be one line for easy commenting
- Comments should be concise (1 line max when needed)
- Code should be clean, concise, effective and efficient
- Follow existing codebase patterns and style
- Reference `genotype_utils.erl` for process identification logic
- Reference `qlog.erl` for logging patterns

---

## Top 4 Most Useful Commands / Use Cases

### Use Case 1: Find Memory Hogs (Top 20 processes by memory)
**When to use:** During system execution to identify processes consuming excessive memory

**Command:**
```erlang
process_monitor:log_memory_usage([{limit, 20}, {format, per_line}]).
```

**What it does:**
- Logs top 20 processes sorted by memory usage (high to low)
- Includes: memory, heap_size, total_heap_size, reductions, status, priority
- Each process on separate line for easy reading
- Helps identify memory leaks or processes growing unexpectedly

**Expected output:** Processes using most memory, typically ExoSelf, Cortex, Substrate, or Neuron processes

---

### Use Case 2: Detect Message Queue Backlog (Top 10 processes by queue length)
**When to use:** When system appears slow or unresponsive, check for processes with message buildup

**Command:**
```erlang
process_monitor:log_message_queue([{limit, 10}, {format, per_line}]).
```

**What it does:**
- Logs top 10 processes sorted by message_queue_len (high to low)
- Includes: queue length, reductions, status, priority
- Identifies processes that are receiving messages faster than they can process them
- Critical for debugging deadlocks or performance bottlenecks

**Expected output:** Processes with queue > 0, especially those with high queue lengths (> 100)

---

### Use Case 3: Find CPU-Intensive Processes (Top 15 by reductions)
**When to use:** When system is slow but memory/message queues look normal, identify CPU-bound processes

**Command:**
```erlang
process_monitor:log_process_info([{limit, 15}, {sort_by, reductions}, {format, per_line}]).
```

**What it does:**
- Logs top 15 processes sorted by reductions (CPU work done)
- Includes all metrics: memory, heap, queue, reductions, status, priority
- Shows which processes are doing the most computational work
- Useful for performance profiling and identifying optimization targets

**Expected output:** Cortex, Neuron, or Substrate processes with highest reduction counts

---

### Use Case 4: Comprehensive System Health Check (All processes, sorted by memory)
**When to use:** Regular monitoring during benchmark runs or when investigating system-wide issues

**Command:**
```erlang
process_monitor:log_process_info([{format, per_line}]).
```

**What it does:**
- Logs ALL processes with complete information
- Sorted by memory (default)
- Includes all metrics: memory, heap_size, total_heap_size, queue, reductions, status, priority
- Provides complete system snapshot for analysis
- Use when you need full visibility into all system processes

**Expected output:** Complete process list showing system-wide resource usage patterns

---

### Quick Reference Cheat Sheet

```erlang
% Memory monitoring
process_monitor:log_memory_usage([{limit, 20}, {format, per_line}]).

% Message queue monitoring  
process_monitor:log_message_queue([{limit, 10}, {format, per_line}]).

% CPU monitoring
process_monitor:log_process_info([{limit, 15}, {sort_by, reductions}, {format, per_line}]).

% Full system check
process_monitor:log_process_info([{format, per_line}]).
```

**Note:** All commands write to `logs/Benchmarker/process_monitor.log`. Check the log file after execution to view results.

---

## Additional Metrics Included

The following additional metrics are now included in all process monitoring functions:

1. **reductions** - Number of reductions (function calls/operations) executed by the process. Useful for identifying CPU-intensive processes and performance profiling.

2. **status** - Current process status: `running`, `waiting`, `garbage_collecting`, `suspended`, `exiting`. Critical for debugging hangs and understanding process state.

3. **priority** - Process priority level: `low`, `normal`, `high`, `max`. Helps understand process scheduling behavior.

4. **heap_size** - Current heap size in words. Provides detailed memory breakdown beyond total memory.

5. **total_heap_size** - Total heap size including overhead in words. Shows actual memory allocation including overhead.

These metrics provide comprehensive insights into:
- **Performance**: reductions show CPU work done
- **Debugging**: status helps identify stuck processes
- **Memory**: heap_size and total_heap_size provide detailed memory analysis
- **Scheduling**: priority shows process importance

---

**Version:** 1.2  
**Created:** 2025-01-15  
**Updated:** 
- 2025-01-15 v1.1: Added reductions, status, priority, heap_size, total_heap_size
- 2025-01-15 v1.2: Added top 4 use cases and command examples
**Status:** Ready for Implementation
