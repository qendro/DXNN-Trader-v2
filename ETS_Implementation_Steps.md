# ETS Implementation Steps: Align live_scape_new with fx.erl Table Format

## Objective
Modify only `live_scape_new.erl` and `ib_service.py` to create per-symbol ETS tables using `#technical` records, matching fx.erl's table structure. This enables future fx.erl modifications to read from live tables without changing any other modules.

## Scope Limitations
- **ONLY modify:** `live_scape_new.erl` and `ib_service.py`
- **NO persistence** - pure in-memory ETS tables
- **NO optional features** - minimal implementation only
- **NO backward compatibility** - clean, simple implementation
- **NO changes** to fx.erl, sensors, actuators, or any other modules

---

## Step 1: Modify ib_service.py

### 1.1 Add Timeframe Information to OHLC Messages
**File:** `priv/ib_service.py`

**Changes needed:**
- Modify `LiveTickAggregator.process_tick()` to include `tf_s` (timeframe in seconds) in outgoing messages
- Update `HistoricalDataLoader.load_weeks_of_data()` to include `tf_s` based on `bar_size` parameter
- Add helper function to convert bar_size strings ("1 min", "5 min") to seconds (60, 300)

**Expected message format after changes:**
```json
{
  "type": "ohlc_bar",
  "data": {
    "symbol": "EUR.USD",
    "t_open": "2024-08-29T15:20:00Z",
    "o": 1.0850,
    "h": 1.0855,
    "l": 1.0845,
    "c": 1.0852,
    "vol": 1000,
    "tf_s": 60,
    "source": "live"
  }
}
```

### 1.2 Implementation Details
- Add `bar_size_to_seconds()` helper function
- Modify tick aggregator to track and send timeframe
- Update historical data loader to derive timeframe from bar_size parameter
- Ensure all OHLC messages include `tf_s` field

---

## Step 2: Modify live_scape_new.erl

### 2.1 Add Symbol-to-Table Conversion Functions
**File:** `live_scape_new.erl`

**New functions to add:**
```erlang
% Convert "EUR.USD" + 60 seconds -> 'EURUSD1'
symbol_to_table(SymbolBin, SamplingRateSec) -> TableAtom

% Convert ISO timestamp to fx.erl tuple format
iso_to_technical_id(ISOString, SamplingRateSec) -> {Y,Mo,D,H,Mi,S,SamplingRateSec}

% Ensure per-symbol table exists
ensure_symbol_table(TableAtom) -> ok
```

### 2.2 Update handle_ohlc_bar/1 Function
**Current behavior:** Inserts into single `ohlc_data` table with `#ohlc_bar` records

**New behavior:**
- Extract `tf_s` from incoming message (default to 60 if missing)
- Convert symbol using `symbol_to_table/2`
- Convert ISO timestamp using `iso_to_technical_id/2`
- Create `#technical` record instead of `#ohlc_bar`
- Insert into per-symbol table instead of `ohlc_data`

### 2.3 Update Sensor Interface Functions
**Functions to modify:**
- `get_price_list/2` - Use provided TableName parameter, work with `#technical` records
- `collect_recent_bars/4` - Work with `#technical` records instead of `#ohlc_bar`
- `handle_sense/4` - Pass through TableName to price list functions

**Key changes:**
- Replace `#ohlc_bar` field access with `#technical` field access
- Use `TableName` parameter from sensor requests
- Clean implementation using fx.erl format directly

### 2.4 Add fx.erl Compatibility Shims
**New functions to add:**
```erlang
% Direct ETS operations for per-symbol tables
lookup_technical(TableName, Key) -> #technical{} | undefined
next_technical(TableName, Key) -> Key | '$end_of_table'
prev_technical(TableName, Key) -> Key | '$end_of_table'
```

### 2.5 Update Table Management
**Modify:**
- Remove `ensure_ohlc_tables/0` and `ohlc_data` table completely
- Add dynamic table creation in `ensure_symbol_table/1`
- Use `#technical` record keypos (`{keypos, #technical.id}`)
- Remove all `#ohlc_bar` record definitions and usage

---

## Step 3: Implementation Sequence

### 3.1 Phase 1: Python Changes
1. Add `bar_size_to_seconds()` helper function
2. Modify `LiveTickAggregator` to include `tf_s` in messages
3. Modify `HistoricalDataLoader` to include `tf_s` in messages
4. Test that all outgoing OHLC messages contain `tf_s` field

### 3.2 Phase 2: Erlang Helper Functions
1. Add `symbol_to_table/2` function
2. Add `iso_to_technical_id/2` function
3. Add `ensure_symbol_table/1` function
4. Test symbol conversion logic independently

### 3.3 Phase 3: Update OHLC Processing
1. Modify `handle_ohlc_bar/1` to use new functions
2. Create `#technical` records instead of `#ohlc_bar`
3. Insert into per-symbol tables
4. Remove `ohlc_data` table creation and usage
5. Test with single symbol first

### 3.4 Phase 4: Update Sensor Interface
1. Modify `get_price_list/2` to use TableName parameter
2. Update `collect_recent_bars/4` for `#technical` records
3. Remove all `#ohlc_bar` record handling
4. Test sensor interface with new format

### 3.5 Phase 5: Add Compatibility Functions
1. Add `lookup_technical/2`, `next_technical/2`, `prev_technical/2`
2. Test ETS navigation functions
3. Verify table structure matches fx.erl expectations

---

## Step 4: Testing Strategy

### 4.1 Unit Tests
- `symbol_to_table("EUR.USD", 60)` → `'EURUSD1'`
- `symbol_to_table("GBP.USD", 300)` → `'GBPUSD5'`
- ISO timestamp parsing produces correct tuple format
- Table creation works for multiple symbols

### 4.2 Integration Tests
- Python sends OHLC with `tf_s`, Erlang creates correct table
- Multiple symbols create separate tables
- Sensor requests work with new `#technical` format
- ETS navigation functions work correctly

### 4.3 Format Tests
- Table structure matches fx.erl `#technical` format exactly
- Record fields map correctly (open, high, low, close, volume)
- Key format matches fx.erl expectations

---

## Step 5: Expected Outcomes

### 5.1 Table Structure After Implementation
```erlang
% Tables created: 'EURUSD1', 'GBPUSD1', 'USDJPY5', etc.
% Records: #technical{id={2024,8,29,15,20,0,60}, open=1.0850, ...}
% Keys: {Year,Month,Day,Hour,Minute,Second,SamplingRateSec}
```

### 5.2 Clean Implementation
- Pure `#technical` record format throughout
- No legacy `#ohlc_bar` code remaining
- Minimal, focused codebase

### 5.3 Future fx.erl Integration
- fx.erl can be modified to read from live tables using existing functions
- Table names and record formats will match exactly
- Seamless transition between historical and live data

---

## Implementation Notes

### Key Design Decisions
1. **Minimal scope:** Only touch two files to reduce risk
2. **No persistence:** Keep implementation simple, focus on ETS alignment
3. **No backward compatibility:** Clean, simple implementation
4. **Future-ready:** Enable fx.erl integration without breaking changes

### Risk Mitigation
- Test each phase independently
- Remove all legacy code completely
- Keep changes isolated to two modules only
- Clean implementation without compatibility layers

### Success Criteria
- Per-symbol ETS tables created with `#technical` records
- Clean sensor interface using fx.erl format directly
- Table structure identical to fx.erl format
- No legacy code remaining
- Ready for future fx.erl live data integration