# Plan: Align `live_scape_new` ETS Layout With `fx.erl` Per‑Symbol Tables

## Goals

- Store live OHLC bars in ETS using the same per‑symbol/timeframe pattern as `fx.erl` so sensors and NN flows can consume data uniformly.
- Preserve backward compatibility while enabling multiple symbols and timeframes.
- Keep persistence optional; avoid write‑amplification on each tick.

## Target Table Layout (fx‑style)

- Table per {symbol, timeframe}:
  - Example atoms: `'EURUSD1'`, `'GBPUSD1'`, `'USDJPY5'`, `'EURUSD15'`.
  - One ETS table per atom: `ordered_set`, `public`, `named_table`, `{keypos, 2}`.
- Record shape (reuse `fx.erl`): `#technical{id, open, high, low, close, volume}`
  - `id` is the key and encodes time: `{Year,Month,Day,Hour,Minute,Second, SamplingRateSec}`.
  - Remaining fields are numeric OHLCV.
- Naming convention:
  - Symbol normalization: Python `"EUR.USD"` → remove dot and uppercase → `"EURUSD"`.
  - Timeframe suffix: integer minutes (1, 5, 15, 30, 60, …) concatenated to the symbol name.
  - Final table atom: `list_to_atom(SymbolNoDot ++ integer_to_list(Minutes))`.

## Mapping From Python Bars to ETS Rows

- Inputs from Python (`priv/ib_service.py`):
  - Symbol: `"EUR.USD"` (string)
  - Bar time: ISO‑8601 string `t_open` (e.g., `"2024-08-29T15:20:00Z"`)
  - OHLCV: numeric
  - Bar size/timeframe: derive from config (`data.bar_size`, e.g., `"1 min"`) or include an explicit `tf_s`/`bar_sec` per message.

- Transform per bar:
  - Compute `{Year,Month,Day,Hour,Minute,Second}` from `t_open`.
  - Derive `SamplingRateSec` from bar size: `1 min → 60`, `5 min → 300`, etc.
  - Build record: `#technical{id={Y,Mo,D,H,Mi,S,SamplingRateSec}, open=O, high=H, low=L, close=C, volume=Vol}`.
  - Compute table atom via the naming convention, e.g., `'EURUSD1'`.

- Upsert:
  - Ensure table exists (lazy create on first insert).
  - `ets:insert(TableAtom, TechnicalRecord)` (idempotent by key).

## Changes in `live_scape_new.erl`

1) Table management
- Add a helper to normalize symbol and timeframe and produce table atom:
  - `symbol_to_table(SymbolBin, SamplingRateSec) -> TableAtom` (e.g., `<<"EUR.USD">>, 60 -> 'EURUSD1'`).
- Add `ensure_table(TableAtom)` to `ets:new` if missing: `[ordered_set, public, named_table, {keypos, #technical.id}]`.

2) Write path (from Python → ETS)
- In `handle_ohlc_bar/1`:
  - Parse `Symbol`, `t_open`, OHLCV.
  - Determine `SamplingRateSec`:
    - Option A: expect Python to include `tf_s` (preferred for explicitness).
    - Option B: default to 60s if absent; optionally allow override in config.
  - Convert `t_open` (ISO) → `{Y,Mo,D,H,Mi,S}`; build `Id = {Y,Mo,D,H,Mi,S,SamplingRateSec}`.
  - Compute `TableAtom = symbol_to_table(Symbol, SamplingRateSec)`.
  - `ensure_table(TableAtom)`, then `ets:insert(TableAtom, #technical{...})`.
  - Optional dual‑write: also insert a canonical row into existing `ohlc_data` for tooling continuity (feature‑flagged).

3) Read path for sensors (neural network compatibility)
- Respect the `TableName` parameter passed by sensors (e.g., `'EURUSD1'`):
  - `get_price_list/2` should use the provided table rather than `ohlc_data`.
  - Seed with `ets:last(TableName)` and walk back with `ets:prev/2` as `fx.erl` does.
- Maintain the same outward list/plane encodings so existing networks remain unchanged.

4) Trade path cleanup
- Pass symbol context through NN→scape trade messages (optionally infer from the active `TableName`).
- For now, when receiving `{From, trade, TableName, TradeSignal}`, resolve `TableName` → symbol string (e.g., `'EURUSD1'` → `"EUR.USD"`) for Python.

5) Backward compatibility
- Keep the current `#ohlc_bar` canonical path as an optional mirror (debug/inspection/testing).
- If enabled, insert both into `ohlc_data` and the per‑symbol `#technical` table.

## Persistence Strategy (Optional)

- fx‑style persistence lives under `fx_tables/` via `ets:tab2file/2`.
- For live tables, prefer periodic snapshots (timer or on controlled shutdown) rather than every insert.
  - Example: every N minutes, snapshot active live tables to `fx_tables/<TableAtom>`.
  - On startup, attempt `ets:file2tab/1` to warm cache if snapshots exist.

## Integration/Testing Checklist

- Unit checks
  - `symbol_to_table/2` maps examples: `"EUR.USD" + 60 → 'EURUSD1'`, `"USD.JPY" + 300 → 'USDJPY5'`.
  - ISO parser produces correct `Id` values and round‑trip ordering (`ets:last/prev`).

- End‑to‑end checks
  - Python sends bars for 2+ symbols/timeframes; confirm tables are created and updated.
  - Sensors request `'EURUSD1'` and receive consistent last‑N close/plane encodings.
  - Optional: snapshot and restore a live table, then verify continuity (`ets:last/1`).

## Open Decisions

- Whether to dual‑write into `ohlc_data` (debug/inspection) or fully migrate reads to per‑table only.
- Whether Python should include explicit `tf_s` per bar (recommended) or Erlang infers/draws from config.

---

## Time Formatting Options (Pick One)

You have two reasonable places to reconcile time formats: change `live_scape_new` to match `fx.erl`’s key, or change `fx.erl` to accept ISO‑based keys/records. Recommendation: change `live_scape_new` (Option A) to match `#technical.id`; it keeps all historical code and benchmarks untouched.

### Option A: Change `live_scape_new` to `fx` time key

- What changes:
  - Convert ISO `t_open` → `{Y,Mo,D,H,Mi,S,SamplingRateSec}`.
  - Write `#technical` records per‑table; stop relying on `t_open` string ordering.
  - Sensors use their provided `TableName` (`'EURUSD1'`), identical to historical.
- Pros:
  - Perfect alignment with `fx.erl`; sensors/benchmarks behave the same in live and historical.
  - Strong ordering semantics; avoids lexicographic pitfalls.
- Cons:
  - Requires a small conversion function and timeframe awareness in `live_scape_new`.

### Option B: Change `fx.erl` to accept ISO string keys / `#ohlc_bar`

- What changes:
  - Switch `fx.erl` to read from a canonical table (e.g., `ohlc_data`) keyed by `{Symbol, TOpenISO}` or add a parsing path for `#ohlc_bar`.
  - Adjust sensors/consumers to pass a symbol/timeframe filter rather than a per‑table atom.
- Pros:
  - Minimal change to `live_scape_new` if you keep its current schema.
- Cons:
  - Touches a lot of historical/benchmark code paths; loses the elegant per‑table scoping (`ets:last/prev` naturally partitioned).
  - Requires prefix filters or a secondary index to avoid mixing symbols/timeframes.

### Recommendation

- Adopt Option A: Change `live_scape_new` to emit `#technical` into per‑symbol tables (`'EURUSD1'`, etc.) with `#technical.id = {Y,Mo,D,H,Mi,S,SamplingRateSec}`.
- Optionally mirror into `ohlc_data` for debugging while migrating.

