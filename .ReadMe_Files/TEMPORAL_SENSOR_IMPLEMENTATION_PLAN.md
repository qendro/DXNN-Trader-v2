# Temporal Sensor Implementation Plan - Option 1 (Pre-compute and Store)

## Overview
This plan implements a temporal sensor by pre-computing 28 temporal features and storing them as additional columns in the FX data files. This approach provides maximum runtime performance at the cost of increased storage and pre-processing complexity.

## Current Data Format Analysis
**Current EURUSD1.txt format:**
```
2022-09-21 21:15:00,0.983775,0.9838,0.983685,0.98375,-1.0
Timestamp,Open,High,Low,Close,Volume
```

**Target format with temporal features:**
```
2022-09-21 21:15:00,0.983775,0.9838,0.983685,0.98375,-1.0,0,0,1,0,0,0,0,0,0,0,0,0,0.677,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0
Timestamp,Open,High,Low,Close,Volume,[28 temporal features]
```

## Phase 1: Data Structure Design

### 1.1 Extended Technical Record
```erlang
% Update fx.erl record definition
-record(technical_temporal,{
    id,        % key={Year,Month,Day,Hour,Minute,Second,sampling_rate}
    open,
    high,
    low,
    close,
    volume,
    % Temporal features (28 elements)
    month_1, month_2, month_3, month_4, month_5, month_6,
    month_7, month_8, month_9, month_10, month_11, month_12,  % 12 elements
    day_normalized,                                            % 1 element
    weekday_mon, weekday_tue, weekday_wed, weekday_thu, 
    weekday_fri, weekday_weekend,                             % 6 elements
    hour_0_5, hour_6_11, hour_12_17, hour_18_23,            % 4 elements
    holiday,                                                  % 1 element
    quarter_1, quarter_2, quarter_3, quarter_4               % 4 elements
}).
```

### 1.2 Temporal Feature Calculator Module
Create `temporal_calculator.erl`:
```erlang
-module(temporal_calculator).
-export([calculate_temporal_features/1, parse_timestamp/1]).

calculate_temporal_features(TimestampString) ->
    {{Year, Month, Day}, {Hour, _Minute, _Second}} = parse_timestamp(TimestampString),
    
    % Calculate all 28 features
    MonthFeatures = create_month_onehot(Month),
    DayNormalized = Day / 31,
    WeekdayFeatures = create_weekday_onehot(Year, Month, Day),
    HourFeatures = create_hour_block_onehot(Hour),
    HolidayFeature = is_holiday(Year, Month, Day),
    QuarterFeatures = create_quarter_onehot(Month),
    
    % Return as list of 28 values
    MonthFeatures ++ [DayNormalized] ++ WeekdayFeatures ++ 
    HourFeatures ++ [HolidayFeature] ++ QuarterFeatures.
```

## Phase 2: Data Processing Pipeline

### 2.1 Temporal Data Preprocessor
Create `temporal_preprocessor.erl`:
```erlang
-module(temporal_preprocessor).
-export([process_fx_file/2, batch_process_all_files/0]).

process_fx_file(InputFile, OutputFile) ->
    {ok, InputHandle} = file:open(InputFile, [read]),
    {ok, OutputHandle} = file:open(OutputFile, [write]),
    
    process_lines(InputHandle, OutputHandle),
    
    file:close(InputHandle),
    file:close(OutputHandle).

process_lines(InputHandle, OutputHandle) ->
    case file:read_line(InputHandle) of
        {ok, Line} ->
            ProcessedLine = add_temporal_features(Line),
            file:write(OutputHandle, ProcessedLine),
            process_lines(InputHandle, OutputHandle);
        eof ->
            ok
    end.

add_temporal_features(Line) ->
    % Parse: "2022-09-21 21:15:00,0.983775,0.9838,0.983685,0.98375,-1.0\n"
    [TimestampStr | PriceData] = string:tokens(string:strip(Line, right, $\n), ","),
    
    % Calculate temporal features
    TemporalFeatures = temporal_calculator:calculate_temporal_features(TimestampStr),
    
    % Combine original data with temporal features
    AllData = [TimestampStr] ++ PriceData ++ [float_to_list(F) || F <- TemporalFeatures],
    string:join(AllData, ",") ++ "\n".
```

### 2.2 Batch Processing Script
Create `scripts/preprocess_temporal_data.erl`:
```erlang
-module(preprocess_temporal_data).
-export([run/0]).

run() ->
    InputDir = "fx_tables/",
    OutputDir = "fx_tables_temporal/",
    
    % Ensure output directory exists
    file:make_dir(OutputDir),
    
    % Process all FX data files
    Files = ["EURUSD1.txt", "EURUSD1_LIVE.txt", "EURUSD1_EPOCH.txt"],
    
    lists:foreach(fun(File) ->
        InputPath = InputDir ++ File,
        OutputPath = OutputDir ++ File,
        io:format("Processing ~s -> ~s~n", [InputPath, OutputPath]),
        temporal_preprocessor:process_fx_file(InputPath, OutputPath)
    end, Files),
    
    io:format("Temporal preprocessing complete!~n").
```

## Phase 3: FX Module Updates

### 3.1 Update fx.erl Data Loading
```erlang
% Modify fx.erl to handle temporal data
load_temporal_fx_data(TableName) ->
    FileName = ?FX_TABLES_DIR ++ atom_to_list(TableName) ++ ".txt",
    {ok, File} = file:open(FileName, [read]),
    load_temporal_lines(File, TableName, 1),
    file:close(File).

load_temporal_lines(File, TableName, Index) ->
    case file:read_line(File) of
        {ok, Line} ->
            TechnicalData = parse_temporal_line(Line, Index),
            ets:insert(TableName, TechnicalData),
            load_temporal_lines(File, TableName, Index + 1);
        eof ->
            ok
    end.

parse_temporal_line(Line, Index) ->
    Tokens = string:tokens(string:strip(Line, right, $\n), ","),
    [TimestampStr, OpenStr, HighStr, LowStr, CloseStr, VolumeStr | TemporalStrs] = Tokens,
    
    % Parse temporal features (28 values)
    TemporalValues = [list_to_float(S) || S <- TemporalStrs],
    [M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,DayNorm,WMon,WTue,WWed,WThu,WFri,WWknd,H1,H2,H3,H4,Holiday,Q1,Q2,Q3,Q4] = TemporalValues,
    
    #technical_temporal{
        id = Index,
        open = list_to_float(OpenStr),
        high = list_to_float(HighStr),
        low = list_to_float(LowStr),
        close = list_to_float(CloseStr),
        volume = list_to_float(VolumeStr),
        % Temporal features
        month_1=M1, month_2=M2, month_3=M3, month_4=M4, month_5=M5, month_6=M6,
        month_7=M7, month_8=M8, month_9=M9, month_10=M10, month_11=M11, month_12=M12,
        day_normalized=DayNorm,
        weekday_mon=WMon, weekday_tue=WTue, weekday_wed=WWed, weekday_thu=WThu,
        weekday_fri=WFri, weekday_weekend=WWknd,
        hour_0_5=H1, hour_6_11=H2, hour_12_17=H3, hour_18_23=H4,
        holiday=Holiday,
        quarter_1=Q1, quarter_2=Q2, quarter_3=Q3, quarter_4=Q4
    }.
```

## Phase 4: Sensor Implementation

### 4.1 Update morphology.erl
```erlang
% Add temporal sensor to forex_trader morphology
forex_trader(sensors)->
    PLI_Sensors = [#sensor{name=fx_PLI,type=standard,scape={private,fx_sim},format=no_geo,vl=HRes,parameters=[HRes,close]} || HRes<-config:pli_resolutions()],
    PCI_Sensors = [#sensor{name=fx_PCI,type=standard,scape={private,fx_sim},format={symmetric,[HRes,VRes]},vl=HRes*VRes,parameters=[HRes,VRes]} || HRes <-config:pci_horizontal_resolutions(), VRes<-config:pci_vertical_resolutions()],
    InternalSensors = [#sensor{name=fx_Internals,type=standard,scape={private,fx_sim},format=no_geo,vl=config:internal_sensor_dimensions(),parameters=[config:internal_sensor_dimensions()]}],
    TemporalSensors = [#sensor{name=fx_Temporal,type=standard,scape={private,fx_sim},format=no_geo,vl=28,parameters=[precomputed]}],
    
    PLI_Sensors ++ TemporalSensors.  % Add temporal to active sensors
```

### 4.2 Update sensor.erl
```erlang
% Add fx_Temporal sensor function
fx_Temporal(ExoSelf_PId, VL, Parameters, Scape) ->
    % Get current bar data (which now includes temporal features)
    CurrentBar = get_current_bar(Scape),
    
    % Extract pre-computed temporal features from the bar
    TemporalVector = [
        CurrentBar#technical_temporal.month_1,
        CurrentBar#technical_temporal.month_2,
        CurrentBar#technical_temporal.month_3,
        CurrentBar#technical_temporal.month_4,
        CurrentBar#technical_temporal.month_5,
        CurrentBar#technical_temporal.month_6,
        CurrentBar#technical_temporal.month_7,
        CurrentBar#technical_temporal.month_8,
        CurrentBar#technical_temporal.month_9,
        CurrentBar#technical_temporal.month_10,
        CurrentBar#technical_temporal.month_11,
        CurrentBar#technical_temporal.month_12,
        CurrentBar#technical_temporal.day_normalized,
        CurrentBar#technical_temporal.weekday_mon,
        CurrentBar#technical_temporal.weekday_tue,
        CurrentBar#technical_temporal.weekday_wed,
        CurrentBar#technical_temporal.weekday_thu,
        CurrentBar#technical_temporal.weekday_fri,
        CurrentBar#technical_temporal.weekday_weekend,
        CurrentBar#technical_temporal.hour_0_5,
        CurrentBar#technical_temporal.hour_6_11,
        CurrentBar#technical_temporal.hour_12_17,
        CurrentBar#technical_temporal.hour_18_23,
        CurrentBar#technical_temporal.holiday,
        CurrentBar#technical_temporal.quarter_1,
        CurrentBar#technical_temporal.quarter_2,
        CurrentBar#technical_temporal.quarter_3,
        CurrentBar#technical_temporal.quarter_4
    ],
    
    TemporalVector.
```

## Phase 5: Configuration Updates

### 5.1 Update config.erl
```erlang
% Add temporal sensor configuration
temporal_sensor_enabled() -> true.
temporal_timezone() -> 'US/Eastern'.
temporal_holiday_calendar() -> us_markets.
fx_tables_temporal_dir() -> "fx_tables_temporal/".

% Update morphology to include temporal
morphology() -> forex_trader_temporal.  % New morphology with temporal
```

### 5.2 Create Holiday Calendar Module
Create `holiday_calendar.erl`:
```erlang
-module(holiday_calendar).
-export([is_holiday/3, get_us_holidays/1]).

is_holiday(Year, Month, Day) ->
    Holidays = get_us_holidays(Year),
    lists:member({Month, Day}, Holidays).

get_us_holidays(Year) ->
    [
        {1, 1},   % New Year's Day
        {7, 4},   % Independence Day  
        {12, 25}, % Christmas
        % Add more holidays as needed
        easter_monday(Year),
        thanksgiving(Year)
    ].

% Calculate dynamic holidays
easter_monday(Year) ->
    % Easter calculation algorithm
    % Return {Month, Day}
    {4, 10}.  % Placeholder

thanksgiving(Year) ->
    % Fourth Thursday in November
    % Return {Month, Day}  
    {11, 24}. % Placeholder
```

## Phase 6: Implementation Steps

### Step 1: Create Support Modules
1. Create `temporal_calculator.erl`
2. Create `temporal_preprocessor.erl` 
3. Create `holiday_calendar.erl`
4. Create preprocessing script

### Step 2: Process Existing Data
1. Run preprocessing script on all FX data files
2. Verify temporal feature accuracy
3. Create backup of original files

### Step 3: Update Core Modules
1. Update `fx.erl` with temporal data loading
2. Update `sensor.erl` with `fx_Temporal` function
3. Update `morphology.erl` with temporal sensor
4. Update `config.erl` with temporal settings

### Step 4: Testing & Validation
1. Test temporal feature accuracy
2. Verify sensor output matches expected 28-element vector
3. Run integration tests with existing system
4. Performance benchmarking

### Step 5: Migration Strategy
1. Keep original data files as backup
2. Update file paths in config to point to temporal versions
3. Gradual rollout with fallback capability

## Storage Impact Analysis

### File Size Increase
- **Original**: 6 columns per row
- **With Temporal**: 34 columns per row (6 + 28)
- **Size Increase**: ~567% larger files

### Example Calculation
- **EURUSD1.txt**: ~1M rows × 34 columns = ~34M data points
- **Storage**: ~200MB per file (vs ~35MB original)

## Performance Benefits

### Runtime Performance
- **Zero calculation overhead** during trading
- **Direct memory access** to temporal features
- **Consistent temporal data** across all simulations

### Computational Savings
- **No timestamp parsing** during sensor calls
- **No holiday lookups** during runtime  
- **No one-hot encoding** calculations per cycle

## Risks & Mitigation

### Data Consistency Risk
- **Risk**: Temporal features become inconsistent with timestamps
- **Mitigation**: Automated validation scripts, checksums

### Storage Risk  
- **Risk**: 5x larger data files
- **Mitigation**: Compression, selective temporal features

### Flexibility Risk
- **Risk**: Hard to change temporal encoding
- **Mitigation**: Keep preprocessing pipeline, version temporal data

## Success Metrics

1. **Temporal sensor produces correct 28-element vectors**
2. **Zero runtime calculation overhead**
3. **Consistent results across simulation runs**
4. **Neural network can access temporal features**
5. **File size increase acceptable (<1GB total)**

This implementation plan provides a complete roadmap for pre-computing and storing temporal features, maximizing runtime performance while managing the complexity of data preprocessing and storage overhead.
