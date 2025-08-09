%% Test module for risk management functionality
%% Tests position sizing, risk limits, and margin requirements

-module(test_risk_management).
-compile(export_all).
-include("records.hrl").

%% Test position sizing logic
test_position_sizing() ->
    io:format("Testing position sizing logic...~n"),
    
    %% Test basic position sizing
    AccountBalance = 10000,
    RiskParams = #{
        position_size => 0.1,
        max_position_per_pair => 0.2
    },
    
    %% Should use the more conservative limit (0.1)
    ExpectedSize = AccountBalance * 0.1,
    
    io:format("Account Balance: ~p~n", [AccountBalance]),
    io:format("Expected Position Size: ~p~n", [ExpectedSize]),
    
    %% Test with different risk parameters
    test_position_size_scenarios(),
    
    io:format("Position sizing tests completed~n").

test_position_size_scenarios() ->
    Scenarios = [
        {1000, 0.05, 0.1, 50},    % Small account, conservative
        {10000, 0.1, 0.2, 1000},  % Medium account, moderate
        {100000, 0.2, 0.3, 20000} % Large account, aggressive
    ],
    
    lists:foreach(fun({Balance, PosSize, MaxPerPair, Expected}) ->
        Effective = min(PosSize, MaxPerPair),
        Actual = Balance * Effective,
        io:format("Balance: ~p, PosSize: ~p, MaxPerPair: ~p -> ~p (expected ~p)~n",
                 [Balance, PosSize, MaxPerPair, Actual, Expected])
    end, Scenarios).

%% Test risk limit checking
test_risk_limits() ->
    io:format("Testing risk limit checking...~n"),
    
    %% Create test risk state
    RiskState = #risk_state{
        daily_start_balance = 10000,
        daily_pnl = -400,  % 4% daily loss
        daily_trades = 45,
        max_drawdown = -600,
        total_exposure = 0.3
    },
    
    %% Create test performance metrics
    Performance = #performance_metrics{
        total_pnl = 9600  % Down from 10000
    },
    
    %% Test daily loss limit (5% = 500)
    MaxDailyLoss = 0.05,
    DailyLossLimit = 10000 * MaxDailyLoss,
    
    io:format("Daily P&L: ~p, Limit: ~p~n", [RiskState#risk_state.daily_pnl, -DailyLossLimit]),
    
    if
        RiskState#risk_state.daily_pnl < -DailyLossLimit ->
            io:format("FAIL: Daily loss limit exceeded~n");
        true ->
            io:format("PASS: Daily loss within limits~n")
    end,
    
    %% Test trade limit
    MaxTrades = 50,
    io:format("Daily trades: ~p, Limit: ~p~n", [RiskState#risk_state.daily_trades, MaxTrades]),
    
    if
        RiskState#risk_state.daily_trades >= MaxTrades ->
            io:format("FAIL: Daily trade limit exceeded~n");
        true ->
            io:format("PASS: Daily trades within limits~n")
    end,
    
    io:format("Risk limit tests completed~n").

%% Test margin requirements
test_margin_requirements() ->
    io:format("Testing margin requirements...~n"),
    
    %% Test scenarios
    Scenarios = [
        {1000, 0.02, 20, 800, pass},    % Normal case
        {1000, 0.05, 50, 500, pass},    % Higher margin
        {1000, 0.1, 100, 200, fail}     % Insufficient margin
    ],
    
    lists:foreach(fun({PositionValue, MarginReq, RequiredMargin, AvailableMargin, Expected}) ->
        ActualRequired = PositionValue * MarginReq,
        Result = if
            ActualRequired > AvailableMargin -> fail;
            true -> pass
        end,
        
        Status = if
            Result =:= Expected -> "PASS";
            true -> "FAIL"
        end,
        
        io:format("~s: Position: ~p, Required: ~p, Available: ~p -> ~p~n",
                 [Status, PositionValue, ActualRequired, AvailableMargin, Result])
    end, Scenarios),
    
    io:format("Margin requirement tests completed~n").

%% Test exposure limits
test_exposure_limits() ->
    io:format("Testing exposure limits...~n"),
    
    %% Create test positions
    Positions = [
        #position_info{
            symbol = "EURUSD",
            side = long,
            quantity = 10000,
            entry_price = 1.1000,
            exposure_amount = 11000
        },
        #position_info{
            symbol = "GBPUSD", 
            side = short,
            quantity = 8000,
            entry_price = 1.2500,
            exposure_amount = 10000
        }
    ],
    
    %% Calculate total exposure
    TotalExposure = lists:sum([P#position_info.exposure_amount || P <- Positions]),
    io:format("Total exposure: ~p~n", [TotalExposure]),
    
    %% Test against limits
    AccountBalance = 50000,
    MaxTotalExposure = 0.5,  % 50%
    ExposureLimit = AccountBalance * MaxTotalExposure,
    
    io:format("Exposure limit: ~p~n", [ExposureLimit]),
    
    if
        TotalExposure > ExposureLimit ->
            io:format("FAIL: Total exposure exceeds limit~n");
        true ->
            io:format("PASS: Total exposure within limits~n")
    end,
    
    %% Test per-pair limits
    MaxPerPair = 0.2,  % 20%
    PerPairLimit = AccountBalance * MaxPerPair,
    
    lists:foreach(fun(Position) ->
        Symbol = Position#position_info.symbol,
        Exposure = Position#position_info.exposure_amount,
        
        if
            Exposure > PerPairLimit ->
                io:format("FAIL: ~s exposure (~p) exceeds per-pair limit (~p)~n", 
                         [Symbol, Exposure, PerPairLimit]);
            true ->
                io:format("PASS: ~s exposure (~p) within per-pair limit (~p)~n",
                         [Symbol, Exposure, PerPairLimit])
        end
    end, Positions),
    
    io:format("Exposure limit tests completed~n").

%% Test daily counter reset
test_daily_reset() ->
    io:format("Testing daily counter reset...~n"),
    
    %% Create risk state from yesterday
    Yesterday = {2024, 1, 1},
    Today = {2024, 1, 2},
    
    OldRiskState = #risk_state{
        daily_pnl = -200,
        daily_trades = 25,
        last_reset_date = Yesterday
    },
    
    %% Simulate reset check
    case Today of
        Yesterday ->
            io:format("FAIL: Should have detected new day~n");
        _ ->
            %% Reset counters
            NewRiskState = OldRiskState#risk_state{
                daily_pnl = 0.0,
                daily_trades = 0,
                last_reset_date = Today
            },
            
            io:format("PASS: Daily counters reset~n"),
            io:format("Old: PnL=~p, Trades=~p, Date=~p~n", 
                     [OldRiskState#risk_state.daily_pnl,
                      OldRiskState#risk_state.daily_trades,
                      OldRiskState#risk_state.last_reset_date]),
            io:format("New: PnL=~p, Trades=~p, Date=~p~n",
                     [NewRiskState#risk_state.daily_pnl,
                      NewRiskState#risk_state.daily_trades,
                      NewRiskState#risk_state.last_reset_date])
    end,
    
    io:format("Daily reset tests completed~n").

%% Run all risk management tests
run_all_tests() ->
    io:format("=== Risk Management Test Suite ===~n"),
    
    test_position_sizing(),
    io:format("~n"),
    
    test_risk_limits(),
    io:format("~n"),
    
    test_margin_requirements(),
    io:format("~n"),
    
    test_exposure_limits(),
    io:format("~n"),
    
    test_daily_reset(),
    io:format("~n"),
    
    io:format("=== All Risk Management Tests Completed ===~n").

%% Sync function for development
sync() ->
    make:all([load]).