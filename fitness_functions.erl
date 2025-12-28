-module(fitness_functions).
-compile(export_all).
-include("records.hrl").

%% FX-specific records (from fx.erl) - needed for fitness calculations
-record(state,{table_name,feature,index_start,index_end,index,price_list=[],cycle=0,realized_pl_by_cycle=[]}).
-record(account,{leverage,lot,spread,margin,balance,net_asset_value,realized_PL=0,unrealized_PL=0,order}).

%% ===================================================================
%% Fitness Functions Module
%% ===================================================================
%% This module contains multiple fitness calculation functions for evaluating trading agents.
%% The active fitness function is selected via config:fitness_function().
%%
%% Available Fitness Functions:
%% - time_weighted: Time-weighted realized profits with bonuses/penalties (current default)
%% - total_profit: Simple sum of balance + unrealized P/L
%% - sharpe_ratio: Risk-adjusted return using realized P/L volatility
%% - profit_factor: Gross profit / gross loss ratio
%% - total_return: Percentage return from initial balance
%% - sortino_ratio: Downside deviation-adjusted return
%% - calmar_ratio: Return / maximum drawdown ratio
%% - curriculum_risk_penalty: Curriculum learning with risk penalty based on drawdown
%% - phase0_close_trades: Phase 0 - Focus on closing trades without blowing up (strictly positive)
%% - phase1_profit_risk: Phase 1 - Profit optimization with drawdown control (strictly positive)
%% - curriculum_trade_quality_profit: Single curriculum fitness that smoothly transitions from trade activity to profit quality over generations
%% - phase2_profit_optimization: Phase 2 - Aggressive profit optimization with strong risk control for mature agents (strictly positive)
%% - phase_size_reward: Rewards larger neural networks - used in early runs to encourage network growth (strictly positive)
%% ===================================================================

%% ===================================================================
%% Main Dispatch Function
%% ===================================================================
%% Calculates fitness using the function specified in config.
%% Returns: Fitness value (float)
calculate_fitness(State, Account) ->
    FunctionName = config:fitness_function(),
    Generation = config:fitness_curriculum_generation(),
    calculate_fitness(State, Account, FunctionName, Generation).

%% Calculate fitness with explicit function name and generation
calculate_fitness(State, Account, FunctionName, Generation) ->
    case FunctionName of
        time_weighted -> time_weighted(State, Account);
        total_profit -> total_profit(State, Account);
        sharpe_ratio -> sharpe_ratio(State, Account);
        profit_factor -> profit_factor(State, Account);
        total_return -> total_return(State, Account);
        sortino_ratio -> sortino_ratio(State, Account);
        calmar_ratio -> calmar_ratio(State, Account);
        curriculum_risk_penalty -> curriculum_risk_penalty(State, Account, Generation);
        phase0_close_trades -> phase0_close_trades(State, Account);
        phase1_profit_risk -> phase1_profit_risk(State, Account);
        curriculum_trade_quality_profit -> curriculum_trade_quality_profit(State, Account, Generation);
        phase2_profit_optimization -> phase2_profit_optimization(State, Account);
        phase_size_reward -> phase_size_reward(State, Account);
        _ -> 
            io:format("Warning: Unknown fitness function ~p, using time_weighted~n", [FunctionName]),
            time_weighted(State, Account)
    end.

%% ===================================================================
%% Fitness Function: time_weighted
%% ===================================================================
%% Time-weighted fitness with discounts for late profits and bonuses/penalties.
%% This is the current default implementation from fx.erl.
time_weighted(State, Account) ->
    case config:fitness_time_weighted_enabled() of
        false -> 
            total_profit(State, Account);
        true ->
            Starting_Balance = config:account_initial_balance(),
            Discount_Rate = config:fitness_discount_rate(),
            Loss_Discount_Rate = config:fitness_loss_discount_rate(),
            Realized_Bonus = config:fitness_realized_bonus(),
            Loss_Penalty = config:fitness_loss_penalty(),
            Unrealized_Penalty = config:fitness_unrealized_penalty(),
            Realized_By_Cycle = State#state.realized_pl_by_cycle,
            Realized_Weighted = lists:sum([
                case PL >= 0 of
                    true -> PL * (1 - Discount_Rate * Cycle) * Realized_Bonus;
                    false -> PL * (1 + Loss_Discount_Rate * Cycle) * Loss_Penalty
                end
                || {Cycle, PL} <- Realized_By_Cycle
            ]),
            Trades_Bonus = case Realized_By_Cycle of 
                [] -> 0; 
                _ -> config:fitness_trades_bonus() 
            end,
            Starting_Balance + Realized_Weighted + (Account#account.unrealized_PL * Unrealized_Penalty) + Trades_Bonus
    end.

%% ===================================================================
%% Fitness Function: total_profit
%% ===================================================================
%% Simple total profit: balance + unrealized P/L.
%% No time-weighting, bonuses, or penalties.
total_profit(State, Account) ->
    Account#account.balance + Account#account.unrealized_PL.

%% ===================================================================
%% Fitness Function: sharpe_ratio
%% ===================================================================
%% Risk-adjusted return using Sharpe Ratio formula.
%% Sharpe Ratio = (Mean Return - Risk Free Rate) / StdDev(Returns)
%% For trading: uses realized P/L returns per cycle.
%% Higher values indicate better risk-adjusted returns.
sharpe_ratio(State, Account) ->
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    Starting_Balance = config:account_initial_balance(),
    
    case Realized_By_Cycle of
        [] -> 
            % No trades: return small negative value to discourage inactivity
            Starting_Balance - 1.0;
        [_] ->
            % Single trade: cannot calculate Sharpe, use total profit
            total_profit(State, Account);
        _ ->
            % Multiple trades: calculate Sharpe ratio
            Returns = [PL / Starting_Balance || {_, PL} <- Realized_By_Cycle],
            MeanReturn = functions:avg(Returns),
            StdDev = functions:std(Returns),
            RiskFreeRate = 0.0,  % Can be made configurable
            
            case StdDev > 0.0001 of  % Avoid division by zero
                true ->
                    Sharpe = (MeanReturn - RiskFreeRate) / StdDev,
                    % Scale Sharpe ratio to be comparable to profit-based fitness
                    % Multiply by a scaling factor and add to starting balance
                    ScaledFitness = Starting_Balance * (1.0 + Sharpe * 0.1),
                    ScaledFitness + Account#account.unrealized_PL;
                false ->
                    % Low volatility: use total profit
                    total_profit(State, Account)
            end
    end.

%% ===================================================================
%% Fitness Function: profit_factor
%% ===================================================================
%% Profit Factor = Gross Profit / Gross Loss
%% Measures the ratio of total profits to total losses.
%% Values > 1.0 indicate profitable trading.
%% Returns scaled fitness value.
profit_factor(State, Account) ->
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    Starting_Balance = config:account_initial_balance(),
    
    {GrossProfit, GrossLoss} = lists:foldl(
        fun({_, PL}, {ProfitAcc, LossAcc}) ->
            case PL >= 0 of
                true -> {ProfitAcc + PL, LossAcc};
                false -> {ProfitAcc, LossAcc + abs(PL)}
            end
        end,
        {0.0, 0.0},
        Realized_By_Cycle
    ),
    
    case GrossLoss > 0.01 of  % Avoid division by zero
        true ->
            Factor = GrossProfit / GrossLoss,
            % Scale profit factor to fitness value
            % Factor of 2.0 = 100% bonus, Factor of 0.5 = 50% penalty
            ScaledFitness = Starting_Balance * Factor,
            ScaledFitness + Account#account.unrealized_PL;
        false ->
            % No losses or only profits: use total profit
            total_profit(State, Account)
    end.

%% ===================================================================
%% Fitness Function: total_return
%% ===================================================================
%% Percentage return from initial balance.
%% Return % = ((Current Balance - Initial Balance) / Initial Balance) * 100
%% Returns absolute profit scaled by return percentage.
total_return(State, Account) ->
    Starting_Balance = config:account_initial_balance(),
    CurrentBalance = Account#account.balance,
    CurrentTotal = CurrentBalance + Account#account.unrealized_PL,
    
    ReturnPercent = ((CurrentTotal - Starting_Balance) / Starting_Balance) * 100.0,
    
    % Scale by return percentage (encourages higher percentage returns)
    Starting_Balance + (Starting_Balance * ReturnPercent / 100.0).

%% ===================================================================
%% Fitness Function: sortino_ratio
%% ===================================================================
%% Downside deviation-adjusted return (like Sharpe but only penalizes downside).
%% Sortino Ratio = (Mean Return - Risk Free Rate) / DownsideStdDev
%% More appropriate for trading where only losses (not volatility) are penalized.
sortino_ratio(State, Account) ->
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    Starting_Balance = config:account_initial_balance(),
    
    case Realized_By_Cycle of
        [] -> 
            Starting_Balance - 1.0;
        [_] ->
            total_profit(State, Account);
        _ ->
            Returns = [PL / Starting_Balance || {_, PL} <- Realized_By_Cycle],
            MeanReturn = functions:avg(Returns),
            
            % Calculate downside deviation (only negative returns)
            NegativeReturns = [R || R <- Returns, R < 0],
            case length(NegativeReturns) > 0 of
                true ->
                    DownsideDev = functions:std(NegativeReturns),
                    RiskFreeRate = 0.0,
                    
                    case DownsideDev > 0.0001 of
                        true ->
                            Sortino = (MeanReturn - RiskFreeRate) / DownsideDev,
                            ScaledFitness = Starting_Balance * (1.0 + Sortino * 0.1),
                            ScaledFitness + Account#account.unrealized_PL;
                        false ->
                            total_profit(State, Account)
                    end;
                false ->
                    % No negative returns: use total profit (perfect performance)
                    total_profit(State, Account)
            end
    end.

%% ===================================================================
%% Fitness Function: calmar_ratio
%% ===================================================================
%% Return / Maximum Drawdown ratio.
%% Calmar Ratio = Annual Return / Maximum Drawdown
%% Higher values indicate better risk-adjusted returns.
%% Note: This requires tracking drawdown, which we approximate from realized P/L.
calmar_ratio(State, Account) ->
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    Starting_Balance = config:account_initial_balance(),
    
    case Realized_By_Cycle of
        [] -> 
            Starting_Balance - 1.0;
        _ ->
            % Calculate cumulative returns to find maximum drawdown
            CumulativePL = lists:reverse(
                lists:foldl(
                    fun({_, PL}, Acc) -> 
                        case Acc of
                            [] -> [PL];
                            [Last | _] -> [Last + PL | Acc]
                        end
                    end,
                    [],
                    Realized_By_Cycle
                )
            ),
            
            % Find peak and drawdown
            {MaxDrawdown, _} = lists:foldl(
                fun(PL, {MaxDD, Peak}) ->
                    NewPeak = max(Peak, PL),
                    NewDD = case Peak > PL of
                        true -> max(MaxDD, (Peak - PL) / max(Peak, 0.01));
                        false -> MaxDD
                    end,
                    {NewDD, NewPeak}
                end,
                {0.0, 0.0},
                CumulativePL
            ),
            
            TotalReturn = case CumulativePL of
                [] -> 0.0;
                _ -> lists:last(CumulativePL)
            end,
            
            case MaxDrawdown > 0.0001 of
                true ->
                    Calmar = (TotalReturn / Starting_Balance) / MaxDrawdown,
                    ScaledFitness = Starting_Balance * (1.0 + Calmar * 0.1),
                    ScaledFitness + Account#account.unrealized_PL;
                false ->
                    % No drawdown: use total profit
                    total_profit(State, Account)
            end
    end.

%% ===================================================================
%% Fitness Function: curriculum_risk_penalty
%% ===================================================================
%% Curriculum learning fitness with risk penalty based on drawdown.
%% Early generations focus on trade activity, later generations focus on profit.
%% Implements risk penalty based on maximum drawdown from equity curve.
%% 
%% Based on Python implementation with parameters:
%% - k: unrealized discount (default 0.3)
%% - S_P: P&L scale per 1000 steps (default 100.0)
%% - trades_per_1000: desired trades per 1000 steps (default 20.0)
%% - G_trade_focus: generation where profit dominates (default 50)
%% - DD_floor: drawdown tolerated (default 0.10 = 10%)
%% - lam: drawdown penalty strength (default 3.0)
curriculum_risk_penalty(State, Account) ->
    curriculum_risk_penalty(State, Account, config:fitness_curriculum_generation()).

curriculum_risk_penalty(State, Account, Generation) ->
    % Parameters (configurable via config.erl)
    K = config:fitness_curriculum_unrealized_discount(),              % k = 0.3
    S_P = config:fitness_curriculum_pnl_scale(),                      % S_P = 100.0
    Trades_Per_1000 = config:fitness_curriculum_trades_per_1000(),    % 20.0
    G_Trade_Focus = config:fitness_curriculum_generation_focus(),     % 50
    DD_Floor = config:fitness_curriculum_drawdown_floor(),            % 0.10
    Lambda = config:fitness_curriculum_drawdown_penalty(),            % 3.0
    
    Starting_Balance = config:account_initial_balance(),
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    Num_Trades = length(Realized_By_Cycle),
    T = State#state.cycle,  % Episode length (total cycles)
    
    % Handle empty trades case
    case Num_Trades == 0 andalso T == 0 of
        true ->
            % No trading activity - return negative fitness
            -1.0;
        false ->
            Realized_PnL = lists:sum([PL || {_, PL} <- Realized_By_Cycle]),
            Unrealized_PnL = Account#account.unrealized_PL,
            
            % Step 1: Normalize PnL per 1000 steps
            Scale_T = case T > 0 of
                true -> T / 1000.0;
                false -> 1.0  % Avoid division by zero
            end,
            P_Eff = Realized_PnL + K * Unrealized_PnL,
            P_Norm = case Scale_T > 0.0001 of
                true -> P_Eff / Scale_T;
                false -> P_Eff
            end,
            
            % Squash with tanh
            P_Clipped = max(min(P_Norm, 3*S_P), -3*S_P),
            P_Tanh = math:tanh(P_Clipped / S_P),  % in [-1, 1]
            
            % Step 2: TradeScore
            N_Target = Trades_Per_1000 * Scale_T,
            TradeScore = case Num_Trades == 0 of
                true -> -1.0;
                false ->
                    Ratio = min(Num_Trades, N_Target) / max(N_Target, 0.001),
                    2.0 * Ratio - 1.0  % in (-1, 1]
            end,
            
            % Step 3: Curriculum over generations
            G_Float = float(Generation),
            G_Focus_Float = float(G_Trade_Focus),
            Alpha = max(0.0, 1.0 - G_Float / max(G_Focus_Float, 0.001)),
            Beta = 1.0 - Alpha,
            
            Base_Fitness = Alpha * TradeScore + Beta * P_Tanh,
            
            % Optional small extra penalty for no trades
            Base_Fitness_Adjusted = case Num_Trades == 0 of
                true -> Base_Fitness - 0.2;
                false -> Base_Fitness
            end,
            
            % Step 4: Risk penalty from equity curve
            Equity_Curve = build_equity_curve(State, Account, Starting_Balance),
            
            Risk_Penalty = case Equity_Curve of
                [] -> 0.0;  % No equity data
                [E_First | _] ->
                    {E_Max, DD_Max} = calculate_max_drawdown(Equity_Curve, E_First, E_First, 0.0),
                    E_End = lists:last(Equity_Curve),
                    
                    DD_Pct = case E_Max > 0.001 of
                        true -> DD_Max / E_Max;
                        false -> 1.0  % bad - zero or negative equity
                    end,
                    
                    Excess_DD = max(0.0, DD_Pct - DD_Floor),
                    math:exp(-Lambda * Excess_DD)
            end,
            
            Fitness_Final = Base_Fitness_Adjusted * Risk_Penalty,
            
            Fitness_Final + 300.0
    end.

%% Helper function to build equity curve from state
build_equity_curve(State, Account, Starting_Balance) ->
    Realized_By_Cycle = State#state.realized_pl_by_cycle,
    
    % Sort by cycle to process in order
    Sorted_Trades = lists:sort(Realized_By_Cycle),
    
    % Build cumulative equity at each cycle
    {Equity_At_Trades, FinalCumulativePL} = lists:foldl(
        fun({Cycle, PL}, {Acc, CumulativePL}) ->
            NewCumulativePL = CumulativePL + PL,
            NewEquity = Starting_Balance + NewCumulativePL,
            {[{Cycle, NewEquity} | Acc], NewCumulativePL}
        end,
        {[], 0.0},
        Sorted_Trades
    ),
    
    % Get final equity (current balance + unrealized)
    Final_Equity = Account#account.balance + Account#account.unrealized_PL,
    Current_Equity = Starting_Balance + FinalCumulativePL + Account#account.unrealized_PL,
    
    % Build equity curve: start + equity at each trade + current
    Equity_Curve_Values = case Equity_At_Trades of
        [] -> 
            % No trades - just start and end
            [Starting_Balance, Current_Equity];
        _ ->
            % Start with initial balance
            Equity_List = [Starting_Balance] ++ [Equity || {_, Equity} <- lists:reverse(Equity_At_Trades)],
            % Add current equity at end
            Equity_List ++ [Current_Equity]
    end,
    
    Equity_Curve_Values.

%% Helper function to calculate maximum drawdown
calculate_max_drawdown([], _E_Max, _Peak, DD_Max) ->
    {_E_Max, DD_Max};
calculate_max_drawdown([E | Equity_Curve], E_Max, Peak, DD_Max) ->
    New_E_Max = max(E_Max, E),
    New_Peak = max(Peak, E),
    DD = New_Peak - E,
    New_DD_Max = max(DD_Max, DD),
    calculate_max_drawdown(Equity_Curve, New_E_Max, New_Peak, New_DD_Max).


%% =========================================================
%% Small helpers
%% =========================================================
clamp(X, Lo, Hi) when X < Lo -> Lo;
clamp(X, Lo, Hi) when X > Hi -> Hi;
clamp(X, _Lo, _Hi) -> X.

to_01(Core) ->
    %% Core is intended in [-1,1]. Clamp anyway so Score01 is in [0,1].
    CoreC = clamp(Core, -1.0, 1.0),
    0.5 * (CoreC + 1.0).

%% =========================================================
%% Phase 0 Fitness: Close Trades (STRICTLY POSITIVE)
%% =========================================================
phase0_close_trades(State, Account) ->
    MinFitness = 1.0,
    Scale      = 1000.0,

    Trades    = State#state.realized_pl_by_cycle,
    NumTrades = length(Trades),
    T         = max(State#state.cycle, 1),
    ScaleT    = T / 1000.0,

    TargetTradesPer1000    = 50.0,
    OvertradeThreshPer1000 = 150.0,
    LossFloorPct           = 0.03,

    %% Trade completion score [-1,1]
    NTarget = max(TargetTradesPer1000 * ScaleT, 0.001),
    Ratio   = min(NumTrades, NTarget) / NTarget,
    TradeScore0 = 2.0 * Ratio - 1.0,

    %% Penalize no-trade case but CLAMP back into [-1,1]
    TradeScore =
        case NumTrades of
            0 -> clamp(TradeScore0 - 0.5, -1.0, 1.0);
            _ -> TradeScore0
        end,

    %% Overtrade penalty (0,1]
    TradesPer1000 = NumTrades / max(ScaleT, 0.001),
    ExcessOT      = max(0.0, TradesPer1000 - OvertradeThreshPer1000),
    OvertradePenalty = math:exp(-0.05 * ExcessOT),

    %% Loss guard
    SB = config:account_initial_balance(),
    RealizedPnL = lists:sum([PL || {_C, PL} <- Trades]),
    LossFloor   = -LossFloorPct * SB,
    LossPenalty =
        case RealizedPnL < LossFloor of
            true  -> 0.2;
            false -> 1.0
        end,

    %% Small unrealized shaping term (bounded)
    UnrealTerm = math:tanh((0.1 * Account#account.unrealized_PL) / max(SB, 1.0)),

    Core    = 0.90 * TradeScore + 0.10 * UnrealTerm,  % may drift, so clamp in to_01/1
    Score01 = to_01(Core),

    %% Strictly positive
    MinFitness + Scale * (Score01 * OvertradePenalty * LossPenalty).


%% =========================================================
%% Phase 1 Fitness: Profit + Drawdown Control (STRICTLY POSITIVE)
%% =========================================================
phase1_profit_risk(State, Account) ->
    MinFitness = 1.0,
    Scale      = 1000.0,

    SB       = config:account_initial_balance(),
    Trades   = lists:sort(State#state.realized_pl_by_cycle),
    NumTrades = length(Trades),
    T        = max(State#state.cycle, 1),
    ScaleT   = T / 1000.0,

    UnrealizedDiscount      = 0.3,
    PnLScale                = 100.0,
    TargetTradesPer1000     = 50.0,
    OvertradeThreshPer1000  = 150.0,
    DrawdownFloorPct        = 0.10,
    DrawdownLambda          = 4.0,

    %% PnL score [-1,1]
    RealizedPnL = lists:sum([PL || {_C, PL} <- Trades]),
    UnrealPnL   = Account#account.unrealized_PL,
    %% Penalize unrealized P/L more when no trades (encourage closing trades)
    UnrealizedDiscountAdjusted = case NumTrades of
        0 -> 0.1;  % Much lower discount for unrealized when no trades
        _ -> UnrealizedDiscount
    end,
    PEff        = RealizedPnL + UnrealizedDiscountAdjusted * UnrealPnL,

    PNorm = PEff / max(ScaleT, 0.001),
    PClip = max(min(PNorm, 3.0 * PnLScale), -3.0 * PnLScale),
    PScore = math:tanh(PClip / PnLScale),

    %% Trade activity encouragement [-1,1] (clamped)
    NTarget = max(TargetTradesPer1000 * ScaleT, 0.001),
    Ratio   = min(NumTrades, NTarget) / NTarget,
    TradeScore0 = 2.0 * Ratio - 1.0,
    TradeScore =
        case NumTrades of
            0 -> clamp(TradeScore0 - 0.5, -1.0, 1.0);  % Stronger penalty for no trades
            _ -> TradeScore0
        end,

    %% Drawdown penalty
    EquityCurve = build_equity_curve(State, Account, SB),
    MaxDDPct    = max_drawdown_pct(EquityCurve),
    ExcessDD    = max(0.0, MaxDDPct - DrawdownFloorPct),
    RiskPenalty = math:exp(-DrawdownLambda * ExcessDD),

    %% Overtrade penalty
    TradesPer1000 = NumTrades / max(ScaleT, 0.001),
    ExcessOT = max(0.0, TradesPer1000 - OvertradeThreshPer1000),
    OvertradePenalty = math:exp(-0.08 * ExcessOT),

    %% Weighted combination of PnL score and trade activity (configurable via config.erl)
    PScoreWeight = config:fitness_phase1_pscore_weight(),
    TradeScoreWeight = config:fitness_phase1_tradescore_weight(),
    Core    = PScoreWeight * PScore + TradeScoreWeight * TradeScore,
    Score01 = to_01(Core),

    MinFitness + Scale * (Score01 * RiskPenalty * OvertradePenalty).


%% =========================================================
%% Curriculum Trade Quality Profit Fitness (STRICTLY POSITIVE)
%% =========================================================
%% Single curriculum fitness function that smoothly transitions behavior over generations.
%% Early generations: Focus on trade activity and closing trades.
%% Later generations: Focus on profit quality, big wins, and risk control.
curriculum_trade_quality_profit(State, Account) ->
    Generation = config:fitness_curriculum_generation(),
    curriculum_trade_quality_profit(State, Account, Generation).

curriculum_trade_quality_profit(State, Account, Generation) ->
    MinFitness = 1.0,
    Scale = 1000.0,
    
    %% Get starting balance from config (not hard-coded)
    SB = config:account_initial_balance(),
    
    %% Extract trade data
    Trades = lists:sort(State#state.realized_pl_by_cycle),
    N = length(Trades),
    T = max(State#state.cycle, 1),
    ScaleT = T / 1000.0,
    
    %% =========================================================
    %% 1. Compute Trade Activity Score (target 50/1000, saturating)
    %% =========================================================
    TargetTradesPer1000 = config:fitness_target_trades_per_1000(),
    NTarget = max(TargetTradesPer1000 * ScaleT, 0.001),
    Ratio = min(N, NTarget) / NTarget,
    TradeScore0 = 2.0 * Ratio - 1.0,  %% in [-1,1]
    
    %% Apply no-trade penalty
    NoTradePenalty = config:fitness_no_trade_penalty(),
    TradeScore = case N == 0 of
        true -> clamp(TradeScore0 - NoTradePenalty, -1.0, 1.0);
        false -> TradeScore0
    end,
    
    %% =========================================================
    %% 2. Compute Win Rate and Dominance Scores
    %% =========================================================
    NPos = length([PL || {_C, PL} <- Trades, PL > 0]),
    NNeg = length([PL || {_C, PL} <- Trades, PL < 0]),
    WinRate = NPos / max(N, 1),  %% in [0,1]
    WinRateScore = 2.0 * WinRate - 1.0,  %% in [-1,1]
    
    %% Dominance score: tanh((NPos - NNeg) / DomScale)
    DomScale = config:fitness_dom_scale(),
    DominanceScore = math:tanh((NPos - NNeg) / max(DomScale, 0.001)),  %% in [-1,1]
    
    %% Combine win rate and dominance
    WinCombo = 0.6 * WinRateScore + 0.4 * DominanceScore,  %% in [-1,1]
    
    %% =========================================================
    %% 3. Overtrade Penalty (reward less if too many trades)
    %% =========================================================
    TradesPer1000 = N / max(ScaleT, 0.001),
    OvertradeThreshPer1000 = config:fitness_overtrade_thresh_per_1000(),
    OvertradeLambda = config:fitness_overtrade_lambda(),
    ExcessOT = max(0.0, TradesPer1000 - OvertradeThreshPer1000),
    OvertradePenalty = math:exp(-OvertradeLambda * ExcessOT),  %% in (0,1]
    
    %% =========================================================
    %% 4. Big Win Metrics (large winners + many large winners)
    %% =========================================================
    BigWinPct = config:fitness_bigwin_pct(),
    BigWinThreshold = BigWinPct * SB,
    BigWins = [PL || {_C, PL} <- Trades, PL >= BigWinThreshold],
    BigWinCount = length(BigWins),
    BigWinSum = lists:sum(BigWins),
    
    %% Big win value score
    %% BigWinSumScale is a multiplier: default 1.0 means use SB * BigWinPct as base scale
    %% Higher values (e.g., 2.0) increase the scale, making it harder to saturate tanh
    BigWinSumScale = config:fitness_bigwin_sum_scale(),
    BaseBigWinSumScale = SB * BigWinPct,
    EffectiveBigWinSumScale = case BigWinSumScale > 0.001 of
        true -> BigWinSumScale * BaseBigWinSumScale;
        false -> BaseBigWinSumScale
    end,
    BigWinValueScore = math:tanh(BigWinSum / max(EffectiveBigWinSumScale, 0.001)),  %% in [-1,1]
    
    %% Big win count score
    TargetBigWinsPer1000 = config:fitness_target_bigwins_per_1000(),
    BigWinCountTarget = max(TargetBigWinsPer1000 * ScaleT, 0.001),
    BigWinCountRatio = min(BigWinCount, BigWinCountTarget) / BigWinCountTarget,
    BigWinCountScore = 2.0 * BigWinCountRatio - 1.0,  %% in [-1,1]
    
    %% =========================================================
    %% 5. PnL Score (ultimate objective)
    %% =========================================================
    RealizedPnL = lists:sum([PL || {_C, PL} <- Trades]),
    UnrealPnL = Account#account.unrealized_PL,
    KUnreal = config:fitness_curriculum_unrealized_discount(),
    
    %% Reduce unreal discount if no trades to discourage never closing
    KUnrealAdjusted = case N == 0 of
        true -> config:fitness_unreal_discount_no_trades();
        false -> KUnreal
    end,
    
    PEff = RealizedPnL + KUnrealAdjusted * UnrealPnL,
    PNorm = PEff / max(ScaleT, 0.001),
    PnLScale = config:fitness_curriculum_pnl_scale(),
    PClip = clamp(PNorm, -3.0 * PnLScale, 3.0 * PnLScale),
    PnLScore = math:tanh(PClip / max(PnLScale, 0.001)),  %% in [-1,1]
    
    %% =========================================================
    %% 6. Risk Penalty (drawdown), increasing later in curriculum
    %% =========================================================
    EquityCurve = build_equity_curve(State, Account, SB),
    MaxDDPct = max_drawdown_pct(EquityCurve),
    DDFloor = config:fitness_curriculum_drawdown_floor(),
    DDLambdaEarly = config:fitness_dd_lambda_early(),
    DDLambdaLate = config:fitness_dd_lambda_late(),
    
    %% Ramp DDLambda from early to late with generation
    G1 = config:fitness_curriculum_g1(),
    G2 = config:fitness_curriculum_g2(),
    G = float(Generation),
    R2 = clamp((G - float(G1)) / max(float(G2 - G1), 1.0), 0.0, 1.0),
    DDLambda = DDLambdaEarly * (1.0 - R2) + DDLambdaLate * R2,
    
    ExcessDD = max(0.0, MaxDDPct - DDFloor),
    RiskPenalty = math:exp(-DDLambda * ExcessDD),  %% in (0,1]
    
    %% =========================================================
    %% 7. Curriculum Schedule (weights change with generation)
    %% =========================================================
    %% Define ramps
    R1 = clamp(G / max(float(G1), 1.0), 0.0, 1.0),  %% progress into "middle"
    R2 = clamp((G - float(G1)) / max(float(G2 - G1), 1.0), 0.0, 1.0),  %% progress into "late"
    
    %% Set weights (early emphasizes trading, later emphasizes PnL and big wins)
    W_Trade = 0.75 * (1.0 - R1) + 0.10 * R1,  %% ends near 0.10
    W_Win = 0.20 * (1.0 - R1) + 0.20 * R1,  %% keep ~0.20 baseline
    W_Big = 0.00 * (1.0 - R2) + 0.25 * R2,
    W_Pnl = 0.05 * (1.0 - R2) + 0.45 * R2,
    
    %% =========================================================
    %% 8. Ordering/gating so later terms don't dominate too early
    %% =========================================================
    GateTrade = to_01(TradeScore),  %% in [0,1]
    GateWin = to_01(WinRateScore),  %% in [0,1]
    
    %% Effective late terms (gated)
    PnLScoreEff = GateTrade * PnLScore,
    BigWinScoreEff = GateTrade * GateWin * (0.5 * BigWinCountScore + 0.5 * BigWinValueScore),
    
    %% =========================================================
    %% 9. Core combination + final fitness (strictly positive)
    %% =========================================================
    Core = W_Trade * TradeScore + W_Win * WinCombo + W_Big * BigWinScoreEff + W_Pnl * PnLScoreEff,
    Score01 = to_01(Core),
    
    %% Apply penalties
    Penalty = OvertradePenalty * RiskPenalty,
    
    %% Final fitness (strictly positive)
    MinFitness + Scale * (Score01 * Penalty).


%% =========================================================
%% Phase 2 Fitness: Profit Optimization (STRICTLY POSITIVE)
%% =========================================================
%% Aggressive profit optimization for mature agents that already know how to trade.
%% Focus: Maximum P/L with strong risk control, big win rewards, minimal trade count emphasis.
phase2_profit_optimization(State, Account) ->
    MinFitness = 1.0,
    Scale = 1000.0,
    
    SB = config:account_initial_balance(),
    Trades = lists:sort(State#state.realized_pl_by_cycle),
    NumTrades = length(Trades),
    T = max(State#state.cycle, 1),
    ScaleT = T / 1000.0,
    
    %% Parameters - optimized for profit focus
    UnrealizedDiscount = 0.2,  % Lower discount - prefer realized profits
    PnLScale = 150.0,  % Higher scale to allow larger P/L values
    DrawdownFloorPct = 0.08,  % Tighter drawdown tolerance (8%)
    DrawdownLambda = 6.0,  % Stronger drawdown penalty
    BigWinPct = 0.01,  % 1% of SB for big win threshold
    OvertradeThreshPer1000 = 200.0,  % Allow more trades but still penalize excess
    
    %% PnL score [-1,1] - PRIMARY FOCUS
    RealizedPnL = lists:sum([PL || {_C, PL} <- Trades]),
    UnrealPnL = Account#account.unrealized_PL,
    %% Strongly penalize unrealized when no trades
    UnrealizedDiscountAdjusted = case NumTrades of
        0 -> 0.05;  % Very low discount for unrealized when no trades
        _ -> UnrealizedDiscount
    end,
    PEff = RealizedPnL + UnrealizedDiscountAdjusted * UnrealPnL,
    
    PNorm = PEff / max(ScaleT, 0.001),
    PClip = clamp(PNorm, -3.0 * PnLScale, 3.0 * PnLScale),
    PScore = math:tanh(PClip / max(PnLScale, 0.001)),  %% in [-1,1]
    
    %% Big win bonus - reward large profitable trades
    BigWinThreshold = BigWinPct * SB,
    BigWins = [PL || {_C, PL} <- Trades, PL >= BigWinThreshold],
    BigWinSum = lists:sum(BigWins),
    BigWinBonus = math:tanh(BigWinSum / max(SB * 0.02, 0.001)),  %% Normalize by 2% of SB
    
    %% Win rate bonus - reward consistency
    NPos = length([PL || {_C, PL} <- Trades, PL > 0]),
    WinRate = case NumTrades > 0 of
        true -> NPos / NumTrades;
        false -> 0.0
    end,
    WinRateBonus = 2.0 * WinRate - 1.0,  %% in [-1,1]
    
    %% Minimal trade activity score - just ensure some trading happens
    MinTradesPer1000 = 20.0,  % Lower minimum - agents already know how to trade
    NMinTarget = max(MinTradesPer1000 * ScaleT, 0.001),
    TradeActivityScore = case NumTrades == 0 of
        true -> -0.3;  % Small penalty for no trades
        false ->
            case NumTrades < NMinTarget of
                true -> (NumTrades / NMinTarget) * 0.2 - 0.1;  %% Small bonus for meeting minimum
                false -> 0.1  %% Small positive score for adequate trading
            end
    end,
    
    %% Drawdown penalty - STRONG
    EquityCurve = build_equity_curve(State, Account, SB),
    MaxDDPct = max_drawdown_pct(EquityCurve),
    ExcessDD = max(0.0, MaxDDPct - DrawdownFloorPct),
    RiskPenalty = math:exp(-DrawdownLambda * ExcessDD),  %% in (0,1]
    
    %% Overtrade penalty - moderate
    TradesPer1000 = NumTrades / max(ScaleT, 0.001),
    ExcessOT = max(0.0, TradesPer1000 - OvertradeThreshPer1000),
    OvertradePenalty = math:exp(-0.05 * ExcessOT),  %% in (0,1]
    
    %% Weighted combination - HEAVY on P/L (85%), bonuses (10%), minimal trade activity (5%)
    Core = 0.85 * PScore + 0.10 * (0.6 * BigWinBonus + 0.4 * WinRateBonus) + 0.05 * TradeActivityScore,
    Score01 = to_01(Core),
    
    %% Final fitness (strictly positive)
    MinFitness + Scale * (Score01 * RiskPenalty * OvertradePenalty).


%% =========================================================
%% Helper: Maximum Drawdown Percentage (>= 0)
%% =========================================================
max_drawdown_pct([]) ->
    1.0;
max_drawdown_pct([E0 | Rest]) ->
    max_drawdown_pct(Rest, E0, 0.0).

max_drawdown_pct([], _Peak, MaxDDPct) ->
    MaxDDPct;
max_drawdown_pct([E | Rest], Peak, MaxDDPct) ->
    NewPeak = max(Peak, E),
    DD      = NewPeak - E,
    DDPct   =
        case NewPeak > 0.001 of
            true  -> DD / NewPeak;
            false -> 1.0
        end,
    max_drawdown_pct(Rest, NewPeak, max(MaxDDPct, DDPct)).


%% =========================================================
%% Phase Size Reward Fitness: Reward Larger Networks (STRICTLY POSITIVE)
%% =========================================================
%% Fitness function designed to encourage larger neural networks in early evolution.
%% For run 1: Returns very small base fitness so size_proportional postprocessor makes 
%%           neuron count the primary selection factor (divides by size^0.01, but since
%%           base is tiny, larger networks win).
%% For runs 2-5: Gradually increases base fitness component to transition away from pure size focus.
phase_size_reward(State, Account) ->
    % Always calculate performance-based fitness regardless of weight
    % The size_first postprocessor handles size-based sorting
    % Weight parameter is ignored - fitness always includes performance variation
    
    Trades    = State#state.realized_pl_by_cycle,
    NumTrades = length(Trades),
    T         = max(State#state.cycle, 1),
    ScaleT    = T / 1000.0,

    TargetTradesPer1000    = 30.0,
    OvertradeThreshPer1000 = 150.0,
    LossFloorPct           = 0.10,

    %% Trade completion score [-1,1]
    NTarget = max(TargetTradesPer1000 * ScaleT, 0.001),
    Ratio   = min(NumTrades, NTarget) / NTarget,
    TradeScore0 = 2.0 * Ratio - 1.0,
    TradeScore = case NumTrades of
        0 -> clamp(TradeScore0 - 0.1, -1.0, 1.0);
        _ -> TradeScore0
    end,

    %% Overtrade penalty - lenient
    TradesPer1000 = NumTrades / max(ScaleT, 0.001),
    ExcessOT      = max(0.0, TradesPer1000 - OvertradeThreshPer1000),
    OvertradePenalty = math:exp(-0.01 * ExcessOT),

    %% Loss guard - lenient
    SB = config:account_initial_balance(),
    RealizedPnL = lists:sum([PL || {_C, PL} <- Trades]),
    LossFloor   = -LossFloorPct * SB,
    LossPenalty = case RealizedPnL < LossFloor of
        true  -> 0.5;
        false -> 1.0
    end,

    %% Small unrealized term
    UnrealTerm = math:tanh((0.1 * Account#account.unrealized_PL) / max(SB, 1.0)),
    Core    = 0.80 * TradeScore + 0.20 * UnrealTerm,
    Score01 = to_01(Core),

    %% Base fitness with performance component
    % Base fitness ensures all values are positive and above 1.0
    % Performance component adds variation based on trading results
    % The size_first postprocessor will sort by neuron count primarily
    BaseFitness = 1.0,
    PerformanceComponent = Score01 * OvertradePenalty * LossPenalty,
    
    % Return fitness that varies based on performance
    % Size-based sorting is handled by the postprocessor
    BaseFitness + (BaseFitness * PerformanceComponent).
