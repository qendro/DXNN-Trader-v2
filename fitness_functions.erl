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
    SB = 10000.0,  %% MUST match your sim's initial balance
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

    SB       = 10000.0, %% MUST match your sim's initial balance
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
