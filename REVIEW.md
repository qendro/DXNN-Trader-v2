# DXNN-Trader training core: design review

Branch `fix/training-core` at `45160b9`. Read-only review: nothing in the repository was changed except adding this file. Measurements were run on `fx_tables/EURUSD1.txt` and on scratch copies of the code outside the repository (methods in Appendix A). Claims I could not check are tagged **[unverified]**; literature is cited from memory and listed in Appendix B.

## 1. Verdict

The system cannot produce an agent that is profitable out of sample as it is designed today, and network size is a symptom, not the cause. Three structural facts explain the history of "no profitable run, 1 to 8 neurons", and each one is measured below.

1. **Fitness on a single window rewards a direction bet, not a skill.** On ten of the twelve training windows, opening one position and never closing it earns $11 to $30 on a $300 account (the other two earn $2 to $3), while a trade costs $0.64. I ran the real system on the phase-1 window: a randomly initialised agent in generation 0 already scored 318.69 against an always-short value of 318.26, and by generation 1 the population average was 315.3. One neuron with a bias is the global optimum of the current objective. Nothing larger can beat it, so nothing larger survives.
2. **The decision problem sits on the wrong side of the cost wall.** Deciding every 1-minute bar at 1.5 pips round trip needs a hit rate above 100% at a 1-bar hold and 60.5% at a 60-bar hold. A walk-forward linear model on engineered features finds a gross edge indistinguishable from zero at every intraday horizon (within ±0.6 pips per trade, |t| ≤ 0.8). There is very little to learn at this timeframe, and what little exists is 30 times smaller than the cost.
3. **Selection cannot tell luck from skill.** Training uses 1.22% of the table (8 to 19 September 2025), the out-of-sample check is 200 bars on a Friday evening (the quietest hours of the week), and each agent's fitness is the maximum over 10 to 20+ weight perturbations scored on the same window. Across windows, an agent's rank is explained mostly by how rarely it trades.

The seven changes below attack these in order of leverage. The first is a prerequisite for measuring anything else.

| # | Change | Replaces | What it buys |
|---|---|---|---|
| 1 | Drift-neutral, multi-window, risk-adjusted objective; no best-of-N on the same data | `total_profit` on one window; max over tuning attempts | Fitness that measures timing skill and generalises |
| 2 | 15-minute decision clock, holds of hours, 1-minute data used only for fills | A decision on every 1-minute bar | Moves the problem to where cost is 5 to 20% of a typical move instead of 170% |
| 3 | A causal, stationary feature tape (multi-timeframe, volatility regime, session) | 90/270 raw closes and a 30×20 chart grid | 3 to 20× fewer inputs; exposes the only strongly predictable structure in the data |
| 4 | Exposure output with hysteresis, minimum hold, scape-enforced risk; later a risk actuator | Trinary long/flat/short re-decided every bar at fixed $5,000 notional | Stops churn, which is the dominant loss today; makes sizing and exits learnable |
| 5 | Validation-gated curriculum: direction at zero cost, then cost annealing, risk, robustness | 12 fixed-length phases on overlapping recent windows | Each phase is the final objective under easier conditions; a failed gate is a result |
| 6 | Neural encoding by default, rank selection, neutral structural mutation, behaviour niches, population ~150 | Substrate default, allotment divided by neuron count, one species of 12 to 48 | Structure can grow where validation says it helps; 7× more evaluations per hour |
| 7 | Hall of fame and an ensemble of diverse champions; flat evaluator as the enabler | Whole-population cloning between phases; single champion | Variance reduction, a confidence signal for sizing, fewer flips |

Odds, stated up front: for EURUSD only, price only, at 1.5 pips, I put the chance of a real, durable net edge from this system at roughly 5 to 10%, and close to zero for holds under an hour. Section 7 says what moves that number.

## 2. Evidence

All numbers in this section were measured, not assumed.

### 2.1 Where training and testing happen

| | Bars back from last row | Dates (timestamps as stored, apparently UTC) |
|---|---|---|
| All 12 training windows | 13,000 to 1,800 | 2025-09-08 13:35 to 2025-09-18 14:44 |
| Out-of-sample check | last 200 | Friday 2025-09-19 17:39 to 20:59 |
| Whole table | 1,062,197 | 2022-09-21 to 2025-09-19 (about 781 trading days) |

Training touches 1.22% of the table. The windows overlap heavily (phase 7 re-covers phases 4 to 6). The test window sits in hours 17 to 20, where the mean 1-minute move is 0.5 to 0.9 pips against 1.5 pips at 13:00 to 14:00; always-long makes −$2.06 there and always-short +$0.79, so any test score is a rounding error. The volume column is −1.0 on every row.

### 2.2 What a constant position earns on each training window

Simulator sizing ($100 × 50 leverage, about 4,270 units), one 1.5-pip entry paid.

| Phase | Bars | Drift (pips) | Always long ($) | Always short ($) |
|---|---|---|---|---|
| 1 | 800 | −44.6 | −19.53 | **+18.26** |
| 2 | 1,000 | −32.1 | −14.20 | **+12.94** |
| 3 | 1,600 | +47.3 | **+19.41** | −20.69 |
| 4 | 1,600 | +70.8 | **+29.45** | −30.72 |
| 5 | 1,800 | +44.8 | **+18.48** | −19.76 |
| 6 | 2,000 | −9.6 | −4.75 | +3.47 |
| 7 | 1,800 | +50.9 | **+21.06** | −22.34 |
| 8 | 2,000 | +41.3 | **+16.99** | −18.27 |
| 9 | 2,400 | +36.4 | **+14.94** | −16.23 |
| 10 | 2,800 | +6.7 | +2.22 | −3.50 |
| 11 | 3,200 | −28.6 | −12.77 | **+11.49** |
| 12 | 3,600 | −61.9 | −26.97 | **+25.69** |

The winning side flips four times across the curriculum, so what the phases teach is to re-learn a bias sign. Over the whole table, the absolute drift of a random 3,600-bar window averages $28.77 (median $20.84); one entry costs $0.64 to $0.68.

Confirmation from the real code (scratch copy, phase-1 window, 12 agents, 3 generations, default substrate encoding): specie average fitness 303.8 → 315.3 → 315.2, maximum 318.69 in every generation including generation 0, average CPPN size 1.27 neurons. 84% of evaluations closed no trade at all: 6% never traded and 78% opened one position and held it to the end. The neural-encoding run reached the same 318.7 ceiling.

### 2.3 Cost against movement

Close-to-close, non-overlapping bar-count horizons. Break-even hit rate assumes symmetric wins and losses of average size.

| Hold (1-min bars) | Mean abs move (pips) | Break-even hit rate at 1.5 pips | at 0.7 | at 0.3 |
|---|---|---|---|---|
| 1 | 0.89 | 134.5% | 89.5% | 66.9% |
| 15 | 3.56 | 71.1% | 59.8% | 54.2% |
| 60 | 7.12 | 60.5% | 54.9% | 52.1% |
| 240 | 14.99 | 55.0% | 52.3% | 51.0% |
| 1,440 | 40.04 | 51.9% | 50.9% | 50.4% |

(Your figures of 1.09 and 8.89 pips are a little higher than mine; a different averaging method would explain it and the conclusion is the same.)

Predictable structure in this table is almost entirely in volatility, not direction: lag-1 autocorrelation of returns is −0.001 to −0.014 at every bar size from 1 to 240 minutes, while autocorrelation of absolute returns is 0.30 at 1 minute, 0.23 at 60 minutes and 0.15 to 0.17 at a one-day lag. The hourly profile of 1-minute movement spans a factor of 4.5 (0.36 pips at 21:00, 1.59 at 14:00).

### 2.4 Baselines under the simulator's rules

| Experiment | Result |
|---|---|
| Random trinary action every bar, 1,000 bars, 1.5 pips | 445 trades, −$308 (the whole account) |
| Same, action redrawn 1% of bars | 4.6 trades, −$3.21 (std $16.6) |
| Random single-neuron agents on the 90-bar price list (neural gen-0 analogue), 3,600 bars | median 293 trades; none of 2,000 breaks even; best in sample −$48.79 |
| Same at zero spread: best of 2,000 in sample, then scored on 20 other windows | +$74.02 in sample, +$6.63 elsewhere (about 0.04 pips per trade; not significance-tested) |
| Rank agreement of 200 random agents between two windows, 1.5 pips | Spearman 0.39 to 0.72, driven by trade count, i.e. by cost paid |
| Neural-encoding agents in the real code, 800 bars, 3 generations | 0 to 442 trades per evaluation; 44% of evaluations exactly $300 (never traded) |

So the two attractors are "never trade" and "trade once and hold", and the selection gradient from a random start points at "trade less", not "trade better".

### 2.5 Is there a learnable edge? Linear walk-forward baseline

Ridge regression on 19 causal features (ten volatility-scaled past returns, volatility regime and level, range position, distance from moving averages, time of day), expanding window, five out-of-sample folds over the last half of the table, trading the top 30% of predictions by magnitude.

| Bar (min) | Hold (bars) | OOS correlation | Trades | Gross pips per trade (t) | Net at 1.5 | Net at 0.3 |
|---|---|---|---|---|---|---|
| 1 | 15 | 0.011 | 10,604 | +0.05 (0.8) | −1.45 | −0.25 |
| 1 | 60 | 0.006 | 2,646 | +0.15 (0.6) | −1.35 | −0.15 |
| 5 | 12 | 0.005 | 2,532 | −0.16 (−0.6) | −1.66 | −0.46 |
| 15 | 4 | −0.005 | 2,603 | +0.12 (0.5) | −1.38 | −0.18 |
| 15 | 16 | −0.002 | 611 | −0.57 (−0.6) | −2.07 | −0.87 |
| 60 | 4 | 0.005 | 660 | −0.25 (−0.3) | −1.75 | −0.55 |
| 60 | 24 | 0.048 | 105 | +7.36 (1.2) | +5.86 | +7.06 |

No cell is statistically significant. The only positive one is the one-day hold, on 105 trades with t = 1.2, in a period when EURUSD trended from about 1.03 to 1.17. This is a linear model with a small feature set, so it bounds what is easy to find, not what exists. A gradient-boosted model on the same features is the proper reference and is part of the protocol in section 5.

### 2.6 Throughput

Three generations, 12 agents, 800 bars, 10 cores: substrate encoding about 4,800 bar-steps per second in aggregate (1,429 µs per bar per agent), neural encoding about 35,000 (125 µs). A single-process loop in another language evaluated the same single-neuron policy at several million bars per second per core, so the process-per-neuron phenotype costs two to three orders of magnitude. At the measured substrate rate, today's phase 12 alone (48 agents × at least 21 evaluations × 3,600 bars × 100 generations) is about 20 hours.

### 2.7 The reference design never showed average out-of-sample profit either

The bundled Handbook (chapter 19, Table 1) uses 15-minute EURUSD bars, 800 training and 200 test steps, and the same $300 and 0.00015 spread. Across 14 sensor configurations the test average ranges from 230 to 310 around the $300 start; buy and hold scores 300. This port kept the spread and moved to 1-minute bars, which makes the cost-to-move ratio about four times worse.

## 3. The seven changes

### Change 1. Score timing skill across many windows, and stop taking the best of N on the same data

**Idea.** For each evaluation episode, score the part of P&L that comes from *when* the agent holds exposure, not from its average exposure:

```
r_t      = (mid[t+1] - mid[t]) / sigma_e          % sigma_e: trailing 20-day daily volatility at episode start
gross_e  = sum_t u_t * r_t                         % u_t in [-1,1] is the exposure held over (t, t+1]
cost_e   = sum_t |u_t - u_(t-1)| * (c/2) / sigma_e % c: round-trip cost; a flip pays c
alpha_e  = sum_t (u_t - mean(u)) * r_t - cost_e    % timing alpha: a constant position scores -cost
```

Fitness is a lower confidence bound over K episodes drawn fresh each generation, the same K for every agent in that generation, stratified by volatility and trend regime across the whole training span:

```
alpha_lcb = mean_e(alpha_e) - z * std_e(alpha_e) / sqrt(K)        % z = 1.0
eligible  = total_trades >= min_trades_per_episode * K
fitness   = alpha_lcb                    if eligible
          = -1000 + total_trades         otherwise    % a gradient out of the never-trade basin
```

Two further rules. Survivors are re-scored on the new generation's episodes like everyone else, so a lucky score does not persist. Exoself's weight tuning is split: perturbations are accepted or rejected on a *tune* subset of the episodes, and the fitness reported to selection comes from one pass over a disjoint *score* subset.

**Why it should work.** The always-long or always-short agent, which is the optimum today (section 2.2), scores exactly minus its cost under `alpha_e`. This is the standard separation of market-timing skill from average exposure (Treynor and Mazuy; Henriksson and Merton). Fresh, shared episodes per generation are the usual treatment for noisy fitness in evolutionary search (Jin and Branke 2005): common random numbers make the ranking fair within a generation, and resampling denies memorisation. The tune/score split removes the winner's curse built into `exoself.erl:143-151`, where fitness is the maximum over repeated evaluations on one window; that hill climb was designed for deterministic tasks such as pole balancing. Normalising by `sigma_e` makes episodes from 2022 and 2025 commensurable.

**Replaces.** `fitness_functions:total_profit/2` on one window; the max-over-attempts in `exoself:loop/2`; `gt_start`/`gt_end` as a single "bars back" range; the `highest_fitness=-2` initial value and the assumption of positive fitness.

**Expected impact.** Necessary, not sufficient. It creates no edge, but without it every other change will be selected for its ability to bet on drift. The measurable first effect is that the share of agents in the hold or never-trade attractors falls and that population rank on training episodes starts to correlate with rank on validation episodes.

**Effort.** Medium: `fx.erl` (episode list, exposure and return accumulators), `fitness_functions.erl` (three new functions), `exoself.erl` (tune/score split), `population_monitor.erl` (per-generation episode sampling), `config.erl`.

**Main risk.** Removing the between-episode directional component also removes genuine slow trend-following. With 8 episodes per generation that component is 8 coin flips, so excluding it is right in early phases; reintroduce it at 25 to 50% weight once K ≥ 16 and episodes are longer (phase D). The eligibility floor can create a "trade exactly the minimum" plateau, which is acceptable: it is a neutral starting point from which better-timed trades are selected.

### Change 2. Decide every 15 minutes, hold for hours, use 1-minute data only for execution

**Idea.** Step the simulator on a 15-minute decision clock. Features are computed from the 1-minute history up to the decision time; the order fills at the next 1-minute bar's open plus half the spread; stops are checked against 1-minute highs and lows inside the interval. A 960-step episode then covers 10 trading days instead of 16 hours.

**Why it should work.** Section 2.3: cost is 170% of the typical 1-bar move, 42% at 15 bars, 21% at 60, 10% at 240. The break-even hit rate falls from impossible to 55% at a 4-hour hold and 52% at a day. For the same compute per evaluation the agent sees 15 times more market, which is 15 times more independent evidence per fitness estimate. The reference design used 15-minute bars for the same spread (section 2.7). Filling one bar later removes the current zero-latency assumption (`fx.erl:502-520` fills at the close the agent has just observed).

**Replaces.** Per-row stepping in `fx:update_state/1`; `primary_currency_pair` as the only notion of timeframe.

**Expected impact.** Large and immediate on cost paid: the same policy that trades every 12 bars at 1 minute would trade at most every 15 minutes, and Change 4 pushes holds to hours. It is the single change that moves the search into a region where a positive answer is possible at retail cost.

**Effort.** Small to medium: `fx.erl` (step size, next-bar fill, intra-interval stop check), `live_scape.erl` (same clock), `config.erl`.

**Main risk.** Fewer independent bets: three years hold roughly 750 to 4,500 independent multi-hour decisions. That is thin for model selection, and it is why Changes 1 and 7 and the data recommendations in section 7 matter.

### Change 3. Replace raw price lists and the chart grid with a causal feature tape

**Idea.** Precompute once, per decision step, a vector of about 30 unitless, causal features and serve it from ETS. Group them so the curriculum can unlock groups:

| Group | Features (all scaled by trailing volatility where applicable) |
|---|---|
| `core` | log return over 15 min, 1 h, 4 h, 1 d, 1 w, each divided by σ·√h |
| `vol` | σ(1 h)/σ(1 d), σ(1 d)/σ(20 d), volatility level z-score, cost divided by expected 1-hour move |
| `session` | sin and cos of time of day and of day of week, minutes since London and New York opens |
| `structure` | position inside the 4 h, 1 d and 1 w high-low range; distance from 1 h and 1 d moving averages in σ; efficiency ratio over 4 h and 1 d |
| `candle` | 15-minute body/range, upper and lower wick fractions, range/σ |
| `account` | current exposure, open P&L in σ, log bars in trade, distance to stop |

For the substrate control specie, present the multi-timeframe block as a `{symmetric,[5,6]}` grid (timeframe × feature type), which gives HyperNEAT a geometry that is real.

**Why it should work.** The current inputs make evolution rediscover normalisation before it can learn anything. `(close − mean)/mean × 1000` (`sensor.erl:89-96`) changes scale with the volatility regime, and volatility varies 4.5× within a day. The chart grid rescales every window to its own high-low range (`fx.erl:584-588`), so a 5-pip range and a 50-pip range look identical: it discards exactly the volatility information that section 2.3 shows is the predictable part. A neuron attached to the 270-bar sensor carries 270 weights; with populations of 12 to 48 and the throughput in section 2.6, that space is not searchable. Thirty stationary inputs are 3 to 20 times fewer than today's sensors and make a policy learned in 2023 applicable in 2025. `vol` and `session` let an agent learn *when* trading is affordable, which is the most reliable lever available.

**Replaces.** `fx_PLI` (90, 270) and `fx_PCI` (30×20) in `morphology:forex_trader/1`; the per-bar window rebuilds in `fx:list_encoded/2` and `fx:plane_encoded/3`.

**Expected impact.** Medium to large, and it compounds with Change 6 (neural encoding becomes cheap and adequate). It also speeds evaluation, since sensing becomes one ETS lookup.

**Effort.** Medium: new `fx_features.erl` (tape builder), `sensor.erl` (`fx_Features`), `morphology.erl` (`forex_trader_v3` with groups), `live_scape.erl` (same features online).

**Main risk.** The features encode my priors. Mitigations: keep one raw multi-scale return group so evolution is not confined to them, and run the truncation test in section 5 so no feature can leak the future.

### Change 4. Output an exposure, not a trade; add hysteresis, minimum hold and scape-level risk

**Idea.** In three steps that match the curriculum:

1. *Exposure actuator* (`fx_Exposure`, one output): the network emits a desired exposure in [−1, 1]. The scape applies hysteresis (enter when |u| ≥ 0.5, exit when |u| ≤ 0.15, flip only on crossing the opposite entry level), a minimum hold in decision bars, and a cap on trades per day. Exposure maps to {−1, 0, +1} units of a volatility-targeted position.
2. *Scape-enforced limits* (not learned): leverage cap, daily loss limit that flattens until the next session, equity floor that ends the episode, and a stop at k × ATR checked on 1-minute bars.
3. *Risk actuator* (`fx_Risk`, unlocked in phase C): stop distance in ATR and size fraction, acquired through the existing `add_actuator` mutation. Disagreement among ensemble members (Change 7) supplies the confidence signal for size.

**Why it should work.** Churn is the dominant loss: random gen-0 agents trade every 12 bars and lose two thirds of the account per window (section 2.4). With `functions:trinary/1` thresholds at ±0.33 and a decision every bar, any output that hovers near a threshold flips repeatedly, and each flip costs a full spread. Hysteresis and a minimum hold remove that failure mode by construction rather than hoping evolution finds it; cost-aware position-based outputs with the current position fed back as an input are the standard formulation since Moody and Saffell (2001). Today's sizing is a fixed $5,000 notional on $300 (16.7× leverage) regardless of volatility, and `account_extinction_threshold` of 1 disables ruin, so risk is neither learnable nor penalised.

**Replaces.** `actuator:fx_Trade/4` with `functions:trinary/1`; `buy_money_fixed` × `account_leverage` sizing in `fx:open_order/3`; the disabled extinction threshold.

**Expected impact.** Large on cost drag in every phase; moderate on risk-adjusted return once sizing is active. It also realises the intent of growing structure with skills: the risk actuator and later sensor groups are new modules that attach when a phase unlocks them.

**Effort.** Medium: `actuator.erl`, `fx.erl` (`make_trade/3`, sizing, stops, limits), `morphology.erl`, `config.erl`; re-enable `add_actuator` from phase C.

**Main risk.** Hand-set bands and holds may cap a real fast edge. Section 2.5 says there is none to cap at these costs; keep the band and hold as config so a low-cost variant can loosen them.

### Change 5. A curriculum whose every phase is the final objective under easier conditions, with gates

**Idea.** Four skills in sequence, each scored by the same family of fitness functions:

- **A. Direction** at zero cost: can the population time exposure at all? Sensor groups unlock in two steps.
- **B. Cost**: anneal round-trip cost 0.3 → 0.7 → 1.0 → 1.5 pips with one bar of latency. The agent must trade less and hold longer to stay positive.
- **C. Risk**: unlock the risk actuator, volatility-targeted sizing and scape limits; penalise drawdown.
- **D. Robustness**: jitter cost and latency, add feature noise, include stress windows, score the worst quartile of episodes.
- **E. Consolidation**: no structural mutation, weights only, wide evaluation, build the ensemble.

A phase ends when its gate passes on the validation span, or when validation has not improved for a patience window. It does not end on a generation count. If a gate fails, the run halts and reports the last level passed. In phase B that report is the answer to "what cost can this edge afford", which is worth knowing before opening an account. Between phases the next population is seeded from a hall of fame of validated, behaviourally distinct champions plus their mutants plus 20% fresh agents, not a clone of a converged population.

**Why it should work.** Incremental evolution works when each stage's solution lies on the path to the next (Gomez and Miikkulainen 1997). The earlier phase functions rewarded trade counts, which is off that path; the current windows reward a bias sign that flips between phases (section 2.2). Cost annealing starts where the fitness landscape has a gradient: at full cost the nearest neighbours of a never-trading agent are all worse, so nothing leaves that basin. Early stopping on validation is the simplest guard against fitting the training span, and a fixed generation count either wastes compute or overfits.

**Replaces.** `exp_runner:get_run_configs/0` (section 4 has the replacement), generation-count termination in `population_monitor`, and `clone_population_for_next_run/2` as the only carry mechanism.

**Expected impact.** Medium on its own, large in combination: it converts compute into answers. Phase A's gate is the cheapest decisive experiment in this document.

**Effort.** Medium: `exp_runner.erl` (gates, hall of fame, seeding), `population_monitor.erl` (gate checks, patience), `config.erl`; a new `hall_of_fame` Mnesia table.

**Main risk.** Skills learned at zero cost may not survive cost (high-turnover policies are a dead end). Mitigations: the minimum hold from Change 4 is active from phase A, and the first cost step is small.

### Change 6. Make structure able to grow when it helps: encoding, selection, mutation, niches, population

**Idea.** Five linked changes.

1. *Neural encoding by default*, with a substrate specie kept as a control on the grid sensor from Change 3.
2. *Rank-based selection on fitness alone.* Complexity enters only as a tie-break within a small fitness margin.
3. *Neutral structural mutation.* A new neuron or splice is inserted so that the network's output is unchanged at insertion (pass-through input weight, small output weight), as in NEAT, and then tuned with the existing age-annealed perturbation.
4. *Behaviour niches.* Bin agents by trades per day and median holding time; keep elites per bin (MAP-Elites style), implemented with the existing specie records.
5. *Smaller menu, larger population.* Four activation functions, no plasticity and a single aggregator through phase C; population around 150.

**Why it should work.** Three mechanisms currently hold agents at 1 to 8 neurons, beyond the fact that the objective's optimum needs one neuron. Offspring allotment is fitness divided by neuron count (`selection_algorithm.erl:50-54`), so a 4-neuron agent needs four times the fitness margin of a 1-neuron agent. A new neuron arrives with a random activation function and random weights (`genome_mutator.erl:717-750`), so it injects noise into a working policy and is removed within a generation; NEAT's answer is speciation and neutral insertion (Stanley and Miikkulainen 2002). And there is one species, because species are defined by morphology × architecture (`exp_runner.erl:20-21`), so new structure gets no protection. Niching by behaviour also keeps the population out of a single attractor and supplies diverse members for the ensemble (Mouret and Clune 2015; Lehman and Stanley 2011).

On encoding: with a 1 to 2 neuron CPPN, the substrate's weights are one smooth function of coordinates, which cannot express "this feature matters and its neighbour does not"; indirect encodings degrade as the target becomes irregular (Clune et al. 2011). By my reading of `substrate:create_substrate/4`, depth 2 yields a single 25-neurode hidden plane, so the default substrate is also a fixed small network whose reported "neurons" are CPPN neurons. It is 7 times slower in aggregate (section 2.6), and the Handbook's own table shows chart-plane substrate agents testing worse on average than price-list agents. With 30 inputs there is no large input geometry for HyperNEAT to exploit.

On the menu: 9 activation functions × 12 plasticity rules × 3 aggregators × 8 tuning selectors is a meta-search no population of 48 can cover. Plasticity also makes behaviour depend on the whole episode history, which is hard to validate and differs at deployment. Recurrence stays, with state reset per episode and a warm-up before trades count.

**Replaces.** `agent_encoding_types` default; `selection_algorithm:competition/4` allotment; weight initialisation in `add_neuron` and `outsplice`; the single specie; the default function menus; population sizes of 12 to 48.

**Expected impact.** Medium, and conditional: it matters only after Changes 1 to 3 give structure something to earn. The check is direct: plot validation fitness against neuron count per lineage and expect growth only where validation improves.

**Effort.** Medium to high: `selection_algorithm.erl`, `genome_mutator.erl`, `population_monitor.erl` (niches, migration between bins), `config.erl`.

**Main risk.** More machinery to tune and debug. Sequence it after phase A's gate, and adopt one piece at a time against that fixed experiment.

### Change 7. Keep a hall of fame, trade an ensemble, and build a flat evaluator

**Idea.** Persist every gate-passing champion with its validation metrics. Deploy the mean exposure of M members chosen greedily on validation from different niches, phases and seeds; trade when the mean crosses the hysteresis levels, and scale size by agreement. Separately, add a single-process evaluator that runs a phenotype as a sorted array of neurons with no message passing, and cross-check it trade for trade against the process-based one.

**Why it should work.** Each champion is a noisy draw; averaging uncorrelated errors reduces variance (Breiman 1996), and forward selection from a library is a proven way to build the committee (Caruana et al. 2004). A mean of several exposures is smoother than any member, so it flips less and pays less cost. Agreement is a confidence output that needs no extra training. The flat evaluator is what makes populations of 150 with 12 to 24 episodes routine: section 2.6 suggests 20 to 100× is available **[unverified estimate]**.

**Replaces.** Single-champion benchmarking in `population_monitor:run_GenTest/2`; whole-population cloning as the only memory between phases.

**Expected impact.** Moderate and reliable: ensembles rarely beat the best member's backtest but usually beat its live result. Throughput gain is an enabler for everything else.

**Effort.** Ensemble low to medium (`ensemble.erl`, `live_scape.erl`); flat evaluator medium (`flat_eval.erl`, a hook in `exoself.erl`).

**Main risk.** Selecting members on validation overfits validation; that is what the untouched test and lockbox spans are for. The flat evaluator can diverge from the reference semantics for recurrent links, so it must be verified on a thousand random genomes before use.

## 4. Redesigned curriculum

`apply_run_configs/3` clears all overrides before loading a run's list, so each entry must be complete. The entries below are `Base ++ Delta`; later keys override earlier ones in `config:load_from_list/1`.

```erlang
get_run_configs() ->
    Base = [
        %% ---- data, clock, episodes ----
        {primary_currency_pair, 'EURUSD1'},
        {decision_tf_minutes, 15},
        {train_span, {{2022,10,3},{2024,6,28}}},
        {val_span,   {{2024,7,8},{2024,12,31}}},       % one-week embargo each side
        {episode_bars, 960},                           % 10 trading days of 15-min decisions
        {episode_warmup_bars, 32},
        {episode_sampler, {stratified, [vol_tercile, trend_tercile, year]}},
        {episodes_tune, 4}, {episodes_score, 8},
        {reevaluate_survivors, true},
        %% ---- execution ----
        {fill_model, next_bar_open}, {latency_bars, 0},
        {account_spread, 0.0}, {slippage_pips, 0.0},
        {account_initial_balance, 300},
        %% ---- perception and action ----
        {morphology, forex_trader_v3},
        {sensor_groups, [core, vol, session, account]},
        {actuator_set, [fx_Exposure]},
        {exposure_enter, 0.5}, {exposure_exit, 0.15},
        {min_hold_bars, 4}, {max_trades_per_day, 6},
        %% ---- search ----
        {agent_encoding_types, [neural]},
        {connection_architecture, recurrent},
        {neural_activation_functions, [tanh, linear, gaussian, absolute]},
        {neural_plasticity_functions, [none]},
        {neural_aggregation_functions, [dot_product]},
        {heredity_types, [darwinian]},
        {population_selection_f, rank_tournament},
        {complexity_policy, {tie_break, 0.02}},
        {structural_mutation_init, neutral},
        {niching, {behaviour_cells, [{trades_per_day,[0.2,1,3]}, {median_hold_bars,[8,32]}]}},
        {niche_elites, 4},
        {specie_size_limit, 144}, {init_specie_size, 144},
        {survival_percentage, 0.35},
        {tuning_duration, {const,4}},
        {tot_topological_mutations_functions, [{ncount_exponential,0.5}]},
        %% ---- objective and gates ----
        {fitness_function, alpha_lcb}, {fitness_lcb_z, 1.0},
        {min_trades_per_episode, 2},
        {min_generations, 15}, {generation_limit, 80},
        {gate_check_every, 5}, {gate_patience, 15}, {on_gate_fail, halt},
        {carry, {hall_of_fame, 24}}, {carry_fresh_fraction, 0.2}
    ],
    Gate = fun(Extra) -> {phase_gate, [{trades,'>=',150} | Extra]} end,
    [
     %% ===== A. Direction: timing skill at zero cost =====
     {1, Base ++ [{phase_name, a1_direction_core},
                  Gate([{alpha_tstat,'>=',3.0}, {gross_edge_pips,'>=',0.5}, {beats_random_pct,'>=',95}])]},
     {2, Base ++ [{phase_name, a2_direction_multitf},
                  {sensor_groups, [core, vol, session, account, structure, candle]},
                  Gate([{alpha_tstat,'>=',3.0}, {gross_edge_pips,'>=',1.0}, {improves_on_phase,1}])]},

     %% ===== B. Cost: selectivity and holding period =====
     {3, Base ++ [{phase_name, b1_cost_0p3}, {sensor_groups, all},
                  {account_spread, 0.00003}, {latency_bars, 1},
                  Gate([{net_alpha_lcb,'>',0.0}, {net_edge_pips,'>',0.0}])]},
     {4, Base ++ [{phase_name, b2_cost_0p7}, {sensor_groups, all},
                  {account_spread, 0.00007}, {latency_bars, 1},
                  Gate([{net_alpha_lcb,'>',0.0}, {net_edge_pips,'>',0.0}])]},
     {5, Base ++ [{phase_name, b3_cost_1p0}, {sensor_groups, all},
                  {account_spread, 0.00010}, {latency_bars, 1}, {slippage_pips, 0.1},
                  Gate([{net_alpha_lcb,'>',0.0}, {net_edge_pips,'>',0.0}])]},
     {6, Base ++ [{phase_name, b4_cost_1p5}, {sensor_groups, all},
                  {account_spread, 0.00015}, {latency_bars, 1}, {slippage_pips, 0.1},
                  Gate([{net_alpha_lcb,'>',0.0}, {net_edge_pips,'>',0.0}, {beats_random_pct,'>=',95}])]},

     %% ===== C. Risk: sizing, exits, drawdown =====
     {7, Base ++ [{phase_name, c1_risk_actuator}, {sensor_groups, all},
                  {account_spread, 0.00015}, {latency_bars, 1}, {slippage_pips, 0.1},
                  {actuator_set, [fx_Exposure, fx_Risk]},
                  {sizing, {vol_target, 0.10}}, {max_leverage, 10},
                  {stop_atr_range, {1.0, 4.0}},
                  {episodes_score, 12},
                  {fitness_function, alpha_lcb_dd}, {fitness_dd_floor, 0.05}, {fitness_dd_lambda, 2.0},
                  Gate([{net_alpha_lcb,'>',0.0}, {max_drawdown,'=<',0.10}, {sharpe_vs_phase,{6,0.9}}])]},
     {8, Base ++ [{phase_name, c2_risk_limits}, {sensor_groups, all},
                  {account_spread, 0.00015}, {latency_bars, 1}, {slippage_pips, 0.1},
                  {actuator_set, [fx_Exposure, fx_Risk]},
                  {sizing, {vol_target, 0.10}}, {max_leverage, 10}, {stop_atr_range, {1.0, 4.0}},
                  {daily_loss_limit, 0.02}, {account_extinction_threshold, 210},
                  {spread_schedule, by_hour},
                  {episodes_score, 12},
                  {fitness_function, alpha_lcb_dd}, {fitness_dd_floor, 0.05}, {fitness_dd_lambda, 2.0},
                  Gate([{net_alpha_lcb,'>',0.0}, {max_drawdown,'=<',0.10}])]},

     %% ===== D. Robustness: survive what was not trained on =====
     {9, Base ++ [{phase_name, d1_robust_noise}, {sensor_groups, all},
                  {account_spread, 0.00015}, {slippage_pips, 0.1},
                  {actuator_set, [fx_Exposure, fx_Risk]},
                  {sizing, {vol_target, 0.10}}, {max_leverage, 10}, {stop_atr_range, {1.0, 4.0}},
                  {daily_loss_limit, 0.02}, {account_extinction_threshold, 210}, {spread_schedule, by_hour},
                  {cost_jitter, 0.3}, {latency_bars, {uniform,1,2}}, {feature_noise_sd, 0.05},
                  {episodes_score, 24}, {stress_episode_fraction, 0.25},
                  {fitness_function, alpha_cvar}, {fitness_cvar_q, 0.25},
                  Gate([{net_alpha_lcb,'>',0.0}, {retention_cost_plus_30pct,'>=',0.7},
                        {retention_delay_2bars,'>=',0.7}, {positive_vol_terciles,'>=',2}])]},
     {10, Base ++ [{phase_name, d2_robust_long}, {sensor_groups, all},
                  {account_spread, 0.00015}, {slippage_pips, 0.1},
                  {actuator_set, [fx_Exposure, fx_Risk]},
                  {sizing, {vol_target, 0.10}}, {max_leverage, 10}, {stop_atr_range, {1.0, 4.0}},
                  {daily_loss_limit, 0.02}, {account_extinction_threshold, 210}, {spread_schedule, by_hour},
                  {cost_jitter, 0.3}, {latency_bars, {uniform,1,2}}, {feature_noise_sd, 0.05},
                  {episode_bars, 1920}, {episodes_score, 16}, {stress_episode_fraction, 0.25},
                  {between_episode_weight, 0.25},
                  {fitness_function, alpha_cvar}, {fitness_cvar_q, 0.25},
                  Gate([{net_alpha_lcb,'>',0.0}, {mirror_asymmetry,'=<',0.3}, {positive_vol_terciles,'>=',2}])]},

     %% ===== E. Consolidation: weights only, wide evaluation, ensemble =====
     {11, Base ++ [{phase_name, e1_consolidate}, {sensor_groups, all},
                  {account_spread, 0.00015}, {latency_bars, 1}, {slippage_pips, 0.1},
                  {actuator_set, [fx_Exposure, fx_Risk]},
                  {sizing, {vol_target, 0.10}}, {max_leverage, 10}, {stop_atr_range, {1.0, 4.0}},
                  {daily_loss_limit, 0.02}, {account_extinction_threshold, 210}, {spread_schedule, by_hour},
                  {mutation_operators, [{mutate_weights,1}]},
                  {tuning_duration, {const,8}}, {annealing_parameters, [0.5]},
                  {episodes_tune, 8}, {episodes_score, 32},
                  {carry_fresh_fraction, 0.0}, {generation_limit, 30},
                  {fitness_function, alpha_cvar}, {fitness_cvar_q, 0.25},
                  {build_ensemble, {greedy_forward, 7}},
                  Gate([{ensemble_net_alpha_lcb,'>',0.0}, {ensemble_beats_best_member, true}])]}
    ].
```

Budget check: 144 agents × (4 tune episodes × 4 attempts + 8 score episodes) × 960 steps is 3.3 million steps per generation, about 95 seconds at the measured neural rate, so an 80-generation cap is about two hours per phase on this machine before any evaluator speed-up.

### New config keys

| Key | Example | Meaning | Read by |
|---|---|---|---|
| `decision_tf_minutes` | 15 | Simulator step and sensor clock | `fx`, `live_scape` |
| `train_span`, `val_span`, `test_span`, `lockbox_span` | date pairs | Calendar ranges; replaces `gt_start`/`gt_end`/`bench_*` | `fx`, `population_monitor`, eval harness |
| `episode_bars`, `episode_warmup_bars` | 960, 32 | Episode length in decision steps; steps before trades count | `fx` |
| `episode_sampler` | `{stratified,[...]}` | How episode start points are drawn each generation | `population_monitor` |
| `episodes_tune`, `episodes_score` | 4, 8 | Disjoint subsets for exoself tuning and for reported fitness | `exoself` |
| `reevaluate_survivors` | true | Survivors are scored again on each generation's episodes | `population_monitor` |
| `fill_model`, `latency_bars`, `slippage_pips` | `next_bar_open`, 1, 0.1 | Execution realism | `fx` |
| `spread_schedule`, `cost_jitter` | `by_hour`, 0.3 | Time-varying and randomised cost | `fx` |
| `sensor_groups`, `actuator_set` | lists | What the morphology exposes in this phase | `morphology` |
| `exposure_enter`, `exposure_exit`, `min_hold_bars`, `max_trades_per_day` | 0.5, 0.15, 4, 6 | Hysteresis and churn limits | `fx` |
| `sizing`, `max_leverage`, `stop_atr_range`, `daily_loss_limit` | see entries | Risk model | `fx` |
| `fitness_lcb_z`, `min_trades_per_episode`, `fitness_dd_floor`, `fitness_dd_lambda`, `fitness_cvar_q`, `between_episode_weight` | see entries | Parameters of `alpha_lcb`, `alpha_lcb_dd`, `alpha_cvar` | `fitness_functions` |
| `complexity_policy`, `structural_mutation_init`, `niching`, `niche_elites` | see entries | Search behaviour | `selection_algorithm`, `genome_mutator`, `population_monitor` |
| `phase_name`, `phase_gate`, `min_generations`, `gate_check_every`, `gate_patience`, `on_gate_fail` | see entries | Validation-gated transitions | `exp_runner`, `population_monitor` |
| `carry`, `carry_fresh_fraction`, `build_ensemble` | `{hall_of_fame,24}`, 0.2, `{greedy_forward,7}` | Champion carry and ensemble construction | `exp_runner` |
| `feature_noise_sd`, `stress_episode_fraction` | 0.05, 0.25 | Robustness perturbations | `sensor`, `population_monitor` |

Gate metrics are computed on the full validation span, tiled into episodes, for the mean exposure of the top five hall-of-fame members, under the phase's own cost and latency.

## 5. How we will know it is working

### 5.1 Data protocol

| Span | Dates | Use |
|---|---|---|
| Train | 2022-10-03 to 2024-06-28 | Episodes for fitness |
| Validation | 2024-07-08 to 2024-12-31 | Gates, early stopping, ensemble selection |
| Test | 2025-01-08 to 2025-05-30 | Scored once per curriculum version |
| Lockbox | 2025-06-09 to 2025-09-19 | Scored once, at the end |

One-week embargo between spans. For the headline result, repeat the whole curriculum walk-forward over four to six expanding folds and pool the test segments, because a single five-month test has little statistical power (5.3).

Three integrity tests come before any evolution. *Truncation*: recompute every feature at time t with the data cut at t and require identical values. *Parity*: the Erlang simulator and an independent re-implementation must agree trade for trade on scripted signals (a first sanity check already agrees: an independent calculation of always-short on the phase-1 window gives 318.26, and the real run's median fitness was 318.3). *Null*: random agents must centre on minus cost times trades.

### 5.2 Baselines, all run through the same simulator

| Baseline | Purpose |
|---|---|
| Never trade; always long; always short | Exposes drift; with Change 1 the last two must score minus cost |
| Matched random | For each candidate, 1,000 agents with the same number of trades, the same holding-time distribution and long/short split, entries permuted within the same session hours. Gives a p-value for "same activity, no information" |
| Simple rules on the same features | Donchian breakout, moving-average cross, range mean-reversion, parameters chosen on train only |
| Ridge and gradient-boosted trees on the exact feature tape | Predict the next 4-hour and 1-day volatility-scaled return, threshold to trade, tune on validation. Tells signal problems apart from search problems |
| Current design's champion | The 12-phase run as it is today |

### 5.3 Metrics, minimum counts, significance

Report for every candidate on validation, test and lockbox, at 0.3, 0.7 and 1.5 pips: net and gross pips, trades, edge per trade, hit rate, mean hold, annualised Sharpe of daily P&L, maximum drawdown, share of profitable months, break-even cost per trade, sensitivity to 1 to 3 bars of delay, results by volatility tercile and session, and the mirror test (score on the sign-flipped series; a large asymmetry means directional bias).

- **Minimum counts.** At least 150 trades on validation and 100 on each test segment. Below that, report P&L but no Sharpe and no pass.
- **Significance.** Block bootstrap by week for confidence intervals; the matched-random p-value; and the Deflated Sharpe Ratio using the count of every curriculum variant ever scored against validation, which must be logged from day one.
- **Power.** The t-statistic of a Sharpe ratio is about Sharpe × √years. A true Sharpe of 1.0 gives t ≈ 0.65 on a 5-month test and 1.4 on two years of pooled walk-forward. So on this table only a Sharpe of about 1.5 or more can be confirmed at all; with ten years of history the threshold drops to about 0.65.

### 5.4 Leading indicators, logged every generation

These show progress long before profit does.

1. Spearman correlation between the population's training fitness and its validation fitness. Target: sustained above 0.3. Today it is undefined in any useful sense.
2. Validation gross edge per trade, in pips, rising across generations at zero cost.
3. Attractor census: share of agents that never trade or hold one position for over 90% of an episode. Target: under 20%.
4. Validation fitness against neuron count per lineage: growth should appear only where validation improves.
5. Trades per day and mean holding time drifting down and up respectively through phase B.

### 5.5 Go / no-go

| Gate | Pass criteria (validation unless stated) | If it fails |
|---|---|---|
| G0 Plumbing | Truncation, parity and null tests pass; always-long scores minus cost | Fix before any run |
| G1 Direction, cost 0 | Timing-alpha t ≥ 3 (block bootstrap); gross edge ≥ 0.5 pips per trade on ≥ 300 trades; above the 95th percentile of matched random; in three independent seeds | See the table below |
| G2 Cost, per level | Net alpha lower bound > 0; ≥ 150 trades; beats matched random at the final level | Record the cost ceiling; do not proceed to higher cost; decide whether a lower-cost venue makes the project viable |
| G3 Risk | Maximum drawdown ≤ 10% at the volatility target; Sharpe ≥ 90% of phase B's | Keep phase B champions with scape-level risk only |
| G4 Robustness | Retains ≥ 70% of net alpha at +30% cost and at 2 bars of delay; profitable in ≥ 2 of 3 volatility terciles; mirror asymmetry ≤ 30% | Return to phase C with more episodes; do not deploy |
| Final | Pooled walk-forward test: net Sharpe ≥ 1.5 at the target cost, p < 0.05 against matched random, ≥ 60% profitable months, Deflated Sharpe ≥ 0.9; then lockbox net positive with Sharpe > 0.5 | No live capital |
| Live | Three months of paper trading at live spreads within the bootstrap interval of the test | Stop and re-examine execution assumptions |

How to read a G1 failure:

| | Boosted-tree baseline finds gross edge | Boosted-tree baseline finds none |
|---|---|---|
| **Evolution finds gross edge** | Proceed | Proceed, with extra scrutiny for overfitting |
| **Evolution finds none** | Search problem: do Change 6 and the flat evaluator before anything else | Signal problem: change timeframe, instrument or data (section 7); more evolution will not help |

## 6. Roadmap

Each stage ends with one experiment that must pass before the next begins.

| Stage | What | Files | Validating experiment |
|---|---|---|---|
| 0 | Evaluation harness: date spans, per-episode ledger (trades, holds, exposure), baselines, matched-random test, variant counter | new `eval_protocol.erl`; `fx.erl`, `qlog.erl` | G0. Reproduce section 2.2 and 2.4 from inside the system |
| 1 | Objective and multi-window evaluation (Change 1) on the *existing* sensors and actuator | `fx.erl`, `fitness_functions.erl`, `exoself.erl`, `population_monitor.erl`, `config.erl` | Always-long scores minus cost; attractor census drops; train-to-validation rank correlation is measurable. Expect no edge yet |
| 2 | Decision clock, next-bar fills, feature tape (Changes 2 and 3) | new `fx_features.erl`; `fx.erl`, `sensor.erl`, `morphology.erl`, `live_scape.erl` | Truncation test; ridge and boosted-tree baselines on the tape; then phase A1 and A2 against G1 |
| 3 | Exposure actuator, hysteresis, cost annealing, gates, hall of fame (Changes 4 and 5, first halves) | `actuator.erl`, `fx.erl`, `exp_runner.erl`, `population_monitor.erl` | Phases B1 to B4 against G2; report the cost ceiling |
| 4 | Search upgrades and flat evaluator (Changes 6 and 7). Move this before stage 3 if G1 fails as a search problem | `selection_algorithm.erl`, `genome_mutator.erl`, `population_monitor.erl`; new `flat_eval.erl` | Same phase-A experiment, same seeds: higher validation edge per compute hour; evaluator parity on 1,000 random genomes |
| 5 | Risk actuator, scape limits, robustness phases, ensemble | `actuator.erl`, `fx.erl`, `morphology.erl`; new `ensemble.erl` | Phases C and D against G3 and G4; ensemble beats its best member on validation |
| 6 | Walk-forward folds, single test read, lockbox, paper trading | harness, `live_scape.erl` | Final and Live gates |

Stages 0 and 1 are small and change no perception or action code. Do them first even if nothing else is adopted, because they turn every later experiment from anecdote into measurement.

## 7. Honest odds

These are judgments, informed by the measurements above and by the literature; they are not measured probabilities.

**EURUSD, price only, 1.5 pips, this dataset.**

- Holds under one hour: effectively zero. The required hit rate is above 60%, and neither the linear baseline nor the best-of-2,000 experiment finds more than a few hundredths of a pip of gross edge.
- Holds of four hours to a few days: roughly 5 to 10% that the redesigned system finds an edge that is real, net positive and persists for six months or more. Even then, three years of one pair can barely confirm it (5.3).

Reasons for the low number: EURUSD is the most liquid and most studied instrument there is; the inputs are public price history alone; published evidence on evolved and technical trading rules in major currencies shows profits that were real in the 1970s to early 1990s and have since faded, with intraday rules unprofitable after realistic costs (Neely, Weller and Dittmar 1997; Neely and Weller 2003; Neely, Weller and Ulrich 2009); and the design this code descends from did not show average out-of-sample profit in its own book (section 2.7).

**What would change the odds most, in order.**

1. **Cost.** A raw-spread account at roughly 0.6 to 0.8 pips all-in **[unverified; broker dependent]** instead of 1.5 lowers the 1-hour break-even hit rate from 60.5% to about 55%, and the 4-hour one from 55% to about 52%. That roughly doubles the set of strategies that can be net positive. Phase B measures directly what cost the evolved edge can bear.
2. **Breadth.** Several instruments rather than one. Expected risk-adjusted return scales with skill × √(number of independent bets) (Grinold 1989), and a basket allows relative-value signals that one series cannot express. Seven or eight major pairs plus gold would multiply independent bets several times over and is the largest single improvement available to the search.
3. **Information beyond price.** Rate differentials, the economic calendar, cross-asset moves, and real volume or order flow (the volume column here is empty). Price-only signals in EURUSD are the most arbitraged kind.
4. **History.** Ten or more years of 1-minute data **[unverified that the project's source provides it]** makes validation meaningful: the confirmable Sharpe falls from about 1.5 to about 0.65, and regimes repeat.
5. **Timeframe.** Already in the redesign: decide at 15 minutes to an hour, hold for hours to days.

With lower cost, a basket and longer history together, I would put the chance of a modest validated edge at perhaps 20 to 30%. That is still more likely to fail than succeed, which is why the plan is built around cheap, early, decisive gates: G1 answers the main question within stages 0 to 2, before the expensive work.

## 8. Implementation status: stages 0 and 1, with trading costs off

Stages 0 and 1 of the roadmap are implemented in this folder (uncommitted). Sections 1 to 7 above are the plan as written before implementation; this section records what was built, where it differs from the plan, and what was measured. Trading cost is zero everywhere by request: `account_spread` now defaults to `0.0` and every run config sets it explicitly.

### 8.1 What was built

| Piece | Where | What it does |
|---|---|---|
| Episode sampling | new `episodes.erl` | Date spans (`train_span`, `val_span`), a row index over the price table, one episode start per time stratum, fresh tune and score sets each generation (the same for every agent), a fixed validation set tiled over `val_span` |
| Multi-episode simulator | `fx.erl` | One evaluation runs a list of episodes with the account reset between them; per episode it accumulates exposure, returns, entries and the timing statistics; the halt flag and fitness come after the last episode. The legacy single-window path is unchanged and still selected by `{evaluation_mode, legacy}` |
| Fitness | `fitness_functions.erl` | `alpha_t`, `alpha_lcb`, `alpha_mean`, the eligibility rule, and averaging of any legacy function over episodes |
| Score and validation passes | `exoself.erl` | Each agent is run once on the score episodes and that fitness goes to selection; optionally once more on validation, for logging only. Tuning the weights first on separate tune episodes is a setting (`episode_tuning`), switched off in the run configs after the four-run trial (8.5) |
| Watchdog | `cortex.erl`, `exoself.erl` | Ends agents whose network stalled (no bar completed for `stall_timeout_ms`) or ran away (a mailbox over `runaway_queue_limit`), and kills every agent's processes when its turn ends. Added after the four-run trial (8.5) |
| Selection | `selection_algorithm.erl` | `rank`: offspring by fitness rank only. Pulled forward from stage 4 because `competition` deletes the whole population when total fitness is negative and still divides by neuron count |
| Generation summary | `population_monitor.erl`, `eval_protocol.erl` | One `GEN_SUMMARY` line per generation in `logs/Benchmarker/generation_summary.log`: fitness, eligibility, never-trade and holder counts, entries per episode, network size, rank agreement tune-to-score and score-to-validation, validation results of the better half |
| Harness | new `eval_protocol.erl` | `g0/0` integrity checks, `baselines/0`, `legacy_windows/1`, `evaluate_agent/1` and `evaluate_population/1` with a shift test, `record_trial/2` and `trial_count/0` |
| Curriculum | `exp_runner.erl` | `get_run_configs/0` is now four runs of direction-at-zero-cost with increasing score episodes (8, 16, 24, 32 since 8.5) and population; the old 12 phases are kept as `get_run_configs_legacy/0` |
| Memory fix: neuron loop | `neuron.erl` | `loop/6` wrapped its recursive calls in `try`, so they were not tail calls: every message left a stack frame that kept that cycle's state and weights alive. See 8.3 |
| Memory fix: price loader | `fx.erl` | The file is parsed line by line instead of as one character list, and a file that has not changed is not re-read on later heartbeats. See 8.3 |

### 8.2 Where the implementation differs from the plan, and why

1. **The default fitness is `alpha_t`, not `alpha_lcb`.** Running `alpha_lcb` showed a side effect that section 3 did not anticipate: while no agent has skill, the lower bound `mean − z·std/√K` is highest for agents whose alpha barely varies, which means agents that barely trade. In an 8-generation run the median entries per episode fell 50 → 67 → 35 → 32 → 13 → 5 → 4.5 → 5.75 and the share of market variance the agents were exposed to fell to under 1% for most of the final population. `alpha_t` scores timing in units of its own no-skill standard deviation (`z` in `fx:ep_stat/4`), so every eligible agent's fitness is about 0 ± 1 without skill, whether it trades 4 times an episode or 400. `alpha_lcb` remains available for later phases, where a population with real edge should be asked for consistency.
2. **The shift test compares `z`, not alpha.** The first version rotated the exposure series and compared mean alpha. For an agent that is only in the market during volatile bars this gave p-values of 0.013 to 0.022 where the t-statistic was 1.0: rotated copies mostly land on quiet bars and have a narrow alpha, so volatility timing was being read as a directional edge. With `z` the same agent has p = 0.14, in agreement with its t-statistic.
3. **Episode volatility is the episode's own realised volatility**, not the trailing 20-day figure, because it is only a scoring unit and needs no extra pass over the data.
4. **Strata are time slices**, not volatility and trend terciles. A draw always spans the whole train span; regime-aware strata are left for stage 2, when the feature tape computes those regimes anyway.
5. **Neural encoding is set in the run configs** (stage 4 in the plan), for throughput. `config.erl` still defaults to substrate.
6. **Not built yet from the plan's key list:** gates, hall of fame, the 15-minute clock, the feature tape, the exposure actuator. The 11-phase config in section 4 therefore does not run yet; the four-run config in `exp_runner.erl` is what runs today.

### 8.3 What was measured

**Integrity (gate G0).** `eval_protocol:g0()` passes all ten checks on the full table at zero cost and at 1.5 pips: a constant position scores exactly minus its cost, never-trade and buy-and-hold are ineligible, random trading centres on minus its cost (mean alpha −18.75 against a cost of 18.75 at 1.5 pips), dollar and price-unit accounting agree, tune and score episodes are disjoint, validation lies after training.

**Parity with an independent implementation.** For always-long, always-short and a flip-every-50-bars script on the 12 old windows at 1.5 pips, the Erlang simulator and the Node re-implementation agree to four decimals in dollars and exactly in trade counts.

**Legacy path unchanged.** The old configuration (single window, `total_profit`, 1.5 pips, substrate) still converges on always-short: in a 3-generation run the two most common fitness values are 300.00 (never traded) and 318.33, the value the parity script gives for always-short on that window.

**Baselines at zero cost** (1,000-bar episodes, 30 warm-up bars; `timing_t` is `alpha_t`):

| Policy | Train, 48 episodes: timing t | Validation, 24 episodes: timing t | Entries per episode |
|---|---|---|---|
| Random, redraw every bar / 10% / 1% of bars | −0.12 / −0.03 / −0.03 | 0.46 / −0.83 / 1.61 | 431 / 43 / 4 |
| Flip every 50 bars | −0.01 | −0.76 | 20 |
| Reversal over 15 / 60 / 240 bars | 0.64 / **2.29** / 1.22 | 1.11 / −0.05 / **3.17** | 115 / 58 / 24 |
| Momentum over 15 / 60 / 240 bars | −0.64 / −2.29 / −1.22 | −1.11 / 0.05 / −3.17 | same |

So at zero cost there is something to learn at this timeframe: short-horizon mean reversion is positive at all three horizons on train and at two of three on validation (the 60-bar rule is flat there). The random rows show how large chance alone gets on 24 episodes (1.61). Reversal is the yardstick for the evolved agents: a run that cannot match these rules has not learned anything. At 24 to 115 entries per 1,000 bars none of them would survive a realistic cost, which is what the later phases are for.

**Compute.** The full table (1,062,197 rows) loads in about 8 seconds; the episode index builds in 0.1 seconds.

**Memory.** A full-table test run of the run-1 config exhausted a 16 GB laptop about four minutes in, during a validation generation. A watchdog-guarded rerun on a 60,000-row table found two causes, both older than this work and both made worse by it:

| | Before | After the fix |
|---|---|---|
| Neuron processes | 67 MB each within one second of the first evaluation, growing with every bar until the agent's turn ends. Neural encoding (90 to 600 inputs per bar, per-cycle weight updates under plasticity) and the longer per-generation life of an agent in episode mode multiply it; the default substrate encoding mostly hid it, because CPPN neurons see only a few coordinates per query | No growth |
| Price loader | 92 MB held for a 3.5 MB file, i.e. about 26 bytes per byte of file, re-incurred every 5 seconds. For the 63 MB file that extrapolates to roughly 1.6 GB, and more during garbage collection (the VM was at 3 GB while idle) | Not measured separately; the file is parsed once, line by line |
| Whole VM, 60,000-row table, 24 agents, 6 generations | Over 1.1 GB one second after the agents started (halted by the watchdog) | Peak 142 MB; the run completed in 97 seconds on 4 cores |

**Full-table run after the fixes** (the real run-1 config: 24 agents, 15 generations, 2 tune and 4 score episodes, 24 validation episodes every second generation, default schedulers, no memory limit):

| | Result |
|---|---|
| Completion | 15 generations in 3 minutes 10 seconds; no errors, no timeouts |
| Memory | Peak 508 MB in the VM, of which 262 MB is the price table; process memory between 34 and 203 MB with no upward trend |
| Speed | About 3 to 9 seconds per generation, 10 to 30 with the validation pass. Before the neuron fix the same generation took about 32 seconds |
| Activity | Median entries per episode rose from 57 to about 480 and stayed there: with trades free, nothing discourages flipping nearly every other bar |
| Network size | Average 1.0 → 1.6 neurons, largest 5 |
| Tuning carry-over | Rank agreement between tuning fitness and score fitness averaged about 0 (range −0.42 to 0.52): at this noise level the weight tuning mostly fits its two tune episodes |
| Generalisation during the run | Rank agreement between score fitness and validation fitness averaged about 0.03 over the 8 validated generations; mean validation timing t of the population about −0.1 |
| Final population on validation | 18 distinct behaviours, 17 eligible; one with p < 0.05 in the shift test against 0.9 expected by chance; best timing t 2.00, against 3.17 for the 240-bar reversal rule |

Reading: the machinery works at full scale and is cheap to run, and this run shows no learned skill, which is the expected result for 24 agents scored on 4 episodes for 15 generations (fitness noise is ±1 and the reversal-sized edge is about 0.3·√4 = 0.6). Runs 2 to 4 of the config raise the score episodes and the population to 48. I estimated an hour and a half for the whole four-run config from this run; that estimate was wrong by more than ten times, because cost per bar grows with network size. The actual trial is in 8.5.

### 8.4 Things to know when running it

- Per-run changes to `tuning_duration`, the selection function, the encoding, activation functions and mutation operators have no effect after run 1: those values are stored in each agent and in the population record when they are created, and cloning into the next run keeps them. This was already true of the old 12-phase config (its change from `{const,10}` to `{const,20}` at run 3 never took effect). Only settings read live from `config` change between runs: windows and episodes, fitness function, spread, population size, generation limit, survival percentage.
- A simulator or neuron process that crashes, or a network that mutation left unable to finish a bar, no longer waits for a flat 10-minute timeout. The exoself's watchdog ends the agent after `stall_timeout_ms` (2 minutes) without a completed bar, or as soon as a mailbox passes `runaway_queue_limit`. See 8.5.
- Do not estimate run time from the first generations. Cost per bar rose 37 times over the four-run trial as networks grew; the `ms_per_bar` and `gen_seconds` columns of the generation summary show it as it happens.
- Episode mode works with the generational algorithm only; `steady_state` has no generation boundary at which to redraw episodes.
- With the two memory fixes the full table runs locally in about 0.5 GB (the table itself is about 260 MB in ETS). Run one VM at a time; for anything experimental, a small table and a memory watchdog are still the safe way to test.
- In the last diagnostic the total number of queued messages across all processes peaked near 21,000 for a short time. Memory stayed low, so it was not pursued; which process held them is unknown.

### 8.5 The four-run trial, and what changed after it

**The trial.** The four-run config was run at full size on 2026-11-10 and stopped by hand after 11.5 hours, at generation 94 of 140 (run 4, generation 13). All three hand-overs between runs worked, there were no crashes, and memory stayed near 0.75 GB. Scores below are `alpha_t`: 0 means no timing skill and ±1 is ordinary noise.

| Check | Result | What pure chance gives |
|---|---|---|
| Agent scores on fresh training episodes (3,369 scores) | mean 0.01, standard deviation 0.95; 3.3% to 5.2% above 1.645 per run | mean 0, standard deviation 1, 5% |
| Survivors re-scored one generation later (1,626 cases) | +0.64 when selected, then +0.01 ± 0.02; newcomers +0.015 | 0 |
| Weight tuning | best tuning fitness +1.10 on the tune episodes, the same weights +0.01 on the score episodes, correlation −0.004 | 0 |
| Rank agreement, score against validation (48 generations) | 0.005 ± 0.031 | 0 |
| Each generation's champion on validation (60 champions) | mean −0.37 ± 0.11; one above 1.645, four below −1.645 | mean 0; three each side |
| Every validation pass (1,504) | mean −0.28, standard deviation 0.96; 2.5% above 1.645 | mean 0, 5% |

Reading: no learned skill, on training data or on validation. Selection kept lucky agents, not skilled ones, and tuning fitted its few tune episodes. This is what the roadmap expected of stage 1 ("Expect no edge yet"): the objective now measures honestly, and the inputs and the one-minute decision clock (stage 2) are what has to change. The agents were in the market about 86% of the time and changed position about 210 times per 1,000 bars. The slight negative lean on validation is shared by close relatives and should not be read as an effect.

**Two defects the trial exposed.**

1. *Slow agents were failed.* `exoself` gave any agent that took more than 10 minutes on a pass the ruined-account fitness (−2000), even after it had been scored. This happened 269 times, from run 3, generation 8. In run 4's validation generations on average 17 of 48 agents finished in time, so every second generation selected on speed. The first 48 generations had no timeouts and show the same no-skill result.
2. *Run time.* Wall time per bar rose from 0.28 ms in run 1 to 10.4 ms in run 4 as networks grew from 1 to about 15 neurons, each linked to 90-, 270- or 600-value sensors. Tuning took 8.1 evaluations per agent per generation, about two thirds of the compute in run 4. The whole config would have taken about 25 hours.

**What changed.**

| Change | Where | What it does |
|---|---|---|
| Progress reports | `cortex.erl` | The cortex tells its exoself every `progress_every_bars` (50) completed bars |
| Watchdog | `exoself.erl` | Every 100 ms while an evaluation runs. *Stalled*: no bar completed for `stall_timeout_ms` (2 minutes); a slow network keeps reporting and is never failed for being slow. *Runaway*: any process of the network has more than `runaway_queue_limit` (5,000) messages waiting. Either ends the agent with the failure fitness |
| Score saved before validation | `exoself.erl` | Fitness and weights are written as soon as the score pass ends. A failure during validation keeps the score and logs validation as missing |
| Processes killed at the end of every turn | `exoself.erl` | A neuron that fires forever never reads its mailbox, so the terminate message alone left it running. Not applied in live trading |
| Tuning off | `config.erl`, `exoself.erl`, `exp_runner.erl` | `episode_tuning` (default false). Weights change through the `mutate_weights` mutation and are judged on the score episodes. The four runs now use 8, 16, 24 and 32 score episodes, twice as many as before |
| Champion check cannot block | `population_monitor.erl`, `eval_protocol.erl` | The population monitor's wait for the champion's validation run now ends if that agent stalls or its exoself dies |
| Summary columns | `eval_protocol.erl` | `stalls`, `runaways`, `gen_seconds`, `ms_per_bar`, `survivors`, `survivor_prev`, `survivor_rescore`, `newcomer_score`, appended to each `GEN_SUMMARY` line |

Not changed, by the owner's decision: network growth. There is no cap on neurons and no size penalty; larger networks are wanted.

**The runaway case.** The first version of the watchdog only looked for silence. A test with a neuron that has no input showed that this is not a stall: the neuron fires as fast as the scheduler allows, the actuator it feeds keeps completing bars on its constant output, and the mailboxes fill. Unguarded, the VM went from 66 MB to 824 MB in 6 seconds, with 5.8 million messages queued, before the test's memory guard halted it. With the mailbox check the same agent is ended about 150 ms after it starts (120 ms of that is the test's own wait); memory went 65 MB → 133 MB → 65 MB.

**Tests** (60,000-row table, one VM, 4 schedulers, memory guard at 800 MB):

| Test | Result |
|---|---|
| A working agent whose pass takes longer than the stall limit (limit 400 ms, pass 575 ms to 2.2 s) | Finishes with its real fitness; no stall recorded |
| Control: the same with progress reports off | Failed with −2000, so it is the reports that keep a slow agent alive |
| A network frozen during scoring (one neuron suspended, limit 2 s) | Ended after 2.0 s with −2000; stall recorded; no process left |
| A network frozen during validation | Keeps its score (0.287 in that run); validation logged as missing; no process left |
| A neuron firing forever | Ended as a runaway with −2000; memory back to its starting level; no process left |
| Three runs in a chain: tuning off, tuning on, legacy mode | Completed, no stalls, no errors, peak 101 MB |
| Substrate encoding, 2 generations | Completed, no stalls |
| `eval_protocol:g0()` | All ten checks pass |

**A mutation bug the watchdog exposed.** In the first minutes of the next full run the watchdog ended four agents as runaways, and they were not neurons firing forever: each had a neuron holding one or two unread messages per bar. All four traced to `genome_mutator:link_ToNeuron/4`. For a neuron with `neuromodulation` plasticity, half of the new links are meant to become modulatory inputs, but the result was written to `input_idps`, which replaced every standard input of the neuron with that one link. The elements that had been feeding it still listed it as an output and kept sending. `cutlink_ToNeuron/3` had the same mistake. The checks in `mutate_check.erl` repaired this kind of mismatch between sensors and neurons, not between two neurons.

| Change | Where |
|---|---|
| Modulatory links go to `input_idps_modulation`, in linking and in cutting | `genome_mutator.erl` |
| A link becomes modulatory only once the neuron has a standard input, so no neuron is left without a signal input (without this, 44 of 218 neurons in the test ended up that way) | `genome_mutator.erl` |
| `add_inlink` does not offer an element that already feeds the neuron as a modulatory input | `genome_mutator.erl` |
| `mutate_weights` perturbs modulatory weights too; with tuning off nothing else would ever change them | `genome_mutator.erl` |
| The connectivity checks count modulatory inputs as inputs; otherwise they would "repair" a valid modulatory link by adding a second link from the same element | `mutate_check.erl` |

This switches on a feature that had never worked: before the fix no neuron ever had a modulatory link.

| Test (60,000-row table, small VM) | Before the fix | After the fix |
|---|---|---|
| Wiring check after every mutation, all neurons `neuromodulation`, 40 mutation rounds per agent | 12 of 12 agents develop links recorded on one end only; 271 such links in the final networks; no modulatory links | 0 of 24 agents; 908 successful mutations; 454 modulatory links |
| 10 generations of 12 agents, all neurons `neuromodulation`, 8,000-bar score passes | 13 agents ended by the watchdog | none ended |
| Three runs in a chain (tuning off, tuning on, legacy), default plasticity list | | Completed, no stalls, runaways or errors |
| Substrate encoding, 2 generations | | Completed, no stalls or runaways |

Before this fix such agents ran to the end of every pass with a growing mailbox in the affected neuron, and each receive there had to skip the pile. That may have contributed to the slowdown of the first trial; it was not measured there. In the small before/after run above, time per bar was 0.99 ms at 1.9 neurons before and 0.58 ms at 2.4 neurons after, on a machine that was busy with another run.

**Still open.**

- Populations created before this fix can contain neurons with one-sided links; the fix does not repair existing genotypes.
- The full four-run config was re-run with these changes; see 8.6.
- 5,000 queued messages and 2 minutes of silence are judgment calls. A healthy process never has more messages waiting than it has inputs, so the first only needs raising for neurons with thousands of input links.
- A runaway is ended, not prevented. The post-mutation checks remain the first line; each one ended here is logged with its genotype snapshot (`STALL | kind=runaway` in the benchmarker log, `timeout:` in `qStatus.log`).

### 8.6 The second trial, and what the agents learned

> **Correction (8.9).** The score used here credited reversal behaviour whether or not it made money. The scores of the agents and of the reversal rules in this section are about twice what the money supports.

**The trial.** The four-run config was run again on 2026-11-10 with every change in 8.5, and stopped by hand at generation 124 of 140 (run 4, generation 44 of 60) after about six hours. No stalls, no runaways and no errors in about 4,800 agent evaluations. Networks grew from 1 to 23 neurons on average (largest 29); time per bar reached 10.4 ms.

| Run | Score on fresh training episodes (population mean) | Same agents on the 24 validation episodes |
|---|---|---|
| 1 | 0.10 | −0.48 |
| 2 | 0.44 | −0.54 |
| 3 | 0.79 | 0.10 |
| 4 (44 generations) | 1.75 | −0.80 |

Unlike the first trial, selection worked on the training period: in run 4 the population median was above 0 in every generation (mean 1.78 ± 0.14), survivors selected at 2.05 scored 1.75 one generation later, and their offspring scored 1.69. Three things had changed at once (tuning off with twice the score episodes, the link fix, no false timeouts), so the cause is not isolated.

**What they learned** (all 48 agents of the final population, on 24 tiled episodes in each quarter from 2022Q4 to 2024Q4, with scripted rules on the same episodes; run on a copy of the database, 2025 data not used):

- *It is a 60-bar reversal.* The agents' position is against the price move of the last 60 bars on 68% of bars (30 bars: 61%; 5 bars: 52%). Episode by episode, their results correlate most with the 60-bar reversal rule (0.42). The 48 agents agree with each other on 82% of bars.
- *They do not beat the one-line rule.* Over the 168 training-period episodes the agents score 3.6 (range 3.0 to 4.3) and the 60-bar reversal rule 3.7. On 61 non-overlapping validation-period episodes the agents score 0.9 (range 0.1 to 1.7), the 60-bar rule 1.3 and the 240-bar rule 3.4.
- *The effect was strongest early.* Mean z per episode, agents against the 60-bar rule: October 2022 to March 2023, 0.51 and 0.51; April 2023 to March 2024, 0.23 and 0.21; April to December 2024, 0.11 and 0.22. In the most recent era the agents fall below the rule.
- *It is far too small to trade.* Measured from the price moves, the agents make 0.07 pips per trade on the training period (109 entries per 1,000 bars) and about 0.00 on the validation period; the 60-bar rule makes 0.09 and 0.05. A typical round-trip cost is 1.5 pips. The agents change position every 5.4 bars on average.
- *Significance on the official validation set:* none of 48 agents has p < 0.05 in the shift test.

**A correction to how validation was read during the run.** The fixed validation set has 24 episodes, and on it the population scored −0.9. On 48 other episodes from the same six months it scored +1.1, and on the 61 non-overlapping episodes of both sets together +0.9. Twenty-four episodes are too few: a score moves by about ±1 with the choice of episodes, and a random policy scored 1.3 on the merged set. The validation result is "about zero", not "negative".

**Reading.** With trades free and a decision every minute, the easiest thing to find is short-horizon mean reversion in one-minute closes, and evolution found it. A scripted rule captures it as well, it has weakened since early 2023, and it is worth less than a tenth of a pip per trade. More generations of this setup would refine the same effect. What has to change is the objective and the clock (cost in the objective, a slower decision interval, feature inputs: stage 2), and two parts of the protocol: a validation set of at least 60 episodes, and the reversal rules as a hurdle every agent is compared with on the same episodes.

### 8.7 After the second trial: sensors, removal, cost, a larger validation set

Built on 2026-11-10/11 after 8.6, at the owner's direction: stay with EUR/USD, do not limit how often an agent may trade, add a reasonable cost, keep the raw inputs and offer more sensor types next to them, and let networks shed parts as well as grow.

| Change | Where | What it does |
|---|---|---|
| Larger validation set | `exp_runner.erl` | 60 validation episodes, run every 4 generations (was 24 every 2) |
| Plain rules in the report | `eval_protocol.erl` | `rev60_score`, `rev240_score`, `above_rule`, `rev60_val`, `rev240_val`, `val_above_rule`, `spread`: the 60- and 240-bar reversal rules on the same episodes, by the same fitness function, at the same cost |
| Shared price arrays | new `fx_series.erl`, `population_monitor.erl` | The price table is packed once into binaries: closes, running sums, and 15-minute, hourly, 4-hour and daily bars. Built before the first agent starts; about 45 MB and 0.9 s for 1,062,197 rows |
| New sensor types | `sensor.erl`, `morphology.erl`, `fx.erl`, `config.erl` | `fx_PLT` (price list by timeframe: `plt_specs`, here 32 quarter-hours, 24 hours, 30 four-hour bars, 20 days), `fx_IND` (12 indicators: change over six ranges, distance from three moving averages, two activity ratios, place in the last day's range), `fx_Clock` (time of day, day of week). Offered next to `fx_PLI`, `fx_PCI`, `fx_Internals`; off unless switched on in config. About 1 microsecond per answer |
| Removal mutations | `genome_mutator.erl`, `config.erl` | `remove_neuron` (weight 20), `remove_inlink` (15), `remove_outlink` (15), `remove_sensorlink` (10), `remove_sensor` (4): about a third of the matching add weights. Neural encoding only |
| Cloning | `genotype.erl` | The mutation history may name removed elements, and the layer pattern now follows the clone's ids (it used to keep the parent's) |
| Cost | `exp_runner.erl` | `account_spread` 0, 0.25, 0.5 and 1.0 pip over the four runs |

**Not built: a shared table of answers for the existing sensors.** Measured first: building the answers for all three existing market sensors takes about 23 microseconds per bar, against about 290 for a whole 24-neuron agent. The rest is per link: copying each vector to every neuron linked to the sensor (5 microseconds for 600 values), multiplying it by the weights (7), and updating plastic weights. Sharing answers could save 8% at most, so only the arrays were built; the new sensors read from them directly.

**Guards on removal.** Each removal operator exits, and is rolled back, unless afterwards every neuron has a standard input other than its bias and at least one output, every sensor is connected, the actuator has an input, a sensor and a neuron remain, every neuron can be reached from a sensor, and the `mutate_check` checks pass. The reachability rule is what prevents runaways: a neuron reached from a sensor fires at most once per bar, and a group that only feeds itself does not wait for bars at all.

**Tests** (60,000-row table unless stated):

| Test | Result |
|---|---|
| No look-ahead: every new sensor's answer at a row, against the answer from a table cut off at that row | Identical at 43 rows on the small table and 9 on the full table |
| Scale of the new sensors' values | Median absolute value 0.30 to 0.53, 95th percentile 1.0 to 2.4; the existing 270-minute list: 0.36 and 1.56 |
| Wiring after every mutation with removals enabled, 32 agents, 120 rounds each | 3,679 successful rounds; 253 neurons and 73 sensors removed; networks of 36 to 86 neurons; no fault at any point; all 32 clones consistent |
| Four runs in a chain: new sensors and removals at zero cost, the same at 0.5 pip, tuning on, legacy mode | Completed; no stalls, runaways or errors |
| The five watchdog tests of 8.5 | All pass |
| Substrate encoding, 2 generations; `g0` at zero cost and at 1 pip | Completed; 20 of 20 checks pass |
| The real run-1 config and five generations of run 2 on the full table | Run 1 in about 1.5 minutes; peak memory 564 MB; no stalls, runaways or errors |

One defect found by the full-table test and fixed: every simulator whose first request arrived before the arrays existed built its own copy, all at once (peak 999 MB, first generation 31 s). The arrays are now built once, under a lock, before the agents start (564 MB, 12 s).

**What to expect from a run, from these tests.** With cost on, scores fall sharply. In the small chain at 0.5 pip, entries per episode fell from about 130 to about 5 within a few generations and no agent had a positive score; the 60-bar rule was negative too. On the full table at 0.25 pip the population median started at −11. On the 60 validation episodes the 240-bar rule scores 4.2 at zero cost and 2.1 at 0.25 pip; the 60-bar rule 2.7 and −1.2. Since nothing limits trading, the cost is the only pressure, and the likely outcome in runs 2 to 4 is a population that trades as little as the eligibility rule allows (`min_trades_per_episode`, 2).

**Still open.**

- The new sensors are not available on the live-trading path (`live_scape.erl`); an agent that uses them returns zeros there.
- The full four-run config has not been run with these changes; only run 1 and the start of run 2.
- Removal operators exit on substrate-encoded agents.
- The cost figure (1.0 pip at the end of the ramp) is a placeholder chosen as reasonable, not the owner's broker's figure.

### 8.8 The third trial: cost ramp, new sensors, removal mutations

> **Correction (8.9).** The rule scores in this section (the two "240-bar rule" columns, the fourth finding and the last sentence of the reading) came from a flaw in the score. In money the 240-bar rule loses at a quarter pip on both spans.

**The trial.** The four-run config of 8.7, run to completion on 2026-11-11: 140 generations in 3.6 hours, no stalls, no runaways, no errors in about 5,700 agent evaluations. Networks grew from 1 to 26 neurons on average (largest 31). Time per bar in run 4 went from 2.2 to 5.4 ms, about half the second trial's figure at the same network sizes; the cause was not isolated.

| Run | Cost per round trip | Median score | Best score (mean over generations) | Entries per episode, start → end | 240-bar rule, training | Agents on validation | 240-bar rule on validation |
|---|---|---|---|---|---|---|---|
| 1 | 0 | 0.00 | 1.72 | 284 → 304 | 1.09 | 0.14 | 4.20 |
| 2 | 0.25 pip | −2.68 | −0.96 | 89 → 50 | 0.75 | −9.9 | 2.08 |
| 3 | 0.5 pip | −4.02 | −2.33 | 41 → 5 | −0.12 | −7.3 | −0.05 |
| 4 | 1.0 pip | below −1 | −0.53 | 6 → 3 | −2.88 | −19.9 | −4.30 |

**Findings.**

- *No agent makes money once trades cost anything.* The share of agent scores above 1.645 is 4.3% in run 1 (5% by chance) and 0.0% in runs 2, 3 and 4. The share above zero is 1%, 3% and 5%.
- *The population learned to trade as little as the rules allow.* By the end of run 4 an agent opens about 3 positions per 1,000 bars and is in the market 15% of the time; the eligibility rule requires 2. Losses per episode shrank accordingly: $14, $13, then $4.
- *Run 1 was too short to learn the reversal.* Fifteen free generations left the median at 0.00, as in the second trial's run 1. The cost arrived before the population had found anything to protect.
- *A plain rule did better than every agent at a quarter pip.* The 240-bar reversal rule scores +0.75 on training episodes and +2.08 on the 60 validation episodes at 0.25 pip. At 0.5 pip it is at zero, and at 1 pip clearly negative.
- *The new sensors spread.* By run 4 every agent has the indicator sensor and 99% have the 4-hour price list, alongside both raw price lists and the chart; agents average 6.3 sensors. Whether they help or were carried along is not known.
- *Survivors are better than mutants.* In run 4 survivors score −1.4 to −1.6 and newcomers −7.4: a mutation usually makes an agent trade more, and trading more costs.

**Two things in the scoring that the cost exposed.**

1. `alpha_t` divides by the spread of the agent's own timing result. With a cost, an agent that is exposed only briefly gets a very large negative score (the cost is fixed, the divisor is small), so scores are no longer on the "0 ± 1" scale: values of −10 to −100 appear, and the population average on validation reads −20. The direction is right, the size is not comparable between runs.
2. `above_rule` is misleading under cost: at 1 pip about 35 of 48 agents are "above the rule" only because the rule trades 24 to 58 times per episode and they trade 3. Under cost the hurdle that matters is zero.

**Reading.** The search does what the objective rewards: with free trades it found the reversal (second trial), with costs it found the cheapest way to stay eligible. Neither is a model to trade. At one-minute decisions on EUR/USD nothing in three trials has beaten a cost of half a pip or more. The one lead is the slow reversal at very low cost.

### 8.9 A flaw in the score, and the staged run

**The flaw.** Until 2026-11-11 the per-episode score was `timing = sum(u·r) − mean(u)·sum(r) − cost`, with `mean(u)` the agent's average position in that same episode. The middle term was there so that holding one side in a drifting episode scores nothing. It does that, but it also pays any rule that sells after a rise: such a rule is short, on average, in exactly the episodes that rose, so `−mean(u)·sum(r)` is positive for it whether or not it makes money. On a random walk the expected credit is about E|change over N bars| per episode: about 7 pips for a 60-bar reversal rule and 11 for a 240-bar one. Momentum rules are charged the same amount.

Measured on 616 consecutive 1,000-bar episodes of the train span (`score_vs_money.js`, which reproduces the rule scores in the trial logs):

| Rule | Money before cost, pips per episode | Credit from the correction, pips per episode | Old score on 16 episodes, free | Money after 0.25 pip, pips per episode |
|---|---|---|---|---|
| 60-bar reversal | +6.2 | +7.0 | +1.41 | −8.4 |
| 240-bar reversal | +1.1 | +11.7 | +1.52 | −4.9 |
| 60-bar momentum | −6.2 | −7.0 | −1.41 | −20.8 |
| 240-bar momentum | −1.1 | −11.7 | −1.52 | −7.1 |

**What this changes in earlier sections.**

- 8.6: the agents' score of 3.6 and the 60-bar rule's 3.7 were both about twice what the money supports. The 60-bar reversal is real but small: about 6 pips per 1,000 bars before cost, 0.1 pip per trade. Selection was pulled towards reversal by the score itself, on top of that small real effect.
- 8.8: "the 240-bar rule scores +0.75 on training and +2.08 on validation at a quarter pip" was the credit. In money the rule loses 4.9 pips per episode on training and 2.2 on the 60 validation episodes at a quarter pip. "The one lead is the slow reversal at very low cost" is withdrawn.
- The shift test (`eval_protocol:shift_test/3`) used the same formula for the observed value, so it favoured reversal-type agents too.
- What stands: no agent made money under any cost in the third trial (their dollar P&L was negative), the run-health results, and the sensor and network-size observations.
- Why the checks missed it: `g0` tested constant positions and random trading, and both are unaffected. Nothing tested a rule whose position reacts to the price.

**The fix.** `timing = sum(u·r) − mu·sum(u) − cost` and `z = timing / sqrt(sum(u²·r²))` (`fx:ep_stat/4`). `mu` is one number fixed before any agent runs (`episodes:drift/1`): for train episodes the average change per bar over the whole train span, for validation episodes the same average over the validation set (so a constant position scores exactly zero over the set). Nothing in the score depends on the agent's own average position any more.

`g0` gained two checks. `eval_protocol:random_walk_check/0` runs the reversal rules on 399 episodes of a random walk made for the purpose: mean z is +0.002 (60 bars) and −0.081 (240 bars) with a standard error of 0.050. The old formula gave about +0.2 and +0.3. The other check is that a constant long position scores zero over the validation set. All eleven checks pass on the real table.

The rules under the corrected score (300 consecutive train episodes, 60 validation episodes; "score" is mean z × √32, what a 32-episode evaluation would show):

| Span | Rule | Entries per episode | Money before cost, pips per episode | Score, free | Money after 0.25 pip | Score at 0.25 pip |
|---|---|---|---|---|---|---|
| train | 60-bar reversal | 58 | +5.8 | +0.93 | −8.8 | −1.44 |
| train | 240-bar reversal | 23 | −0.4 | +0.03 | −6.2 | −1.11 |
| validation | 60-bar reversal | 58 | +1.7 | +1.02 | −12.7 | −1.73 |
| validation | 240-bar reversal | 25 | +4.1 | +1.08 | −2.2 | −0.35 |

**What plain rules earn after a quarter pip.** Train span only, in money (`selective.js`). The rule: when the price has moved more than k typical moves over the last N bars, take the other side; leave when the move has been undone. k = 0 is the plain rule.

| N | k | Entries per 1,000 bars | Money before cost, pips per trade | After 0.25 pip, pips per 1,000 bars | t over 616 blocks | First half / second half of the span |
|---|---|---|---|---|---|---|
| 60 | 0 | 59 | 0.11 | −8.2 | −4.7 | −6.3 / −10.1 |
| 60 | 1 | 6.9 | 0.52 | +1.8 | 1.3 | +4.0 / −0.3 |
| 240 | 0 | 30 | 0.09 | −4.7 | −2.7 | −3.6 / −5.9 |
| 240 | 1 | 2.0 | 0.72 | +0.9 | 0.7 | +2.2 / −0.3 |
| 720 | 1 | 0.8 | 2.01 | +1.4 | 1.1 | +0.8 / +2.0 |
| 720 | 1.5 | 0.5 | 4.35 | +1.9 | 1.7 | +2.7 / +1.1 |

Being selective raises the money per trade from 0.1 pip to between 0.5 and 4 pips, which is the direction a rising cost pushes in. But none of the fifteen rules tried is reliably above zero after a quarter pip (best t 1.7), and the 60- and 240-bar ones are positive only in the first half of the span. An agent doing exactly the best of these would score about +0.7 on a 32-episode evaluation, against noise of ±1. **On this data no simple rule has a reliable profit after a quarter pip.** Whether a network can find something conditional that the rules cannot is what the staged run tests.

**The staged run.** The owner proposed stages: free trading with trading itself rewarded, then a cost with trading still rewarded, then profit. What was built keeps the stages and changes what is rewarded:

- The objective is money after cost in every stage; only the cost changes. Run 1 is free (30 generations), run 2 raises the cost a little every generation from nothing to a quarter pip (`cost_schedule`, 60 generations), run 3 holds a quarter pip (40 generations). The same population goes through all three.
- Trading itself is rewarded only up to the existing minimum of 2 positions per episode (below it, agents are ranked by how many they open). Beyond that it is not, for two measured reasons: with free trades agents already open about 300 positions per 1,000 bars unprompted, and the rules that come closest to paying after a quarter pip open between 0.5 and 14.
- Validation always costs a quarter pip (`validation_spread`), so the validation columns read at the real cost from the first generation.
- Population 48 and 32 score episodes from the start (run 1 of the third trial, with 24 agents and 8 episodes, learned nothing).

New config keys: `cost_schedule`, `validation_spread`, `pip_size`. New `GEN_SUMMARY` columns: `positive` (agents above zero after cost), `gross_pips_trade` (money before cost per trade; must exceed the cost in pips), `net_pips_ep` (median agent's money after cost per episode), `val_spread`, `val_positive`, `val_net_pips_ep`. The pips columns are plain money with no drift taken out.

Tested with a three-run experiment of 8 agents, 4 score episodes and 3 + 6 + 2 generations on the full table: completed, the cost read 0, 0.0625, 0.125, 0.1875, 0.25, 0.25 pip over run 2, validation at 0.25 pip throughout, peak memory 367 MB.

Run time is not predictable from this. For reference, at 48 agents and 32 episodes the third trial took 83 seconds per generation with 14-neuron networks and 169 with 24-neuron ones, and about 2.5 times that in generations that validate.

**Still open.**

- Selection sees one 32-episode evaluation per generation, where the best known rule would show +0.7 ± 1. Survivors are re-scored on fresh episodes every generation, so their evidence could be pooled across generations (the score at the current cost can be rebuilt exactly from two running sums per agent). Not built.
- Episodes are 1,000 bars, so a rule that holds for 400 to 700 bars gets one or two trades per episode. Longer episodes at the same total number of bars would suit slow behaviour better; not changed, to keep the validation set as it is.
- The items listed at the end of 8.7.

### 8.10 The fourth trial: the staged run

**The run.** 2026-11-11, the three-stage config of 8.9 unchanged: 48 agents, 32 score episodes, 130 generations in 53 minutes. No stalls, no runaways. Networks grew from 1 neuron to 5 by the end of stage 1 and 11 by the end of stage 2, and were between 11 and 17 in stage 3 (largest 20).

| Stage | Cost, pips | Generations | Positions per episode | Money before cost, pips per trade | Median score | Median money after cost, pips per episode | Minutes |
|---|---|---|---|---|---|---|---|
| 1 | 0 | 30 | 100 (16 to 194) | 0.01 | +0.13 | +0.6 | 6 |
| 2, first 10 generations | 0 to 0.04 | 10 | 32 falling to 4 | 0.13 | +0.77 | +3.3 | 3 |
| 2, the other 50 | 0.04 to 0.25 | 50 | 4.3 | 0.41 | +0.52 | +2.7 | 18 |
| 3 | 0.25 | 40 | 3.2 (2.3 to 4.9) | 0.61 | +0.49 | +2.1 | 26 |

All columns are means over the stage's generations, on that generation's training episodes.

- **Stage 1 learned nothing that pays, even free.** About 100 positions per episode at 0.01 pip each. The 60-bar reversal rule scored +1.10 on the same episodes; on average 9 of 48 agents beat the better of the two rules. (In the second trial the agents found that rule because the old score paid for it, 8.9.)
- **Stage 2 did not produce gradual selectivity.** Trading fell from 32 positions per episode to 4 within nine generations, by which time the cost was 0.04 pip, and stayed there. There was no profitable frequent trading for a rising cost to refine.
- **Stage 3 sat just above the minimum.** 2.3 to 3 positions per episode at the end, against `min_trades_per_episode` 2.
- **The population wins or loses together.** In stage 3 at least 40 of 48 agents were above zero in 23 generations, and at most 8 in 11 generations. A generation's score says more about which 32 episodes were drawn than about the agents.

**What the survivors do.** The 24 agents left after the last selection were replayed bar by bar on consecutive episodes (scratch `t4.erl`, `t4_money.js`, `t4_character.js`). They are one family: 15 to 17 distinct behaviours that differ in the second decimal. All read the 60-minute chart of the last 24 hours (`fx_PLT [60,24]`); a third also read a list of the last 90 or 270 closes. None reads the clock, and the position does not depend on the hour.

- 2.4 to 2.8 positions per episode, in the market about 80% of the time (40% long, 40% short), median holding time 3 hours, changes mostly at hour steps.
- They fade the move of the last 8 to 12 hours. After a rise over the previous 720 bars the top agent is short 69% of the time and long 11%; after a fall, long 70% and short 12%. Over 60 bars or less there is no relation.

**In money.** Top agent, a quarter pip per position, plain pips with nothing taken out:

| Period | Episodes | Positions per episode | Money before cost, pips per trade | After cost, pips per episode | t | Total, pips | Months above zero |
|---|---|---|---|---|---|---|---|
| Training, Oct 2022 to Jun 2024 | 613 | 2.65 | +0.95 | +1.85 | +1.2 | +1,135 | 15 of 21 |
| Validation, Jul to Dec 2024, every episode | 171 | 2.49 | −0.47 | −1.80 | −0.8 | −308 | 1 of 6 |
| of which the 60 the run reported on | 60 | 2.32 | +0.41 | +0.36 | +0.1 | +22 | |
| of which the 111 never looked at | 111 | | | −2.97 | −1.0 | | |

The other survivors are within 1 pip of these numbers in every row.

**Reading.**

- In the run the stage 3 median score averaged +0.49 with t = 3.5 over 40 generations, and survivors beat newcomers by 0.39 (t = 4.9). So selection kept something that holds on fresh draws from the training period. On consecutive training episodes it is worth +1.85 pips per episode (t = 1.2 in money, 2.0 in z). The size agrees with the run; the in-run t is too high because its 1,280 episode draws come from 613 episodes of data.
- It did not carry over: −1.8 pips per episode on the whole validation period.
- Neither number is distinguishable from zero, and nor is the gap between them (3.7 ± 2.8). An agent like this makes two or three decisions per episode and an episode's result varies by 30 to 40 pips. Confirming +2 pips per episode at t = 2 takes about 1,200 unseen episodes, three and a half years. The table has six months of validation and nine months of 2025.
- So for slow behaviour this data can neither show a profit nor rule one out, and for fast behaviour the cost rules it out (8.9). That is the position after four trials.
- The 60 validation episodes flatter these agents: +0.4 pips per episode on them against −3.0 on the other 111 of the same period. The gap is within noise, but it is a reason to validate on all of them. (The 60 also rose 1.7 pips per 1,000 bars while the whole period fell 2.0, which matters for any agent that leans one way.)

**The plain rules, out of sample.** The selective reversal rules of 8.9 were fixed on training data. Applied to the validation period for the first time, with the size of a typical move taken from training (`selective_oos.js`; a quarter pip; pips per 1,000 bars, t in brackets; 2025 not read):

| N | k | Positions per 1,000 bars | In the market | Training | Validation | Both periods |
|---|---|---|---|---|---|---|
| 60 | 1 | 6.9 | 37% | +1.8 (1.3) | −1.4 (−0.7) | +1.1 (0.9) |
| 60 | 1.5 | 3.7 | 22% | +1.0 (0.8) | −1.3 (−0.8) | +0.5 (0.5) |
| 240 | 0.5 | 4.5 | 67% | +0.6 (0.4) | +1.6 (0.7) | +0.8 (0.6) |
| 240 | 1 | 2.0 | 41% | +0.9 (0.7) | +2.0 (1.1) | +1.2 (1.0) |
| 240 | 1.5 | 1.1 | 25% | +0.3 (0.3) | +2.8 (1.7) | +0.9 (0.9) |
| 240 | 2 | 0.6 | 15% | +0.1 (0.2) | +2.8 (2.1) | +0.7 (1.0) |
| 720 | 0.5 | 1.8 | 72% | +1.3 (0.8) | +0.1 (0.1) | +1.0 (0.8) |
| 720 | 1 | 0.8 | 47% | +1.4 (1.1) | +1.8 (1.0) | +1.5 (1.4) |
| 720 | 1.5 | 0.5 | 28% | +1.9 (1.7) | +0.7 (0.6) | +1.6 (1.8) |
| 720 | 2 | 0.2 | 16% | −0.1 (−0.2) | +0.4 (0.5) | 0.0 (0.0) |

The plain rules (k = 0, always in the market) lose in both periods: −8.2 and −11.2 (N = 60), −4.7 and −0.4 (240), −1.1 and −5.4 (720).

- Seven of the eight selective rules with N = 240 or 720 are above zero after cost in both periods. Of the 60-bar ones only the loosest (k = 0.5: +1.0 and +0.5) is.
- This is not evidence of a profit. The rules overlap heavily, the best t over both periods is 1.8, and the best of fifteen unrelated rules with no edge would show about 1.7.
- It is the only family tried in this project that the unseen data has not contradicted. What it has in common: wait for a move that is large for its horizon (4 to 12 hours), take the other side, leave when it is undone, and otherwise stay out.

**The minimum number of trades is in the way.** Six of those seven rules open between 0.5 and 2 positions per 1,000 bars, and five are out of the market more than half the time. `min_trades_per_episode` 2 makes them ineligible or borderline. The agents fade the same slow moves but have to keep trading, so they stay in the market 80% of the time and sit at 2.3 to 2.8 positions per episode. The always-in version of the slow fade is the one that loses.

**What to change, in order of what the evidence supports.**

1. Lower the minimum from 2 positions per episode to about 0.25 (8 per 32-episode evaluation), so that waiting is allowed. The ranking below the minimum stays, so "never trade" is still at the bottom.
2. Pool the evidence of survivors across generations (the design of 8.9 was replaced by a relative one, built in 8.11). An agent that opens one position per episode has 32 trades behind its score; selection on that is close to a draw. Pooling is the cheap way to more evidence per agent; three times the score episodes is the simple way and costs three times the run time.
3. Validate on every episode of the validation period (171) instead of 60.
4. More history. Three years cannot confirm slow behaviour whatever the method. Ten years or more of EUR/USD one-minute data would turn the ±1.6 pips per episode of the training figure into ±0.7 and leave three or more years to confirm on.
5. Keep 2025 unread until there is one candidate to test, and read it once.

Items 1 to 3 were built for the overnight run (8.11). The owner's idea of rewarding trading was not built either (8.9), and this trial is the evidence against it: trading was plentiful and worthless when free, and what has not been contradicted trades rarely.

### 8.11 The overnight run: what was changed for it

**The question.** After the fourth trial the owner proposed a far longer run, of the order of 10,000 generations. The reading of 8.10 says run length was not the limit: the training score was flat for the last 90 generations (+0.52, then +0.49), validation stayed at zero, and network size had stopped rising. But 130 generations is short for growing structure, so the next run is one night long and is built to show whether more generations help. Nothing below adds data; more history (8.10, item 4) is still the larger lever.

**The night.** Six runs on one population, about ten hours:

| Run | Cost | Minimum positions per episode | Ends after |
|---|---|---|---|
| 1 | free | 2 | 30 generations |
| 2 | rising to a quarter pip | 2 | 60 generations |
| 3, 4 | a quarter pip | 2 | 2 h 20 min each |
| 5, 6 | a quarter pip | 0.5 | 2 h 20 min each |

- Runs 3 and 4 are the fourth trial's last stage with many times its generations (it had 40). They answer the owner's question at the old settings: does validation rise as generations accumulate?
- Runs 5 and 6 lower the trade minimum from the population run 4 leaves. They answer the question of 8.10: do the agents start to wait, and does that carry over?
- Each run saves its final population, so the night leaves four quarter-pip populations to replay on validation.

**What was changed.**

1. **A run can end on wall time** (`run_time_limit_s`, `population_monitor:run_time_up/2`). It is checked at a generation boundary after selection, so the run ends as it does at `generation_limit`. Generation time grew fourfold between 2 and 15 neurons in the fourth trial, so a generation count cannot fix the length of a night; a time limit can.
2. **Selection remembers earlier generations** (`selection_memory.erl`, `{selection_memory, 0.85}`). Each generation an eligible agent's score is taken relative to the median score of that generation, and selection ranks by a decaying average of those relative scores (weight 0.85 per generation back, and a newcomer's single result counts half). `GEN_SUMMARY` still reports raw scores.
   - Why: agents of one family share the luck of a draw. A family that is really better by 0.3 is still below the old one in about two generations of five, and while it is the minority one such generation removes all of it. With memory its proven members outlast a bad draw.
   - Why not the design of 8.9 (pool each survivor's own scores): a survivor's pooled score carries a tenth of this generation's luck and a newcomer's carries all of it. In a generation the whole population gains, every newcomer would outrank every survivor, and in the opposite kind none would. The fourth trial had 34 such generations in 40. Scores relative to the same generation's median do not have this problem, because everyone ran the same episodes.
3. **The trade minimum is set per run**, 2 then 0.5 (16 positions in a 32-episode evaluation). The fourth trial's agents matched the 720-bar rule at k = 0.5 almost exactly (1.8 positions per 1,000 bars plus a re-entry at the start of most episodes, 2.5 per episode, 72% in the market), and a minimum of 2 stops them moving to k = 1 (about 1.3 per episode), which is where the rule did better on validation. At 0.5 an agent that enters once per episode and holds is eligible again. Its score is zero on average, so it should lose to a family that is above zero on training; if the `holders` column fills up in runs 5 and 6, the family was not.
4. **Validation on every episode of the period** (171; `episodes_val` is now an upper limit), every 20 generations.
5. **`GEN_SUMMARY` reports `vm_mb` and `processes`**, so a leak shows in the log, and `memory_evals_avg`, `memory_evals_max` for the selection memory.

**Tested** with a four-run experiment of 8 agents and 4 score episodes on the full table (scratch `e10`):

- All four runs completed; the two time-limited ones stopped at 100 seconds after 118 and 63 generations.
- 190 generations in all, no stalls or runaways. `vm_mb` was between 358 and 368 and `processes` 70 at almost every summary (79 at most); peak memory 422 MB.
- The fitness selection sorted by was recomputed from the raw scores in `benchmarker.log` for all 1,520 agent-generations (`check_memory.js`): no difference. Memory kept a different agent than the raw score would have in 226 of 760 survivor places. The oldest history was 37 evaluations in a run of 63 generations, so no agent was kept for good.
- With 8 agents and 4 episodes there is no skill to find, and the population ended as holders at exactly one position per episode. That is the expected result for a population with nothing above zero, and it is what to look for in runs 5 and 6.

**Not tested.** The full-size config was not run. `exp_runner:continue/1` (resuming an interrupted experiment) exists and was not tried; if the night is interrupted, the finished runs' populations and summary lines are kept.

**How to read the night.**

- Runs 3 and 4: `val_t_all` and `val_net_pips_ep` in the validating generations (every 20th), against the generation number. Rising with generations supports longer runs. Training columns rising while these stay at zero says the limit is the data.
- Runs 5 and 6: `entries_per_ep` falling below 2 with `holders` staying low means the agents are waiting. Then the same two validation columns.
- `vm_mb` and `processes` should be level. `memory_evals_max` rising in step with the generation counter for hundreds of generations would mean one agent is never displaced.
- Afterwards each run's saved population can be replayed on the validation period (as in 8.10) for the money figures.

### 8.12 More history: the new price file, and the rules on years never seen

**The file.** `fx_tables/EURUSD_M1_MIDPOINT_UTC.txt` (2026-11-11): 9,036,929 one-minute bars from 2000-05-30 to 2026-11-06, in the format of `EURUSD1.txt`. No malformed line, no duplicate or out-of-order timestamp, no bar with its high below its low, volume −1 throughout. It is not Interactive Brokers data (there is no 15-minute daily break); its source is not recorded here. Checked with scratch `check_price_file.js`, `align_price_files.js`, `week_offsets.js`, `fix_check.js`. Only structure was examined from 2021 on; no return or rule statistic was computed there.

**What is right.**

- The prices. On the 1.0 million minutes it shares with the Interactive Brokers file, once the clocks are aligned, the median difference of the close is 0.05 pip, the 99th percentile 0.45 pip, and 99.1% are within half a pip.
- It has no Wednesday hole and no daily break, so it adds about 57,000 minutes inside the old file's range. (`EURUSD1.txt` lacks about five hours every Wednesday, from 16:30 UTC to the daily reopen, in 116 weeks up to the end of 2024: the seams of its weekly download chunks. US Federal Reserve announcements fall inside that hole.)
- It runs 14 months past the old file, to 6 November 2026.

**What is wrong.**

1. **The clock is not UTC in summer.** Against the Interactive Brokers file (which is true UTC: its week ends at 17:00 New York in all 154 weeks) the best alignment is a shift of 0 minutes in every winter month and 60 minutes in every summer month. The FX week ends at 17:00 New York, so the last bar before each weekend gives the clock's offset for that week in every year: the file is one hour ahead of UTC while summer time is in force, by the US rule through 2018 (50 to 52 weeks of 52 fit, each year from 2008) and by the European rule from 2019 (51 or 52 of 52, except 2023, see 2). 62% of all rows are affected. Applying that correction in memory puts the Friday close at the true time in every week of 2008, 2011 to 2019, 2021, 2022, 2024 and 2025, and aligns all 37 overlapping months at a shift of 0. Before 2007 the file stops 30 minutes before the weekly close and the rule cannot be verified.
2. **February to July 2023 is damaged.** Between 10% and 30% of the minutes of each of those months are missing (about 47,000 rows, in 681 one-hour holes). May 2026 has five holes of one or two hours.
3. **Before 2012 it is not a minute-by-minute series.** Median rows per day: 956 to 1,067 in 2000 to 2002, 1,124 to 1,226 in 2003 to 2007, 1,310 to 1,367 in 2008 to 2011, and 1,408 to 1,439 from 2012. Quotes are in whole pips until about 2011. Episodes and sensors count rows, so the sparse years would not mean what the recent ones do.

**Verdict.** Usable from 2012 (5.47 million bars to November 2026) after the clock is corrected and 2023 is repaired, not as it is. With the clock wrong, the hour of day would be off by one for seven months of the year, and training would disagree with the live feed, which is true UTC.

**The rules on 2012 to 2020.** The fifteen rules of 8.9 and 8.10, unchanged, thresholds still from the old training span of the Interactive Brokers file, a quarter pip per position, 3,343 consecutive 1,000-bar blocks that nothing in the project had seen (`rules_unseen.js`). The rules look only at the order of the bars, so the clock error does not touch them. 2021 to September 2022 was left unread as validation for later.

| N | k | Positions per 1,000 bars | Money before cost, pips per trade | After 0.25 pip, pips per 1,000 bars | t | Years above zero |
|---|---|---|---|---|---|---|
| 60 | 0 | 63.5 | 0.17 | −5.0 | −6.2 | 1 of 9 |
| 60 | 0.5 | 16.8 | 0.40 | +2.4 | 3.3 | 9 of 9 |
| 60 | 1 | 8.3 | 0.57 | +2.7 | 4.0 | 9 of 9 |
| 60 | 1.5 | 4.7 | 0.73 | +2.2 | 3.7 | 8 of 9 |
| 60 | 2 | 2.8 | 0.69 | +1.2 | 2.4 | 7 of 9 |
| 240 | 0.5 to 2 | 5.1 to 0.8 | 0.36 to 0.63 | +0.1 to +0.9 | 0.3 to 1.4 | 4 to 7 of 9 |
| 720 | 0.5 to 2 | 2.0 to 0.3 | 0.37 to 1.84 | +0.1 to +0.5 | 0.2 to 1.1 | 3 to 7 of 9 |

- **The slow rules, the lead of 8.10, are not confirmed.** All eight are above zero, but by +0.4 pip per 1,000 bars on average against a standard error of 0.6. The +1 to +2 of the three recent years was mostly those years.
- **The 60-bar selective rules are the first result in this project that clears noise on unseen data.** k = 1 (take the other side of a move of more than 10.7 pips within 60 bars, leave when it is undone): +2.7 pips per 1,000 bars after a quarter pip, t = 4.0, above zero in each of nine years. With fifteen rules tried, a t of 4 by chance has odds of about 1 in 2,000.

Year by year for k = 1: +4.8, +6.3, +1.8, +1.4, +3.4, +2.3, +2.5, +0.7, +0.9 (2012 to 2020). On the Interactive Brokers file it was +1.8 from October 2022 to June 2024 and −1.4 in the second half of 2024. The new file gives +1.2 and −1.0 for the same two periods, and matches the old one on the slow rules as well, so the two sources agree where they can be compared.

**Why it is not yet a profit** (`rules_diag.js`, k = 1 unless said):

| Check | Result |
|---|---|
| Cost of half a pip, not a quarter | +0.6 per 1,000 bars (t = 0.9). The trade earns 0.57 pip before cost; that is the break-even cost |
| Acting one bar late | +1.5 (t = 2.2). Two bars late +1.1, five bars late +0.4 |
| Where the money is made | The largest 1% of trades make 57% of it. Best trade +163 pips, worst −266 |
| When (2012 to 2018, 22:00 on the file's clock is 17:00 New York) | The two hours around the daily rollover hold 3.6% of the trades and 16% of the money; the two hours before London opens 23%; the hour of the US data releases 12% |
| Over time | +5.5 in 2012 and 2013, +0.8 in 2019 and 2020 |

- The pattern is real in the prices: after a fast, large move part of it comes back, and a quarter of that happens within the first minute.
- It is found where a flat quarter pip is least true. After a spike, around a data release and at the rollover, the real spread is several times its usual size, and the result does not survive a cost of 0.6 pip or a delay of a few minutes.
- It has been shrinking for a decade.
- What it does show is that these prices hold structure that persists across years and sits just at the edge of the cost. That is what a network trained on nine years could be selective about, where three years could not tell it from luck.

**Consequences.**

1. Periods for the new file: train 2012 to 2020, validation 2021 to 2024 (from October 2022 on it was used by trials 1 to 4, so that part is weaker), final test January 2025 to November 2026, read once.
2. The cost should depend on the moment. A flat cost rewards trading exactly when the real one is highest. Without bid and ask history it can only be modelled (wider after a fast move and at the rollover); Interactive Brokers can supply bid and ask bars to calibrate it for recent years. Not built.
3. Not built yet: the corrected file, the repair of 2023, the move of the loader and the run configs to the new file. In-run validation will need a sample of the four validation years, with the full set replayed at the end.

### 8.13 The move to the long history, and what counts as proof

**Decision (owner, 2026-11-11).** The aim is to show that an agent can trade in simulation, not to build one that will trade. The long file is good enough for that from 2012 as it stands, so its clock and its 2023 holes (8.12) are not corrected.

- The clock is local market time: New York time plus five hours through 2018, London time from 2019. That is steady against the trading sessions all year, which is what an agent reading the hour needs. It matters only when an agent is connected to a live feed in UTC.
- February to July 2023 lies in the validation period. A missing hour reads as one bar with an hour's move in it, like a small weekend.

**What was done.**

- `fx_tables/EURUSD1.txt` is now 2012-01-02 to 2024-12-31 of the long file: 4,780,342 bars. The Interactive Brokers file it replaced is kept as `fx_tables/EURUSD1_IB_2022_2025.txt`. The long file is untouched.
- `exp_runner:get_run_configs/0`: train 2012 to 2020 (3,325 episodes of 1,000 bars fit), validation 2021 to 2024 (1,429 fit). In a run, 200 validation episodes spread evenly over the four years are used every 20 generations; the whole period is for replaying saved populations afterwards.
- The final test, January 2025 to 6 November 2026 (688,349 bars), is not in the loaded file. Nothing in the system can reach it.
- Everything else is the overnight config of 8.11.

**Measured** (scratch `e12`, one VM): the table loads in 14 seconds; the shared price arrays take 95 seconds to build, once per experiment; memory is 1.2 GB after loading and peaked at 1.5 GB. `eval_protocol:g0()` passes on the new table with the new periods. A four-run experiment of 8 agents completed on it: score episodes were drawn from 2012 to 2020, validation ran on the 200 episodes, both time-limited runs ended on time, `vm_mb` stayed between 1,432 and 1,446 (peak 1.51 GB), and the selection memory matched its recomputation over 560 agent-generations. The full-size run has not been run.

**What counts as proof.** Written down before any agent has been trained on this data, so that it cannot move afterwards:

1. After the run, one agent is chosen from the saved populations by its money after cost on the whole validation period.
2. That agent is run once on the test period: every consecutive 1,000-bar episode from January 2025 to November 2026 (about 690), a quarter pip per position.
3. The proof is money after cost that is above zero by at least twice its standard error (t of 2 or more).
4. Reported with it, but not part of the mark: the same at half a pip, and the 60-bar rule of 8.12 on the same episodes.

What the test can resolve: one episode's result varies by 30 to 40 pips, so 690 episodes resolve about ±1.5 pips per episode, and the mark needs about +3. The 60-bar rule's +2.7 would fall just short of it. A result above zero but below the mark is encouragement, not proof.

**The caveat that stays.** The cost is a flat quarter pip. 8.12 showed that the one pattern confirmed so far lives where the real cost is several times that. A pass at a quarter pip proves that an agent can find and time structure in these prices; item 4's half-pip figure says how far that is from money.

### 8.14 The first overnight run hung, and why

**What happened.** The overnight run started on 2026-11-11 at 20:35 stopped advancing at 21:07, in generation 10 of run 3, after 100 generations. 47 of 48 agents had been scored. The process stayed at about 190% CPU with nothing logged. It was not finished and would never have finished: `run_time_limit_s` is only checked between generations.

**The cause.** The 48th agent, `{5.572729388354266e-10,agent}`, had lost its price sensor to `remove_sensor` and was left with one sensor, `fx_Internals` (position and open profit). The simulator started an evaluation only on a price sense; the internals sense did not start it, and a trade sent to a simulator that has not started was answered "nothing happened". So this agent's network ran, the cortex completed cycle after cycle (about 25,000 a second), and the evaluation never began, so it could never end. The watchdog saw nothing wrong: bars were "completing", so it was not a stall, and no mailbox filled up, so it was not a runaway. Replayed alone from a copy of the database, the agent did the same.

`remove_sensor` has been in the operator list since the second trial; the fourth trial did not happen to produce such an agent.

**Fixed in three places.**

1. `fx:sim/3`: in episode mode an evaluation now starts on the internals sense as well (`fx:start_if_idle/1`). No agent can leave the simulator idle.
2. `exoself:watch_progress/3`: an evaluation that reports more than twice its bars plus 1,000 (`bar_limit/1`) is ended as an overrun, logged and counted with the runaways. This catches any other way of cycling without the simulator advancing.
3. `genome_mutator:remove_sensor/1`: a network keeps at least one sensor that carries prices (anything but `fx_Internals` and `fx_Clock`), the rule `morphology.erl` already applies to the first sensor.

**Tested** on the stuck agent, from a copy of the run's database: with fix 1 its evaluation of 200 validation episodes completes (fitness −800: one position per episode, below the trade minimum). With the old simulator and fix 2 it is ended at 401,050 reported bars, within 20 seconds, as `STALL | kind=runaway | overrun`. A four-run experiment of 8 agents then completed with all three fixes in place. The full-size run has not been rerun.

**What the 100 generations showed** before the hang is not analysed here; the run is to be restarted from the beginning.

### 8.15 The overnight run: what the agents learned, and why it stops in 2021

**The run.** Restarted 2026-11-11 at 21:56 with the three fixes of 8.14, finished 2026-11-12 at 07:39. Six runs, 748 generations (30, 60, 341, 189, 84, 44). No stall, no runaway, no overrun. `vm_mb` stayed between 1,423 and 1,468. The four time-limited runs ended 15 to 141 seconds after their limit (one generation). Its files are in the repo directory as left (`run.out`, `logs`, `Mnesia.nonode@nohost`).

**In the log, at a quarter pip** (608 generations, from generation 50 of run 3 to the end; scratch `t6_runs.js`, `t6_val.js`):

- The median eligible agent made +5.2 pips per episode after cost on that generation's new training episodes (standard error 0.34; above zero in 74% of generations). 28 positions per episode, 0.33 pip before cost per position.
- Survivors scored 0.94 in the generation that selected them and 0.82 in the next, on new episodes. Newcomers scored 0.02. The selection memory of 8.11 does what it was built for: what survives is skill on the training years, not the luck of one draw.
- On the 200 validation episodes the median agent lost 2.9 pips per episode (33 checks), and 2 of 1,543 agent-checks were above zero.

**The replay** (scratch `e15`, `t6.erl`, `t6_money.js`; one VM, 28 minutes, peak 2.3 GB). The top four evaluated agents of each saved quarter-pip population (the ends of runs 3, 4, 5 and 6) on all 1,429 validation episodes, and the top two of each on 900 episodes spread evenly over the training years. Money after a quarter pip per position, pips per episode:

| | Training years, 900 episodes | Validation years, 1,429 episodes |
|---|---|---|
| Money after cost | +5.9 to +6.8 (standard error 1.56; t 3.8 to 4.4) | −0.60 to +0.61 (standard error 1.1) |
| Before cost, per position | 0.46 to 0.50 pip | 0.23 to 0.27 pip |
| At half a pip | −1.0 to +0.1 | −6.1 to −7.5 |
| Acting one bar late | +3.0 to +4.0 | −1.2 to −2.8 |

By year, mean of the replayed agents (one agent's yearly figure is uncertain by about ±5 in the training years, 100 episodes each, and ±2.2 in the validation years):

| 2012 | 2013 | 2014 | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| +7.6 | +15.3 | +5.6 | +2.5 | +8.0 | +2.1 | +6.9 | +5.3 | +3.2 | −0.3 | +2.7 | −1.1 | −1.6 |

**What the agents do.** One strategy, in every population: always in the market (never flat), 25 to 28 positions per episode of about 36 bars each, long 51 to 54% of the time, turning against the last move. The per-episode money of the best agents of the four populations correlates 0.90 to 0.97. 2 to 3% of entries fall in hour 22 of the file's clock, the daily rollover, and earn 1.4 to 2.2 pips each before cost, three to six times the average: the caveat of 8.12 applies to them as it did to the 60-bar rule.

**Size and time did not matter after run 3.** The best agent at the end of run 3 has 6 neurons and 140 weights; the best at the end of run 6, 320 generations later, has 48 neurons and 2,302 weights. On the same episodes: +6.42 against +6.77 in the training years, +0.25 against +0.02 in the validation years. A generation took about 20 seconds at the end of run 3 and about 150 at the end of run 6, validation generations aside. The log's higher training figure for run 6 (+9.2 against +4.3 to +5.2 in runs 3 to 5) was the draw of episodes.

**The 200 validation episodes of the run are an unlucky 200.** The same 16 agents make −2.5 on those 200 and −0.05 on all 1,429 (scratch `t6_subset.js`). One set of 200 resolves ±3 pips, and because it is the same set at every check the same error repeats. Read `val_net_pips_ep` as "minus zero to minus six", not as a loss.

**Why it stops: the pattern fades in the prices themselves.** Three measurements that do not involve an agent (scratch `noise_by_year.js`, `rules_unseen.js`):

| | 2012–2020 | 2021 | 2022 | 2023 | 2024 |
|---|---|---|---|---|---|
| Variance of 15-bar moves against 15 one-bar moves (below 1 means moves reverse) | 0.90 to 0.95, except 2014 (1.00) | 0.97 | 0.98 | 0.97 | 1.02 |
| 60-bar selective rule (k=1) after a quarter pip, pips per 1,000 bars | +2.7 (above zero in 9 years of 9) | +0.1 | +1.3 | +2.1 | −0.8 |
| The same rule, before cost per position | 0.57 pip | 0.34 pip over the four years | | | |

The reversal over minutes to an hour was there in every training year. It has been weaker in each validation year, and in 2024 it is absent. The agents earn 0.48 pip per position before cost where it exists and 0.25 where it has faded, which is exactly the cost. It is also consistent with the first four trials (8.5 to 8.10), which trained on October 2022 to June 2024 and found nothing that held.

**Reading.**

1. The machinery now does its job: selection finds a real pattern and keeps it, at the target cost, in every one of nine years.
2. No agent passes on 2021 to 2024 (best t 0.55), so there is no candidate for the test of 8.13. The test period stays unopened.
3. More generations, a larger population or larger networks would not change this: the agents stopped improving within the first hundred quarter-pip generations, and what holds them back afterwards is in the prices, not in the search.
4. All agents found the always-in form of the strategy. The fixed rules say the selective form is the one with a margin: the 60-bar rule that waits for a large move earns 0.57 pip per position in the training years and 0.34 in the validation years, against the agents' 0.48 and 0.25. An agent is flat only while its output is between −0.33 and +0.33 (`functions:trinary/1`), and none of the 16 was flat for a single bar of the 29 million replayed.

**Proposed next, not built** (owner's decision):

1. *A forward test inside the long history, with no code change.* Train 2012 to 2016, validate on 2017, then run one chosen agent once on 2018 to 2020 (about 1,100 episodes; the mark of 8.13, twice the standard error, is about +2.8 pips per episode). This asks the question the project set out to answer (can an evolved agent trade prices it has never seen) in years where there was something to trade. Its limit must be stated with any result: the years were chosen knowing the pattern was alive in them, so a pass proves the method, not that it would make money today. This run's agents, which trained on those years, made +6.9, +5.3 and +3.2 in them.
2. *Let an agent stay out of the market, and train it at a higher cost than it is tested at.* A second output that decides whether to hold a position at all, and training at half a pip. Aim: the selective form of the strategy, which is the one with a chance in 2021 to 2024.
3. The test on 2025 to 2026 waits for an agent that is above zero on 2021 to 2024.

### 8.16 The forward test inside the long history

**Decision (owner, 2026-11-12).** Go ahead with the first step proposed in 8.15.

**The question.** Can an agent evolved on one stretch of prices make money, after cost, on later years it has never seen? 2021 to 2024 cannot answer it: the one pattern found so far has faded there (8.15), so a failure in those years says nothing about the method. 2018 to 2020 can: the pattern was still in the prices.

**What a pass will and will not show.** A pass shows that the method works: evolution finds something real in 2012 to 2016 and it carries forward three years. It does not show that the agent would make money today, and the test years were not picked blind: the agents of the overnight run, which trained on them, made +6.9, +5.3 and +3.2 pips per episode there, and the 60-bar rule made +2.5, +0.7 and +0.9. The new agents will never have seen them. The test of 8.13 on 2025 and 2026 is unchanged and unopened.

**Config** (`exp_runner:get_run_configs/0`). Train 2012-01-02 to 2016-12-31. Validation is every episode of 2017, every 40 generations. Four runs: free (30 generations), ramp (60), and two quarter-pip runs of one hour each (`Hold = 3600`); about 2 h 25 min in all. The trade minimum is 2 in every run. Everything else is as in 8.11. No evaluation in a run reads a bar after 2017: score episodes are drawn from `train_span`, and validation and the periodic champion test are tiled over `val_span`.

**Protocol, fixed before the run.**

1. *Candidates.* The four best evaluated agents of each of the two saved quarter-pip populations (the ends of runs 3 and 4): by stored fitness, one agent per value (offspring carry their parent's number; the parent is taken).
2. *Choice.* The candidate with the most money after a quarter pip on all validation episodes of 2017. If none is above zero there is no candidate, and the test is not run. *(Amended in 8.17 before any look at the test: a candidate must first make money on the training years.)*
3. *Test.* That one agent, once, on every consecutive 1,000-bar episode from 2018-01-01 to 2020-12-31 (about 1,100), a quarter pip per position.
4. *Mark.* Money after cost above zero by at least twice its standard error. One episode varies by about 47 pips, so the mark is about +2.8 pips per episode.
5. *Reported with it, not part of the mark.* The same at half a pip; acting one bar late; without the positions opened in hours 21 to 23 of the file's clock, around the daily rollover (for the overnight run's agents that took +6.4 down to about +3.9 in the training years); the 60-bar rule on the same years; the same agent on 2021 to 2024.
6. *One look.* If the experiment is repeated and tested on 2018 to 2020 again, every look is reported, and a pass is not claimed from the best of several.

The tools are in scratch `tools/t7.erl` (replay) and `analysis/t7_money.js`.

**Tested small** (scratch `e16`, 8 agents, one VM, peak 1.55 GB): the four-run experiment completed. All 360 score episodes of its 90 generations started in 2012 to 2016 (69 to 75 per year), the validation set was the 369 episodes of 2017 at each of its 5 checks, both time-limited runs ended at the first generation boundary after their limit, `vm_mb` stayed between 1,431 and 1,445, and there was no stall. The full-size run has not been run.

### 8.17 The forward run learned nothing: every agent read four-hour bars

**The run.** Started 2026-11-12 at 11:23, finished at 13:43: four runs, 306 generations (30, 60, 161, 55). No stall, no runaway, `vm_mb` 1,422 to 1,441. All 9,920 score episodes started in 2012 to 2016 (1,958 to 2,002 per year), each of the 10 validation checks used the 369 episodes of 2017, and both timed runs ended at the first generation boundary after their hour.

**It did not learn.** In the log, at a quarter pip (from generation 50 of run 3 to the end):

| | this run (166 generations) | overnight run of 8.15 (608) |
|---|---|---|
| median agent, money after cost per episode on new training episodes | −0.8 pips (se 0.6) | +5.2 (se 0.34) |
| survivors: score when selected, then on the next generation's new episodes | 0.08, then −0.15 | 0.94, then 0.82 |
| gross per position | 0.07 pip | 0.33 pip |
| positions per episode | 9.5 | 27.9 |
| agents holding one side for more than 90% of most episodes, median generation | 18 of 48 | 0 of 48 |

On 2017 the median agent read between −4.5 and +1.8 pips per episode at the seven quarter-pip checks, and 36 of 323 agent-checks were above zero.

**No candidate.** The four best evaluated agents of each saved quarter-pip population, replayed on all 369 episodes of 2017 and on 500 episodes spread over the training years (scratch `e17`, one VM, three and a half minutes, peak 1.6 GB):

| population | rank | neurons | positions per episode | bars per position | long, % of bars | 2017, pips per episode (se 2.2) | 2012 to 2016, pips per episode (se 2.5) |
|---|---|---|---|---|---|---|---|
| end of run 3 | 1 | 23 | 4.8 | 203 | 20 | −2.35 | +0.37 |
| end of run 3 | 2 | 23 | 4.8 | 203 | 20 | −2.35 | +0.74 |
| end of run 3 | 3 | 22 | 29.8 | 32 | 34 | −12.11 | −6.60 |
| end of run 3 | 4 | 21 | 4.8 | 202 | 21 | −2.75 | +0.22 |
| end of run 4 | 1 | 29 | 9.4 | 102 | 66 | −2.06 | −5.88 |
| end of run 4 | 2 | 29 | 6.2 | 154 | 67 | −0.15 | −2.69 |
| end of run 4 | 3 | 29 | 6.4 | 149 | 67 | +0.003 | −3.19 |
| end of run 4 | 4 | 34 | 11.0 | 88 | 36 | −5.80 | −2.25 |

All eight are in the market 98 to 100% of the time and hold a position for hours: mostly short at the end of run 3, mostly long at the end of run 4. None makes money on new episodes of the years it was trained on (best +0.74, a third of one standard error). Their stored fitness of 0.58 to 0.82 was luck that selection remembered.

**The test was not run.** The best candidate on 2017 made +1.05 pips over the whole year, +0.003 per episode: zero. By the letter of rule 2 of 8.16 that is "above zero". It was not taken: the test asks whether what an agent learned carries forward, and on new episodes of its own training years this agent is at −3.2 pips per episode (se 2.5). It has learned nothing that could carry. A fail would say nothing about the method and a pass (one chance in forty) would be luck. No agent of this experiment has been run on 2018 to 2020; the first look is still unused.

**Rule 2 of 8.16, amended** (2026-11-12, before any look at 2018 to 2020): a candidate is eligible only if its money after a quarter pip on 900 episodes spread over the training years is above zero by at least twice its standard error. Among the eligible, the one with the most money on 2017 is chosen, and it must be above zero there. If there is none, the test is not run. The overnight run's agents would have been eligible (t 3.8 to 4.4 on such a replay, 8.15).

**Why it did not learn.** Which sensors feed at least one neuron, counted over the agents of each saved population:

| experiment | end of run 1 (30 generations) | end of the experiment |
|---|---|---|
| this run | four-hour list: 48 of 48 | four-hour list 24 of 24; 90 one-minute closes 13; indicators 2 |
| overnight run (8.15) | hourly list: 48 of 48 | hourly list and indicators in 48 of 48 at the end of run 3; seven sensors in nearly all by run 6 |
| first overnight attempt (8.14) | 90 one-minute closes: 48 of 48 | the same when it hung |
| fourth trial | hourly list: 48 of 48 | hourly list 24 of 24 |
| third trial | 270 one-minute closes: 24 of 24 | six sensors in nearly all |

- `morphology:get_InitSensors/1` gave a new agent **one** sensor, drawn at random from the eight that carry prices.
- A population of 48 with rank selection and half surviving is one family within the 30 free-trade generations. From then on the whole experiment reads the sensor that family's founder drew.
- Another sensor arrives only through `add_sensor`: weight 10 of the 449 that apply to a neural network, then one of the nine sensors not yet used, connected to one neuron with random weights. For a given sensor that is about one mutation in 400.
- This run's family drew the four-hour list (`fx_PLT [240,30]`): 29 completed four-hour closes and the current close. The price change over the last 15 or 60 bars, which is what the reversal of 8.12 and 8.15 is read from, is not in it. The networks grew to 21 to 34 neurons on that one input and found nothing, because there was nothing in it to find at this horizon.
- The overnight run's family drew the hourly list, and during run 3 the indicators (`fx_IND`: change over 5, 15, 60, 240 bars, distance from the 60-bar average) reached all 48 agents; they are in none at the end of run 2. Its 6-neuron agent reads them with 5 of its 6 neurons. In that log, between generations 20 and 40 of run 3, positions per episode go from 4 to 45 and survivors' score on new episodes rises to +0.4. The saved populations do not date the arrival of the sensor more closely than "during run 3".

So whether a run learned the reversal was decided by two draws: the founder's sensor, and one rare mutation. One of the two runs on the long history had both. More generations do not repair this: an added sensor matters less the larger the network already is.

**Change.** `{init_sensors, series}` (config; in the run configs): a new agent starts with every sensor answered from the shared price arrays, the four price lists and the indicators (118 inputs in all). Its first neuron reads one of them or all of them (`genotype:construct_InitialNeurons/6`, as before), and `add_sensorlink` (weight 30) connects the others. The raw sensors, the clock and the account state still arrive through `add_sensor`. `{init_sensors, one}` is the old behaviour and stays the default outside the run configs.

**Tested small** (scratch `e18`, 8 agents, one VM, peak 1.55 GB): the four-run experiment completed with no stall. Every new agent carried the five sensors (118 weights on its one neuron when it reads all of them), and at the end of each run all five fed a neuron in 7 or 8 of the 8 agents. All 60 score episodes started in 2012 to 2016, each validation check used the 369 episodes of 2017, and `vm_mb` stayed between 1,432 and 1,445. In that small test a one-neuron agent took about three times as long per bar with five sensors as with one. At full size it is about twice: one generation of 48 new agents with validation took 192 seconds (scratch `e19`), against 96 with one sensor. The first two runs of the full experiment will therefore take about 35 minutes, not 16, and the whole experiment about 2 h 40 min. No full-size experiment has been run with the change, so whether it makes a run learn is not yet known.

**What this does not change.** A run is still one family, so a run can still fail to find the pattern. It shows early in the log: in the overnight run the 20-generation mean of `survivor_rescore` was above 0.3 from generation 44 of run 3 (27 minutes after the experiment started) to the end; in this run it never passed 0.2. A run that is not above 0.3 an hour after its start can be stopped and started again. Restarts cost no look at 2018 to 2020: the choice of agent uses the training years and 2017 only.

**Next.** The same forward experiment (8.16), with the sensor change and the amended rule 2.


### 8.18 Before more agents: the position lists removed, and families as a setting

**Decision (owner, 2026-11-12).** Remove the position lists. Families are not agreed as the approach; they are built as a setting so that they can be tested.

**The position lists.** During a validation pass the simulator kept each agent's position at every bar of every episode: 369 lists of 1,000 numbers per agent. They were built in the simulator, copied to the statistics table, copied again to the agent's process, and dropped. Nothing in a run read them: the generation summary uses per-episode totals. The only readers are `eval_protocol:shift_test/3`, which is run by hand after a run, and the replay tools.

Measured on one generation with validation, 48 new agents (scratch `e19`, one VM at a time):

| | lists kept | lists not kept |
|---|---|---|
| process memory at the peak | 656 MB | 80 MB |
| total memory at the peak | 2,076 MB | 1,499 MB |
| per agent | about 13 MB | about 1.3 MB |

`vm_mb` in `GEN_SUMMARY` read 1,448 and 1,438: it is taken after the generation, so it never showed the peak. At 8 agents the lists cost about the same per agent (15 MB), so the cost grows in step with the population: about 6 GB at 480 agents, 12 GB at 1,000.

*Change.* `config:validation_series/0`, default `false`: the lists are not kept. `eval_protocol:evaluate_agent/2` switches them on for its own evaluation and puts the setting back, and the replay tool sets `{validation_series, true}`. `shift_test/3` returns "nothing to test" if it is given statistics without lists.

**Families.** `{families, N}` (config, default 1; `Families` in `exp_runner:get_run_configs/0`) makes the population N species of 48 agents that never mix.

- Already in the code: `population_monitor:mutate_population/5` selects and breeds inside each species, mutants stay in their parent's species (`genotype:speciate/1` is never called), and `clone_population_for_next_run/2` carries every species into the next run in the same order. There had only ever been one species.
- Added: `exp_runner:get_init_constraints/0` repeats the constraint N times. The generation ledger row carries the agent's species. `selection_memory:update/0` takes its reference from the agent's own family: against a median of the whole population every agent of a strong family sits above the reference and every agent of a weak one below it, and the prior then ranks proven agents over newcomers in the first and newcomers over proven agents in the second, whatever their merit. One `FAMILY_SUMMARY` line per family and generation is written next to `GEN_SUMMARY`, with the same quantities over that family's agents.
- All families run the same episodes each generation, so their lines can be compared directly.
- With 1 the experiment is exactly as it was.

**What a test of families can show.** There is no evidence yet that families differ once every agent starts with the same five sensors: the two long-history runs differed in the sensor their founders drew (8.17), and that draw is gone. A run with three families is three attempts on the same episodes. If their `survivor_rescore` columns end alike, families add cost and nothing else. If one learns and another does not, families are what turns more agents into more chances.

*Cost.* A generation takes N times as long. With `Families = 3` and `Hold = 10800` each family gets the same number of generations as in the one-family experiment: about 1 h 50 min for the first two runs and 6 hours for the two quarter-pip runs, about 8 hours in all (from the timings of the full-size test below). Memory with 144 one-neuron agents peaked 0.3 GB above the 1.4 GB base.

**The protocol of 8.16 with several families** (fixed 2026-11-12, before any look at 2018 to 2020). Stored fitness is relative to the family's own median, so it does not rank agents of different families. Candidates are therefore the four best evaluated agents of each family in each saved quarter-pip population. That is 24 with three families, and 24 agents without skill clear a gate of two standard errors on the training years about four times in ten. With more than one family the gate of 8.17 is three standard errors (about 3 in 100 for 24). The overnight run's agents were at 3.8 to 4.4. The rest is unchanged: among the eligible, the most money on 2017, above zero, one look. With one family nothing changes.

**Tested small** (scratch `e20`, one VM at a time).

- Two families of 8, four runs: completed, no stall, no error. Every saved population holds two species of 8 agents and every agent names its own species. Each of the 11 generations has its two `FAMILY_SUMMARY` lines, with 8 agents and at most 4 survivors per family. Process memory peaked at 38 MB with 16 agents validating; with the lists it was 131 MB for 8 (8.17's test).
- One family of 8, one run: completed, three generations, no `FAMILY_SUMMARY` line.
- The replay tool lists the best agents per family (`run3.f1`, `run3.f2`, ...).

- Three families of 48 at full size, two generations of run 1 from the real run configs (scratch `e21`): completed, no stall, no error. The generation with validation took 609 seconds and the ordinary one 55; total memory peaked at 1,702 MB; each family had its 48 agents and 23 or 24 survivors.

Not tested: whether families end in different places. That is what the run is for.

**Set up** (owner, 2026-11-12, "lets set it up and I can run it"): the run configs are `Families = 3`, `Hold = 10800`. It is the forward experiment of 8.16 with the sensor change of 8.17, three times over on the same episodes.


### 8.19 The three-family run: the families ended in three places, and no agent qualified

**The run.** Started 2026-11-12 at 19:49, finished 2026-11-13 at 05:17: 9 h 28 min, four runs, 187 generations (30, 60, 56, 41), 144 agents. No stall, no runaway, `vm_mb` 1,433 to 1,477. All 6,112 score episodes started in 2012 to 2016, and each of the 7 validation checks used the 369 episodes of 2017. Every saved population holds three species of 48.

Each family had 97 generations at a quarter pip. The overnight run of 8.15 had 608 and the one-family forward run of 8.17 had 216. Sampled an hour in, the run was using 2 of the machine's 10 cores (3.8 on average over its first hour), with the machine three quarters idle. The cause is not known yet.

**The families ended in three places.** From the `FAMILY_SUMMARY` lines of the two quarter-pip runs:

| | family 1 | family 2 | family 3 |
|---|---|---|---|
| survivors' score on the next generation's new episodes, run 3 | −0.29 | −0.12 | +0.56 |
| the same, run 4 | +0.34 | +0.10 | +0.97 |
| median agent, money after cost per episode, run 3 | −3.3 pips | −2.2 | +1.9 |
| the same, run 4 | +0.7 | −1.3 | +4.1 (se 1.2) |
| positions per episode | 77, falling to 14 during run 4 | 3 | 52, then 33 |
| agents holding one side | 0 to 9 of 48 | 47 or 48 of 48 | 0 or 1 |

- Family 3 learned from generation 10 of run 3 and kept improving.
- Family 1 traded 77 times an episode for 0.19 pip a position, which loses at a quarter pip, until generation 20 of run 4. In its last 20 generations it moved to 14 to 24 positions for 0.31 to 0.48 pip, and its survivors scored +0.55 to +0.72 on new episodes.
- Family 2 held one side from the end of the ramp to the end of the night.

The sensors do not explain it. At every save the indicators feed 47 or 48 agents of every family, and from the end of the ramp on at least two price lists do as well. The draw of 8.17 is gone, and attempts still differ.

**The 24 candidates** (four best of each family in each saved quarter-pip population; scratch `e22`, one VM, 15 minutes, peak 1.9 GB). Agents with the same numbers are clones and share a row. Money is pips per episode after a quarter pip.

| family, population | positions per episode × bars held | in the market | 900 training-year episodes (se), t | at half a pip | one bar late | without hours 21 to 23 | 2017 (se) |
|---|---|---|---|---|---|---|---|
| 1, end of run 3 (4 clones) | 81 × 12 | 99% | −2.9 (1.8), −1.6 | −23.2 | −8.2 | −4.3 | −6.8 (2.2) |
| 1, end of run 4 (4 near-clones) | 14 × 71 | 100% | +2.5 to +2.7 (1.75), 1.4 to 1.5 | −0.9 to −0.7 | +0.6 | +2.1 to +2.2 | +4.3 to +4.4 (2.2) |
| 2, both (7 holders, always long) | 3 × 270 to 390 | 100% | −1.2 to −1.5 (1.9), −0.7 | −1.9 to −2.2 | −1.3 to −1.5 | −0.4 to +0.4 | +2.3 to +3.0 (2.1) |
| 2, end of run 4, rank 1 | 11 × 3 | 3% | −2.9 (0.6), −4.6 | −5.6 | −2.4 | −2.3 | −1.0 (0.2) |
| 3, end of run 3 (4 near-clones) | 50 × 14 | 71% | +2.6 (1.6), 1.6 to 1.7 | −10.0 | −0.4 | +0.3 to +0.5 | −3.1 to −3.5 (2.0) |
| 3, end of run 4, ranks 1 and 2 | 39 × 20 | 81% | +2.9 (1.6), 1.8 | −6.7 | 0.0 | +0.4 | −2.2, −2.3 (2.1) |
| 3, end of run 4, rank 3 | 38 × 21 | 82% | +4.4 (1.6), 2.7 | −5.0 | +1.2 | +2.5 | −2.7 (2.1) |
| 3, end of run 4, rank 4 | 36 × 23 | 83% | +3.9 (1.6), 2.4 | −5.0 | +1.0 | +1.8 | −1.5 (2.1) |

**No candidate. The test was not run.** None of the 24 is three standard errors above zero on the training years (the best is 2.7). The outcome does not hang on the height of the gate: the two agents that clear the two standard errors of the one-family rule are both below zero on 2017. No agent of any forward experiment has been run on 2018 to 2020.

**What each family learned.**

1. *Family 3: the fast reversal again, weaker, and it does not reach 2017.* Positions of 14 to 23 bars, out of the market 17 to 29% of the time. Acting one bar late leaves −0.5 to +1.2 of its +2.6 to +4.4, and without the positions opened around the daily rollover +0.3 to +2.5 is left. By training year it is 2013 that pays (+6 to +11); 2012 is below zero. On 2017 all eight agents are below zero (−1.5 to −3.5), and one bar late there −3.6 to −7.0. This is the pattern of 8.12 and 8.15: it lives in the bar and the hours where a flat quarter pip understates the cost.
2. *Family 1, at the very end: something slower.* 14 positions an episode held 71 bars, always in the market, long half the time. Above zero in each training year (+3.6, +1.3, +4.7, +0.2, +3.1) and in 2017 (+4.4). Without the rollover hours +2.1 of its +2.6 remains, and one bar late +0.6 (on 2017 +2.3). At half a pip it is at about zero. It is not established: 1.5 standard errors on the training years and 2.0 on 2017, it is one of eight behaviours in the table, and it is 20 generations old.
3. *Family 2: nothing.* Always long. It lost in 2012 to 2016, when the euro fell, and made +2.3 to +3.0 in 2017, when it rose 3.5 pips per 1,000 bars. That is the year, not the agent.

**Reading.**

- *Families.* Three attempts with the same sensors, episodes and settings gave one that learned at once, one that began to in the last hour, and one that never did. On this evidence a one-family night is family 2 one time in three. Three is a small number, but the question of 8.18 was whether attempts still differ without the sensor draw, and they do.
- *Depth.* 97 generations at a quarter pip is a sixth of what the overnight run had. Family 3 was still rising and family 1 had just started when the night ended. The cores the run did not use are the cheapest depth there is.
- *The rule of choice and 2017.* The year rose steadily, so an agent that is simply long makes +2.3 to +3.0 on it with no skill. The gate on the training years kept those out. The rule is not changed here; the next protocol should read 2017 with the year's drift taken out, as the score does.
- *What a pass needs.* On 900 training-year episodes one standard error is 1.6 to 1.8 pips, so three of them is about +5. The overnight run's agents were at +5.9 to +6.8. This run's best were at +2.6 to +4.4.

**Next.** Find why the run uses 2 of 10 cores. Then the same three families can be given the depth one family had. (Found: 8.20. It is worth less than this section assumed.)

### 8.20 Why the run used two cores, and what the cores are worth

**Short version.** The Erlang VM puts its scheduler threads to sleep when there is little to do. Every 500 evaluations the population monitor stopped the population and ran each family's best agent alone on 2017, one after the other. During such a stretch the VM went down to two threads, and when the next generation's 144 agents started it stayed on two. Starting the VM with `+scl false` prevents it. Fixing this, removing the stop, and one cheaper lookup make the early generations about twice as fast. They do not give the factor of five that "2 of 10 cores" suggested: the same work on all ten cores is 1.5 to 1.8 times as fast as on two, not five times. The largest remaining cost is the validation pass, 27% of the night.

**Where the night went** (three-family run of 8.19, from `benchmarker.log`):

| run | generations | minutes | scoring | validation passes | benchmark steps |
|---|---|---|---|---|---|
| 1 | 30 | 47 | 33 (71%) | 8 (16%) | 4 (8%) |
| 2 | 60 | 138 | 88 (64%) | 35 (25%) | 12 (9%) |
| 3 | 56 | 183 | 119 (65%) | 46 (25%) | 14 (8%) |
| 4 | 41 | 201 | 118 (59%) | 65 (32%) | 14 (7%) |
| all | 187 | 568 | 359 (63%) | 154 (27%) | 44 (8%) |

- *Scoring* is what produces generations: 144 agents on 32 episodes each.
- *A validation pass* is every agent on the 369 episodes of 2017: the work of 11.5 generations. There were seven: at generation 0 of each run and at generation 40 of runs 2, 3 and 4. They took 8 to 38 minutes each.
- *A benchmark step* (`population_monitor:run_GenTest/2`) runs each family's best agent on 2017, one agent at a time, inside the population monitor, while every other agent waits. It happens every 500 evaluations (52 times). Its result is stored in the species statistics. Nothing in the training core reads it, and the generation summary reports validation for every agent already.

**The two-core state.**

1. *What was sampled overnight.* Eight readings between 20:52 and 20:56 on 2026-11-12, all 192% to 200% of one core, with the machine 73% idle. By the log, the first three fell in the validation pass of generation 0 of run 2, and the last three, taken over 24 seconds, in the scoring of generation 1 with all 144 agents running. So this was not the benchmark step itself.
2. *The same thing in a replay.* A fresh three-family population on the run's own path, nothing watching inside the VM, processor use sampled from outside every 2 seconds (scratch `e23`):

   | generation | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
   |---|---|---|---|---|---|---|---|---|---|
   | seconds | 41.8 | 44.4 | 46.1 | 72.0 | 52.2 | 53.8 | 76.0 | 84.0 | 81.8 |
   | ms per bar | 1.26 | 1.35 | 1.40 | 1.47 | 1.59 | 1.64 | 1.65 | 2.56 | 2.40 |

   Generations 3 and 6 contain a benchmark step (24 and 21 seconds at about 200%). After the first one the VM went back to 860 to 870%. After the second it stayed at 199 to 200% through all of generations 7 and 8. Overnight the same jump is at generation 4 of run 1 (1.49 to 2.26 ms per bar), right after that run's first benchmark step.
3. *On demand.* The 144 agents of the population saved at the end of run 1, all at once on the same 12 episodes, then three agents one after the other on 369 episodes (19 seconds), then the 144 again, and so on:

   | VM started with | first pass | every later pass |
   |---|---|---|
   | nothing (the default) | 88,900 bars per second on 7.8 cores | 44,500 to 49,100 on 1.95 to 1.99 cores, six passes in a row |
   | `+scl false` | 86,200 on 7.7 cores | 83,500, 70,400, 70,300 on 7.8 cores |
   | `+sub true` | 87,000 on 7.8 cores | 70,400 on 3.5 cores, then 63,600 and 58,900 on 2.9 |

   `+scl` is "scheduler compaction of load": by default the VM moves the work onto fewer scheduler threads when threads often run out of work. One agent is a chain of about a dozen processes that hand a bar to each other, so its threads run out of work all the time.
4. *When it does not happen.* Not after a single-agent stretch of the same length made of 80 to 90 short evaluations (five tries), so the few seconds at the end of each generation, when the last agents finish, are not enough. Not with the population saved at the end of run 4: its single agent keeps 2.8 cores busy, and the VM came back to 8.4 cores both times. Overnight, time per bar jumps between two levels in runs 1 and 2 (low in generations 0 to 3, 13 and 22 to 27 of run 1 and 20 to 32 of run 2, high otherwise) and moves smoothly with network size in runs 3 and 4. There is no processor sample from runs 3 and 4.

**What ten cores are worth.** Same 144 agents, same episodes, `+scl false`, scheduler threads limited by hand (in the order measured):

| threads | 10 | 2 | 4 | 6 | 8 | 3 | 5 | 4 again | 10 again |
|---|---|---|---|---|---|---|---|---|---|
| bars per second | 89,300 | 37,200 | 64,100 | 65,600 | 66,800 | 43,400 | 51,100 | 55,200 | 72,400 |

- The machine has four fast cores and six slow ones. Four threads give 1.7 times two. Ten give 1.3 to 1.4 times four.
- The machine slows as it heats: the same pass fell from 89,300 to 72,400 within a few minutes of load. Long runs should be compared with the lower figure.
- So the two-core state cost about a third of the throughput of small networks (45,000 to 49,000 against 70,000 once warm), not four fifths.

**One cheap cost found on the way.** `config:get_val/2` asked `ets:info/1` whether the settings table exists before every lookup. `ets:info/1` builds a fifteen-item list, and every agent reads about thirty settings per bar. With one lookup and no `ets:info`, and the settings and price tables opened for concurrent reading:

| threads | before | after |
|---|---|---|
| 10 | 91,700 and 88,300 | 108,300 and 104,500 (+18%) |
| 4 | 66,700 and 65,000 | 79,400 and 76,700 (+19%) |

That is for the small networks of run 1. On the nine-neuron networks of run 4 I can measure no gain (32,000 bars per second on all cores before and 30,700 after, in separate runs): there the time goes to the network itself. A tracing profile of one such agent, which overstates small functions, shows the weighted sums at 11%, the plasticity rules at 10%, and, for an agent that has one, the raw one-minute price list sensor at 27%. No other single cost stands out.

**Changes.**

1. Start the VM with `+scl false` (README). `exp_runner:start_internal/2` prints a `WARNING` line at the start of an experiment when the flag is missing.
2. `{generation_test, false}` in the run configs (new setting, default `true`): no benchmark step.
3. `config:get_val/2`: one lookup. A missing table still returns the default.
4. The settings table and the price tables are created with `read_concurrency`.

Files: `config.erl`, `fx.erl`, `population_monitor.erl`, `exp_runner.erl`.

**Checked** (scratch `e25`, built from the changed source, one VM at a time, memory capped from outside at 3.5 GB, peak 1.7 to 1.9 GB):

- After a normal boot the price table and the settings table report `read_concurrency` true, an unset setting returns its default with and without the table, and `generation_test` is `true` unless set.
- The run's own path, fresh three-family population, nine generations, `+scl false`: 370 seconds (33 to 46 per generation), no benchmark step, no warning line, 808% of a core on average including the boot. The replay before the changes took 552 seconds for the same nine generations, and generations 1 to 9 overnight took 715.

**What this buys for a repeat of the three-family night, and what it does not.**

- Runs 1 and 2 took 3 h 5 min. I expect about 2 hours: most of their generations ran in the two-core state.
- Runs 3 and 4 are timed at 3 hours each. The 14 minutes each spent in benchmark steps become scoring: about 12% more generations (56 to about 62, 41 to about 46). If the VM was partly in the two-core state there, more; I cannot tell.
- That is 97 quarter-pip generations per family becoming roughly 108. It is not the depth of the overnight run (608). Three families cost three times one family, and a nine-neuron agent costs 270 to 300 microseconds of processor per bar on all cores. The sentence in 8.19, "the cores the run did not use are the cheapest depth there is", overstated it.

**The larger item: the validation pass.** 27% of the night and 32% of run 4. Selection never uses it, and the choice of candidates uses a replay after the run (8.17, 8.18), so during a run it is a monitor. Two ways to make it cheaper:

- *Once per run:* keep the pass at generation 0 of each run and drop the one at generation 40. Saves 17, 26 and 38 minutes in runs 2, 3 and 4. With the benchmark step gone as well, runs 3 and 4 would get about 75 and 59 generations: about 134 quarter-pip generations per family against 97. Cost: the summary has one validation reading per run where it had two.
- *Fewer agents:* validate the 16 best of each family. Saves two thirds of every pass. Cost: the validation columns then describe the top third, not the population.

**Decided 2026-11-13: once per run.** `{validate_population_every, 1000000}` in the run configs: the pass runs at generation 0 of each run and not again. The summary's validation columns read `na` in every other generation. The populations saved at the end of each run can still be replayed on 2017 afterwards.

**Not done.**

- The raw price list sensors still read the price table row by row. Serving them from the shared arrays would remove most of that 27% for the agents that carry one.
- Evaluating a network inside one process, without a message per connection, is the change that would move those 270 to 300 microseconds. It is a redesign of the phenotype, not a fix.


### 8.21 What the score should pay for: four ways to fill an order, measured

**The question.** Before a large run: what should training reward? The score itself (`alpha_t`: money after cost per unit of risk taken, as a t-statistic over the episodes) found the pattern in every run that had the inputs, and it already charges for exposure that knows nothing. What is known to be wrong is the fill: every order is filled at the bar's close for a flat quarter pip, at any hour.

**Four fill rules** (scratch `analysis/r1_fills.js`; it reads the price file to the end of 2017 only):

- *now:* the simulator as it is.
- *late:* the position is held from the next bar's close, one minute later.
- *no rollover:* no position is opened, closed or reversed in hours 21 to 23 of the file's clock. The position held at 21:00 is kept. (Stricter than the "without the positions opened in hours 21 to 23" of 8.16, which only removed those positions.)
- *both.*

Money after a quarter pip, pips per 1,000 bars, with t in brackets:

| | now | late | no rollover | both |
|---|---|---|---|---|
| Overnight run's agents (8.15), 900 episodes of 2012 to 2020, positions as they were | +6.3 (4.1) | +3.5 (2.3) | +3.6 (2.4) | +1.8 (1.2) |
| The same agents on 2021 to 2024 | +0.1 | −1.7 | −1.2 | −2.6 |
| What the score sees of them in one evaluation of 32 episodes, in its own standard deviations | 1.05 | 0.63 | 0.68 | 0.41 |
| Fixed rule, 15 bars, enter after one typical move, 2012 to 2016 | +4.2 (4.3) | +2.1 (2.3) | +3.4 (3.4) | +1.5 (1.6) |
| Fixed rule, 60 bars, the same, 2012 to 2016 | +3.0 (3.1) | +1.6 (1.7) | +2.5 (2.6) | +1.3 (1.4) |
| The 60-bar rule on 2017 | +2.2 | +2.0 | +1.7 | +1.5 |
| Family 3 of 8.19, end of run 4, training years | +3.5 | +0.5 | +1.4 | −1.4 |
| Family 1 of 8.19, end of run 4, training years | +2.6 | +0.6 | +1.6 | −0.1 |
| Family 3 on 2017 | −2.2 | −4.6 | −4.9 | −5.7 |
| Family 1 on 2017 | +4.4 | +2.3 | +2.6 | +1.0 |

At half a pip with both restrictions every fixed rule tried is below zero (−0.3 to −3.9).

**Reading.**

- Each restriction alone takes about half of what the agents make. Both together leave under a third, and nothing that reaches two standard errors.
- The fixed rules lose a fifth to the rollover rule and half to the delay. The agents lose 43% to the rollover rule: they had learned to trade the rollover, which a rule that waits for a large move does not need.
- The pass mark of the forward test is about +2.8 pips per episode. Under *both* nothing measured here reaches it. Under either one alone the agents as they are sit at +3.5.
- No rule changes the order of the existing agents on unseen years. Family 1's slow strategy stays above zero on 2017 under all four and family 3 stays below.
- The agents, always in the market, make more than the best fixed rule (+6.3 on 2012 to 2020 against +4.2 on 2012 to 2016), and fixed rules that are always in lose (−2.4 to −13.5). So the agents' always-in form is not what holds them back, and a separate "stay out" output (8.15, item 2) has no support here.

**Assessment.**

1. *Keep the score and the quarter pip.* Nothing measured argues against them.
2. *Close the rollover.* It is the one place where the flat cost is known to be wrong by a multiple, and the agents use it. It costs an agent a fifth to two fifths.
3. *Do not train one bar late.* A real order is not a minute late. The delay stands in for a wider spread after a fast move, which cannot be calibrated without bid and ask prices. Keep it as a reported figure.
4. *Do not train at half a pip.* Nothing is left there to select on.
5. *Judge the forward test under the same rule as training*, with the figure under the present rule reported next to it.

**Test proposed, not built.** The three-family night of 8.19 again with one change, the rollover closed. Same episodes per evaluation, same families. It answers whether selection still finds and keeps something when the rollover is gone: last night's best family had its median agent at +4.1 pips per episode and survivors' score on new episodes at +0.97. The candidates are replayed on the training years and 2017 as before. 2018 to 2020 stays unread unless a candidate qualifies.


### 8.22 Wider options: what evolution chose in our own populations, and what the literature says

**The question.** Before a large run: change the config, use the substrate encoding, encode the sensors differently, add sensors, change the activation functions? Answered from two places: a count of what the saved populations actually contain (scratch `tools/census.erl`, `census2.erl`, `census3.erl`), and a search of the literature (sources at the end).

**1. The neuron that drives the trade decides what a family can do, and it is drawn by lot.** The activation function, the aggregator and the plasticity rule of the output neuron, by family:

| population | output neuron | what it does | what the family did (8.15, 8.19) |
|---|---|---|---|
| overnight run, runs 3 to 6 | `sgn` of a weighted sum, 98 to 100% of agents | only −1 or +1 | always in the market: not one flat bar in 29 million |
| three families, family 1, runs 2 to 4 | `tanh` of a weighted sum, 100% | the ordinary neuron | lost most of the night, found the slow strategy at the end |
| three families, family 2, runs 2 to 4 | `cos` of a product of about nine weighted inputs, 94 to 100% | a product of many small numbers is near zero, and cos(0) = 1 | held long all night |
| three families, family 3, runs 3 to 4 | `sin` or `tanh` of one input, 96 to 100% | passes one hidden neuron on | learned the fast reversal |
| every family, end of free trading (run 1) | change of the inputs since the last bar (`diff_product`), 90 to 98% | reacts to every change | 260 to 490 positions per episode |

- Within a family the output neuron is one type in 94 to 100% of agents. Between families it differs. So it is set by which agent's descendants took over, early, and then kept.
- Family 2's draw explains its night without any other cause.
- The overnight run's agents were never flat because their output could not be: `functions:trinary/1` is flat between −0.33 and +0.33, and `sgn` never lands there.
- Free trading selects an output that reacts to change. The cost ramp then needs another kind, and each family reaches one by a rare mutation. That is where the families part.

**2. The hidden neurons are not being selected on their type.** Three-family run, end of run 4, all 1,384 neurons: activation `linear` 24%, `tanh` 17%, `cos` 15%, `absolute` 12%, `gaussian` 8%, `sgn` 8%, `sqrt` 6%, `log` 6%, `sin` 4%. Aggregator: weighted sum 37%, product 32%, change since last bar 31%. Plasticity: 82% of neurons carry one of eleven rules. The eight best agents of each family have the same mix as the rest, within a few points, and so do the overnight run's populations. Nine functions, three aggregators and twelve rules are 324 kinds of neuron, kept in circulation by mutation.

**3. The mutation mix.** Of the 449 units of weight that apply to a neural agent, 25 (5.6%) go to `mutate_weights`, which changes the weights of one neuron, and 100 (22%) go to the four operators that change a neuron's function, aggregator or plasticity. With `episode_tuning` off, `mutate_weights` is the only way a weight changes.

**4. A time-of-day pattern, in our file.** Breedon and Ranaldo (2013) report that a currency tends to fall during its own trading hours, and that for EUR/USD this makes a simple strategy. Krohn, Mueller and Whelan (2024) report reversals around the daily fixes. On our file (`analysis/r2_clock.js`, to the end of 2017 only; the file's clock is London time):

- 2012 to 2016: EUR/USD falls 0.65 to 1.05 pips an hour from 09:00 to 13:00 and rises 1.1 to 1.2 an hour from 16:00 to 18:00. The hourly profile of 2012 to 2014 correlates +0.77 with that of 2015 to 2016.
- A rule fixed in advance, short 08:00 to 12:00 and long 16:00 to 20:00, two positions a day: +2.9 pips a day after cost in 2012 to 2016 (t 3.1), which is +2.0 per 1,000 bars. In 2017 +1.3 a day (t 0.8): one year can neither confirm nor reject it.
- It does not depend on the fill: two positions a day, none at the rollover, and a minute's delay is nothing against a four-hour hold.
- The clock sensor feeds a neuron in a third of the final three-family agents. It is not among the sensors an agent starts with.

**5. Sensors that cost and have shown nothing.** The chart sensor (600 inputs) is in 87% of the final three-family agents and all of the overnight run's last population. That run's 6-neuron agent without it made the same money as the 48-neuron agent with it (8.15). The raw one-minute lists took 27% of an agent's time in the profile of 8.20.

**6. How the price lists are scaled.** The indicators are scaled by the last day's typical move and clipped, so they read the same in a quiet year and a busy one. The price lists are scaled by the price level, so the same pattern arrives two to three times larger in a busy year. Work on trading networks normalises returns by volatility (Lim, Zohren and Roberts 2019). Fractional differencing has single studies in its favour and is not a priority.

**7. Substrate encoding.** Sher (2011) reports that substrate-encoded agents reading a price chart generalised more consistently than directly encoded agents reading a price list. I could not read the paper's tables, so I do not know its sample sizes or whether costs were charged. Against it here: the substrate evaluates about 7 times slower (4,800 against 35,000 bar-steps a second, measured early in this review), and indirect encodings do well on regular problems and fall behind on irregular ones (Clune et al. 2011; van den Berg and Whiteson 2013). Twelve indicators that each mean something different are an irregular input. Not recommended now; it can be one family in a side-by-side test once evaluation is cheaper.

**8. Other points from the literature.**

- Mixed activation functions gave smaller networks on regression and classification benchmarks (Turner and Miller 2017). Nothing found for noisy trading scores.
- Hebbian plasticity helps when the task changes within a lifetime, such as a damaged robot (Najarro and Risi 2020). Nothing found that it helps a trading agent, and here it changes the weights within every episode.
- Noisy scores: average more samples or keep a larger population (Jin and Branke 2005). The selection memory already averages across generations.
- Studies of FX trading rules find profits at zero cost and none at realistic cost, repeatedly. That matches sections 8.12 and 8.21.

**Recommended.**

1. *Plain neurons:* `tanh` and `linear`, weighted sum only, no plasticity. Three settings in the run configs, no code. It removes family 2's failure by construction, makes a flat output reachable, and is the kind of network a fast evaluator can run.
2. *More weight mutation:* raise `mutate_weights` from 5.6% of mutations to about half.
3. *The clock among the starting sensors.*
4. *Drop the chart sensor and the raw minute lists* from the forex morphology.
5. *Price lists scaled by volatility.* A test, after the others.
6. *The free-trading stage:* shorten or drop it once neurons are plain. A test.

**Not recommended:** substrate encoding, more activation functions, sensors from other markets (no data in the project).

**Built 2026-11-13: items 1, 2 and 4, in the run configs only.** Activation `tanh` or `linear`, weighted sum, no plasticity; `mutate_weights` 350 of 699 units of mutation weight (it was 25 of 449), with `mutate_pf`, `mutate_aggrf` and `mutate_plasticity_parameters` left out; no raw one-minute lists and no chart image. Checked (scratch `e26`): eight generations of a fresh three-family population at 8 episodes, no stall, runaway or error; every neuron is `tanh` (55%) or `linear` (45%) of a weighted sum with no plasticity; the sensors are the four price lists, the indicators, the clock and, in 14% of agents, the account state. Two things differ from every earlier start: positions per episode in free trading are 21 to 53 where they were 260 to 490 (the output that reacted to every change is gone), and a generation took 8.0 to 8.3 seconds against 9.0 to 11.7 with the old neurons and sensors. Not yet shown: that a full run learns better. The three changes and the clock go in together, so a better or worse night cannot be put down to one of them.

**Built 2026-11-13: item 3, the clock.** `{init_sensors, series_clock}` puts `fx_Clock` among the starting sensors, and `{clock_harmonics, 2}` gives it six values: sine and cosine of the time of day once and twice a day, and of the place in the week. The second daily pair lets one neuron pick out a window of a few hours. Year, month and date are left out: they never repeat within the training years. The account-state sensor (position and open profit) is unchanged and still arrives by mutation only. The sensor stores its own parameters, so agents saved before keep the four-value clock and replays need no setting. Checked (scratch `e26`): values as expected for a known time; four generations of a fresh three-family population, no stall or runaway; every agent has the clock and it feeds a neuron in 91 of 120.

**Sources.** Sher 2011, arxiv.org/abs/1111.5892. Clune, Stanley, Pennock and Ofria 2011, stars.library.ucf.edu/facultybib2010/1196. van den Berg and Whiteson 2013, dare.uva.nl/id/23661abd-4628-4238-b948-3857541337a0. Turner and Miller 2017, arxiv.org/abs/1703.07122. Najarro and Risi 2020, arxiv.org/abs/2007.02686. Jin and Branke 2005, www.cs.le.ac.uk/people/sy11/ECiDUE/JinBranke-TEVC05.pdf. Breedon and Ranaldo 2013, www.snb.ch/en/publications/research/working-papers/2011/working_paper_2011_04. Krohn, Mueller and Whelan 2024, www.bankofcanada.ca/2021/10/staff-working-paper-2021-48/. Lim, Zohren and Roberts 2019, arxiv.org/abs/1904.04912. FX rules and costs, www.jbs.cam.ac.uk/wp-content/uploads/2020/08/wp0030.pdf.


### 8.23 Episodes in isolation, reproducible runs, the four-arm experiment and re-simulation (set up, not run)

**Short version.** Four changes, each with a test; every test passes. (1) Each episode of an evaluation now starts from a freshly reset network. Before, the 32 episodes ran back to back on one network, and on 48 saved agents of the three-family night reversing the order of the same episodes changed the money of 51% of agent-episodes, by 6.7 pips on average. While measuring that I found a second fault: a sensor off the path to the trade could read the next bar, so the same agent on the same episodes gave different results from run to run (17.5% of agent-episodes). Both are fixed. (2) Three environment variables choose the arm and the seeds; with the same seeds two runs give identical `GEN_SUMMARY` score columns. (3) The four arms (feedforward or recurrent, price lists scaled by level or by volatility) are set up, with the position as a sensor, the rollover rule, validation at every run's last generation and four more baselines. (4) Three chronological folds and a re-simulation tool are set up; the tool reproduces the simulator's own validation figures exactly. Nothing was run at full size. All four changes are settings whose defaults keep the old behaviour; the run configs switch them on.

**Data and machine.** Every test ran on a copy of `fx_tables/EURUSD1.txt` cut at the end of 2017 (2,225,779 rows, 2012-01-02 to 2017-12-29), made in the scratch area, so no test could read 2018 or later. `EURUSD_M1_MIDPOINT_UTC.txt` was not read. One VM at a time, started with `+Bd +scl false`, memory watched from outside and capped at 3.5 GB: the peak was 0.65 to 1.2 GB, and 2.5 GB once, in an instrumented debugging run.

#### 1. Episode isolation

**What was wrong.**

1. *Episodes ran back to back.* At an episode boundary `fx:episode_trade/6` reset the account and moved to the next episode, but nothing reached the network. A neuron's recurrent inputs, its plastic weights and the previous input that the `diff_product` aggregator keeps in the neuron's process dictionary all carried from one unrelated episode into the next. The 30 warm-up bars started from whatever the previous episode left.
2. *The reset between evaluations was incomplete.* `exoself:reset_network/1` (empty mailboxes, then a zero on every recurrent output) left the `diff_product` memory in place, and a message from a neuron still computing when its target emptied its mailbox survived into the next evaluation. Plastic weights were put back, but by the weight messages sent after the reset, not by the reset.
3. *Found while measuring: a sensing race.* The simulator acts on a bar's trade as soon as the actuator sends it. A sensor whose neurons reach the trade only through recurrent links is not waited for, and if it is scheduled late it reads the next bar. Twenty copies of one saved agent run at the same time on the same episode diverged at different bars (one copy from all others at scored bar 29); run alone, the agent repeated exactly, on ten scheduler threads and on one. Instrumented copies showed the cause: the raw 90-close list sensor of that agent, whose only neuron feeds the trade through a recurrent link, returned a different list in 2 of 20 copies from some bar on.
4. *Found and not changed:* `exoself:spawn_Scapes/5` starts one simulator per sensor that names it and links every sensor and the actuator to the last one. An agent with ten sensors has nine idle simulators. It changes nothing an agent does; it cost me one wrong measurement (below) and costs a few hundred idle processes per generation.

**What changed.**

- `config.erl`: `episode_isolation/0` and `sense_before_trade/0`, both `false` by default; both `true` in every run config (`exp_runner:reference_base/2`).
- `fx.erl`: at an episode boundary with isolation on (`episode_trade/6`), the simulator tells the exoself `episode_boundary` and holds the actuator's answer, so the cortex cannot start the next bar (`boundary_wait/6`); a sense that arrives meanwhile is answered from the bar just finished. With isolation on, `eval_mode` also puts the simulator back to idle (`on_eval_mode/4`): a late sensor could otherwise start the next evaluation on the old episode set. `sim/3` is split into `sim_main/3` and `sim_senses/3`: with `sense_before_trade` a bar's trade waits in the mailbox until every sensor has read that bar (the exoself sends the count, `{_,sensors,N}`). New `internals/2` (also answers the position sensor of section 3).
- `exoself.erl`: `episode_boundary/3` resets the network and tells the simulator to go. `full_reset/2`: (1) every sensor answers once it has sent what it was asked for, (2) every neuron empties its mailbox and holds, (3) once all have answered, a second emptying removes what a still-busy neuron sent after its target's first one, (4) each neuron goes back to the weights the evaluation started with, drops its `diff_product` memory and sends the zero on its recurrent outputs. `reset_network/1` uses `full_reset/2` when isolation is on and the old reset (`legacy_reset/1`) when it is off; the tuning path calls it too (its tail is now `tuning_evaluated/7`). `prep/3` sends each simulator the number of sensors linked to it, not to the list of simulators started (point 4 above).
- `neuron.erl`: `reset_hold/2` (the old `reset`, or `reset_flush` then `reset_full`). `sensor.erl`: answers `reset_prep`.
- *Watchdog.* While the exoself waits in a reset, its usual watch does not run, so the wait has its own (`await_ready/4`): a process with more than `runaway_queue_limit` messages waiting is a runaway, and no answer within `stall_timeout_ms` is a stall (`give_up/3`, logged as a stall with the step that did not answer). After a reset the stall clock starts again from zero, so a reset never counts as silence.
- Warm-up: unchanged, 30 unscored, flat bars per episode; they now start from the reset state.

**Test** (`eval_protocol:isolation_check/0`, permanent). A five-neuron agent built by hand with every kind of memory: H1 reads the indicators and its own previous output, H2 the hourly list through `diff_product`, H3 the indicators with a hebbian rule, H4 the 15-minute list and feeds the output only through a recurrent link (a sensor off the path to the trade), O reads H1 to H4 and itself. Three train episodes (A 2013-02-15, B 2014-10-30, C 2016-05-26), run as B alone, A then B, C then B:

| | B alone | A then B | C then B |
|---|---|---|---|
| isolation off: B's money, pips | −58.5 (47 positions) | +60.25; position differs at 543 of 969 bars | −103.5; differs at 120 of 969 |
| isolation on | −58.5 | identical at all 969 bars, same money | identical |

Passes. Its first version failed: it also compared the score (z and alpha), which takes out the drift of the whole episode set, and the three sets differ. It now compares positions and money only. `eval_protocol:g0/0` passes all 12 checks; `random_walk_check/0` gives +0.0022 and −0.0810 (standard error 0.050), as in 8.9; `fx_series:test/2` passes (258 checks on 57,419 rows of March and April 2015).

**How much the old results depended on episode order** (`eval_protocol:order_effect/3`, on the 16 best survivors of each family of the three-family night, end of run 4: 48 agents of 5 to 14 neurons with plasticity, `diff_product`, the chart and raw list sensors; 32 training episodes of 2012 to 2016 drawn with a fixed seed; a quarter pip). Each comparison runs the same agents on the same episodes twice:

| comparison | agent-episodes whose money changed | mean change, pips | largest, pips | mean change of score | rank correlation of scores | better half kept |
|---|---|---|---|---|---|---|
| sensing as before (race present) | | | | | | |
| same order twice, no isolation | 17.5% | 1.8 | 149 | 0.06 | 0.97 | 100% |
| reversed, no isolation | 54.1% | 7.0 | 220 | 0.11 | 0.83 | 83% |
| sensing in order (`sense_before_trade`) | | | | | | |
| same order twice, no isolation | 0 | 0 | 0 | 0 | 1.00 | 100% |
| reversed, no isolation | 51.2% | 6.7 | 197 | 0.17 | 0.65 | 61% |
| rotated by half, no isolation | 43.5% | 6.0 | 126 | 0.12 | 0.72 | 78% |
| no isolation against isolation, same order | 56.8% | 8.8 | 197 | 0.14 | 0.84 | 91% |
| reversed, isolation on | 0 | 0 | 0 | 0 | 1.00 | 100% |

The score is the t-statistic of 8.9 (alpha_t); without skill its spread is 1. One episode's money varies by about 47 pips (8.16). So in old results about half of all agent-episodes carried several pips from the episode before, the agents' scores moved by a sixth of their no-skill spread, and when the order was reversed two in five agents of the better half (what rank selection keeps) left it. On top of that, a sixth of agent-episodes changed from one run to the next through the sensing race alone. With isolation and ordered sensing, reversing changes nothing on these agents, whatever their plasticity or aggregators.

An earlier version of the second half of this table, taken before point 4 was found, showed 17.6% changes for "same order twice" with ordered sensing switched on: the count had never reached the simulator. The numbers above are from the corrected code. The forward run's population (`trial6`) was not measured.

*Cost.* Same 48 agents and 32 episodes: 47.4 s without isolation, 51.6 s with it (in reverse order 50.6 and 51.1 s); ordered sensing showed no measurable cost (47.4 s with it, 53.6 s without, in separate VMs).

#### 2. Reproducible seeds and per-run settings

`exp_runner:get_run_configs/0` now reads three environment variables. With none set (or set empty) it returns exactly what it returned before (`reference_run_configs/0`; checked with `=:=`).

- `DXNN_TEST_ARM`: an arm of section 3 or 4. Any other value stops the experiment at start (`{unknown_test_arm,...}`), so a typo cannot run the wrong experiment.
- `DXNN_EPISODE_SEED`: an integer; each run gets `{episode_seed,{Seed,RunIndex}}`. With a plain integer, as `episode_seed` was used before, generation G drew the same episodes in every run of a chain; now each chained run draws its own (`episodes:rand_state/1`).
- `DXNN_EVOLUTION_SEED`: an integer; each run gets `{evolution_seed,{Seed,RunIndex}}` (new, `config:evolution_seed/0`). A value that is not an integer stops the experiment.

The arm and seeds are stored with the experiment's run configs, so a resumed experiment keeps them, and the `run_start` line of the experiment log names them.

*Every source of randomness and time on the path from the first population to the next generation, and what was done:*

| source | where | now |
|---|---|---|
| the first genotypes: sensors, weights, functions, encoding | `genotype`, `morphology`, `plasticity` in the process that creates the population | seeded with `{Seed,Run,0}` just before (`exp_runner:prep/3`, `config:seed_rand/1`) |
| selection and mutation | `selection_algorithm`, `genome_mutator`, `mutate_check`, `tot_topological_mutations`, `plasticity` in the population monitor | seeded with `{Seed,Run,Generation+1}` just before each generation's selection, so a stage does not depend on what ran before it in that process, and a run resumed from a checkpoint continues as it would have |
| offspring made in spawned processes, each with a generator seeded from the clock, finishing in any order | `selection_algorithm:parallel_create_mutants/2` | with the seed set they are made one after another in the seeded process |
| repair of a network whose sensors are all unconnected, during its evaluation | `exoself:link_Sensors/4` | the exoself is seeded from the seed, the agent's fingerprint (its structure without ids) and its generation |
| the evaluation itself | network and simulator | deterministic with `sense_before_trade` (section 1); checked on 60 concurrent copy-runs |
| episode draws | `episodes:begin_generation/2` | `DXNN_EPISODE_SEED` |
| order of the agents in the generation summary | the ledger table, ordered by agent id | sorted by what was measured, so sums of floats come out the same |
| ids of agents, neurons, sensors, species: `1/time` | `genotype:generate_UniqueId/0` | not changed. Their values differ between two runs, but with the seed set they are created in the same order, and nothing reads their value except to break ties in a sort, where it follows creation order. Making them a counter would have to survive restarts and several experiments in one database |
| population and lineage ids: time stamps and four random letters | `exp_runner:generate_population_id/1` | not changed: names only, drawn in the caller's process, not the one that creates the population |
| run length by wall time (`run_time_limit_s`) | reference runs 3 and 4 | not reproducible by nature; the arms use fixed generation counts |
| stall and runaway watchdog | `exoself` | not changed: it acts on timing, but only on networks that are broken |
| weight perturbation when `episode_tuning` is on | the neurons' own generators | not seeded (tuning is off in every config) |
| a Mnesia transaction that restarts on a lock conflict and draws again | `genome_mutator:mutate/1` | not changed; nothing else writes while selection runs, and no restart was seen |

**Test.** Reference configs, one family of 24, 5 generations, 40 validation episodes, each run in a fresh VM and a fresh database: `DXNN_EPISODE_SEED=7 DXNN_EVOLUTION_SEED=11` twice, then `DXNN_EVOLUTION_SEED=12`. Every `GEN_SUMMARY` column except `gen_seconds`, `ms_per_bar`, `vm_mb` and `processes` (56 per line):

| | lines | columns compared | columns that differ |
|---|---|---|---|
| same seeds | 5 | 280 | 0 |
| evolution seed 11 against 12 | 5 | 280 | 75 (best and median score, eligible, positions per episode, network size, survivors, ...) |

Passes. The same holds across a chained run with every new setting of section 3: `rec_vol` twice with seeds 1 and 1, two runs of 2 and 1 generations, 168 columns, none differs. `GCP_TOOLING_REQUEST.md` names the third variable and the new arm tags; no variable was renamed.

#### 3. The four-arm experiment (set up, not run)

`DXNN_TEST_ARM` = `ff_level`, `rec_level`, `ff_vol` or `rec_vol` (`exp_runner:arm_run_configs/1`). Each arm is the reference setup of `reference_base/2` (neural encoding, tanh and linear, weighted sum, no plasticity, the present mutation mix, the four price lists, the indicators, the six-value clock, isolation, ordered sensing) and:

- *Same start for both architectures* (`{init_hidden_neurons,3}`, `genotype:construct_InitialHiddenLayer/7`): three hidden neurons in layer 0, each reading every starting sensor, and one output neuron in layer 0.5 reading the three and driving the trade. Before, an agent started as one neuron reading one sensor or all of them. `rec`: `{init_recurrent,true}` gives each hidden neuron a link to itself, and mutation may add recurrent links (`connection_architecture` recurrent). `ff`: no recurrent link at the start, and `{connection_architecture,feedforward}`, which the mutation operators and the connectivity repair already respected (`genome_mutator:filter_OutlinkIdPool/3`, `filter_InlinkIdPool/3`), so none can appear.
- *Price lists* (`{plt_encoding,level|vol}`, stored in each sensor's parameters as `[TF,N]` or `[TF,N,vol]`; `fx_series:answer_at/3`). `level` is as before. `vol`: the same closes and the same difference from their mean, divided by the last day's mean one-minute move (floored at 1e-5 of the price) times sqrt(TF·N/6), the deviation a random walk with that move would have over the window, and clipped to ±5, as `fx_IND` is. Only rows up to the decision bar are read.
- *Position as a sensor* (`{position_sensor,true}`): `fx_Position` (−1, 0 or 1, nothing about open profit) replaces `fx_Internals` in the forex morphology and is among the starting sensors, read by every hidden neuron. `genome_mutator:remove_sensor/1` treats it, like the clock, as not carrying prices.
- *Fill rules in `fx:ep_step/4`*, shared by agents and scripted rules: `{rollover_rule,true}`: no position opened, closed or reversed on a bar whose hour on the file's clock is 21, 22 or 23. `{late_fill,false}`: when true (stress tests only), the signal of a bar is filled at the next bar's close.
- One family of 48, 32 episodes, fixed generation counts, no wall-time limit: free 30, ramp 60, then 150 generations at a quarter pip in three runs of 50. Each chained run's population stays in Mnesia, so one is saved every 50 generations.
- `{validate_last_generation,true}`: every agent is validated at generation 0 (as before) and at the last generation of each run (`exoself:validation_due/0`). Generation 0 of a chained run re-validates the survivors of the run before; that is half of a pass repeated, about 2 minutes per run at full size.
- `{extra_baselines,true}` (also in the reference configs): `GEN_SUMMARY` ends with `flat_`, `long_`, `mom60_` and `clock_` `score` and `pips` on the generation's score episodes, and `_val`, `_val_pips` on validation (`eval_protocol:extra_baselines/2`): never trading, always long, the 60-bar momentum rule (mirror of the 60-bar reversal rule) and the time-of-day rule of 8.22 (short 08:00 to 12:00, long 16:00 to 20:00). Flat and always long open fewer than 2 positions per episode and get the floor score; read their pips. The time-of-day rule opens about 1.6 per episode and often gets the floor too.

**Tests.**

- *Each arm starts and runs* (`DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1`, 24 agents, 40 validation episodes, two chained runs of 2 and 1 generations): all four completed, every generation had 24 agents, `stalls=0`, `runaways=0`, no error; validation ran at generation 0, at the last generation of run 1 and at generation 0 of run 2. A generation (32 score and 40 validation episodes) took 22 to 28 s. A fold arm (`fold_2015`, 8 agents, 1 generation) also started; its 32 score episodes all fell in 2012 to 2014 and its validation set was the 370 episodes of 2015.
- *Census* (`eval_protocol:census/1`) of both saved populations of each arm:

  | arm | architecture | agents with a recurrent link | recurrent links per agent | price lists | sensors present and connected | neurons |
  |---|---|---|---|---|---|---|
  | ff_level | feedforward 24 | 0 | 0 | level 24 | position, indicators, clock, price lists: 24 each | 4.0 to 4.1 |
  | rec_level | recurrent 24 | 24 | 3.0 to 3.1 (2.9 to 3.0 self-links) | level 24 | the same | 4.1 |
  | ff_vol | feedforward 24 | 0 | 0 | vol 24 | the same | 4.0 |
  | rec_vol | recurrent 24 | 24 | 3.2 to 3.4 (2.9 to 3.0 self-links) | vol 24 | the same | 4.1 to 4.3 |

  No agent carries the open-profit sensor.
- *Fill rules* (`eval_protocol:fill_rule_check/0`, permanent, 40 train episodes): a random rule that changes position on 20% of bars changed it 686 times in hours 21 to 23 without the rollover rule and 0 times with it; with the late fill the 15-bar momentum rule's position at every bar equals its position at the bar before without it. Passes.
- *The new encoding* (`fx_series:test/2`, now with the four `vol` lists): 430 checks on 2015 rows and 280 at the start of the table (2012-01-02 to 2012-02-10, where the floor and the padding apply), all identical to a rebuild from a table cut at that row. Passes. Over 4,000 random bars of 2013 to 2016: standard deviation 1.40 to 1.47 per list, 0.1% to 1.1% of values at ±5; `fx_IND` 1.11. Year by year the hourly list has 1.37 to 1.49 with `vol` and 0.66 (2014) to 1.39 (2015) with `level`: the old lists arrive twice as large in a busy year as in a quiet one, the new ones do not.

**The twelve runs.** Same episode seed for all, three evolution seeds per arm. From the training-core folder, one VM at a time:

```
DXNN_TEST_ARM=<arm> DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=<seed> caffeinate -is erl +Bd +scl false -noshell -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." > run_<arm>_e<seed>.out 2>&1
```

| run | DXNN_TEST_ARM | DXNN_EPISODE_SEED | DXNN_EVOLUTION_SEED |
|---|---|---|---|
| 1 | ff_level | 1 | 1 |
| 2 | ff_level | 1 | 2 |
| 3 | ff_level | 1 | 3 |
| 4 | rec_level | 1 | 1 |
| 5 | rec_level | 1 | 2 |
| 6 | rec_level | 1 | 3 |
| 7 | ff_vol | 1 | 1 |
| 8 | ff_vol | 1 | 2 |
| 9 | ff_vol | 1 | 3 |
| 10 | rec_vol | 1 | 1 |
| 11 | rec_vol | 1 | 2 |
| 12 | rec_vol | 1 | 3 |

On the GCP fleet the same three pairs per VM: `--env DXNN_TEST_ARM=ff_level --env DXNN_EPISODE_SEED=1 --env DXNN_EVOLUTION_SEED=1` (syntax from `GCP_TOOLING_RESPONSE.md`; I have not run the fleet). Runs that share one folder share its Mnesia and its `logs/`; every `GEN_SUMMARY` line names its population and the `run_start` line in `logs/exp_runner/exp_runner.log` names the arm and seeds, but a folder per run is easier to read.

*Time, estimated, not measured at full size:* in the tests the small networks ran at about 70,000 bars a second on this Mac. A full generation of 48 agents on 32 episodes is 1.5 million bars, about 20 s; 240 generations about 80 minutes; ten validation passes of 48 agents on 369 episodes about 4 minutes each. About 2 hours per run while networks stay near four neurons, more as they grow (nine-neuron networks ran at about half the speed, 8.20).

#### 4. Chronological folds and re-simulation (set up, not run)

**Folds.** `fold_2015` (train 2012-01-02 to 2014-12-31, validate 2015), `fold_2016` (train to 2015-12-31, validate 2016), `fold_2017` (train to 2016-12-31, validate 2017). They differ from an arm only in the spans. A plain fold name uses `rec_level`; any arm can be put on a fold with a plus sign. `fold_2017` has the spans of the four arms, so with the same seeds it is the same run as `rec_level` (section 2): run it only with another evolution seed.

```
DXNN_TEST_ARM=fold_2015 DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1 caffeinate -is erl +Bd +scl false -noshell -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." > run_fold_2015_e1.out 2>&1
DXNN_TEST_ARM=fold_2016 DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1 (same command) > run_fold_2016_e1.out 2>&1
DXNN_TEST_ARM=fold_2017 DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1 (same command; identical to rec_level seed 1) > run_fold_2017_e1.out 2>&1
```

For the folds of another arm: `DXNN_TEST_ARM=ff_vol+fold_2015`, `ff_vol+fold_2016`, `ff_vol+fold_2017`. For more seeds per fold: `DXNN_EVOLUTION_SEED=2` and `3`.

**Re-simulation** (`resim.erl`, new). `resim:run(Agents,EpisodeSet,Label)` runs saved agents again, unchanged, through the ordinary path (an exoself on the named episodes, `eval_protocol:run_agents_on/4`), once per condition: 0.25, 0.5 and 1 pip per position, each with the rollover rule on and off and the fill now and one bar late, twelve conditions (`resim:conditions/0`; a subset can be given). Stored positions are never re-scored: the agents read their own position, so a different rule can change what they do. Episode sets: `validation`, `{tile,Span,MaxK}`, `{sample,Span,K,Seed}` or a list. Isolation follows the config in force unless given. It writes `logs/Resim/<Label>/`:

- `episodes.csv`: per condition, agent and episode: money after cost, positions, bars in the market, bars per position, the largest drawdown inside the episode (pips), and whether the money rebuilt bar by bar from the positions equals the simulator's own figure;
- `agents.csv`: per condition and agent: money per episode with standard errors from day blocks and from week blocks of the file's clock (each calendar day's, or ISO week's, money is one observation: sqrt(blocks) × sd(block money) / episodes), the two t values, positions and bars per episode, drawdowns, eligibility and score;
- `conditions.csv`: per condition: agents, eligible, the median money per episode of the eligible, how many make money and how many score above zero.

**Test.** On `rec_vol`'s test population saved at the end of its first run (24 agents), on its 40 validation episodes of 2017, all twelve conditions (17 s each). At 0.25 pip with the rollover rule on and the fill now it gives 24 eligible agents, a median of −3.521875 pips per episode and 6 agents scoring above zero. The simulator's own validation of the same networks, in generation 0 of the next run (`GEN_SUMMARY`), reported `val_agents=24`, `val_net_pips_ep=-3.5219`, `val_positive=6`. All 11,520 episode rows rebuild to the simulator's money; no agent had an error. Passes. (The other conditions, for the record: half a pip −8.4, one pip −17.2; rollover rule off −5.9; one bar late −3.5 pips per episode, median of 24 three-generation agents.)

**Added afterwards: a pilot arm.** `DXNN_TEST_ARM=pilot` is `rec_vol` cut to three chained runs of 6, 6 and 8 generations with 40 validation episodes, at full size otherwise (48 agents, 32 episodes). It exists to time a machine and to exercise the cloud path end to end. Checked only that it resolves to those settings and that a misspelt arm still stops the experiment; it has not been run.

#### Not done, and limits

- No run at full size; the twelve runs and the fold runs are set up only.
- `trial6` was not measured for episode order; one saved population was.
- Ids stay time-based and population names stay time-stamped (section 2 says why that does not affect reproducibility). Weight tuning is not seeded. Runs limited by wall time cannot be reproduced.
- The idle extra simulators of `spawn_Scapes/5` are left as they were.
- The `vol` lists come out a little wider than `fx_IND` (1.4 against 1.1); the divisor is the random-walk one, not fitted.
- Not gated by a setting, though not behaviour: the generation summary sorts its rows before summing (the same values to the 4 decimals printed), the validation pass restarts the stall clock after its reset, a neuron held in a reset accepts `terminate`, and the `run_start` log line carries more fields.

**Files changed:** `config.erl`, `fx.erl`, `exoself.erl`, `neuron.erl`, `sensor.erl`, `eval_protocol.erl`, `exp_runner.erl`, `episodes.erl`, `population_monitor.erl` (one line before selection; the checkpoint hooks are untouched), `selection_algorithm.erl`, `genotype.erl`, `morphology.erl`, `genome_mutator.erl`, `fx_series.erl`, `resim.erl` (new), `README.md`, `GCP_TOOLING_REQUEST.md`, `REVIEW.md`. `launcher.erl` is unchanged, and so are the hooks named in `GCP_TOOLING_REQUEST.md`. Nothing was committed.

### 8.24 The night arm (set up 2026-11-13, not yet run)

**What it is.** `DXNN_TEST_ARM=night`: the `rec_vol` setup of 8.23 (recurrent links allowed, three hidden neurons at the start, price lists scaled by volatility, position sensor, clock, plain neurons, rollover closed, episodes isolated, sensing in order) with four families of 48 agents on the same episodes. Free trading 20 generations, the cost ramp 40, then four runs of 45 generations at a quarter pip: 240 generations, 180 of them at the target cost, against 97 in the three-family night of 8.19. Each run leaves its population in Mnesia.

**Validation.** Every agent at the last generation of each run, on 120 episodes of 2017. A full pass of 192 agents on all 369 would take up to half an hour; the full year is for the replay afterwards (`resim.erl`).

**Length.** One generation of 192 four-neuron agents took 72 seconds on this Mac (one generation measured, then the test was stopped). If networks double in cost through the night that is about 7 hours with the validation passes. Each quarter-pip run is capped at 100 minutes, so the worst case is about 8 hours; a run ended by its cap skips its validation pass (finding F7 of `REVIEW_8_23_FINDINGS.md`).

**What it is not.** It is one configuration, so it does not say whether recurrence or the new scaling helps: that is the four-arm experiment. It answers whether the corrected simulator and the new setup learn at all over 180 quarter-pip generations, in how many of four families, and what the agents look like under the rollover rule.

**How it will be read.** Per family: when survivors' score on new episodes passes 0.3, the validation readings against the rule baselines in the same line, and whether any family holds one side. Then the best agents of each saved population re-simulated on the training years and on 2017 under the twelve conditions of 8.23. Nothing is run on 2018 to 2020.

### 8.25 The night run: every family learned to score, and the score had stopped meaning money

**The run.** `DXNN_TEST_ARM=night`, started 2026-11-13 at 21:36, finished 04:38: 240 generations in 7 hours 1 minute, six chained runs, 192 agents in four families. No stall, no runaway, no run ended by its time cap, `vm_mb` 1,449 at the start. A generation took 72 seconds at the start and 106 at the end.

**In the log it looks like success.** Over the 180 quarter-pip generations, with the rollover closed:

| | family 1 | family 2 | family 3 | family 4 |
|---|---|---|---|---|
| survivors' score on the next generation's new episodes | +1.18 | +1.27 | +0.86 | +0.59 |
| median agent, money per episode on new training episodes, pips (se) | +1.7 (0.4) | +1.2 (0.4) | +0.3 (0.5) | +4.4 (0.6) |
| positions per episode | 11 | 9 | 5 | 14 |
| agents holding one side, of 48 | 1.6 | 1.1 | 1.6 | 0.3 |
| neurons at the end | 6 | 19 | 13 | 6 |

No family failed to start and none held one side, which is what plain neurons were meant to fix (8.22). The 20-generation mean of survivors' score was above 0.3 from the first window in every family. But the score is three times what it was in the family that learned in 8.19 (+0.97) while the money is the same or less.

**The replay shows why.** The four best agents of each family at the end of runs 4 and 6 (32 agents), run again on 600 episodes of 2012 to 2016 and on all 369 episodes of 2017, a quarter pip, rollover closed (scratch `e30`, 10 minutes, peak under 2 GB; standard errors from day blocks):

| family, end of run 6 | score on the 600 training episodes | money per episode, pips (t) | in the market | money on 2017 (t) |
|---|---|---|---|---|
| 1 | +1.1 to +4.6 | −1.1 to +0.8 (−0.6 to +0.5) | 39 to 56% | +0.5 to +1.3 (+0.3 to +0.8) |
| 2 | +5.0 to +5.4 | −1.6 to −1.0 (−1.0 to −0.7) | 36 to 40% | −1.2 to −0.3 |
| 3 | +4.7 to +5.3 | −1.5 to −1.3 (−0.9 to −0.7) | 45 to 46% | −0.4 to +0.1 |
| 4 | +1.7 to +2.8 | +0.8 to +3.3 (+0.4 to +1.4) | 99% | −0.6 to +1.1 |

Families 2 and 3 have a score of +5 on 600 fresh episodes and lose money on them. Across the 32 agents the higher the score, the lower the money: rank correlation −0.71 on the training episodes (+0.13 on 2017).

**How a score of +5 loses money.** The score (`alpha_t`, 8.9) is the average over episodes of money divided by the size of the agent's own exposure in that episode. An agent controls that divisor. The best agent of family 2, per episode:

- it wins 54% of episodes, by +21 pips on average, and is in the market for 292 bars in those;
- it loses 46%, by −31 pips, and is in the market for 514 bars in those;
- in the third of episodes with its smallest exposure it makes +6.3 pips, in the third with its largest −15.6.

It takes a small gain and leaves, and stays in a losing position. A small gain over a small exposure is a large ratio; a large loss over a large exposure is a moderate one. The average ratio is high and the sum of money is negative. Family 3's best agent is the same (+24 and 373 bars against −37 and 617). Family 4 is in the market 99% of the time, cannot change the divisor, and its score and its money agree.

**Why now.** Until 8.22 the agents could not do this: their outputs kept them in the market all the time (the `sgn` output of 8.15) and few could see their own position. Plain `tanh` outputs can be flat, and the position sensor of 8.23 tells the agent whether it is in. I added both. The assumption behind the score, that it is about 0 ± 1 for an agent without skill however it trades, holds only when the time in the market does not depend on how the position is doing.

**What stands.**

- No candidate. The best training-year money is +3.3 to +4.2 pips per episode (t 1.4 to 2.1, family 4 and one agent of family 2 at run 4) and nothing is above +1.3 on 2017. No agent was run on 2018 to 2020.
- The machinery works: 7 hours unattended, deterministic evaluation, four families that all search.
- Family 4 is the honest reading of this setup with the rollover closed: about +2 to +3 on the training years, about zero on 2017. That is in line with what 8.21 predicted for the rollover rule (+3.6 for the old agents, +1.3 to +2.5 for fixed rules).

**Next, proposed, not built.** A score the agent cannot move by choosing when to be in: the money of each episode, drift taken out, divided by the market's own movement in that episode (the `alpha` already computed per episode), and the t-statistic of that across the episodes. The existing eligibility rule stays. Check before any run: on these 32 agents the new score must rank with money, and a "take the gain, hold the loss" policy written by hand must score at or below zero.

### 8.26 A score the agent cannot inflate: the first form fails on last night's agents; a second form, `alpha_pips_t`, passes

**Short version.** The proposed score of 8.25, `alpha_money_t`, is built and passes every hand-written test: no-skill policies score about 0 ± 1, and "take the gain, hold the loss" no longer scores above its money (its excess over the money's own t-statistic fell from +2.8 to +4.8 under `alpha_t` to +0.3 to +0.4). It **fails** the test that matters, on last night's 32 agents: its rank correlation with money is **+0.30** (needed at least +0.7), and the two families that lose money (2 and 3 at the end of run 6) still score **+0.8 to +1.2**. `alpha_t` gives −0.71 on the same replay, as in 8.25. By the rule of the task I stopped there: the run configs are unchanged, the per-generation warning (step 3) and the pilot were not done. Two causes were found and measured (below). A variant that drops one of them passes every test but is a different formula, so it was the owner's decision. **Added afterwards, on the owner's go-ahead:** that variant is now built as `alpha_pips_t`. With the built code it passes (a) to (c), (e) and the +0.7 of (d) (rank correlation +0.73); families 2 and 3 of run 6 fall from +5 to +0.04 to +0.32, about zero but not at or below it, because with the drift taken out they do not lose money. See the later parts of this section. Steps 3 and 4 were then done with `alpha_pips_t`: every arm uses it, the pilot ran twice with identical results, and `rho_score_money` was 0.87 to 1.00 in every generation.

**Data and machine.** Every test ran in a scratch copy: the code, a copy of `Mnesia.nonode@nohost`, and a copy of `EURUSD1.txt` cut at 2017-12-29 (2,225,779 rows), so nothing could read 2018 or later. `EURUSD_M1_MIDPOINT_UTC.txt` was not read. The folder's Mnesia, logs and `run.out` were not touched. One VM at a time, `+Bd +scl false`, memory watched from outside: peak 0.75 to 1.42 GB; the longest run was 6 min 18 s. Settings: the night arm's last run (`DXNN_TEST_ARM=night`, run 6: 0.25 pip, rollover rule on, fill now, 2 positions per episode minimum).

#### The formula and the floor

`alpha_money_t = mean(alpha) / max(sd(alpha), F) * sqrt(K)` over the K episodes of an evaluation (`fitness_functions:money_t/2`). An episode's `alpha` is the one `fx:ep_stat/4` already computed: money after cost minus `mu × sum(u)` (the span's drift), divided by `sqrt(sum(r²))`, the market's own movement in that episode. `sd` is the sample standard deviation over the K episodes; with K = 1 it is 0 and the floor applies. With F ≤ 0 and sd = 0 the score is 0. The eligibility rule (`if_eligible/2`) and the floor and ruin values are unchanged. One addition: the score is kept above −99, so an eligible agent always ranks above every ineligible one (which get −1000 plus at most 900).

**How the floor F was chosen.** `eval_protocol:sd_floor_table(1280,0.25)`: policies without skill that enter a random side with a fixed probability and hold it 10, 60 or 240 bars, on 1,280 train episodes at 0.25 pip with the rollover rule. sd of the per-episode alpha:

| time in the market | hold 10 bars | hold 60 | hold 240 | sd as one 32-episode evaluation sees it (median, the three holds) |
|---|---|---|---|---|
| 1% | 0.089 | 0.088 | 0.077 | 0.08, 0.07, 0.03 |
| 2% | 0.126 | 0.123 | 0.116 | 0.12, 0.12, 0.08 |
| 3% | 0.174 | 0.149 | 0.146 | 0.16, 0.14, 0.11 |
| 5% | 0.214 | 0.198 | 0.194 | 0.20, 0.18, 0.17 |
| 10% | 0.298 | 0.288 | 0.271 | 0.30, 0.29, 0.25 |
| 25% | 0.486 | 0.446 | 0.424 | 0.49, 0.43, 0.40 |
| always in (switches 1 bar in 250) | 0.873 | | | 0.89 |

The sd grows like the square root of the time in the market. **F = 0.15** (`{alpha_money_sd_floor, 0.15}`, the default of the new setting): the sd of an agent without skill in the market 3% of the time. An agent without skill in the market 5% of the time or more is above it (0.19 and up), so the floor does not hold it back; an agent that is in for less, or whose gains are steadier than chance allows, is divided by 0.15 instead of its own sd. The same table without cost (600 episodes) gave the same sds to within 0.01.

#### Step 2: results

Source: `eval_protocol:money_score_check()` for (a) to (c), `resim` and a scratch analysis for (d).

**(a) No skill, no cost.** Disjoint 32-episode evaluations, score = what selection would see. Trade minimum off so that every block is scored.

| policy | episodes | in the market | positions per episode | `alpha_money_t` mean | sd | `alpha_t` mean | sd |
|---|---|---|---|---|---|---|---|
| random walk, always in | 640 (19 evaluations) | 100% | 3.0 | +0.03 | 1.06 | +0.03 | 1.06 |
| random walk, a tenth of the time | 640 | 9.9% | 4.9 | −0.21 | 0.86 | −0.22 | 0.79 |
| random walk, trading often | 640 | 66% | 86 | +0.07 | 0.96 | +0.08 | 0.95 |
| train, always in | 1,280 (40 evaluations) | 99% | 2.7 | −0.15 | 0.89 | −0.12 | 0.78 |
| train, a tenth of the time | 1,280 | 9.8% | 4.4 | −0.31 | 1.03 | −0.30 | 0.86 |
| train, trading often | 1,280 | 65% | 76 | +0.08 | 0.94 | +0.07 | 0.89 |

All within three standard errors of zero, spread 0.86 to 1.06. Pass.

**(b) Take the gain, hold the loss.** `{gain_hold,X,60}`: enter a random side; leave as soon as the open position is ahead by X pips; a position not ahead is held to the end of the episode; after a gain, 60 bars flat, then a new random side (re-entry is needed to have the 2 positions per episode). 600 train episodes (`{sample,2012-01-02..2016-12-31,600,77}`, the episodes of (d)), 0.25 pip. "Money's own t" is the money per episode over its standard error across the 600 episodes; "excess" is a score minus that.

| | money, pips per episode (se) | money's own t | `alpha_t` | `alpha_money_t` | in the market | positions per episode |
|---|---|---|---|---|---|---|
| X = 3, fixed seed | −1.64 (1.71) | −0.96 | **+4.13** | −0.54 | 67% | 5.8 |
| X = 3, mean of 20 other seeds | −0.41 | | +4.61 (above 2 in 20 of 20) | +0.09 (above 2 in 0) | | |
| X = 3, excess, mean of 20 seeds | | | +4.84 | +0.32 | | |
| X = 8, fixed seed | **+1.13** (1.91) | +0.59 | **+3.31** | +0.91 | 82% | 3.5 |
| X = 8, mean of 20 other seeds | **+0.79** | | +3.25 (above 2 in 20) | +0.78 (above 2 in 1) | | |
| X = 8, excess, mean of 20 seeds | | | +2.83 | +0.37 | | |

As asked, alpha_t is clearly above zero for both. The new score is not: it stays within 0.4 of the money's own t-statistic. **The pass rule as written fails for X = 8**, and the reason is the policy, not the score: on these episodes taking 8-pip gains and holding losses makes money (+1.13 pips per episode on the fixed seed, +0.79 on average over 20 seeds), and the new score is positive because the money is. Its score (+0.91) is still within two standard errors of zero. I replaced "money at or below zero" in the permanent check with "the score does not exceed the money's own t by more than 2 on the fixed seed, and by less than 1 on average over 20 seeds". This is a change to the pass rule, and the owner should judge it.

**(c) Tiny and steady.** No policy without skill can make a small gain in nearly every episode, so the test policy cheats: `{peek,Steps}` reads the next bar's close on three bars per episode and holds that side for that bar. Same 600 episodes, mean of 32-episode evaluations:

| cost | money, pips per episode | sd of alpha | `alpha_money_t` | the same without the floor | `alpha_t` |
|---|---|---|---|---|---|
| none | +3.07 | 0.048 | 2.15 | 7.82 | 7.63 |
| 0.25 pip | +2.46 | 0.047 | 1.66 | 6.14 | 4.80 |

The floor cuts the score by 3.6 times, to what an agent in the market 3% of the time making the same money would get. Pass (below 3). It does not go to zero, because the money is real.

**(d) Last night's agents. Fails.** The best four agents by stored fitness of each of the four families of the populations ending `_run4` and `_run6` (32 agents, the same selection as 8.25, `n1:top/2`), `resim` on `{sample,{{2012,1,2},{2016,12,31}},600,77}`, 0.25 pip, rollover on, fill now, night settings: 6 min 8 s, all 32 eligible, all 600 episodes rebuild to the simulator's money.

| family | money per episode, pips, run 4 / run 6 | `alpha_t`, run 4 / run 6 | `alpha_money_t`, run 4 / run 6 | in the market |
|---|---|---|---|---|
| 1 | −0.46 to +1.27 / −1.11 to +0.81 | +3.50 to +5.10 / +1.07 to +4.62 | +0.46 to +1.65 / −0.26 to +1.32 | 39 to 56% |
| 2 | −1.75 to +4.18 / **−1.55 to −1.00** | +1.52 to +5.73 / +5.02 to +5.42 | +0.16 to +1.46 / **+0.83 to +1.23** | 36 to 82% |
| 3 | +1.15 to +2.30 / **−1.53 to −1.32** | +2.74 to +3.28 / +4.71 to +5.33 | −0.63 to +0.35 / **+0.81 to +1.08** | 44 to 50% |
| 4 | +1.80 to +3.73 / +0.83 to +3.28 | +1.69 to +2.93 / +1.72 to +2.78 | +1.93 to +3.33 / +1.99 to +3.10 | 99% |

Rank correlation with money over the 32 agents: `alpha_t` **−0.71** (8.25 reproduced), `alpha_money_t` **+0.30**. The new score puts family 4 at the top, which is right, but it does not push families 2 and 3 of run 6 to zero or below.

**Why: two things separate alpha from money.** From the same replay's per-episode statistics (`fx:ep_stat/4`: gross, cost, drift `mu`, average position, volatility), for every agent:

1. *The drift.* `alpha` takes out the span's drift, and the train span fell: `mu` = −4.1 × 10⁻⁷ per bar, −4.0 pips per 970 bars. Families 2 and 3 of run 6 are net long (average position +0.36 to +0.46), so the fall alone costs them 1.5 to 1.8 pips per episode. Their money with the drift taken out is **+0.05 to +0.49** pips per episode, about zero. A score that takes the drift out cannot rank with plain money for agents like these. The drift is taken out on purpose (8.9: otherwise holding the side the span happened to go is rewarded).
2. *Dividing each episode by its own volatility.* The market decides the divisor, but the agent decides in which episodes it wins. Families 2 and 3 of run 6 make +0.9 to +1.7 pips per episode in the quietest third of episodes and lose 5.0 to 6.8 in the busiest third (median episode movement 51 pips, range 16 to 216). A fixed few-pip gain in a quiet episode is a large alpha, and a loss in a busy one is a moderate alpha. So per-episode alpha still pays a milder form of "small gains, large losses". For family 2 of run 6, the t-statistic of the drift-free money in pips is +0.04 to +0.32, and `alpha_money_t` is +0.83 to +1.23.

Rank correlation over the 32 agents of each candidate, with plain money and with drift-free money:

| score (t-statistic over the 600 episodes) | with money | with drift-free money |
|---|---|---|
| `alpha_t` (z: divided by the agent's own exposure) | −0.71 | −0.37 |
| `alpha_money_t` (alpha: drift out, divided by the episode's volatility) | +0.30 | +0.75 |
| money divided by the episode's volatility, drift kept | +0.94 | +0.78 |
| plain money in pips | +0.99 | +0.70 |
| **drift-free money in pips, no volatility division** | **+0.73** | **+0.99** |

**The variant that passes (measured first in scratch code, then built: see below).** `mean(m) / max(sd(m), F) * sqrt(K)` with `m` = money after cost minus `mu × sum(u)`, in pips: the drift is taken out as now, but there is no per-episode division. Floor by the same rule: the sd of drift-free money of a no-skill agent in the market 3% of the time is 9.1 to 12.2 pips (1% 4.4 to 4.9, 5% 12.2 to 13.9, always in 56.1), so F = 9 pips. Scratch code (`a2.erl` in the scratch area), same episodes and seeds as above (the built version gives the same figures to the last digit):

| test | result |
|---|---|
| (a) no skill, the six rows above | means −0.23 to +0.11, sd 0.83 to 1.05 |
| (b) X = 3: fixed seed / excess over the money's own t, 20 seeds | −0.95 / 0.00 |
| (b) X = 8: fixed seed / excess, 20 seeds | +0.60 / 0.00 |
| (c) tiny and steady, no cost / 0.25 pip | 1.96 / 1.57 (without the floor 6.98 / 5.63) |
| (d) rank correlation with money over the 32 agents | **+0.73** |
| (d) families 2 and 3 of run 6 | +0.04 to +0.32 (drift-free money +0.05 to +0.49 pips) |

It passes (a) to (c) and the +0.7 of (d), but families 2 and 3 of run 6 come out slightly above zero, not at or below it, because their drift-free money is slightly above zero. Its weakness is the reverse of `alpha_money_t`: the busy episodes dominate both the mean and the sd, so skill in quiet episodes counts for less. That is what money does too.

**(e) The existing checks.** `isolation_check/0` passes (B alone −62.75 pips, identical after A and after C with isolation on). `fill_rule_check/0` passes (686 position changes in hours 21 to 23 without the rule, 0 with it). `fx_series:test/2` passes (330 checks on 63,413 rows of March and April 2015). `random_walk_check/0` gives +0.0022 and −0.0810, as in 8.9 and 8.23. **`g0/0` fails three checks under the night settings:** the constant long, constant short and validation-set constant checks. The cause is the rollover rule, not this work. The code from before this section (the copy used for 8.25's replay) fails the same three under the same settings, and with the rollover rule off all twelve checks pass with the new code. A constant position cannot be opened when an episode starts in hours 21 to 23, so it is not constant. g0 was last reported passing (8.23) without the night settings. The check should either switch the rule off for itself or allow for it; not changed here.

#### Added afterwards: `alpha_pips_t`, built and tested

**What it is.** `{fitness_function, alpha_pips_t}`: `mean(m) / max(sd(m), F) * sqrt(K)` over the K episodes of an evaluation, with `m` the money of an episode after cost with the span's drift taken out, in pips: `(sum(u·r) − mu·sum(u) − cost) / pip_size`. This is `timing` in `fx:ep_stat/4`, the numerator that `alpha` and `z` both divide; there is no division per episode at all. The eligibility rule, the floor and ruin values and the −99 limit are those of `alpha_money_t` (the same `fitness_functions:money_t/2`). **F = 9 pips** (`{alpha_pips_sd_floor, 9.0}`): the sd of drift-free money of a no-skill agent in the market 3% of the time on the train years, 9.1 to 12.2 pips by holding time (`sd_floor_table(1280,0.25)`, columns `sd_pips`; 1% 4.4 to 4.9, 2% 6.8 to 7.8, 5% 12.2 to 13.9, 10% 16.7 to 17.6, always in 56.1). The pips floor depends on how busy the market is in the train years, unlike the alpha floor; it would need measuring again for other years or another pair.

**Results with the built code** (`eval_protocol:money_score_check(alpha_pips_t)`, 67 s together with the floor table and the `alpha_money_t` check; `resim` for (d), 6 min 19 s, peak 1.2 GB):

| test | `alpha_t` | `alpha_money_t` | **`alpha_pips_t`** | pass rule | |
|---|---|---|---|---|---|
| (a) no skill, six policies: means / spreads of 32-episode evaluations | −0.30 to +0.08 / 0.78 to 1.06 | −0.31 to +0.08 / 0.86 to 1.06 | **−0.23 to +0.11 / 0.83 to 1.05** | about 0 ± 1 | pass |
| (b) X = 3, fixed seed (money −1.64, its own t −0.96) | +4.13 | −0.54 | **−0.95** | ≤ 2 | pass |
| (b) X = 3, 20 seeds: excess over the money's own t | +4.84 | +0.32 | **0.00** (sd 0.02) | < 1 | pass |
| (b) X = 8, fixed seed (money +1.13, its own t +0.59) | +3.31 | +0.91 | **+0.60** | ≤ 2 + 0.59 | pass |
| (b) X = 8, 20 seeds: excess; rank correlation with money | +2.83; 0.93 | +0.37; 0.92 | **0.00; 1.00** | < 1 | pass |
| (c) tiny and steady, no cost / 0.25 pip (without the floor) | 7.63 / 4.80 | 2.15 / 1.66 | **1.96 / 1.57** (6.98 / 5.63) | < 3 | pass |
| (d) rank correlation with money, 32 agents | −0.71 | +0.30 | **+0.73** | ≥ +0.7 | pass |
| (d) families 2 and 3 of run 6 (money −1.55 to −1.00) | +4.71 to +5.42 | +0.81 to +1.23 | **+0.04 to +0.32** | ≤ 0 | not met: about zero |

(d) per family for `alpha_pips_t`, run 4 / run 6: family 1 −0.20 to +0.91 / −0.44 to +0.76; family 2 −0.02 to +0.45 / +0.04 to +0.32; family 3 −0.51 to +0.62 / +0.18 to +0.29; family 4 +0.86 to +1.87 / +0.44 to +1.52. The best agent by this score is family 4's at run 4 (+1.87, +3.73 pips per episode). The families that scored +5 and lost money fall from +5 to between 0 and +0.3, a t-statistic indistinguishable from zero. They do not go below zero because, with the drift taken out, they do not lose money: +0.05 to +0.49 pips per episode. The rule "at or below zero" can only be met by a score that keeps the drift, which 8.9 removed on purpose.

(e) with the built code (`fx.erl` now also stores `timing` per episode): `isolation_check`, `fill_rule_check`, `fx_series:test/2` (330 checks) pass; `random_walk_check` +0.0022 and −0.0810 as before; `g0` passes all twelve with the rollover rule off, and fails the same three constant-position checks with it on, as before this work.

**What it does not fix.** Busy episodes weigh more than quiet ones, in the mean and in the sd, exactly as in money. An agent cannot inflate the score by when it trades; it can only raise it by making more drift-free money relative to its own episode-to-episode spread. Holding one side over the whole span scores zero over the span, as before (the drift is the span's, fixed before any agent runs).

#### Steps 3 and 4, on the owner's go-ahead

**Step 3: a warning in every generation** (`eval_protocol.erl`). `GEN_SUMMARY` and every `FAMILY_SUMMARY` line end with two fields, computed over the eligible agents of that generation (or family) from what the ledger already holds:

- `rho_score_money`: rank correlation between the score selection saw and the money per episode after cost, both on that generation's own episodes;
- `median_alpha_money`: the median over those agents of mean(alpha) (money after cost with the drift taken out, in units of the market's movement), whatever the fitness in force.

They come after the extra baselines, at the very end of the line, so nothing that reads the earlier fields by position moves. `analysis/pipeline.js` carries both into `learning_curve.csv` as its last two columns. `eval_protocol:score_money_check(PopulationId, N, K)` (and `/4` with a seed) replays the best N agents of each family by stored fitness on K training episodes drawn with a fixed seed (97 by default), at the validation cost, and prints each agent's money per episode with its standard error, the score in force, `alpha_t` and `alpha_pips_t`, and each one's rank correlation with money, over all and per family.

**Step 4: run configs** (`exp_runner.erl`). `{fitness_function, alpha_pips_t}` is in the settings every arm adds to `reference_base/2` (`arm_run_configs/1`), so `night`, `pilot`, `ff_level`, `rec_level`, `ff_vol`, `rec_vol`, the three folds and `<arm>+<fold>` all use it. Checked by resolving each name: all give `alpha_pips_t`, and the reference configs (no `DXNN_TEST_ARM`) give `alpha_t`. Nothing else in the arms changed: position sensor, rollover rule and 32 episodes per evaluation as before.

Selection memory (`selection_memory.erl`) was checked against the new scale. It ranks a decaying average of each agent's score minus its family's median, and every step is linear in the score (`sum/(weight+prior)`), so the order does not depend on the score's scale. The only fixed value it meets is the floor given to ineligible agents (−1000 plus at most 900), and the −99 limit keeps every eligible score above that. The exoself starts each agent's best fitness at −1e300, and rank selection reads only the order. Nothing assumes the old score's scale.

**Test.** `DXNN_TEST_ARM=pilot DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1`, twice, each in a fresh scratch folder with an empty database and the price table cut at 2017 (the pilot's spans are 2012–2016 and 2017). Run A: 10 min 40 s; run B: 11 min 45 s (a little over the ten minutes set, at full pilot size); peak 820 and 821 MB. Both completed all three chained runs (6, 6 and 8 generations). Every generation had 48 agents, `stalls=0`, `runaways=0`. All 20 `GEN_SUMMARY` lines are identical between the two runs in every column except `gen_seconds`, `ms_per_bar`, `vm_mb` and `processes` (1,160 values compared, 0 differ), including the two new fields.

Per generation (identical in both runs; the score is `alpha_pips_t`; validation is 40 episodes of 2017 at 0.25 pip):

| run | generation | cost, pip | eligible | best score | median score | median money per episode, pips | median_alpha_money | **rho_score_money** | validation: median money, pips (positive of 48) |
|---|---|---|---|---|---|---|---|---|---|
| 1 | 0 | 0.000 | 48 | 2.76 | -0.28 | -2.78 | -0.035 | **0.99** | -4.32 (12) |
| 1 | 1 | 0.000 | 48 | 1.55 | 0.12 | 0.76 | 0.036 | **0.98** |  |
| 1 | 2 | 0.000 | 48 | 1.11 | -0.19 | -1.61 | -0.017 | **0.99** |  |
| 1 | 3 | 0.000 | 48 | 1.75 | 0.51 | 4.19 | 0.044 | **0.97** |  |
| 1 | 4 | 0.000 | 48 | 3.20 | 0.16 | 1.65 | 0.030 | **0.98** |  |
| 1 | 5 | 0.000 | 48 | 1.98 | -0.35 | -2.65 | -0.060 | **0.98** | -4.59 (17) |
| 2 | 0 | 0.000 | 48 | 1.86 | 0.48 | 4.60 | 0.033 | **0.99** | -4.63 (14) |
| 2 | 1 | 0.004 | 47 | 2.28 | 0.40 | 3.31 | 0.051 | **0.97** |  |
| 2 | 2 | 0.008 | 48 | 2.22 | 0.15 | 1.29 | 0.004 | **0.98** |  |
| 2 | 3 | 0.013 | 48 | 2.25 | 0.81 | 6.49 | 0.143 | **0.99** |  |
| 2 | 4 | 0.017 | 48 | 2.56 | 1.69 | 18.94 | 0.273 | **0.94** |  |
| 2 | 5 | 0.021 | 47 | 1.14 | 0.54 | 4.98 | 0.100 | **0.98** | -12.40 (3) |
| 3 | 0 | 0.250 | 48 | 0.57 | -2.10 | -20.24 | -0.340 | **0.98** | -11.56 (3) |
| 3 | 1 | 0.250 | 48 | 1.19 | -1.07 | -9.30 | -0.190 | **0.99** |  |
| 3 | 2 | 0.250 | 46 | 1.06 | -0.38 | -4.08 | -0.108 | **0.87** |  |
| 3 | 3 | 0.250 | 48 | 1.45 | 1.11 | 11.59 | 0.097 | **0.98** |  |
| 3 | 4 | 0.250 | 48 | 2.15 | -3.01 | -18.71 | -0.376 | **0.94** |  |
| 3 | 5 | 0.250 | 48 | 1.84 | -0.20 | -1.48 | 0.022 | **1.00** |  |
| 3 | 6 | 0.250 | 48 | 1.36 | -0.73 | -7.83 | -0.140 | **0.96** |  |
| 3 | 7 | 0.250 | 48 | 1.20 | -0.50 | -5.05 | -0.094 | **1.00** | -11.24 (10) |

`rho_score_money` is 0.87 to 1.00 in every generation. It must be high when the score is a t-statistic of drift-free money: the gap from 1 is the drift taken out and the sd in the divisor. Under `alpha_t` the night's best agents showed −0.71 on fresh episodes (8.25). It is a warning, not proof of skill: it shows whether the score means money, not whether that money is real. Twenty generations do not show learning: validation money stays at −4.3 to −12.4 pips per episode.

*Families.* The pilot has one family and writes no `FAMILY_SUMMARY` line, so a scratch-only copy of the pilot (2 families, 2, 1 and 1 generations, 7 min) was run once. Every `FAMILY_SUMMARY` line carries both fields (`rho_score_money` 0.96 to 0.99 per family). The generation's median of mean alpha, recomputed from the agents' own `FITNESS_EVAL` log lines, agrees with the `GEN_SUMMARY` values (−0.030, +0.025, +0.030, −0.117). In generation 0 of the second run both families and the generation all show 0.0297; the agents nearest the middle sit at 0.0294 and 0.0299, so this looks like near-identical agents around the median, not a fault.

*score_money_check.* `score_money_check(<final population of run A>, 12, 600)`: 12 agents, 600 episodes, 2 min 29 s, all eligible. Money per episode −2.67 to +0.42 pips (se 1.2 to 2.3). Rank correlation with money: score (`alpha_pips_t`) +0.90, `alpha_t` +0.55. The 0.90 rather than 1.0 comes from the replay taking out the drift of its own episodes, as `resim` does.

#### Not done, and limits

- No night run with `alpha_pips_t`. The pilot shows the arm runs and is reproducible, not that agents learn under the new score.
- The pips floor (9 pips) was measured on 2012–2016 EURUSD; a fold or another pair with different volatility should measure it again (`sd_floor_table/2`, column `sd_pips`).
- Families 2 and 3 of run 6 score +0.04 to +0.32 under `alpha_pips_t`, about zero but not at or below it (above).
- The literal pass rule of (b) failed for X = 8 (above); the permanent check uses the rule stated there.
- `g0/0` still fails its three constant-position checks with the rollover rule on (above); not changed.

**Files changed:** `fitness_functions.erl` (`alpha_money_t`, `alpha_pips_t`, `money_t/2`, `episode_pips/1`, `scores/1`), `config.erl` (`alpha_money_sd_floor/0`, default 0.15; `alpha_pips_sd_floor/0`, default 9.0; documentation of `fitness_function`), `fx.erl` (`ep_stat/4` also returns `timing`), `eval_protocol.erl` (policies `always_in`, `{in_market,P,Hold}`, `{gain_hold,XPips,Wait}`, `{peek,Steps}`; `sd_floor_table/2` with the pips columns; `money_score_check/0,1`; `score_money_check/3,4`; `rho_score_money` and `median_alpha_money` in `generation_summary/2` and the family lines; `with_random_walk/2` taken out of `random_walk_check/1`, same results), `exp_runner.erl` (one setting in `arm_run_configs/1`), `resim.erl` (`agents.csv` ends with `alpha_t`, `alpha_money_t`, `mean_alpha`, `alpha_pips_t`), `analysis/pipeline.js` (two columns at the end of `learning_curve.csv`), `README.md`, `REVIEW.md`. The reference configs are unchanged. `population_monitor.erl` and `launcher.erl` are unchanged, and so are the cloud hooks (`dxnn_checkpoint_pause`, `dxnn_checkpoint_cancel`, `dxnn_runtime:finish()`, `resolve_experiment_for_continue/1`, `dxnn_runtime:resume_or_new()`). Nothing was committed.

**To start the night arm again**, now under `alpha_pips_t` (from the training-core folder, after moving last night's `Mnesia.nonode@nohost`, `logs` and `run.out` somewhere safe, since a new run writes into them):

```
DXNN_TEST_ARM=night DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1 caffeinate -is erl +Bd +scl false -noshell -eval "{ok,P}=launcher:start(), R=erlang:monitor(process,P), receive {'DOWN',R,_,_,_} -> ok end, timer:sleep(1000), halt()." > run.out 2>&1
```

### 8.27 The night arm under `alpha_pips_t`: no candidate, so 2018–2020 stays unread

**Short version.** The night arm of 8.24 ran once more, now under `alpha_pips_t` and the pass rule in `PREREG_NIGHT_ALPHA_PIPS.md`, on a GCP VM. It finished. The score tracked money in every generation (`rho_score_money` median 0.99). On 2017 all four registered candidates lost money: −2.43, −0.58, −4.45 and −34.61 pips per episode. None passes, so nothing was run on 2018–2020. The candidate choice turned out to have a flaw (ties, below). It does not change the result: all twelve tied agents lose on 2017.

**The run.** `DXNN_TEST_ARM=night DXNN_EPISODE_SEED=1 DXNN_EVOLUTION_SEED=1` on `n2-custom-12-24576` (12 vCPU, x86-64), OTP 28.4.1 with the JIT, `+scl false`. Started 2026-11-14 18:07 UTC, finished 2026-11-15 05:17 UTC (11 h 10 min, `run.json`). Six chained runs, 240 generations, 192 agents in four families of 48. Exit code 0, `stalls` 0, `runaways` 0, `vm_mb` 1,450 to 1,497. A generation took 117 s at the start and about 180 s at the end; a generation with a validation pass took 586 to 832 s. Neurons per agent: 4 at the start, 15.2 on average (at most 24) at the end, 9 to 22 by family.

**Checks on the download.** All 14 files in `final/result` match the SHA-256 values in `snapshot.json` (checked here). `config.erl` is identical to the training-core folder's (SHA-256 `7295378c…`). **Not verified:** the price-table hash registered in the PREREG (`e3047acf…`) is not recorded in any downloaded file, so I could not check that the VM loaded that table. The replay below checked its own copy: 2,225,779 rows, last row 2017-12-29 21:59. `run.json` also has `"checkpoint_error": "Error"` next to exit code 0. That is not explained.

**Warning signs, in the order the PREREG gives.**

1. *`rho_score_money`.* Median 0.99 over the 180 quarter-pip generations, lowest 0.83. Per family the medians are 0.98 to 0.99. The lowest single family value is 0.28 (family 2). No family's score rose while its money did not.
2. *Money on each generation's new training episodes,* 0.25 pip. Mean over the 180 quarter-pip generations of each generation's median; se across generations:

| | median agent | clock rule | always long | 60-bar momentum |
|---|---|---|---|---|
| all four families, pips per episode (se) | +2.24 (0.39) | +1.96 (0.44) | −1.49 (0.65) | −22.13 (0.72) |

   The agents minus the clock rule, generation by generation: +0.29 ± 0.56. By family: +0.91 (0.64), +0.71 (0.61), +2.46 (0.59) and **+6.67 (0.61)** for family 4. Generations share agents, so these se values are not from independent samples.
3. *Validation at the end of each run* (120 episodes of 2017, 0.25 pip, `GEN_SUMMARY`):

| run | 1 | 2 | 3 | 4 | 5 | 6 |
|---|---|---|---|---|---|---|
| median money per episode, pips | −6.72 | −2.24 | −0.59 | −1.11 | −0.89 | −0.81 |
| agents above zero, of 192 | 7 | 45 | 83 | 68 | 35 | 90 |

   The baselines on the same 120 episodes: always long +1.99, clock rule +0.30, 60-bar momentum −15.71, never trade 0. The population's median was below zero at the end of every run. Some families' medians were above zero: family 4 in runs 2 to 4 (+0.12, +0.91, +0.64), family 1 in run 4 (+0.28), and families 2 and 4 in run 6 (+3.35, +2.32).
4. *`score_money_check`* on the run-4 and run-6 populations (best 3 agents of each family by stored fitness, 600 training episodes, seed 97, 0.25 pip). Rank correlation of score with money: **1.00** on both. `alpha_t` gives 0.64 and 0.90. Money per episode, pips (se 2.0 to 2.5 per agent):

| family | run 4 | run 6 |
|---|---|---|
| 1 | −6.40, +2.61, +2.65 | +3.66, +4.30, +4.41 |
| 2 | −0.58, −0.38, −0.37 | −0.80, +0.45, +0.50 |
| 3 | +2.25, +2.45, +2.47 | +1.20, +2.02, +2.30 |
| 4 | +1.52, +6.76, +7.06 | **−31.53**, +2.91, +2.96 |

   The three agents of each family here share one stored fitness: they are the tied agents described below. Their money can differ a lot, by up to 34 pips per episode in family 4 at run 6.

**The registered 2017 replay** (`replay_2017/night_replay.erl`). Candidates by PREREG rule 1: per family, the agent with the highest stored fitness in the run-6 population. `resim` on all 369 episodes of 2017 (1,000 bars each), 0.25 pip, rollover rule on, fill now, price table cut at 2017-12-29 21:59, 55 s.

| family | stored fitness | money per episode, pips (se, day blocks) | t | positions per episode | bars in the market, of 1,000 |
|---|---|---|---|---|---|
| 1 | 0.485 | −2.43 (2.19) | −1.11 | 12.8 | 955 |
| 2 | 0.238 | −0.58 (2.19) | −0.27 | 3.7 | 951 |
| 3 | 0.623 | −4.45 (1.95) | −2.28 | 6.9 | 956 |
| 4 | 0.461 | −34.61 (2.98) | −11.62 | 156.7 | 940 |

Baselines on the same 369 episodes (se across episodes): always long **+2.80** (2.13), clock rule **+1.40** (1.38), 60-bar momentum −14.95 (2.19), never trade 0. No candidate is above zero, so none passes rule 3. By rule 4, 2018–2020 stays unread.

**A flaw in the pass rule: ties at the top** (`replay_2017/night_ties.erl`, `ties.out`). In each family of the run-6 population, the 48 agents hold only 24 distinct stored fitness values. Three agents share the top value. One comes from an earlier generation (7 to 9). The other two are one generation later (8 to 10) and carry one more mutation in `evo_hist`. `genotype:clone_Agent/2` copies the agent record, fitness included. So the saved population holds each survivor plus two mutated offspring that carry its score without ever having been evaluated. The candidate script sorts by fitness and takes the last of the tied agents. In all four families that was an offspring (`replay.out`: "next best" equals the candidate's value every time). Family 4's candidate, with 157 positions per episode, is an unevaluated mutant.

All 12 tied agents, on the same 369 episodes:

| family | lowest generation (the evaluated survivor) | the two offspring |
|---|---|---|
| 1 | −2.29 | −2.06, −2.43 |
| 2 | −1.62 | −1.62, −0.58 |
| 3 | −1.55 | −1.66, −4.45 |
| 4 | −0.99 | −1.03, −34.61 |

All twelve are below zero (median −1.64). The result does not depend on the tie-break. But the next pass rule must say how ties are broken, or the population must be saved before offspring are made. The 8.16 protocol had such a rule ("offspring carry their parent's number; the parent is taken"); `PREREG_NIGHT_ALPHA_PIPS.md` left it out. That the lowest-generation agent is the evaluated parent is inferred from generation and `evo_hist`. I did not trace it parent by parent.

**What stands.**

- No candidate. Nothing dated 2018 or later was read.
- The score now means money: 0.99 per generation, 1.00 on fresh episodes. Under `alpha_t` the same arm gave −0.71 (8.25).
- In-sample, the median agent made about what the clock rule made, and family 4 made more (+6.67). On 2017 no tied agent beat always long or the clock rule.
- At the end the agents were in the market on 98 to 99% of bars (`active`).

**Files.** `AWS-Deployment/output/gcp/downloads/dxxn-gcp-dxnn-checkpoints/night-alpha-pips/night-alpha-pips-e1b/final/result/` (logs, `run.json`, `snapshot.json`, Mnesia backups) and `…/final/replay_2017/` (`replay.out`, `ties.out`, `Resim/prereg_2017/`, `Resim/ties_2017/`, `baseline_*.csv`, the two `.erl` scripts).

### 8.28 What a position really costs: Dukascopy bid and ask, 2003–2017

**Source.** `session_flow/cost/COST_SCHEDULE.md` and `COST_TABLES.md` (git repository `session_flow`, commit `fff50db`). Dukascopy one-minute bid and ask bars for seven pairs, 2003–2017. The figures are half-spreads at each minute's close. A position is two half-spreads, plus a commission of 0.2 bp of value per side where stated. No return was computed and nothing dated 2018 or later was read. Rebuild: `python3 cost_schedule.py` in `session_flow/cost/` (about 8 minutes).

**EUR/USD, one position, pips** (London hours = 03:00–11:59 New York):

| period | spread only, London hours | spread only, all hours | spread only, hour 17 NY | + commission, London hours | + commission, all hours | + commission, hour 17 NY |
|---|---|---|---|---|---|---|
| 2008–2012 | 0.90 | 1.10 | 1.77 | 1.45 | 1.65 | 2.31 |
| 2012 | 0.76 | 0.87 | 1.67 | 1.27 | 1.39 | 2.19 |
| **2013–2017** | **0.27** | **0.36** | 1.13 | **0.75** | **0.84** | 1.61 |

**Against what this log assumed** (0.25 pip per position, 0.5 as the central case):

- The bare spread in liquid hours, 2013–2017, is about the 0.25 assumed (0.27). The assumption leaves the commission out. The commission alone is 0.44–0.53 pip per position, twice the whole 0.25.
- With commission: 0.75 pip in London hours (3.0 × 0.25, 1.5 × 0.5) and 0.84 over all hours (3.4 × 0.25, 1.7 × 0.5). In 2012, the first training year, it is 1.39 over all hours.
- With commission, no minute of 2003–2017 cost 0.5 pip or less.

**The rollover.** Hour 17 New York costs 2.6 to 4.3 times the cheapest hours, depending on the pair. Minute by minute (Monday–Thursday, 2013–2017), the spread peaks at 17:00 at 4 to 7 times the 06:00–11:00 level. For EUR/USD that is 0.88 against 0.13 pip of half-spread. It halves within 5 to 10 minutes and stays at about 2 to 3.5 times daytime until 17:59. At 18:00 it drops in one step. The Sunday open is worse: EUR/USD 1.47 pip of half-spread at 17:00. The Asian session costs only 15–45% more than the cheapest hours.

**The proposed cost model** (COST_EVIDENCE.md section 8: a year level times an hour cell, plus a term for the previous minute's range). It was fitted on 2008–2014 and checked on 2015–2017. The pass test was fixed before the fit: at least 2% lower RMSE under both year-level settings, and at least half the under-prediction removed on the 1% largest-range minutes. **The range term passed for no pair and no form.** For EUR/USD its fitted coefficient is about 0. The hour-and-year schedule carries almost all the predictable cost. The main error is the year level: carrying 2014 forward under-predicts EUR/USD's 2015–2017 mean by 31%.

**Limits, as the source states them.** These are one vendor's quotes (Dukascopy SWFX) at the close of each minute, not fills. Slippage, size, depth, last-look rejections and latency are not measured. The level before 2013 may reflect how Dukascopy quoted (spreads sit on steps of 1.0, 1.4 and 2.0 pips); no second source was checked. Trust the shape (hour multipliers, rollover profile) more than the level. The commission is the project's assumption (Interactive Brokers, 0.2 bp per side); Dukascopy's own would add another 0.3–0.4 pip. Financing at the rollover is not included.

### 8.29 The time-of-day rule on a basket of six pairs: registered, run once, STOP

**What was tested.** The rule of 8.22 item 4, written as a template for other pairs. Short the home currency 08:00–12:00 in its own centre and long it against the dollar 11:00–15:00 New York (`session_flow/PREREG_BASKET.md`, commit `7a6525d`). Six pairs other than EUR/USD (GBP/USD, USD/JPY, AUD/USD, USD/CAD, USD/CHF, NZD/USD), equal notional, 2008–2017. Fills at the ask to buy and the bid to sell, plus 0.2 bp commission per side. t is the smaller of the day-block and week-block t. GO at t ≥ 3, OWNER DECIDES at 2 to 3, STOP below 2. Amendment 1 (`PREREG_BASKET_AMENDMENT_1.md`, commit `a174bb5`) fixed the data and what each outcome means, before the run. It changed no rule. Run once on 2026-11-15 (`RESULT.md`, `result.txt`, commit `e20ca03`).

**Result.**

| line (2008–2017) | bp a day | t day / t week | days |
|---|---|---|---|
| **six-pair basket, bid/ask + commission (decides)** | **−2.48** | −4.97 / **−5.06** | 2,604 |
| the same, 5 minutes late | −2.88 | −5.80 / −5.99 | 2,605 |
| the same, spreads × 2 | −5.40 | −10.79 / −10.88 | 2,604 |
| the same, 2003–2007 (reported only) | −3.59 | −6.08 / −6.07 | 1,216 |
| **six-pair basket at the mid, no cost** | **+1.21** | **+2.42** / +2.45 | 2,604 |
| EUR/USD alone, bid/ask + commission (seven-pair line) | +1.41 | +1.75 / +1.80 | 2,600 |

- **Decision printed by the tool: STOP** (t −5.06). The basket after cost is negative in every year 2008–2017 (−0.22 to −4.43 bp a day).
- At the mid without cost, t_mid = +2.42. That is the amendment's row "OWNER DECIDES or STOP, t_mid 2 to 3: not settled. The owner decides. The default is to stop." No owner decision is recorded in these files.
- At the mid, the home legs carry more than the dollar legs (+0.70, t 2.35, against +0.51, t 1.25). USD/JPY is negative even at the mid (−2.45, t −2.99).
- The data guards were not triggered. Lowest coverage was NZD/USD 0.895 and AUD/USD 0.919.
- The amendment had said in advance that at these spreads a STOP was the expected outcome even if the pattern is real.

**Data problems, reported next to the result** (`market_data/dukascopy_m1/DATA_REPORT.md`; they do not change it):

- **AUD/USD 2003 and 2007–2010 failed the clock check.** In those years it opens Sunday 22:00 UTC in every season and often stops Friday 16:00 UTC. The rollover spread points to short quoting hours on a UTC clock rather than a shifted clock, but that is not proven. The test was run on these data by the owner's decision (2026-11-15, before the run; amendment section 1).
- The tool skipped 2,475 crossed AUD/USD rows (ask below bid, all 2007–2010) and 1 USD/JPY row. There are no 2002 files.

**Corrections to earlier statements in this log.**

- *The clock of `fx_tables/EURUSD1.txt`.* 8.22 item 4 says "the file's clock is London time". Against Dukascopy, 2012–2017, the file is UTC in winter and UTC + 1 hour in summer, with the change on the **US** dates (second Sunday of March, first Sunday of November). Late-October weeks after the UK change still need the hour. So the clock is New York time plus 5 hours, as 8.13 says (`DATA_REPORT.md` sections 1 and 6; all 72 months fit). It differs from London time only in the weeks when the US and European clocks disagree. For the same reason, Appendix C's "timestamps are assumed to be UTC" and section 2.1's "apparently UTC" hold only in winter.
- *AUD/USD.* The log said nothing about AUD/USD before this section. The facts are as above: it did not pass the clock check for 2003 and 2007–2010, and it was run on the owner's decision.

### 8.30 `flat_eval`: a single-process evaluator that matches the process path (built, not wired in)

**What it is.** `flat_eval.erl` evaluates a saved agent's network in one Erlang process. The process path uses one process per neuron, sensor and actuator. `run_many/3` builds every price-list, indicator and clock input once per episode and shares it across agents. Only the position sensor is answered per agent. Git worktree `DXNN-flat-eval`, branch `sf/flat-eval`, commit `f1ef75a` (versions `356c584` and `91fb89d` before it). **Not merged, and not wired into the population loop.** No existing module was changed. Notes: `DXNN-flat-eval/FLAT_EVAL_NOTES.md`.

**Same results.** Every episode's statistics were compared field by field with `=:=`: positions at every scored bar, money, cost, entries, drift, z, alpha. The runs used scratch copies of the 8.25 populations and a price table cut at the end of 2017.

| set | agents | bars compared | mismatches |
|---|---|---|---|
| saved `_run4` + `_run6`, 100 training episodes | 384 | 37,209,600 | 0 |
| saved, all 369 episodes of 2017 | 384 | 137,303,424 | 0 |
| fresh agents, mutated or grown (992 with 21 to 42 neurons), 21 episodes | 2,091 (of 2,100 made; 9 refused) | 42,549,759 | 0 |

**Speed.** This Mac (4 performance and 6 efficiency cores). 96 agents, 20 episodes, three repeats. The shared path against the process path: **9.5 times on one core** (225,000 against 23,600 bars a second), **13.0 times on ten** (824,000 against 63,000). The process path stops gaining at 4 cores. Ten cores here are worth 6 to 7.7 single-core equivalents even for work that shares nothing. Most of the remaining time is the weighted sums (`dot/3`, 57.5% in eprof). A native network step was estimated, not measured or built, at about 35 times the process path per core. This replaces Appendix C's estimate of "20 to 100×" with a measurement.

**What it refuses** (each case was built and run through the process path, `flat_eval_check:refusal_check/2`):

- circles of non-recurrent links, sensorless self-loops that feed the trade, neurons with no input, and inputs missing from a sender's outputs. The process path stalls or runs away on these.
- malformed weights or bias, and duplicate inputs or outputs. These crash or silently change the network.
- an output neuron one or two bars ahead. On the process path its leftover trade message stays in the actuator's mailbox across the reset, so every later episode trades one bar late. flat_eval does not imitate that.
- all sensors unconnected.
- episodes long enough for stray links to approach the runaway limit.

It runs, and matches, sensorless self-loops that do not reach the trade, stray fanouts and stray outputs.

**A bug it found, not fixed.** `exoself:link_Sensors/4` calls `genome_mutator:add_sensorlink/1` outside a transaction. When that fails, it logs with `io_lib:format/1`, which does not exist, so the exoself dies with `undef` (`exoself.erl` line 765). When it succeeds, the exoself has changed the genotype at random before running it.

**What wiring it in would take** (notes section 6, not built). In `population_monitor:summon_agents/3`, give the agents `flat_eval` accepts to one helper process. That helper scores them, writes fitness and the ledger row, sends the casts the monitor counts, and writes the log lines. Everything else stays on the process path: tuning, plasticity, substrates, other aggregators, the refused shapes, and live trading.

### 8.31 The overnight-against-daytime rule on US funds: registered, not run

**What was registered** (`auction_flow/PREREG_AUCTION.md`, commit `c2d9e8e`). In every fund, every day: long overnight (buy in the closing auction, sell in the next opening auction) and short by day (sell at the open, buy back at the close). One unit each. 25 US-listed funds, SPY excluded, equal risk, 2008–2017, after a base cost of $0.0049 per share per side plus SEC and FINRA fees on sales. GO at t ≥ 3, STOP below 2. A missing or unreadable fund file gives **NO RESULT**. The tool (`auction_test.py`) was validated on SPY only: hand-built funds with dividends and splits, look-ahead tests, a planted 10 bp a day recovered (t 6.05), and 200 random walks, all STOP after cost.

**It has not been run, and the recommendation is not to run it.** The reasons:

- **SPY itself shows nothing in the test period.** In 2008–2017, SPY's overnight minus daytime was +0.29 bp a day before cost, t 0.12. In the equal-risk book the registration uses, it was +1.19 bp a day, t 0.57. The registered cost on SPY averaged 1.82 bp a day. In 1993–2007 the gross figure was +5.80 bp a day (t 3.17). The registration was written expecting a STOP.
- **The data are not complete.** `market_data/etf_daily/DATA_REPORT.md`: of 26 funds collected (the 25 plus SPY), 17 are ready. Nine are quarantined: XLF because CNBC's "unadjusted" series is rescaled in most years, and EFA, LQD, XLU, XLV, XLY, EWZ, FXI and EWJ because dividend history is missing. So 16 of the 25 registered funds are usable, and the registration voids a run with any file missing.
- **The "open" is probably not the auction price.** In the collected files the open behaves like the first consolidated trade. Sub-penny opens prove a non-auction print on 0.2% to 5% of days per fund. `EXECUTION_EVIDENCE.md` (commit `64eb069`) finds the same for other free vendors: Norgate and Alpaca document the first trade, and Yahoo does not document its open. It found no study of the size of the gap. NYSE's own figures show ETF opening prints up to 20 bp from the next five minutes' average. The registered stress of "open 5 bp worse" costs about 10 bp a day and may be too small for thin funds.
- **Cost.** The registered rule costs about 1.8 bp a day on SPY, and per-share fees weigh several times more on cheaper funds (5.86 bp a day at a $37 price). An automated run through the IBKR API with routing directed to the listing exchange would cost about 1.8 times the registered per-share amount (evidence section 0.1). Not modelled: Rule 201 days from 2011-02-28 and locate failures, when the daytime short does not fill; listing moves (TLT to Nasdaq in 2016, IEF in 2017); and the $25,000 pattern-day-trader minimum to 2026-06-04.

**What stays unseen.** No fund other than SPY has had a return computed by anyone (`DATA_REPORT.md` section 2, PREREG preamble). If the test is ever wanted, those data are still unseen. One leak, noted in the data report: onvista's and dividendhistory.org's pre-2018 scaling reveals that XLB, XLE, XLK, XLU and XLY split 2:1 after 2017 and USO reverse-split 1:8. No post-2017 price, date or return was seen. A status note is in `auction_flow/STATUS.md`.

### 8.32 Where this leaves the project

**What was found.**

- **Three ideas, all smaller than their cost.** Short-horizon reversal in EUR/USD is real at zero cost. It is worth about 0.1 pip per trade at one-minute decisions (8.9). The overnight run's agents earned 0.48 pip per position before cost in the years where it existed (8.15). Both are below the real cost of 0.75 to 0.84 pip (8.28). The time-of-day rule, outside EUR/USD, makes +1.21 bp a day at the mid and −2.48 after bid and ask (8.29). On EUR/USD alone it makes +1.41 after cost, t 1.75. The overnight effect is +0.29 bp a day gross on SPY in 2008–2017 against a cost of 1.82 (8.31; the basket was not run).
- **The real cost of trading** EUR/USD in 2013–2017 is 0.75 to 0.84 pip per position with commission (8.28), not the 0.25 this log simulated.
- **The evolution machinery works.** It finds real in-sample structure: family 4 made +6.67 pips per episode on new training episodes (8.27). Since 8.26 its score means money. It runs unattended on a cloud VM, and the pilot reproduced exactly (8.26). `flat_eval` matches it 9.5 to 13 times faster, but is not wired in.
- **Nothing found in-sample has held on 2017**, the gate year.

**What is still unread.** No agent evolved on 2012–2016 has been run on 2018–2020 (the overnight run of 8.15 trained on those years; 8.16 set them aside for the forward test). January 2025 to November 2026 has not been read by anything. The other 24 funds' returns have not been computed.

**What is open.** The owner's stated aim is to learn whether neuroevolution can find behaviour of its own that pays. None of the tests above measures that. Each one asked whether a known pattern survives cost, or whether agents beat it. The proposal on the table, **not yet decided**, has two parts: measure the smallest planted effect the system can recover and keep on unseen data, and pick an environment with many instruments and richer inputs. Nothing has been built or run for it.

## Appendix A. Methods

- **Data statistics, simulator baselines, ridge baseline.** Three Node.js scripts (no dependencies) reading `fx_tables/EURUSD1.txt` in place. The simulator re-implementation follows `fx.erl`: act at the close just observed, one position, units = round(5000/entry), spread charged once on entry, flip = close then open, fitness = balance + open P&L. Random numbers are seeded.
- **Real-system runs.** Two scratch copies of the `.erl` and `.hrl` files with the last 20,000 rows of the table, `get_run_configs/0` redirected to a one-run config: phase-1 window (`gt_start` 2600, `gt_end` 1800), 12 agents, 3 generations, `tuning_duration {const,5}`; one copy with the default substrate encoding, one with `agent_encoding_types [neural]`. Erlang/OTP 28, 10 cores. Timings come from `TRAIN_EVAL` lines, trade counts from `FITNESS_EVAL` lines. These are single short runs; treat throughput as indicative.
- **Not run.** No experiment with any proposed change; no boosted-tree baseline (no Python numerical stack was available on this machine); no test of the full 12-phase curriculum end to end.

## Appendix B. References (from memory; verify before citing externally)

- Sher, G. (2012). *Handbook of Neuroevolution Through Erlang*, ch. 19 (bundled in `.ReadMe_Files/`, checked directly).
- Stanley, K. and Miikkulainen, R. (2002). Evolving neural networks through augmenting topologies. *Evolutionary Computation*.
- Stanley, K., D'Ambrosio, D. and Gauci, J. (2009). A hypercube-based encoding for evolving large-scale neural networks. *Artificial Life*.
- Clune, J., Stanley, K., Pennock, R. and Ofria, C. (2011). On the performance of indirect encoding across the continuum of regularity. *IEEE Trans. Evolutionary Computation*.
- Lehman, J. and Stanley, K. (2011). Abandoning objectives: evolution through the search for novelty alone. *Evolutionary Computation*.
- Mouret, J.-B. and Clune, J. (2015). Illuminating search spaces by mapping elites. arXiv:1504.04909.
- Gomez, F. and Miikkulainen, R. (1997). Incremental evolution of complex general behavior. *Adaptive Behavior*.
- Jin, Y. and Branke, J. (2005). Evolutionary optimization in uncertain environments: a survey. *IEEE Trans. Evolutionary Computation*.
- Moody, J. and Saffell, M. (2001). Learning to trade via direct reinforcement. *IEEE Trans. Neural Networks*.
- Treynor, J. and Mazuy, K. (1966); Henriksson, R. and Merton, R. (1981). Market-timing measurement.
- Grinold, R. (1989). The fundamental law of active management. *Journal of Portfolio Management*.
- White, H. (2000). A reality check for data snooping. *Econometrica*.
- Bailey, D. and López de Prado, M. (2014). The deflated Sharpe ratio. *Journal of Portfolio Management*.
- López de Prado, M. (2018). *Advances in Financial Machine Learning* (purged cross-validation, embargo).
- Neely, C., Weller, P. and Dittmar, R. (1997). Is technical analysis in the foreign exchange market profitable? A genetic programming approach. *JFQA*.
- Neely, C. and Weller, P. (2003). Intraday technical trading in the foreign exchange market. *Journal of International Money and Finance*.
- Neely, C., Weller, P. and Ulrich, J. (2009). The adaptive markets hypothesis: evidence from the foreign exchange market. *JFQA*.
- Breiman, L. (1996). Bagging predictors. *Machine Learning*.
- Caruana, R. et al. (2004). Ensemble selection from libraries of models. *ICML*.

## Appendix C. Unverified items in one place

- Timestamps are assumed to be UTC (consistent with a Friday 20:59 last bar).
- Retail all-in cost levels (0.3, 0.7 pips) are typical figures, not quotes from a specific broker; swap/financing for positions held across the daily rollover is not modelled anywhere in this review.
- The 20 to 100× speed-up from a flat evaluator is an estimate from a single-neuron loop in another language.
- That depth 2 yields one hidden substrate plane is from reading `create_substrate/4`, not from instrumenting a run.
- The zero-cost out-of-sample result in section 2.4 (+$6.63) was not tested for significance.
- Availability of longer EURUSD history and other instruments from the project's data source.
- All probabilities in section 7.
