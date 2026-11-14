%% Configuration file for FX Trading System
%% Centralized configuration for all system parameters
%% Options listed in comments show available alternatives

-module(config).
-compile(export_all).

-define(CONFIG_TAB, dxnn_config).

%% ===================================================================
%% Trading & Account Setup
%% ===================================================================
account_leverage() -> get_val(account_leverage, 50).                     % Options: 1-500 (typical: 10, 50, 100, 200)
account_initial_balance() -> get_val(account_initial_balance, 300).             % Options: any positive number (typical: 100-10000)
account_lot_size() -> get_val(account_lot_size, 10000).                  % Options: 1000, 10000, 100000 (micro, mini, standard lots)
account_margin() -> get_val(account_margin, 0).                        % Options: 0-1.0 (percentage, 0=no margin requirement)
account_spread() -> get_val(account_spread, 0.0).                      % Round-trip cost in price units, charged once on entry. 0.0 = trades are free (current setting, was 0.000150 = 1.5 pips). Options: 0.0-0.001. In episode mode a cost_schedule() overwrites it every generation, and validation_spread() replaces it on validation episodes.
account_extinction_threshold() -> get_val(account_extinction_threshold, 1).    % Options: 1-inf (account equity threshold, 1=disabled extinction)
buy_money_fixed() -> get_val(buy_money_fixed, 100).                     % Options: any positive number (fixed trade size)
min_profit_threshold() -> get_val(min_profit_threshold, 0.000150).           % Options: 0.00001-0.01 (minimum profit in pips)
order_size_percentage() -> get_val(order_size_percentage, 0.2).               % Options: 0.01-1.0 (1%-100% of balance per trade)

%% ===================================================================
%% Market Data Windows
%% ===================================================================
primary_currency_pair() -> get_val(primary_currency_pair, 'EURUSD1').         % Options: 'EURUSD1', 'EURUSD1_LIVE', 'EURUSD15', 'EURUSD30', 'EURUSD60'

% Core data window configuration (all values are "bars back from latest data")
gt_start() -> get_val(gt_start, 1000).                           % Options: 1-N (starting row for training data)
gt_end() -> get_val(gt_end, 200).                              % Options: 1 to gt_start-1 (ending row for training)
bench_start() -> get_val(bench_start, 200).                         % Options: 1-N (starting row for benchmark data)
bench_end() -> get_val(bench_end, last).                          % Options: last, or specific number (ending row for benchmark)

%% ===================================================================
%% Episode-Based Evaluation (multi-window scoring)
%% ===================================================================
%% evaluation_mode = legacy   : one fixed window per run (gt_start/gt_end above), fitness from fitness_function() on that window.
%% evaluation_mode = episodes : every generation draws fresh episodes from train_span (the same ones for every agent of that
%%                              generation). Each agent is run once on the "score" episodes and that fitness goes to selection.
%%                              With episode_tuning true, exoself first tunes weights on the separate "tune" episodes.
%%                              val_span is never used for selection, only logged.
evaluation_mode() -> get_val(evaluation_mode, legacy).                  % Options: legacy, episodes
episode_tuning() -> get_val(episode_tuning, false).                     % Options: false (no weight tuning inside a generation; weights change through the mutate_weights mutation) | true (exoself tunes on the tune episodes first, tuning_duration attempts)
train_span() -> get_val(train_span, {{2022,10,3},{2024,6,28}}).          % Options: {{Y,M,D},{Y,M,D}} inclusive dates | {bars_back,StartBack,EndBack} | all
val_span() -> get_val(val_span, {{2024,7,8},{2024,12,31}}).              % Same formats as train_span. Keep a gap between the two spans.
episode_bars() -> get_val(episode_bars, 800).                           % Options: 200-5000 (bars per episode)
episode_warmup_bars() -> get_val(episode_warmup_bars, 0).               % Options: 0-500 (first bars of each episode are forced flat and not scored; lets recurrent state settle)
episodes_tune() -> get_val(episodes_tune, 3).                           % Options: 1-16 (episodes per tuning evaluation; only used when episode_tuning is true)
episodes_score() -> get_val(episodes_score, 6).                         % Options: 2-64 (episodes for the fitness reported to selection)
episodes_val() -> get_val(episodes_val, 12).                            % Options: 1 and up (at most this many fixed validation episodes tiled over val_span; a number larger than what fits means every episode of the span)
episode_anchor_step() -> get_val(episode_anchor_step, 15).              % Options: 1-240 (episode start points are every N bars)
episode_min_history() -> get_val(episode_min_history, 1000).            % Options: >= largest sensor window (bars kept before the first episode start)
episode_seed() -> get_val(episode_seed, undefined).                     % Options: undefined (random) | integer (reproducible episode draws: generation G draws the same episodes in every run of a chained experiment) | {Seed,RunIndex} (reproducible, and each chained run draws its own; what DXNN_EPISODE_SEED sets, exp_runner:get_run_configs/0)
evolution_seed() -> get_val(evolution_seed, undefined).                 % Options: undefined (random, as before) | integer | {Seed,RunIndex} (what DXNN_EVOLUTION_SEED sets): the first population's genotypes, selection and mutation are reproducible (seed_rand/1; REVIEW.md 8.23 lists what is and is not covered)
validate_population_every() -> get_val(validate_population_every, 1).   % Options: 0 (off) | N (every N generations each agent is also run on the validation episodes, for logging only)
min_trades_per_episode() -> get_val(min_trades_per_episode, 2).         % Options: 0-50, fractions allowed (average entries per episode needed for an agent to be eligible for alpha fitness; 0.5 = 16 entries in a 32-episode evaluation)
validation_spread() -> get_val(validation_spread, undefined).           % Options: undefined (validation episodes cost account_spread, like train episodes) | price units per round trip. Set it to the cost of real trading, so validation always reads at the real cost while the training cost is still being raised.
cost_schedule() -> get_val(cost_schedule, []).                          % Options: [] (account_spread is used as set) | [{Generation,Spread},...] in rising generation order: the training cost per round trip at those generations of the run, a straight line between them, the last value afterwards. Example [{0,0.0},{60,0.000025}] goes from free to a quarter pip over 60 generations. Applied at the start of every generation by population_monitor.
selection_memory() -> get_val(selection_memory, 0).                     % Options: 0 (off: selection ranks by the generation's score alone) | 0 < W < 1 (selection ranks by a decaying average of each agent's score relative to the generation's median; W is the weight of the past, 0.85 = about 7 generations. See selection_memory.erl)
selection_memory_prior() -> get_val(selection_memory_prior, 1.0).       % Options: >= 0 (evaluations' worth of "no better than the median" every agent starts with; 1 = a newcomer's first result counts half)
run_time_limit_s() -> get_val(run_time_limit_s, undefined).             % Options: undefined | seconds (the run also ends at the first generation boundary after this much wall time, as if generation_limit had been reached)
pip_size() -> get_val(pip_size, 0.0001).                                % Options: 0.0001 (EUR/USD and most pairs) | 0.01 (yen pairs). Only used to print results in pips.
episode_isolation() -> get_val(episode_isolation, false).                % Options: false (the episodes of one evaluation run back to back on one network: recurrent inputs, plastic weights and diff_product memory carry from one episode into the next) | true (at every episode boundary the simulator holds the next bar until the exoself has reset the whole network, so each episode starts from the state of a fresh network; the reset between evaluations also clears plastic weights and diff_product memory. REVIEW.md 8.23)
rollover_rule() -> get_val(rollover_rule, false).                        % Options: false | true (fx:ep_step/4: no position is opened, closed or reversed on a bar whose hour, on the file's clock, is 21, 22 or 23; the position held is kept. Agents and the scripted rules share it. REVIEW.md 8.21, 8.23)
late_fill() -> get_val(late_fill, false).                                % Options: false (an order is filled at the close of the bar the agent decided on) | true (it is filled one bar later, at the next close; a stress test, not for training. REVIEW.md 8.21, 8.23)
validate_last_generation() -> get_val(validate_last_generation, false).  % Options: false | true (every agent is also run on the validation episodes at the last generation of each run, generation_limit - 1, besides the generations validate_population_every chooses)
extra_baselines() -> get_val(extra_baselines, false).                    % Options: false | true (GEN_SUMMARY also scores never trading, always long, the 60-bar momentum rule and the time-of-day rule on the generation's episodes, eval_protocol:generation_summary/2)
sense_before_trade() -> get_val(sense_before_trade, false).             % Options: false | true (the simulator does not act on a bar's trade until every sensor of the agent has read that bar. Without it a sensor whose neurons reach the actuator only through recurrent links can read the bar after, depending on timing, so the same agent can trade differently on the same episodes. REVIEW.md 8.23)

%% seed_rand/1 seeds the calling process's random number generator from evolution_seed() for one stage of a run, and
%% does nothing when the setting is undefined. Stage is an integer: 0 for the creation of the first population
%% (exp_runner:prep/3), Generation+1 for the selection and mutation that end that generation (population_monitor).
%% Seeding at each stage, not once, keeps a stage reproducible whatever ran before it in the same process, and a run
%% resumed from a checkpoint at a generation boundary continues as the uninterrupted run would have.
seed_rand(Stage) ->
	case evolution_seed() of
		{Seed, Run} when is_integer(Seed), is_integer(Run) -> rand:seed(exsss, {Seed, Run, Stage}), ok;
		Seed when is_integer(Seed) -> rand:seed(exsss, {Seed, 0, Stage}), ok;
		_ -> ok
	end.

%% The training cost per round trip for a generation of the current run, from cost_schedule() (see above).
scheduled_spread(Generation) ->
	scheduled_spread(cost_schedule(), Generation).
scheduled_spread([], _Generation) ->
	account_spread();
scheduled_spread([{G0, S0}|_], Generation) when Generation =< G0 ->
	S0;
scheduled_spread([{G0, S0}, {G1, S1}|_], Generation) when Generation < G1 ->
	S0 + (S1 - S0) * (Generation - G0) / (G1 - G0);
scheduled_spread([{_G, S}], _Generation) ->
	S;
scheduled_spread([_|Rest], Generation) ->
	scheduled_spread(Rest, Generation).
fitness_lcb_z() -> get_val(fitness_lcb_z, 1.0).                         % Options: 0-3 (only for alpha_lcb = mean - z*std/sqrt(K) over episodes)
alpha_money_sd_floor() -> get_val(alpha_money_sd_floor, 0.15).        % Only for alpha_money_t: the smallest sd of the per-episode alpha the score divides by (fitness_functions:money_t/2). Derived from data, REVIEW.md 8.26: the sd of alpha of a no-skill agent in the market a few percent of the time
alpha_pips_sd_floor() -> get_val(alpha_pips_sd_floor, 9.0).          % Only for alpha_pips_t: the smallest sd, in pips, of the per-episode drift-free money the score divides by (fitness_functions:money_t/2). REVIEW.md 8.26: the sd of a no-skill agent in the market 3% of the time on the train years (9.1 to 12.2 pips)

%% ===================================================================
%% Sensor Profiles
%% ===================================================================
%% --- Standard Forex Trader ---
pli_resolutions() -> get_val(pli_resolutions, [90, 270]).                    % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], etc. (creates one sensor per value)
pci_horizontal_resolutions() -> get_val(pci_horizontal_resolutions, [30]).         % Options: [10], [20], [50], [10,20], [20,50], etc. (creates one sensor per value)
pci_vertical_resolutions() -> get_val(pci_vertical_resolutions, [20]).           % Options: [10], [15], [20], [10,15], [15,20], etc. (PCI: Cartesian product HRes×VRes)

%% --- 1-Minute Optimized Trader ---
pli_1m_resolutions() -> get_val(pli_1m_resolutions, [5,10,20]).            % Options: [5], [10], [20], [5,10], [10,20], [5,10,20], [3,5,10,20]
pci_1m_horizontal_resolutions() -> get_val(pci_1m_horizontal_resolutions, [20,30]).   % Options: [10], [20], [30], [10,20], [20,30], [10,20,30]
pci_1m_vertical_resolutions() -> get_val(pci_1m_vertical_resolutions, [10,15]).     % Options: [5], [10], [15], [5,10], [10,15], [5,10,15]
internal_sensor_dimensions() -> get_val(internal_sensor_dimensions, 3).            % Options: 1-10 (trading state dimensions: position, profit, time)
%% Sensors answered from the shared price arrays (fx_series.erl). Off by default; the run configs switch them on.
%% They are not available in live trading yet.
plt_specs() -> get_val(plt_specs, []).                                   % Options: [] (off) | [{TF,N},...] price list of the last N bars of TF minutes, TF in 15, 60, 240, 1440. e.g. [{15,24},{60,24},{240,30},{1440,20}]
indicator_sensor() -> get_val(indicator_sensor, false).                  % Options: false | true (fx_IND: 12 indicators, see fx_series:ind/2)
clock_sensor() -> get_val(clock_sensor, false).                          % Options: false | true (fx_Clock: time of day and day of week)
validation_series() -> get_val(validation_series, false).                % Options: false | true (keep every validation episode's position at each bar; only eval_protocol:shift_test/3 and replay tools read them, and they cost about 15 MB per agent per 370 episodes while it validates)
families() -> get_val(families, 1).                                      % Options: 1 | N (N independent species of specie_size_limit agents each: selection, offspring and selection memory stay inside a family; they share each generation's episodes. Fixed when the population is created. REVIEW.md 8.18)
generation_test() -> get_val(generation_test, true).                     % Options: true | false. true: every 500 evaluations the population monitor stops and runs each species' best agent alone, one after the other, on the validation episodes (population_monitor:run_GenTest/2). Nothing in the training core reads the result, and while it runs one agent has the machine to itself (REVIEW.md 8.20)
clock_harmonics() -> get_val(clock_harmonics, 1).                        % Options: 1 (fx_Clock gives four values: time of day and place in the week, each as a sine and a cosine) | N (N daily pairs, 2N+2 values; 2 lets a neuron pick out a window of a few hours). Fixed when an agent's sensor is created
plt_encoding() -> get_val(plt_encoding, level).                          % Options: level (fx_PLT lists scaled by the price level, as before) | vol (scaled by the trailing volatility, on the scale of fx_IND; fx_series:answer_at/3). Stored in each sensor when it is created, so saved agents keep theirs
position_sensor() -> get_val(position_sensor, false).                    % Options: false (the account-state sensor fx_Internals: position and open profit, by mutation only) | true (fx_Position instead: the position only, -1, 0 or 1, among the sensors a new agent starts with)
init_hidden_neurons() -> get_val(init_hidden_neurons, 0).                % Options: 0 (a new agent is one neuron per actuator output, reading one or all starting sensors, as before) | H (H hidden neurons, each reading every starting sensor, and one output neuron reading them; genotype:construct_InitialHiddenLayer/7)
init_recurrent() -> get_val(init_recurrent, false).                      % Options: false | true (with init_hidden_neurons > 0: each hidden neuron of a new agent also reads its own previous output)
init_sensors() -> get_val(init_sensors, one).                           % Options: one (a new agent starts with one price sensor drawn at random) | series (it starts with every fx_PLT list and fx_IND; see morphology:first_sensors/1) | series_clock (the same and fx_Clock)

%% ===================================================================
%% Evolution Strategy
%% ===================================================================
evaluation_timeout_ms() -> get_val(evaluation_timeout_ms, 600000).               % No longer used. Stalled networks are caught by the two settings below.
%% Watchdog for networks that mutation left broken (see the watchdog section in exoself.erl). Two cases:
%%   stalled: no bar completed for stall_timeout_ms (neurons in a circle waiting for each other, a crashed process).
%%            The cortex reports to its exoself every progress_every_bars completed bars, so a large, slow network keeps
%%            reporting and is never failed for being slow.
%%   runaway: a neuron that fires forever floods another process's mailbox (about 100 MB per second). The agent is failed
%%            as soon as any of its processes has more than runaway_queue_limit messages waiting.
%% Not applied in live trading, where bars arrive in real time.
stall_timeout_ms() -> get_val(stall_timeout_ms, 120000).                         % Options: any positive integer in milliseconds (default: 120000 = 2 minutes of silence)
progress_every_bars() -> get_val(progress_every_bars, 50).                       % Options: 0 (cortex sends no reports; every evaluation must then finish within stall_timeout_ms) | N bars between reports
runaway_queue_limit() -> get_val(runaway_queue_limit, 5000).                     % Options: any positive integer (messages waiting in one process). A healthy process never has more waiting than it has inputs, so raise this only for neurons with thousands of input links
specie_size_limit() -> get_val(specie_size_limit, 2).                     % Options: 1-100 (max agents per species)
init_specie_size() -> get_val(init_specie_size, 2).                      % Options: 1-50 (initial agents per species)
evaluations_limit() -> get_val(evaluations_limit, 100000000000).                    % Options: 10-100000 (max evaluations per run)
generation_limit() -> get_val(generation_limit, 1).                     % Options: 1-1000 (max generations per run)
survival_percentage() -> get_val(survival_percentage, 0.5).                  % Options: 0.1-0.9 (percentage of agents that survive)
tot_runs() -> get_val(tot_runs, 1).                               % Options: 1-100 (number of benchmark runs)

%% --- Termination Logic Reference ---
%% Generation = evaluations_limit() / (agents_per_generation * tuning_duration())
%% Example: 100000 evaluations / (100 agents * 10 attempts) = 100 generations

population_evo_alg_f() -> get_val(population_evo_alg_f, generational).        % Options: generational, steady_state
population_selection_f() -> get_val(population_selection_f, competition).       % Options: competition, top3, rank (rank = offspring by fitness rank only; required when fitness can be negative, e.g. alpha_lcb)
population_fitness_postprocessor_f() -> get_val(population_fitness_postprocessor_f, none). % Options: none, size_proportional
tot_topological_mutations_functions() -> get_val(tot_topological_mutations_functions, [{ncount_exponential,0.5}]). % Options: {ncount_exponential,0.5}, {ncount_linear,1}. Mutations per offspring = rand(1..round(N^0.5)). Large counts (old default ncount_linear 7 = 7 per neuron) scramble offspring so larger nets cannot inherit what worked.

%% ===================================================================
%% Substrate-Specific Configuration
%% ===================================================================
substrate_plasticities() -> get_val(substrate_plasticities, [none, modular_none, hebbian, ojas]).            %Options: none, iterative, abcn, modular_none, hebbian, ojas (iterative - high CPU / Memory usage)
substrate_linkforms() -> get_val(substrate_linkforms, [l2l_feedforward, jordan_recurrent, neuronself_recurrent]).    % Options: l2l_feedforward, fully_interconnected, jordan_recurrent, neuronself_recurrent (l2l_feedforward - best general-purpose choice, fast and stable)
substrate_density() -> get_val(substrate_density, 5).                  % Options: 1-1000 (neurode resolution per dimension, typical: 10, 20, 50, 100)
substrate_depth() -> get_val(substrate_depth, 2).                    % Options: 1-100 (number of hidden layers in substrate, typical: 1, 5, 10, 20)

%% ===================================================================
%% NEAT Neuron Configuration (Applies to neurons if fully_interconnected is used in both Neural and Substrate encoding)
%% ===================================================================

morphology() -> get_val(morphology, forex_trader).                  % Options: forex_trader, forex_trader_1m
connection_architecture() -> get_val(connection_architecture, recurrent).        % Options: feedforward, recurrent
agent_encoding_types() -> get_val(agent_encoding_types, [substrate]).         % Options: neural, substrate
tuning_duration() -> get_val(tuning_duration, {const,10}).               % Options: {const,N}, {wsize_proportional,N}, {nsize_proportional,N}
tuning_selection_functions() -> get_val(tuning_selection_functions, [dynamic, dynamic_random, active, active_random, current, current_random, all, all_random]). % Note: lastgen/lastgen_random not implemented
annealing_parameters() -> get_val(annealing_parameters, [0.1, 0.5, 1]).                 % Options: [0.1], [0.5], [1], [0.5,1], [0.1,0.5,1]
perturbation_ranges() -> get_val(perturbation_ranges, [0.1, 0.5, 1, 2]).                  % Options: [0.1], [0.5], [1], [2], [0.5,1], [1,2]
heredity_types() -> get_val(heredity_types, [darwinian, lamarckian]).               % Options: darwinian, lamarckian

neural_activation_functions() -> get_val(neural_activation_functions, [tanh, sin, cos, gaussian, absolute, sgn, log, sqrt, linear]). % Note: quadratic, sigmoid, sigmoid1, multiquadric, bin, trinary are implemented but cause crashes (not tracked in AF_Distribution)
neural_plasticity_functions() -> get_val(neural_plasticity_functions, [none, hebbian, hebbian_w, ojas, ojas_w, self_modulationV1, self_modulationV2, self_modulationV3, self_modulationV4, self_modulationV5, self_modulationV6, neuromodulation]). 
neural_aggregation_functions() -> get_val(neural_aggregation_functions, [dot_product, mult_product, diff_product]). % Options: dot_product, mult_product, diff_product

%% --- Mutation Operators ---
%% Format: {OperatorName, Weight} - Weight determines relative probability of selection
%% Available: mutate_weights, mutate_af, mutate_pf, mutate_aggrf, mutate_plasticity_parameters,
%% add_bias, remove_bias, add_neuron, add_inlink, add_outlink, outsplice, add_sensor, add_actuator,
%% add_sensorlink, add_actuatorlink, add_cpp, add_cep,
%% remove_neuron, remove_inlink, remove_outlink, remove_sensorlink, remove_sensor
mutation_operators() -> get_val(mutation_operators, [
    %% Weight & Bias Adaptation
    {mutate_weights,25},
    {mutate_af,25},
    {mutate_pf,25},
    {mutate_aggrf,25},
    {mutate_plasticity_parameters,25}, 
    {add_bias,25},
    {remove_bias,10},

    %% Topology Growth
    {add_neuron,60},
    {add_inlink,50},
    {add_outlink,40},
    {outsplice,15},

    %% Sensor & Actuator Expansion
    {add_sensor,10},
    %% add_actuator omitted: forex morphologies have a single actuator, so it always fails
    {add_sensorlink,30},
    {add_actuatorlink,20},

    %% Topology Removal (neural encoding only). About a third of the matching add weights, so networks still
    %% grow while parts that do nothing can drop out. Each one refuses a removal that would break the network.
    {remove_neuron,20},
    {remove_inlink,15},
    {remove_outlink,15},
    {remove_sensorlink,10},
    {remove_sensor,4},

    %% CPP / CEP Extensions (for HyperNEAT substrate encoding)
    {add_cpp,30},
    {add_cep,30}
]).

%% ===================================================================
%% Fitness Function Selection
%% ===================================================================
fitness_function() -> get_val(fitness_function, total_profit).                % total_profit = final equity (balance + open P&L, spread paid on entry). Episode mode adds alpha_t, alpha_money_t, alpha_pips_t, alpha_lcb and alpha_mean (timing skill: a constant position scores zero; see fitness_functions:aggregate_episodes/2); any other name is averaged over the episodes. Options: alpha_t, alpha_money_t, alpha_pips_t, alpha_lcb, alpha_mean, time_weighted, total_profit, sharpe_ratio, profit_factor, total_return, sortino_ratio, calmar_ratio, curriculum_risk_penalty, phase0_close_trades, phase1_profit_risk, curriculum_trade_quality_profit, phase2_profit_optimization

%% ===================================================================
%% Time-Weighted Fitness Configuration
%% ===================================================================
fitness_discount_rate() -> get_val(fitness_discount_rate, 0.000025).          % Discount rate per cycle for profits (0.000025 = 0.0025%)
fitness_loss_discount_rate() -> get_val(fitness_loss_discount_rate, 0.000025).     % Discount rate per cycle for losses (additive, 0.000025 = 0.0025%)
fitness_realized_bonus() -> get_val(fitness_realized_bonus, 1.50).             % Bonus multiplier for realized profits (1.50 = 50% bonus, encourages closing profitable trades)
fitness_loss_penalty() -> get_val(fitness_loss_penalty, 1.3).                % Penalty multiplier for realized losses (1.3 = 30% penalty, reduced to encourage closing losing trades)
fitness_unrealized_penalty() -> get_val(fitness_unrealized_penalty, 0.4).         % Penalty multiplier for unrealized PL (0.4 = 40% of value, strongly encourages realization)
fitness_trades_bonus() -> get_val(fitness_trades_bonus, 100).                 % Flat bonus added to fitness if any trades were made (encourages trading activity)
fitness_time_weighted_enabled() -> get_val(fitness_time_weighted_enabled, true).      % Enable/disable time-weighted fitness (true/false)

%% ===================================================================
%% Curriculum Risk Penalty Fitness Configuration
%% ===================================================================
fitness_curriculum_generation() -> get_val(fitness_curriculum_generation, 0).         % Current generation index (for curriculum learning, default: 0)
fitness_curriculum_unrealized_discount() -> get_val(fitness_curriculum_unrealized_discount, 0.3).  % k: Unrealized PnL discount factor (default: 0.3)
fitness_curriculum_pnl_scale() -> get_val(fitness_curriculum_pnl_scale, 100.0).       % S_P: P&L scale per 1000 steps (default: 100.0)
fitness_curriculum_trades_per_1000() -> get_val(fitness_curriculum_trades_per_1000, 3.0).  % Desired trades per 1000 steps (default: 20.0)
fitness_curriculum_generation_focus() -> get_val(fitness_curriculum_generation_focus, 50).  % G_trade_focus: Generation where profit dominates (default: 50)
fitness_curriculum_drawdown_floor() -> get_val(fitness_curriculum_drawdown_floor, 0.10).    % DD_floor: Drawdown tolerated (default: 0.10 = 10%)
fitness_curriculum_drawdown_penalty() -> get_val(fitness_curriculum_drawdown_penalty, 3.0). % lam: Drawdown penalty strength (default: 3.0)

%% ===================================================================
%% Phase 1 Profit Risk Fitness Configuration
%% ===================================================================
fitness_phase1_pscore_weight() -> get_val(fitness_phase1_pscore_weight, 0.40).      % Weight for PnL score in phase1_profit_risk (default: 0.40)
fitness_phase1_tradescore_weight() -> get_val(fitness_phase1_tradescore_weight, 0.60).  % Weight for trade score in phase1_profit_risk (default: 0.60)

%% ===================================================================
%% Curriculum Trade Quality Profit Fitness Configuration
%% ===================================================================
fitness_target_trades_per_1000() -> get_val(fitness_target_trades_per_1000, 50.0).  % Target trades per 1000 timesteps (default: 50.0)
fitness_overtrade_thresh_per_1000() -> get_val(fitness_overtrade_thresh_per_1000, 150.0).  % Overtrade threshold per 1000 timesteps (default: 150.0)
fitness_overtrade_lambda() -> get_val(fitness_overtrade_lambda, 0.08).  % Overtrade penalty decay rate (default: 0.08)
fitness_curriculum_g1() -> get_val(fitness_curriculum_g1, 15).  % Generation milestone 1 for curriculum schedule (default: 15)
fitness_curriculum_g2() -> get_val(fitness_curriculum_g2, 60).  % Generation milestone 2 for curriculum schedule (default: 60)
fitness_no_trade_penalty() -> get_val(fitness_no_trade_penalty, 0.5).  % Penalty for no trades in trade score (default: 0.5)
fitness_dom_scale() -> get_val(fitness_dom_scale, 10.0).  % Scale for dominance score calculation (default: 10.0)
fitness_bigwin_pct() -> get_val(fitness_bigwin_pct, 0.005).  % Big win threshold as percentage of starting balance (default: 0.005 = 0.5%)
fitness_bigwin_sum_scale() -> get_val(fitness_bigwin_sum_scale, 1.0).  % Scale for big win sum normalization (default: 1.0)
fitness_target_bigwins_per_1000() -> get_val(fitness_target_bigwins_per_1000, 5.0).  % Target big wins per 1000 timesteps (default: 5.0)
fitness_unreal_discount_no_trades() -> get_val(fitness_unreal_discount_no_trades, 0.1).  % Unrealized discount when no trades (default: 0.1)
fitness_dd_lambda_early() -> get_val(fitness_dd_lambda_early, 1.0).  % Early generation drawdown penalty strength (default: 1.0)
fitness_dd_lambda_late() -> get_val(fitness_dd_lambda_late, 4.0).  % Late generation drawdown penalty strength (default: 4.0)
%% Note: fitness_curriculum_drawdown_floor, fitness_curriculum_pnl_scale, and fitness_curriculum_unrealized_discount
%% are already defined in the "Curriculum Risk Penalty Fitness Configuration" section above

%% ===================================================================
%% Checkpoint Configuration
%% ===================================================================
checkpoint_enabled() -> get_val(checkpoint_enabled, auto).              % Options: auto, true, false (auto = detect AWS environment)

%% ===================================================================
%% Population Configuration
%% ===================================================================
population_id() -> get_val(population_id, test).                        % Options: test, or any atom/string identifier for the population

%% ===================================================================
%% System & Diagnostics
%% ===================================================================
fx_tables_directory() -> get_val(fx_tables_directory, "fx_tables/").         % Options: any valid directory path
source_directory() -> get_val(source_directory, "fx_tables/").            % Options: any valid directory path
actuator_debug_tag() -> get_val(actuator_debug_tag, false).                 % Options: true, false (enables trade-by-trade debug output)
sensor_debug_tag() -> get_val(sensor_debug_tag, false).                   % Options: true, false (enables sensor debug output)

%% ===================================================================
%% Dynamic Configuration Override System (ETS-based)
%% ===================================================================
init() ->
	case ets:info(?CONFIG_TAB) of
		undefined -> ets:new(?CONFIG_TAB, [set, public, named_table, {read_concurrency,true}]), ok;
		_ -> ok
	end.

set(Key, Value) ->
	init(),
	ets:insert(?CONFIG_TAB, {Key, Value}).

%Every agent reads about thirty settings per bar, so this is one lookup and nothing else. It used to ask ets:info/1
%first whether the table exists, which builds a fifteen-item list on every call and doubled the traffic on the table's
%lock. A missing table still means "use the default" (REVIEW.md 8.20).
get_val(Key, Default) ->
	try ets:lookup(?CONFIG_TAB, Key) of
		[{Key, Value}] -> Value;
		[] -> Default
	catch
		error:badarg -> Default
	end.

clear() ->
	case ets:info(?CONFIG_TAB) of
		undefined -> ok;
		_ -> ets:delete_all_objects(?CONFIG_TAB)
	end.

load_from_list(ConfigList) ->
	init(),
	[set(Key, Value) || {Key, Value} <- ConfigList],
	ok.
