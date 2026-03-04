-module(exp_runner).
-compile(export_all).
-include("records.hrl").

%% ============================================================================
%% EXP RUNNER - Orchestrates reproducible evolutionary experiments
%% ============================================================================

%% API
-export([
    start/1,              % start(fresh) | start(new_evo) | start({evo, PopId})
    continue/1,            % continue(ExperimentId)
    get_run_configs/0,    % get_run_configs() - returns default run configs
    prep/3,              % prep(E, Mode, SourcePopId) - internal
    loop/2                % loop(E, P_Id) - internal
]).

%% Standard forex trader morphology (uses EURUSD1 with existing sensor configuration)
%% Fixed: Use function call instead of macro for dynamic config
get_init_constraints() ->
    [#constraint{morphology=Morphology,connection_architecture=CA, population_evo_alg_f=config:population_evo_alg_f(), neural_pfns=config:neural_plasticity_functions(),agent_encoding_types=config:agent_encoding_types()} || Morphology<-[config:morphology()],CA<-[config:connection_architecture()]].

%% ============================================================================
%% POPULATION ID GENERATION
%% ============================================================================

%% Generate population ID for fresh run (new lineage)
generate_population_id(RunIndex) ->
    LineageId = generate_lineage_id(),
    generate_population_id_with_lineage(RunIndex, LineageId).

%% Generate population ID for cloned run (reuse lineage from source)
%% Only match if SourcePopId looks like a full population ID (longer than 4 chars, has underscores)
generate_population_id(RunIndex, SourcePopId) when is_binary(SourcePopId), byte_size(SourcePopId) > 20 ->
    LineageId = extract_lineage_id(SourcePopId),
    generate_population_id_with_lineage(RunIndex, LineageId).

%% Internal helper with lineage ID (4-char binary)
generate_population_id_with_lineage(RunIndex, LineageId) when is_binary(LineageId), byte_size(LineageId) =:= 4 ->
    % Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
    % Example: <<"2025-02-11T15-30-45Z_a3f9_run1">>
    Timestamp = format_iso8601(erlang:timestamp()),
    RunIndexStr = integer_to_list(RunIndex),
    <<Timestamp/binary, "_", LineageId/binary, "_run", (list_to_binary(RunIndexStr))/binary>>.

%% Generate 4 random alphanumeric characters
generate_lineage_id() ->
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
    RandomChars = [lists:nth(rand:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 4)],
    list_to_binary(RandomChars).

%% Extract lineage ID from source population ID
%% Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
extract_lineage_id(SourcePopId) ->
    case binary:split(SourcePopId, <<"_">>, [global]) of
        [_Timestamp, LineageId | _] when byte_size(LineageId) =:= 4 ->
            LineageId;
        _ ->
            % Fallback: generate new if parsing fails
            generate_lineage_id()
    end.

%% Format timestamp as ISO8601
format_iso8601({MegaSecs, Secs, _MicroSecs}) ->
    % Convert to ISO8601: 2025-02-11T15:30:45Z
    % erlang:timestamp() returns Unix epoch (seconds since 1970-01-01)
    % Need to convert to Gregorian seconds first
    UnixEpoch = MegaSecs * 1000000 + Secs,
    GregorianEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    TotalSecs = GregorianEpoch + UnixEpoch,
    {{Y,Mo,D},{H,Mi,S}} = calendar:gregorian_seconds_to_datetime(TotalSecs),
    list_to_binary(lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
        [Y,Mo,D,H,Mi,S]))).

%% ============================================================================
%% CONFIG SUMMARY FORMATTING
%% ============================================================================

format_config_summary(RunIndex, RunConfigs) ->
    % Get config for this run (if specified) or use defaults
    ConfigOverrides = case lists:keyfind(RunIndex, 1, RunConfigs) of
        {RunIndex, ConfigList} -> ConfigList;
        false -> []
    end,
    
    % Extract key values from config (overrides + defaults)
    % Use config module functions which have defaults, not direct get_val
    Morph = get_config_value(morphology, ConfigOverrides, fun config:morphology/0),
    Enc = get_config_value(agent_encoding_types, ConfigOverrides, fun config:agent_encoding_types/0),
    Arch = get_config_value(connection_architecture, ConfigOverrides, fun config:connection_architecture/0),
    GtStart = get_config_value(gt_start, ConfigOverrides, fun config:gt_start/0),
    GtEnd = get_config_value(gt_end, ConfigOverrides, fun config:gt_end/0),
    BenchStart = get_config_value(bench_start, ConfigOverrides, fun config:bench_start/0),
    BenchEnd = get_config_value(bench_end, ConfigOverrides, fun config:bench_end/0),
    EvoAlg = get_config_value(population_evo_alg_f, ConfigOverrides, fun config:population_evo_alg_f/0),
    Selection = get_config_value(population_selection_f, ConfigOverrides, fun config:population_selection_f/0),
    FPost = get_config_value(population_fitness_postprocessor_f, ConfigOverrides, fun config:population_fitness_postprocessor_f/0),
    Survival = get_config_value(survival_percentage, ConfigOverrides, fun config:survival_percentage/0),
    SpecieSizeLimit = get_config_value(specie_size_limit, ConfigOverrides, fun config:specie_size_limit/0),
    InitSpecieSize = get_config_value(init_specie_size, ConfigOverrides, fun config:init_specie_size/0),
    TuningDur = get_config_value(tuning_duration, ConfigOverrides, fun() -> {const,1} end),
    
    % Format compactly with line breaks for readability
    TuningDurStr = case TuningDur of
        {const, N} -> io_lib:format("const(~p)", [N]);
        Other -> io_lib:format("~p", [Other])
    end,
    lists:flatten(io_lib:format(
        "morph=~w enc=~w arch=~w | gt=~p-~p bench=~p-~p | "
        "evo=~w sel=~w fpost=~w | survival=~p | "
        "specie_limit=~p init_size=~p tuning=~s",
        [Morph, Enc, Arch, GtStart, GtEnd, BenchStart, BenchEnd,
         EvoAlg, Selection, FPost, Survival,
         SpecieSizeLimit, InitSpecieSize, TuningDurStr]
    )).

get_config_value(Key, ConfigOverrides, DefaultFun) ->
    case lists:keyfind(Key, 1, ConfigOverrides) of
        {Key, Value} -> Value;
        false -> 
            % Try to get from loaded config first, then use default function
            case config:get_val(Key, undefined) of
                undefined -> DefaultFun();
                Value -> Value
            end
    end.

%% ============================================================================
%% POPULATION REUSE (Simpler than cloning - reuses existing agents)
%% ============================================================================

%% Reuse population for next run: reset stats and assign new population ID
%% This is much simpler and more efficient than cloning - just updates metadata
reuse_population_for_next_run(SourcePopId, NewPopId) ->
    %qlog:benchmarker(NewPopId, io_lib:format("REUSE_POPULATION_START | from ~p to ~p", [SourcePopId, NewPopId])),
    F = fun() ->
        SourcePop = case mnesia:read({population, SourcePopId}) of
            [] -> error({population_not_found, SourcePopId});
            [P] -> P
        end,
        
        % Create new population record with reset trace
        NewPop = SourcePop#population{
            id = NewPopId,
            specie_ids = [],
            trace = #trace{}
        },
        mnesia:write(NewPop),
        
        % Reuse species - create new specie records pointing to same agents
        NewSpecieIds = [
            reuse_specie_for_next_run_in_tx(SpecieId, NewPopId)
            || SpecieId <- SourcePop#population.specie_ids
        ],
        
        % Update population with new specie IDs
        mnesia:write(NewPop#population{specie_ids = NewSpecieIds})
    end,
    mnesia:transaction(F).

%% Reuse specie for next run: create new specie record, reuse existing agents
%% Called from within transaction - uses mnesia directly
reuse_specie_for_next_run_in_tx(SourceSpecieId, NewPopId) ->
    SourceSpecie = case mnesia:read({specie, SourceSpecieId}) of
        [] -> error({specie_not_found, SourceSpecieId});
        [S] -> S
    end,
    NewSpecieId = genotype:generate_UniqueId(),
    %qlog:benchmarker(NewPopId, io_lib:format("REUSE_SPECIE | specie=~p | claimed_agents=~p", [SourceSpecieId, length(SourceSpecie#specie.agent_ids)])),
    % Reset all agents in place (reuse same agent IDs)
    [reset_agent_for_next_run_in_tx(AgentId, NewSpecieId, NewPopId)
     || AgentId <- SourceSpecie#specie.agent_ids],
    
    % Create new specie record pointing to existing agents
    NewSpecie = SourceSpecie#specie{
        id = NewSpecieId,
        population_id = NewPopId,
        agent_ids = SourceSpecie#specie.agent_ids,
        dead_pool = [],
        champion_ids = [],
        fitness = undefined,
        innovation_factor = {0, 0},
        stats = []
    },
    mnesia:write(NewSpecie),
    NewSpecieId.

%% Reset agent for next run: update runtime fields, preserve neural structure
%% Called from within transaction - uses mnesia directly
reset_agent_for_next_run_in_tx(AgentId, NewSpecieId, NewPopId) ->
    Agent = case mnesia:read({agent, AgentId}) of
        [] -> undefined;
        [A] -> A
    end,
    case Agent of
        undefined ->
            %qlog:benchmarker(NewPopId, io_lib:format("REUSE_AGENT_NOT_FOUND | agent=~p | specie=~p", [AgentId, NewSpecieId])),
            error({agent_not_found_for_reset, AgentId, NewSpecieId, NewPopId});
        _ ->
            ResetAgent = Agent#agent{
                population_id = NewPopId,
                specie_id = NewSpecieId,
                generation = 0,
                fitness = 0,
                evo_hist = [],
                innovation_factor = 0
            },
            mnesia:write(ResetAgent)
    end.

%% ============================================================================
%% POPULATION CLONING (Creates new agent IDs for each run - preserves artifacts)
%% ============================================================================

%% Clone population for next run: creates new agent IDs, preserves all artifacts
%% This preserves logs, genotypes, and other artifacts by giving each run unique agent IDs
clone_population_for_next_run(SourcePopId, NewPopId) ->
    qlog:benchmarker(NewPopId, io_lib:format("CLONE_POPULATION_START | from ~p to ~p", [SourcePopId, NewPopId])),
    
    % First, read source population to get all agent IDs (outside transaction)
    SourcePop = case genotype:dirty_read({population, SourcePopId}) of
        undefined -> error({population_not_found, SourcePopId});
        P -> P
    end,
    
    % Clone all agents outside transaction (clone_Agent has its own transaction)
    % Build map: SourceSpecieId -> [ClonedAgentIds]
    SpecieClonesMap = lists:foldl(fun(SourceSpecieId, Acc) ->
        SourceSpecie = genotype:dirty_read({specie, SourceSpecieId}),
        case SourceSpecie of
            undefined -> Acc;
            _ ->
                % Clone all agents in this specie
                ClonedAgentIds = [
                    begin
                        CloneAgentId = genotype:clone_Agent(SourceAgentId),
                        qlog:benchmarker(NewPopId, io_lib:format("CLONED_AGENT | source=~p | clone=~p | specie=~p", [SourceAgentId, CloneAgentId, SourceSpecieId])),
                        CloneAgentId
                    end
                    || SourceAgentId <- SourceSpecie#specie.agent_ids
                ],
                maps:put(SourceSpecieId, ClonedAgentIds, Acc)
        end
    end, maps:new(), SourcePop#population.specie_ids),
    
    % Now update all records in transaction
    F = fun() ->
        % Create new population record with reset trace
        NewPop = SourcePop#population{
            id = NewPopId,
            specie_ids = [],
            trace = #trace{}
        },
        mnesia:write(NewPop),
        
        % Clone species - create new specie records with cloned agents
        NewSpecieIds = lists:map(fun(SpecieId) ->
            ClonedAgentIds = maps:get(SpecieId, SpecieClonesMap),
            clone_specie_for_next_run_in_tx(SpecieId, NewPopId, ClonedAgentIds)
        end, SourcePop#population.specie_ids),
        
        % Update population with new specie IDs
        mnesia:write(NewPop#population{specie_ids = NewSpecieIds})
    end,
    mnesia:transaction(F).

%% Clone specie for next run: create new specie record, clone all agents
%% Note: Clones agents outside transaction (clone_Agent has its own transaction),
%%       then updates runtime fields inside transaction
clone_specie_for_next_run_in_tx(SourceSpecieId, NewPopId, ClonedAgentIds) ->
    SourceSpecie = case mnesia:read({specie, SourceSpecieId}) of
        [] -> error({specie_not_found, SourceSpecieId});
        [S] -> S
    end,
    NewSpecieId = genotype:generate_UniqueId(),
    
    % Update runtime fields for all cloned agents
    [update_cloned_agent_runtime_fields_in_tx(CloneAgentId, NewSpecieId, NewPopId)
     || CloneAgentId <- ClonedAgentIds],
    
    % Create new specie record pointing to cloned agents
    NewSpecie = SourceSpecie#specie{
        id = NewSpecieId,
        population_id = NewPopId,
        agent_ids = ClonedAgentIds,  % New agent IDs from clones
        dead_pool = [],
        champion_ids = [],
        fitness = undefined,
        innovation_factor = {0, 0},
        stats = []
    },
    mnesia:write(NewSpecie),
    NewSpecieId.

%% Update cloned agent's runtime fields for next run
%% Called from within transaction - uses mnesia directly
update_cloned_agent_runtime_fields_in_tx(CloneAgentId, NewSpecieId, NewPopId) ->
    ClonedAgent = case mnesia:read({agent, CloneAgentId}) of
        [] -> error({clone_agent_not_found, CloneAgentId, NewSpecieId, NewPopId});
        [A] -> A
    end,
    
    % Reset runtime fields for the new run
    ResetClonedAgent = ClonedAgent#agent{
        population_id = NewPopId,
        specie_id = NewSpecieId,
        generation = 0,          % Reset generation
        fitness = 0,             % Reset fitness
        evo_hist = [],           % Reset evolution history
        innovation_factor = 0   % Reset innovation factor
    },
    mnesia:write(ResetClonedAgent).

%% ============================================================================
%% START FUNCTIONS
%% ============================================================================

start(fresh) ->
    RunConfigs = get_run_configs(),
    start_internal(fresh, RunConfigs);
start(new_evo) ->
    RunConfigs = get_run_configs(),
    start_internal(new_evo, RunConfigs);
start({evo, SourcePopId}) ->
    RunConfigs = get_run_configs(),
    start_internal({evo, SourcePopId}, RunConfigs).

%% Internal start function (similar to benchmarker:start)
start_internal(Mode, RunConfigs) ->
    % 1. Initialize config
    config:init(),
    
    % 2. Generate population ID first (auto-generated, includes lineage and run_index)
    % The first population ID becomes the experiment ID
    {PopId, SourcePopId} = case Mode of
        fresh ->
            PopId1 = generate_population_id(1),
            {PopId1, undefined};  % New lineage ID
        new_evo ->
            PopId2 = generate_population_id(1),
            {PopId2, undefined};  % Will be updated for subsequent runs
        {evo, SrcPopId} ->
            PopId3 = generate_population_id(1, SrcPopId),
            {PopId3, SrcPopId}  % Reuse lineage from source
    end,
    ExperimentId = PopId,  % Use first population ID as experiment ID
    
    % 3. Apply run configs
    apply_run_configs(ExperimentId, 1, RunConfigs),
    
    % 4. Create PMP (Population Monitor Parameters) - like benchmarker
    PMP = #pmp{
        op_mode = benchmark,
        population_id = PopId,
        survival_percentage = config:survival_percentage(),
        specie_size_limit = config:specie_size_limit(),
        init_specie_size = config:init_specie_size(),
        polis_id = mathema,
        generation_limit = config:generation_limit(),
        evaluations_limit = config:evaluations_limit(),
        fitness_goal = inf,
        benchmarker_pid = self()  % Will be updated in prep
    },
    
    % 5. Calculate total runs
    TotRuns = case RunConfigs of
        [] -> config:tot_runs();
        _ -> lists:max([RunIndex || {RunIndex, _} <- RunConfigs])
    end,
    
    % 6. Create experiment record
    InitConstraints = get_init_constraints(),
    E = #experiment{
        id = ExperimentId,
        backup_flag = true,
        pm_parameters = PMP,
        init_constraints = InitConstraints,
        progress_flag = in_progress,
        run_index = 1,
        tot_runs = TotRuns,
        run_configs = RunConfigs,
        started = {date(), time()},
        interruptions = []
    },
    
    % 7. Log experiment start
    qlog:exp_runner(experiment_start, {ExperimentId, PopId, TotRuns, length(RunConfigs)}),
    qlog:xLog(qStatus, "exp_runner:start | experiment_id=~p | tot_runs=~p | mode=~p", [ExperimentId, TotRuns, Mode]),
    
    % 8. Store experiment record
    genotype:write(E),
    
    % 9. Spawn prep process (like benchmarker) and return immediately
    ExpRunnerPid = spawn(exp_runner, prep, [E, Mode, SourcePopId]),
    {ok, ExpRunnerPid}.

%% Apply run configs for a specific run index
apply_run_configs(ExperimentId, RunIndex, RunConfigs) ->
    case RunConfigs of
        [] -> ok;
        _ ->
            case lists:keyfind(RunIndex, 1, RunConfigs) of
                {RunIndex, ConfigList} when is_list(ConfigList) ->
                    config:clear(),
                    config:load_from_list(ConfigList);
                false -> ok
            end
    end.

%% Generate a unique run ID (simple sequential or random ID, no timestamp)
generate_run_id() ->
    % Use a simple random ID or sequential counter
    % For now, use a random 8-character alphanumeric ID
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
    RandomChars = [lists:nth(rand:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 8)],
    list_to_atom("run_" ++ RandomChars).

%% ============================================================================
%% RUN CONFIG HELPERS
%% ============================================================================

%% Get run configs - customize this function with your experiment configs
%% Usage: exp_runner:start(fresh) - automatically uses these configs
%%
%% CURRICULUM LEARNING PROGRESSION:
%% Phase -1 (Runs 1-5):   Size Reward - Focus: Reward larger neural networks to encourage growth
%% Phase 0 (Runs 6-10):   Learn to trade - Focus: Close trades without blowing up
%% Phase 1 (Runs 11-15):  Make positive trades - Profit optimization with drawdown control
%% Phase 2 (Runs 16-25):  Win rate focus - More positive than negative trades (curriculum early)
%% Phase 3 (Runs 26-35):  Big wins focus - Large positive trades (curriculum mid)
%% Phase 4 (Runs 36-45):  Profit optimization - Maximum P/L with strong risk control
%%
get_run_configs() ->
    [
        %% Phase -1: Size Reward (Runs 1-5) - Focus: Pure size focus in run 1, gradually transitioning
        %% Run 1: fitness_size_focus_weight=0.0 means constant fitness (1.0), size_first postprocessor
        %%        sorts primarily by neuron count (larger networks win regardless of trading performance)
        %% Runs 2-5: Gradually increase focus_weight so trading performance becomes important alongside size
        %{1, [{fitness_function, phase_size_reward}, {population_fitness_postprocessor_f, size_first}, {fitness_size_focus_weight, 0.0}, {tuning_duration, {const,4}}, {gt_start, 1000}, {gt_end, 500}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 5}, {survival_percentage, 1.0}]},
        %{2, [{fitness_function, phase_size_reward}, {population_fitness_postprocessor_f, size_first}, {fitness_size_focus_weight, 0.2}, {tuning_duration, {const,4}}, {gt_start, 2000}, {gt_end, 1000}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 5}, {survival_percentage, 0.9}]},
        %{3, [{fitness_function, phase_size_reward}, {population_fitness_postprocessor_f, size_first}, {fitness_size_focus_weight, 0.4}, {tuning_duration, {const,4}}, {gt_start, 3000}, {gt_end, 1500}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 5}, {survival_percentage, 0.8}]},
        %{4, [{fitness_function, phase_size_reward}, {population_fitness_postprocessor_f, size_first}, {fitness_size_focus_weight, 0.6}, {tuning_duration, {const,4}}, {gt_start, 4000}, {gt_end, 2000}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 5}, {survival_percentage, 0.7}]},
        %{5, [{fitness_function, phase_size_reward}, {population_fitness_postprocessor_f, size_first}, {fitness_size_focus_weight, 0.8}, {tuning_duration, {const,4}}, {gt_start, 5000}, {gt_end, 2500}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 5}, {survival_percentage, 0.6}]},
        
        %% Phase 0: Learn to Trade (Runs 6-10) - Focus: Close trades without blowing up
        %% Runs 1-3: Use size_first postprocessor to encourage network growth
        {1, [{fitness_function, phase0_close_trades}, {population_fitness_postprocessor_f, size_first}, {tuning_duration, {const,3}}, {gt_start, 2000}, {gt_end, 1500}, {specie_size_limit, 10}, {init_specie_size, 10}, {generation_limit, 5}, {survival_percentage, 1.0}]},
        {2, [{fitness_function, phase0_close_trades}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,3}}, {gt_start, 3000}, {gt_end, 2000}, {specie_size_limit, 10}, {init_specie_size, 10}, {generation_limit, 10}, {survival_percentage, 0.8}]},
        {3, [{fitness_function, phase0_close_trades}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,3}}, {gt_start, 4000}, {gt_end, 2500}, {specie_size_limit, 10}, {init_specie_size, 10}, {generation_limit, 10}, {survival_percentage, 0.7}]},
        {4, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.30}, {fitness_phase1_tradescore_weight, 0.70}, {tuning_duration, {const,10}}, {gt_start, 4000}, {gt_end, 2500}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        {5, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.35}, {fitness_phase1_tradescore_weight, 0.65}, {tuning_duration, {const,10}}, {gt_start, 5000}, {gt_end, 3000}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        
        %% Phase 1: Make Positive Trades (Runs 11-15) - Focus: Profit optimization with drawdown control
        {6, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.40}, {fitness_phase1_tradescore_weight, 0.70}, {tuning_duration, {const,10}}, {gt_start, 4000}, {gt_end, 2500}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        {7, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.45}, {fitness_phase1_tradescore_weight, 0.65}, {tuning_duration, {const,10}}, {gt_start, 5000}, {gt_end, 3000}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        {8, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.50}, {fitness_phase1_tradescore_weight, 0.60}, {tuning_duration, {const,10}}, {gt_start, 6000}, {gt_end, 3500}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        {9, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.55}, {fitness_phase1_tradescore_weight, 0.55}, {tuning_duration, {const,10}}, {gt_start, 7000}, {gt_end, 4000}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        {10, [{fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.60}, {fitness_phase1_tradescore_weight, 0.50}, {tuning_duration, {const,10}}, {gt_start, 8000}, {gt_end, 4500}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 50}]},
        
        %% Phase 2: Win Rate Focus (Runs 16-25) - Focus: More positive than negative trades
        {11, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 0}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 6000}, {gt_end, 3500}, {specie_size_limit, 80}, {init_specie_size, 80}, {generation_limit, 75}]},
        {12, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 5}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 7000}, {gt_end, 4000}, {specie_size_limit, 90}, {init_specie_size, 90}, {generation_limit, 75}]},
        {13, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 10}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 8000}, {gt_end, 4500}, {specie_size_limit, 100}, {init_specie_size, 100}, {generation_limit, 75}]},
        {14, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 15}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 9000}, {gt_end, 5000}, {specie_size_limit, 120}, {init_specie_size, 120}, {generation_limit, 75}]},
        {15, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 20}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 10000}, {gt_end, 5500}, {specie_size_limit, 140}, {init_specie_size, 140}, {generation_limit, 75}]},
        {16, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 25}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 11000}, {gt_end, 6000}, {specie_size_limit, 160}, {init_specie_size, 160}, {generation_limit, 75}]},
        {17, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 30}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 12000}, {gt_end, 6500}, {specie_size_limit, 200}, {init_specie_size, 200}, {generation_limit, 75}]},
        {18, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 35}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 13000}, {gt_end, 7000}, {specie_size_limit, 200}, {init_specie_size, 200}, {generation_limit, 75}]},
        {19, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 40}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 14000}, {gt_end, 7500}, {specie_size_limit, 200}, {init_specie_size, 200}, {generation_limit, 75}]},
        {20, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 45}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_dd_lambda_early, 1.0}, {fitness_dd_lambda_late, 3.0}, {tuning_duration, {const,10}}, {gt_start, 15000}, {gt_end, 8000}, {specie_size_limit, 200}, {init_specie_size, 200}, {generation_limit, 75}]},
        
        %% Phase 3: Big Wins Focus (Runs 26-35) - Focus: Large positive trades
        {21, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 50}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.005}, {fitness_target_bigwins_per_1000, 5.0}, {fitness_bigwin_sum_scale, 1.0}, {fitness_dd_lambda_early, 1.5}, {fitness_dd_lambda_late, 4.0}, {tuning_duration, {const,10}}, {gt_start, 10000}, {gt_end, 5500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {22, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 55}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.005}, {fitness_target_bigwins_per_1000, 5.0}, {fitness_bigwin_sum_scale, 1.2}, {fitness_dd_lambda_early, 1.5}, {fitness_dd_lambda_late, 4.0}, {tuning_duration, {const,10}}, {gt_start, 11000}, {gt_end, 6000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {23, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 60}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.007}, {fitness_target_bigwins_per_1000, 5.0}, {fitness_bigwin_sum_scale, 1.2}, {fitness_dd_lambda_early, 1.5}, {fitness_dd_lambda_late, 4.0}, {tuning_duration, {const,10}}, {gt_start, 12000}, {gt_end, 6500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {24, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 65}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.007}, {fitness_target_bigwins_per_1000, 6.0}, {fitness_bigwin_sum_scale, 1.5}, {fitness_dd_lambda_early, 2.0}, {fitness_dd_lambda_late, 4.5}, {tuning_duration, {const,10}}, {gt_start, 13000}, {gt_end, 7000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {25, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 70}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.007}, {fitness_target_bigwins_per_1000, 6.0}, {fitness_bigwin_sum_scale, 1.5}, {fitness_dd_lambda_early, 2.0}, {fitness_dd_lambda_late, 4.5}, {tuning_duration, {const,10}}, {gt_start, 14000}, {gt_end, 7500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {26, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 75}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.01}, {fitness_target_bigwins_per_1000, 6.0}, {fitness_bigwin_sum_scale, 2.0}, {fitness_dd_lambda_early, 2.5}, {fitness_dd_lambda_late, 5.0}, {tuning_duration, {const,10}}, {gt_start, 15000}, {gt_end, 8000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {27, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 80}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.01}, {fitness_target_bigwins_per_1000, 7.0}, {fitness_bigwin_sum_scale, 2.0}, {fitness_dd_lambda_early, 2.5}, {fitness_dd_lambda_late, 5.0}, {tuning_duration, {const,10}}, {gt_start, 16000}, {gt_end, 8500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {28, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 85}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.01}, {fitness_target_bigwins_per_1000, 7.0}, {fitness_bigwin_sum_scale, 2.5}, {fitness_dd_lambda_early, 3.0}, {fitness_dd_lambda_late, 5.5}, {tuning_duration, {const,10}}, {gt_start, 17000}, {gt_end, 9000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {29, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 90}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.01}, {fitness_target_bigwins_per_1000, 8.0}, {fitness_bigwin_sum_scale, 2.5}, {fitness_dd_lambda_early, 3.0}, {fitness_dd_lambda_late, 5.5}, {tuning_duration, {const,10}}, {gt_start, 18000}, {gt_end, 9500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        {30, [{fitness_function, curriculum_trade_quality_profit}, {fitness_curriculum_generation, 95}, {fitness_curriculum_g1, 20}, {fitness_curriculum_g2, 80}, {fitness_target_trades_per_1000, 50.0}, {fitness_bigwin_pct, 0.01}, {fitness_target_bigwins_per_1000, 8.0}, {fitness_bigwin_sum_scale, 3.0}, {fitness_dd_lambda_early, 3.5}, {fitness_dd_lambda_late, 6.0}, {tuning_duration, {const,10}}, {gt_start, 19000}, {gt_end, 10000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 250}]},
        
        %% Phase 4: Profit Optimization (Runs 36-45) - Focus: Maximum P/L with strong risk control
        {31, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 12000}, {gt_end, 6500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {32, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 13000}, {gt_end, 7000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {33, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 14000}, {gt_end, 7500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {34, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 15000}, {gt_end, 8000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {35, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 16000}, {gt_end, 8500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {36, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 17000}, {gt_end, 9000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {37, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 18000}, {gt_end, 9500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {38, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 19000}, {gt_end, 10000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {39, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 20000}, {gt_end, 10500}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]},
        {40, [{fitness_function, phase2_profit_optimization}, {tuning_duration, {const,10}}, {gt_start, 21000}, {gt_end, 11000}, {specie_size_limit, 300}, {init_specie_size, 300}, {generation_limit, 300}]}
    ].

%% Prep function (similar to benchmarker:prep)
prep(E, Mode, SourcePopId) ->
    % Ensure config is applied for this run BEFORE reading config valuess
    apply_run_configs(E#experiment.id, E#experiment.run_index, E#experiment.run_configs),
    
    Old_PMP = E#experiment.pm_parameters,
    PMP = Old_PMP#pmp{
        population_id = case Mode of
            fresh ->
                % Use population_id from experiment (already set)
                Old_PMP#pmp.population_id;
            new_evo ->
                % First run: use generated ID, subsequent: will be updated
                Old_PMP#pmp.population_id;
            {evo, _} ->
                % Use new population ID (already generated)
                Old_PMP#pmp.population_id
        end,
        survival_percentage = config:survival_percentage(),
        specie_size_limit = config:specie_size_limit(),
        init_specie_size = config:init_specie_size(),
        generation_limit = config:generation_limit(),
        evaluations_limit = config:evaluations_limit(),
        benchmarker_pid = self()
    },
    Constraints = E#experiment.init_constraints,
    Population_Id = PMP#pmp.population_id,
    
    % Log run start
    ConfigStr = format_config_summary(E#experiment.run_index, E#experiment.run_configs),
    qlog:exp_runner(run_start, {E#experiment.id, E#experiment.run_index, Population_Id, Mode, ConfigStr}),
    
    % Handle population creation/cloning
    case Mode of
        fresh ->
            S = population_monitor:prep_PopState(PMP, Constraints),
            population_monitor:init_population(S, Constraints);
        new_evo ->
            % First run: fresh, subsequent: clone
            S = population_monitor:prep_PopState(PMP, Constraints),
            population_monitor:init_population(S, Constraints);
        {evo, _} ->
            % Clone population from source (creates new agent IDs, preserves artifacts)
            case clone_population_for_next_run(SourcePopId, Population_Id) of
                {atomic, _} ->
                    % Population already exists, just start monitor
                    S = population_monitor:prep_PopState(PMP, Constraints),
                    population_monitor:start(S);
                Error ->
                    io:format("Error cloning population: ~p~n", [Error]),
                    Error
            end
    end,
    
    % Enter loop
    loop(E#experiment{pm_parameters = PMP}, Population_Id).

%% ============================================================================
%% RUN LOOP
%% ============================================================================

%% Loop function (similar to benchmarker:loop)
loop(E, P_Id) ->
    qlog:xLog(qStatus, "exp_runner:loop | waiting | pop_id=~p | run_index=~p/~p", [P_Id, E#experiment.run_index, E#experiment.tot_runs]),
    receive
        {P_Id, completed, Trace} ->
            qlog:xLog(qStatus, "exp_runner:loop | completed | run=~p", [E#experiment.run_index]),
            U_TraceAcc = [Trace | E#experiment.trace_acc],
            U_RunIndex = E#experiment.run_index + 1,
            
            % Log run end (stats extracted directly in qlog:exp_runner)
            qlog:exp_runner(run_end, {E#experiment.id, E#experiment.run_index, P_Id, Trace}),
            qlog:benchmarker(P_Id,io_lib:format("Run End: Experiment id: ~p Run Index: ~p/~p", [E#experiment.id, E#experiment.run_index, E#experiment.tot_runs])),
            
            % Checkpoint between runs
            checkpoint(),
            
            % Trigger S3 upload after each run (incremental backup) with current population_id
            trigger_s3_upload(P_Id),
            
            qlog:xLog(qStatus, "exp_runner:loop | next_run=~p | tot_runs=~p", [U_RunIndex, E#experiment.tot_runs]),
            case U_RunIndex > E#experiment.tot_runs of
                true ->
                    % All runs completed
                    qlog:xLog(qStatus, "exp_runner:loop | all_completed | runs=~p", [U_RunIndex - 1]),
                    
                    % Final checkpoint
                    checkpoint(),
                    
                    config:clear(),
                    U_E = E#experiment{
                        trace_acc = U_TraceAcc,
                        run_index = U_RunIndex,
                        completed = {date(), time()},
                        progress_flag = completed
                    },
                    genotype:write(U_E),
                    qlog:exp_runner(experiment_complete, {E#experiment.id, U_RunIndex - 1}),
                    qlog:benchmarker(P_Id, io_lib:format("Experiment ~p completed with ~p runs", [E#experiment.id, U_RunIndex - 1])),
                    io:format("Experiment ~p completed with ~p runs~n", [E#experiment.id, U_RunIndex - 1]),
                    
                    % Checkpoint and exit if running in AWS (triggers S3 upload and finalization)
                    checkpoint_and_exit();
                false ->
                    % Continue to next run
                    qlog:xLog(qStatus, "exp_runner:loop | continuing | next_run=~p", [U_RunIndex]),
                    apply_run_configs(E#experiment.id, U_RunIndex, E#experiment.run_configs),
                    
                    % Generate new population ID for next run (reuse lineage from current)
                    NextPopId = generate_population_id(U_RunIndex, P_Id),
                    qlog:xLog(qStatus, "exp_runner:loop | generated_next_pop | next_pop_id=~p | run=~p", [NextPopId, U_RunIndex]),
                    
                    Old_PMP = E#experiment.pm_parameters,
                    U_PMP = Old_PMP#pmp{
                        population_id = NextPopId,
                        survival_percentage = config:survival_percentage(),
                        specie_size_limit = config:specie_size_limit(),
                        init_specie_size = config:init_specie_size(),
                        generation_limit = config:generation_limit(),
                        evaluations_limit = config:evaluations_limit(),
                        benchmarker_pid = self()
                    },
                    U_E = E#experiment{
                        trace_acc = U_TraceAcc,
                        run_index = U_RunIndex,
                        pm_parameters = U_PMP
                    },
                    genotype:write(U_E),
                    qlog:xLog(qStatus, "exp_runner:loop | updated_experiment | run_index=~p", [U_RunIndex]),
                    
                    % Log next run start
                    ConfigStr = format_config_summary(U_RunIndex, E#experiment.run_configs),
                    qlog:exp_runner(run_start, {E#experiment.id, U_RunIndex, NextPopId, new_evo, ConfigStr}),
                    qlog:benchmarker(P_Id, io_lib:format("Run Start: Experiment id: ~p Run Index: ~p, NextPopId: ~p", [E#experiment.id, U_RunIndex, NextPopId])),
                    
                    % Clone population for next run (new_evo mode clones from previous run, preserves artifacts)
                    qlog:xLog(qStatus, "exp_runner:loop | cloning_population | from=~p | to=~p", [P_Id, NextPopId]),
                    case clone_population_for_next_run(P_Id, NextPopId) of
                        {atomic, _} ->
                            qlog:xLog(qStatus, "exp_runner:loop | clone_success | next_pop_id=~p", [NextPopId]),
                            qlog:benchmarker(P_Id, io_lib:format("Successfully cloned population from ~p to ~p", [P_Id, NextPopId])),
                            Constraints = U_E#experiment.init_constraints,
                            % Population already exists, just start monitor
                            S = population_monitor:prep_PopState(U_PMP, Constraints),
                            qlog:xLog(qStatus, "exp_runner:loop | starting_population_monitor | next_pop_id=~p", [NextPopId]),
                            population_monitor:start(S),
                            qlog:xLog(qStatus, "exp_runner:loop | recursive_loop_call | next_pop_id=~p", [NextPopId]),
                            loop(U_E, NextPopId);
                        Error ->
                            qlog:xLog(qStatus, "exp_runner:loop | clone_failed | error=~p", [Error]),
                            qlog:exp_runner(run_failed, {E#experiment.id, U_RunIndex, {clone_failed, Error}}),
                            qlog:benchmarker(P_Id, io_lib:format("Error cloning population from ~p to ~p: ~p", [P_Id, NextPopId, Error])),
                            Error
                    end
            end;
        {P_Id, failed, Reason} ->
            qlog:xLog(qStatus, "exp_runner:loop | run_failed | run=~p | reason=~p", [E#experiment.run_index, Reason]),
            qlog:exp_runner(run_failed, {E#experiment.id, E#experiment.run_index, Reason}),
            io:format("Run ~p failed for experiment ~p: ~p~n", [E#experiment.run_index, E#experiment.id, Reason]),
            qlog:benchmarker(P_Id, io_lib:format("Run ~p failed for experiment ~p: ~p", [E#experiment.run_index, E#experiment.id, Reason])),
            ok;
        terminate ->
            qlog:xLog(qStatus, "exp_runner:loop | terminated | experiment=~p", [E#experiment.id]),
            qlog:exp_runner(experiment_terminate, {E#experiment.id, P_Id}),
            qlog:benchmarker(P_Id, io_lib:format("Experiment ~p terminated by request", [E#experiment.id])),
            ok
    end.


%% Continue an existing experiment (similar to benchmarker:continue)
continue(ExperimentIdOrPopulationId) ->
    case resolve_experiment_for_continue(ExperimentIdOrPopulationId) of
        undefined ->
            qlog:benchmarker(
                ExperimentIdOrPopulationId,
                io_lib:format(
                    "Can't continue experiment for id ~p, it's not present in the database.",
                    [ExperimentIdOrPopulationId]
                )
            ),
            io:format(
                "Can't continue experiment for id ~p, it's not present in the database.~n",
                [ExperimentIdOrPopulationId]
            );
        E ->
            continue_experiment(E)
    end.

continue_experiment(E) ->
    ExperimentId = E#experiment.id,
    case E#experiment.progress_flag of
        completed ->
            qlog:benchmarker(ExperimentId, io_lib:format("Experiment ~p already completed.", [ExperimentId])),
            io:format("Experiment ~p already completed: ~p~n", [ExperimentId, E#experiment.trace_acc]);
        in_progress ->
            config:init(),
            CurrentRunIndex = E#experiment.run_index,
            apply_run_configs(ExperimentId, CurrentRunIndex, E#experiment.run_configs),
            Interruptions = E#experiment.interruptions,
            U_Interruptions = [erlang:timestamp() | Interruptions],
            CurrentPopId = (E#experiment.pm_parameters)#pmp.population_id,
            case genotype:dirty_read({population, CurrentPopId}) of
                undefined ->
                    qlog:benchmarker(
                        ExperimentId,
                        io_lib:format(
                            "Continue failed: population ~p not found for experiment ~p.",
                            [CurrentPopId, ExperimentId]
                        )
                    ),
                    io:format(
                        "Continue failed: population ~p not found for experiment ~p.~n",
                        [CurrentPopId, ExperimentId]
                    );
                _Pop ->
                    U_E = E#experiment{
                        interruptions = U_Interruptions
                    },
                    genotype:write(U_E),
                    qlog:benchmarker(
                        ExperimentId,
                        io_lib:format(
                            "Resuming experiment ~p run ~p from population ~p",
                            [ExperimentId, CurrentRunIndex, CurrentPopId]
                        )
                    ),
                    spawn(exp_runner, resume, [U_E, CurrentPopId])
            end
    end.

resolve_experiment_for_continue(ExperimentIdOrPopulationId) ->
    case genotype:dirty_read({experiment, ExperimentIdOrPopulationId}) of
        undefined ->
            find_experiment_by_population_id(ExperimentIdOrPopulationId);
        E ->
            E
    end.

find_experiment_by_population_id(PopulationId) ->
    ExperimentIds = mnesia:dirty_all_keys(experiment),
    find_experiment_by_population_id(ExperimentIds, PopulationId).

find_experiment_by_population_id([ExperimentId | Rest], PopulationId) ->
    case genotype:dirty_read({experiment, ExperimentId}) of
        undefined ->
            find_experiment_by_population_id(Rest, PopulationId);
        E ->
            PMP = E#experiment.pm_parameters,
            case PMP =/= undefined andalso PMP#pmp.population_id =:= PopulationId of
                true ->
                    E;
                false ->
                    find_experiment_by_population_id(Rest, PopulationId)
            end
    end;
find_experiment_by_population_id([], _PopulationId) ->
    undefined.

resume(E, Population_Id) ->
    % Apply the current run config before rebuilding PM parameters.
    apply_run_configs(E#experiment.id, E#experiment.run_index, E#experiment.run_configs),
    Old_PMP = E#experiment.pm_parameters,
    PMP = Old_PMP#pmp{
        population_id = Population_Id,
        survival_percentage = config:survival_percentage(),
        specie_size_limit = config:specie_size_limit(),
        init_specie_size = config:init_specie_size(),
        generation_limit = config:generation_limit(),
        evaluations_limit = config:evaluations_limit(),
        benchmarker_pid = self()
    },
    U_E = E#experiment{pm_parameters = PMP},
    genotype:write(U_E),

    ConfigStr = format_config_summary(U_E#experiment.run_index, U_E#experiment.run_configs),
    qlog:exp_runner(run_start, {U_E#experiment.id, U_E#experiment.run_index, Population_Id, resume, ConfigStr}),
    qlog:benchmarker(
        Population_Id,
        io_lib:format(
            "Run Resume: Experiment id: ~p Run Index: ~p Population: ~p",
            [U_E#experiment.id, U_E#experiment.run_index, Population_Id]
        )
    ),
    
    qlog:xLog(qStatus, "exp_runner:resume | pop_id=~p | run_index=~p/~p", [Population_Id, U_E#experiment.run_index, U_E#experiment.tot_runs]),
    population_monitor:continue(Population_Id),
    loop(U_E, Population_Id).

%% ============================================================================
%% CHECKPOINT SYSTEM
%% ============================================================================

%% Checkpoint configuration
should_checkpoint() ->
    case config:checkpoint_enabled() of
        true -> true;
        false -> false;
        auto -> detect_aws_environment()
    end.

detect_aws_environment() ->
    filelib:is_dir("/var/lib/dxnn/checkpoints") andalso 
    os:getenv("S3_BUCKET") =/= false.

%% Find Mnesia directory (handles both nonode@nohost and distributed node names)
find_mnesia_directory() ->
    % Try standard name first
    case filelib:is_dir("Mnesia.nonode@nohost") of
        true -> {ok, "Mnesia.nonode@nohost"};
        false ->
            % Look for any Mnesia.* directory
            case filelib:wildcard("Mnesia.*") of
                [] -> not_found;
                [Dir | _] -> {ok, Dir}
            end
    end.

%% Create checkpoint - copies full Mnesia and logs directories
checkpoint() ->
    case should_checkpoint() of
        true -> do_checkpoint();
        false -> ok
    end.

do_checkpoint() ->
    % Pause population monitor
    catch gen_server:call(population_monitor, pause, 500),
    
    % Sync Mnesia before checkpoint
    catch mnesia:sync_log(),
    
    % Generate checkpoint timestamp and directory
    Timestamp = integer_to_list(erlang:system_time(second)),
    CheckpointDir = "/var/lib/dxnn/checkpoints/checkpoint-" ++ Timestamp,
    
    % Ensure checkpoint directory exists
    catch filelib:ensure_dir(CheckpointDir ++ "/"),
    
    % Find and copy Mnesia directory (name varies based on node name)
    MnesiaDir = find_mnesia_directory(),
    case MnesiaDir of
        {ok, SourceDir} ->
            % Always copy to standard name for portability
            copy_directory(SourceDir, CheckpointDir ++ "/Mnesia.nonode@nohost"),
            qlog:exp_runner(checkpoint_mnesia_copied, {CheckpointDir});
        not_found ->
            error_logger:warning_msg("Mnesia directory not found, skipping~n")
    end,
    
    % Copy full logs directory if it exists
    case filelib:is_dir("logs") of
        true ->
            copy_directory("logs", CheckpointDir ++ "/logs"),
            qlog:exp_runner(checkpoint_logs_copied, {CheckpointDir});
        false ->
            error_logger:warning_msg("Logs directory not found, skipping~n")
    end,
    
    % Copy config.erl if it exists
    case filelib:is_file("config.erl") of
        true ->
            file:copy("config.erl", CheckpointDir ++ "/config.erl");
        false ->
            ok
    end,
    
    % Create checkpoint metadata
    create_checkpoint_metadata(CheckpointDir, Timestamp),
    
    qlog:exp_runner(checkpoint_complete, {CheckpointDir}),
    ok.

%% Copy directory recursively
copy_directory(Source, Dest) ->
    catch filelib:ensure_dir(Dest ++ "/"),
    case file:list_dir(Source) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                SourcePath = filename:join(Source, File),
                DestPath = filename:join(Dest, File),
                case filelib:is_dir(SourcePath) of
                    true ->
                        copy_directory(SourcePath, DestPath);
                    false ->
                        catch filelib:ensure_dir(DestPath),
                        file:copy(SourcePath, DestPath)
                end
            end, Files);
        {error, Reason} ->
            error_logger:warning_msg("Failed to list directory ~p: ~p~n", [Source, Reason])
    end.

%% Create checkpoint metadata file with lineage_id and population_id
create_checkpoint_metadata(CheckpointDir, Timestamp) ->
    MetadataFile = CheckpointDir ++ "/_CHECKPOINT_INFO",
    
    % Get current population_id from environment or experiment
    PopulationId = case os:getenv("POPULATION_ID") of
        false -> "unknown";
        PopId -> PopId
    end,
    
    % Extract lineage_id from population_id
    LineageId = extract_lineage_from_string(PopulationId),
    
    Metadata = io_lib:format(
        "{\"timestamp\": ~p, \"node\": \"~s\", \"type\": \"checkpoint\", \"created_at\": \"~s\", \"population_id\": \"~s\", \"lineage_id\": \"~s\"}~n",
        [Timestamp, atom_to_list(node()), format_timestamp(), PopulationId, LineageId]
    ),
    case file:write_file(MetadataFile, Metadata) of
        ok -> ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to write metadata: ~p~n", [Reason])
    end.

%% Extract lineage_id from population_id string (for metadata)
extract_lineage_from_string(PopulationId) when is_list(PopulationId) ->
    case string:split(PopulationId, "_", all) of
        [_Timestamp, LineageId | _] when length(LineageId) =:= 4 ->
            LineageId;
        _ ->
            "unknown"
    end;
extract_lineage_from_string(_) ->
    "unknown".

%% Format timestamp as ISO 8601
format_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec]).

%% Trigger S3 upload without exiting (for incremental backups after each run)
%% Calls the finalize script directly to upload current checkpoint to S3
trigger_s3_upload(PopulationId) ->
    case should_checkpoint() of
        true ->
            qlog:exp_runner(s3_upload_triggered, {incremental_backup}),
            error_logger:info_msg("Triggering S3 upload for incremental backup~n"),
            
            % Get required environment variables
            S3Bucket = case os:getenv("S3_BUCKET") of
                false -> "dxnn-checkpoints";
                Bucket -> Bucket
            end,
            
            S3Prefix = case os:getenv("S3_PREFIX") of
                false -> "dxnn-prod";
                Prefix -> Prefix
            end,
            
            % Convert population_id to string (handle both atom and binary)
            PopIdStr = case PopulationId of
                P when is_binary(P) -> binary_to_list(P);
                P when is_atom(P) -> atom_to_list(P);
                P when is_list(P) -> P;
                _ -> "unknown"
            end,
            
            % Extract lineage_id from population_id
            LineageId = extract_lineage_from_string(PopIdStr),
            
            % Build command with all required environment variables
            Cmd = io_lib:format(
                "S3_BUCKET=~s S3_PREFIX=~s POPULATION_ID=~s LINEAGE_ID=~s COMPLETION_STATUS=incremental EXIT_CODE=0 /usr/local/bin/finalize_run.sh >> /var/log/dxnn-run.log 2>&1 &",
                [S3Bucket, S3Prefix, PopIdStr, LineageId]
            ),
            
            % Execute in background
            Result = os:cmd(lists:flatten(Cmd)),
            error_logger:info_msg("S3 upload triggered for population: ~s~n", [PopIdStr]),
            qlog:exp_runner(s3_upload_initiated, {population_id, PopIdStr}),
            ok;
        false ->
            qlog:exp_runner(s3_upload_skipped, {local_env}),
            ok
    end.

%% Checkpoint and exit (for experiment completion and spot interruptions)
%% Only exits if running in AWS environment to trigger S3 upload
checkpoint_and_exit() ->
    checkpoint(),
    
    case should_checkpoint() of
        true ->
            qlog:exp_runner(checkpoint_and_exit, {aws_detected, exiting}),
            init:stop();
        false ->
            qlog:exp_runner(checkpoint_and_exit, {local_env, continuing}),
            ok
    end.

%% Restore from latest checkpoint (no-op if absent)
maybe_restore() ->
    case filelib:wildcard("/var/lib/dxnn/checkpoints/checkpoint-*/_CHECKPOINT_INFO") of
        [] -> 
            error_logger:info_msg("No checkpoint files found~n"),
            ok;
        Files ->
            % Get latest checkpoint directory
            Latest = lists:last(lists:sort(Files)),
            CheckpointDir = filename:dirname(Latest),
            error_logger:info_msg("Restoring from: ~p~n", [CheckpointDir]),
            
            % Restore Mnesia if exists
            MnesiaSource = CheckpointDir ++ "/Mnesia.nonode@nohost",
            case filelib:is_dir(MnesiaSource) of
                true ->
                    restore_directory(MnesiaSource, "Mnesia.nonode@nohost"),
                    error_logger:info_msg("Mnesia restored from checkpoint~n");
                false ->
                    error_logger:warning_msg("No Mnesia directory in checkpoint~n")
            end,
            
            % Restore logs if exists
            LogsSource = CheckpointDir ++ "/logs",
            case filelib:is_dir(LogsSource) of
                true ->
                    restore_directory(LogsSource, "logs"),
                    error_logger:info_msg("Logs restored from checkpoint~n");
                false ->
                    error_logger:warning_msg("No logs directory in checkpoint~n")
            end,
            
            % Restore config if exists
            ConfigSource = CheckpointDir ++ "/config.erl",
            case filelib:is_file(ConfigSource) of
                true ->
                    file:copy(ConfigSource, "config.erl"),
                    error_logger:info_msg("Config restored from checkpoint~n");
                false ->
                    ok
            end,
            
            ok
    end.

%% Restore directory recursively
restore_directory(Source, Dest) ->
    catch filelib:ensure_dir(Dest ++ "/"),
    case file:list_dir(Source) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                SourcePath = filename:join(Source, File),
                DestPath = filename:join(Dest, File),
                case filelib:is_dir(SourcePath) of
                    true ->
                        restore_directory(SourcePath, DestPath);
                    false ->
                        catch filelib:ensure_dir(DestPath),
                        file:copy(SourcePath, DestPath)
                end
            end, Files);
        {error, Reason} ->
            error_logger:warning_msg("Failed to restore directory ~p: ~p~n", [Source, Reason])
    end.

