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
    %% One species per entry. config:families() copies of the same constraint give that many independent families:
    %% population_monitor:mutate_population/5 selects and breeds inside each species, mutants stay in their parent's
    %% species, and clone_population_for_next_run/2 carries every species into the next run.
    lists:append(lists:duplicate(erlang:max(1,config:families()),family_constraints())).

family_constraints() ->
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

%% Internal helper with lineage ID (now includes timestamp: "2026-03-09T15-30-45Z_a3f9")
generate_population_id_with_lineage(RunIndex, LineageId) when is_binary(LineageId) ->
    % Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
    % Example: <<"2025-02-11T15-30-45Z_2026-03-09T15-30-45Z_a3f9_run1">>
    % Wait, that's redundant. Let me fix this.
    %
    % Better format: Just use lineage_id directly since it already has timestamp
    % Population ID: <run_timestamp>_<lineage_id>_run<N>
    % Where lineage_id = <experiment_start_timestamp>_<code>
    %
    % Example: 2025-02-11T15-30-45Z_2026-03-09T15-30-45Z_a3f9_run1
    %          ↑ run start time      ↑ experiment start (lineage)
    Timestamp = format_iso8601(erlang:timestamp()),
    RunIndexStr = integer_to_list(RunIndex),
    <<Timestamp/binary, "_", LineageId/binary, "_run", (list_to_binary(RunIndexStr))/binary>>.

%% Generate 4 random alphanumeric characters + timestamp prefix for experiment folder
%% Returns format: "2026-03-09T15-30-45Z_a3f9" (timestamp + 4-char code)
generate_lineage_id() ->
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
    RandomChars = [lists:nth(rand:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 4)],
    Timestamp = format_iso8601(erlang:timestamp()),
    % Replace colons with hyphens for filesystem compatibility
    TimestampStr = binary:replace(Timestamp, <<":">>, <<"-">>, [global]),
    <<TimestampStr/binary, "_", (list_to_binary(RandomChars))/binary>>.

%% Extract lineage ID from source population ID
%% Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
%% LineageId is now: "2026-03-09T15-30-45Z_a3f9" (timestamp + 4-char code)
extract_lineage_id(SourcePopId) ->
    case binary:split(SourcePopId, <<"_">>, [global]) of
        [_Timestamp, Part2, Part3 | _] ->
            % LineageId is the middle parts: timestamp_code
            % Reconstruct: _Timestamp is first part, Part2 is the timestamp of lineage, Part3 is the 4-char code
            % Actually the format is: <timestamp>_<lineage_timestamp>_<lineage_code>_run<N>
            % Wait, that's wrong. Let me reconsider.
            % 
            % New format should be:
            % Population ID: <timestamp>_<lineage_id>_run<N>
            % Where lineage_id itself is: <timestamp>_<code>
            %
            % So split gives: [timestamp, lineage_timestamp, code, "runN"]
            % We want lineage_id = lineage_timestamp_code
            <<Part2/binary, "_", Part3/binary>>;
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
    % Evaluation settings are read live: the run's overrides are already loaded when this is called.
    % In episode mode with episode_tuning off the agents are not tuned at all, whatever their tuning_duration says.
    EpisodeTuningOff = (config:evaluation_mode() == episodes) andalso (config:episode_tuning() =/= true),
    TuningDurStr = case {EpisodeTuningOff, TuningDur} of
        {true, _} -> "off";
        {_, {const, N}} -> io_lib:format("const(~p)", [N]);
        {_, Other} -> io_lib:format("~p", [Other])
    end,
    TuneEpisodesStr = case EpisodeTuningOff of
        true -> "off";
        false -> integer_to_list(config:episodes_tune())
    end,
    EvalStr = case config:evaluation_mode() of
        episodes ->
            io_lib:format(" | eval=episodes fitness=~w spread=~p cost_schedule=~w val_spread=~w min_trades=~p | episode_bars=~p warmup=~p tune=~s score=~p val=~p val_every=~p | train=~w val=~w | stall_ms=~p | selection_memory=~p time_limit_s=~p generation_limit=~p",
                [config:fitness_function(), config:account_spread(), config:cost_schedule(), config:validation_spread(), config:min_trades_per_episode(),
                 config:episode_bars(), config:episode_warmup_bars(),
                 TuneEpisodesStr, config:episodes_score(), config:episodes_val(), config:validate_population_every(), config:train_span(), config:val_span(),
                 config:stall_timeout_ms(), config:selection_memory(), config:run_time_limit_s(), config:generation_limit()])
                ++ io_lib:format(" | arm=~s episode_seed=~w evolution_seed=~w | isolation=~w sense_before_trade=~w rollover_rule=~w late_fill=~w | plt_encoding=~w position_sensor=~w init_hidden=~w init_recurrent=~w validate_last=~w",
                [case config:get_val(test_arm, undefined) of undefined -> "reference"; Arm -> Arm end,
                 config:episode_seed(), config:evolution_seed(), config:episode_isolation(), config:sense_before_trade(),
                 config:rollover_rule(), config:late_fill(), config:plt_encoding(), config:position_sensor(),
                 config:init_hidden_neurons(), config:init_recurrent(), config:validate_last_generation()]);
        EvalMode ->
            io_lib:format(" | eval=~w fitness=~w spread=~p", [EvalMode, config:fitness_function(), config:account_spread()])
    end,
    lists:flatten(io_lib:format(
        "morph=~w enc=~w arch=~w | gt=~p-~p bench=~p-~p | "
        "evo=~w sel=~w fpost=~w | survival=~p | "
        "specie_limit=~p init_size=~p tuning=~s~s",
        [Morph, Enc, Arch, GtStart, GtEnd, BenchStart, BenchEnd,
         EvoAlg, Selection, FPost, Survival,
         SpecieSizeLimit, InitSpecieSize, TuningDurStr, EvalStr]
    )).

%% Every run whose agents are compared on the validation span counts as one trial (see eval_protocol:record_trial/2).
record_validation_trial(Population_Id, RunIndex, ConfigStr) ->
    case config:evaluation_mode() of
        episodes -> eval_protocol:record_trial(Population_Id, io_lib:format("run=~p | ~s", [RunIndex, ConfigStr]));
        _ -> ok
    end.

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
    warn_if_load_compaction(),
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

%% The VM must be started with "+scl false". With the default (compaction of load on) the scheduler threads that go to
%% sleep while few agents run do not all come back when the next generation starts: after one stretch with a single agent
%% running, 144 agents were evaluated on 2 of 10 cores for the rest of the run (REVIEW.md 8.20).
warn_if_load_compaction() ->
    Args = case catch erlang:system_info(emu_args) of
        L when is_list(L) -> L;
        _ -> []
    end,
    case scl_off(Args) of
        true -> ok;
        false -> io:format("WARNING | this VM was started without \"+scl false\": expect it to use about 2 cores, whatever the machine has (REVIEW.md 8.20)~n")
    end.

scl_off(["-scl","false"|_]) -> true;
scl_off([_|Rest]) -> scl_off(Rest);
scl_off([]) -> false.

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
%% get_run_configs/0 is what exp_runner:start/1 runs. Three environment variables choose what it returns (REVIEW.md 8.23):
%%   DXNN_TEST_ARM        unset or empty: the reference experiment, reference_run_configs/0, exactly as before.
%%                        ff_level | rec_level | ff_vol | rec_vol: one arm of the four-arm experiment (arm_run_configs/1).
%%                        fold_2015 | fold_2016 | fold_2017: rec_level on a chronological fold. Any arm can be put on a
%%                        fold with a plus sign, e.g. ff_vol+fold_2015. Any other value stops the experiment at start.
%%   DXNN_EPISODE_SEED    an integer: the episode draws of every run are reproducible ({episode_seed,{Seed,RunIndex}}:
%%                        each chained run draws its own, episodes:begin_generation/2).
%%   DXNN_EVOLUTION_SEED  an integer: the first population's genotypes, every generation's selection and mutation are
%%                        reproducible ({evolution_seed,{Seed,RunIndex}}, config:seed_rand/1).
%% With none of them set the result is reference_run_configs/0, unchanged. The arm, once an experiment has started, is
%% stored in its experiment record with the rest of its run configs, so a resumed experiment keeps it.
get_run_configs() ->
    Configs = case env_string("DXNN_TEST_ARM") of
        undefined -> reference_run_configs();
        Arm -> [{I, [{test_arm, Arm} | List]} || {I, List} <- arm_run_configs(Arm)]
    end,
    with_seeds(Configs, env_integer("DXNN_EPISODE_SEED"), env_integer("DXNN_EVOLUTION_SEED")).

with_seeds(Configs, undefined, undefined) ->
    Configs;
with_seeds(Configs, EpisodeSeed, EvolutionSeed) ->
    [{I, List ++ [{episode_seed, {EpisodeSeed, I}} || EpisodeSeed =/= undefined]
              ++ [{evolution_seed, {EvolutionSeed, I}} || EvolutionSeed =/= undefined]} || {I, List} <- Configs].

env_string(Name) ->
    case os:getenv(Name) of
        false -> undefined;
        Value ->
            case string:trim(Value) of
                "" -> undefined;
                Trimmed -> Trimmed
            end
    end.

env_integer(Name) ->
    case env_string(Name) of
        undefined ->
            undefined;
        Value ->
            case string:to_integer(Value) of
                {Int, ""} -> Int;
                _ -> erlang:error({bad_environment_variable, Name, Value})
            end
    end.

%% arm_run_configs/1: the four-arm experiment of REVIEW.md 8.23 and its chronological folds. Two questions, crossed:
%%   ff | rec        feedforward against recurrent. Both start from the same network: three hidden neurons that read
%%                   every starting sensor, and one output neuron that reads them and drives the trade
%%                   (genotype:construct_InitialHiddenLayer/7). rec gives each hidden neuron a link to itself
%%                   (init_recurrent) and allows recurrent links in mutation; ff allows none
%%                   (connection_architecture feedforward, genome_mutator:filter_OutlinkIdPool/3).
%%   level | vol     the price lists (fx_PLT) scaled by the price level as before, or by the trailing volatility
%%                   (plt_encoding, fx_series:answer_at/3).
%% Common to all: the reference setup of reference_base/2 (plain neurons, the mutation mix, price lists, indicators,
%% the six-value clock), one family of 48, 32 episodes per evaluation; the position as a sensor from the start
%% (position_sensor; position only, not open profit); no position opened, closed or reversed in hours 21 to 23
%% (rollover_rule); every agent validated at generation 0 and at the last generation of each run
%% (validate_last_generation); fixed generation counts and no wall-time limit, so machines of different speed do the
%% same work: free 30, ramp 60, then 150 generations at a quarter pip in three runs of 50 (each run's population stays
%% in Mnesia, so one is saved every 50 generations).
%% The folds differ only in the spans: fold_2017 has the spans of the four arms.
%% "pilot": the rec_vol arm cut to 6, 6 and 8 generations and 40 validation episodes, for testing a machine or the cloud
%% path end to end. Full size otherwise (48 agents, 32 episodes), so its gen_seconds can be compared between machines.
%% It passes through two chained-run changes, the cost ramp, validation at first and last generations, and completion.
arm_run_configs("pilot") ->
    [{I, lists:keystore(episodes_val, 1, lists:keystore(generation_limit, 1, List, {generation_limit, G}), {episodes_val, 40})}
     || {{I, List}, G} <- lists:zip(lists:sublist(arm_run_configs("rec_vol"), 3), [6, 6, 8])];
%% "night": one long run of the rec_vol setup with more agents (REVIEW.md 8.24). Four families of 48 on the same
%% episodes; free trading 20 generations, the cost ramp 40, then four runs of NightGens at a quarter pip, each leaving
%% its population in Mnesia. Every agent is validated at the last generation of each run, on 120 episodes of 2017 (a
%% full pass of 192 agents on all 369 would take up to half an hour). Each quarter-pip run also has a wall-time cap; a
%% run ended by the cap skips its validation pass. The cap is 4 h so that it only catches a hang: on a 12-vCPU GCP
%% n2 a generation takes about twice as long as on the Mac, and 100 minutes would cut runs 3 to 6 short.
arm_run_configs("night") ->
    NightGens = 45,
    [R1, R2, R3 | _] = arm_run_configs("rec_vol"),
    Set = fun(List, Changes) -> lists:foldl(fun({K, _} = Kv, Acc) -> lists:keystore(K, 1, Acc, Kv) end, List, Changes) end,
    Common = [{families, 4}, {validate_population_every, 0}, {validate_last_generation, true}, {episodes_val, 120}],
    [{1, Set(element(2, R1), Common ++ [{generation_limit, 20}])},
     {2, Set(element(2, R2), Common ++ [{generation_limit, 40}, {cost_schedule, [{0,0.0},{40,0.000025}]}])}]
    ++ [{I, Set(element(2, R3), Common ++ [{generation_limit, NightGens}, {run_time_limit_s, 14400}])} || I <- [3, 4, 5, 6]];
arm_run_configs(Name) ->
    {Arch, Encoding, Fold} = parse_arm(Name),
    Target = 0.000025,
    {TrainSpan, ValSpan} = fold_spans(Fold),
    ArmSettings = [
        {families, 1},
        {connection_architecture, case Arch of ff -> feedforward; rec -> recurrent end},
        {init_hidden_neurons, 3},
        {init_recurrent, Arch == rec},
        {plt_encoding, Encoding},
        {position_sensor, true},
        {rollover_rule, true}, {late_fill, false},
        {validate_last_generation, true},
        %% The score selection ranks by (REVIEW.md 8.26): the t-statistic of drift-free money in pips. alpha_t (in
        %% reference_base/2) divides by the agent's own exposure and was inflated by "take the gain, hold the loss" (8.25).
        {fitness_function, alpha_pips_t},
        {train_span, TrainSpan}, {val_span, ValSpan}
    ],
    Base = lists:foldl(fun({Key, _} = Setting, Acc) -> lists:keystore(Key, 1, Acc, Setting) end, reference_base(Target, 1), ArmSettings),
    Stage = fun(Extra) -> Base ++ [{min_trades_per_episode, 2} | Extra] end,
    [
        {1, Stage([{account_spread, 0.0}, {generation_limit, 30}])},
        {2, Stage([{account_spread, 0.0}, {cost_schedule, [{0,0.0},{60,Target}]}, {generation_limit, 60}])},
        {3, Stage([{account_spread, Target}, {generation_limit, 50}])},
        {4, Stage([{account_spread, Target}, {generation_limit, 50}])},
        {5, Stage([{account_spread, Target}, {generation_limit, 50}])}
    ].

parse_arm(Name) ->
    {ArmName, Fold} = case string:split(Name, "+") of
        [A, F] -> {A, F};
        [A] ->
            case lists:member(A, fold_names()) of
                true -> {"rec_level", A};
                false -> {A, "fold_2017"}
            end
    end,
    case {lists:keyfind(ArmName, 1, arm_table()), lists:member(Fold, fold_names())} of
        {{_, Arch, Encoding}, true} -> {Arch, Encoding, Fold};
        _ -> erlang:error({unknown_test_arm, Name, ["pilot", "night"] ++ [Known || {Known, _, _} <- arm_table()] ++ fold_names()})
    end.

arm_table() ->
    [{"ff_level", ff, level}, {"rec_level", rec, level}, {"ff_vol", ff, vol}, {"rec_vol", rec, vol}].

fold_names() ->
    ["fold_2015", "fold_2016", "fold_2017"].

fold_spans("fold_2015") -> {{{2012,1,2},{2014,12,31}}, {{2015,1,1},{2015,12,31}}};
fold_spans("fold_2016") -> {{{2012,1,2},{2015,12,31}}, {{2016,1,1},{2016,12,31}}};
fold_spans("fold_2017") -> {{{2012,1,2},{2016,12,31}}, {{2017,1,1},{2017,12,31}}}.

%% Data and periods (REVIEW.md 8.12, 8.13, 8.15 and 8.16): fx_tables/EURUSD1.txt holds 2012 to 2024 of the long price
%% file (4.78 million one-minute bars). This config is the forward test inside that history: training draws its
%% episodes from 2012 to 2016, validation is 2017, and 2018 to 2020 is the test. The test years are in the loaded file
%% but no run reads them: one agent chosen on 2017 is replayed on them once, afterwards (the protocol is in 8.16).
%% Why these years: the pattern the agents of the overnight run learned (8.15) was in the prices up to about 2020 and
%% has faded since, so 2021 to 2024 cannot show whether an agent learns to trade, only that the pattern is gone.
%% January 2025 to November 2026 is not in the loaded file at all: it stays in fx_tables/EURUSD_M1_MIDPOINT_UTC.txt
%% and is not touched by this experiment.
%% The file's clock is local market time, not UTC (one hour ahead of UTC in summer). That is harmless here and matters
%% only when a trained agent is connected to a live feed.
%%
%% Staged curriculum (see REVIEW.md, sections 8.9 to 8.11): the same population goes through three stages. The last
%% stage is split into two runs so that the experiment leaves two saved populations to choose from.
%%
%%   run 1     free   30 generations. Trades cost nothing.
%%   run 2     ramp   60 generations. The cost per round trip rises a little every generation, from nothing to a
%%                    quarter pip (cost_schedule).
%%   runs 3-4  hold   the cost stays at a quarter pip, the level real trading is assumed to cost. Each run lasts Hold
%%                    seconds of wall time (run_time_limit_s) however many generations that is, so the experiment ends
%%                    about 2 x Hold + 25 minutes after it starts, whatever size the networks grow to.
%%                    In the overnight run the agents stopped improving within the first hundred quarter-pip
%%                    generations, about half an hour (8.15); raise Hold to give them longer.
%%                    The trade minimum is 2 positions per episode in every run. Lowering it to 0.5 (runs 5 and 6 of
%%                    the overnight run) changed nothing except letting agents that hold one position in for a few
%%                    generations.
%%
%% If the experiment is interrupted, the populations of the runs that finished are in Mnesia and their GEN_SUMMARY lines
%% in logs/Benchmarker/generation_summary.log.
%%
%% The objective is the same in all three stages: money after cost. Only the cost changes. Trading as such is not
%% rewarded beyond the minimum below: with free trades the agents of the second trial already opened about 300 positions
%% per 1,000 bars without being asked to, and on the train span the rules that come closest to paying after a quarter
%% pip open between 0.5 and 14 (measured, REVIEW.md 8.9; none of them pays reliably), so a reward for trading would
%% pull away from them.
%%
%% What every run has in common (Base):
%%   - evaluation_mode episodes: each generation draws fresh score episodes from train_span; fitness is alpha_t on the
%%     score episodes: the t-statistic of the money the agent made per unit of position, after cost, with the average
%%     drift of the whole span taken out (fx:ep_stat/4). One lucky window does not win, and being long in a span that
%%     happened to rise earns nothing.
%%   - min_trades_per_episode 2: an agent that opens fewer positions per episode on average is ranked below every
%%     agent that does, by how many it opened. That is the only place where trading itself is rewarded; it keeps
%%     "never trade" out. Fractions are allowed (0.5 means 16 positions in a 32-episode evaluation).
%%   - selection_memory 0.85: selection ranks by a decaying average of each agent's score relative to the median of its
%%     generation (selection_memory.erl), not by one generation's score. A family that is really better is no longer
%%     removed by one unlucky draw. GEN_SUMMARY still reports the raw scores.
%%   - validation_spread: validation episodes always cost a quarter pip, also while training is still free, so the
%%     validation columns of GEN_SUMMARY read at the real cost from the first generation to the last.
%%   - episode_tuning false: no weight tuning inside a generation (it took two thirds of the compute in the first trial
%%     and nothing it found carried over). Weights change through the mutate_weights mutation.
%%   - val_span is never used for selection. Every 40 generations each agent is also run on every validation episode
%%     (episodes_val is an upper limit; about 370 fit in 2017) and the result is logged (GEN_SUMMARY lines in
%%     logs/Benchmarker/generation_summary.log). All of them, because a subset is the same subset at every check and
%%     its luck repeats: the 200 of the overnight run read 2.4 pips per episode worse than the whole period (8.15).
%%     One year resolves about 2.4 pips per episode, so read val_net_pips_ep as a range, not as a result.
%%   - the sensors answered from the shared price arrays (price lists on 15-minute, hourly, 4-hour and daily bars,
%%     indicators, a clock) are offered next to the raw ones, and removal mutations are in the default operator list.
%%   - init_sensors series: a new agent starts with all four price lists and the indicators, not with one sensor drawn
%%     at random. With one, the whole population read its founder's sensor for the rest of the experiment: the forward
%%     run of 2026-11-12 read four-hour bars only and learned nothing (REVIEW.md 8.17). Like the encoding, this is
%%     fixed when the agents are created, so only its run-1 value takes effect.
%%   - families: with more than one, the population is that many separate species of 48 that never mix. Each is an
%%     independent attempt on the same episodes, and each gets a FAMILY_SUMMARY line per generation next to GEN_SUMMARY.
%%     The default 1 is the experiment as it was. Also fixed at creation.
%%   - selection is rank (the fitness can be negative, which competition cannot handle).
%%   - neural encoding: it evaluates about 7 times faster than the substrate. Use [substrate] to compare.
%%   - population 48 and 32 score episodes from the first generation. Small networks are cheap, and with 8 episodes the
%%     first stage of the third trial learned nothing.
%% Nothing limits how often an agent may trade and nothing limits network size.
%% In GEN_SUMMARY read positive (agents above zero after cost), gross_pips_trade (must exceed the cost in pips) and
%% net_pips_ep, and the val_ versions of the same. vm_mb and processes should stay level through the run.
%% episode_bars stays fixed so the validation episodes are identical in all runs.
%% Yardstick (REVIEW.md 8.12): on the training years a one-line rule (take the other side of a move of more than about
%% 11 pips within 60 bars, leave when it is undone) makes +2.7 pips per 1,000 bars after a quarter pip. net_pips_ep is
%% in the same unit.
%%
%% apply_run_configs/3 clears every override before loading a run, so each entry is Base ++ its own changes.
%% Note: tuning_duration, the selection function, the encoding and the mutation operators are stored in the agents and
%% the population when they are created, so only their run-1 values take effect.
reference_run_configs() ->
    Target = 0.000025,   % a quarter pip per round trip (0.0001 = 1 pip)
    %% Three families overnight (REVIEW.md 8.18): 9 h 28 min on 2026-11-12, about 8 h 30 min expected since the changes
    %% of REVIEW.md 8.20. For the one-family experiment set Families = 1 and Hold = 3600 (about 2 h 40 min).
    %% Start the VM with "+scl false" (README.md).
    Hold = 10800,        % wall seconds for each of the two quarter-pip runs (3 h: each of three families gets the generations one family gets in 1 h)
    Families = 3,        % independent families of 48 agents. A generation takes Families times as long: change Hold with it
    Base = reference_base(Target, Families),
    [
        {1, Base ++ [{min_trades_per_episode, 2},   {account_spread, 0.0},                                        {generation_limit, 30}]},
        {2, Base ++ [{min_trades_per_episode, 2},   {account_spread, 0.0}, {cost_schedule, [{0,0.0},{60,Target}]}, {generation_limit, 60}]},
        {3, Base ++ [{min_trades_per_episode, 2},   {account_spread, Target}, {generation_limit, 100000}, {run_time_limit_s, Hold}]},
        {4, Base ++ [{min_trades_per_episode, 2},   {account_spread, Target}, {generation_limit, 100000}, {run_time_limit_s, Hold}]}
    ].

%% reference_base/2: what every run of the reference experiment has in common (see the comment above
%% reference_run_configs/0). The arms of arm_run_configs/1 start from it too.
reference_base(Target, Families) ->
    [
        {evaluation_mode, episodes},
        {fitness_function, alpha_t},
        %% Forward test inside the long history (REVIEW.md 8.16): train on 2012 to 2016, validate on all of 2017.
        %% 2018 to 2020 is the test. No run reads it; one agent chosen on 2017 is replayed on it once, afterwards.
        %% (The overnight run of 8.15 used train 2012-01-02..2020-12-31, validation 2021-01-04..2024-12-31, 200 episodes
        %% every 20 generations.)
        {train_span, {{2012,1,2},{2016,12,31}}}, {val_span, {{2017,1,1},{2017,12,31}}},
        {episode_bars, 1000}, {episode_warmup_bars, 30}, {episodes_val, 400},
        {validate_population_every, 1000000},   % validation at generation 0 of each run only (it was every 40 generations: 27% of the night, REVIEW.md 8.20)
        {validation_spread, Target},
        {selection_memory, 0.85},
        %% Sensors answered from the shared price arrays, offered next to the raw price lists and the chart image.
        {plt_specs, [{15,32},{60,24},{240,30},{1440,20}]}, {indicator_sensor, true}, {clock_sensor, true}, {clock_harmonics, 2},
        {init_sensors, series_clock},   % the price lists, the indicators and the clock from the first generation (REVIEW.md 8.22)
        {families, Families},
        %% Plain neurons (REVIEW.md 8.22): tanh or linear of a weighted sum, no plasticity. With nine functions, three
        %% aggregators and twelve plasticity rules each family's output neuron was one type drawn by lot, and one draw
        %% (cos of a product) held long all night. Fixed when the agents are created.
        {neural_activation_functions, [tanh, linear]},
        {neural_plasticity_functions, [none]},
        {neural_aggregation_functions, [dot_product]},
        %% Weights: half of all mutations (350 of 699; it was 25 of 449, and with episode_tuning off nothing else changes a
        %% weight). The operators that changed a neuron's aggregator or plasticity are gone with the choices they had.
        {mutation_operators, [
            {mutate_weights,350}, {mutate_af,25}, {add_bias,25}, {remove_bias,10},
            {add_neuron,60}, {add_inlink,50}, {add_outlink,40}, {outsplice,15},
            {add_sensor,10}, {add_sensorlink,30}, {add_actuatorlink,20},
            {remove_neuron,20}, {remove_inlink,15}, {remove_outlink,15}, {remove_sensorlink,10}, {remove_sensor,4}]},
        %% No chart image and no raw one-minute lists: 600 and 90 to 270 inputs each, a quarter of an agent's time, and
        %% no measured gain (REVIEW.md 8.15, 8.20, 8.22).
        {pli_resolutions, []}, {pci_horizontal_resolutions, []},
        {generation_test, false},   % no benchmark step: it ran each family's best agent alone on 2017 every 500 evaluations (8% of the night of REVIEW.md 8.19) and left the VM on two cores afterwards (8.20)
        %% Each episode starts from a freshly reset network, and every sensor reads a bar before its trade is taken
        %% (REVIEW.md 8.23). Before, the 32 episodes of an evaluation ran back to back on one network.
        {episode_isolation, true}, {sense_before_trade, true},
        %% GEN_SUMMARY also scores never trading, always long, the 60-bar momentum rule and a time-of-day rule on the
        %% generation's episodes (REVIEW.md 8.23), next to the two reversal rules it always had.
        {extra_baselines, true},
        {agent_encoding_types, [neural]},
        {population_selection_f, rank}, {population_fitness_postprocessor_f, none},
        {episode_tuning, false}, {episodes_tune, 1},   % episodes_tune is only drawn, not run, while tuning is off
        {tuning_duration, {const,4}},                  % only used if episode_tuning is switched on
        {survival_percentage, 0.5},
        {episodes_score, 32}, {specie_size_limit, 48}, {init_specie_size, 48}
    ].

%% The previous 12-phase curriculum (one fixed recent window per run, total_profit). Kept as the reference design:
%% point get_run_configs/0 at it to reproduce the old behaviour. account_spread now defaults to 0.0, so add
%% {account_spread, 0.00015} to each entry to reproduce it with costs.
get_run_configs_legacy() ->
    [
        %% All runs use total_profit (net equity after spread). The previous phase0/phase1/phase2
        %% functions rewarded trade counts more than P&L (a losing 49-trade agent outscored a
        %% profitable 1-trade agent). Windows and population sizes are unchanged; the phased
        %% curriculum (spread ramp, multi-window evaluation) is a separate step.
        %% Phase A: Behavior bootstrap (short)
        {1, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,10}}, {gt_start, 2600}, {gt_end, 1800}, {specie_size_limit, 12}, {init_specie_size, 12}, {generation_limit, 10}, {survival_percentage, 0.5}]},
        {2, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,10}}, {gt_start, 3600}, {gt_end, 2600}, {specie_size_limit, 14}, {init_specie_size, 14}, {generation_limit, 12}, {survival_percentage, 0.50}]},

        %% Phase B
        {3, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 5000}, {gt_end, 3400}, {specie_size_limit, 20}, {init_specie_size, 20}, {generation_limit, 40}, {survival_percentage, 0.5}]},
        {4, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 6200}, {gt_end, 4600}, {specie_size_limit, 24}, {init_specie_size, 24}, {generation_limit, 50}, {survival_percentage, 0.5}]},
        {5, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 7400}, {gt_end, 5600}, {specie_size_limit, 28}, {init_specie_size, 28}, {generation_limit, 60}, {survival_percentage, 0.5}]},
        {6, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 8600}, {gt_end, 6600}, {specie_size_limit, 32}, {init_specie_size, 32}, {generation_limit, 60}, {survival_percentage, 0.5}]},

        %% Phase C
        {7, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 7000}, {gt_end, 5200}, {specie_size_limit, 32}, {init_specie_size, 32}, {generation_limit, 70}, {survival_percentage, 0.50}]},
        {8, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 8200}, {gt_end, 6200}, {specie_size_limit, 36}, {init_specie_size, 36}, {generation_limit, 70}, {survival_percentage, 0.50}]},
        {9, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 9400}, {gt_end, 7000}, {specie_size_limit, 40}, {init_specie_size, 40}, {generation_limit, 80}, {survival_percentage, 0.50}]},
        {10, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 10600}, {gt_end, 7800}, {specie_size_limit, 44}, {init_specie_size, 44}, {generation_limit, 80}, {survival_percentage, 0.45}]},
        {11, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 11800}, {gt_end, 8600}, {specie_size_limit, 48}, {init_specie_size, 48}, {generation_limit, 90}, {survival_percentage, 0.45}]},
        {12, [{fitness_function, total_profit}, {population_fitness_postprocessor_f, none}, {tuning_duration, {const,20}}, {gt_start, 13000}, {gt_end, 9400}, {specie_size_limit, 48}, {init_specie_size, 48}, {generation_limit, 100}, {survival_percentage, 0.45}]}
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
    record_validation_trial(Population_Id, E#experiment.run_index, ConfigStr),

    % Handle population creation/cloning
    % With config:evolution_seed() set, the random choices of the new genotypes are reproducible (config:seed_rand/1).
    case Mode of
        fresh ->
            S = population_monitor:prep_PopState(PMP, Constraints),
            config:seed_rand(0),
            population_monitor:init_population(S, Constraints);
        new_evo ->
            % First run: fresh, subsequent: clone
            S = population_monitor:prep_PopState(PMP, Constraints),
            config:seed_rand(0),
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
                    % Trigger S3 upload only when another run is pending.
                    % For the final run, wrapper-driven finalization handles upload/termination.
                    trigger_s3_upload(P_Id),

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
                    record_validation_trial(NextPopId, U_RunIndex, ConfigStr),
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
    case supervised() of
        true ->
            %% A restored run keeps its experiment's limits and reports back to this loop (benchmarker_pid).
            population_monitor:start(population_monitor:prep_PopState(PMP, E#experiment.init_constraints));
        false ->
            population_monitor:continue(Population_Id)
    end,
    loop(U_E, Population_Id).

%% ============================================================================
%% CHECKPOINT SYSTEM
%% ============================================================================

%% True when a cloud worker (dxnn_worker.py) supervises this VM: GCP ("gcs") or OCI ("oci_object_storage").
%% The worker takes transactional Mnesia backups (dxnn_runtime) and uploads them itself.
supervised() ->
    case os:getenv("DXNN_STORAGE_PROVIDER") of
        "gcs" -> true;
        "oci_object_storage" -> true;
        _ -> false
    end.

%% Checkpoint configuration
should_checkpoint() ->
    %% Legacy file-copy checkpoints and S3 uploads stay on the AWS path.
    case supervised() of
        true -> false;
        false -> legacy_should_checkpoint()
    end.

legacy_should_checkpoint() ->
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
%% New format: population_id has embedded lineage with timestamp
%% Population: 2025-02-11T15-30-45Z_2026-03-09T15-30-45Z_a3f9_run1
%% Lineage: 2026-03-09T15-30-45Z_a3f9
extract_lineage_from_string(PopulationId) when is_list(PopulationId) ->
    % Split by underscore
    Parts = string:split(PopulationId, "_", all),
    case Parts of
        [_RunTimestamp, LineageTimestamp, LineageCode, _Run] ->
            % Reconstruct lineage: timestamp_code
            LineageTimestamp ++ "_" ++ LineageCode;
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
    case supervised() of
        true -> dxnn_runtime:finish();
        false -> legacy_checkpoint_and_exit()
    end.

legacy_checkpoint_and_exit() ->
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
