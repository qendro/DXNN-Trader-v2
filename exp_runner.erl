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
    io:format("exp_runner: generate_population_id/1 called, RunIndex=~p~n", [RunIndex]),
    io:format("exp_runner: generating lineage ID...~n"),
    LineageId = generate_lineage_id(),
    io:format("exp_runner: lineage ID generated: ~p~n", [LineageId]),
    generate_population_id_with_lineage(RunIndex, LineageId).

%% Generate population ID for cloned run (reuse lineage from source)
%% Only match if SourcePopId looks like a full population ID (longer than 4 chars, has underscores)
generate_population_id(RunIndex, SourcePopId) when is_binary(SourcePopId), byte_size(SourcePopId) > 20 ->
    io:format("exp_runner: generate_population_id/2 (binary source) called, RunIndex=~p, SourcePopId=~p~n", [RunIndex, SourcePopId]),
    LineageId = extract_lineage_id(SourcePopId),
    io:format("exp_runner: extracted LineageId=~p~n", [LineageId]),
    generate_population_id_with_lineage(RunIndex, LineageId).

%% Internal helper with lineage ID (4-char binary)
generate_population_id_with_lineage(RunIndex, LineageId) when is_binary(LineageId), byte_size(LineageId) =:= 4 ->
    io:format("exp_runner: generate_population_id_with_lineage called, RunIndex=~p, LineageId=~p~n", 
              [RunIndex, LineageId]),
    % Format: <<"<ISO8601>_<LineageId>_run<RunIndex>">>
    % Example: <<"2025-02-11T15-30-45Z_a3f9_run1">>
    io:format("exp_runner: formatting timestamp...~n"),
    Timestamp = format_iso8601(erlang:timestamp()),
    io:format("exp_runner: Timestamp=~p~n", [Timestamp]),
    RunIndexStr = integer_to_list(RunIndex),
    io:format("exp_runner: creating binary...~n"),
    Result = <<Timestamp/binary, "_", LineageId/binary, "_run", (list_to_binary(RunIndexStr))/binary>>,
    io:format("exp_runner: binary created: ~p~n", [Result]),
    Result.

%% Generate 4 random alphanumeric characters
generate_lineage_id() ->
    io:format("exp_runner: generate_lineage_id called~n"),
    Chars = "abcdefghijklmnopqrstuvwxyz0123456789",
    io:format("exp_runner: Chars defined, length=~p~n", [length(Chars)]),
    io:format("exp_runner: generating random chars...~n"),
    RandomChars = [lists:nth(random:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 4)],
    io:format("exp_runner: RandomChars=~p~n", [RandomChars]),
    Result = list_to_binary(RandomChars),
    io:format("exp_runner: lineage ID result=~p~n", [Result]),
    Result.

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
                innovation_factor = 0,
                pattern = []
            },
            mnesia:write(ResetAgent)
    end.

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
    io:format("exp_runner: config initialized~n"),
    
    % 2. Generate population ID first (auto-generated, includes lineage and run_index)
    % The first population ID becomes the experiment ID
    io:format("exp_runner: about to generate population ID, Mode=~p~n", [Mode]),
    {PopId, SourcePopId} = case Mode of
        fresh ->
            io:format("exp_runner: calling generate_population_id for fresh mode~n"),
            PopId1 = generate_population_id(1),
            io:format("exp_runner: generated PopId: ~p~n", [PopId1]),
            {PopId1, undefined};  % New lineage ID
        new_evo ->
            io:format("exp_runner: calling generate_population_id for new_evo mode~n"),
            PopId2 = generate_population_id(1),
            io:format("exp_runner: generated PopId: ~p~n", [PopId2]),
            {PopId2, undefined};  % Will be updated for subsequent runs
        {evo, SrcPopId} ->
            io:format("exp_runner: calling generate_population_id for evo mode, SourcePopId=~p~n", [SrcPopId]),
            PopId3 = generate_population_id(1, SrcPopId),
            io:format("exp_runner: generated PopId: ~p~n", [PopId3]),
            {PopId3, SrcPopId}  % Reuse lineage from source
    end,
    ExperimentId = PopId,  % Use first population ID as experiment ID
    io:format("exp_runner: population ID generated: ~p (also used as experiment ID)~n", [PopId]),
    
    % 3. Apply run configs
    apply_run_configs(ExperimentId, 1, RunConfigs),
    io:format("exp_runner: run configs applied~n"),
    
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
    io:format("exp_runner: PMP created~n"),
    
    % 5. Calculate total runs
    TotRuns = case RunConfigs of
        [] -> config:tot_runs();
        _ -> lists:max([RunIndex || {RunIndex, _} <- RunConfigs])
    end,
    io:format("exp_runner: total runs calculated: ~p~n", [TotRuns]),
    
    % 6. Create experiment record
    io:format("exp_runner: getting init constraints...~n"),
    InitConstraints = get_init_constraints(),
    io:format("exp_runner: init constraints obtained~n"),
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
    io:format("exp_runner: experiment record created~n"),
    
    % 7. Log experiment start
    qlog:exp_runner(experiment_start, {ExperimentId, PopId, TotRuns, length(RunConfigs)}),
    io:format("exp_runner: experiment start logged~n"),
    
    % 8. Store experiment record
    io:format("exp_runner: writing experiment to database...~n"),
    genotype:write(E),
    io:format("exp_runner: experiment written to database~n"),
    
    % 9. Spawn prep process (like benchmarker) and return immediately
    io:format("exp_runner: spawning prep process...~n"),
    ExpRunnerPid = spawn(exp_runner, prep, [E, Mode, SourcePopId]),
    io:format("exp_runner: prep process spawned: ~p~n", [ExpRunnerPid]),
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
    RandomChars = [lists:nth(random:uniform(length(Chars)), Chars) 
                   || _ <- lists:seq(1, 8)],
    list_to_atom("run_" ++ RandomChars).

%% ============================================================================
%% RUN CONFIG HELPERS
%% ============================================================================

%% Get run configs - customize this function with your experiment configs
%% Usage: exp_runner:start(fresh) - automatically uses these configs
get_run_configs() ->
    [
        {1, [{tuning_duration, {const,1}}, {gt_start, 5000}, {gt_end, 4000}, {specie_size_limit, 20}, {init_specie_size, 15}, {evaluations_limit, 100000000}, {generation_limit, 2}, {fitness_function, phase0_close_trades}]},
        {2, [{tuning_duration, {const,1}}, {gt_start, 8000}, {gt_end, 6000}, {specie_size_limit, 20}, {init_specie_size, 20}, {evaluations_limit, 100000000}, {generation_limit, 2}, {fitness_function, phase0_close_trades}]},
        {3, [{tuning_duration, {const,10}}, {gt_start, 4000}, {gt_end, 2000}, {specie_size_limit, 200}, {init_specie_size, 200}, {evaluations_limit, 100000000}, {generation_limit, 5}, {fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.40}, {fitness_phase1_tradescore_weight, 0.60}]},
        {4, [{tuning_duration, {const,10}}, {gt_start, 5000}, {gt_end, 2500}, {specie_size_limit, 200}, {init_specie_size, 200}, {evaluations_limit, 200000000}, {generation_limit, 5}, {fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.50}, {fitness_phase1_tradescore_weight, 0.50}]},
        {5, [{tuning_duration, {const,10}}, {gt_start, 5000}, {gt_end, 2500}, {specie_size_limit, 200}, {init_specie_size, 200}, {evaluations_limit, 200000000}, {generation_limit, 5}, {fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.60}, {fitness_phase1_tradescore_weight, 0.40}]},
        {6, [{tuning_duration, {const,10}}, {gt_start, 5000}, {gt_end, 2500}, {specie_size_limit, 200}, {init_specie_size, 200}, {evaluations_limit, 200000000}, {generation_limit, 5}, {fitness_function, phase1_profit_risk}, {fitness_phase1_pscore_weight, 0.70}, {fitness_phase1_tradescore_weight, 0.30}]}
    ].

%% Prep function (similar to benchmarker:prep)
prep(E, Mode, SourcePopId) ->
    % Ensure config is applied for this run BEFORE reading config values
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
            % Reuse population from source
            case reuse_population_for_next_run(SourcePopId, Population_Id) of
                {atomic, _} ->
                    % Population already exists, just start monitor
                    S = population_monitor:prep_PopState(PMP, Constraints),
                    population_monitor:start(S);
                Error ->
                    io:format("Error reusing population: ~p~n", [Error]),
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
    receive
        {P_Id, completed, Trace} ->
            U_TraceAcc = [Trace | E#experiment.trace_acc],
            U_RunIndex = E#experiment.run_index + 1,
            
            % Log run end (stats extracted directly in qlog:exp_runner)
            qlog:exp_runner(run_end, {E#experiment.id, E#experiment.run_index, P_Id, Trace}),
            qlog:benchmarker(P_Id,io_lib:format("Run End: Experiment id: ~p Run Index: ~p/~p", [E#experiment.id, E#experiment.run_index, E#experiment.tot_runs])),
            
            case U_RunIndex > E#experiment.tot_runs of
                true ->
                    % All runs completed
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
                    io:format("Experiment ~p completed with ~p runs~n", [E#experiment.id, U_RunIndex - 1]);
                false ->
                    % Continue to next run
                    apply_run_configs(E#experiment.id, U_RunIndex, E#experiment.run_configs),
                    
                    % Generate new population ID for next run (reuse lineage from current)
                    NextPopId = generate_population_id(U_RunIndex, P_Id),
                    
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
                    
                    % Log next run start
                    ConfigStr = format_config_summary(U_RunIndex, E#experiment.run_configs),
                    qlog:exp_runner(run_start, {E#experiment.id, U_RunIndex, NextPopId, new_evo, ConfigStr}),
                    qlog:benchmarker(P_Id, io_lib:format("Run Start: Experiment id: ~p Run Index: ~p, NextPopId: ~p", [E#experiment.id, U_RunIndex, NextPopId])),
                    
                    % Reuse population for next run (new_evo mode reuses from previous run)
                    case reuse_population_for_next_run(P_Id, NextPopId) of
                        {atomic, _} ->
                            io:format("Successfully reused population from ~p to ~p~n", [P_Id, NextPopId]),
                            qlog:benchmarker(P_Id, io_lib:format("Successfully reused population from ~p to ~p", [P_Id, NextPopId])),
                            Constraints = U_E#experiment.init_constraints,
                            % Population already exists, just start monitor
                            S = population_monitor:prep_PopState(U_PMP, Constraints),
                            population_monitor:start(S),
                            loop(U_E, NextPopId);
                        Error ->
                            io:format("Error reusing population from ~p to ~p: ~p~n", [P_Id, NextPopId, Error]),
                            qlog:exp_runner(run_failed, {E#experiment.id, U_RunIndex, {reuse_failed, Error}}),
                            qlog:benchmarker(P_Id, io_lib:format("Error reusing population from ~p to ~p: ~p", [P_Id, NextPopId, Error])),
                            Error
                    end
            end;
        {P_Id, failed, Reason} ->
            qlog:exp_runner(run_failed, {E#experiment.id, E#experiment.run_index, Reason}),
            io:format("Run ~p failed for experiment ~p: ~p~n", [E#experiment.run_index, E#experiment.id, Reason]),
            qlog:benchmarker(P_Id, io_lib:format("Run ~p failed for experiment ~p: ~p", [E#experiment.run_index, E#experiment.id, Reason])),
            ok;
        terminate ->
            qlog:exp_runner(experiment_terminate, {E#experiment.id, P_Id}),
            qlog:benchmarker(P_Id, io_lib:format("Experiment ~p terminated by request", [E#experiment.id])),
            ok
    end.


%% Continue an existing experiment (similar to benchmarker:continue)
continue(ExperimentId) ->
    case genotype:dirty_read({experiment, ExperimentId}) of
        undefined ->
            qlog:benchmarker(io_lib:format("Can't continue experiment ~p, it's not present in the database.", [ExperimentId])),
            io:format("Can't continue experiment ~p, it's not present in the database.~n", [ExperimentId]);
        E ->
            case E#experiment.progress_flag of
                completed ->
                    qlog:benchmarker(io_lib:format("Experiment ~p already completed.", [ExperimentId])),
                    io:format("Experiment ~p already completed: ~p~n", [ExperimentId, E#experiment.trace_acc]);
                in_progress ->
                    config:init(),
                    CurrentRunIndex = E#experiment.run_index,
                    apply_run_configs(ExperimentId, CurrentRunIndex, E#experiment.run_configs),
                    Interruptions = E#experiment.interruptions,
                    U_Interruptions = [erlang:timestamp() | Interruptions],
                    U_E = E#experiment{
                        interruptions = U_Interruptions
                    },
                    genotype:write(U_E),
                    
                    % Determine mode from experiment state
                    % For now, assume new_evo mode (can be enhanced later)
                    Mode = new_evo,
                    SourcePopId = undefined,  % Will be determined from previous run if needed
                    
                    register(exp_runner, spawn(exp_runner, prep, [U_E, Mode, SourcePopId]))
            end
    end.

