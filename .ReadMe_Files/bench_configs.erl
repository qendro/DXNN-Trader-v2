-module(bench_configs).
-export([get_run_configs/0]).

%% Reduced sweep: 38 runs (27 scaling + 11 spot checks)
get_run_configs() ->
    GT_End = config:gt_end(),
    BaseCommon = [
        {bench_start, config:bench_start()},
        {bench_end, config:bench_end()},
        {tuning_duration, config:tuning_duration()},
        {pli_resolutions, config:pli_resolutions()},
        {pci_vertical_resolutions, [20]}
    ],

    %% --- Scaling grid: pop size × GT duration × PCI H-res (baseline settings) ---
    PopSizes = [{50,50}, {300,300}, {500,500}],
    GT_Durations = [500, 2000, 5000],
    PCI_HRes = [60, 90, 180],
    ScalingVariants = [{SpecieLimit, InitSize, Dur, HRes} ||
        {SpecieLimit, InitSize} <- PopSizes,
        Dur <- GT_Durations,
        HRes <- PCI_HRes],
    ScalingRuns =
        [{Index,
          BaseCommon ++ [
              {population_id, list_to_binary(io_lib:format("bench_scale_~p", [Index]))},
              {connection_architecture, recurrent},
              {tuning_selection_functions, [dynamic]},
              {neural_plasticity_functions, [none]},
              {neural_aggregation_functions, [dot_product]},
              {substrate_plasticities, [none]},
              {substrate_linkforms, [l2l_feedforward]},
              {specie_size_limit, SpecieLimit},
              {init_specie_size, InitSize},
              {gt_end, GT_End},
              {gt_start, GT_End + Dur},
              {pci_horizontal_resolutions, [HRes]}
          ]}
         || {Index, {SpecieLimit, InitSize, Dur, HRes}} <-
                lists:zip(lists:seq(1, length(ScalingVariants)), ScalingVariants)],

    %% --- Spot checks for other knobs (short runs: 50 agents, Dur=500, HRes=60) ---
    SpotBase = BaseCommon ++ [
        {specie_size_limit, 50},
        {init_specie_size, 50},
        {gt_end, GT_End},
        {gt_start, GT_End + 500},
        {pci_horizontal_resolutions, [60]},
        {connection_architecture, recurrent},
        {tuning_selection_functions, [dynamic]},
        {neural_plasticity_functions, [none]},
        {neural_aggregation_functions, [dot_product]},
        {substrate_plasticities, [none]},
        {substrate_linkforms, [l2l_feedforward]}
    ],
    SpotVariants = [
        {connection_architecture, feedforward},
        {tuning_selection_functions, [active]},
        {tuning_selection_functions, [all_random]},
        {neural_plasticity_functions, [hebbian]},
        {neural_plasticity_functions, [self_modulationV3]},
        {neural_aggregation_functions, [mult_product]},
        {substrate_plasticities, [modular_none]},
        {substrate_plasticities, [abcn]},
        {substrate_linkforms, [jordan_recurrent]},
        {substrate_linkforms, [neuronself_recurrent]},
        {substrate_linkforms, [fully_interconnected]}
    ],
    SpotRuns =
        [{length(ScalingRuns) + I,
          SpotBase ++ [
              {population_id, list_to_binary(io_lib:format("bench_spot_~p", [I]))},
              Variant
          ]}
         || {I, Variant} <- lists:zip(lists:seq(1, length(SpotVariants)), SpotVariants)],

    ScalingRuns ++ SpotRuns.
