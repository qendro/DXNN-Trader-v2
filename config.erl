%% Configuration file for DXNN experiments
%% Change these values to run different experiment configurations

-module(config).
-compile(export_all).

%% Get configuration value with default fallback
get(Key, Default) ->
    case Key of
        fx_table -> 'EURUSD1';%%'EURUSD15'; 'EURUSD1';
        specie_size_limit -> 100;
        init_specie_size -> 100;
        evaluations_limit -> 10000;
        tot_runs -> 3;
        morphology -> forex_trader;
        _ -> Default
    end.
