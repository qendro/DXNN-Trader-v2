#!/usr/bin/env escript
%% Script to print the best genotype from Mnesia database

main([]) ->
    main(["test"]);
main([Population_Id_Str]) ->
    % Convert string to atom
    Population_Id = case Population_Id_Str of
        "all" -> all;
        _ -> list_to_atom(Population_Id_Str)
    end,
    
    io:format("Starting Mnesia and looking for best genotype in population: ~p~n", [Population_Id]),
    
    % Start Mnesia
    mnesia:start(),
    
    % Add current directory to code path
    code:add_path("."),
    
    % Compile the genotype_utils module
    case compile:file(genotype_utils, []) of
        {ok, _} ->
            io:format("Compiled genotype_utils module successfully~n");
        Error ->
            io:format("Failed to compile genotype_utils: ~p~n", [Error]),
            halt(1)
    end,
    
    % Try to find and print the best genotype
    try
        genotype_utils:print_best_genotype(Population_Id)
    catch
        Error:Reason ->
            io:format("Error occurred: ~p:~p~n", [Error, Reason]),
            io:format("~nTrying to list all populations...~n"),
            try
                Populations = mnesia:dirty_all_keys(population),
                io:format("Available populations: ~p~n", [Populations])
            catch
                _:_ ->
                    io:format("Could not access population table~n")
            end
    end,
    
    % Stop Mnesia
    mnesia:stop().
