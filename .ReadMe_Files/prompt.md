You are an expert in Erlang, and HyperNeat and NEAT Neural Networks. Review the current workspace (HyperNEAT trading system) @.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md , @.ReadMe_Files/AI_README.md and review the entire workspace.

I want to create a prompt for an agent to implement the following functionality in Erlang help me structure it and then we can rewrite it. 

I want to implment a new module and process that is manually called when my program is running and logs the following using qlog.erl under the Benchmarker folder under logs. It should have functionality to do the following: using prcess_info it should list all processes and their current memory usage. It should also sort them by high to low. It should also only print X if not all are required. It should do this for message queue as well to check process mailbox message queue. 
So one function for messages and one for memory. It should log with each process per line or all the processes on one line in the log file as directed by the user. 
So it should look something like this:
Process ID: <pid> Type of Element: <type> (if known such as Cortex, Sensor, Scape etc) Current Function: <function name>
Process ID: <pid> Type of Element: <type> (if known such as Cortex, Sensor, Scape etc) Current Function: <function name>

or Process ID: <pid> Type of Element: <type> (if known such as Cortex, Sensor, Scape etc) Current Function: <function name> | Process ID: <pid> Type of Element: <type> (if known such as Cortex, Sensor, Scape etc) Current Function: <function name>

Same for memory. 

It should also do both memory and message queue in one function call that lists both memory and message queue length for each process sorted by either memory or message queue length.


 cpu usage, message queue length, and current function call. It should log this information every X seconds (configurable) to a log file using qlog.erl.



Find the root cause of this issue. and provide proposed code fix that actually address the rootcause and IS NOT a bandaid. 



I want to provide me a new get_run_config as show below in @benchmarker.erl. The intent is to cycle through different configurations to 1. ensure that the configuration works and through mutations 2. provide a time impact on that specific configuration item as we scale through time-steps and through sensor length as it relates to time to complete. 

Assume all config parameters are fixed as currently set and we want to only cycle through these parameters.

- connection_architecture() feedforward, recurrent
- tuning_selection_functions() -> pick 3
- neural_plasticity_functions() -> pick 3
neural_aggregation_functions() -> pick 2 extremes for computation
- substrate_plasticities() -> pick 3 one must be none, iterative is not an option

substrate_linkforms() -> pick 4 one must be l2l_feedforward

Number of agents = max 50, max 300, max 500
gt duration = 500, 2000, 5000
PCI will only be used with vertical resolution of 20
PCI horizontal resolution = 60, 90, 180

Everything else remains constant.

Tell me how many runs I will have to do and provide me the copy and paste config to put in benchmareker.erl. 


PCI Sensor
Neural vs. substrate 



get_run_configs() ->
	%% Example: [{1, [{population_id, test}, {evaluations_limit, 10000000}, {generation_limit, 1}, {specie_size_limit, 2}, {init_specie_size, 2}, {survival_percentage, 0.5}, {account_leverage, 50}, {account_initial_balance, 300}, {account_lot_size, 10000}, {account_margin, 0}, {account_spread, 0.000150}, {primary_currency_pair, 'EURUSD1'}, {gt_start, 1000}, {gt_end, 200}, {bench_start, 200}, {bench_end, last}, {morphology, forex_trader}, {connection_architecture, recurrent}]}],
	[{1, [{population_id, test}, {tuning_duration, {const,1}}, {gt_start, 5000}, {gt_end, 4500}, {specie_size_limit, 300}, {init_specie_size, 100}, {evaluations_limit, 10000000}, {generation_limit, 5}]},
	{2, [{population_id, test2}, {tuning_duration, {const,1}}, {gt_start, 4000}, {gt_end, 2500}, {specie_size_limit, 500}, {init_specie_size, 250}, {evaluations_limit, 20000000}, {generation_limit, 5}]}].




I want you to implement:

Code should be clean, consice, effective and efficient. All one time code or test code should be in a seperate module marked "DELETE" so we can delet it later. 

All logging should be done using qlog.erl (feel free to modify qlog.erl as you wish). However all log calls qlog:... should be one line so that we can comment them out later. All comments should be concise and limited to 1 line if needed (if possible). 

If you have any questions or unsure about something ask me first. 


You are an expert in Erlang, Python, and Elixir. Review the current workspace (HyperNEAT trading system) @.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md , @.ReadMe_Files/AI_README.md and review the entire workspace. I am considering creating a new benchmarker.erl. currenlty benchmarker.erl Does multiple runs of the same instance for statistical purposes. Moving forward I want bench marker to do the following. I want it to run X number of runs, but each run would be different and built upon the previous. 

I essentially want to be able to run multiple runs with a different config.erl file. or load different variables in config.erl 

The second phase of this is instead of having my program spawn a new set of agents each time it takes the saved agents from the previous run and runs those on the next run. 

You are an expert in Erlang, Python, and Elixir. Review the current workspace (HyperNEAT trading system) @.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md , @.ReadMe_Files/AI_README.md and review the entire workspace. Review these implementation plans and provide me your findings. 


I think what we need to do is after we spawn and prep the sensor, neuron and actuator instead of sending them to the main loop we send them to a connectivity_check loop which once you recieve 1 connectivity_check you pass a connectivity_check to all your Fanout_PIds and move to the main loop. The concern I have is the garbage collection of messages from all the other inputs where you might get hundreds or thousands connectivity_checks after going to the main loop. 



You are an expert in Erlang, Python, and HyperNeat Neural Networks. Review the current workspace (HyperNEAT trading system) @.ReadMe_Files/HYPERNEAT_SYSTEM_GUIDE.md , @.ReadMe_Files/AI_README.md and review the entire workspace.

I want you to implement @.ReadMe_Files/PROCESS_MONITOR_IMPLEMENTATION_PLAN.md 

Code should be clean, consice, effective and efficient. All one time code or test code should be in a seperate module marked "DELETE" so we can delet it later. 

All logging should be done using qlog.erl (feel free to modify qlog.erl as you wish). However all log calls qlog:... should be one line so that we can comment them out later. All comments should be concise and limited to 1 line if needed (if possible). 

Code changes should be limited to the new module and modification to qlog.erl
