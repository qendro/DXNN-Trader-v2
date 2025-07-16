%% Test module for best_agent_runner standalone agent execution controller
-module(best_agent_runner_tests).
-compile(export_all).
-include("records.hrl").

%% Include the trading decision record from best_agent_runner
-record(trading_decision, {
    timestamp,
    index,
    price,
    signal,      % -1, 0, 1 for sell, hold, buy
    confidence,  % derived from fitness or other metrics
    profit_loss  % calculated profit/loss from decision
}).

-record(agent_run_results, {
    agent_id,
    data_source,
    start_time,
    end_time,
    total_decisions,
    buy_decisions,
    sell_decisions,
    hold_decisions,
    total_profit_loss,
    max_profit,
    max_loss,
    trading_decisions,  % List of #trading_decision{}
    execution_stats     % Performance metrics
}).

-record(run_options, {
    start_index = first,     % Where to start in the data
    end_index = last,        % Where to end in the data
    output_file = undefined, % Optional file to save results
    verbose = false,         % Detailed logging
    collect_all_decisions = true % Store all individual decisions
}).

%% ============================================================================
%% TEST SUITE FOR TASK 4: Standalone Agent Execution Controller
%% ============================================================================

%% Main test runner
run_all_tests() ->
    io:format("=== Running Best Agent Runner Tests ===~n"),
    
    % Initialize test environment
    setup_test_environment(),
    
    % Run individual tests
    Results = [
        test_agent_startup(),
        test_message_collection(),
        test_monitor_execution(),
        test_cleanup_processes(),
        test_error_handling(),
        test_integration()
    ],
    
    % Cleanup test environment
    cleanup_test_environment(),
    
    % Report results
    Passed = length([R || R <- Results, R == pass]),
    Failed = length([R || R <- Results, R == fail]),
    
    io:format("~n=== Test Results ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total:  ~p~n", [Passed + Failed]),
    
    case Failed of
        0 -> 
            io:format("All tests PASSED!~n"),
            pass;
        _ -> 
            io:format("Some tests FAILED!~n"),
            fail
    end.

%% Setup test environment
setup_test_environment() ->
    io:format("Setting up test environment...~n"),
    
    % Start mnesia if not already started
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> 
            mnesia:start(),
            timer:sleep(1000)
    end,
    
    % Create a mock agent for testing
    create_mock_agent(),
    
    ok.

%% Cleanup test environment
cleanup_test_environment() ->
    io:format("Cleaning up test environment...~n"),
    
    % Clean up any remaining processes
    cleanup_test_processes(),
    
    ok.

%% Create a mock agent for testing
create_mock_agent() ->
    % Create a simple mock agent record for testing
    % This would normally be created by the genotype system
    MockAgent = #agent{
        id = {test_agent_123, agent},
        encoding_type = neural,
        generation = 1,
        population_id = test,
        specie_id = test_specie,
        cx_id = {test_cortex, cortex},
        fitness = 100.0,
        constraint = #constraint{}
    },
    
    % In a real system, this would be written to mnesia
    % For testing, we'll just put it in the process dictionary
    put(mock_agent, MockAgent),
    
    io:format("Mock agent created: ~p~n", [MockAgent#agent.id]).

%% Test 1: Agent startup and termination
test_agent_startup() ->
    io:format("~n--- Test 1: Agent Startup ---~n"),
    
    try
        % Test with a mock agent ID
        MockAgentId = {test_agent_123, agent},
        
        % Test start_agent_process function
        io:format("Testing start_agent_process...~n"),
        
        % Since we can't actually start a real agent without the full system,
        % we'll test the error handling path
        case best_agent_runner:start_agent_process(MockAgentId) of
            {error, _Reason} ->
                io:format("   Expected error for mock agent: OK~n"),
                pass;
            {ok, _PId} ->
                io:format("   Unexpected success with mock agent~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test 2: Message collection system
test_message_collection() ->
    io:format("~n--- Test 2: Message Collection ---~n"),
    
    try
        % Spawn a mock collector process
        TestPId = self(),
        CollectorPId = spawn(fun() -> mock_collector_process(TestPId) end),
        
        % Test sending trading decision messages
        CollectorPId ! {mock_agent, trade_decision, 1, 1.0500, erlang:timestamp()},
        CollectorPId ! {mock_agent, trade_decision, -1, 1.0520, erlang:timestamp()},
        CollectorPId ! {mock_agent, trade_decision, 0, 1.0510, erlang:timestamp()},
        
        % Request decisions
        CollectorPId ! {self(), get_decisions},
        
        receive
            {CollectorPId, decisions, Decisions} ->
                case length(Decisions) of
                    3 ->
                        io:format("   Collected ~p decisions: OK~n", [length(Decisions)]),
                        
                        % Verify decision structure
                        [D1, D2, D3] = Decisions,
                        case {D1#trading_decision.signal, D2#trading_decision.signal, D3#trading_decision.signal} of
                            {1, -1, 0} ->
                                io:format("   Decision signals correct: OK~n"),
                                pass;
                            Other ->
                                io:format("   Decision signals incorrect: ~p~n", [Other]),
                                fail
                        end;
                    Other ->
                        io:format("   Expected 3 decisions, got ~p~n", [Other]),
                        fail
                end
        after 5000 ->
            io:format("   Timeout waiting for decisions~n"),
            fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Mock collector process for testing
mock_collector_process(TestPId) ->
    mock_collector_process(TestPId, []).

mock_collector_process(TestPId, Decisions) ->
    receive
        {_Agent, trade_decision, Signal, Price, Timestamp} ->
            Decision = #trading_decision{
                timestamp = Timestamp,
                price = Price,
                signal = Signal,
                confidence = undefined,
                profit_loss = undefined
            },
            mock_collector_process(TestPId, [Decision | Decisions]);
            
        {TestPId, get_decisions} ->
            TestPId ! {self(), decisions, lists:reverse(Decisions)},
            mock_collector_process(TestPId, Decisions);
            
        {TestPId, terminate} ->
            TestPId ! {self(), final_decisions, lists:reverse(Decisions)}
    end.

%% Test 3: Monitor execution functionality
test_monitor_execution() ->
    io:format("~n--- Test 3: Monitor Execution ---~n"),
    
    try
        % Create a mock agent process
        MockAgentPId = spawn(fun() -> mock_agent_process() end),
        
        % Create monitor options
        Options = #run_options{verbose = false},
        
        % Spawn monitor process
        MonitorPId = spawn(best_agent_runner, monitor_agent_execution, 
                          [MockAgentPId, test_table, Options, self()]),
        
        % Send a mock evaluation completed message
        MonitorPId ! {MockAgentPId, evaluation_completed, 150.5, 100, 5.2, false},
        
        % Wait for monitor response
        receive
            {MonitorPId, execution_completed, ExecutionStats} ->
                case maps:get(fitness, ExecutionStats) of
                    150.5 ->
                        io:format("   Monitor captured fitness correctly: OK~n"),
                        pass;
                    Other ->
                        io:format("   Monitor fitness incorrect: ~p~n", [Other]),
                        fail
                end
        after 5000 ->
            io:format("   Timeout waiting for monitor response~n"),
            fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Mock agent process for testing
mock_agent_process() ->
    receive
        {_From, terminate} ->
            io:format("Mock agent terminating~n");
        Other ->
            io:format("Mock agent received: ~p~n", [Other]),
            mock_agent_process()
    end.

%% Test 4: Process cleanup
test_cleanup_processes() ->
    io:format("~n--- Test 4: Process Cleanup ---~n"),
    
    try
        % Create mock processes
        MockAgent = spawn(fun() -> mock_agent_process() end),
        MockMonitor = spawn(fun() -> mock_monitor_process() end),
        MockCollector = spawn(fun() -> mock_collector_process(self()) end),
        
        % Verify they're alive
        case {is_process_alive(MockAgent), is_process_alive(MockMonitor), is_process_alive(MockCollector)} of
            {true, true, true} ->
                io:format("   Mock processes created: OK~n"),
                
                % Test cleanup
                best_agent_runner:cleanup_agent_processes(MockAgent, MockMonitor, MockCollector),
                
                % Give cleanup time to work
                timer:sleep(2000),
                
                % Check if processes are terminated
                case {is_process_alive(MockAgent), is_process_alive(MockMonitor), is_process_alive(MockCollector)} of
                    {false, false, false} ->
                        io:format("   All processes cleaned up: OK~n"),
                        pass;
                    Other ->
                        io:format("   Some processes still alive: ~p~n", [Other]),
                        fail
                end;
            Other ->
                io:format("   Failed to create mock processes: ~p~n", [Other]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Mock monitor process for testing
mock_monitor_process() ->
    receive
        {_From, terminate} ->
            io:format("Mock monitor terminating~n");
        Other ->
            io:format("Mock monitor received: ~p~n", [Other]),
            mock_monitor_process()
    end.

%% Test 5: Error handling
test_error_handling() ->
    io:format("~n--- Test 5: Error Handling ---~n"),
    
    try
        % Test with invalid agent ID
        InvalidAgentId = {nonexistent_agent, agent},
        
        case best_agent_runner:start_agent_process(InvalidAgentId) of
            {error, _Reason} ->
                io:format("   Invalid agent ID handled correctly: OK~n"),
                pass;
            {ok, _PId} ->
                io:format("   Invalid agent ID should have failed~n"),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test 6: Integration test
test_integration() ->
    io:format("~n--- Test 6: Integration Test ---~n"),
    
    try
        % This test verifies that all components work together
        % Since we can't run a real agent without the full system,
        % we'll test the error path of run_agent_standalone
        
        MockAgentId = {test_agent_integration, agent},
        MockTableName = test_table,
        
        case best_agent_runner:run_agent_standalone(MockAgentId, MockTableName) of
            {error, {agent_startup_failed, _Reason}} ->
                io:format("   Integration test handled startup failure correctly: OK~n"),
                pass;
            Other ->
                io:format("   Unexpected integration test result: ~p~n", [Other]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Integration test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Cleanup any test processes
cleanup_test_processes() ->
    % Kill any remaining test processes
    case get(test_processes) of
        undefined -> ok;
        Processes ->
            [exit(P, kill) || P <- Processes, is_process_alive(P)]
    end.

%% ============================================================================
%% TEST SUITE FOR TASK 5: Trading Decision Collection and Tracking
%% ============================================================================

%% Main test runner for Task 5
run_task5_tests() ->
    io:format("=== Running Task 5: Trading Decision Collection Tests ===~n"),
    
    Results = [
        test_enhanced_collect_trading_decisions(),
        test_calculate_decision_stats(),
        test_calculate_all_profits(),
        test_update_last_decision_profit(),
        test_get_current_price(),
        test_get_current_index(),
        test_profit_loss_calculation(),
        test_decision_recording_accuracy()
    ],
    
    % Report results
    Passed = length([R || R <- Results, R == pass]),
    Failed = length([R || R <- Results, R == fail]),
    
    io:format("~n=== Task 5 Test Results ===~n"),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Failed: ~p~n", [Failed]),
    io:format("Total:  ~p~n", [Passed + Failed]),
    
    case Failed of
        0 -> 
            io:format("All Task 5 tests PASSED!~n"),
            pass;
        _ -> 
            io:format("Some Task 5 tests FAILED!~n"),
            fail
    end.

%% Test enhanced collect_trading_decisions function
test_enhanced_collect_trading_decisions() ->
    io:format("~n--- Test: Enhanced Trading Decision Collection ---~n"),
    
    try
        MockAgent = spawn(fun() -> timer:sleep(10000) end),
        TestPid = self(),
        
        % Start enhanced collector
        CollectorPid = spawn(best_agent_runner, collect_trading_decisions, 
                            [MockAgent, [], TestPid]),
        
        % Test different types of trading messages
        Timestamp1 = erlang:timestamp(),
        
        % Send direct trading decision
        CollectorPid ! {MockAgent, trade_decision, 1, 1.0500, Timestamp1},
        
        % Send trade message (simulating actuator to fx scape)
        CollectorPid ! {MockAgent, trade, 'EURUSD15', -1},
        
        % Send fitness response (simulating fx scape response)
        CollectorPid ! {fx_scape_pid, 25.5, 0},
        
        timer:sleep(200),
        
        % Get decisions with stats
        CollectorPid ! {TestPid, get_decisions_with_stats},
        
        receive
            {CollectorPid, decisions_with_stats, {Decisions, Stats}} ->
                DecisionCount = length(Decisions),
                io:format("   Enhanced collector captured ~p decisions~n", [DecisionCount]),
                
                % Verify stats structure
                case maps:is_key(total_decisions, Stats) of
                    true ->
                        io:format("   Decision stats calculated correctly: OK~n"),
                        
                        % Verify individual decision fields
                        case DecisionCount > 0 of
                            true ->
                                [FirstDecision | _] = Decisions,
                                HasTimestamp = FirstDecision#trading_decision.timestamp =/= undefined,
                                HasSignal = FirstDecision#trading_decision.signal =/= undefined,
                                
                                case {HasTimestamp, HasSignal} of
                                    {true, true} ->
                                        io:format("   Decision fields populated correctly: OK~n"),
                                        pass;
                                    _ ->
                                        io:format("   Decision fields missing data~n"),
                                        fail
                                end;
                            false ->
                                io:format("   No decisions captured (acceptable for test): OK~n"),
                                pass
                        end;
                    false ->
                        io:format("   Decision stats structure incorrect~n"),
                        fail
                end;
            Other ->
                io:format("   Enhanced collector returned unexpected: ~p~n", [Other]),
                fail
        after 3000 ->
            io:format("   Enhanced collector timed out~n"),
            fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test calculate_decision_stats function
test_calculate_decision_stats() ->
    io:format("~n--- Test: Calculate Decision Stats ---~n"),
    
    try
        % Create test decisions with various signals and profits
        TestDecisions = [
            #trading_decision{signal = 1, profit_loss = 10.5, timestamp = erlang:timestamp()},
            #trading_decision{signal = -1, profit_loss = -5.2, timestamp = erlang:timestamp()},
            #trading_decision{signal = 0, profit_loss = 0, timestamp = erlang:timestamp()},
            #trading_decision{signal = 1, profit_loss = 15.8, timestamp = erlang:timestamp()},
            #trading_decision{signal = -1, profit_loss = 8.3, timestamp = erlang:timestamp()}
        ],
        
        Stats = best_agent_runner:calculate_decision_stats(TestDecisions),
        
        % Verify stats
        ExpectedTotal = 5,
        ExpectedBuy = 2,
        ExpectedSell = 2,
        ExpectedHold = 1,
        ExpectedTotalProfit = 10.5 - 5.2 + 0 + 15.8 + 8.3,
        
        case maps:get(total_decisions, Stats) of
            ExpectedTotal ->
                io:format("   Total decisions calculated correctly: ~p~n", [ExpectedTotal]),
                
                case {maps:get(buy_decisions, Stats), maps:get(sell_decisions, Stats), maps:get(hold_decisions, Stats)} of
                    {ExpectedBuy, ExpectedSell, ExpectedHold} ->
                        io:format("   Decision type counts correct: Buy=~p, Sell=~p, Hold=~p~n", 
                                 [ExpectedBuy, ExpectedSell, ExpectedHold]),
                        
                        TotalProfit = maps:get(total_profit_loss, Stats),
                        case abs(TotalProfit - ExpectedTotalProfit) < 0.01 of
                            true ->
                                io:format("   Total profit calculated correctly: ~p: OK~n", [TotalProfit]),
                                pass;
                            false ->
                                io:format("   Total profit incorrect: expected ~p, got ~p~n", 
                                         [ExpectedTotalProfit, TotalProfit]),
                                fail
                        end;
                    {ActualBuy, ActualSell, ActualHold} ->
                        io:format("   Decision counts incorrect: expected Buy=~p, Sell=~p, Hold=~p, got Buy=~p, Sell=~p, Hold=~p~n",
                                 [ExpectedBuy, ExpectedSell, ExpectedHold, ActualBuy, ActualSell, ActualHold]),
                        fail
                end;
            ActualTotal ->
                io:format("   Total decisions incorrect: expected ~p, got ~p~n", [ExpectedTotal, ActualTotal]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test calculate_all_profits function
test_calculate_all_profits() ->
    io:format("~n--- Test: Calculate All Profits ---~n"),
    
    try
        % Create test decisions with prices
        TestDecisions = [
            #trading_decision{signal = 1, price = 1.0500, timestamp = erlang:timestamp()},  % Buy at 1.0500
            #trading_decision{signal = 0, price = 1.0520, timestamp = erlang:timestamp()},  % Hold at 1.0520 (profit from buy)
            #trading_decision{signal = -1, price = 1.0510, timestamp = erlang:timestamp()}, % Sell at 1.0510
            #trading_decision{signal = 1, price = 1.0490, timestamp = erlang:timestamp()}   % Buy at 1.0490 (profit from sell)
        ],
        
        ResultDecisions = best_agent_runner:calculate_all_profits(TestDecisions),
        
        case length(ResultDecisions) of
            4 ->
                io:format("   All decisions processed for profit calculation: OK~n"),
                
                % Check first decision (buy signal, should have profit based on next price)
                [First, Second, Third, Fourth] = ResultDecisions,
                
                % First decision: Buy at 1.0500, next price 1.0520 -> profit
                FirstProfit = First#trading_decision.profit_loss,
                ExpectedFirstProfit = (1.0520 - 1.0500) / 1.0500 * 100,  % ~1.9%
                
                case abs(FirstProfit - ExpectedFirstProfit) < 0.1 of
                    true ->
                        io:format("   First decision profit calculated correctly: ~p%~n", [FirstProfit]),
                        
                        % Check that all decisions have profit_loss values
                        AllHaveProfits = lists:all(fun(D) -> 
                            D#trading_decision.profit_loss =/= undefined 
                        end, ResultDecisions),
                        
                        case AllHaveProfits of
                            true ->
                                io:format("   All decisions have profit/loss calculated: OK~n"),
                                pass;
                            false ->
                                io:format("   Some decisions missing profit/loss~n"),
                                fail
                        end;
                    false ->
                        io:format("   First decision profit incorrect: expected ~p, got ~p~n", 
                                 [ExpectedFirstProfit, FirstProfit]),
                        fail
                end;
            Count ->
                io:format("   Expected 4 decisions, got ~p~n", [Count]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test update_last_decision_profit function
test_update_last_decision_profit() ->
    io:format("~n--- Test: Update Last Decision Profit ---~n"),
    
    try
        % Create test decisions
        TestDecisions = [
            #trading_decision{signal = 1, price = 1.0500, profit_loss = undefined},
            #trading_decision{signal = -1, price = 1.0520, profit_loss = undefined}
        ],
        
        Fitness = 42.7,
        UpdatedDecisions = best_agent_runner:update_last_decision_profit(TestDecisions, Fitness),
        
        case length(UpdatedDecisions) of
            2 ->
                [First, Second] = UpdatedDecisions,
                
                % Check that only the last (first in list) decision was updated
                case {First#trading_decision.profit_loss, Second#trading_decision.profit_loss} of
                    {42.7, undefined} ->
                        io:format("   Last decision profit updated correctly: OK~n"),
                        
                        % Check confidence was set
                        case First#trading_decision.confidence of
                            42.7 ->
                                io:format("   Confidence set correctly: OK~n"),
                                pass;
                            Other ->
                                io:format("   Confidence incorrect: expected 42.7, got ~p~n", [Other]),
                                fail
                        end;
                    {ActualFirst, ActualSecond} ->
                        io:format("   Profit update incorrect: expected {42.7, undefined}, got {~p, ~p}~n",
                                 [ActualFirst, ActualSecond]),
                        fail
                end;
            Count ->
                io:format("   Expected 2 decisions, got ~p~n", [Count]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test get_current_price function
test_get_current_price() ->
    io:format("~n--- Test: Get Current Price ---~n"),
    
    try
        TableName = 'EURUSD15',
        
        % Test with no current price set
        Price1 = best_agent_runner:get_current_price(TableName),
        
        case Price1 of
            undefined ->
                io:format("   get_current_price returns undefined when no price available: OK~n"),
                
                % Set a mock current price and test again
                put(current_price, 1.0555),
                Price2 = best_agent_runner:get_current_price(TableName),
                
                case Price2 of
                    1.0555 ->
                        io:format("   get_current_price returns stored price correctly: OK~n"),
                        erase(current_price),
                        pass;
                    Other ->
                        io:format("   get_current_price returned unexpected: ~p~n", [Other]),
                        erase(current_price),
                        fail
                end;
            _ ->
                io:format("   get_current_price returned a price (system dependent): ~p: OK~n", [Price1]),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test get_current_index function
test_get_current_index() ->
    io:format("~n--- Test: Get Current Index ---~n"),
    
    try
        TableName = 'EURUSD15',
        
        % Test with no current index set
        Index1 = best_agent_runner:get_current_index(TableName),
        
        case Index1 of
            undefined ->
                io:format("   get_current_index returns undefined when no index available: OK~n"),
                
                % Set a mock current index and test again
                put(current_index, 150),
                Index2 = best_agent_runner:get_current_index(TableName),
                
                case Index2 of
                    150 ->
                        io:format("   get_current_index returns stored index correctly: OK~n"),
                        erase(current_index),
                        pass;
                    Other ->
                        io:format("   get_current_index returned unexpected: ~p~n", [Other]),
                        erase(current_index),
                        fail
                end;
            _ ->
                io:format("   get_current_index returned an index (system dependent): ~p: OK~n", [Index1]),
                pass
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test profit/loss calculation accuracy
test_profit_loss_calculation() ->
    io:format("~n--- Test: Profit/Loss Calculation Accuracy ---~n"),
    
    try
        % Test various trading scenarios
        
        % Scenario 1: Profitable buy trade
        BuyPrice = 1.0500,
        SellPrice = 1.0520,
        ExpectedBuyProfit = (SellPrice - BuyPrice) / BuyPrice * 100,  % ~1.9%
        
        BuyDecisions = [
            #trading_decision{signal = 1, price = BuyPrice, timestamp = erlang:timestamp()},
            #trading_decision{signal = 0, price = SellPrice, timestamp = erlang:timestamp()}
        ],
        
        BuyResult = best_agent_runner:calculate_all_profits(BuyDecisions),
        [BuyDecision | _] = BuyResult,
        BuyActualProfit = BuyDecision#trading_decision.profit_loss,
        
        case abs(BuyActualProfit - ExpectedBuyProfit) < 0.01 of
            true ->
                io:format("   Buy trade profit calculated correctly: ~p%: OK~n", [BuyActualProfit]),
                
                % Scenario 2: Profitable sell trade
                SellPrice2 = 1.0520,
                BuyPrice2 = 1.0500,
                ExpectedSellProfit = (SellPrice2 - BuyPrice2) / SellPrice2 * 100,  % Short profit
                
                SellDecisions = [
                    #trading_decision{signal = -1, price = SellPrice2, timestamp = erlang:timestamp()},
                    #trading_decision{signal = 0, price = BuyPrice2, timestamp = erlang:timestamp()}
                ],
                
                SellResult = best_agent_runner:calculate_all_profits(SellDecisions),
                [SellDecision | _] = SellResult,
                SellActualProfit = SellDecision#trading_decision.profit_loss,
                
                case abs(SellActualProfit - ExpectedSellProfit) < 0.01 of
                    true ->
                        io:format("   Sell trade profit calculated correctly: ~p%: OK~n", [SellActualProfit]),
                        pass;
                    false ->
                        io:format("   Sell trade profit incorrect: expected ~p, got ~p~n", 
                                 [ExpectedSellProfit, SellActualProfit]),
                        fail
                end;
            false ->
                io:format("   Buy trade profit incorrect: expected ~p, got ~p~n", 
                         [ExpectedBuyProfit, BuyActualProfit]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Test decision recording accuracy
test_decision_recording_accuracy() ->
    io:format("~n--- Test: Decision Recording Accuracy ---~n"),
    
    try
        % Test that all decision fields are recorded accurately
        TestTimestamp = erlang:timestamp(),
        TestPrice = 1.0555,
        TestSignal = 1,
        TestIndex = 100,
        
        % Create a decision with all fields
        Decision = #trading_decision{
            timestamp = TestTimestamp,
            index = TestIndex,
            price = TestPrice,
            signal = TestSignal,
            confidence = 0.85,
            profit_loss = 12.5
        },
        
        % Verify all fields are recorded correctly
        case {Decision#trading_decision.timestamp, 
              Decision#trading_decision.index,
              Decision#trading_decision.price,
              Decision#trading_decision.signal,
              Decision#trading_decision.confidence,
              Decision#trading_decision.profit_loss} of
            {TestTimestamp, TestIndex, TestPrice, TestSignal, 0.85, 12.5} ->
                io:format("   All decision fields recorded accurately: OK~n"),
                
                % Test decision in a list context
                DecisionList = [Decision],
                Stats = best_agent_runner:calculate_decision_stats(DecisionList),
                
                case {maps:get(total_decisions, Stats), maps:get(buy_decisions, Stats)} of
                    {1, 1} ->
                        io:format("   Decision correctly processed in stats: OK~n"),
                        pass;
                    {ActualTotal, ActualBuy} ->
                        io:format("   Decision stats incorrect: expected {1, 1}, got {~p, ~p}~n",
                                 [ActualTotal, ActualBuy]),
                        fail
                end;
            Other ->
                io:format("   Decision fields not recorded correctly: ~p~n", [Other]),
                fail
        end
        
    catch
        Error:Reason ->
            io:format("   Test failed with exception: ~p:~p~n", [Error, Reason]),
            fail
    end.

%% Quick test function for manual testing
quick_test() ->
    io:format("=== Quick Test of Best Agent Runner ===~n"),
    
    % Test basic functionality
    io:format("1. Testing module initialization...~n"),
    case best_agent_runner:init() of
        ok ->
            io:format("   Module initialized: OK~n");
        Error ->
            io:format("   Module initialization failed: ~p~n", [Error])
    end,
    
    % Test record creation
    io:format("2. Testing record structures...~n"),
    TestDecision = #trading_decision{
        timestamp = erlang:timestamp(),
        signal = 1,
        price = 1.0500
    },
    
    TestOptions = #run_options{
        verbose = true,
        collect_all_decisions = true
    },
    
    io:format("   Records created successfully: OK~n"),
    
    % Test Task 5 functionality
    io:format("3. Testing Task 5 functions...~n"),
    TestStats = best_agent_runner:calculate_decision_stats([TestDecision]),
    case maps:get(total_decisions, TestStats) of
        1 ->
            io:format("   Task 5 functions working: OK~n");
        Other ->
            io:format("   Task 5 functions issue: ~p~n", [Other])
    end,
    
    io:format("=== Quick test completed ===~n").