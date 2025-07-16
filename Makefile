# Makefile for DXNN Erlang project

# Erlang compiler
ERL = erl
ERLC = erlc

# Compilation flags
ERLC_FLAGS = +debug_info +warn_export_all +warn_export_vars +warn_shadow_vars +warn_obsolete_guard

# Source files
SOURCES = $(wildcard *.erl)
BEAMS = $(SOURCES:.erl=.beam)

# Default target
all: $(BEAMS)

# Compile Erlang files
%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<

# Clean compiled files
clean:
	rm -f *.beam
	rm -f erl_crash.dump

# Test targets
test: all
	$(ERL) -noshell -eval "c(best_agent_runner_tests), best_agent_runner_tests:run_all_tests(), halt()."

quick-test: all
	$(ERL) -noshell -eval "c(best_agent_runner_tests), best_agent_runner_tests:quick_test(), halt()."

# Test task 4 specifically
test-task4: all
	escript test_task4.erl

# Compile specific modules for task 4
task4: best_agent_runner.beam best_agent_runner_tests.beam

# Initialize the system (for Docker environment)
init:
	$(ERL) -noshell -eval "mnesia:create_schema([node()]), mnesia:start(), make:all(), fx:init(), fx:start(), polis:create(), polis:start(), halt()."

# Help target
help:
	@echo "Available targets:"
	@echo "  all        - Compile all Erlang files"
	@echo "  clean      - Remove compiled files"
	@echo "  test       - Run all tests"
	@echo "  quick-test - Run quick tests only"
	@echo "  test-task4 - Test Task 4 implementation"
	@echo "  task4      - Compile Task 4 modules only"
	@echo "  init       - Initialize the system (for Docker)"
	@echo "  help       - Show this help message"

.PHONY: all clean test quick-test test-task4 task4 init help