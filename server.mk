.PHONY: all clean setup compile start lint check test

# Default target
all: compile

clean:
	@echo "[CLEAN] Removing build artifacts..."
	@rm -rf _build deps *.beam ebin/*.beam || true

setup:
	@echo "[SETUP] Fetching dependencies..."
	@rebar3 get-deps
	@rebar3 dialyzer plt

compile:
	@echo "[COMPILE] Compiling the project..."
	@rebar3 compile

start:
	@echo "[START] Starting the application shell..."
	@rebar3 shell --sname app1_shell

lint:
	@echo "[LINT] Formatting Erlang files..."
	@rebar3 fmt -w --verbose

check:
	@echo "[CHECK] Running Erlang checks..."
	@rebar3 dialyzer

test:
	@echo "[TEST] Running Erlang tests..."
	@rebar3 eunit
