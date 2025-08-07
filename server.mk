.PHONY: all clean setup compile start lint check test

# Default target
all: compile

clean:
	@echo "[CLEAN] Removing build artifacts..."
	@rm -rf _build deps *.beam ebin/*.beam || true

setup:
	@echo "[SETUP] Fetching dependencies..."
	@rebar3 get-deps

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
	@echo "[CHECK] Running eqwalize checks..."
	@./elp eqwalize-all

test:
	@echo "[TEST] Running Rebar3 tests..."
	@rebar3 eunit
