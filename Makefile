# Default target
default: help

# Frontend context and Terlan backend compiler
FRONTEND_CONTEXT = make -f frontend.mk
TERLAN_COMPILER_ROOT ?= /home/anatoly/Applications/terlan/terlan
TERLC ?= $(shell debug="$(TERLAN_COMPILER_ROOT)/target/debug/terlc"; release="$(TERLAN_COMPILER_ROOT)/target/release/terlc"; if [ -x "$$release" ] && { [ ! -x "$$debug" ] || [ "$$release" -nt "$$debug" ]; }; then printf "%s" "$$release"; else printf "%s" "$$debug"; fi)
TERLC_QUALITY ?= $(TERLAN_COMPILER_ROOT)/target/debug/terlc
TERLAN_OUT_DIR = _build
TERLAN_TEST_ROOT = _build/terlan-tests
TERLAN_COVERAGE_HITS = $(CURDIR)/_build/coverage/terlan-callables.txt
TERLAN_TEST_FLAGS ?=
TERLAN_WEB_DIR = $(TERLAN_OUT_DIR)/web
TERLAN_INTEGRATION_PORT ?= 18080

# Info formatting
INFO = "\033[32m[INFO]\033[0m"
DONE = "\033[32m✔\033[0m"

# Export environment variables if needed
include ./config/dev.env

.PHONY: all clean setup compile compile-backend compile-web start format lint check unit-test unit-test-tap test coverage functional-test integration-test quality help db-start db-up db-down db-rebuild terlan-check terlan-grouped-binding-check terlan-function-reference-check terlan-build terlan-build-backend terlan-build-web terlan-test vm-contract-check

all: compile

clean:
	@echo $(INFO) "Cleaning project..."
	@$(FRONTEND_CONTEXT) clean
	@rm -rf $(TERLAN_OUT_DIR) $(TERLAN_TEST_ROOT)
	@echo $(DONE) " Clean complete. Run 'make setup' to install dependencies."

setup:
	@echo $(INFO) "Setting up project dependencies..."
	@$(FRONTEND_CONTEXT) setup
	@echo $(DONE) " Setup complete. Run 'make start' to start the server."

compile:
	@echo $(INFO) "Compiling Terlan backend..."
	@$(MAKE) terlan-build
	@echo $(DONE) " Compile complete."

compile-backend:
	@echo $(INFO) "Compiling Terlan backend..."
	@$(MAKE) terlan-build-backend
	@echo $(DONE) " Backend compile complete."

compile-web:
	@echo $(INFO) "Compiling Terlan web package..."
	@$(MAKE) terlan-build-web
	@echo $(DONE) " Web compile complete."

start: db-start
	@set -e; \
	setsid $(MAKE) frontend-serve & \
	frontend_pid=$$!; \
	cleanup() { \
		kill -TERM -$$frontend_pid 2>/dev/null || kill $$frontend_pid 2>/dev/null || true; \
		wait $$frontend_pid 2>/dev/null || true; \
	}; \
	trap cleanup INT TERM EXIT; \
	sleep 2; \
	$(MAKE) backend-serve; \
	status=$$?; \
	cleanup; \
	trap - INT TERM EXIT; \
	exit $$status

build:
	@echo $(INFO) "Building frontend..."
	@$(FRONTEND_CONTEXT) build
	@echo $(DONE) " Build complete."

frontend-serve:
	@$(FRONTEND_CONTEXT) start

backend-serve:
	@if [ ! -f "$(TERLAN_WEB_DIR)/manifest.json" ]; then \
		echo $(INFO) "No Terlan build artifacts found; building once before serving..."; \
		$(MAKE) terlan-build; \
	else \
		echo $(INFO) "Using existing Terlan build artifacts. Run 'make compile' after source changes."; \
	fi
	@$(TERLC) serve $(TERLAN_WEB_DIR) --host 127.0.0.1 --port 8080

db-start:
	@echo $(INFO) "Starting database..."
	@docker compose up -d postgres
	@echo $(INFO) "Waiting for database..."
	@for attempt in $$(seq 1 30); do \
		if docker compose exec -T postgres pg_isready -U "$(POSTGRES_USER)" -d "$(POSTGRES_DB)" >/dev/null 2>&1; then \
			exit 0; \
		fi; \
		sleep 1; \
	done; \
	docker compose ps postgres; \
	docker compose logs --tail=40 postgres; \
	exit 1

format:
	@echo $(INFO) "Formatting project..."
	@$(FRONTEND_CONTEXT) format
	@npm run --silent format:sql
	@$(TERLC) fmt --write src tests/battleship
	@echo $(DONE) " Format complete."

lint:
	@echo $(INFO) "Linting project..."
	@$(FRONTEND_CONTEXT) lint
	@npm run --silent lint:sql
	@$(TERLC) fmt --check src tests/battleship
	@$(TERLC_QUALITY) lint --only TL0506 src tests/battleship
	@$(TERLC_QUALITY) lint --only TL0907 src tests/battleship
	@$(MAKE) terlan-check
	@echo $(DONE) " Linting complete."

check:
	@echo $(INFO) "Running static checks..."
	@$(FRONTEND_CONTEXT) check
	@$(MAKE) terlan-check
	@echo $(DONE) " Static checks complete."

terlan-check: terlan-grouped-binding-check terlan-function-reference-check
	@echo $(INFO) "Checking Terlan migration sources..."
	@$(TERLC) check src
	@echo $(DONE) " Terlan migration checks complete."

terlan-grouped-binding-check:
	@test -x "$(TERLC_QUALITY)" || { echo "missing terlc: $(TERLC_QUALITY)" >&2; exit 1; }
	@$(TERLC_QUALITY) lint --only TL0009 src
	@$(TERLC_QUALITY) lint --only TL0009 tests/battleship

terlan-function-reference-check:
	@test -x "$(TERLC_QUALITY)" || { echo "missing terlc: $(TERLC_QUALITY)" >&2; exit 1; }
	@$(TERLC_QUALITY) lint --only TL0010 src
	@$(TERLC_QUALITY) lint --only TL0010 tests/battleship

terlan-build:
	@echo $(INFO) "Building Terlan backend and web package..."
	@$(MAKE) terlan-build-web
	@$(MAKE) terlan-build-backend
	@$(TERLC) serve $(TERLAN_WEB_DIR) --check
	@echo $(DONE) " Terlan backend and web package preflight complete."

terlan-build-backend:
	@$(TERLC) build . --target terlan-vm --out-dir $(TERLAN_OUT_DIR)

terlan-build-web:
	@$(TERLC) build --target js.browser --out-dir $(TERLAN_OUT_DIR)

terlan-test:
	@echo $(INFO) "Running Terlan migration test modules..."
	@rm -rf $(TERLAN_TEST_ROOT)
	@mkdir -p $(TERLAN_TEST_ROOT)/battleship
	@cp -R src/battleship $(TERLAN_TEST_ROOT)/
	@cp -R tests/battleship/. $(TERLAN_TEST_ROOT)/battleship/
	@$(TERLC) check $(TERLAN_TEST_ROOT)
	@$(TERLC) test $(TERLAN_TEST_ROOT) --target terlan-vm $(TERLAN_TEST_FLAGS)
	@echo $(DONE) " Terlan migration tests complete."

unit-test:
	@echo $(INFO) "Running frontend unit tests..."
	@$(FRONTEND_CONTEXT) unit-test
	@echo $(DONE) " Frontend unit tests complete."

unit-test-tap:
	@echo $(INFO) "Running frontend unit tests as TAP..."
	@$(FRONTEND_CONTEXT) unit-test-tap
	@echo $(DONE) " Frontend unit TAP complete."

test:
	@echo $(INFO) "Running Terlan backend tests..."
	@$(MAKE) terlan-test
	@echo $(DONE) " Backend tests complete."

coverage:
	@mkdir -p $(dir $(TERLAN_COVERAGE_HITS))
	@rm -f $(TERLAN_COVERAGE_HITS)
	@TERLAN_CALLABLE_COVERAGE_FILE=$(TERLAN_COVERAGE_HITS) $(MAKE) integration-test
	@TERLAN_CALLABLE_COVERAGE_FILE=$(TERLAN_COVERAGE_HITS) $(MAKE) terlan-test TERLAN_TEST_FLAGS="--coverage --coverage-threshold 100"
	@$(FRONTEND_CONTEXT) coverage

vm-contract-check:
	@test -x "$(TERLC)" || { echo "missing terlc: $(TERLC)" >&2; exit 1; }
	@set -eu; \
	workspace=$$(mktemp -d "$${TMPDIR:-/tmp}/battleship-vm-contract.XXXXXX"); \
	trap 'rm -rf "$$workspace"' EXIT INT TERM; \
	mkdir -p "$$workspace/source/battleship/model" "$$workspace/artifacts"; \
	cp -R "$(CURDIR)/src/battleship/." "$$workspace/source/battleship/"; \
	"$(TERLC)" build "$$workspace/source" --target terlan-vm --out-dir "$$workspace/artifacts"; \
	test -n "$$(find "$$workspace/artifacts" -type f -name '*.tvm' -print -quit)"; \
	test -z "$$(find "$$workspace/artifacts" -type f \( -name '*.beam' -o -name '*.erl' \) -print -quit)"

functional-test:
	@echo $(INFO) "Running frontend functional tests..."
	@$(FRONTEND_CONTEXT) test
	@echo $(DONE) " Functional tests complete."

integration-test:
	@echo $(INFO) "Running Terlan server/database integration test..."
	@$(TERLC) integration-test . --out-dir $(TERLAN_OUT_DIR) --port $(TERLAN_INTEGRATION_PORT)
	@echo $(DONE) " Terlan server/database integration test complete."

quality:
	@echo $(INFO) "Running all quality checks..."
	@$(MAKE) lint
	@$(MAKE) check
	@$(MAKE) test
	@$(MAKE) coverage
	@$(MAKE) vm-contract-check
	@echo $(DONE) " Quality checks complete."

include ./config/dev.env
DB_DSN:="host=$(POSTGRES_HOST) user=$(POSTGRES_USER) password=$(POSTGRES_PASSWORD) dbname=$(POSTGRES_DB) port=$(POSTGRES_PORT) sslmode=disable"
MIGRATE_OPTIONS=-allow-missing -dir="./sql"

db-up: ## up down on database
	goose -v $(MIGRATE_OPTIONS) postgres $(DB_DSN) up

db-down: ## Migrate down on database
	goose -v $(MIGRATE_OPTIONS) postgres $(DB_DSN) reset

db-rebuild: ## Reset the database
	make db-down
	make db-up

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Available targets:"
	@echo "  clean            Remove build artifacts"
	@echo "  setup            Install frontend dependencies"
	@echo "  compile          Compile the Terlan backend"
	@echo "  compile-backend  Check only Terlan VM backend sources"
	@echo "  compile-web      Compile only Terlan web package"
	@echo "  start            Start the development server"
	@echo "  format           Format frontend and Terlan code"
	@echo "  lint             Format & lint frontend and backend"
	@echo "  check            Run static type checks"
	@echo "  terlan-check     Check Terlan migration sources"
	@echo "  terlan-build     Build Terlan migration sources"
	@echo "  terlan-test      Run Terlan migration tests"
	@echo "  test             Run Terlan backend tests"
	@echo "  coverage         Enforce 100% Terlan declaration and frontend application coverage"
	@echo "  vm-contract-check Validate the Battleship Terlan VM artifact contract"
	@echo "  functional-test  Run frontend Playwright tests"
	@echo "  integration-test Start DB and server, then run HTTP smoke checks"
	@echo "  quality          Run all lint, check, and test targets"
	@echo "  db-up            Run up all migrations"
	@echo "  db-down          Run down all migrations"
	@echo "  db-rebuild       Rebuild the database"
	@echo "  help             Show this help message"
