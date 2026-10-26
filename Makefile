# Default target
default: help

# Frontend context and Terlan backend compiler
FRONTEND_CONTEXT = make -f frontend.mk
TERLAN_COMPILER_ROOT ?= /home/anatoly/Applications/terlan/terlan
# Select a compiler with its paired VM and native worker; a partial release build
# must not displace the complete development toolchain.
TERLC ?= $(shell debug="$(TERLAN_COMPILER_ROOT)/target/debug/terlc"; release="$(TERLAN_COMPILER_ROOT)/target/release/terlc"; if [ -x "$$release" ] && [ -x "$${release%/*}/terlan-vm" ] && [ -x "$${release%/*}/terlan-native-worker" ] && { [ ! -x "$$debug" ] || [ "$$release" -nt "$$debug" ]; }; then printf "%s" "$$release"; else printf "%s" "$$debug"; fi)
# Keep nested Make invocations on the same toolchain during concurrent compiler builds.
TERLC := $(TERLC)
export TERLC
TERLC_QUALITY ?= $(TERLAN_COMPILER_ROOT)/target/debug/terlc
TERLAN_OUT_DIR = _build
TERLAN_COVERAGE_HITS = $(CURDIR)/_build/coverage/terlan-callables.txt
TERLAN_TEST_FLAGS ?=
TERLAN_WEB_DIR = $(TERLAN_OUT_DIR)/.terlan/application/web
TERLAN_EXECUTABLE = $(TERLAN_OUT_DIR)/bin/battleship
TERLAN_INTEGRATION_PORT ?= 18080
TERLAN_BROWSER_TEST_PORT ?= 4000
TERLAN_ANGULAR_BINDINGS = $(TERLAN_OUT_DIR)/bindings/angular
TERLAN_JS_BINDING_MANIFEST = $(CURDIR)/$(TERLAN_ANGULAR_BINDINGS)/std/js/manifests/std_js_bindings.json
TERLAN_HOST_BINDINGS = $(TERLAN_OUT_DIR)/bindings/host
TERLAN_JS_BINDING_MANIFESTS = $(TERLAN_JS_BINDING_MANIFEST):$(CURDIR)/$(TERLAN_HOST_BINDINGS)/std/js/manifests/std_js_bindings.json

# Info formatting
INFO = "\033[32m[INFO]\033[0m"
DONE = "\033[32m✔\033[0m"

# Development configuration used by Make targets
include ./config/dev.env
export POSTGRES_HOST POSTGRES_PORT POSTGRES_USER POSTGRES_PASSWORD POSTGRES_DB
export DATABASE_URL ?= postgresql://$(POSTGRES_USER):$(POSTGRES_PASSWORD)@$(POSTGRES_HOST):$(POSTGRES_PORT)/$(POSTGRES_DB)
export TERLAN_DATABASE_URL ?= $(DATABASE_URL)
export TERLAN_SERVE_TRUSTED_HOST_CAPABILITIES ?= 1
TERLAN_SERVE_RUNTIME_ONLY ?= 1

.PHONY: angular-compat all clean setup compile compile-backend compile-web start format lint check unit-test unit-test-tap test coverage functional-test browser-test-server integration-test quality help db-start db-up db-down db-rebuild db-validate db-status terlan-check terlan-grouped-binding-check terlan-function-reference-check terlan-build terlan-build-backend terlan-build-browser terlan-build-web terlan-test vm-contract-check

all: compile

clean:
	@echo $(INFO) "Cleaning project..."
	@$(FRONTEND_CONTEXT) clean
	@rm -rf $(TERLAN_OUT_DIR)
	@rm -f .terlan/*.typi .terlan/*.typi.deps
	@find src -type f -path '*/.terlan/*' \( -name '*.typi' -o -name '*.typi.deps' \) -delete
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
	@echo $(INFO) "Compiling the native package with embedded browser resources..."
	@$(MAKE) terlan-build
	@echo $(DONE) " Web compile complete."

start: db-up
	@$(MAKE) backend-serve

build:
	@echo $(INFO) "Building frontend..."
	@$(FRONTEND_CONTEXT) build
	@echo $(DONE) " Build complete."

frontend-serve:
	@$(FRONTEND_CONTEXT) start

backend-serve:
	@if [ ! -x "$(TERLAN_EXECUTABLE)" ]; then \
		echo $(INFO) "No Terlan build artifacts found; building once before serving..."; \
		$(MAKE) terlan-build; \
	else \
		echo $(INFO) "Using existing Terlan build artifacts. Run 'make compile' after source changes."; \
	fi
	@PORT=8080 TERLAN_SERVE_PORT=8080 TERLAN_SERVE_HOST=127.0.0.1 $(TERLAN_EXECUTABLE)

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
	@$(TERLC) fmt --write src browser client
	@echo $(DONE) " Format complete."

lint:
	@echo $(INFO) "Linting project..."
	@$(FRONTEND_CONTEXT) lint
	@npm run --silent lint:sql
	@$(TERLC) fmt --check src browser client
	@$(TERLC_QUALITY) lint --only TL0506 src
	@$(TERLC_QUALITY) lint --only TL0907 src
	@$(MAKE) terlan-check
	@echo $(DONE) " Linting complete."

check:
	@echo $(INFO) "Running static checks..."
	@$(FRONTEND_CONTEXT) check
	@$(MAKE) terlan-check
	@echo $(DONE) " Static checks complete."

terlan-check: terlan-grouped-binding-check terlan-function-reference-check $(TERLAN_ANGULAR_BINDINGS)/.complete $(TERLAN_HOST_BINDINGS)/.complete
	@echo $(INFO) "Checking Terlan sources..."
	@$(TERLC) check src
	@$(TERLC) check browser --cache-dir .terlan
	@TERLAN_JS_BINDING_MANIFESTS="$(TERLAN_JS_BINDING_MANIFESTS)" $(TERLC) check client --cache-dir .terlan
	@echo $(DONE) " Terlan checks complete."

terlan-grouped-binding-check:
	@test -x "$(TERLC_QUALITY)" || { echo "missing terlc: $(TERLC_QUALITY)" >&2; exit 1; }
	@$(TERLC_QUALITY) lint --only TL0009 src

terlan-function-reference-check:
	@test -x "$(TERLC_QUALITY)" || { echo "missing terlc: $(TERLC_QUALITY)" >&2; exit 1; }
	@$(TERLC_QUALITY) lint --only TL0010 src

terlan-build:
	@echo $(INFO) "Building Terlan backend and web package..."
	@$(MAKE) terlan-build-backend
	@$(TERLC) serve $(TERLAN_WEB_DIR) --check
	@echo $(DONE) " Terlan backend and web package preflight complete."

terlan-build-backend: terlan-build-browser
	@$(TERLC) static emit tools/Shell.terl --out-dir _build/templates
	@$(TERLC) build . --target terlan-vm --out-dir $(TERLAN_OUT_DIR)

angular-compat:
	@if ! rg -q 'simpleCompare\(deProxy\(state._lastParentValue' node_modules/@angular-wave/angular.ts/dist/angular-ts.esm.js || \
	    ! rg -q 'if \(expr\._computed\) \{' node_modules/@angular-wave/angular.ts/dist/angular-ts.esm.js || \
	    ! rg -q 'simpleCompare\(deProxy\(state._lastParentValue' node_modules/@angular-wave/angular.ts/dist/core/compile/compile.js || \
	    ! rg -q 'if \(expr\._computed\) \{' node_modules/@angular-wave/angular.ts/dist/core/scope/scope.js; then \
		$(TERLC) test tools/AngularCompatibilityTest.terl --target terlan-vm; \
	fi

$(TERLAN_ANGULAR_BINDINGS)/.complete: $(TERLC) node_modules/@angular-wave/angular.ts/@types/namespace.d.ts node_modules/@angular-wave/angular.ts/@types/interface.d.ts
	@mkdir -p $(TERLAN_OUT_DIR)/bindings
	@set -eu; bindings_stage=$$(mktemp -d "$(TERLAN_OUT_DIR)/bindings/angular.stage.XXXXXX"); \
		$(TERLC) bind angular-ts --out "$$bindings_stage"; \
		if [ -d "$(TERLAN_ANGULAR_BINDINGS)" ]; then mv "$(TERLAN_ANGULAR_BINDINGS)" "$$bindings_stage.previous"; fi; \
		mv "$$bindings_stage" "$(TERLAN_ANGULAR_BINDINGS)"
	@touch $@

$(TERLAN_HOST_BINDINGS)/.complete: $(TERLC) bindings/Host.d.ts
	@mkdir -p $(TERLAN_OUT_DIR)/bindings
	@set -eu; bindings_stage=$$(mktemp -d "$(TERLAN_OUT_DIR)/bindings/host.stage.XXXXXX"); \
		$(TERLC) bind typescript --input bindings/Host.d.ts --namespace battleship.host --out "$$bindings_stage"; \
		if [ -d "$(TERLAN_HOST_BINDINGS)" ]; then mv "$(TERLAN_HOST_BINDINGS)" "$$bindings_stage.previous"; fi; \
		mv "$$bindings_stage" "$(TERLAN_HOST_BINDINGS)"
	@touch $@

terlan-build-browser: angular-compat $(TERLAN_ANGULAR_BINDINGS)/.complete $(TERLAN_HOST_BINDINGS)/.complete
	@$(TERLC) interface $$(rg --files src/battleship/shared -g '*.terl' -g '!*Test.terl') --out-dir .terlan
	@$(TERLC) check browser --cache-dir .terlan
	@for source in $$(rg --files browser src/battleship/shared -g '*.terl' -g '!*Test.terl'); do \
		$(TERLC) build "$$source" --target js.shared --out-dir $(TERLAN_OUT_DIR)/browser --cache-dir .terlan --declarations || exit $$?; \
	done
	@TERLAN_JS_BINDING_MANIFESTS="$(TERLAN_JS_BINDING_MANIFESTS)" $(TERLC) check client --cache-dir .terlan
	@for source in $$(rg --files client -g '*.terl' -g '!*Test.terl'); do \
		TERLAN_JS_BINDING_MANIFESTS="$(TERLAN_JS_BINDING_MANIFESTS)" $(TERLC) build "$$source" --target js.shared --out-dir $(TERLAN_OUT_DIR)/browser --cache-dir .terlan --declarations || exit $$?; \
	done

terlan-build-web: terlan-build-browser
	@$(TERLC) static emit tools/Shell.terl --out-dir _build/templates
	@$(TERLC) build --target js.browser --out-dir $(TERLAN_OUT_DIR)

terlan-test:
	@echo $(INFO) "Running Terlan tests..."
	@$(TERLC) test src browser --target terlan-vm $(TERLAN_TEST_FLAGS)
	@echo $(DONE) " Terlan tests complete."

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

browser-test-server: db-up compile
	@PORT=$(TERLAN_BROWSER_TEST_PORT) TERLAN_SERVE_PORT=$(TERLAN_BROWSER_TEST_PORT) TERLAN_SERVE_HOST=127.0.0.1 $(TERLAN_EXECUTABLE)

integration-test: terlan-build-browser
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

db-validate: ## Validate Terlan migration files without changing the database
	@$(TERLC) db validate sql

db-status: db-start ## Show applied and pending Terlan migrations
	@$(TERLC) db status sql

db-up: db-start ## Apply pending Terlan database migrations
	@$(TERLC) db migrate sql

db-down: ## Stop the development database while retaining its data
	@docker compose stop postgres

db-rebuild: db-start ## Rebuild the local development schema from Terlan migrations
	@$(TERLC) db rebuild --dev --confirm sql

help:
	@echo "Usage: make [target]"
	@echo ""
	@echo "Available targets:"
	@echo "  clean            Remove build artifacts"
	@echo "  setup            Install frontend dependencies"
	@echo "  compile          Build the Terlan backend and browser package"
	@echo "  compile-backend  Build only the Terlan VM backend"
	@echo "  compile-web      Compile only Terlan web package"
	@echo "  start            Start the development server"
	@echo "  format           Format frontend and Terlan code"
	@echo "  lint             Format & lint frontend and backend"
	@echo "  check            Run static type checks"
	@echo "  terlan-check     Check Terlan sources"
	@echo "  terlan-build     Build Terlan sources"
	@echo "  terlan-test      Run Terlan tests"
	@echo "  test             Run Terlan backend tests"
	@echo "  coverage         Enforce 100% Terlan declaration and frontend application coverage"
	@echo "  vm-contract-check Validate the Battleship Terlan VM artifact contract"
	@echo "  functional-test  Run frontend Playwright tests"
	@echo "  integration-test Check database, HTTP, and WebSocket behavior"
	@echo "  quality          Run all lint, check, and test targets"
	@echo "  db-up            Apply pending Terlan migrations"
	@echo "  db-down          Stop PostgreSQL and retain its data"
	@echo "  db-rebuild       Rebuild the database"
	@echo "  help             Show this help message"
