.PHONY: clean clean_build setup start build format lint check unit-test unit-test-tap coverage test

# -----------------------------------------------------
#  Configuration
# -----------------------------------------------------
INFO      := [INFO]
TERLAN_COMPILER_ROOT ?= /home/anatoly/Applications/terlan/terlan
TERLC ?= $(shell debug="$(TERLAN_COMPILER_ROOT)/target/debug/terlc"; release="$(TERLAN_COMPILER_ROOT)/target/release/terlc"; if [ -x "$$release" ] && { [ ! -x "$$debug" ] || [ "$$release" -nt "$$debug" ]; }; then printf "%s" "$$release"; else printf "%s" "$$debug"; fi)
BUILD_DIR := _build/terlan
DEPS_DIR  := node_modules
LIVE_RELOAD_PORT ?= 35729

# -----------------------------------------------------
#  Utility Targets
# -----------------------------------------------------

## Remove node_modules and lockfile
clean:
	@if [ -d "$(DEPS_DIR)" ]; then \
		echo "$(INFO) Removing $(DEPS_DIR)..."; \
		rm -rf "$(DEPS_DIR)"; \
	fi
	@if [ -f "package-lock.json" ]; then \
		echo "$(INFO) Removing package-lock.json..."; \
		rm -f "package-lock.json"; \
	fi

## Remove build output
clean_build:
	@if [ -d "$(BUILD_DIR)" ]; then \
		echo "$(INFO) Removing $(BUILD_DIR)..."; \
		rm -rf "$(BUILD_DIR)"; \
	fi

# -----------------------------------------------------
#  Setup & Dev Targets
# -----------------------------------------------------

## Install dependencies and browsers
setup: clean
	@echo "$(INFO) Installing npm dependencies..."
	@npm ci || npm install
	@echo "$(INFO) Installing Playwright browsers..."
	@npx playwright install --with-deps

## Start local dev server
start:
	@echo "$(INFO) Frontend build is owned by terlc serve."
	
## Build prod
build: clean_build
	@echo "$(INFO) Building frontend through terlc web profile..."
	@$(TERLC) build --target js.browser --out-dir $(BUILD_DIR)

# -----------------------------------------------------
#  Code Quality Targets
# -----------------------------------------------------

## Run Prettier & ESLint
format:
	@echo "$(INFO) Formatting code with Prettier..."
	@npx prettier --write --cache --log-level=silent .

lint:
	@echo "$(INFO) Formatting code with Prettier..."
	@npx prettier --write --cache --log-level=silent .
	@echo "$(INFO) Linting code with ESLint..."
	@npx eslint ./assets --fix

## Typecheck code with TypeScript
check:
	@echo "$(INFO) Typechecking with tsc..."
	@npx tsc --noEmit --pretty false
	@$(MAKE) -f frontend.mk unit-test

unit-test:
	@echo "$(INFO) Running JS unit tests..."
	@npx jasmine --reporter=./tools/jasmine/color-tap-reporter.cjs "assets/**/*.test.js"

unit-test-tap:
	@echo "$(INFO) Running JS unit tests as TAP..."
	@npx jasmine --reporter=./tools/jasmine/tap-reporter.cjs "assets/**/*.test.js"

coverage:
	@echo "$(INFO) Measuring frontend application coverage at 100%..."
	@npm run --silent test:coverage

# -----------------------------------------------------
#  Testing
# -----------------------------------------------------

## Run Playwright tests
test:
	@echo "$(INFO) Running Playwright tests..."
	@npx playwright test
