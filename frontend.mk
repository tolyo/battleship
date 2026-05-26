.PHONY: clean clean_build setup start build format lint check test

# -----------------------------------------------------
#  Configuration
# -----------------------------------------------------
INFO      := [INFO]
BUILD_DIR := dist
DEPS_DIR  := node_modules

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
	@echo "$(INFO) Starting BrowserSync..."
	@node browsersync.cjs
	
## Build prod
build: clean_build
	@echo "$(INFO) Starting Rollup..."
	@npx rollup -c

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
	@npx eslint ./app --fix

## Typecheck code with TypeScript
check:
	@echo "$(INFO) Typechecking with tsc..."
	@npx tsc --noEmit

# -----------------------------------------------------
#  Testing
# -----------------------------------------------------

## Run Playwright tests
test:
	@echo "$(INFO) Running Playwright tests..."
	@npx playwright test
