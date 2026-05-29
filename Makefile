# Default target
default: help

# Frontend and Server context
FRONTEND_CONTEXT = make -f frontend.mk
SERVER_CONTEXT   = make -f server.mk

# Info formatting
INFO = "\033[32m[INFO]\033[0m"
DONE = "\033[32m✔\033[0m"

# Export environment variables if needed
include ./config/dev.env

.PHONY: all clean setup compile start format lint check unit-test unit-test-tap test functional-test quality help db-start db-up db-down db-rebuild

all: compile

clean:
	@echo $(INFO) "Cleaning project..."
	@$(FRONTEND_CONTEXT) clean
	@$(SERVER_CONTEXT) clean
	@echo $(DONE) " Clean complete. Run 'make setup' to install dependencies."

setup:
	@echo $(INFO) "Setting up project dependencies..."
	@$(FRONTEND_CONTEXT) setup
	@$(SERVER_CONTEXT) setup
	@echo $(DONE) " Setup complete. Run 'make start' to start the server."

compile:
	@echo $(INFO) "Compiling server..."
	@$(SERVER_CONTEXT) compile
	@echo $(DONE) " Compile complete."

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
	@$(SERVER_CONTEXT) start

db-start:
	@echo $(INFO) "Starting database..."
	@docker compose up -d db
	@echo $(INFO) "Waiting for database..."
	@for attempt in $$(seq 1 30); do \
		if docker compose exec -T db pg_isready -U "$(POSTGRES_USER)" -d "$(POSTGRES_DB)" >/dev/null 2>&1; then \
			exit 0; \
		fi; \
		sleep 1; \
	done; \
	docker compose ps db; \
	docker compose logs --tail=40 db; \
	exit 1

format:
	@echo $(INFO) "Formatting project..."
	@$(FRONTEND_CONTEXT) format
	@$(SERVER_CONTEXT) format
	@echo $(DONE) " Format complete."

lint:
	@echo $(INFO) "Linting project..."
	@$(FRONTEND_CONTEXT) lint
	@$(SERVER_CONTEXT) lint
	@echo $(DONE) " Linting complete."

check:
	@echo $(INFO) "Running static checks..."
	@$(FRONTEND_CONTEXT) check
	@$(SERVER_CONTEXT) check
	@echo $(DONE) " Static checks complete."

unit-test:
	@echo $(INFO) "Running frontend unit tests..."
	@$(FRONTEND_CONTEXT) unit-test
	@echo $(DONE) " Frontend unit tests complete."

unit-test-tap:
	@echo $(INFO) "Running frontend unit tests as TAP..."
	@$(FRONTEND_CONTEXT) unit-test-tap
	@echo $(DONE) " Frontend unit TAP complete."

test:
	@echo $(INFO) "Running backend tests..."
	@$(SERVER_CONTEXT) test
	@echo $(DONE) " Backend tests complete."

functional-test:
	@echo $(INFO) "Running frontend functional tests..."
	@$(FRONTEND_CONTEXT) test
	@echo $(DONE) " Functional tests complete."

quality:
	@echo $(INFO) "Running all quality checks..."
	@$(MAKE) lint
	@$(MAKE) check
	@$(MAKE) test
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
	@echo "  setup            Install frontend & server dependencies"
	@echo "  compile          Compile the server code"
	@echo "  start            Start the development server"
	@echo "  format           Format frontend and Erlang code"
	@echo "  lint             Format & lint frontend and backend"
	@echo "  check            Run static type checks"
	@echo "  test             Run backend tests"
	@echo "  functional-test  Run frontend Playwright tests"
	@echo "  quality          Run all lint, check, and test targets"
	@echo "  db-up            Run up all migrations"
	@echo "  db-down          Run down all migrations"
	@echo "  db-rebuild       Rebuild the database"
	@echo "  help             Show this help message"
