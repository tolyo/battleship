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

.PHONY: all clean setup compile start lint check test functional-test quality help db-up db-down db-rebuild

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

start:
	$(MAKE) frontend-serve &
	sleep 2
	$(MAKE) backend-serve

frontend-serve:
	@$(FRONTEND_CONTEXT) start

backend-serve:	
	@$(SERVER_CONTEXT) start

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
	@echo "  lint             Format & lint frontend and backend"
	@echo "  check            Run static type checks"
	@echo "  test             Run backend tests"
	@echo "  functional-test  Run frontend Playwright tests"
	@echo "  quality          Run all lint, check, and test targets"
	@echo "  db-up            Run up all migrations"
	@echo "  db-down          Run down all migrations"
	@echo "  db-rebuild       Rebuild the database"
	@echo "  help             Show this help message"
