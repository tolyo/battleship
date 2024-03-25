default: help

# Frontend make file context
FRONTEND_CONTEXT = make -C priv/static -f frontend.mk
SERVER_CONTEXT = make -f server.mk

INFO = "\033[32m[INFO]\033[0m"

#❓ help: @ Displays all commands and tooling
help:
	@grep -E '[a-zA-Z\.\-]+:.*?@ .*$$' $(MAKEFILE_LIST)| tr -d '#'  | awk 'BEGIN {FS = ":.*?@ "}; {printf "\033[32m%-30s\033[0m %s\n", $$1, $$2}'

clean:
	@echo $(INFO) "Cleaning project..."
	$(FRONTEND_CONTEXT) clean
	$(SERVER_CONTEXT) clean
	@echo $(INFO) "Complete. Run 'make setup' to install dependencies"

setup:
	$(FRONTEND_CONTEXT) setup
	$(SERVER_CONTEXT) setup
	@echo $(INFO) "Complete. Run 'make start' to start server"
	
compile:
	$(SERVER_CONTEXT) compile

# Helper for running dev mode
start:
	$(SERVER_CONTEXT) start

include ./config/dev.env

lint:
	$(FRONTEND_CONTEXT) lint
	$(SERVER_CONTEXT) lint
	@echo $(INFO) "Complete"

check:
	$(FRONTEND_CONTEXT) check
	$(SERVER_CONTEXT) check
	@echo $(INFO) "Complete"

.PHONY: test
test:
	$(SERVER_CONTEXT) test
	@echo $(INFO) "Complete"

functional-test:
	$(FRONTEND_CONTEXT) test
	@echo $(INFO) "Complete"

quality:
	@make lint
	@make check
	@make test
