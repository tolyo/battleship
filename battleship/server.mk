clean:
	@echo "TODO"

setup: 
	@echo "TODO"

compile:
	@echo "TODO"

start: 
	@echo "TODO"

lint:
	@echo $(INFO) "Formatting Erlang"
	@rebar3 fmt -w --verbose

check: 
	@echo "Checking w/ equalize"
	@./elp eqwalize-all

function-test:
	@echo "TODO"