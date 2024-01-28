clean:
	@echo "TODO"

setup:
	@rebar3 get-deps

compile:
	@echo "TODO"

start:
	@rebar3 shell --sname=app1_shell

lint:
	@echo $(INFO) "Formatting Erlang"
	@rebar3 fmt -w --verbose

check: 
	@echo "Checking w/ equalize"
	@./elp eqwalize-all

function-test:
	@echo "TODO"