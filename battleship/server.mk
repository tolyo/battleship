clean:
	@echo "TODO"

setup:
	curl -sS https://webinstall.dev/watchexec | bash

compile:
	@echo "TODO"

start:
	watchexec --project-origin . \
		-r -w src \
		-- make -f server.mk ershell_update & \
	rebar3 shell --sname=app_shell

USERNAME := $(shell whoami)
ershell_update:
	erl -sname remote -eval 'rpc:call(app_shell@$(USERNAME), r3, do, [compile]), halt(0).' -noshell

lint:
	@echo $(INFO) "Formatting Erlang"
	@rebar3 fmt -w --verbose

check: 
	@echo "Checking w/ equalize"
	@./elp eqwalize-all

function-test:
	@echo "TODO"