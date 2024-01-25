clean:
	@rm -rf node_modules

setup:
	@echo $(INFO) "Installing NPM dependencies..."
	@npm i 
	@npx playwright install

start:
	@echo $(INFO) "Starting browsersync ..."
	@npm run browsersync

lint:
	@echo $(INFO) "Formatting Js/CSS"
	@npm run format
	@echo $(INFO) "Linting Js"
	@npm run lint

check:
	@echo $(INFO) "Typechecking Js"
	@npm run typecheck

test:
	@echo $(INFO) "Playwright test JS"
	@npm run playwright