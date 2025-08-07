.PHONY: clean setup start lint check test

INFO = [INFO]

clean:
	@echo "$(INFO) Cleaning up node_modules..."
	@rm -rf node_modules

setup:
	@echo "$(INFO) Installing NPM dependencies..."
	@npm install
	@echo "$(INFO) Installing Playwright browsers..."
	@npx playwright install

start:
	@echo "$(INFO) Starting BrowserSync..."
	@npm run browsersync

lint:
	@echo "$(INFO) Formatting JavaScript/CSS..."
	@npm run format
	@echo "$(INFO) Linting JavaScript..."
	@npm run lint

check:
	@echo "$(INFO) Typechecking JavaScript..."
	@npm run typecheck

test:
	@echo "$(INFO) Running Playwright tests..."
	@npm run playwright
