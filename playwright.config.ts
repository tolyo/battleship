import type { PlaywrightTestConfig } from '@playwright/test';
import { devices } from '@playwright/test';

const config: PlaywrightTestConfig = {
  testDir: '.',
  testMatch: ['**/assets/**/*.spec.ts', '**/client/**/*.spec.ts'],
  testIgnore: ['**/node_modules/**', '**/_build/**'],
  timeout: 30 * 1000,
  expect: { timeout: 5000 },
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  // Anonymous games share one matchmaking queue on the test server.
  workers: 1,
  reporter: 'html',
  use: {
    actionTimeout: 0,
    baseURL: 'http://127.0.0.1:4000',
    trace: 'on-first-retry',
  },
  projects: [
    { name: 'chromium', use: { ...devices['Desktop Chrome'] } },
    { name: 'firefox', use: { ...devices['Desktop Firefox'] } },
    { name: 'webkit', use: { ...devices['Desktop Safari'] } },
  ],
  webServer: {
    command: 'make browser-test-server',
    // Allow a cold native build before the server becomes ready.
    timeout: 300_000,
    port: 4000,
    reuseExistingServer: false,
  },
};

export default config;
