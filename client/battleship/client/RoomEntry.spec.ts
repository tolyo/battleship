import { expect, test, type Page } from '@playwright/test';

test('room entry applies snapshots and restores the same seat after reload', async ({
  browser,
}) => {
  test.setTimeout(90_000);
  const contexts = await Promise.all([
    browser.newContext(),
    browser.newContext(),
  ]);
  const pages = await Promise.all(contexts.map((context) => context.newPage()));
  const errors: string[] = [];
  let connections = 0;
  pages[0].on('websocket', () => {
    connections += 1;
  });

  async function join(page: Page) {
    page.on('pageerror', (error) => errors.push(error.message));
    await page.route('https://fonts.googleapis.com/**', (route) =>
      route.abort()
    );
    await page.goto('/room', { waitUntil: 'domcontentloaded' });
    await page.locator('#random').click();
    await expect(page.locator('#ready')).toBeEnabled();
    await page.locator('#ready').click();
  }

  async function turns() {
    return (
      await Promise.all(
        pages.map((page) => page.locator('#match-status').textContent())
      )
    ).sort();
  }

  try {
    await Promise.all(pages.map(join));
    await expect
      .poll(turns, { timeout: 30_000 })
      .toEqual(["Awaiting opponent's move", 'Your turn']);
    await expect(pages[0]).toHaveURL(/\/room\/[^/?#]+$/);
    expect(pages[1].url()).toBe(pages[0].url());
    for (const page of pages) {
      await expect(page.locator('#own-board [data-state="X"]')).toHaveCount(20);
      await expect(page.locator('#target-board')).toBeVisible();
    }

    const roomUrl = pages[0].url();
    const previousConnections = connections;
    await pages[0].reload({ waitUntil: 'domcontentloaded' });
    await expect
      .poll(() => connections, { timeout: 10_000 })
      .toBeGreaterThan(previousConnections);
    await expect(pages[0]).toHaveURL(roomUrl);
    await expect(pages[0].locator('#own-board [data-state="X"]')).toHaveCount(
      20
    );
    await expect(pages[0].locator('#target-board')).toBeVisible();
    await expect
      .poll(turns, { timeout: 30_000 })
      .toEqual(["Awaiting opponent's move", 'Your turn']);
    const shooterIndex =
      (await pages[0].locator('#match-status').textContent()) === 'Your turn'
        ? 0
        : 1;
    const shooter = pages[shooterIndex];
    const defender = pages[1 - shooterIndex];
    await shooter.locator('#target-board-0-0').click();
    await expect(shooter.locator('#target-board-0-0')).toHaveAttribute(
      'data-state',
      /[+m]/,
      { timeout: 15_000 }
    );
    await expect(defender.locator('#own-board-0-0')).toHaveAttribute(
      'data-state',
      (await shooter.locator('#target-board-0-0').getAttribute('data-state')) ??
        ''
    );
    expect(errors).toEqual([]);
  } finally {
    await Promise.all(contexts.map((context) => context.close()));
  }
});
