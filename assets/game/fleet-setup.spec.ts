import { expect, test, type Page } from '@playwright/test';

async function dragShip(page: Page, id: number, row: number, column: number) {
  const ship = page.locator(`.unit[id="${id}"]`);
  const source = await ship.boundingBox();
  const target = await page
    .locator(`#own-board-${row}-${column}`)
    .boundingBox();
  expect(source).not.toBeNull();
  expect(target).not.toBeNull();
  await page.mouse.move(source!.x + 10, source!.y + 10);
  await page.mouse.down();
  await page.mouse.move(target!.x + 10, target!.y + 10, { steps: 12 });
  await page.mouse.up();
  await expect(page.locator(`#own-board-${row}-${column}`)).toHaveAttribute(
    'data-state',
    'X'
  );
}

test.use({ viewport: { width: 1280, height: 1100 } });

test('fleet placement, random, reset, and ready work through the rendered templates', async ({
  page,
}) => {
  test.setTimeout(60_000);
  const errors: string[] = [];
  page.on('pageerror', (error) => errors.push(error.message));
  await page.route('https://fonts.googleapis.com/**', (route) => route.abort());
  await page.goto('/room', { waitUntil: 'domcontentloaded' });
  await expect(page.locator('.unit')).toHaveCount(10);
  await expect
    .poll(async () => {
      const ship = await page.locator('.unit[id="0"]').boundingBox();
      const placeholder = await page.locator('#placeholder-0').boundingBox();
      if (!ship || !placeholder) return Infinity;
      return (
        Math.abs(ship.x - placeholder.x) + Math.abs(ship.y - placeholder.y)
      );
    })
    .toBeLessThan(5);
  await expect(page.locator('#own-board')).toBeVisible();
  await expect(page.locator('#target-board')).toBeHidden();
  await expect(page.locator('#ready')).toBeDisabled();

  for (const [id, row, column] of [
    [0, 0, 0],
    [1, 2, 0],
    [2, 4, 0],
    [3, 6, 0],
    [4, 8, 0],
    [5, 0, 6],
    [6, 2, 6],
    [7, 4, 6],
    [8, 6, 6],
    [9, 8, 6],
  ]) {
    await dragShip(page, id, row, column);
  }
  await expect(page.locator('#ready')).toBeEnabled();
  await expect(page.locator('#match-status')).toHaveText('Fleet ready');

  // Reposition a placed ship after the page has scrolled.
  await page.evaluate(() => {
    document.body.style.minHeight = '1600px';
    window.scrollTo(0, 100);
  });
  expect(await page.evaluate(() => window.scrollY)).toBe(100);
  await dragShip(page, 0, 0, 1);
  await expect(page.locator('#own-board-0-0')).toHaveAttribute(
    'data-state',
    'o'
  );
  await page.evaluate(() => window.scrollTo(0, 0));

  await page.locator('#reset').click();
  await expect(page.locator('#own-board [data-state="_"]')).toHaveCount(100);
  await expect(page.locator('#ready')).toBeDisabled();
  await page.locator('#random').click();
  await expect(page.locator('#ready')).toBeEnabled();
  await expect(page.locator('#own-board [data-state="X"]')).toHaveCount(20);
  await page.locator('#reset').click();
  await expect(page.locator('#ready')).toBeDisabled();
  await page.locator('#random').click();
  await page.locator('#ready').click();
  await expect(page.locator('#target-board')).toBeVisible();
  await expect(page.locator('#setup-controls')).toBeHidden();
  await expect(page.locator('#match-status')).toHaveText(
    'Waiting for opponent...'
  );
  expect(errors).toEqual([]);
});
