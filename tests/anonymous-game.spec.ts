import { expect, test, type Page } from '@playwright/test';

const GRID_SIZE = 10;
const MAX_SHOTS_PER_PLAYER = GRID_SIZE * GRID_SIZE;

type Player = {
  name: string;
  page: Page;
  nextShot: number;
};

async function joinAnonymousGame(player: Player) {
  await player.page.goto('/room', { waitUntil: 'networkidle' });
  await player.page.getByRole('button', { name: 'Random' }).click();
  await expect(player.page.locator('#ready')).toBeEnabled();
  await player.page.getByRole('button', { name: 'Ready' }).click();
  await expect(player.page.locator('#hitboard')).toBeVisible();
}

async function status(player: Player) {
  return player.page.locator('#match-status').textContent();
}

async function hitboardEnabled(player: Player) {
  return player.page
    .locator('#hitboard')
    .evaluate((hitboard) => !hitboard.classList.contains('disabled'));
}

async function waitForInitialTurns(players: Player[]) {
  await expect
    .poll(async () => Promise.all(players.map(status)))
    .toEqual(expect.arrayContaining(['Your turn', "Awaiting opponent's move"]));
}

async function activePlayer(players: Player[]) {
  await expect
    .poll(async () => {
      if (await gameFinished(players)) {
        return 2;
      }

      const enabled = await Promise.all(players.map(hitboardEnabled));
      return enabled.filter(Boolean).length === 1 ? 1 : 0;
    })
    .toBeGreaterThan(0);

  for (const player of players) {
    if (await hitboardEnabled(player)) {
      return player;
    }
  }

  return undefined;
}

async function gameFinished(players: Player[]) {
  const statuses = await Promise.all(players.map(status));
  return statuses.every((value) => value === 'Game finished');
}

async function clickNextRowMajorTarget(player: Player) {
  if (player.nextShot >= MAX_SHOTS_PER_PLAYER) {
    throw new Error(`${player.name} exhausted every grid target.`);
  }

  const row = Math.floor(player.nextShot / GRID_SIZE);
  const column = player.nextShot % GRID_SIZE;
  player.nextShot += 1;
  await player.page.locator(`#hitboard-${row}-${column}`).click();
}

test.describe('anonymous game', () => {
  test.skip(
    ({ browserName }) => browserName !== 'chromium',
    'Full game simulation is covered in Chromium only.'
  );

  test('two anonymous players can play a complete row-major game', async ({
    browser,
  }) => {
    test.setTimeout(120_000);

    const playerOneContext = await browser.newContext();
    const playerTwoContext = await browser.newContext();
    const players: Player[] = [
      {
        name: 'player one',
        page: await playerOneContext.newPage(),
        nextShot: 0,
      },
      {
        name: 'player two',
        page: await playerTwoContext.newPage(),
        nextShot: 0,
      },
    ];

    try {
      await Promise.all(players.map(joinAnonymousGame));
      await waitForInitialTurns(players);

      for (let move = 0; move < MAX_SHOTS_PER_PLAYER * players.length; move += 1) {
        if (await gameFinished(players)) {
          break;
        }

        const player = await activePlayer(players);
        if (!player) {
          break;
        }

        await clickNextRowMajorTarget(player);
      }

      await expect
        .poll(async () => Promise.all(players.map(status)))
        .toEqual(['Game finished', 'Game finished']);
    } finally {
      await Promise.all([
        playerOneContext.close(),
        playerTwoContext.close(),
      ]);
    }
  });
});
