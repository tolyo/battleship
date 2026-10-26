import { expect, test, type Page } from '@playwright/test';

const GRID_SIZE = 10;
const MAX_SHOTS_PER_PLAYER = GRID_SIZE * GRID_SIZE;

type Player = {
  name: string;
  page: Page;
  nextShot: number;
};

async function joinAnonymousGame(player: Player) {
  await player.page.route('https://fonts.googleapis.com/**', (route) =>
    route.abort()
  );
  await player.page.goto('/room', { waitUntil: 'domcontentloaded' });
  await player.page.getByRole('button', { name: 'Random' }).click();
  await expect(player.page.locator('#ready')).toBeEnabled();
  await player.page.getByRole('button', { name: 'Ready' }).click();
  await expect(player.page.locator('#target-board')).toBeVisible();
}

async function status(player: Player) {
  return player.page.locator('#match-status').textContent();
}

async function targetBoardEnabled(player: Player) {
  return player.page
    .locator('#target-board')
    .evaluate((targetBoard) => !targetBoard.classList.contains('disabled'));
}

async function waitForInitialTurns(players: Player[]) {
  await expect
    .poll(async () => Promise.all(players.map(status)))
    .toEqual(expect.arrayContaining(['Your turn', "Awaiting opponent's move"]));
}

async function activePlayer(players: Player[]) {
  await expect
    .poll(
      async () => {
        if (await gameFinished(players)) {
          return 2;
        }

        const enabled = await Promise.all(players.map(targetBoardEnabled));
        return enabled.filter(Boolean).length === 1 ? 1 : 0;
      },
      { timeout: 30_000 }
    )
    .toBeGreaterThan(0);

  for (const player of players) {
    if (await targetBoardEnabled(player)) {
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
  await player.page.locator(`#target-board-${row}-${column}`).click();
}

test.describe('anonymous game', () => {
  test.skip(
    ({ browserName }) => browserName !== 'chromium',
    'Full game simulation is covered in Chromium only.'
  );

  test('two anonymous players can play a complete row-major game', async ({
    browser,
  }) => {
    test.setTimeout(600_000);

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

    for (const player of players) {
      player.page.on('websocket', (socket) => {
        let submittedAt = 0;
        socket.on('framesent', ({ payload }) => {
          const message = JSON.parse(payload.toString());
          if (message.type === 'move') submittedAt = Date.now();
        });
        socket.on('framereceived', ({ payload }) => {
          const message = JSON.parse(payload.toString());
          if (message.type === 'room_update' && submittedAt) {
            const elapsed = Date.now() - submittedAt;
            if (elapsed > 4000)
              console.info(`${player.name} move reply took ${elapsed}ms`);
            submittedAt = 0;
          }
        });
      });
    }

    try {
      await Promise.all(players.map(joinAnonymousGame));
      await waitForInitialTurns(players);

      for (
        let move = 0;
        move < MAX_SHOTS_PER_PLAYER * players.length;
        move += 1
      ) {
        if (await gameFinished(players)) {
          break;
        }

        const player = await activePlayer(players);
        if (!player) {
          break;
        }

        await clickNextRowMajorTarget(player);
        if ((move + 1) % 20 === 0) {
          console.info(`Completed ${move + 1} moves`);
        }
      }

      await expect
        .poll(async () => Promise.all(players.map(status)))
        .toEqual(['Game finished', 'Game finished']);
    } catch (error) {
      console.info(
        await Promise.all(
          players.map(async (player) => ({
            name: player.name,
            shots: player.nextShot,
            status: await status(player),
          }))
        )
      );
      throw error;
    } finally {
      await Promise.all([playerOneContext.close(), playerTwoContext.close()]);
    }
  });
});
