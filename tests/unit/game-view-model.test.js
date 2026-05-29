import { MapTile } from '../../app/game/constants.js';
import { gameViewModelFromState } from '../../app/game/game-view-model.js';

describe('gameViewModelFromState', () => {
  it('uses current_turn when the server sends one', () => {
    const viewModel = gameViewModelFromState(
      gameState({
        current_turn: 'p1',
      }),
      'p1'
    );

    expect(viewModel?.isMyTurn).toBe(true);
    expect(viewModel?.status).toBe('Your turn');
  });

  it('keeps the turn with the player who made a hit', () => {
    const viewModel = gameViewModelFromState(
      gameState({
        turns: [{ id: 'p1', res: 'HIT' }],
      }),
      'p1'
    );

    expect(viewModel?.isMyTurn).toBe(true);
    expect(viewModel?.status).toBe('Your turn');
  });

  it('switches the turn after a miss', () => {
    const viewModel = gameViewModelFromState(
      gameState({
        turns: [{ id: 'p1', res: 'MISS' }],
      }),
      'p1'
    );

    expect(viewModel?.isMyTurn).toBe(false);
    expect(viewModel?.status).toBe("Awaiting opponent's move");
  });

  it('projects own ship coordinates for fleet layout', () => {
    const playerBoard = emptyBoard();
    playerBoard[2][3] = '0';
    playerBoard[2][4] = '0';

    const viewModel = gameViewModelFromState(
      gameState({
        player_one: { id: 'p1', board: playerBoard },
      }),
      'p1'
    );

    expect(viewModel?.shipCoordinatesById).toEqual({
      0: [
        { row: 2, column: 3 },
        { row: 2, column: 4 },
      ],
    });
  });

  it('marks fully sunk own hit clusters', () => {
    const playerBoard = emptyBoard();
    playerBoard[2][3] = MapTile.HIT;
    playerBoard[2][4] = MapTile.HIT;

    const viewModel = gameViewModelFromState(
      gameState({
        player_one: { id: 'p1', board: playerBoard },
      }),
      'p1'
    );

    expect(viewModel?.sunkClusters).toEqual([
      {
        coordinates: [
          { row: 2, column: 3 },
          { row: 2, column: 4 },
        ],
        orientation: 'horizontal',
      },
    ]);
  });
});

function gameState(overrides = {}) {
  return {
    player_one: { id: 'p1', board: emptyBoard() },
    player_two: { id: 'p2', board: emptyBoard() },
    first_turn: 'p1',
    turns: [],
    ...overrides,
  };
}

function emptyBoard() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => MapTile.EMPTY)
  );
}
