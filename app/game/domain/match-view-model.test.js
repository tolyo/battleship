import { CellState } from './constants.js';
import { matchViewModelFromView } from './match-view-model.js';

describe('matchViewModelFromView', () => {
  it('uses allowed actions as the authoritative turn signal', () => {
    const viewModel = matchViewModelFromView({
      ...viewState(),
      allowed_actions: [{ action: 'move', target: 'opponent_board' }],
    });

    expect(viewModel?.isMyTurn).toBe(true);
    expect(viewModel?.status).toBe('Your turn');
  });

  it('waits when the server does not allow moves', () => {
    const viewModel = matchViewModelFromView({
      ...viewState(),
      allowed_actions: [],
    });

    expect(viewModel?.isMyTurn).toBe(false);
    expect(viewModel?.status).toBe("Awaiting opponent's move");
  });

  it('uses named player views from the server boundary', () => {
    const ownBoard = emptyBoard();
    const opponentBoard = emptyBoard();
    ownBoard[2][3] = '0';
    opponentBoard[5][5] = CellState.HIT;

    const viewModel = matchViewModelFromView({
      ...viewState(),
      own_player: { id: 'p1', board: ownBoard },
      opponent: { id: 'p2', board: opponentBoard },
    });

    expect(
      viewModel?.ownTiles.find((tile) => tile.row === 2 && tile.column === 3)
        ?.state
    ).toBe('unit');
    expect(
      viewModel?.targetTiles.find((tile) => tile.row === 5 && tile.column === 5)
        ?.state
    ).toBe('hit');
  });

  it('reports finished rooms from server phase', () => {
    const viewModel = matchViewModelFromView({
      ...viewState(),
      phase: 'finished',
      allowed_actions: [{ action: 'move', target: 'opponent_board' }],
    });

    expect(viewModel?.isMyTurn).toBe(false);
    expect(viewModel?.status).toBe('Room finished');
  });

  it('projects own unit coordinates for unit layout', () => {
    const ownBoard = emptyBoard();
    ownBoard[2][3] = '0';
    ownBoard[2][4] = '0';

    const viewModel = matchViewModelFromView({
      ...viewState(),
      own_player: { id: 'p1', board: ownBoard },
    });

    expect(viewModel?.unitCoordinatesById).toEqual({
      0: [
        { row: 2, column: 3 },
        { row: 2, column: 4 },
      ],
    });
  });

  it('marks fully sunk own hit clusters', () => {
    const ownBoard = emptyBoard();
    ownBoard[2][3] = CellState.HIT;
    ownBoard[2][4] = CellState.HIT;

    const viewModel = matchViewModelFromView({
      ...viewState(),
      own_player: { id: 'p1', board: ownBoard },
    });

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

function viewState(overrides = {}) {
  return {
    own_player: { id: 'p1', board: emptyBoard() },
    opponent: { id: 'p2', board: emptyBoard() },
    allowed_actions: [],
    ...overrides,
  };
}

function emptyBoard() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => CellState.EMPTY)
  );
}
