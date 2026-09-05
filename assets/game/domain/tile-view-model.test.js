import {
  ownTileViews,
  targetTileViews,
  unitCoordinatesById,
  sunkClusters,
} from './tile-view-model.js';
import { CellState } from './constants.js';

describe('tile view model', () => {
  it('reveals own units on own tiles', () => {
    const board = emptyBoard();
    board[1][2] = '7';

    const tile = ownTileViews(board).find(
      (candidate) => candidate.row === 1 && candidate.column === 2
    );

    expect(tile).toEqual({
      row: 1,
      column: 2,
      state: 'unit',
      unitId: '7',
    });
  });

  it('hides target units on target tiles', () => {
    const board = emptyBoard();
    board[1][2] = '7';

    const tile = targetTileViews(board).find(
      (candidate) => candidate.row === 1 && candidate.column === 2
    );

    expect(tile).toEqual({
      row: 1,
      column: 2,
      state: 'empty',
    });
  });

  it('maps hits, blocked cells, and misses to tile states', () => {
    const board = emptyBoard();
    board[0][0] = CellState.HIT;
    board[0][1] = CellState.BLOCKED;
    board[0][2] = CellState.MISS;
    board[0][3] = 'x';

    const tiles = targetTileViews(board);

    expect(tileAt(tiles, 0, 0).state).toBe('hit');
    expect(tileAt(tiles, 0, 1).state).toBe('hit');
    expect(tileAt(tiles, 0, 2).state).toBe('miss');
    expect(tileAt(tiles, 0, 3).state).toBe('miss');
  });

  it('groups unit coordinates by unit id', () => {
    const board = emptyBoard();
    board[2][0] = '1';
    board[2][1] = '1';
    board[4][5] = '8';

    expect(unitCoordinatesById(board)).toEqual({
      1: [
        { row: 2, column: 0 },
        { row: 2, column: 1 },
      ],
      8: [{ row: 4, column: 5 }],
    });
  });

  it('does not mark a hit cluster as sunk while touching a live unit cell', () => {
    const board = emptyBoard();
    board[2][2] = CellState.HIT;
    board[2][3] = '3';

    expect(sunkClusters(board)).toEqual([]);
  });

  it('detects vertical sunk clusters', () => {
    const board = emptyBoard();
    board[2][2] = CellState.HIT;
    board[3][2] = CellState.HIT;

    expect(sunkClusters(board)).toEqual([
      {
        coordinates: [
          { row: 2, column: 2 },
          { row: 3, column: 2 },
        ],
        orientation: 'vertical',
      },
    ]);
  });
});

function emptyBoard() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => CellState.EMPTY)
  );
}

function tileAt(tiles, row, column) {
  return tiles.find(
    (candidate) => candidate.row === row && candidate.column === column
  );
}
