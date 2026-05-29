import {
  fleetTileViews,
  hitTileViews,
  shipCoordinatesById,
  sunkClusters,
} from '../../app/game/board-view-model.js';
import { MapTile } from '../../app/game/constants.js';

describe('board view model', () => {
  it('reveals own ships on fleet tiles', () => {
    const board = emptyBoard();
    board[1][2] = '7';

    const tile = fleetTileViews(board).find(
      (candidate) => candidate.row === 1 && candidate.column === 2
    );

    expect(tile).toEqual({
      row: 1,
      column: 2,
      state: 'ship',
      shipId: '7',
    });
  });

  it('hides opponent ships on hit tiles', () => {
    const board = emptyBoard();
    board[1][2] = '7';

    const tile = hitTileViews(board).find(
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
    board[0][0] = MapTile.HIT;
    board[0][1] = MapTile.BLOCKED;
    board[0][2] = MapTile.MISS;
    board[0][3] = 'x';

    const tiles = hitTileViews(board);

    expect(tileAt(tiles, 0, 0).state).toBe('hit');
    expect(tileAt(tiles, 0, 1).state).toBe('hit');
    expect(tileAt(tiles, 0, 2).state).toBe('miss');
    expect(tileAt(tiles, 0, 3).state).toBe('miss');
  });

  it('groups ship coordinates by ship id', () => {
    const board = emptyBoard();
    board[2][0] = '1';
    board[2][1] = '1';
    board[4][5] = '8';

    expect(shipCoordinatesById(board)).toEqual({
      1: [
        { row: 2, column: 0 },
        { row: 2, column: 1 },
      ],
      8: [{ row: 4, column: 5 }],
    });
  });

  it('does not mark a hit cluster as sunk while touching a live ship cell', () => {
    const board = emptyBoard();
    board[2][2] = MapTile.HIT;
    board[2][3] = '3';

    expect(sunkClusters(board)).toEqual([]);
  });

  it('detects vertical sunk clusters', () => {
    const board = emptyBoard();
    board[2][2] = MapTile.HIT;
    board[3][2] = MapTile.HIT;

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
    Array.from({ length: 10 }, () => MapTile.EMPTY)
  );
}

function tileAt(tiles, row, column) {
  return tiles.find(
    (candidate) => candidate.row === row && candidate.column === column
  );
}
