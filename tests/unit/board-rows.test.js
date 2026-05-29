import {
  emptyTileRows,
  rowsFromTiles,
  rowsWithSunkState,
  setupRowsFromDataState,
} from '../../app/game/board-rows.js';
import { MapTile } from '../../app/game/constants.js';

describe('emptyTileRows', () => {
  it('creates a decorated 10 by 10 empty grid', () => {
    const rows = emptyTileRows('hitboard');

    expect(rows.length).toBe(10);
    expect(rows[0].length).toBe(10);
    expect(rows[3][4]).toEqual(
      jasmine.objectContaining({
        boardName: 'hitboard',
        row: 3,
        column: 4,
        state: 'empty',
        dataState: MapTile.EMPTY,
        sunk: false,
      })
    );
  });
});

describe('setupRowsFromDataState', () => {
  it('keeps blocked setup cells as empty tiles with blocked data state', () => {
    const dataState = emptyDataState();
    dataState[0][0] = MapTile.FILLED;
    dataState[1][0] = MapTile.BLOCKED;

    const rows = setupRowsFromDataState(dataState);

    expect(rows[0][0].state).toBe('ship');
    expect(rows[0][0].dataState).toBe(MapTile.FILLED);
    expect(rows[1][0].state).toBe('empty');
    expect(rows[1][0].dataState).toBe(MapTile.BLOCKED);
  });
});

describe('rowsFromTiles', () => {
  it('decorates sparse tile view models with data states', () => {
    const rows = rowsFromTiles('hitboard', [
      { row: 0, column: 0, state: 'ship', shipId: '0' },
      { row: 1, column: 0, state: 'hit' },
      { row: 2, column: 0, state: 'miss' },
    ]);

    expect(rows[0][0].dataState).toBe(MapTile.FILLED);
    expect(rows[1][0].dataState).toBe(MapTile.HIT);
    expect(rows[2][0].dataState).toBe(MapTile.MISS);
    expect(rows[3][0].dataState).toBe(MapTile.EMPTY);
  });
});

describe('rowsWithSunkState', () => {
  it('marks horizontal sunk cluster edges', () => {
    const rows = rowsWithSunkState(emptyTileRows('fleetboard'), [
      {
        coordinates: [
          { row: 2, column: 3 },
          { row: 2, column: 4 },
          { row: 2, column: 5 },
        ],
        orientation: 'horizontal',
      },
    ]);

    expect(rows[2][3].sunk).toBe(true);
    expect(rows[2][3].sunkHorizontal).toBe(true);
    expect(rows[2][3].sunkStart).toBe(true);
    expect(rows[2][4].sunkStart).toBe(false);
    expect(rows[2][4].sunkEnd).toBe(false);
    expect(rows[2][5].sunkEnd).toBe(true);
  });

  it('marks single-tile sunk clusters', () => {
    const rows = rowsWithSunkState(emptyTileRows('fleetboard'), [
      {
        coordinates: [{ row: 6, column: 7 }],
        orientation: 'vertical',
      },
    ]);

    expect(rows[6][7].sunk).toBe(true);
    expect(rows[6][7].sunkSingle).toBe(true);
    expect(rows[6][7].sunkVertical).toBe(false);
  });
});

function emptyDataState() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => MapTile.EMPTY)
  );
}
