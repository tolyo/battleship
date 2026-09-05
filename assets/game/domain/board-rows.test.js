import {
  emptyTileRows,
  rowsFromTiles,
  rowsWithSunkState,
  setupRowsFromDataState,
} from './board-rows.js';
import { CellState } from './constants.js';

describe('emptyTileRows', () => {
  it('creates a decorated 10 by 10 empty grid', () => {
    const rows = emptyTileRows('target-board');

    expect(rows.length).toBe(10);
    expect(rows[0].length).toBe(10);
    expect(rows[3].id).toBe('target-board-row-3');
    expect(rows[3][4]).toEqual(
      jasmine.objectContaining({
        boardName: 'target-board',
        id: 'target-board-3-4',
        row: 3,
        column: 4,
        state: 'empty',
        dataState: CellState.EMPTY,
        sunk: false,
        classes: jasmine.objectContaining({
          placed: false,
          hit: false,
          miss: false,
        }),
      })
    );
  });
});

describe('setupRowsFromDataState', () => {
  it('keeps blocked setup cells as empty tiles with blocked data state', () => {
    const dataState = emptyDataState();
    dataState[0][0] = CellState.FILLED;
    dataState[1][0] = CellState.BLOCKED;

    const rows = setupRowsFromDataState(dataState);

    expect(rows[0][0].state).toBe('unit');
    expect(rows[0][0].dataState).toBe(CellState.FILLED);
    expect(rows[0][0].classes.placed).toBe(true);
    expect(rows[1][0].state).toBe('empty');
    expect(rows[1][0].dataState).toBe(CellState.BLOCKED);
  });

  it('marks previewed setup cells as droppable targets', () => {
    const rows = setupRowsFromDataState(emptyDataState(), [
      { row: 2, column: 3 },
      { row: 2, column: 4 },
    ]);

    expect(rows[2][3].preview).toBe(true);
    expect(rows[2][3].classes['droppable-target']).toBe(true);
    expect(rows[2][4].classes['droppable-target']).toBe(true);
    expect(rows[2][5].classes['droppable-target']).toBe(false);
  });

  it('defaults missing setup cells to empty state', () => {
    const rows = setupRowsFromDataState([]);

    expect(rows[0][0].state).toBe('empty');
    expect(rows[0][0].dataState).toBe(CellState.EMPTY);
  });
});

describe('rowsFromTiles', () => {
  it('decorates sparse tile view models with data states', () => {
    const rows = rowsFromTiles('target-board', [
      { row: 0, column: 0, state: 'unit', unitId: '0' },
      { row: 1, column: 0, state: 'hit' },
      { row: 2, column: 0, state: 'miss' },
    ]);

    expect(rows[0][0].dataState).toBe(CellState.FILLED);
    expect(rows[0][0].classes.placed).toBe(true);
    expect(rows[1][0].dataState).toBe(CellState.HIT);
    expect(rows[1][0].classes.hit).toBe(true);
    expect(rows[2][0].dataState).toBe(CellState.MISS);
    expect(rows[2][0].classes.miss).toBe(true);
    expect(rows[3][0].dataState).toBe(CellState.EMPTY);
  });
});

describe('rowsWithSunkState', () => {
  it('marks horizontal sunk cluster edges', () => {
    const rows = rowsWithSunkState(emptyTileRows('own-board'), [
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
    expect(rows[2][3].classes.sunk).toBe(true);
    expect(rows[2][3].sunkHorizontal).toBe(true);
    expect(rows[2][3].classes['sunk-horizontal']).toBe(true);
    expect(rows[2][3].sunkStart).toBe(true);
    expect(rows[2][4].sunkStart).toBe(false);
    expect(rows[2][4].sunkEnd).toBe(false);
    expect(rows[2][5].sunkEnd).toBe(true);
  });

  it('marks single-tile sunk clusters', () => {
    const rows = rowsWithSunkState(emptyTileRows('own-board'), [
      {
        coordinates: [{ row: 6, column: 7 }],
        orientation: 'vertical',
      },
    ]);

    expect(rows[6][7].sunk).toBe(true);
    expect(rows[6][7].sunkSingle).toBe(true);
    expect(rows[6][7].sunkVertical).toBe(false);
  });

  it('marks vertical sunk cluster edges in row order', () => {
    const rows = rowsWithSunkState(emptyTileRows('own-board'), [
      {
        coordinates: [
          { row: 5, column: 7 },
          { row: 3, column: 7 },
          { row: 4, column: 7 },
        ],
        orientation: 'vertical',
      },
    ]);

    expect(rows[3][7].sunkStart).toBe(true);
    expect(rows[4][7].sunkVertical).toBe(true);
    expect(rows[5][7].sunkEnd).toBe(true);
  });

  it('ignores sunk coordinates outside the rendered board', () => {
    const rows = emptyTileRows('own-board');

    expect(
      rowsWithSunkState(rows, [
        {
          coordinates: [{ row: 10, column: 10 }],
          orientation: 'vertical',
        },
      ])
    ).toBe(rows);
    expect(rows.flat().some((tile) => tile.sunk)).toBe(false);
  });
});

function emptyDataState() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => CellState.EMPTY)
  );
}
