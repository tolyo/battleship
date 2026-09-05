import { GRID, CellState } from './constants.js';

/**
 * @typedef {import('./match-view-model.js').BoardTileView & {
 *   id: string,
 *   boardName: string,
 *   dataState: string,
 *   classes: Record<string, boolean>,
 *   preview: boolean,
 *   sunk: boolean,
 *   sunkHorizontal: boolean,
 *   sunkVertical: boolean,
 *   sunkSingle: boolean,
 *   sunkStart: boolean,
 *   sunkEnd: boolean
 * }} BoardGridTile
 */

/**
 * @typedef {BoardGridTile[] & { id: string }} BoardGridRow
 */

/**
 * @param {string} boardName
 * @returns {BoardGridRow[]}
 */
export function emptyTileRows(boardName) {
  return GRID.map((row) =>
    rowFromTiles(
      boardName,
      row,
      GRID.map((column) =>
        decorateTile(boardName, { row, column, state: 'empty' })
      )
    )
  );
}

/**
 * @param {string[][]} dataState
 * @param {import('./match-view-model.js').Coordinate[]=} previewCoordinates
 * @returns {BoardGridRow[]}
 */
export function setupRowsFromDataState(dataState, previewCoordinates = []) {
  const previewKeys = new Set(
    previewCoordinates.map(
      (coordinate) => `${coordinate.row}:${coordinate.column}`
    )
  );

  return GRID.map((row) =>
    rowFromTiles(
      'own-board',
      row,
      GRID.map((column) => {
        const state = dataState[row]?.[column] ?? CellState.EMPTY;
        return decorateTile(
          'own-board',
          {
            row,
            column,
            state: state === CellState.FILLED ? 'unit' : 'empty',
          },
          state,
          previewKeys.has(`${row}:${column}`)
        );
      })
    )
  );
}

/**
 * @param {string} boardName
 * @param {import('./match-view-model.js').BoardTileView[]} tiles
 * @returns {BoardGridRow[]}
 */
export function rowsFromTiles(boardName, tiles) {
  return GRID.map((row) =>
    rowFromTiles(
      boardName,
      row,
      GRID.map((column) => {
        const tile = tiles.find(
          (candidate) => candidate.row === row && candidate.column === column
        );
        return decorateTile(
          boardName,
          tile ?? { row, column, state: 'empty' },
          dataStateForTile(tile)
        );
      })
    )
  );
}

/**
 * @param {BoardGridRow[]} rows
 * @param {import('./match-view-model.js').SunkClusterView[]} clusters
 * @returns {BoardGridRow[]}
 */
export function rowsWithSunkState(rows, clusters) {
  const tileByKey = new Map(
    rows.flatMap((row) =>
      row.map((tile) => [`${tile.row}:${tile.column}`, tile])
    )
  );

  clusters.forEach((cluster) => {
    const sorted = [...cluster.coordinates].sort((left, right) =>
      cluster.orientation === 'horizontal'
        ? left.column - right.column
        : left.row - right.row
    );

    sorted.forEach((coordinate, index) => {
      const tile = tileByKey.get(`${coordinate.row}:${coordinate.column}`);
      if (!tile) {
        return;
      }

      tile.sunk = true;
      tile.sunkSingle = sorted.length === 1;
      tile.sunkHorizontal =
        sorted.length > 1 && cluster.orientation === 'horizontal';
      tile.sunkVertical =
        sorted.length > 1 && cluster.orientation === 'vertical';
      tile.sunkStart = index === 0;
      tile.sunkEnd = index === sorted.length - 1;
      tile.classes = tileClasses(tile);
    });
  });

  return rows;
}

/**
 * @param {import('./match-view-model.js').BoardTileView | undefined} tile
 * @returns {string}
 */
function dataStateForTile(tile) {
  if (!tile) {
    return CellState.EMPTY;
  }

  if (tile.state === 'unit') {
    return CellState.FILLED;
  }

  if (tile.state === 'hit') {
    return CellState.HIT;
  }

  if (tile.state === 'miss') {
    return CellState.MISS;
  }

  return CellState.EMPTY;
}

/**
 * @param {string} boardName
 * @param {import('./match-view-model.js').BoardTileView} tile
 * @param {string=} dataState
 * @param {boolean=} preview
 * @returns {BoardGridTile}
 */
function decorateTile(
  boardName,
  tile,
  dataState = dataStateForTile(tile),
  preview = false
) {
  /** @type {Omit<BoardGridTile, 'classes'>} */
  const baseTile = {
    ...tile,
    id: `${boardName}-${tile.row}-${tile.column}`,
    boardName,
    dataState,
    preview,
    sunk: false,
    sunkHorizontal: false,
    sunkVertical: false,
    sunkSingle: false,
    sunkStart: false,
    sunkEnd: false,
  };

  return {
    ...baseTile,
    classes: tileClasses(baseTile),
  };
}

/**
 * @param {string} boardName
 * @param {number} row
 * @param {BoardGridTile[]} tiles
 * @returns {BoardGridRow}
 */
function rowFromTiles(boardName, row, tiles) {
  return Object.assign(tiles, { id: `${boardName}-row-${row}` });
}

/**
 * @param {Omit<BoardGridTile, 'classes'>} tile
 * @returns {Record<string, boolean>}
 */
function tileClasses(tile) {
  return {
    placed: tile.state === 'unit',
    hit: tile.state === 'hit',
    miss: tile.state === 'miss',
    'droppable-target': tile.preview,
    sunk: tile.sunk,
    'sunk-horizontal': tile.sunkHorizontal,
    'sunk-vertical': tile.sunkVertical,
    'sunk-single': tile.sunkSingle,
    'sunk-start': tile.sunkStart,
    'sunk-end': tile.sunkEnd,
  };
}
