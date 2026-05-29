import { GRID, MapTile } from './constants.js';

/**
 * @typedef {import('../map/game-view-model.js').BoardTileView & {
 *   boardName: string,
 *   dataState: string,
 *   sunk: boolean,
 *   sunkHorizontal: boolean,
 *   sunkVertical: boolean,
 *   sunkSingle: boolean,
 *   sunkStart: boolean,
 *   sunkEnd: boolean
 * }} BoardGridTile
 */

/**
 * @param {string} boardName
 * @returns {BoardGridTile[][]}
 */
export function emptyTileRows(boardName) {
  return GRID.map((row) =>
    GRID.map((column) =>
      decorateTile(boardName, { row, column, state: 'empty' })
    )
  );
}

/**
 * @param {string[][]} dataState
 * @returns {BoardGridTile[][]}
 */
export function setupRowsFromDataState(dataState) {
  return GRID.map((row) =>
    GRID.map((column) => {
      const state = dataState[row]?.[column] ?? MapTile.EMPTY;
      return decorateTile(
        'fleetboard',
        {
          row,
          column,
          state: state === MapTile.FILLED ? 'ship' : 'empty',
        },
        state
      );
    })
  );
}

/**
 * @param {string} boardName
 * @param {import('../map/game-view-model.js').BoardTileView[]} tiles
 * @returns {BoardGridTile[][]}
 */
export function rowsFromTiles(boardName, tiles) {
  return GRID.map((row) =>
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
  );
}

/**
 * @param {BoardGridTile[][]} rows
 * @param {import('../map/game-view-model.js').SunkClusterView[]} clusters
 * @returns {BoardGridTile[][]}
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
    });
  });

  return rows;
}

/**
 * @param {import('../map/game-view-model.js').BoardTileView | undefined} tile
 * @returns {string}
 */
function dataStateForTile(tile) {
  if (!tile) {
    return MapTile.EMPTY;
  }

  if (tile.state === 'ship') {
    return MapTile.FILLED;
  }

  if (tile.state === 'hit') {
    return MapTile.HIT;
  }

  if (tile.state === 'miss') {
    return MapTile.MISS;
  }

  return MapTile.EMPTY;
}

/**
 * @param {string} boardName
 * @param {import('../map/game-view-model.js').BoardTileView} tile
 * @param {string=} dataState
 * @returns {BoardGridTile}
 */
function decorateTile(boardName, tile, dataState = dataStateForTile(tile)) {
  return {
    ...tile,
    boardName,
    dataState,
    sunk: false,
    sunkHorizontal: false,
    sunkVertical: false,
    sunkSingle: false,
    sunkStart: false,
    sunkEnd: false,
  };
}
