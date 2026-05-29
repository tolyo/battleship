import { GRID, MapTile } from './constants.js';

/**
 * @param {unknown[][]} board
 * @returns {import('./game-view-model.js').BoardTileView[]}
 */
export function fleetTileViews(board) {
  return tileViews(board, true);
}

/**
 * @param {unknown[][]} board
 * @returns {import('./game-view-model.js').BoardTileView[]}
 */
export function hitTileViews(board) {
  return tileViews(board, false);
}

/**
 * @param {unknown[][]} board
 * @returns {Record<string, import('./game-view-model.js').Coordinate[]>}
 */
export function shipCoordinatesById(board) {
  const coordinatesByShipId =
    /** @type {Record<string, import('./game-view-model.js').Coordinate[]>} */ (
      {}
    );

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const cell = board[row]?.[column];
      if (!isShipCell(cell)) {
        return;
      }

      coordinatesByShipId[cell] ??= [];
      coordinatesByShipId[cell].push({ row, column });
    });
  });

  return coordinatesByShipId;
}

/**
 * @param {unknown[][]} board
 * @returns {import('./game-view-model.js').SunkClusterView[]}
 */
export function sunkClusters(board) {
  /** @type {Set<string>} */
  const visited = new Set();
  /** @type {import('./game-view-model.js').SunkClusterView[]} */
  const clusters = [];

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      if (
        visited.has(tileKey(row, column)) ||
        board[row]?.[column] !== MapTile.HIT
      ) {
        return;
      }

      const coordinates = collectHitCluster(board, row, column, visited);
      if (!hasAdjacentShipCell(board, coordinates)) {
        clusters.push({
          coordinates,
          orientation: clusterOrientation(coordinates),
        });
      }
    });
  });

  return clusters;
}

/**
 * @param {unknown[][]} board
 * @param {boolean} revealShips
 * @returns {import('./game-view-model.js').BoardTileView[]}
 */
function tileViews(board, revealShips) {
  return GRID.flatMap((row) =>
    GRID.map((column) => {
      const cell = board[row]?.[column];
      if (isShipCell(cell) && revealShips) {
        return {
          row,
          column,
          state: /** @type {const} */ ('ship'),
          shipId: cell,
        };
      }
      if (cell === MapTile.HIT || cell === MapTile.BLOCKED) {
        return { row, column, state: /** @type {const} */ ('hit') };
      }
      if (cell === MapTile.MISS || cell === 'x') {
        return { row, column, state: /** @type {const} */ ('miss') };
      }
      return { row, column, state: /** @type {const} */ ('empty') };
    })
  );
}

/**
 * @param {unknown[][]} board
 * @param {number} startRow
 * @param {number} startColumn
 * @param {Set<string>} visited
 * @returns {import('./game-view-model.js').Coordinate[]}
 */
function collectHitCluster(board, startRow, startColumn, visited) {
  /** @type {import('./game-view-model.js').Coordinate[]} */
  const cluster = [];
  /** @type {import('./game-view-model.js').Coordinate[]} */
  const pending = [{ row: startRow, column: startColumn }];

  while (pending.length > 0) {
    const coordinate = pending.pop();
    if (
      coordinate &&
      !visited.has(tileKey(coordinate.row, coordinate.column)) &&
      board[coordinate.row]?.[coordinate.column] === MapTile.HIT
    ) {
      visited.add(tileKey(coordinate.row, coordinate.column));
      cluster.push(coordinate);
      adjacentCoordinates(coordinate.row, coordinate.column).forEach(
        (adjacent) => {
          if (board[adjacent.row]?.[adjacent.column] === MapTile.HIT) {
            pending.push(adjacent);
          }
        }
      );
    }
  }

  return cluster;
}

/**
 * @param {unknown[][]} board
 * @param {import('./game-view-model.js').Coordinate[]} cluster
 * @returns {boolean}
 */
function hasAdjacentShipCell(board, cluster) {
  return cluster.some((coordinate) =>
    adjacentCoordinates(coordinate.row, coordinate.column).some((adjacent) =>
      isShipCell(board[adjacent.row]?.[adjacent.column])
    )
  );
}

/**
 * @param {import('./game-view-model.js').Coordinate[]} cluster
 * @returns {'horizontal' | 'vertical'}
 */
function clusterOrientation(cluster) {
  return cluster.length > 1 &&
    cluster.every((coordinate) => coordinate.row === cluster[0].row)
    ? 'horizontal'
    : 'vertical';
}

/**
 * @param {unknown} value
 * @returns {value is string}
 */
function isShipCell(value) {
  return typeof value === 'string' && /^[0-9]$/.test(value);
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {import('./game-view-model.js').Coordinate[]}
 */
function adjacentCoordinates(row, column) {
  return [
    { row: row - 1, column },
    { row: row + 1, column },
    { row, column: column - 1 },
    { row, column: column + 1 },
  ].filter(
    (coordinate) =>
      coordinate.row >= 0 &&
      coordinate.row <= 9 &&
      coordinate.column >= 0 &&
      coordinate.column <= 9
  );
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {string}
 */
function tileKey(row, column) {
  return `${row}:${column}`;
}
