import { GRID, CellState } from './constants.js';

/**
 * @param {unknown[][]} board
 * @returns {import('./match-view-model.js').BoardTileView[]}
 */
export function ownTileViews(board) {
  return tileViews(board, true);
}

/**
 * @param {unknown[][]} board
 * @returns {import('./match-view-model.js').BoardTileView[]}
 */
export function targetTileViews(board) {
  return tileViews(board, false);
}

/**
 * @param {unknown[][]} board
 * @returns {Record<string, import('./match-view-model.js').Coordinate[]>}
 */
export function unitCoordinatesById(board) {
  const coordinatesByUnitId =
    /** @type {Record<string, import('./match-view-model.js').Coordinate[]>} */ (
      {}
    );

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const cell = board[row]?.[column];
      if (!isUnitCell(cell)) {
        return;
      }

      coordinatesByUnitId[cell] ??= [];
      coordinatesByUnitId[cell].push({ row, column });
    });
  });

  return coordinatesByUnitId;
}

/**
 * @param {unknown[][]} board
 * @returns {import('./match-view-model.js').SunkClusterView[]}
 */
export function sunkClusters(board) {
  /** @type {Set<string>} */
  const visited = new Set();
  /** @type {import('./match-view-model.js').SunkClusterView[]} */
  const clusters = [];

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      if (
        visited.has(tileKey(row, column)) ||
        board[row]?.[column] !== CellState.HIT
      ) {
        return;
      }

      const coordinates = collectHitCluster(board, row, column, visited);
      if (!hasAdjacentUnitCell(board, coordinates)) {
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
 * @param {boolean} revealUnits
 * @returns {import('./match-view-model.js').BoardTileView[]}
 */
function tileViews(board, revealUnits) {
  return GRID.flatMap((row) =>
    GRID.map((column) => {
      const cell = board[row]?.[column];
      if (isUnitCell(cell) && revealUnits) {
        return {
          row,
          column,
          state: /** @type {const} */ ('unit'),
          unitId: cell,
        };
      }
      if (cell === CellState.HIT || cell === CellState.BLOCKED) {
        return { row, column, state: /** @type {const} */ ('hit') };
      }
      if (cell === CellState.MISS || cell === 'x') {
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
 * @returns {import('./match-view-model.js').Coordinate[]}
 */
function collectHitCluster(board, startRow, startColumn, visited) {
  /** @type {import('./match-view-model.js').Coordinate[]} */
  const cluster = [];
  /** @type {import('./match-view-model.js').Coordinate[]} */
  const pending = [{ row: startRow, column: startColumn }];

  while (pending.length > 0) {
    const coordinate = pending.pop();
    if (
      coordinate &&
      !visited.has(tileKey(coordinate.row, coordinate.column)) &&
      board[coordinate.row]?.[coordinate.column] === CellState.HIT
    ) {
      visited.add(tileKey(coordinate.row, coordinate.column));
      cluster.push(coordinate);
      adjacentCoordinates(coordinate.row, coordinate.column).forEach(
        (adjacent) => {
          if (board[adjacent.row]?.[adjacent.column] === CellState.HIT) {
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
 * @param {import('./match-view-model.js').Coordinate[]} cluster
 * @returns {boolean}
 */
function hasAdjacentUnitCell(board, cluster) {
  return cluster.some((coordinate) =>
    adjacentCoordinates(coordinate.row, coordinate.column).some((adjacent) =>
      isUnitCell(board[adjacent.row]?.[adjacent.column])
    )
  );
}

/**
 * @param {import('./match-view-model.js').Coordinate[]} cluster
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
function isUnitCell(value) {
  return typeof value === 'string' && /^[0-9]$/.test(value);
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {import('./match-view-model.js').Coordinate[]}
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
