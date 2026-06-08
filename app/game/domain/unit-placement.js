import Units from './unit-catalog.js';
import { UNIT_CELLS, GRID, CellState } from './constants.js';
import { emptyBoardState } from './board-state.js';

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 * @returns {{
 *   boardState: string[][],
 *   tileDataState: string[][],
 *   complete: boolean,
 *   placedUnitCells: number
 * }}
 */
export function boardStateFromPlacements(placements) {
  const boardState = emptyBoardState();
  const tileDataState = emptyBoardState();
  const occupiedCells = new Set();
  let placedUnitCells = 0;
  let complete = true;

  Units.forEach((unit) => {
    const coordinates = placements[unit.id] ?? [];
    if (coordinates.length !== unit.size) {
      complete = false;
    }

    coordinates.forEach((coordinate) => {
      if (!isCoordinate(coordinate)) {
        complete = false;
        return;
      }

      const occupiedKey = tileKey(coordinate.row, coordinate.column);
      if (occupiedCells.has(occupiedKey)) {
        complete = false;
        return;
      }

      occupiedCells.add(occupiedKey);
      boardState[coordinate.row][coordinate.column] = unit.id;
      tileDataState[coordinate.row][coordinate.column] = CellState.FILLED;
      placedUnitCells += 1;
    });
  });

  occupiedCells.forEach((key) => {
    const [row, column] = key.split(':').map(Number);
    adjacentCoordinates(row, column).forEach((adjacent) => {
      if (tileDataState[adjacent.row][adjacent.column] === CellState.EMPTY) {
        tileDataState[adjacent.row][adjacent.column] = CellState.BLOCKED;
      }
    });
  });

  return { boardState, tileDataState, complete, placedUnitCells };
}

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 * @returns {boolean}
 */
export function allUnitsPlaced(placements) {
  const { complete, placedUnitCells } = boardStateFromPlacements(placements);
  return complete && placedUnitCells === UNIT_CELLS;
}

/**
 * @returns {Record<string, import('./match-view-model.js').Coordinate[]>}
 */
export function randomUnitPlacements() {
  const nextBoardState = emptyBoardState();
  /** @type {Record<string, import('./match-view-model.js').Coordinate[]>} */
  const nextPlacements = {};

  Units.forEach((unit) => {
    const coordinates = findRandomUnitPlacement(unit.size, nextBoardState);
    if (!coordinates) {
      throw new Error(`Unable to place unit ${unit.id}`);
    }

    coordinates.forEach((coordinate) => {
      nextBoardState[coordinate.row][coordinate.column] = unit.id;
    });
    nextPlacements[unit.id] = coordinates;
  });

  return nextPlacements;
}

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 * @returns {boolean}
 */
export function canPlaceSetupUnit(placements, unitId, coordinates) {
  const unit = Units.find((candidate) => candidate.id === unitId);
  if (!unit || coordinates.length !== unit.size) {
    return false;
  }

  const board = boardStateFromPlacements({
    ...placements,
    [unitId]: [],
  }).boardState;

  return canPlaceOnBoard(board, coordinates);
}

/**
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[]}
 */
export function unitCoordinatesFromStart(row, column, orientation, size) {
  const y = parseInt(row, 10);
  const x = parseInt(column, 10);
  if (!Number.isInteger(y) || !Number.isInteger(x)) {
    return [];
  }

  return GRID.slice(0, size).map((offset) => ({
    row: orientation === 'VERTICAL' ? y + offset : y,
    column: orientation === 'HORIZONTAL' ? x + offset : x,
  }));
}

/**
 * @param {unknown} coordinate
 * @returns {coordinate is import('./match-view-model.js').Coordinate}
 */
function isCoordinate(coordinate) {
  return (
    typeof coordinate === 'object' &&
    coordinate !== null &&
    Number.isInteger(/** @type {{ row?: unknown }} */ (coordinate).row) &&
    Number.isInteger(/** @type {{ column?: unknown }} */ (coordinate).column) &&
    /** @type {{ row: number }} */ (coordinate).row >= 0 &&
    /** @type {{ row: number }} */ (coordinate).row <= 9 &&
    /** @type {{ column: number }} */ (coordinate).column >= 0 &&
    /** @type {{ column: number }} */ (coordinate).column <= 9
  );
}

/**
 * @param {string[][]} boardState
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 * @returns {boolean}
 */
function canPlaceOnBoard(boardState, coordinates) {
  if (coordinates.length === 0) {
    return false;
  }

  return coordinates.every((coordinate) => {
    if (!isCoordinate(coordinate)) {
      return false;
    }

    if (boardState[coordinate.row][coordinate.column] !== CellState.EMPTY) {
      return false;
    }

    return adjacentCoordinates(coordinate.row, coordinate.column).every(
      (adjacent) => boardState[adjacent.row][adjacent.column] === CellState.EMPTY
    );
  });
}

/**
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[]}
 */
function randomUnitCoordinates(size) {
  const orientation = Math.round(Math.random()) > 0 ? 'HORIZONTAL' : 'VERTICAL';
  const maxRow = orientation === 'VERTICAL' ? 10 - size : 9;
  const maxColumn = orientation === 'HORIZONTAL' ? 10 - size : 9;
  const row = randomInteger(maxRow);
  const column = randomInteger(maxColumn);

  return GRID.slice(0, size).map((offset) => ({
    row: orientation === 'VERTICAL' ? row + offset : row,
    column: orientation === 'HORIZONTAL' ? column + offset : column,
  }));
}

/**
 * @param {number} size
 * @param {string[][]} boardState
 * @returns {import('./match-view-model.js').Coordinate[] | undefined}
 */
function findRandomUnitPlacement(size, boardState) {
  return Array.from({ length: 1000 }, () => randomUnitCoordinates(size)).find(
    (coordinates) => canPlaceOnBoard(boardState, coordinates)
  );
}

/**
 * @param {number} max
 * @returns {number}
 */
function randomInteger(max) {
  return Math.floor(Math.random() * (max + 1));
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {string}
 */
function tileKey(row, column) {
  return `${row}:${column}`;
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {{ row: number, column: number }[]}
 */
function adjacentCoordinates(row, column) {
  return [
    { row: row - 1, column: column - 1 },
    { row: row - 1, column },
    { row: row - 1, column: column + 1 },
    { row, column: column - 1 },
    { row, column: column + 1 },
    { row: row + 1, column: column - 1 },
    { row: row + 1, column },
    { row: row + 1, column: column + 1 },
  ].filter(
    (coordinate) =>
      coordinate.row >= 0 &&
      coordinate.row <= 9 &&
      coordinate.column >= 0 &&
      coordinate.column <= 9
  );
}
