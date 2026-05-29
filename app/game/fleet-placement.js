import Fleet from './fleet.js';
import { FLEET_SIZE, GRID, MapTile } from './constants.js';
import { emptyBoardState } from './board-state.js';

/**
 * @param {Record<string, import('./game-view-model.js').Coordinate[]>} placements
 * @returns {{
 *   boardState: string[][],
 *   tileDataState: string[][],
 *   allShipsPlaced: boolean,
 *   placedCells: number
 * }}
 */
export function boardStateFromPlacements(placements) {
  const boardState = emptyBoardState();
  const tileDataState = emptyBoardState();
  const occupiedCells = new Set();
  let placedCells = 0;
  let allShipsPlaced = true;

  Fleet.forEach((ship) => {
    const coordinates = placements[ship.id] ?? [];
    if (coordinates.length !== ship.size) {
      allShipsPlaced = false;
    }

    coordinates.forEach((coordinate) => {
      if (!isCoordinate(coordinate)) {
        allShipsPlaced = false;
        return;
      }

      const occupiedKey = tileKey(coordinate.row, coordinate.column);
      if (occupiedCells.has(occupiedKey)) {
        allShipsPlaced = false;
        return;
      }

      occupiedCells.add(occupiedKey);
      boardState[coordinate.row][coordinate.column] = ship.id;
      tileDataState[coordinate.row][coordinate.column] = MapTile.FILLED;
      placedCells += 1;
    });
  });

  occupiedCells.forEach((key) => {
    const [row, column] = key.split(':').map(Number);
    adjacentCoordinates(row, column).forEach((adjacent) => {
      if (tileDataState[adjacent.row][adjacent.column] === MapTile.EMPTY) {
        tileDataState[adjacent.row][adjacent.column] = MapTile.BLOCKED;
      }
    });
  });

  return { boardState, tileDataState, allShipsPlaced, placedCells };
}

/**
 * @param {Record<string, import('./game-view-model.js').Coordinate[]>} placements
 * @returns {boolean}
 */
export function allFleetShipsPlaced(placements) {
  const { allShipsPlaced, placedCells } = boardStateFromPlacements(placements);
  return allShipsPlaced && placedCells === FLEET_SIZE;
}

/**
 * @returns {Record<string, import('./game-view-model.js').Coordinate[]>}
 */
export function randomFleetPlacements() {
  const nextBoardState = emptyBoardState();
  /** @type {Record<string, import('./game-view-model.js').Coordinate[]>} */
  const nextPlacements = {};

  Fleet.forEach((ship) => {
    const coordinates = findRandomShipPlacement(ship.size, nextBoardState);
    if (!coordinates) {
      throw new Error(`Unable to place ship ${ship.id}`);
    }

    coordinates.forEach((coordinate) => {
      nextBoardState[coordinate.row][coordinate.column] = ship.id;
    });
    nextPlacements[ship.id] = coordinates;
  });

  return nextPlacements;
}

/**
 * @param {Record<string, import('./game-view-model.js').Coordinate[]>} placements
 * @param {string} shipId
 * @param {import('./game-view-model.js').Coordinate[]} coordinates
 * @returns {boolean}
 */
export function canPlaceSetupShip(placements, shipId, coordinates) {
  const ship = Fleet.find((candidate) => candidate.id === shipId);
  if (!ship || coordinates.length !== ship.size) {
    return false;
  }

  const board = boardStateFromPlacements({
    ...placements,
    [shipId]: [],
  }).boardState;

  return canPlaceOnBoard(board, coordinates);
}

/**
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./game-view-model.js').Coordinate[]}
 */
export function shipCoordinatesFromStart(row, column, orientation, size) {
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
 * @returns {coordinate is import('./game-view-model.js').Coordinate}
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
 * @param {import('./game-view-model.js').Coordinate[]} coordinates
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

    if (boardState[coordinate.row][coordinate.column] !== MapTile.EMPTY) {
      return false;
    }

    return adjacentCoordinates(coordinate.row, coordinate.column).every(
      (adjacent) => boardState[adjacent.row][adjacent.column] === MapTile.EMPTY
    );
  });
}

/**
 * @param {number} size
 * @returns {import('./game-view-model.js').Coordinate[]}
 */
function randomShipCoordinates(size) {
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
 * @returns {import('./game-view-model.js').Coordinate[] | undefined}
 */
function findRandomShipPlacement(size, boardState) {
  return Array.from({ length: 1000 }, () => randomShipCoordinates(size)).find(
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
