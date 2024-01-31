import { GRID } from './constants';
import { Fleet } from './model/fleet';
import { MapTile } from './strikemap';

const map = [];

GRID.forEach((col) => {
  map.push([]);
  GRID.forEach(() => map[col].push(MapTile.EMPTY));
});

/**
 * Return a copy of a map of grid elements
 * @return {*[]}
 */
export function getMap() {
  return map.splice(0);
}

export function placeShipsAtRandom() {
  try {
    tryPlacingShips();
  } catch (e) {
    placeShipsAtRandom();
  }
}

export function tryPlacingShips() {
  reset();
  Fleet.forEach((ship) => {
    ship.setLocation(getRandomShipCoordinate());
    let count = 10000; // safety to prevent runaway cycle
    while (isLegal(ship) === false) {
      ship.setLocation(getRandomShipCoordinate());
      count--;
      if (count === 0) {
        console.log(showGrid());
        throw new Error('Count exceeded');
      }
    }
    // attach ship only when valid location is found
    add(ship);
  });

  console.log(showGrid());
}

/**
 * Clear adjacent markings
 */
export function prepareForGame() {
  clearBlocked();
}

export function showGrid() {
  let grid = ``;
  map.forEach((column) => {
    column.forEach((row) => {
      grid += `${row} `;
    });
    grid += `\n`;
  });
  return grid;
}

export function add(ship) {
  placeShip(ship);
  markAdjacent(ship);
}

function strike(column, row) {
  // sanity checks
  if (map[column][row] === MapTile.HIT) {
    throw new Error('Field in illegal state BLOCKED');
  } else if (map[column][row] === MapTile.BLOCKED) {
    throw new Error('Field in illegal state BLOCKED');
  } else if (map[column][row] === MapTile.MISS) {
    throw new Error('Field in illegal state MISS');
  }

  //
  if (map[column][row] === MapTile.FILLED) {
    map[column][row] = MapTile.HIT;
    return true;
  }
  map[column][row] = MapTile.MISS;
  return false;
}

function markAdjacent(ship) {
  ship.getShipMapCoordinates().forEach(({ row, column }) => {
    getAdjacentCoordinates(row, column).forEach(({ row, column }) => {
      if (map[row][column] !== MapTile.FILLED) {
        map[row][column] = MapTile.BLOCKED;
      }
    });
  });
}

function placeShip(ship) {
  updateShipTiles(ship, MapTile.FILLED);
}

function removeShip(ship) {
  updateShipTiles(ship, MapTile.EMPTY);
}

function updateShipTiles(ship, tileState) {
  const { row, column, size, orientation } = ship;
  if (column === undefined || row === undefined || orientation === undefined)
    throw new Error('Cannot add ship. Column row and/or orientation not set');

  for (let i = 0; i < size; i++) {
    if (orientation === 'HORIZONTAL') {
      map[row][column + i] = tileState;
    } else if (orientation === 'VERTICAL') {
      map[row + i][column] = tileState;
    } else {
      throw new Error(
        `Unable to set tile for ${row} ${column}  ${size} ${orientation}`
      );
    }
  }
}

function reset() {
  GRID.forEach((col) => {
    GRID.forEach((row) => {
      map[col][row] = MapTile.EMPTY;
    });
  });
}

function clearBlocked() {
  GRID.forEach((col) => {
    GRID.forEach((row) => {
      if (map[col][row] === MapTile.BLOCKED) {
        map[col][row] = MapTile.EMPTY;
      }
    });
  });
}

function isLegal({ row, column, size, orientation }) {
  // check if grid exceeded
  // console.log(`isLegal ${column} ${row} ${size} ${orientation}`)

  let isLegal = true;

  // size decrease by one to account for head of ship being row or column
  if (orientation === 'HORIZONTAL') {
    if (row + size - 1 >= 10) return false;
    for (let i = 0; i < size; i += 1) {
      if (map[row][column + i] !== MapTile.EMPTY) return false;

      // prevent placement to adjacent ships
      getAdjacentCoordinates(row, column + i).forEach(({ row, column }) => {
        if (map[row][column] === MapTile.FILLED) {
          isLegal = false;
        }
      });
    }
  }

  if (orientation === 'VERTICAL') {
    if (column + size - 1 >= 10) return false;
    for (let i = 0; i < size; i += 1) {
      if (map[row + i][column] !== MapTile.EMPTY) return false;

      // prevent placement to adjacent ships
      getAdjacentCoordinates(row + i, column).forEach(({ row, column }) => {
        if (map[row][column] === MapTile.FILLED) {
          isLegal = false;
        }
      });
    }
  }

  return isLegal;
}

function getAdjacentCoordinates(row, column) {
  const coordinates = [];
  //
  // // top left
  if (row !== 0 && column !== 0) {
    coordinates.push({ row: row - 1, column: column - 1 });
  }

  // top mid
  if (row !== 0) {
    coordinates.push({ row: row - 1, column });
  }

  // top right
  if (row !== 0 && column !== 9) {
    coordinates.push({ row: row - 1, column: column + 1 });
  }

  // mid left
  if (column !== 0) {
    coordinates.push({ row, column: column - 1 });
  }

  // mid right
  if (column !== 9) {
    coordinates.push({ row, column: column + 1 });
  }

  // bot left
  if (row !== 9 && column !== 0) {
    coordinates.push({ row: row + 1, column: column - 1 });
  }

  // bot mid
  if (row !== 9) {
    coordinates.push({ row: row + 1, column });
  }

  // bot right
  if (row !== 9 && column !== 9) {
    coordinates.push({ row: row + 1, column: column + 1 });
  }

  return coordinates;
}

function getRandomTile() {
  return Math.floor(Math.random() * 9);
}

function getRandomOrientation() {
  return ['VERTICAL', 'HORIZONTAL'][Math.round(Math.random())];
}

function getRandomShipCoordinate() {
  return {
    row: getRandomTile(),
    column: getRandomTile(),
    orientation: getRandomOrientation(),
  };
}
