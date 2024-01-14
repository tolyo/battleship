import { GRID } from './constants';
import { FLEET_SIZE } from './fleet';

/**
 * @type {MapTile[][]}
 */
const map = [];
let strikeCount = 0;

/**
 * @enum {string}
 */
export const MapTile = {
  EMPTY: '_',
  FILLED: 'X',
  BLOCKED: 'o',
  HIT: '+',
  MISS: 'm',
};

/**
 *
 * @param {number} row
 * @param {number} column
 */
export function attemptStrike(row, column) {
  if (map[row][column] === MapTile.MISS) {
    throw new Error('Illegal state. Already struck with MISS');
  }

  if (map[row][column] === MapTile.HIT) {
    throw new Error('Illegal state. Already struck with HIT');
  }

  const result = true; // SERVER CALL FOR STATE; WE PROBABLY WILL NOT NEED A SEPARATE VICROTY CALLBACK

  if (result === true) {
    map[row][column] = MapTile.HIT;
    strikeCount += 1;
    if (strikeCount === FLEET_SIZE) {
      // victoryCallback();
    }
  } else if (result === false) {
    map[row][column] = MapTile.MISS;
  } else {
    throw new Error('Callback must return boolean');
  }
}

export function initMap() {
  // initialize map
  GRID.forEach((col) => {
    map.push([]);
    GRID.forEach(() => map[col].push(MapTile.EMPTY));
  });
}

export function showMap() {
  let grid = ``;
  map.forEach((column) => {
    column.forEach((row) => {
      grid += `${row} `;
    });
    grid += `\n`;
  });
  return grid;
}
