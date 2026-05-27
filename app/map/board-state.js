import { GRID, MapTile } from '../game/constants.js';

/**
 * @returns {string[][]}
 */
export function emptyBoardState() {
  return GRID.map(() => GRID.map(() => MapTile.EMPTY));
}

/**
 * @param {HTMLElement} elem
 * @returns {{ row: number, column: number } | undefined}
 */
export function tileCoordinates(elem) {
  const row = Number(elem.dataset.row);
  const column = Number(elem.dataset.column);

  if (!Number.isInteger(row) || !Number.isInteger(column)) {
    return undefined;
  }

  return { row, column };
}

/**
 * @param {HTMLElement} elem
 * @returns {boolean}
 */
export function isClaimedShipTile(elem) {
  return (
    elem.dataset.state === MapTile.FILLED && tileCoordinates(elem) !== undefined
  );
}
