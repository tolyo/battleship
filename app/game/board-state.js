import { GRID, MapTile } from './constants.js';

/**
 * @returns {string[][]}
 */
export function emptyBoardState() {
  return GRID.map(() => GRID.map(() => MapTile.EMPTY));
}
