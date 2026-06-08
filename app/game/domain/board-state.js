import { GRID, CellState } from './constants.js';

/**
 * @returns {string[][]}
 */
export function emptyBoardState() {
  return GRID.map(() => GRID.map(() => CellState.EMPTY));
}
