import {
  grid,
  tile_size as tileSize,
  occupied_cells as occupiedCells,
} from '../../../_build/browser/js/modules/battleship/browser/Fleet.js';
import {
  cell_states_names as cellStateNames,
  statuses_names as statusNames,
  own_board_id as ownBoardId,
} from '../../../_build/browser/js/modules/battleship/browser/SessionConfig.js';

export const GRID = grid();
export const GRID_SIZE = tileSize();
export const OWN_BOARD_ID = ownBoardId();

/** @enum {string} */
export const CellState = Object.fromEntries(
  cellStateNames().map(({ name, value }) => [name, value])
);

export const UNIT_CELLS = occupiedCells();

/** @enum {string} */
export const SessionStatus = Object.freeze(
  Object.fromEntries(statusNames().map(({ name, value }) => [name, value]))
);
