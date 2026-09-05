export const GRID = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
export const GRID_SIZE = 30;
export const OWN_BOARD_ID = 'own-board';

/**
 * @enum {string}
 */
export const CellState = {
  EMPTY: '_',
  FILLED: 'X',
  BLOCKED: 'o',
  HIT: '+',
  MISS: 'm',
};

export const UNIT_CELLS = 20;

/**
 * @enum {string}
 */
export const SessionStatus = Object.freeze({
  PLACE_FLEET: 'Place your fleet',
  PLACE_FLEET_FIRST: 'Place your fleet first',
  CONNECTING: 'Connecting...',
  WAITING_FOR_OPPONENT: 'Waiting for opponent...',
  DISCONNECTED: 'Disconnected',
  CONNECTION_ERROR: 'connection_error',
  OPPONENT_DISCONNECTED: 'Opponent disconnected',
  IN_ROOM: 'In room',
  RECONNECTING: 'Reconnecting...',
  ROOM_UNAVAILABLE: 'Room unavailable',
  FLEET_READY: 'Fleet ready',
});
