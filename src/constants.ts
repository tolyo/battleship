export enum GameState {
  Preparing,
  Playing,
  Ended,
};

export const TOPIC = {
  HIT: 'HIT',
  TOGGLE: 'TOGGLE',
  STRIKE_REQUEST: 'STRIKE_REQUEST',
  STRIKE_RESPONSE: 'STRIKE_RESPONSE',
};

export const BOARD_EVENTS = {
  REMOVE_SHIP: 'REMOVE_SHIP',
};

export const GAME_STATE = {
  LOADING: 1,
  STARTED: 2,
  ENDED: 3,
};

export const GRID = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
export const GRID_SIZE = 30;
export const FLEET_BOARD_ID = 'fleetboard';
