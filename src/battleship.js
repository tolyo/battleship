import fleetboard from './fleetboard';
import gameEngine from './game';
import { FLEET_BOARD_ID } from './constants';

/**
 *
 * @param {object} config - game configuration
 */
const init = (config) => {
  if (gameEngine.getState() === 'PLAYING') {
    throw new Error('Illegal state');
  }
  const noop = () => {};

  // init board
  fleetboard.createBoard(FLEET_BOARD_ID);

  gameEngine.init();
  // configure strikemap
  // strikemap(strikeCallback, victoryCallback);
};

export function placeShipsAtRandom() {
  if (gameEngine.getState() === 'PLAYING') {
    throw new Error('Illegal state');
  }
  fleetmap.placeShipsAtRandom();
  fleetboard.placeFleet();
  fleetdom.init();
}

export function reset() {
  if (gameEngine.getState() === 'PLAYING') {
    throw new Error('Illegal state');
  }
  fleetmap.reset(); // clear the grid
  fleetboard.reset(); // clear the board
  fleetdom.reset(); // remove ship elements
}
