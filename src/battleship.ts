import strikemap from './strikemap'
import fleetboard from './fleetboard'
import fleetmap from './fleetmap'
import fleetdom from './fleetdom'
import gameEngine from './game'
import { FLEET_BOARD_ID, GameState } from './constants'

/**
 *
 * @param {object} config - game configuration
 */
const init = (config) => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error('Illegal state')
  }
  const noop = () => {}

  const strikeCallback = config.strikeCallback || noop
  const victoryCallback = config.victoryCallback || noop

  // init board
  fleetboard.createBoard(FLEET_BOARD_ID)

  gameEngine.init()
  // configure strikemap
  strikemap(strikeCallback, victoryCallback)
}

const placeShipsAtRandom = () => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error('Illegal state')
  }
  fleetmap.placeShipsAtRandom()
  fleetboard.placeFleet()
  fleetdom.init()
}

const reset = () => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error('Illegal state')
  }
  fleetmap.reset() // clear the grid
  fleetboard.reset() // clear the board
  fleetdom.reset() // remove ship elements
}

export {
  init,
  placeShipsAtRandom,
  reset
}
