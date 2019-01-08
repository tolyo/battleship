/**
 *
 * @param {object} config - game configuration
 */
import strikemap from './strikemap'
import fleetboard from './fleetboard'
import fleetmap from './fleetmap'
import gameEngine, { GameState } from './game-engine'

const init = (config) => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error("Illegal state")
  }
  const noop = () => {}

  const id = config.id || "fleetBoard"
  const strikeCallback = config.strikeCallback || noop
  const victoryCallback = config.victoryCallback || noop

  // init board
  fleetboard.createBoard(id)

  gameEngine.init()

  // configure strikemap
  strikemap(strikeCallback, victoryCallback)

}

const placeShipsAtRandom = () => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error("Illegal state")
  }
  fleetmap.placeShipsAtRandom()
  fleetboard.placeFleet()
}

const reset = () => {
  if (gameEngine.getState() === GameState.PLAYING) {
    throw new Error("Illegal state")
  }
  fleetmap.reset()
  fleetboard.reset()
}

export {
  init,
  placeShipsAtRandom,
  reset
}