/**
 *
 * @param {object} config - game configuration
 */
import strikemap from './strikemap'
import fleetboard from './fleetboard'
import fleetmap from './fleetmap'
import Fleet from './fleet'

const init = (config) => {

  const noop = () => {}

  const id = config.id || "fleetBoard"
  const strikeCallback = config.strikeCallback || noop
  const victoryCallback = config.victoryCallback || noop

  // init board
  fleetboard.createBoard(id)

  // configure strikemap
  strikemap(strikeCallback, victoryCallback)

}

const placeShipsAtRandom = () => {
  fleetmap.placeShipsAtRandom()
  fleetboard.placeFleet()
}

const reset = () => {
  fleetmap.reset()
  fleetboard.reset()
}

export {
  init,
  placeShipsAtRandom,
  reset
}