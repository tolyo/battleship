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

  // init fleet

  // init strikemap
  strikemap(strikeCallback, victoryCallback)

}

const placeShipsAtRandom = () => {
  fleetmap.placeShipsAtRandom()
  fleetboard.placeFleet()
}

export {
  init,
  placeShipsAtRandom
}