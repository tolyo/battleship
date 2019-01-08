/**
 *
 * @param {object} config - game configuration
 */
import strikemap from './strikemap'
import fleetboard from './fleetboard'

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

export { init }