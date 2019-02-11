import Fleet from './fleet'
import { BOARD_EVENTS, FLEET_BOARD_ID, GameState, GRID_SIZE } from './constants'
import { ShipOrientation } from './ship'
import gameEngine from './game-engine'
import pubsub from './pubsubservice'

export default (() => {
  const init = () => {
    reset()
    const board = document.getElementById(FLEET_BOARD_ID)
    Fleet.forEach(ship => {
      const element = createDomElement(ship)
      board.parentElement.appendChild(element)
      attachDomElement(ship)
    })
  }

  const reset = () => {
    const htmlList = window.document.getElementsByClassName('ship')
    Array
      .from(htmlList)
      .forEach(elem => elem.parentNode.removeChild(elem))
  }

  /**
   * @param {Ship} ship
   * @return {HTMLElement} shipElement
   */
  const createDomElement = (ship) => {
    const shipElement = document.createElement('div')
    shipElement.classList.add('ship')
    shipElement.id = ship.id // each div is identified by ship id
    const { width, height } = calculateSize(ship)
    shipElement.style.width = width
    shipElement.style.height = height
    shipElement.position = 'absolute'
    return shipElement
  }

  const attachDomElement = (ship) => {
    const { id, column, row } = ship
    const tile = document.getElementById(`fleetboard-${row}-${column}`)
    const shipDom = document.getElementById(id)
    shipDom.style.left = tile.getBoundingClientRect().left + window.pageXOffset + 'px'
    shipDom.style.top = tile.getBoundingClientRect().top + window.pageYOffset + 'px'
    //     // set event handlers
    shipDom.onmousedown = (e) => onmousedown(e, shipDom)
    // override default browser behavior
    shipDom.ondragstart = () => false
    shipDom.onmouseup = () => false
  }

  /**
   * Drag control variables
   * @type {number}
   */
  let shiftX
  let shiftY
  let dragged = false

  /**
   * @param {MouseEvent} e
   * @param {HTMLElement} shipDom
   */
  const onmousedown = (e, shipDom) => {
    if (dragged === true) return // one item at a time
    if (e.button !== undefined && e.button !== 0) return // only touch or left click
    if (e.touches && e.touches.length > 1) return // support one finger touch only
    if ([GameState.PLAYING, GameState.ENDED].includes(gameEngine.getState())) return

    dragged = true
    e.preventDefault()
    document.onmousemove = e => onmousemove(e, shipDom)
    document.onmouseup = e => onmouseup(e, shipDom)
    const shipCoordinates = getShipCoordinates(shipDom)
    // set offsets for the click event
    console.log(shipCoordinates)
    shiftX = e.pageX - shipCoordinates.left
    shiftY = e.pageY - shipCoordinates.top
    shipDom.classList.add('dragged')
    // remove from grid
    pubsub.publish(BOARD_EVENTS.REMOVE_SHIP, [shipDom])
  }

  const fleetBoardElem = document.getElementById(FLEET_BOARD_ID)

  const onmousemove = (e, shipDom) => {
    shipDom.style.left = Math.floor((e.pageX - shiftX) / GRID_SIZE) * GRID_SIZE + fleetBoardElem.offsetLeft + 'px'
    shipDom.style.top = Math.floor((e.pageY - shiftY) / GRID_SIZE) * GRID_SIZE + fleetBoardElem.offsetTop + 'px'
  }

  /**
   *
   * @param {Event} e
   * @param {HTMLElement} shipDom
   */
  const onmouseup = (e, shipDom) => {
    dragged = false
    // clear event bindings
    document.onmousemove = null
    document.onmouseup = null
    shipDom.classList.remove('dragged')
  }

  const getShipCoordinates = srcElement => {
    const box = srcElement.getBoundingClientRect()
    return {
      left: box.left + window.pageXOffset,
      top: box.top + window.pageYOffset
    }
  }

  /**
   * @typedef {Object} shipElementSize
   * @property {string} width - px
   * @property {string} height - px
   */

  /**
   * @param {Ship} ship
   * @return {shipElementSize}
   */
  const calculateSize = (ship) => {
    console.log(ship.size)
    return {
      width: (ship.orientation === ShipOrientation.HORIZONTAL) ? GRID_SIZE * ship.size + 'px' : GRID_SIZE + 'px',
      height: (ship.orientation === ShipOrientation.VERTICAL) ? GRID_SIZE * ship.size + 'px' : GRID_SIZE + 'px'
    }
  }

  return {
    init,
    reset
  }
})()
