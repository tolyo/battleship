import { addTilesToBoard } from './boardutils'
import Fleet from './fleet'

export default (() => {
  const createBoard = (id) => {
    if (id === undefined) throw new Error("board id not provided")
    const elem = window.document.getElementById(id)
    if (elem === null) throw new Error("board element not found")
    addTilesToBoard(elem, "fleetboard")
  }

  const clearPlacedGrids = () => {
    const htmlList = window.document.getElementsByClassName('placed')
    const elemList = Array.from(htmlList)
    elemList.forEach(elem => elem.classList.remove('placed'))
  }

  const placeFleet = () => {
    clearPlacedGrids()
    Fleet.forEach(ship => placeShip(ship))
  }

  /**
   * @param {Ship} ship to place
   */
  const placeShip = (ship) => {
    ship.getShipMapCoordinates().forEach(({ row, column }) => {
      const elem = window.document.getElementById(`fleetboard-${row}-${column}`)
      elem.classList.add('placed')
    })
  }

  const reset = () => {
    clearPlacedGrids()
    Fleet.forEach(ship => ship.reset())
  }

  return {
    createBoard,
    placeFleet,
    reset
  }
})()
