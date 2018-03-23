
import { Destroyer } from './ships'
import { ShipOrientation } from './Ship'

export default class BattleShipBoard {

  constructor(id) {
    if (!id) throw new Error('Board id required');
    this.fleetboard = window.document.getElementById(id);
    if (!this.fleetboard) throw new Error('Board id not found');
    this.fleetboard.setAttribute('class', 'battleshipboard')
    addTiles(this.fleetboard)

    this.addShips()
  }

  addShips() {
    new Destroyer('dest1', ShipOrientation.HORIZONTAL)
  }
}

const addTiles = (board) => {
  const rows = [1,2,3,4,5,6,7,8,9,10]
  const columns = [1,2,3,4,5,6,7,8,9,10]

  rows.forEach((i) => {
    // create row
    const tileRow = document.createElement('div')
    tileRow.className = 'tileRow'
    board.appendChild(tileRow)

    // create tiles
    columns.forEach((j) => {
      const tile = document.createElement('div')
      tile.className = 'tile'
      tile.id = i + '-' + j
      tile.setAttribute('data-column', i)
      tile.setAttribute('data-row', j)

      tile.addEventListener('dragEnter', () => tile.className = 'tile droppable-target')
      tile.addEventListener('dragLeave', () => tile.className = 'tile')

      tileRow.appendChild(tile)
    })

  })
}
