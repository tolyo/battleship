
import { Destroyer } from './ships'
import { ShipOrientation } from './Ship'
import { GridSquare, State } from './state'

export default class BattleShipBoard {

  constructor(id) {
    if (!id) throw new Error('Board id required');
    this.fleetboard = window.document.getElementById(id);
    if (!this.fleetboard) throw new Error('Board id not found');
    this.fleetboard.setAttribute('class', 'battleshipboard')

    this.addTiles()

    this.addShips()
  }

  addShips() {
    new Destroyer('dest1', ShipOrientation.HORIZONTAL)
  }

  addTiles() {
    const size = [1,2,3,4,5,6,7,8,9,10]

    size.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'tileRow'
      this.fleetboard.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      size.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = 'tile'
        tile.id = y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        // tile.addEventListener('dragEnter', () => tile.className = 'tile droppable-target')
        // tile.addEventListener('dragLeave', () => tile.className = 'tile')

        tileRow.appendChild(tile)
        State.grid[y - 1].push(new GridSquare(x, y, tile))
      })
    })

    console.log(State)
  }
}


