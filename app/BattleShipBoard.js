
import { Carrier, Destroyer } from './ships'
import { ShipOrientation } from './Ship'
import { GridSquare, MapTile, State } from './state'
import { BOARD_SIZE } from './constants'
import { getRandomOrientation, getRandomTileCoordinate } from './utils'
import BoardMap from './BoardMap'

export default class BattleShipBoard {

  constructor(id) {
    this.map = new BoardMap()
    if (!id) throw new Error('Board id required');
    this.fleetboard = window.document.getElementById(id);
    if (!this.fleetboard) throw new Error('Board id not found');
    this.fleetboard.setAttribute('class', 'battleshipboard')

    this.addTiles()

    this.addShips()

    console.log(this.map.showGrid())
  }

  addShips() {
    this.placeShipsAtRandom()
  }

  addTiles() {

    BOARD_SIZE.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'tileRow'
      this.fleetboard.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      BOARD_SIZE.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = 'tile'
        tile.id = y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        tile.addEventListener('dragEnter', () => tile.className = 'tile droppable-target')
        tile.addEventListener('dragLeave', () => tile.className = 'tile')

        tileRow.appendChild(tile)
        State.grid[y - 1].push(new GridSquare(x, y, tile))
      })
    })

    console.log(State)

  }

  placeShipsAtRandom() {
    const { column, row } = getRandomTileCoordinate()
    console.log(location)
    const destoyer = new Carrier('dest1', column, row, getRandomOrientation())

    console.log(this.map.isLegal(destoyer))

    if (this.map.isLegal(destoyer)) {
      this.map.add(destoyer)
    } else {
      this.placeShipsAtRandom()
    }
  }
}


