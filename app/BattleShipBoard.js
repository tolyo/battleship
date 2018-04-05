import { Carrier } from './ships'
import { GridSquare, State } from './state'
import { getRandomOrientation, getRandomTileCoordinate } from './utils'
import BoardMap, { GRID } from './BoardMap'
import { Fleet } from './Fleet'

export default class BattleShipBoard {

  constructor(id) {
    this.map = new BoardMap()
    if (!id) throw new Error('Board id required');
    this.fleetboard = window.document.getElementById(id);
    if (!this.fleetboard) throw new Error('Board id not found');
    this.fleetboard.setAttribute('class', 'battleshipboard')

    this.addTiles()
    this.addControls()

    this.addShips()

    console.log(this.map.showGrid())
  }

  addShips() {
    this.placeShipsAtRandom()
    this.attachShipsToBoard()
  }

  addTiles() {

    GRID.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'tileRow'
      this.fleetboard.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      GRID.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = 'tile'
        tile.id = y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        tile.addEventListener('dragEnter', () => tile.className = 'tile droppable-target')
        tile.addEventListener('dragLeave', () => tile.className = 'tile')

        tileRow.appendChild(tile)
        State.grid[y].push(new GridSquare(x, y, tile))
      })
    })

    console.log(State)

  }

  addControls() {
    const clearButton = document.createElement('button')
    clearButton.appendChild(document.createTextNode('Clear'))
    clearButton.className = 'clear-button'
    clearButton.onclick = () => this.clearShips();
    this.fleetboard.appendChild(clearButton)

    const randomButton = document.createElement('button')
    randomButton.appendChild(document.createTextNode('Random map'))
    randomButton.className = 'clear-button'
    randomButton.onclick = () => {
      this.clearShips()
      this.addShips()
    }
    this.fleetboard.appendChild(randomButton)
  }

  placeShipsAtRandom() {

    Fleet.forEach(ship => {
      const { column, row } = getRandomTileCoordinate()
      ship.setLocation(column, row, getRandomOrientation())
      // generate a random location until a legal location is found
      while (this.map.isLegal(ship) == false) {
        const { column, row } = getRandomTileCoordinate()
        ship.setLocation(column, row, getRandomOrientation())
      }
      this.map.add(ship)
    })
  }

  attachShipsToBoard() {
    Fleet.forEach(e => e.attachToBoard())
  }

  clearShips() {
    this.map.clearBoard()
    Fleet.forEach(e => e.removeFromBoard())
    console.log(this.map.showGrid())
  }
}


