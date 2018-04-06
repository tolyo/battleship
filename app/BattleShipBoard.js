import { GridSquare, State } from './state'
import { getRandomOrientation, getRandomTileCoordinate } from './utils'
import BoardMap, { GRID } from './BoardMap'
import { Fleet } from './Fleet'
import pubSubService from './pubsubservice'

export default class BattleShipBoard {

  constructor(id) {
    this.map = new BoardMap()
    if (!id) throw new Error('Board id required')
    const gameboard = window.document.getElementById(id)
    const fleetBoard = document.createElement('div')
    fleetBoard.setAttribute('class', 'battleshipboard')
    gameboard.appendChild(fleetBoard)
    this.fleetboard = fleetBoard
    this.fleetboard.setAttribute('class', 'battleshipboard')

    const hitBoard = document.createElement('div')
    gameboard.appendChild(hitBoard)
    this.hitBoard = hitBoard
    this.hitBoard.setAttribute('class', 'hitboard')

    this.addTiles()

    this.addControls()

    this.addShips()


  }

  addShips() {
    this.placeShipsAtRandom()
    this.attachShipsToBoard()
  }

  addTiles() {

    this.addTilesToBoard(this.fleetboard)
    this.addTilesToBoard(this.hitBoard)

    console.log(State)

  }

  addTilesToBoard(elem) {
    GRID.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'tileRow'
      elem.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      GRID.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = 'tile'
        tile.id = y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        //tile.addEventListener('dragEnter', () => tile.className = 'tile droppable-target')
        tile.addEventListener('dragLeave', () => tile.className = 'tile')

        tileRow.appendChild(tile)
        State.grid[y].push(new GridSquare(x, y, tile))
      })
    })
  }

  addControls() {
    const clearButton = document.createElement('button')
    clearButton.appendChild(document.createTextNode('Clear'))
    clearButton.className = 'clear-button'
    clearButton.onclick = () => this.clearShips()
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
      const location = getRandomTileCoordinate()
      let column = location.column
      let row = location.row
      let orientation = getRandomOrientation()
      // generate a random location until a legal location is found
      while (this.map.isLegal(column, row, ship.size, orientation) == false) {
        const location = getRandomTileCoordinate()
        column = location.column
        row = location.row
        orientation = getRandomOrientation()
      }
      // attach ship only when valid location is found
      ship.setLocation(column, row, orientation)
      this.map.add(ship)
      pubSubService.subscribe('markAdjacent', () => this.map.markAdjacent(ship))
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


