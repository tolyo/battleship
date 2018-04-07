import { GridSquare, State } from './state'
import { getRandomOrientation, getRandomTileCoordinate } from './utils'
import boardmap from './BoardMap'
import { GRID } from './BoardMap'
import { Fleet } from './Fleet'
import pubSubService from './pubsubservice'

export default class BattleShipBoard {

  constructor(id) {
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

    this.addTilesToBoard(this.fleetboard, 'fleetboard-tile')
    this.addTilesToBoard(this.hitBoard, 'hitboard-tile')

    console.log(State)

  }

  addTilesToBoard(elem, boardname) {
    GRID.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'tileRow'
      elem.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      GRID.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = boardname
        tile.id = y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        //tile.addEventListener('dragEnter', () => tile.className = 'fleetboard-tile droppable-target')
        tile.addEventListener('dragLeave', () => tile.className = boardname)

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

    const lockButton = document.createElement('button')
    lockButton.appendChild(document.createTextNode('Lock map'))
    lockButton.className = 'clear-button'
    lockButton.onclick = () => {
      this.lockShips()
    }
    this.fleetboard.appendChild(lockButton)
  }

  placeShipsAtRandom() {

    Fleet.forEach(ship => {
      const location = getRandomTileCoordinate()
      let column = location.column
      let row = location.row
      let orientation = getRandomOrientation()
      // generate a random location until a legal location is found
      while (boardmap.isLegal(column, row, ship.size, orientation) == false) {
        const location = getRandomTileCoordinate()
        column = location.column
        row = location.row
        orientation = getRandomOrientation()
      }
      // attach ship only when valid location is found
      ship.setLocation(column, row, orientation)
      boardmap.add(ship)
      pubSubService.subscribe('markAdjacent', () => boardmap.markAdjacent(ship))
    })
  }

  attachShipsToBoard() {
    Fleet.forEach(e => e.attachToBoard())
  }

  clearShips() {
    boardmap.clearBoard()
    Fleet.forEach(e => e.removeFromBoard())
    console.log(boardmap.showGrid())
  }

  lockShips() {
    Fleet.forEach(e => e.lockShip())
  }
}


