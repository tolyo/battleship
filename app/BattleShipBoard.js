import { GridSquare, State } from './state'
import { getRandomOrientation, getRandomShipCoordinate } from './utils'
import boardmap from './BoardMap'
import { Fleet } from './fleet'
import pubsubservice from './pubsubservice'
import { GRID, TOPIC } from './constants'
import Player from './Player'

export default class BattleShipBoard {
  constructor (id) {
    if (!id) throw new Error('Board id required')
    const gameboard = window.document.getElementById(id)
    const fleetBoard = document.createElement('div')
    gameboard.appendChild(fleetBoard)
    this.fleetboard = fleetBoard
    this.fleetboard.setAttribute('id', 'battleshipboard')

    const hitBoard = document.createElement('div')
    gameboard.appendChild(hitBoard)
    this.hitBoard = hitBoard
    this.hitBoard.setAttribute('id', 'hitboard')

    this.addTiles()

    this.addShips()

    this.addPlayers()

    this.activeBoard = 0

    // server call should go here
    this.strikeRequestCallback = (column, row) => boardmap.strike(column, row)
    // pubsubservice.subscribe(TOPIC.STRIKE_REQUEST, (column, row) => this.strikeRequestCallback(column, row))

    // websocket call should go here
    this.strikeReceiptCallback = (column, row) => {
      const shipHit = boardmap.strike(column, row)
      if (shipHit === true) {
        this.hitBoardPlayer.decreaseHealth()
        if (this.hitBoardPlayer.isDead()) this.endGame(this.hitBoardPlayer)
      }
    }
  }

  endGame (player) {
    console.log('game end ' + player.id)
  }

  addShips () {
    this.placeShipsAtRandom()
    this.attachShipsToBoard()
  }

  addTiles () {
    this.addTilesToBoard(this.fleetboard, 'fleetboard')
    this.addTilesToBoard(this.hitBoard, 'hitboard')

    console.log(State)
  }

  initHitboardTile (tile) {
    tile.className = 'hitboard-tile'
    tile.onclick = () => {
      console.log('attempt strike at' + tile.dataset.column)
      const shipHit = this.strikeRequestCallback(tile.dataset.column, tile.dataset.row)
      if (shipHit === true) {
        tile.className += ' hit'
        this.fleetPlayer.decreaseHealth()
        if (this.fleetPlayer.isDead()) this.endGame(this.fleetPlayer)
      } else {
        tile.className += ' miss'
      }
      tile.onclick = null
    }
  }

  addTilesToBoard (elem, boardname) {
    GRID.forEach((y) => {
      // create row
      const tileRow = document.createElement('div')
      tileRow.className = 'board-row'
      elem.appendChild(tileRow)

      State.grid.push([])

      // create tiles
      GRID.forEach((x) => {
        const tile = document.createElement('div')
        tile.className = boardname + '-tile'
        tile.id = boardname + '-' + y + '-' + x
        tile.setAttribute('data-column', y)
        tile.setAttribute('data-row', x)
        //
        // tile.addEventListener('dragEnter', () => tile.className = 'fleetboard-tile droppable-target')
        tile.addEventListener('dragLeave', () => {
          tile.className = boardname + '-tile'
        })

        if (boardname === 'hitboard') {
          this.initHitboardTile(tile)
        }

        tileRow.appendChild(tile)
        State.grid[y].push(new GridSquare(x, y, tile))
      })
    })
  }

  placeShipsAtRandom () {
    Fleet.forEach(ship => {
      ship.setLocation(getRandomShipCoordinate())
      while (boardmap.isLegal(ship) === false) {
        ship.setLocation(getRandomShipCoordinate())
      }
      // attach ship only when valid location is found
      boardmap.add(ship)
      pubsubservice.subscribe('markAdjacent', () => boardmap.markAdjacent(ship))
    })
  }

  attachShipsToBoard () {
    Fleet.forEach(e => e.attachToBoard())
  }

  randomShips () {
    this.clearShips()
    this.addShips()
  }

  clearHitBoard () {
    GRID.forEach(y => {
      GRID.forEach(x => {
        const tile = document.getElementById(`hitboard-${y}-${x}`)
        this.initHitboardTile(tile)
      })
    })
  }

  clearShips () {
    // clear hitboard
    this.clearHitBoard()
    boardmap.clearBoard()
    Fleet.forEach(e => e.removeFromBoard())
    console.log(boardmap.showGrid())
  }

  toggleLock () {
    Fleet.forEach(e => e.lockShip())
  }

  toggleBoard () {
    if (this.activeBoard === 0) {
      this.activeBoard = 1
      document.getElementById('battleshipboard').classList.add('disabled')
      Array.from(document.getElementsByClassName('ship')).forEach(e => e.classList.add('disabled'))
      document.getElementById('hitboard').classList.remove('disabled')
      Fleet.forEach(e => e.lockShip())
      pubsubservice.publish(TOPIC.TOGGLE, [1])
    } else {
      this.activeBoard = 0
      document.getElementById('battleshipboard').classList.remove('disabled')
      document.getElementById('hitboard').classList.add('disabled')
      Array.from(document.getElementsByClassName('ship')).forEach(e => e.classList.remove('disabled'))
      this.toggleLock()
      pubsubservice.publish(TOPIC.TOGGLE, [0])
    }
  }

  addPlayers () {
    this.fleetPlayer = new Player(1)
    this.hitBoardPlayer = new Player(2)
  }
}
