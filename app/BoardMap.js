
import { MapTile, State } from './state'
import { ShipOrientation } from './ship'
import { GRID } from './constants'
import { getRandomShipCoordinate } from './utils'
import Fleet from './fleet'

class BoardMap {
  constructor() {
    this.map = []
    GRID.forEach(col => {
      this.map.push([])
      GRID.forEach(() => this.map[col].push(MapTile.EMPTY))
    })
  }

  placeShipsAtRandom() {
    try {
      this.tryPlacingShips()
    } catch (e) {
      this.placeShipsAtRandom()
    }
  }

  tryPlacingShips() {
    this.reset()
    Fleet.forEach(ship => {
      console.log(this.showGrid())
      ship.setLocation(getRandomShipCoordinate())
      let count = 10000 // safety to prevent runaway cycle
      while (this.isLegal(ship) === false) {
        ship.setLocation(getRandomShipCoordinate())
        count--
        if (count == 0) {
          console.log(this.showGrid())
          throw new Error("Count exceeded")
        }
      }
      // attach ship only when valid location is found
      this.add(ship)
    })

    console.log(this.showGrid())
  }

  /**
   * Clear adjacent markings
   */
  prepareForGame () {
    this.clearBlocked()
  }

  showGrid() {
    let grid = ``
    this.map.forEach(column => {
      column.forEach(row => {
        grid = grid + `${row} `
      })
      grid = grid + `\n`
    })
    return grid
  }

  add (ship) {
    this.placeShip(ship)
    this.markAdjacent(ship)
  }

  strike (column, row) {
    // sanity checks
    if (this.map[column][row] === MapTile.HIT) {
      throw new Error("Field in illegal state BLOCKED")
    } else if (this.map[column][row] === MapTile.BLOCKED) {
      throw new Error("Field in illegal state BLOCKED")
    } else if (this.map[column][row] === MapTile.MISS) {
      throw new Error("Field in illegal state MISS")
    }

    //
    if (this.map[column][row] === MapTile.FILLED) {
      console.log('HIT')
      this.map[column][row] = MapTile.HIT
      return true
    } else {
      console.log('MISS')
      this.map[column][row] = MapTile.MISS
      return false
    }
  }

  markAdjacent (ship) {
    ship.getShipMapCoordinates().forEach(val => {
      this.getAdjacentCoordinates(val.y, val.x).forEach(j => {
        if (this.map[j.y][j.x] !== MapTile.FILLED) {
          this.map[j.y][j.x] = MapTile.BLOCKED
        }
      })
    })
  }


  placeShip (ship) {
    this.updateShipTiles(ship, MapTile.FILLED)
  }

  removeShip (ship) {
    this.updateShipTiles(ship, MapTile.EMPTY)
  }

  updateShipTiles(ship, tileState) {
    const { column, row, size, orientation } = ship
    if (column === undefined || row === undefined || orientation === undefined) throw new Error("Cannot add ship. Column row and/or orientation not set")
    console.log(`${column} ${row} ${size} ${orientation}`)

    for (let i = 0; i < size; i++) {
      if (orientation === ShipOrientation.HORIZONTAL && this.map[column] && this.map[column][row + i]) {
        this.map[column][row + i] = tileState
      } else if (this.map[column + i] && this.map[column + i][row]) {
        this.map[column + i][row] = tileState
      } else {
        throw new Error(`Unable to set tile for ${column} ${row} ${size} ${orientation}`)
      }
    }
  }

  reset() {
    GRID.forEach(col => {
      GRID.forEach(row => {
        this.map[col][row] = MapTile.EMPTY
      })
    })
  }

  clearBlocked() {
    GRID.forEach(col => {
      GRID.forEach(row => {
        if (this.map[col][row] === MapTile.BLOCKED) this.map[col][row] = MapTile.EMPTY
      })
    })
  }

  isLegal ({ column, row, size, orientation }) {
    // check if grid exceeded
    // console.log(`isLegal ${column} ${row} ${size} ${orientation}`)

    let isLegal = true

    if (isNaN(column) || isNaN(row)) return false
    // size decrease by one to account for head of ship being row or column
    if (orientation === ShipOrientation.HORIZONTAL) {
      if (row + size - 1 >= 10) return false
      for (let i = 0; i < size; i++) {
        if (this.map[column][row + i] !== MapTile.EMPTY) return false

        // prevent placement to adjacent ships
        this.getAdjacentCoordinates(column, row + i).forEach(j => {
          if (this.map[j.y][j.x] === MapTile.FILLED) {
            isLegal = false
          }
        })
      }
    }

    if (orientation === ShipOrientation.VERTICAL) {
      if (column + size - 1 >= 10) return false
      for (let i = 0; i < size; i++) {
        if (this.map[column + i][row] !== MapTile.EMPTY) return false

        // prevent placement to adjacent ships
        this.getAdjacentCoordinates(column + i, row).forEach(j => {
          if (this.map[j.y][j.x] === MapTile.FILLED) {
            isLegal = false
          }
        })
      }
    }

    return isLegal
  }

  getAdjacentCoordinates (y, x) {
    const coordinates = []
    //
    // // top left
    if (y !== 0 && x !== 0) {
      coordinates.push({ y: y - 1, x: x - 1 })
    }

    // top mid
    if (y !== 0) {
      coordinates.push({ y: y - 1, x })
    }

    // top right
    if (y !== 0 && x !== 9) {
      coordinates.push({ y: y - 1, x: x + 1 })
    }

    // mid left
    if (x !== 0) {
      coordinates.push({ y: y, x: x - 1 })
    }

    // mid right
    if (x !== 9) {
      coordinates.push({ y: y, x: x + 1 })
    }

    // bot left
    if (y !== 9 && x !== 0) {
      coordinates.push({ y: y + 1, x: x - 1 })
    }

    // bot mid
    if (y !== 9) {
      coordinates.push({ y: y + 1, x: x })
    }

    // bot right
    if (y !== 9 && x !== 9) {
      coordinates.push({ y: y + 1, x: x + 1 })
    }

    return coordinates
  }
}

const boardmap = new BoardMap()

export default boardmap
