
import { MapTile } from './state'
import { ShipOrientation } from './ship'
import { GRID } from './constants'
import { getRandomShipCoordinate } from './utils'
import Fleet from './fleet'

class BoardMap {
  constructor () {
    this.map = []
    GRID.forEach(col => {
      this.map.push([])
      GRID.forEach(() => this.map[col].push(MapTile.EMPTY))
    })
  }

  placeShipsAtRandom () {
    try {
      this.tryPlacingShips()
    } catch (e) {
      this.placeShipsAtRandom()
    }
  }

  tryPlacingShips () {
    this.reset()
    Fleet.forEach(ship => {
      console.log(this.showGrid())
      ship.setLocation(getRandomShipCoordinate())
      let count = 10000 // safety to prevent runaway cycle
      while (this.isLegal(ship) === false) {
        ship.setLocation(getRandomShipCoordinate())
        count--
        if (count === 0) {
          console.log(this.showGrid())
          throw new Error('Count exceeded')
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

  showGrid () {
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
      throw new Error('Field in illegal state BLOCKED')
    } else if (this.map[column][row] === MapTile.BLOCKED) {
      throw new Error('Field in illegal state BLOCKED')
    } else if (this.map[column][row] === MapTile.MISS) {
      throw new Error('Field in illegal state MISS')
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
    ship.getShipMapCoordinates().forEach(({ row, column }) => {
      this.getAdjacentCoordinates(row, column).forEach(({ row, column }) => {
        if (this.map[row][column] !== MapTile.FILLED) {
          this.map[row][column] = MapTile.BLOCKED
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

  updateShipTiles (ship, tileState) {
    const { row, column, size, orientation } = ship
    if (column === undefined || row === undefined || orientation === undefined) throw new Error('Cannot add ship. Column row and/or orientation not set')
    console.log(`${row} ${column} ${size} ${orientation}`)

    for (let i = 0; i < size; i++) {
      if (orientation === ShipOrientation.HORIZONTAL) {
        this.map[row][column + i] = tileState
      } else if (orientation === ShipOrientation.VERTICAL) {
        this.map[row + i][column] = tileState
      } else {
        throw new Error(`Unable to set tile for ${row} ${column}  ${size} ${orientation}`)
      }
    }
  }

  reset () {
    GRID.forEach(col => {
      GRID.forEach(row => {
        this.map[col][row] = MapTile.EMPTY
      })
    })
  }

  clearBlocked () {
    GRID.forEach(col => {
      GRID.forEach(row => {
        if (this.map[col][row] === MapTile.BLOCKED) this.map[col][row] = MapTile.EMPTY
      })
    })
  }

  isLegal ({row, column, size, orientation }) {
    // check if grid exceeded
    // console.log(`isLegal ${column} ${row} ${size} ${orientation}`)

    let isLegal = true

    // size decrease by one to account for head of ship being row or column
    if (orientation === ShipOrientation.HORIZONTAL) {
      if (row + size - 1 >= 10) return false
      for (let i = 0; i < size; i++) {
        if (this.map[row][column + i] !== MapTile.EMPTY) return false

        // prevent placement to adjacent ships
        this.getAdjacentCoordinates(row, column + i).forEach(({row, column}) => {
          if (this.map[row][column] === MapTile.FILLED) {
            isLegal = false
          }
        })
      }
    }

    if (orientation === ShipOrientation.VERTICAL) {
      if (column + size - 1 >= 10) return false
      for (let i = 0; i < size; i++) {
        if (this.map[row + i][column] !== MapTile.EMPTY) return false

        // prevent placement to adjacent ships
        this.getAdjacentCoordinates(row + i, column).forEach(({row, column}) => {
          if (this.map[row][column] === MapTile.FILLED) {
            isLegal = false
          }
        })
      }
    }

    return isLegal
  }

  getAdjacentCoordinates (row, column) {
    const coordinates = []
    //
    // // top left
    if (row !== 0 && column !== 0) {
      coordinates.push({ row: row - 1, column: column - 1 })
    }

    // top mid
    if (row !== 0) {
      coordinates.push({ row: row - 1, column })
    }

    // top right
    if (row !== 0 && column !== 9) {
      coordinates.push({ row: row - 1, column: column + 1 })
    }

    // mid left
    if (column !== 0) {
      coordinates.push({ row: row, column: column - 1 })
    }

    // mid right
    if (column !== 9) {
      coordinates.push({ row: row, column: column + 1 })
    }

    // bot left
    if (row !== 9 && column !== 0) {
      coordinates.push({ row: row + 1, column: column - 1 })
    }

    // bot mid
    if (row !== 9) {
      coordinates.push({ row: row + 1, column: column })
    }

    // bot right
    if (row !== 9 && column !== 9) {
      coordinates.push({ row: row + 1, column: column + 1 })
    }

    return coordinates
  }
}

const boardmap = new BoardMap()

export default boardmap
