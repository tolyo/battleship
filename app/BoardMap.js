
import { MapTile } from './state'
import { ShipOrientation } from './Ship'

export const GRID = [0,1,2,3,4,5,6,7,8,9]

export default class BoardMap {

  constructor() {
    this.map = []
    GRID.forEach(col => {
      this.map.push([])
      GRID.forEach(row => this.map[col].push(MapTile.EMPTY))
    })
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

  add(ship) {
    this.updateShipTiles(ship, MapTile.FILLED)
  }

  remove(ship) {
    this.updateShipTiles(ship, MapTile.EMPTY)
  }

  clearBoard() {
    GRID.forEach(col => {
      GRID.forEach(row => this.map[col][row] = MapTile.EMPTY)
    })
  }

  updateShipTiles(ship, tileState) {
    const { column, row, size, orientation } = ship
    console.log(`${column} ${row} ${size} ${orientation}`)
    for (let i = 0; i < size; i++) {
      if (orientation == ShipOrientation.HORIZONTAL && this.map[column] && this.map[column][row + i]) {
        this.map[column][row + i] = tileState
      } else if (this.map[column + i] && this.map[column + i][row]) {
        this.map[column + i][row] = tileState
      } else {
        throw new Error(`Unable to set tile for ${column} ${row} ${size} ${orientation}`)
      }
    }
  }

  isLegal(ship) {
    console.log(JSON.parse(JSON.stringify(ship)))
    const { column, row, size, orientation } = ship
    // check if grid exceeded
    console.log(`${column} ${row} ${size} ${orientation}`)
    // size decrease by one to account for head of ship being row or column
    if (orientation == ShipOrientation.HORIZONTAL) {
      if (row + size - 1 >= 10) return false
    }

    if (orientation == ShipOrientation.VERTICAL) {
      if (column + size - 1 >= 10) return false
    }

    return true
  }

}