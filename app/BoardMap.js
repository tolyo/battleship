
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
    ship.clear()
  }

  updateShipTiles(ship, tileState) {
    const { column, row, size, orientation } = ship
    for (let i = 0; i < size; i++) {
      if (orientation == ShipOrientation.HORIZONTAL) {
        this.map[column][row + i] = tileState
      } else {
        this.map[column + i][row] = tileState
      }
    }
  }

  isLegal(ship) {
    console.log(ship)
    const { column, row, size, orientation } = ship
    // check if grid exceeded
    console.log(orientation)
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