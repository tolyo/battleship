
import { BOARD_SIZE } from './constants'

export const State = {
  grid : [],    // dynamic map of dom elements
  mapGrid : []  // static map of ship placement
}

export const MapTile = Object.freeze({
  EMPTY   : '_',
  FILLED  : 'X'
})

// init mapgrid
BOARD_SIZE.forEach(col => {
  State.mapGrid.push([])
  BOARD_SIZE.forEach(row => State.mapGrid[col - 1].push(MapTile.EMPTY))
})

State.showGrid = () => {
  let grid = ``
  State.mapGrid.forEach(column => {
    column.forEach(row => {
      grid = grid + `${row} `
    })
    grid = grid + `\n`
  })
  return grid
}


export const GridState = Object.freeze({
  EMPTY   : 0,
  FILLED  : 1,
  BLOCKED : 2
})

export class GridSquare {

  constructor(x, y, elem) {
    this.x = x            // x coordinate of grid square
    this.y = y            // y coordinate of grid square
    this.elem = elem      // dom element of the grid square
    this.state = GridState.EMPTY
  }
}

