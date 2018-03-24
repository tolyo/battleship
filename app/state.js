
export const State = {
  grid : [],    // dynamic map of dom elements
}

export const MapTile = Object.freeze({
  EMPTY   : '_',
  FILLED  : 'X'
})

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

