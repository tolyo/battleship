
export const State = {
  grid : []
}

export const GridState = {
  EMPTY   : 0,
  FILLED  : 1
}

export class GridSquare {

  constructor(x, y, elem) {
    this.x = x            // x coordinate of grid square
    this.y = y            // y coordinate of grid square
    this.elem = elem      // dom element of the grid square
    this.state = GridState.EMPTY
  }
}