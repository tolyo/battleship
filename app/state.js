
export class GameState {

  constructor() {
    this.grid = []
  }
}

export class GridSquare {

  constructor(x, y, elem) {
    this.x = x
    this.y = y
    this.elem = elem
  }
}