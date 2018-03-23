import Ship from './Ship'

export class Carrier extends Ship {

  constructor(id, orientation) {
    super(id, 4, orientation)
    this.gridState = [0, 0, 0, 0]
  }

}

export class Cruiser extends Ship {

  constructor(id, orientation) {
    super(id, 3, orientation)
    this.gridState = [0, 0, 0]
  }

}

export class Destroyer extends Ship {

  constructor(id, orientation) {
    super(id, 2, orientation)
    this.gridState = [0, 0]
  }

}

export class TorpedoBoat extends Ship {

  constructor(id, orientation) {
    super(id, 1, orientation)
    this.gridState = [0]
  }

}

