import Ship from './Ship'

export class Carrier extends Ship {

  constructor(id, orientation) {
    super(id, 4, orientation)
  }

}

export class Cruiser extends Ship {

  constructor(id, orientation) {
    super(id, 3, orientation)
  }

}

export class Destroyer extends Ship {

  constructor(id, orientation) {
    super(id, 2, orientation)
  }

}

export class TorpedoBoat extends Ship {

  constructor(id, orientation) {
    super(id, 1, orientation)
  }

}

