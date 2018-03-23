import Ship from './Ship'

export const ShipGrid = {
  ALIVE   : 0,
  KILLED  : 1
}

export class Carrier extends Ship {

  constructor(id, orientation) {
    super(id, 4, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Cruiser extends Ship {

  constructor(id, orientation) {
    super(id, 3, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Destroyer extends Ship {

  constructor(id, orientation) {
    super(id, 2, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class TorpedoBoat extends Ship {

  constructor(id, orientation) {
    super(id, 1, orientation)
    this.gridState = [ShipGrid.ALIVE]
  }

}

