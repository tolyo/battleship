import Ship from './Ship'

export const ShipGrid = {
  ALIVE   : 0,
  KILLED  : 1
}

export class Carrier extends Ship {

  constructor(id) {
    super(id, 4)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Cruiser extends Ship {

  constructor(id) {
    super(id, 3)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Destroyer extends Ship {

  constructor(id) {
    super(id, 2)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class TorpedoBoat extends Ship {

  constructor(id) {
    super(id, 1)
    this.gridState = [ShipGrid.ALIVE]
  }

}

