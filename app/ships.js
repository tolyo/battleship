import Ship from './Ship'

export const ShipGrid = {
  ALIVE   : 0,
  KILLED  : 1
}

export class Carrier extends Ship {

  constructor(id,column, row, orientation) {
    super(id, column, row, 4, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Cruiser extends Ship {

  constructor(id, column, row, orientation) {
    super(id, column, row, 3, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Destroyer extends Ship {

  constructor(id, column, row, orientation) {
    super(id, column, row, 2, orientation)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class TorpedoBoat extends Ship {

  constructor(id, column, row, orientation) {
    super(id, column, row, 1, orientation)
    this.gridState = [ShipGrid.ALIVE]
  }

}

