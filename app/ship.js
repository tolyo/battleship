
export const ShipState = Object.freeze({
  ACTIVE: 'ACTIVE',
  WOUNDED: 'WOUNDED',
  KILLED: 'KILLED'
})

export const ShipOrientation = Object.freeze({
  VERTICAL: 'VERTICAL',
  HORIZONTAL: 'HORIZONTAL'
})

export default class Ship {
  constructor (id, size) {
    this.id = id
    this.health = ShipState.ACTIVE
    this.gridState = []

    this.hitcount = 0
    this.size = size
    this.domState = [] // reference to dom elements occupied by a ship
  }

  reset () {
    this.coordinates = undefined
    this.health = ShipState.ACTIVE
    this.gridState.forEach((e, index) => {
      this.gridState[index] = ShipGrid.ALIVE
    })
    this.hitcount = 0
    this.domState = [] // reference to dom elements occupied by a ship
    return this
  }

  setColumn (column) {
    this.column = column
    return this
  }

  setRow (row) {
    this.row = row
    return this
  }

  setOrienation (orientation = ShipOrientation.HORIZONTAL) {
    this.orientation = orientation
    return this
  }

  setLocation ({ column, row, orientation }) {
    this.column = column
    this.row = row
    this.orientation = orientation
    this.coordinates = undefined
    return this
  }

  getShipMapCoordinates () {
    if (this.coordinates !== undefined) {
      return this.coordinates
    }
    const coordinates = []
    for (let i = 0; i < this.size; i++) {
      if (this.orientation === ShipOrientation.HORIZONTAL) {
        coordinates.push({ y: this.row, x: this.column + i })
      } else {
        coordinates.push({ y: this.row + i, x: this.column })
      }
    }
    this.coordinates = coordinates
    return this.coordinates
  }

  attemptStrike (column, row) {
    this.getShipMapCoordinates().forEach(({ y, x }, index) => {
      if (column === x && row === x) {
        this.gridState[index] = ShipGrid.KILLED
        this.strike()
      }
    })
  }

  strike () {
    this.hitcount++
    return this
  }

  isKilled () {
    return this.hitcount === this.size
  }
}

export const ShipGrid = {
  ALIVE: 0,
  KILLED: 1
}

export class Carrier extends Ship {
  constructor (id) {
    super(id, 4)
    this.gridState = [
      ShipGrid.ALIVE,
      ShipGrid.ALIVE,
      ShipGrid.ALIVE,
      ShipGrid.ALIVE
    ]
  }
}

export class Cruiser extends Ship {
  constructor (id) {
    super(id, 3)
    this.gridState = [
      ShipGrid.ALIVE,
      ShipGrid.ALIVE,
      ShipGrid.ALIVE
    ]
  }
}

export class Destroyer extends Ship {
  constructor (id) {
    super(id, 2)
    this.gridState = [
      ShipGrid.ALIVE,
      ShipGrid.ALIVE
    ]
  }
}

export class TorpedoBoat extends Ship {
  constructor (id) {
    super(id, 1)
    this.gridState = [ShipGrid.ALIVE]
  }
}
