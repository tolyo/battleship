/**
 * @typedef {'ACTIVE' | 'DAMAGED' | 'KILLED'} ShipState
 */

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} ShipOrientation
 */

/**
 * @typedef {boolean} GridAlive
 */

/**
 * @abstract
 */
export class Ship {
  /**
   * @param {string} id
   * @param {number} size
   */
  constructor(id, size) {
    /**
     * @type {string}
     */
    this.id = id;

    /**
     * @type {ShipState}
     */
    this.health = "ACTIVE";

    /**
     * @type {GridAlive[]}
     */
    this.gridState = Array(size).fill(true);

    /**
     * @type {number}
     */
    this.hitcount = 0;

    /**
     * @type {number}
     */
    this.size = size;

    /**
     * @type {ShipOrientation}
     */
    this.orientation = "HORIZONTAL";
  }

  reset() {
    this.coordinates = undefined;
    this.health = "ACTIVE";
    this.gridState.forEach((e, index) => {
      this.gridState[index] = true;
    });
    this.hitcount = 0;
    return this;
  }

  setLocation({ column, row, orientation }) {
    this.column = column;
    this.row = row;
    this.orientation = orientation;
    this.coordinates = undefined;
    return this;
  }

  getShipMapCoordinates() {
    if (this.coordinates !== undefined) {
      return this.coordinates;
    }
    const coordinates = [];
    for (let i = 0; i < this.size; i++) {
      if (this.orientation === "HORIZONTAL") {
        coordinates.push({ row: this.row, column: this.column + i });
      } else {
        coordinates.push({ row: this.row + i, column: this.column });
      }
    }
    this.coordinates = coordinates;
    return this.coordinates;
  }

  attemptStrike(targetRow, targetColumn) {
    this.getShipMapCoordinates().forEach(({ row, column }, index) => {
      if (targetRow === row && targetColumn === column) {
        this.gridState[index] = false;
        this.strike();
      }
    });
  }

  /**
   * @returns
   */
  strike() {
    if (this.health === "KILLED") {
      throw new Error("Illegal state. Ship already killed");
    }
    this.hitcount++;
    if (this.isKilled() === true) {
      this.health = "KILLED";
    } else {
      this.health = "DAMAGED";
    }
  }

  /**
   * @returns boolean
   */
  isKilled() {
    return this.hitcount === this.size;
  }
}

export class Carrier extends Ship {
  /**
   *
   * @param {string} id
   */
  constructor(id) {
    super(id, 4);
  }
}

export class Cruiser extends Ship {
  constructor(id) {
    super(id, 3);
  }
}

export class Destroyer extends Ship {
  constructor(id) {
    super(id, 2);
  }
}

export class TorpedoBoat extends Ship {
  constructor(id) {
    super(id, 1);
  }
}
