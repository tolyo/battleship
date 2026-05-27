import { GRID_SIZE } from './constants.js';

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
 * @typedef {{ row: number, column: number }} ShipCoordinate
 */

/**
 * @typedef {{ row: string, column: string, orientation: ShipOrientation }} ShipLocation
 */

export default class Ship {
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
    this.health = 'ACTIVE';

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
     * @type {string}
     */
    this.placeholderWidth = `${size * GRID_SIZE}px`;

    /**
     * @type {ShipOrientation}
     */
    this.orientation = 'HORIZONTAL';

    /**
     * @type {number}
     */
    this.column = 0;

    /**
     * @type {number}
     */
    this.row = 0;

    /**
     * @type {ShipCoordinate[] | undefined}
     */
    this.coordinates = undefined;
  }

  reset() {
    this.coordinates = undefined;
    this.health = 'ACTIVE';
    this.gridState.forEach((_, index) => {
      this.gridState[index] = true;
    });
    this.hitcount = 0;
    return this;
  }

  /**
   * @param {ShipLocation} location
   * @returns {this}
   */
  setLocation({ column, row, orientation }) {
    this.column = parseInt(column, 10);
    this.row = parseInt(row, 10);
    this.orientation = orientation;
    this.coordinates = undefined;
    return this;
  }

  /**
   * @returns {ShipCoordinate[]}
   */
  getShipMapCoordinates() {
    if (this.coordinates !== undefined) {
      return this.coordinates;
    }

    /** @type {ShipCoordinate[]} */
    const coordinates = [];
    for (let i = 0; i < this.size; i += 1) {
      if (this.orientation === 'HORIZONTAL') {
        coordinates.push({ row: this.row, column: this.column + i });
      } else {
        coordinates.push({ row: this.row + i, column: this.column });
      }
    }
    this.coordinates = coordinates;
    return this.coordinates;
  }

  /**
   * @param {number} targetRow
   * @param {number} targetColumn
   */
  attemptStrike(targetRow, targetColumn) {
    this.getShipMapCoordinates().forEach(({ row, column }, index) => {
      if (targetRow === row && targetColumn === column) {
        this.gridState[index] = false;
        this.strike();
      }
    });
  }

  strike() {
    if (this.health === 'KILLED') {
      throw new Error('Illegal state. Ship already killed');
    }

    this.hitcount += 1;
    if (this.hitcount === this.size) {
      this.health = 'KILLED';
    } else {
      this.health = 'DAMAGED';
    }
  }
}
