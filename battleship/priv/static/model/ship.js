import { GRID_SIZE } from '../constants.js';

/**
 * @typedef {'ACTIVE' | 'DAMAGED' | 'KILLED'} ShipState
 */

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} ShipOrientation
 */

/**
 * @typedef {boolean} GridAlive
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
     * @type {ShipOrientation}
     */
    this.orientation = 'HORIZONTAL';

    /**
     * @type {?HTMLElement}
     */
    this.shipElement = null;

    /**
     * @type {?HTMLElement}
     */
    this.placeHolder = null;

    /**
     * @type {number}
     */
    this.shiftX = 0;

    /**
     * @type {number}
     */
    this.shiftY = 0;
  }

  reset() {
    this.coordinates = undefined;
    this.health = 'ACTIVE';
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
    if (this.health === 'KILLED') {
      throw new Error('Illegal state. Ship already killed');
    }
    this.hitcount += 1;
    if (this.isKilled() === true) {
      this.health = 'KILLED';
    } else {
      this.health = 'DAMAGED';
    }
  }

  /**
   * @returns boolean
   */
  isKilled() {
    return this.hitcount === this.size;
  }

  /**
   * Creates a placeholder for this ship and attach it to target element
   * @param {HTMLElement} elem
   */
  createPlaceHolder(elem) {
    this.placeHolder = document.createElement('div');
    this.placeHolder.className = `place-holder`;
    this.placeHolder.id = `placeholder-${this.id}`;
    this.placeHolder.style.width = `${this.size * GRID_SIZE}px`;
    elem.appendChild(this.placeHolder);
  }

  createDomElement() {
    this.shipElement = document.createElement('div');
    this.shipElement.classList.add('ship');
    this.shipElement.id = this.id; // each div is identified by ship id
    const { width, height } = this.calculateSize();
    this.shipElement.style.width = width;
    this.shipElement.style.height = height;
  }

  createOnPlaceholder() {
    this.createDomElement();
    this.placeHolder.appendChild(this.shipElement);
    this.shipElement.style.left = `${this.placeHolder.getBoundingClientRect().left + window.scrollX}px`;
    this.shipElement.style.top = `${this.placeHolder.getBoundingClientRect().top + window.scrollY}px`;
    // //     // set event handlers
    this.shipElement.onmousedown = (e) => this.onmousedown(e);
    // override default browser behavior
    this.shipElement.ondragstart = () => false;
    this.shipElement.onmouseup = () => false;
  }

  /**
   * @param {MouseEvent} e
   */
  onmousedown(e) {
    if (e.button !== undefined && e.button !== 0) return; // only touch or left click
    // if (e.touches && e.touches.length > 1) return; // support one finger touch only
    // if (['PLAYING', 'ENDED'].includes(gameEngine.getState())) return;
    e.preventDefault();

    const shipCoordinates = this.getShipCoordinates();
    // set offsets for the click event
    console.log(shipCoordinates);
    this.shiftX = e.pageX - shipCoordinates.left;
    this.shiftY = e.pageY - shipCoordinates.top;
    this.shipElement.classList.add('dragged');
    document.onmousemove = (e) => this.onmousemove(e);
    document.onmouseup = (e) => this.onmouseup(e);
  }

  /**
   * @typedef {Object} shipElementSize
   * @property {string} width - px
   * @property {string} height - px
   */

  /**
   * @return {shipElementSize}
   */
  calculateSize() {
    return {
      width:
        this.orientation === 'HORIZONTAL'
          ? `${GRID_SIZE * this.size}px`
          : `${GRID_SIZE}px`,
      height:
        this.orientation === 'VERTICAL'
          ? `${GRID_SIZE * this.size}px`
          : `${GRID_SIZE}px`,
    };
  }

  /**
   * @param {MouseEvent} e
   */
  onmousemove(e) {
    this.shipElement.style.left = `${Math.floor(e.pageX - this.shiftX)}px`;
    this.shipElement.style.top = `${Math.floor(e.pageY - this.shiftY)}px`;
  }

  /**
   *
   * @param {MouseEvent} e
   */
  onmouseup(e) {
    // clear event bindings
    document.onmousemove = null;
    document.onmouseup = null;
    this.shipElement.classList.remove('dragged');

    // Check if legal otherwise recreate on placeholder
    if (false) {
      // TODO
    } else {
      this.shipElement.remove();
      this.createOnPlaceholder();
    }
  }

  getShipCoordinates() {
    const box = this.shipElement.getBoundingClientRect();
    return {
      left: box.left + window.scrollX,
      top: box.top + window.scrollY,
    };
  }
}
