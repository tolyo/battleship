import { GRID_SIZE } from '../constants.js';
import { MapTile } from '../strikemap.js';

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
     * @type {HTMLElement[]}
     */
    this.elementsBelow = [];

    /**
     * @type {number}
     */
    this.shiftX = 0;

    /**
     * @type {number}
     */
    this.shiftY = 0;

    /**
     * @type {?Element}
     */
    this.currentDroppable = null;

    // Ensure reclaim of tiles in case 
    document.addEventListener('mousedown', 
    /**
     * 
     * @param {MouseEvent} e 
     */
    (e) => {
      if (/** @type {HTMLElement} */ (e.target).id !== this.id) {
        this.claimTiles();
      }
    });
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
    console.log("onmousedown");
    if (e.button !== undefined && e.button !== 0) return; // only touch or left click
    // if (e.touches && e.touches.length > 1) return; // support one finger touch only
    // if (['PLAYING', 'ENDED'].includes(gameEngine.getState())) return;
    e.preventDefault();

    const shipCoordinates = this.getShipCoordinates();
    // set offsets for the click event
    this.shiftX = e.pageX - shipCoordinates.left;
    this.shiftY = e.pageY - shipCoordinates.top;
    this.shipElement.classList.add('dragged');
    // On move, clear all old tiles
    this.elementsBelow.forEach((tile) => {
      this.getAdjacents(tile).forEach((adjacentTile) => {
        if (adjacentTile.dataset.state === MapTile.BLOCKED) {
          adjacentTile.dataset.state = MapTile.EMPTY;
        }
      });
      tile.dataset.state = MapTile.EMPTY;
    });
    this.elementsBelow = [];
    
    
    document.onmousemove = (moveEvt) => this.onmousemove(moveEvt);
    document.onmouseup = (upEvt) => this.onmouseup(upEvt);
  }
  
  /**
   * @param {MouseEvent} e
   */
  onmousemove(e) {
    const x = Math.floor(e.pageX - this.shiftX);
    const y = Math.floor(e.pageY - this.shiftY);
    this.shipElement.style.left = `${x}px`;
    this.shipElement.style.top = `${y}px`;
    this.shipElement.hidden = true;

    const elementBelow = /** @type {HTMLElement} */ (
      document.elementFromPoint(x + 15, y + 15)
    ); // casting for JSdoc
    if (elementBelow?.classList?.contains('fleetboard-tile')) {
      if (this.isLegal(elementBelow.dataset.row, elementBelow.dataset.column)) {
        this.elementsBelow.forEach((el) => el.classList.add('droppable-target'));
      } else {
        this.elementsBelow = [];
      }
    } 

    this.shipElement.hidden = false;
  }

  /**
   *
   */
  onmouseup() {
    // clear event bindings
    document.onmousemove = null;
    document.onmouseup = null;
    this.shipElement.classList.remove('dragged');

    // Check if legal otherwise recreate on placeholder
    if (this.elementsBelow.length > 0) {
      this.shipElement.style.left = `${this.elementsBelow[0].getBoundingClientRect().left + window.scrollX}px`;
      this.shipElement.style.top = `${this.elementsBelow[0].getBoundingClientRect().top + window.scrollY}px`;
      this.claimTiles();
      // possibly set blocked
    } else {
      this.shipElement.remove();
      this.createOnPlaceholder();
    }
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
   * Checks legality and popullates state with elements belpw
   * @param {string} row
   * @param {string} column
   * @returns {boolean}
   */
  isLegal(row, column) {
    let res = true;
    this.resetElementsBelow();
    const x = parseInt(row, 10);
    const y = parseInt(column, 10);
    if (this.orientation === 'HORIZONTAL') {
      if (y + this.size - 1 >= 10) return false;
      for (let i = y; i < y + this.size; i += 1) {
        const tile = document.getElementById(`fleetboard-${x}-${i}`);
        if (tile.dataset.state !== MapTile.EMPTY) {
          res = false;
        }
        // eslint-disable-next-line no-loop-func
        this.getAdjacents(tile).forEach((adjacent) => {
          if (adjacent.dataset.state === MapTile.FILLED) {
            res = false;
          };
        });
        
        if (res) {
          this.elementsBelow.push(tile);
        }
      }
    }
    return res;
  }

  resetElementsBelow() {
    this.elementsBelow.forEach((e) => e.classList.remove('droppable-target'));
    this.elementsBelow = []; // always reset
  }

  getShipCoordinates() {
    const box = this.shipElement.getBoundingClientRect();
    return {
      left: box.left + window.scrollX,
      top: box.top + window.scrollY,
    };
  }

  claimTiles() {
    this.elementsBelow.forEach((tile) => {
      tile.dataset.state = MapTile.FILLED;
      this.blockAdjacents(tile);
    });
  }

  /**
   * @param {HTMLElement} tile
   */
  blockAdjacents(tile) {
    this.getAdjacents(tile).forEach((adjacentTile) => {
      if (adjacentTile.dataset.state == MapTile.EMPTY) {
        adjacentTile.dataset.state = MapTile.BLOCKED;
      }
    });
  }

  /**
   * @returns {boolean}
   */
  isPlaced() {
    return this.elementsBelow.length > 0;
  }

  /**
   *
   * @param {HTMLElement} elem
   * @returns {HTMLElement[]} list of adjacent tiles
   */
  getAdjacents(elem) {
    const row = parseInt(elem.dataset.row);
    const column = parseInt(elem.dataset.column);
    const coordinates = [];
    // top left
    if (row !== 0 && column !== 0) {
      coordinates.push({ row: row - 1, column: column - 1 });
    }

    // top mid
    if (row !== 0) {
      coordinates.push({ row: row - 1, column });
    }

    // top right
    if (row !== 0 && column !== 9) {
      coordinates.push({ row: row - 1, column: column + 1 });
    }

    // mid left
    if (column !== 0) {
      coordinates.push({ row, column: column - 1 });
    }

    // mid right
    if (column !== 9) {
      coordinates.push({ row, column: column + 1 });
    }

    // bot left
    if (row !== 9 && column !== 0) {
      coordinates.push({ row: row + 1, column: column - 1 });
    }

    // bot mid
    if (row !== 9) {
      coordinates.push({ row: row + 1, column });
    }

    // bot right
    if (row !== 9 && column !== 9) {
      coordinates.push({ row: row + 1, column: column + 1 });
    }

    return coordinates.map(({ row, column }) =>
      document.getElementById(`fleetboard-${row}-${column}`)
    );
  }
}