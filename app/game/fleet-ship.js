import { GRID_SIZE, MapTile } from './constants.js';

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} ShipOrientation
 */

/**
 * @typedef {{ row: number, column: number }} ShipCoordinate
 */

/**
 * @typedef {{ width: string, height: string }} ShipElementSize
 */

export class FleetShipController {
  static $inject = ['$element', 'gameState'];

  /**
   * @param {Element} $element
   * @param {import('./game-state-service.js').GameStateService} gameState
   */
  constructor($element, gameState) {
    this.$element = $element;
    this.gameState = gameState;
    /** @type {import('./ship.js').default | undefined} */
    this.ship = undefined;
    /** @type {HTMLElement | undefined} */
    this.placeHolder = undefined;
    /** @type {HTMLElement | undefined} */
    this.shipElement = undefined;
    /** @type {HTMLElement[]} */
    this.elementsBelow = [];
    /** @type {ShipOrientation} */
    this.orientation = 'HORIZONTAL';
    this.locked = false;
    this.dragging = false;
    /** @type {number | undefined} */
    this.activePointerId = undefined;
    this.firstMove = false;
    this.shiftX = 0;
    this.shiftY = 0;
    /** @type {(() => void) | undefined} */
    this.unsubscribeFleetLock = undefined;
    /** @type {(() => void) | undefined} */
    this.unregisterFleetShip = undefined;
    /** @type {((event: Event) => void) | undefined} */
    this.claimListener = undefined;
    /** @type {((event: PointerEvent) => void) | undefined} */
    this.pointerDownListener = undefined;
  }

  $postLink() {
    if (!this.ship) {
      return;
    }

    const placeHolder = this.$element.parentElement;
    const shipElement = this.$element.querySelector('.ship');
    if (
      !(placeHolder instanceof HTMLElement) ||
      !(shipElement instanceof HTMLElement)
    ) {
      return;
    }

    this.placeHolder = placeHolder;
    this.shipElement = shipElement;
    this.setRotation();
    this.unsubscribeFleetLock = this.gameState.subscribeFleetLock((locked) => {
      this.setLocked(locked);
    });
    this.unregisterFleetShip = this.gameState.registerFleetShip(this);

    this.claimListener = (event) => {
      const customEvent = /** @type {CustomEvent<{ id: string }>} */ (event);
      if (customEvent.detail.id !== this.ship?.id) {
        this.claimTiles();
      }
    };
    document.addEventListener('claim', this.claimListener);

    this.pointerDownListener = (event) => {
      if (
        event.target instanceof HTMLElement &&
        event.target.id !== this.ship?.id
      ) {
        this.claimTiles();
      }
    };
    document.addEventListener('pointerdown', this.pointerDownListener);

    this.setOnPlaceholder();
  }

  /**
   * @returns {HTMLElement}
   */
  getShipElement() {
    if (!this.shipElement) {
      throw new Error('Ship element has not been linked.');
    }

    return this.shipElement;
  }

  /**
   * @returns {HTMLElement}
   */
  getPlaceHolder() {
    if (!this.placeHolder) {
      throw new Error('Ship placeholder has not been linked.');
    }

    return this.placeHolder;
  }

  setOnPlaceholder() {
    this.clearMapBlocks();
    this.orientation = 'HORIZONTAL';
    this.setRotation();
    const shipElement = this.getShipElement();
    const placeHolder = this.getPlaceHolder();
    shipElement.style.left = `${placeHolder.getBoundingClientRect().left + window.scrollX}px`;
    shipElement.style.top = `${placeHolder.getBoundingClientRect().top + window.scrollY}px`;
  }

  realignToLayout() {
    this.setRotation();
    const shipElement = this.getShipElement();

    if (this.elementsBelow.length > 0) {
      const firstTile = this.elementsBelow[0];
      shipElement.style.left = `${firstTile.getBoundingClientRect().left + window.scrollX}px`;
      shipElement.style.top = `${firstTile.getBoundingClientRect().top + window.scrollY}px`;
      return;
    }

    const placeHolder = this.getPlaceHolder();
    shipElement.style.left = `${placeHolder.getBoundingClientRect().left + window.scrollX}px`;
    shipElement.style.top = `${placeHolder.getBoundingClientRect().top + window.scrollY}px`;
  }

  /**
   * @param {{ row: number, column: number }[]} coordinates
   */
  placeOnBoardCoordinates(coordinates) {
    if (!this.ship || coordinates.length === 0) {
      this.getShipElement().hidden = true;
      return;
    }

    const sorted = [...coordinates].sort((left, right) =>
      left.row === right.row ? left.column - right.column : left.row - right.row
    );
    const sameRow = sorted.every((coordinate) => coordinate.row === sorted[0].row);
    this.orientation = sameRow ? 'HORIZONTAL' : 'VERTICAL';
    this.setRotation();
    this.elementsBelow = sorted
      .map((coordinate) =>
        document.getElementById(`fleetboard-${coordinate.row}-${coordinate.column}`)
      )
      .filter((tile) => tile instanceof HTMLElement);

    const firstTile = this.elementsBelow[0];
    const shipElement = this.getShipElement();
    if (!firstTile) {
      shipElement.hidden = true;
      return;
    }

    shipElement.hidden = false;
    shipElement.style.left = `${firstTile.getBoundingClientRect().left + window.scrollX}px`;
    shipElement.style.top = `${firstTile.getBoundingClientRect().top + window.scrollY}px`;
  }

  onDoubleClick() {
    if (
      !this.ship ||
      this.locked ||
      this.ship.size === 1 ||
      this.elementsBelow.length === 0
    ) {
      return;
    }

    const { row, column } = this.elementsBelow[0].dataset;
    if (!row || !column) {
      return;
    }

    this.elementsBelow.forEach((element) =>
      element.classList.remove('droppable-target')
    );
    this.clearMapBlocks();

    if (this.isLegal(row, column, this.getOppositeOrientation())) {
      this.orientation = this.getOppositeOrientation();
      this.setRotation();
    }

    this.elementsBelow = this.getElementsBelow(row, column, this.orientation);
    this.claimTiles();
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerDown(event) {
    if (event.isPrimary === false || !this.startDrag(event)) {
      return;
    }

    event.preventDefault();
    this.activePointerId = event.pointerId;
    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    target?.setPointerCapture(event.pointerId);
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerMove(event) {
    if (!this.dragging || event.pointerId !== this.activePointerId) {
      return;
    }

    this.moveShip(event);
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerUp(event) {
    if (!this.dragging || event.pointerId !== this.activePointerId) {
      return;
    }

    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    target?.releasePointerCapture(event.pointerId);
    this.finishDrag();
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerCancel(event) {
    if (!this.dragging || event.pointerId !== this.activePointerId) {
      return;
    }

    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    target?.releasePointerCapture(event.pointerId);
    this.finishDrag();
  }

  /**
   * @param {PointerEvent} event
   * @returns {boolean}
   */
  startDrag(event) {
    if (!this.ship || this.locked) {
      return false;
    }
    if (event.button !== undefined && event.button !== 0) {
      return false;
    }

    const shipCoordinates = this.getShipCoordinates();
    this.shiftX = event.pageX - shipCoordinates.left;
    this.shiftY = event.pageY - shipCoordinates.top;
    this.getShipElement().classList.add('dragged');
    this.firstMove = true;
    this.dragging = true;
    return true;
  }

  /**
   * @param {PointerEvent} event
   */
  moveShip(event) {
    if (this.firstMove) {
      this.clearMapBlocks();
      this.firstMove = false;
    }

    const x = Math.floor(event.pageX - this.shiftX);
    const y = Math.floor(event.pageY - this.shiftY);
    const shipElement = this.getShipElement();
    shipElement.style.left = `${x}px`;
    shipElement.style.top = `${y}px`;
    shipElement.hidden = true;

    const elementBelow = /** @type {HTMLElement | null} */ (
      document.elementFromPoint(x + 15, y + 15)
    );
    this.resetElementsBelow();

    if (elementBelow?.classList?.contains('fleetboard-tile')) {
      const { row, column } = elementBelow.dataset;
      if (row && column && this.isLegal(row, column, this.orientation)) {
        this.elementsBelow = this.getElementsBelow(
          row,
          column,
          this.orientation
        );
        this.elementsBelow.forEach((element) =>
          element.classList.add('droppable-target')
        );
      }
    }

    shipElement.hidden = false;
  }

  finishDrag() {
    this.dragging = false;
    this.activePointerId = undefined;
    const shipElement = this.getShipElement();
    shipElement.classList.remove('dragged');

    if (this.elementsBelow.length > 0) {
      const firstTile = this.elementsBelow[0];
      shipElement.style.left = `${firstTile.getBoundingClientRect().left + window.scrollX}px`;
      shipElement.style.top = `${firstTile.getBoundingClientRect().top + window.scrollY}px`;
      this.claimTiles();
    } else {
      this.setOnPlaceholder();
    }
  }

  setRotation() {
    const { width, height } = this.calculateSize();
    const shipElement = this.getShipElement();
    shipElement.style.width = width;
    shipElement.style.height = height;
  }

  /**
   * @returns {ShipElementSize}
   */
  calculateSize() {
    if (!this.ship) {
      return { width: `${GRID_SIZE}px`, height: `${GRID_SIZE}px` };
    }

    return {
      width:
        this.orientation === 'HORIZONTAL'
          ? `${GRID_SIZE * this.ship.size}px`
          : `${GRID_SIZE}px`,
      height:
        this.orientation === 'VERTICAL'
          ? `${GRID_SIZE * this.ship.size}px`
          : `${GRID_SIZE}px`,
    };
  }

  /**
   * @returns {ShipOrientation}
   */
  getOppositeOrientation() {
    return this.orientation === 'HORIZONTAL' ? 'VERTICAL' : 'HORIZONTAL';
  }

  /**
   * @param {string} row
   * @param {string} column
   * @param {ShipOrientation} orientation
   * @returns {HTMLElement[]}
   */
  getElementsBelow(row, column, orientation) {
    if (!this.ship) {
      return [];
    }

    /** @type {HTMLElement[]} */
    const elementsBelow = [];
    const y = parseInt(row, 10);
    const x = parseInt(column, 10);
    if (orientation === 'HORIZONTAL') {
      for (let i = x; i < x + this.ship.size; i += 1) {
        const tile = document.getElementById(`fleetboard-${y}-${i}`);
        if (tile instanceof HTMLElement) {
          elementsBelow.push(tile);
        }
      }
    } else {
      for (let i = y; i < y + this.ship.size; i += 1) {
        const tile = document.getElementById(`fleetboard-${i}-${x}`);
        if (tile instanceof HTMLElement) {
          elementsBelow.push(tile);
        }
      }
    }

    return elementsBelow;
  }

  /**
   * @param {string} row
   * @param {string} column
   * @param {ShipOrientation} orientation
   * @returns {boolean}
   */
  isLegal(row, column, orientation) {
    if (!this.ship) {
      return false;
    }

    const y = parseInt(row, 10);
    const x = parseInt(column, 10);
    if (!Number.isInteger(y) || !Number.isInteger(x)) {
      return false;
    }

    const maxOffset = this.ship.size - 1;
    if (
      (orientation === 'HORIZONTAL' && x + maxOffset >= 10) ||
      (orientation === 'VERTICAL' && y + maxOffset >= 10)
    ) {
      return false;
    }

    return this.getElementsBelow(row, column, orientation).every((tile) => {
      if (tile.dataset.state !== MapTile.EMPTY) {
        return false;
      }

      return this.getAdjacents(tile).every(
        (adjacent) => adjacent.dataset.state !== MapTile.FILLED
      );
    });
  }

  clearMapBlocks() {
    this.elementsBelow.forEach((tile) => {
      this.getAdjacents(tile).forEach((adjacentTile) => {
        if (adjacentTile.dataset.state === MapTile.BLOCKED) {
          adjacentTile.dataset.state = MapTile.EMPTY;
        }
      });
      tile.dataset.state = MapTile.EMPTY;
    });
    this.elementsBelow = [];
    document.dispatchEvent(
      new CustomEvent('claim', { detail: { id: this.ship?.id } })
    );
  }

  resetElementsBelow() {
    this.elementsBelow.forEach((element) =>
      element.classList.remove('droppable-target')
    );
    this.elementsBelow = [];
  }

  getShipCoordinates() {
    const box = this.getShipElement().getBoundingClientRect();
    return {
      left: box.left + window.scrollX,
      top: box.top + window.scrollY,
    };
  }

  claimTiles() {
    this.elementsBelow.forEach((tile) => {
      tile.dataset.state = MapTile.FILLED;
      this.blockAdjacents(tile);
      tile.classList.remove('droppable-target');
    });
  }

  /**
   * @param {HTMLElement} tile
   */
  blockAdjacents(tile) {
    this.getAdjacents(tile).forEach((adjacentTile) => {
      if (adjacentTile.dataset.state === MapTile.EMPTY) {
        adjacentTile.dataset.state = MapTile.BLOCKED;
      }
    });
  }

  /**
   * @param {HTMLElement} elem
   * @returns {HTMLElement[]}
   */
  getAdjacents(elem) {
    const row = parseInt(elem.dataset.row ?? '', 10);
    const column = parseInt(elem.dataset.column ?? '', 10);
    if (!Number.isInteger(row) || !Number.isInteger(column)) {
      return [];
    }
    /** @type {ShipCoordinate[]} */
    const coordinates = [];

    if (row !== 0 && column !== 0) {
      coordinates.push({ row: row - 1, column: column - 1 });
    }
    if (row !== 0) {
      coordinates.push({ row: row - 1, column });
    }
    if (row !== 0 && column !== 9) {
      coordinates.push({ row: row - 1, column: column + 1 });
    }
    if (column !== 0) {
      coordinates.push({ row, column: column - 1 });
    }
    if (column !== 9) {
      coordinates.push({ row, column: column + 1 });
    }
    if (row !== 9 && column !== 0) {
      coordinates.push({ row: row + 1, column: column - 1 });
    }
    if (row !== 9) {
      coordinates.push({ row: row + 1, column });
    }
    if (row !== 9 && column !== 9) {
      coordinates.push({ row: row + 1, column: column + 1 });
    }

    return coordinates
      .map(({ row, column }) =>
        document.getElementById(`fleetboard-${row}-${column}`)
      )
      .filter((tile) => tile instanceof HTMLElement);
  }

  /**
   * @returns {boolean}
   */
  tryRandomLocation() {
    this.setOnPlaceholder();
    const { row, column, orientation } = getRandomShipCoordinate();
    if (this.isLegal(row, column, orientation)) {
      this.orientation = orientation;
      this.setRotation();
      this.elementsBelow = this.getElementsBelow(row, column, orientation);
      const firstTile = this.elementsBelow[0];
      if (!firstTile) {
        return false;
      }
      const shipElement = this.getShipElement();
      shipElement.style.left = `${firstTile.getBoundingClientRect().left + window.scrollX}px`;
      shipElement.style.top = `${firstTile.getBoundingClientRect().top + window.scrollY}px`;
      this.claimTiles();
      return true;
    }
    return false;
  }

  /**
   * @param {boolean} locked
   */
  setLocked(locked) {
    this.locked = locked;
    this.shipElement?.classList.toggle('locked', locked);
    this.shipElement?.setAttribute('aria-disabled', locked ? 'true' : 'false');
  }

  $onDestroy() {
    this.dragging = false;
    this.activePointerId = undefined;
    this.unsubscribeFleetLock?.();
    this.unregisterFleetShip?.();
    if (this.claimListener) {
      document.removeEventListener('claim', this.claimListener);
    }
    if (this.pointerDownListener) {
      document.removeEventListener('pointerdown', this.pointerDownListener);
    }
  }
}

export default {
  bindings: {
    ship: '<',
  },
  template: `
    <div
      class="ship"
      ng-class="{ locked: $ctrl.locked }"
      ng-attr-id="{{$ctrl.ship.id}}"
      ng-on-pointerdown="$ctrl.onPointerDown($event)"
      ng-on-pointermove="$ctrl.onPointerMove($event)"
      ng-on-pointerup="$ctrl.onPointerUp($event)"
      ng-on-pointercancel="$ctrl.onPointerCancel($event)"
      ng-on-dblclick="$ctrl.onDoubleClick()">
    </div>
  `,
  controller: FleetShipController,
};

function getRandomTile() {
  return `${Math.floor(Math.random() * 9)}`;
}

/**
 * @returns {ShipOrientation}
 */
function getRandomOrientation() {
  if (Math.round(Math.random()) > 0) {
    return 'HORIZONTAL';
  }
  return 'VERTICAL';
}

function getRandomShipCoordinate() {
  return {
    row: getRandomTile(),
    column: getRandomTile(),
    orientation: getRandomOrientation(),
  };
}
