import {
  alignElementTo,
  coordinatesFromElements,
  elementPagePosition,
  fleetBoardTileAt,
  fleetBoardTileFromPoint,
  fleetBoardTilesForShip,
  orientationFromCoordinates,
  setElementPosition,
  shipElementSize,
  sortedCoordinates,
  tileDataset,
} from './fleet-ship-geometry.js';
import { shipCoordinatesFromStart } from './fleet-placement.js';

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} ShipOrientation
 */

export class FleetShipController {
  static $inject = ['$element', 'gameState', 'fleetLayout'];

  /**
   * @param {Element} $element
   * @param {import('./game-state-service.js').GameStateService} gameState
   * @param {import('./fleet-layout-service.js').FleetLayoutService} fleetLayout
   */
  constructor($element, gameState, fleetLayout) {
    this.$element = $element;
    this.gameState = gameState;
    this.fleetLayout = fleetLayout;
    /** @type {import('./fleet.js').FleetShip | undefined} */
    this.ship = undefined;
    /** @type {HTMLElement | undefined} */
    this.placeHolder = undefined;
    /** @type {HTMLElement | undefined} */
    this.shipElement = undefined;
    /** @type {HTMLElement[]} */
    this.elementsBelow = [];
    /** @type {ShipOrientation} */
    this.orientation = 'HORIZONTAL';
    this.dragging = false;
    /** @type {number | undefined} */
    this.activePointerId = undefined;
    this.firstMove = false;
    this.shiftX = 0;
    this.shiftY = 0;
    /** @type {(() => void) | undefined} */
    this.unregisterFleetShip = undefined;
    /** @type {(() => void) | undefined} */
    this.resizeHandler = undefined;
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
    this.unregisterFleetShip = this.fleetLayout.registerFleetShip(this);

    this.setOnPlaceholder();
    this.resizeHandler = () => this.scheduleRealign();
    window.addEventListener('resize', this.resizeHandler);
    this.scheduleRealign();
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
    alignElementTo(shipElement, placeHolder);
  }

  realignToLayout() {
    this.setRotation();
    const shipElement = this.getShipElement();

    if (this.elementsBelow.length > 0) {
      const firstTile = this.elementsBelow[0];
      alignElementTo(shipElement, firstTile);
      return;
    }

    const placeHolder = this.getPlaceHolder();
    alignElementTo(shipElement, placeHolder);
  }

  scheduleRealign() {
    const realign = () => this.realignToLayout();

    requestAnimationFrame(() => {
      realign();
      requestAnimationFrame(realign);
    });

    document.fonts?.ready.then(realign).catch(() => {});
  }

  /**
   * @param {{ row: number, column: number }[]} coordinates
   */
  placeOnBoardCoordinates(coordinates) {
    if (!this.ship || coordinates.length === 0) {
      this.getShipElement().hidden = true;
      return;
    }

    const sorted = sortedCoordinates(coordinates);
    this.orientation = orientationFromCoordinates(sorted);
    this.setRotation();
    this.elementsBelow = sorted
      .map((coordinate) => fleetBoardTileAt(coordinate))
      .filter((tile) => tile instanceof HTMLElement);

    const firstTile = this.elementsBelow[0];
    const shipElement = this.getShipElement();
    if (!firstTile) {
      shipElement.hidden = true;
      return;
    }

    shipElement.hidden = false;
    alignElementTo(shipElement, firstTile);
  }

  onDoubleClick() {
    if (
      !this.ship ||
      this.gameState.fleetLocked ||
      this.ship.size === 1 ||
      this.elementsBelow.length === 0
    ) {
      return;
    }

    const tile = tileDataset(this.elementsBelow[0]);
    if (!tile) {
      return;
    }

    this.elementsBelow.forEach((element) =>
      element.classList.remove('droppable-target')
    );
    this.clearMapBlocks();

    if (this.isLegal(tile.row, tile.column, this.getOppositeOrientation())) {
      this.orientation = this.getOppositeOrientation();
      this.setRotation();
    }

    this.elementsBelow = this.getElementsBelow(
      tile.row,
      tile.column,
      this.orientation
    );
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
    if (!this.ship || this.gameState.fleetLocked) {
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
    setElementPosition(shipElement, { left: x, top: y });
    shipElement.hidden = true;

    const elementBelow = fleetBoardTileFromPoint(x, y);
    this.resetElementsBelow();

    if (elementBelow) {
      const tile = tileDataset(elementBelow);
      if (tile && this.isLegal(tile.row, tile.column, this.orientation)) {
        this.elementsBelow = this.getElementsBelow(
          tile.row,
          tile.column,
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
      alignElementTo(shipElement, firstTile);
      this.claimTiles();
    } else {
      this.setOnPlaceholder();
    }
  }

  setRotation() {
    const { width, height } = shipElementSize(this.ship, this.orientation);
    const shipElement = this.getShipElement();
    shipElement.style.width = width;
    shipElement.style.height = height;
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

    return fleetBoardTilesForShip(row, column, orientation, this.ship.size);
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

    return this.gameState.canPlaceSetupShip(
      this.ship.id,
      shipCoordinatesFromStart(row, column, orientation, this.ship.size)
    );
  }

  clearMapBlocks() {
    this.elementsBelow.forEach((tile) => {
      tile.classList.remove('droppable-target');
    });
    if (this.ship) {
      this.gameState.clearSetupShip(this.ship.id);
    }
    this.elementsBelow = [];
  }

  resetElementsBelow() {
    this.elementsBelow.forEach((element) =>
      element.classList.remove('droppable-target')
    );
    this.elementsBelow = [];
  }

  getShipCoordinates() {
    return elementPagePosition(this.getShipElement());
  }

  claimTiles() {
    this.elementsBelow.forEach((tile) => {
      tile.classList.remove('droppable-target');
    });
    if (this.ship) {
      this.gameState.placeSetupShip(
        this.ship.id,
        coordinatesFromElements(this.elementsBelow)
      );
    }
  }

  $onDestroy() {
    this.dragging = false;
    this.activePointerId = undefined;
    this.unregisterFleetShip?.();
    if (this.resizeHandler) {
      window.removeEventListener('resize', this.resizeHandler);
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
      ng-class="{ locked: $ctrl.gameState.fleetLocked }"
      ng-attr-aria-disabled="{{$ctrl.gameState.fleetLocked ? 'true' : 'false'}}"
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
