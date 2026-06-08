import {
  elementPagePosition,
  boardTileFromPoint,
  tileDataset,
} from './unit-geometry.js';
import { UnitDragState } from './unit-drag-state.js';
import { UnitLayoutState } from './unit-layout-state.js';
import { UnitSetupPlacement } from './unit-setup-placement.js';
import { UnitStyleState } from './unit-style-state.js';

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} Orientation
 */

export class UnitController {
  static $inject = ['$element', 'setup'];

  /**
   * @param {Element} $element
   * @param {import('../services/setup-service.js').SetupService} setup
   */
  constructor($element, setup) {
    this.$element = $element;
    this.locked = false;
    this.setupActive = false;
    /** @type {import('../domain/unit-catalog.js').Unit | undefined} */
    this.unit = undefined;
    /** @type {import('../domain/match-view-model.js').Coordinate[] | undefined} */
    this.coordinates = undefined;
    /** @type {HTMLElement | undefined} */
    this.placeHolder = undefined;
    /** @type {HTMLElement | undefined} */
    this.unitElement = undefined;
    this.drag = new UnitDragState();
    this.layout = new UnitLayoutState();
    this.setup = new UnitSetupPlacement(setup);
    this.style = new UnitStyleState();
  }

  $postLink() {
    if (!this.unit) {
      return;
    }

    const placeHolder = this.$element.parentElement;
    if (!(placeHolder instanceof HTMLElement)) {
      return;
    }

    this.placeHolder = placeHolder;
    if (!this.unitElement) {
      return;
    }
    this.setRotation();

    this.syncCoordinates();
  }

  $onChanges() {
    if (this.unitElement) {
      this.syncCoordinates();
    }
  }

  /**
   * @returns {HTMLElement}
   */
  getUnitElement() {
    if (!this.unitElement) {
      throw new Error('Unit element has not been linked.');
    }

    return this.unitElement;
  }

  /**
   * @returns {HTMLElement}
   */
  getPlaceHolder() {
    if (!this.placeHolder) {
      throw new Error('Unit placeholder has not been linked.');
    }

    return this.placeHolder;
  }

  setOnPlaceholder() {
    this.clearMapBlocks();
    this.alignToPlaceholder();
  }

  alignToPlaceholder() {
    this.resetPreviewState();
    this.layout.resetToPlaceholder();
    this.setRotation();
    const placeHolder = this.getPlaceHolder();
    this.alignToElement(placeHolder);
  }

  realignToLayout() {
    if (this.drag.active) {
      return;
    }

    this.setRotation();
    if (this.layout.anchorTile) {
      this.alignToElement(this.layout.anchorTile);
      return;
    }

    const placeHolder = this.getPlaceHolder();
    this.alignToElement(placeHolder);
  }

  $afterRender() {
    if (this.unitElement && this.placeHolder) {
      this.realignToLayout();
    }
  }

  syncCoordinates() {
    if (!this.unit || !this.coordinates || this.coordinates.length === 0) {
      if (this.setupActive) {
        this.alignToPlaceholder();
      } else {
        this.layout.hide();
      }
      return;
    }

    this.layout.syncCoordinates(this.coordinates);
    this.setRotation();

    if (!this.layout.anchorTile) {
      return;
    }

    this.alignToElement(this.layout.anchorTile);
  }

  onDoubleClick() {
    if (
      !this.unit ||
      this.locked ||
      this.unit.size === 1 ||
      !this.layout.anchorTile
    ) {
      return;
    }

    const tile = tileDataset(this.layout.anchorTile);
    if (!tile) {
      return;
    }

    const oppositeOrientation = this.layout.oppositeOrientation();
    const coordinates = this.placeAt(tile.row, tile.column, oppositeOrientation);
    if (coordinates) {
      this.layout.orientation = oppositeOrientation;
      this.setRotation();
      this.layout.setPreview(coordinates);
    }
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerDown(event) {
    if (event.isPrimary === false || !this.startDrag(event)) {
      return;
    }

    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    target?.setPointerCapture(event.pointerId);
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerMove(event) {
    if (!this.drag.owns(event)) {
      return;
    }

    this.moveUnit(event);
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerUp(event) {
    if (!this.drag.owns(event)) {
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
    if (!this.drag.owns(event)) {
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
    if (!this.unit || this.locked) {
      return false;
    }
    if (event.button !== undefined && event.button !== 0) {
      return false;
    }

    const unitCoordinates = this.getUnitCoordinates();
    this.drag.start(event, unitCoordinates);
    return true;
  }

  /**
   * @param {PointerEvent} event
   */
  moveUnit(event) {
    if (this.drag.consumeFirstMove()) {
      this.clearMapBlocks();
    }

    const position = this.drag.position(event);
    this.setPosition(position);

    const elementBelow = boardTileFromPoint(position.left, position.top);
    const tile = elementBelow ? tileDataset(elementBelow) : undefined;
    const coordinates = tile
      ? this.previewAt(tile.row, tile.column, this.layout.orientation)
      : undefined;

    if (!tile || !coordinates) {
      this.resetPreviewState();
      return;
    }

    this.layout.setPreview(coordinates);
  }

  finishDrag() {
    this.drag.stop();

    if (this.layout.hasPlacement() && this.layout.anchorTile) {
      this.alignToElement(this.layout.anchorTile);
      this.claimTiles();
    } else {
      this.setOnPlaceholder();
    }
  }

  setRotation() {
    this.style.setSize(this.unit, this.layout.orientation);
  }

  clearMapBlocks() {
    this.setup.clearUnit(this.unit);
    this.setup.clearPreview();
    this.layout.resetPreview();
  }

  resetPreviewState() {
    this.setup.clearPreview();
    this.layout.resetPreview();
  }

  getUnitCoordinates() {
    return elementPagePosition(this.getUnitElement());
  }

  /**
   * @param {HTMLElement} element
   */
  alignToElement(element) {
    this.setPosition(elementPagePosition(element));
  }

  /**
   * @param {{ left: number, top: number }} position
   */
  setPosition(position) {
    this.style.setPosition(position);
  }

  claimTiles() {
    this.setup.commit(this.unit, this.layout.previewCoordinates);
  }

  /**
   * @param {string} row
   * @param {string} column
   * @param {Orientation} orientation
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  previewAt(row, column, orientation) {
    return this.setup.previewAt(this.unit, row, column, orientation);
  }

  /**
   * @param {string} row
   * @param {string} column
   * @param {Orientation} orientation
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  placeAt(row, column, orientation) {
    return this.setup.placeAt(this.unit, row, column, orientation);
  }

  $onDestroy() {
    this.drag.stop();
  }
}

export default {
  bindings: {
    unit: '<',
    coordinates: '<',
    locked: '<',
    setupActive: '<',
  },
  template: `
    <div
      class="unit"
      ng-ref="$ctrl.unitElement"
      ng-ref-read="$element"
      ng-class="{ locked: $ctrl.locked, dragged: $ctrl.drag.active }"
      ng-style="$ctrl.style.value"
      ng-show="$ctrl.layout.visible"
      ng-attr-aria-disabled="{{$ctrl.locked ? 'true' : 'false'}}"
      ng-attr-id="{{$ctrl.unit.id}}"
      ng-window-resize="$ctrl.realignToLayout()"
      ng-on-pointerdown="$ctrl.onPointerDown($event)"
      ng-on-pointermove="$ctrl.onPointerMove($event)"
      ng-on-pointerup="$ctrl.onPointerUp($event)"
      ng-on-pointercancel="$ctrl.onPointerCancel($event)"
      ng-on-dblclick="$ctrl.onDoubleClick()"
      data-event-prevent>
    </div>
  `,
  controller: UnitController,
};
