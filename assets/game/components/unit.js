import { unit as component } from '../../../_build/browser/js/modules/battleship/browser/Components.js';
import {
  elementPagePosition,
  boardTileFromPoint,
  tileDataset,
  boardTileAt,
  sortedCoordinates,
} from './unit-geometry.js';
import {
  initial as initialDrag,
  started as startedDrag,
  owns as ownsPointer,
  position as dragPosition,
  moved as movedDrag,
  stopped as stoppedDrag,
} from '../../../_build/browser/js/modules/battleship/browser/UnitDrag.js';
import {
  initial as initialLayout,
  placeholder as placeholderLayout,
  cleared as clearedLayout,
  hidden as hiddenLayout,
  synchronized as synchronizedLayout,
  previewed as previewedLayout,
  opposite_orientation as oppositeLayoutOrientation,
  has_placement as hasPlacement,
} from '../../../_build/browser/js/modules/battleship/browser/UnitLayout.js';
import { UnitSetupPlacement } from './unit-setup-placement.js';
import {
  initial as initialStyle,
  resized,
  positioned_css as positionedStyle,
} from '../../../_build/browser/js/modules/battleship/browser/UnitStyle.js';

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
    /** @type {import('../../../_build/browser/js/modules/battleship/browser/Fleet.js').Unit | undefined} */
    this.unit = undefined;
    /** @type {import('../domain/match-view-model.js').Coordinate[] | undefined} */
    this.coordinates = undefined;
    /** @type {HTMLElement | undefined} */
    this.placeHolder = undefined;
    /** @type {HTMLElement | undefined} */
    this.unitElement = undefined;
    this.drag = initialDrag();
    this.layout = initialLayout();
    /** @type {HTMLElement | undefined} */
    this.anchorTile = undefined;
    this.setup = new UnitSetupPlacement(setup);
    this.style = initialStyle();
    /** @type {PointerEvent | undefined} */
    this.pendingPointer = undefined;
    /** @type {number | undefined} */
    this.pointerFrame = undefined;
    /** @type {string | undefined} */
    this.previewKey = undefined;
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
    this.layout = placeholderLayout(this.layout);
    this.anchorTile = undefined;
    this.setRotation();
    const placeHolder = this.getPlaceHolder();
    this.alignToElement(placeHolder);
  }

  realignToLayout() {
    if (this.drag.active) {
      return;
    }

    this.setRotation();
    if (this.anchorTile) {
      this.alignToElement(this.anchorTile);
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
    if (this.drag.active) {
      return;
    }
    if (!this.unit || !this.coordinates || this.coordinates.length === 0) {
      if (this.setupActive) {
        this.alignToPlaceholder();
      } else {
        this.layout = hiddenLayout(this.layout);
      }
      return;
    }

    this.syncLayoutCoordinates(this.coordinates);
    this.setRotation();

    if (!this.anchorTile) {
      return;
    }

    this.alignToElement(this.anchorTile);
  }

  /** @param {import('../domain/match-view-model.js').Coordinate[]} coordinates */
  syncLayoutCoordinates(coordinates) {
    const sorted = sortedCoordinates(coordinates);
    const anchor = sorted.length > 0 ? boardTileAt(sorted[0]) : undefined;
    this.anchorTile = anchor;
    this.layout = synchronizedLayout(this.layout, sorted, !!anchor);
  }

  /** @param {import('../domain/match-view-model.js').Coordinate[]} coordinates */
  setLayoutPreview(coordinates) {
    const anchor =
      coordinates.length > 0 ? boardTileAt(coordinates[0]) : undefined;
    this.anchorTile = anchor;
    this.layout = previewedLayout(this.layout, coordinates);
  }

  clearLayoutPreview() {
    this.anchorTile = undefined;
    this.layout = clearedLayout(this.layout);
  }

  onDoubleClick() {
    if (!this.unit || this.locked || this.unit.size === 1 || !this.anchorTile) {
      return;
    }

    const tile = tileDataset(this.anchorTile);
    if (!tile) {
      return;
    }

    const oppositeOrientation = /** @type {Orientation} */ (
      oppositeLayoutOrientation(this.layout)
    );
    const coordinates = this.placeAt(
      tile.row,
      tile.column,
      oppositeOrientation
    );
    if (coordinates) {
      this.layout.orientation = oppositeOrientation;
      this.setRotation();
      this.setLayoutPreview(coordinates);
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
    if (!ownsPointer(this.drag, event)) {
      return;
    }

    this.pendingPointer = event;
    if (this.pointerFrame === undefined) {
      this.pointerFrame = requestAnimationFrame(() => this.flushPointerMove());
    }
  }

  flushPointerMove() {
    this.cancelPointerFrame();
    const event = this.pendingPointer;
    this.pendingPointer = undefined;
    if (event && ownsPointer(this.drag, event)) {
      this.moveUnit(event);
    }
  }

  cancelPointerFrame() {
    if (this.pointerFrame !== undefined) {
      cancelAnimationFrame(this.pointerFrame);
      this.pointerFrame = undefined;
    }
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerUp(event) {
    if (!ownsPointer(this.drag, event)) {
      return;
    }

    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    this.pendingPointer = event;
    this.flushPointerMove();
    target?.releasePointerCapture(event.pointerId);
    this.finishDrag();
  }

  /**
   * @param {PointerEvent} event
   */
  onPointerCancel(event) {
    if (!ownsPointer(this.drag, event)) {
      return;
    }

    const target = /** @type {HTMLElement | null} */ (event.currentTarget);
    this.flushPointerMove();
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
    this.drag = startedDrag(event, unitCoordinates);
    this.getUnitElement().classList.add('dragged');
    this.previewKey = undefined;
    return true;
  }

  /**
   * @param {PointerEvent} event
   */
  moveUnit(event) {
    if (this.drag.firstMove) {
      this.drag = movedDrag(this.drag);
      this.clearMapBlocks();
    }

    const position = dragPosition(this.drag, event);
    this.setPosition(position);

    const elementBelow = boardTileFromPoint(position.left, position.top);
    const tile = elementBelow ? tileDataset(elementBelow) : undefined;
    const previewKey = tile
      ? `${tile.row}:${tile.column}:${this.layout.orientation}`
      : '';
    if (this.previewKey === previewKey) {
      return;
    }
    this.previewKey = previewKey;
    const coordinates = tile
      ? this.previewAt(
          tile.row,
          tile.column,
          /** @type {Orientation} */ (this.layout.orientation)
        )
      : undefined;

    if (!tile || !coordinates) {
      this.resetPreviewState();
      return;
    }

    this.setLayoutPreview(coordinates);
  }

  finishDrag() {
    this.getUnitElement().classList.remove('dragged');
    this.drag = stoppedDrag(this.drag);

    if (hasPlacement(this.layout, !!this.anchorTile) && this.anchorTile) {
      this.alignToElement(this.anchorTile);
      this.claimTiles();
    } else {
      this.setOnPlaceholder();
    }
  }

  setRotation() {
    this.style = resized(
      this.style,
      this.unit?.size ?? 1,
      this.layout.orientation
    );
    if (this.unitElement) {
      if (this.unitElement.style.width !== this.style.value.width) {
        this.unitElement.style.width = this.style.value.width;
      }
      if (this.unitElement.style.height !== this.style.value.height) {
        this.unitElement.style.height = this.style.value.height;
      }
    }
  }

  clearMapBlocks() {
    this.setup.clearUnit(this.unit);
    this.setup.clearPreview();
    this.clearLayoutPreview();
  }

  resetPreviewState() {
    this.setup.clearPreview();
    this.clearLayoutPreview();
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
    const element = this.getUnitElement();
    const left = `${position.left}px`;
    const top = `${position.top}px`;
    if (element.style.left !== left) element.style.left = left;
    if (element.style.top !== top) element.style.top = top;
    if (!this.drag.active) {
      this.style = positionedStyle(this.style, left, top);
    }
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
    this.cancelPointerFrame();
    this.pendingPointer = undefined;
    this.drag = stoppedDrag(this.drag);
  }

  get ariaDisabled() {
    return this.locked ? 'true' : 'false';
  }

  get cssClasses() {
    return {
      locked: this.locked,
      dragged: this.drag.active,
    };
  }
}

export default component(UnitController);
