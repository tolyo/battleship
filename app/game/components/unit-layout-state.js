import {
  boardTileAt,
  orientationFromCoordinates,
  sortedCoordinates,
} from './unit-geometry.js';

export class UnitLayoutState {
  constructor() {
    /** @type {'VERTICAL' | 'HORIZONTAL'} */
    this.orientation = 'HORIZONTAL';
    /** @type {HTMLElement | undefined} */
    this.anchorTile = undefined;
    this.visible = true;
    /** @type {import('../domain/match-view-model.js').Coordinate[]} */
    this.previewCoordinates = [];
  }

  resetToPlaceholder() {
    this.orientation = 'HORIZONTAL';
    this.resetPreview();
    this.visible = true;
  }

  resetPreview() {
    this.anchorTile = undefined;
    this.previewCoordinates = [];
  }

  hide() {
    this.visible = false;
  }

  /**
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   */
  syncCoordinates(coordinates) {
    const sorted = sortedCoordinates(coordinates);

    this.orientation = orientationFromCoordinates(sorted);
    this.setPreview(sorted);
    this.visible = !!this.anchorTile;
  }

  /**
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   */
  setPreview(coordinates) {
    this.previewCoordinates = coordinates;
    this.anchorTile = coordinates.length > 0 ? boardTileAt(coordinates[0]) : undefined;
  }

  /**
   * @returns {'VERTICAL' | 'HORIZONTAL'}
   */
  oppositeOrientation() {
    return this.orientation === 'HORIZONTAL' ? 'VERTICAL' : 'HORIZONTAL';
  }

  /**
   * @returns {boolean}
   */
  hasPlacement() {
    return !!this.anchorTile && this.previewCoordinates.length > 0;
  }
}
