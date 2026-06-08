/**
 * @typedef {{
 *   clearPreview(): void,
 *   clearUnit(unitId: string): void,
 *   placeUnit(
 *     unitId: string,
 *     coordinates: import('../domain/match-view-model.js').Coordinate[],
 *   ): void,
 *   placeUnitAt(
 *     unitId: string,
 *     row: string,
 *     column: string,
 *     orientation: 'VERTICAL' | 'HORIZONTAL',
 *     size: number,
 *   ): import('../domain/match-view-model.js').Coordinate[] | undefined,
 *   previewUnitAt(
 *     unitId: string,
 *     row: string,
 *     column: string,
 *     orientation: 'VERTICAL' | 'HORIZONTAL',
 *     size: number,
 *   ): import('../domain/match-view-model.js').Coordinate[] | undefined,
 * }} SetupPlacementService
 */

export class UnitSetupPlacement {
  /**
   * @param {SetupPlacementService} setup
   */
  constructor(setup) {
    this.setup = setup;
  }

  /**
   * @param {import('../domain/unit-catalog.js').Unit | undefined} unit
   */
  clearUnit(unit) {
    if (!unit) {
      return;
    }

    this.setup.clearUnit(unit.id);
  }

  clearPreview() {
    this.setup.clearPreview();
  }

  /**
   * @param {import('../domain/unit-catalog.js').Unit | undefined} unit
   * @param {string} row
   * @param {string} column
   * @param {'VERTICAL' | 'HORIZONTAL'} orientation
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  previewAt(unit, row, column, orientation) {
    if (!unit) {
      return undefined;
    }

    return this.setup.previewUnitAt(
      unit.id,
      row,
      column,
      orientation,
      unit.size
    );
  }

  /**
   * @param {import('../domain/unit-catalog.js').Unit | undefined} unit
   * @param {string} row
   * @param {string} column
   * @param {'VERTICAL' | 'HORIZONTAL'} orientation
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  placeAt(unit, row, column, orientation) {
    if (!unit) {
      return undefined;
    }

    return this.setup.placeUnitAt(
      unit.id,
      row,
      column,
      orientation,
      unit.size
    );
  }

  /**
   * @param {import('../domain/unit-catalog.js').Unit | undefined} unit
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   */
  commit(unit, coordinates) {
    this.clearPreview();
    if (!unit) {
      return;
    }

    this.setup.placeUnit(unit.id, coordinates);
  }
}
