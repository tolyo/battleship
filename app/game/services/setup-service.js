import {
  canPlaceSetupUnit,
  clearSetupPreview,
  clearSetupUnit,
  placeSetupUnit,
  placeSetupUnitAt,
  previewSetupUnitAt,
  randomizeUnits,
  resetSetupUnits,
} from '../domain/setup-state.js';

export class SetupService {
  static $inject = ['matchState'];

  /**
   * @param {import('./match-state-service.js').MatchStateService} matchState
   */
  constructor(matchState) {
    this.setupState = setupStateAdapter(matchState);
  }

  randomize() {
    randomizeUnits(this.setupState);
  }

  reset() {
    resetSetupUnits(this.setupState);
  }

  /**
   * @param {string} unitId
   */
  clearUnit(unitId) {
    clearSetupUnit(this.setupState, unitId);
  }

  clearPreview() {
    clearSetupPreview(this.setupState);
  }

  /**
   * @param {string} unitId
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   */
  placeUnit(unitId, coordinates) {
    placeSetupUnit(this.setupState, unitId, coordinates);
  }

  /**
   * @param {string} unitId
   * @param {string} row
   * @param {string} column
   * @param {'VERTICAL' | 'HORIZONTAL'} orientation
   * @param {number} size
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  placeUnitAt(unitId, row, column, orientation, size) {
    return placeSetupUnitAt(
      this.setupState,
      unitId,
      row,
      column,
      orientation,
      size
    );
  }

  /**
   * @param {string} unitId
   * @param {string} row
   * @param {string} column
   * @param {'VERTICAL' | 'HORIZONTAL'} orientation
   * @param {number} size
   * @returns {import('../domain/match-view-model.js').Coordinate[] | undefined}
   */
  previewUnitAt(unitId, row, column, orientation, size) {
    return previewSetupUnitAt(
      this.setupState,
      unitId,
      row,
      column,
      orientation,
      size
    );
  }

  /**
   * @param {string} unitId
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   * @returns {boolean}
   */
  canPlaceUnit(unitId, coordinates) {
    return canPlaceSetupUnit(this.setupState, unitId, coordinates);
  }
}

/**
 * @param {import('./match-state-service.js').MatchStateService} state
 * @returns {import('../domain/setup-state.js').SetupState}
 */
function setupStateAdapter(state) {
  return {
    get isSetup() {
      return state.isSetup;
    },
    get data() {
      return state.data;
    },
    get boardState() {
      return state.boardState;
    },
    set boardState(boardState) {
      state.boardState = boardState;
    },
  };
}
