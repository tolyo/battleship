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
  static $inject = ['matchState', '$rootScope'];

  /**
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {ng.Scope=} $rootScope
   */
  constructor(matchState, $rootScope) {
    this.matchState = matchState;
    this.$rootScope = $rootScope;
    this.setupState = setupStateAdapter(matchState);
  }

  randomize() {
    randomizeUnits(this.setupState);
    this.refresh();
  }

  reset() {
    resetSetupUnits(this.setupState);
    this.refresh();
  }

  /**
   * @param {string} unitId
   */
  clearUnit(unitId) {
    clearSetupUnit(this.setupState, unitId);
    this.refresh();
  }

  clearPreview() {
    clearSetupPreview(this.setupState);
    this.refresh();
  }

  /**
   * @param {string} unitId
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   */
  placeUnit(unitId, coordinates) {
    placeSetupUnit(this.setupState, unitId, coordinates);
    this.refresh();
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
    const coordinates = placeSetupUnitAt(
      this.setupState,
      unitId,
      row,
      column,
      orientation,
      size
    );
    this.refresh();
    return coordinates;
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
    const coordinates = previewSetupUnitAt(
      this.setupState,
      unitId,
      row,
      column,
      orientation,
      size
    );
    this.refresh();
    return coordinates;
  }

  /**
   * @param {string} unitId
   * @param {import('../domain/match-view-model.js').Coordinate[]} coordinates
   * @returns {boolean}
   */
  canPlaceUnit(unitId, coordinates) {
    return canPlaceSetupUnit(this.setupState, unitId, coordinates);
  }

  refresh() {
    const handler = /** @type {ScopeRefreshHandler | undefined} */ (
      this.$rootScope?.$handler
    );
    handler?._checkListenersForAllKeys?.(this.matchState.data);
    handler?._flushScheduledTasks?.();
  }
}

/**
 * @typedef {{
 *   _checkListenersForAllKeys?: (value: unknown) => void,
 *   _flushScheduledTasks?: () => void
 * }} ScopeRefreshHandler
 */

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
