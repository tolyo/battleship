import {
  allUnitsPlaced,
  boardStateFromPlacements,
  canPlaceSetupUnit as canPlaceUnit,
  randomUnitPlacements,
  unitCoordinatesFromStart,
} from './unit-placement.js';
import { SessionStatus } from './constants.js';
import { setupRowsFromDataState } from './board-rows.js';

/**
 * @typedef {{
 *   isSetup: boolean,
 *   data: import('./room-state.js').RoomSessionData,
 *   boardState: string[][],
 * }} SetupState
 */

/**
 * @param {SetupState} state
 */
export function resetSetupUnits(state) {
  if (!state.isSetup) {
    return;
  }

  state.data.setupPreviewCoordinates = [];
  state.data.unitPlacements = {};
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 */
export function randomizeUnits(state) {
  if (!state.isSetup) {
    return;
  }

  state.data.setupPreviewCoordinates = [];
  state.data.unitPlacements = randomUnitPlacements();
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function placeSetupUnit(state, unitId, coordinates) {
  if (!state.isSetup) {
    return;
  }

  state.data.setupPreviewCoordinates = [];
  state.data.unitPlacements = {
    ...state.data.unitPlacements,
    [unitId]: coordinates,
  };
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[] | undefined}
 */
export function placeSetupUnitAt(
  state,
  unitId,
  row,
  column,
  orientation,
  size
) {
  const coordinates = legalSetupUnitCoordinates(
    state,
    unitId,
    row,
    column,
    orientation,
    size
  );
  if (!coordinates) {
    return undefined;
  }

  placeSetupUnit(state, unitId, coordinates);
  return coordinates;
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 */
export function clearSetupUnit(state, unitId) {
  if (!state.isSetup) {
    return;
  }

  state.data.setupPreviewCoordinates = [];
  const { [unitId]: _removed, ...nextPlacements } = state.data.unitPlacements;
  state.data.unitPlacements = nextPlacements;
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function previewSetupUnit(state, coordinates) {
  if (!state.isSetup) {
    return;
  }

  if (sameCoordinates(state.data.setupPreviewCoordinates, coordinates)) {
    return;
  }

  state.data.setupPreviewCoordinates = coordinates;
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[] | undefined}
 */
export function previewSetupUnitAt(
  state,
  unitId,
  row,
  column,
  orientation,
  size
) {
  const coordinates = legalSetupUnitCoordinates(
    state,
    unitId,
    row,
    column,
    orientation,
    size
  );
  if (!coordinates) {
    clearSetupPreview(state);
    return undefined;
  }

  previewSetupUnit(state, coordinates);
  return coordinates;
}

/**
 * @param {SetupState} state
 */
export function clearSetupPreview(state) {
  if (!state.isSetup) {
    return;
  }

  if (state.data.setupPreviewCoordinates.length === 0) {
    return;
  }

  state.data.setupPreviewCoordinates = [];
  rebuildSetupBoard(state);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 * @returns {boolean}
 */
export function canPlaceSetupUnit(state, unitId, coordinates) {
  if (!state.isSetup) {
    return false;
  }

  return canPlaceUnit(state.data.unitPlacements, unitId, coordinates);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[] | undefined}
 */
function legalSetupUnitCoordinates(
  state,
  unitId,
  row,
  column,
  orientation,
  size
) {
  const coordinates = unitCoordinatesFromStart(row, column, orientation, size);
  return canPlaceSetupUnit(state, unitId, coordinates)
    ? coordinates
    : undefined;
}

/**
 * @param {import('./match-view-model.js').Coordinate[]} left
 * @param {import('./match-view-model.js').Coordinate[]} right
 * @returns {boolean}
 */
function sameCoordinates(left, right) {
  return (
    left.length === right.length &&
    left.every(
      (coordinate, index) =>
        coordinate.row === right[index].row &&
        coordinate.column === right[index].column
    )
  );
}

/**
 * @param {SetupState} state
 */
export function rebuildSetupBoard(state) {
  const { boardState, tileDataState } = boardStateFromPlacements(
    state.data.unitPlacements
  );

  state.boardState = boardState;
  state.data.ownRows = setupRowsFromDataState(
    tileDataState,
    state.data.setupPreviewCoordinates
  );
  if (allUnitsPlaced(state.data.unitPlacements)) {
    state.data.boardReady = true;
    state.data.status = SessionStatus.FLEET_READY;
  } else {
    state.data.boardReady = false;
    state.data.status = SessionStatus.PLACE_FLEET;
  }
}
