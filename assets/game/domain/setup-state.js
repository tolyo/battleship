import { project as projectUnitViews } from '../../../_build/browser/js/modules/battleship/browser/BoardUnits.js';
import {
  update,
  project,
  can_place as canPlace,
} from '../../../_build/browser/js/modules/battleship/browser/SetupState.js';
import {
  checkedPlacements,
  checkedCoordinates,
  unitCoordinatesFromStart,
} from './unit-placement.js';
import { rowsFromModel } from './board-rows.js';

/**
 * @typedef {{
 *   isSetup: boolean,
 *   data: import('./room-state.js').RoomSessionData,
 *   boardState: string[][],
 * }} SetupState
 */

/** @param {SetupState} state */
function snapshot(state) {
  return {
    placements: checkedPlacements(state.data.unitPlacements),
    preview: checkedCoordinates(state.data.setupPreviewCoordinates),
  };
}

/**
 * @param {SetupState} state
 * @param {import('../../../_build/browser/js/modules/battleship/browser/SetupState.js').Update} result
 */
function applyUpdate(state, result) {
  if (result.error) {
    throw new Error(result.error);
  }
  if (result.changed) {
    if (result.placementsChanged) {
      state.data.unitViews = projectUnitViews(result.placements);
      state.data.unitPlacements = Object.fromEntries(
        result.placements.map(({ id, coordinates }) => [id, coordinates])
      );
    }
    if (result.previewChanged) {
      state.data.setupPreviewCoordinates = result.preview;
    }
    state.boardState = result.boardState;
    state.data.ownRows = rowsFromModel(result.ownRows);
    state.data.boardReady = result.boardReady;
    state.data.status = result.status;
  }
  return result;
}

/**
 * @param {SetupState} state
 * @param {string} event
 * @param {string=} id
 * @param {import('./match-view-model.js').Coordinate[]=} coordinates
 * @param {number=} seed
 */
function change(state, event, id = '', coordinates = [], seed = 1) {
  return applyUpdate(
    state,
    update(
      state.isSetup,
      snapshot(state),
      event,
      id,
      checkedCoordinates(coordinates),
      seed
    )
  );
}

/** @param {SetupState} state */
export function resetSetupUnits(state) {
  change(state, 'reset');
}

/**
 * @param {SetupState} state
 * @param {number=} seed
 */
export function randomizeUnits(state, seed) {
  if (!state.isSetup) {
    return;
  }
  change(
    state,
    'randomize',
    '',
    [],
    seed ?? Math.floor(Math.random() * 2147483646) + 1
  );
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function placeSetupUnit(state, unitId, coordinates) {
  change(state, 'place', unitId, coordinates);
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 */
export function clearSetupUnit(state, unitId) {
  change(state, 'clear', unitId);
}

/**
 * @param {SetupState} state
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function previewSetupUnit(state, coordinates) {
  change(state, 'preview', '', coordinates);
}

/** @param {SetupState} state */
export function clearSetupPreview(state) {
  change(state, 'clear_preview');
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function canPlaceSetupUnit(state, unitId, coordinates) {
  return canPlace(
    state.isSetup,
    snapshot(state),
    unitId,
    checkedCoordinates(coordinates)
  );
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 */
export function placeSetupUnitAt(
  state,
  unitId,
  row,
  column,
  orientation,
  size
) {
  const result = change(
    state,
    'place_at',
    unitId,
    unitCoordinatesFromStart(row, column, orientation, size)
  );
  return result.accepted ? result.coordinates : undefined;
}

/**
 * @param {SetupState} state
 * @param {string} unitId
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 */
export function previewSetupUnitAt(
  state,
  unitId,
  row,
  column,
  orientation,
  size
) {
  const result = change(
    state,
    'preview_at',
    unitId,
    unitCoordinatesFromStart(row, column, orientation, size)
  );
  return result.accepted ? result.coordinates : undefined;
}

/** @param {SetupState} state */
export function rebuildSetupBoard(state) {
  applyUpdate(state, project(snapshot(state)));
}
