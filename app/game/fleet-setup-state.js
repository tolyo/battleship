import {
  allFleetShipsPlaced,
  boardStateFromPlacements,
  canPlaceSetupShip as canPlaceShip,
  randomFleetPlacements,
} from './fleet-placement.js';
import { setupRowsFromDataState } from './board-rows.js';

/**
 * @typedef {{
 *   phase: 'setup' | 'waiting' | 'playing',
 *   boardReady: boolean,
 *   boardState: string[][],
 *   shipPlacements: Record<string, import('./game-view-model.js').Coordinate[]>,
 *   status: string,
 *   fleetRows: import('./board-rows.js').BoardGridTile[][]
 * }} FleetSetupState
 */

/**
 * @param {FleetSetupState} state
 */
export function resetSetupFleet(state) {
  if (state.phase !== 'setup') {
    return;
  }

  state.shipPlacements = {};
  rebuildSetupBoard(state);
}

/**
 * @param {FleetSetupState} state
 */
export function randomizeFleet(state) {
  if (state.phase !== 'setup') {
    return;
  }

  state.shipPlacements = randomFleetPlacements();
  rebuildSetupBoard(state);
}

/**
 * @param {FleetSetupState} state
 * @param {string} shipId
 * @param {import('./game-view-model.js').Coordinate[]} coordinates
 */
export function placeSetupShip(state, shipId, coordinates) {
  if (state.phase !== 'setup') {
    return;
  }

  state.shipPlacements = {
    ...state.shipPlacements,
    [shipId]: coordinates,
  };
  rebuildSetupBoard(state);
}

/**
 * @param {FleetSetupState} state
 * @param {string} shipId
 */
export function clearSetupShip(state, shipId) {
  if (state.phase !== 'setup') {
    return;
  }

  const { [shipId]: _removed, ...nextPlacements } = state.shipPlacements;
  state.shipPlacements = nextPlacements;
  rebuildSetupBoard(state);
}

/**
 * @param {FleetSetupState} state
 * @param {string} shipId
 * @param {import('./game-view-model.js').Coordinate[]} coordinates
 * @returns {boolean}
 */
export function canPlaceSetupShip(state, shipId, coordinates) {
  if (state.phase !== 'setup') {
    return false;
  }

  return canPlaceShip(state.shipPlacements, shipId, coordinates);
}

/**
 * @param {FleetSetupState} state
 */
export function rebuildSetupBoard(state) {
  const { boardState, tileDataState } = boardStateFromPlacements(
    state.shipPlacements
  );

  state.boardState = boardState;
  state.fleetRows = setupRowsFromDataState(tileDataState);
  if (allFleetShipsPlaced(state.shipPlacements)) {
    state.boardReady = true;
    state.status = 'Ready to join';
  } else {
    state.boardReady = false;
    state.status = 'Place your fleet';
  }
}
