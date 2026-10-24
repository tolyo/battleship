import { project as projectUnitViews } from '../../../_build/browser/js/modules/battleship/browser/BoardUnits.js';
import { checkedPlacements } from './unit-placement.js';
import {
  advance,
  initial,
  rules,
  can_target as canTarget,
} from '../../../_build/browser/js/modules/battleship/browser/RoomState.js';
import {
  phases_names as phaseNames,
  events_names as eventNames,
  restore_timeout_ms as restoreTimeoutMs,
} from '../../../_build/browser/js/modules/battleship/browser/SessionConfig.js';
import { SessionStatus } from './constants.js';
import {
  emptyTileRows,
  rowsFromTiles,
  rowsWithSunkState,
} from './board-rows.js';

export const ROOM_UNAVAILABLE_STATUS = SessionStatus.ROOM_UNAVAILABLE;
export const DEFAULT_RESTORE_TIMEOUT_MS = restoreTimeoutMs();
export const SessionPhase = Object.freeze(
  Object.fromEntries(phaseNames().map(({ name, value }) => [name, value]))
);
export const RoomStateEvent = Object.freeze(
  Object.fromEntries(eventNames().map(({ name, value }) => [name, value]))
);

/**
 * @typedef {typeof SessionPhase[keyof typeof SessionPhase]} SessionPhaseValue
 */

/**
 * @typedef {import('../../../_build/browser/js/modules/battleship/browser/RoomState.js').Session & {
 *   roomId: string | undefined,
 *   playerId: string | undefined,
 *   opponentId: string | undefined,
 *   pendingView: unknown,
 *   setupPreviewCoordinates: import('./match-view-model.js').Coordinate[],
 *   unitViews: import('../../../_build/browser/js/modules/battleship/browser/BoardUnits.js').UnitView[],
 *   unitPlacements: Record<string, import('./match-view-model.js').Coordinate[]>,
 *   ownRows: import('./board-rows.js').BoardGridRow[],
 *   targetRows: import('./board-rows.js').BoardGridRow[]
 * }} RoomSessionData
 */

/**
 * @typedef {{
 *   roomId: string,
 *   playerId: string,
 *   opponentId: string | undefined,
 *   view?: unknown
 * }} RoomEnteredPayload
 */

/**
 * @typedef {{
 *   roomId: string,
 *   playerId: string
 * }} RestoreStartedPayload
 */

/**
 * @typedef {{
 *   viewModel: import('./match-view-model.js').MatchViewModel
 * }} ViewSnapshotPayload
 */

/** @returns {RoomSessionData} */
export function initialRoomSessionData() {
  return {
    ...initial(),
    roomId: undefined,
    playerId: undefined,
    opponentId: undefined,
    pendingView: undefined,
    setupPreviewCoordinates: [],
    unitPlacements: {},
    unitViews: projectUnitViews([]),
    ownRows: emptyTileRows('own-board'),
    targetRows: emptyTileRows('target-board'),
  };
}

/** @type {Record<string, (payload: unknown) => { text: string, myTurn: boolean }>} */
const inputs = {
  none: () => ({ text: '', myTurn: false }),
  text: (payload) => ({ text: String(payload), myTurn: false }),
  view(payload) {
    const { viewModel } = /** @type {ViewSnapshotPayload} */ (payload);
    return { text: viewModel.status, myTurn: viewModel.isMyTurn };
  },
};

/** @type {Record<string, (data: RoomSessionData, payload: unknown) => void>} */
const effects = {
  enter(data, payload) {
    const entry = /** @type {RoomEnteredPayload} */ (payload);
    data.roomId = entry.roomId;
    data.playerId = entry.playerId;
    data.opponentId = entry.opponentId;
  },
  restore(data, payload) {
    const entry = /** @type {RestoreStartedPayload} */ (payload);
    data.roomId = entry.roomId;
    data.playerId = entry.playerId;
  },
  reset(data) {
    data.roomId = undefined;
    data.playerId = undefined;
    data.opponentId = undefined;
    data.pendingView = undefined;
    data.setupPreviewCoordinates = [];
  },
  view(data, payload) {
    const { viewModel } = /** @type {ViewSnapshotPayload} */ (payload);
    data.ownRows = rowsWithSunkState(
      rowsFromTiles('own-board', viewModel.ownTiles),
      viewModel.sunkClusters
    );
    data.targetRows = rowsFromTiles('target-board', viewModel.targetTiles);
    data.unitPlacements = viewModel.unitCoordinatesById;
    data.unitViews = projectUnitViews(
      checkedPlacements(viewModel.unitCoordinatesById)
    );
  },
};

/**
 * @typedef {{ data: RoomSessionData, events: Record<string, unknown>, state: string }} RoomSessionContract
 * @returns {ng.MachineConfig<RoomSessionContract>}
 */
export function roomSessionConfig() {
  /** @type {Record<string, { on: NonNullable<ng.MachineConfig<RoomSessionContract>['states'][string]['on']> }>} */
  const states = {};
  rules().forEach((rule) => {
    const state = states[rule.phase] ?? { on: {} };
    states[rule.phase] = state;
    const on = state.on;
    on[rule.event] = {
      to: rule.nextPhase || rule.phase,
      update({ data, payload }) {
        const input = inputs[rule.payload](payload);
        const result = advance(
          data,
          rule.phase,
          rule.event,
          input.text,
          input.myTurn
        );
        Object.assign(data, result.session);
        effects[result.effect]?.(data, payload);
      },
    };
  });
  return {
    initial: SessionPhase.SETUP,
    data: initialRoomSessionData(),
    states,
  };
}

/**
 * @param {SessionPhaseValue} phase
 * @param {boolean} canSubmitMove
 * @param {import('./board-rows.js').BoardGridTile} tile
 * @returns {{ row: number, column: number } | undefined}
 */
export function moveForTarget(phase, canSubmitMove, tile) {
  return canTarget(phase, canSubmitMove, tile.state)
    ? { row: tile.row, column: tile.column }
    : undefined;
}
