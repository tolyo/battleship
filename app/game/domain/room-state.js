import { emptyTileRows, rowsFromTiles, rowsWithSunkState } from './board-rows.js';

export const SessionPhase = Object.freeze({
  SETUP: 'setup',
  CONNECTING: 'connecting',
  WAITING: 'waiting',
  RESTORING: 'restoring',
  PLAYING: 'playing',
});

export const RoomStateEvent = Object.freeze({
  SETUP_INCOMPLETE: 'setup_incomplete',
  JOIN_REQUESTED: 'join_requested',
  SOCKET_OPENED: 'socket_opened',
  CONNECTION_CLOSED: 'connection_closed',
  RETURN_TO_SETUP: 'return_to_setup',
  ROOM_ENTERED: 'room_entered',
  RESTORE_STARTED: 'restore_started',
  STATE_RECEIVED: 'state_received',
  ROOM_UNAVAILABLE: 'room_unavailable',
  OPPONENT_DISCONNECTED: 'opponent_disconnected',
  CONNECTION_ERROR: 'connection_error',
  SERVER_ERROR: 'server_error',
});

/**
 * @typedef {'setup' | 'connecting' | 'waiting' | 'restoring' | 'playing'} SessionPhaseValue
 */

/**
 * @typedef {{
 *   status: string,
 *   boardReady: boolean,
 *   roomId: string | undefined,
 *   playerId: string | undefined,
 *   opponentId: string | undefined,
 *   isMyTurn: boolean,
 *   canSubmitMove: boolean,
 *   pendingView: unknown,
 *   setupPreviewCoordinates: import('./match-view-model.js').Coordinate[],
 *   unitPlacements: Record<string, import('./match-view-model.js').Coordinate[]>,
 *   ownRows: import('./board-rows.js').BoardGridRow[],
 *   targetRows: import('./board-rows.js').BoardGridRow[],
 *   unitsLocked: boolean
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

/**
 * @returns {RoomSessionData}
 */
export function initialRoomSessionData() {
  return {
    status: 'Place your fleet',
    boardReady: false,
    roomId: undefined,
    playerId: undefined,
    opponentId: undefined,
    isMyTurn: false,
    canSubmitMove: false,
    pendingView: undefined,
    setupPreviewCoordinates: [],
    unitPlacements: {},
    ownRows: emptyTileRows('own-board'),
    targetRows: emptyTileRows('target-board'),
    unitsLocked: false,
  };
}

/**
 * @returns {ng.MachineConfig<RoomSessionData, ng.MachineEventMap>}
 */
export function roomSessionConfig() {
  return {
    initial: SessionPhase.SETUP,
    data: initialRoomSessionData(),
    transitions: {
      [SessionPhase.SETUP]: {
        [RoomStateEvent.SETUP_INCOMPLETE](data) {
          data.status = 'Place your fleet first';
        },
        [RoomStateEvent.JOIN_REQUESTED](data) {
          data.unitsLocked = true;
          data.status = 'Connecting...';
          return SessionPhase.CONNECTING;
        },
        [RoomStateEvent.ROOM_ENTERED](data, payload) {
          enterRoom(data, /** @type {RoomEnteredPayload} */ (payload));
          return SessionPhase.PLAYING;
        },
        [RoomStateEvent.RESTORE_STARTED](data, payload) {
          startRestore(data, /** @type {RestoreStartedPayload} */ (payload));
          return SessionPhase.RESTORING;
        },
        [RoomStateEvent.ROOM_UNAVAILABLE](data) {
          data.status = 'Room unavailable';
        },
      },
      [SessionPhase.CONNECTING]: {
        [RoomStateEvent.SOCKET_OPENED](data) {
          data.status = 'Waiting for opponent...';
          return SessionPhase.WAITING;
        },
        [RoomStateEvent.CONNECTION_CLOSED](data) {
          data.status = 'Disconnected';
          data.unitsLocked = false;
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.ROOM_ENTERED](data, payload) {
          enterRoom(data, /** @type {RoomEnteredPayload} */ (payload));
          return SessionPhase.PLAYING;
        },
        [RoomStateEvent.CONNECTION_ERROR](data) {
          data.status = 'connection_error';
        },
        [RoomStateEvent.RETURN_TO_SETUP](data, status) {
          returnToSetup(data, String(status));
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.SERVER_ERROR](data, reason) {
          data.status = String(reason);
        },
      },
      [SessionPhase.WAITING]: {
        [RoomStateEvent.SOCKET_OPENED](data) {
          data.status = 'Waiting for opponent...';
        },
        [RoomStateEvent.CONNECTION_CLOSED](data) {
          data.status = 'Disconnected';
          data.unitsLocked = false;
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.ROOM_ENTERED](data, payload) {
          enterRoom(data, /** @type {RoomEnteredPayload} */ (payload));
          return SessionPhase.PLAYING;
        },
        [RoomStateEvent.ROOM_UNAVAILABLE](data) {
          data.status = 'Room unavailable';
        },
        [RoomStateEvent.RETURN_TO_SETUP](data, status) {
          returnToSetup(data, String(status));
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.SERVER_ERROR](data, reason) {
          data.status = String(reason);
        },
      },
      [SessionPhase.RESTORING]: {
        [RoomStateEvent.ROOM_ENTERED](data, payload) {
          enterRoom(data, /** @type {RoomEnteredPayload} */ (payload));
          return SessionPhase.PLAYING;
        },
        [RoomStateEvent.STATE_RECEIVED](data, payload) {
          applyViewSnapshot(data, /** @type {ViewSnapshotPayload} */ (payload));
          return SessionPhase.PLAYING;
        },
        [RoomStateEvent.RETURN_TO_SETUP](data, status) {
          returnToSetup(data, String(status));
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.CONNECTION_ERROR](data) {
          data.status = 'connection_error';
        },
        [RoomStateEvent.ROOM_UNAVAILABLE](data) {
          data.status = 'Room unavailable';
        },
        [RoomStateEvent.SERVER_ERROR](data, reason) {
          data.status = String(reason);
        },
      },
      [SessionPhase.PLAYING]: {
        [RoomStateEvent.STATE_RECEIVED](data, payload) {
          applyViewSnapshot(data, /** @type {ViewSnapshotPayload} */ (payload));
        },
        [RoomStateEvent.RETURN_TO_SETUP](data, status) {
          returnToSetup(data, String(status));
          return SessionPhase.SETUP;
        },
        [RoomStateEvent.OPPONENT_DISCONNECTED](data) {
          data.status = 'Opponent disconnected';
        },
        [RoomStateEvent.CONNECTION_CLOSED](data) {
          data.status = 'Disconnected';
        },
        [RoomStateEvent.CONNECTION_ERROR](data) {
          data.status = 'connection_error';
        },
        [RoomStateEvent.ROOM_UNAVAILABLE](data) {
          data.status = 'Room unavailable';
        },
        [RoomStateEvent.SERVER_ERROR](data, reason) {
          data.status = String(reason);
        },
      },
    },
  };
}

/**
 * @param {SessionPhaseValue} phase
 * @param {boolean} canSubmitMove
 * @param {import('./board-rows.js').BoardGridTile} tile
 * @returns {{ row: number, column: number } | undefined}
 */
export function moveForTarget(phase, canSubmitMove, tile) {
  if (
    phase !== SessionPhase.PLAYING ||
    !canSubmitMove ||
    tile.state === 'hit' ||
    tile.state === 'miss'
  ) {
    return undefined;
  }

  return {
    row: tile.row,
    column: tile.column,
  };
}

/**
 * @param {RoomSessionData} data
 * @param {RoomEnteredPayload} payload
 */
function enterRoom(data, payload) {
  data.unitsLocked = true;
  data.roomId = payload.roomId;
  data.playerId = payload.playerId;
  data.opponentId = payload.opponentId;
  data.status = 'In room';
}

/**
 * @param {RoomSessionData} data
 * @param {RestoreStartedPayload} payload
 */
function startRestore(data, payload) {
  data.unitsLocked = true;
  data.roomId = payload.roomId;
  data.playerId = payload.playerId;
  data.status = 'Reconnecting...';
}

/**
 * @param {RoomSessionData} data
 * @param {string} status
 */
function returnToSetup(data, status) {
  data.roomId = undefined;
  data.playerId = undefined;
  data.opponentId = undefined;
  data.isMyTurn = false;
  data.canSubmitMove = false;
  data.pendingView = undefined;
  data.setupPreviewCoordinates = [];
  data.unitsLocked = false;
  data.status = status;
}

/**
 * @param {RoomSessionData} data
 * @param {ViewSnapshotPayload} payload
 */
function applyViewSnapshot(data, payload) {
  const { viewModel } = payload;

  data.isMyTurn = viewModel.isMyTurn;
  data.canSubmitMove = viewModel.isMyTurn;
  data.status = viewModel.status;
  data.ownRows = rowsWithSunkState(
    rowsFromTiles('own-board', viewModel.ownTiles),
    viewModel.sunkClusters
  );
  data.targetRows = rowsFromTiles('target-board', viewModel.targetTiles);
  data.unitPlacements = viewModel.unitCoordinatesById;
}
