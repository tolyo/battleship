import { gameViewModelFromState } from './game-view-model.js';
import { rowsFromTiles, rowsWithSunkState } from './board-rows.js';

/**
 * @typedef {{
 *   phase: 'setup' | 'waiting' | 'playing',
 *   status: string,
 *   boardReady: boolean,
 *   roomId: string | undefined,
 *   playerId: string | undefined,
 *   opponentId: string | undefined,
 *   isMyTurn: boolean,
 *   canStrike: boolean,
 *   pendingGame: unknown,
 *   fleetRows: import('./board-rows.js').BoardGridTile[][],
 *   hitRows: import('./board-rows.js').BoardGridTile[][],
 *   setFleetLocked: (locked: boolean) => void
 * }} BattleRoomState
 */

/**
 * @param {BattleRoomState} state
 */
export function enterWaiting(state) {
  state.phase = 'waiting';
  state.setFleetLocked(true);
  state.status = 'Connecting...';
}

/**
 * @param {BattleRoomState} state
 * @returns {boolean}
 */
export function tryEnterWaiting(state) {
  if (state.phase !== 'setup') {
    return false;
  }

  if (!state.boardReady) {
    state.status = 'Place your fleet first';
    return false;
  }

  enterWaiting(state);
  return true;
}

/**
 * @param {BattleRoomState} state
 */
export function waitingForOpponent(state) {
  state.status = 'Waiting for opponent...';
}

/**
 * @param {BattleRoomState} state
 */
export function disconnectWaiting(state) {
  state.status = 'Disconnected';
  if (state.phase === 'waiting') {
    state.phase = 'setup';
    state.setFleetLocked(false);
  }
}

/**
 * @param {BattleRoomState} state
 * @param {string} status
 */
export function returnToSetup(state, status) {
  state.phase = 'setup';
  state.roomId = undefined;
  state.playerId = undefined;
  state.opponentId = undefined;
  state.isMyTurn = false;
  state.canStrike = false;
  state.pendingGame = undefined;
  state.setFleetLocked(false);
  state.status = status;
}

/**
 * @param {BattleRoomState} state
 * @param {string} roomId
 * @param {string} playerId
 * @param {string | undefined} opponentId
 */
export function enterRoom(state, roomId, playerId, opponentId) {
  state.phase = 'playing';
  state.setFleetLocked(true);
  state.roomId = roomId;
  state.playerId = playerId;
  state.opponentId = opponentId;
  state.status = 'In room';
}

/**
 * @param {BattleRoomState} state
 * @param {string} roomId
 * @param {string} playerId
 */
export function startRestore(state, roomId, playerId) {
  state.phase = 'playing';
  state.setFleetLocked(true);
  state.roomId = roomId;
  state.playerId = playerId;
  state.status = 'Reconnecting...';
}

/**
 * @param {BattleRoomState} state
 * @returns {boolean}
 */
export function isRestoring(state) {
  return state.status === 'Reconnecting...';
}

/**
 * @param {BattleRoomState} state
 * @param {{
 *   roomId: string | undefined,
 *   playerId: string | undefined,
 *   opponentId: string | undefined,
 *   game: unknown
 * }} room
 * @returns {{
 *   shipCoordinatesById: Record<string, import('./game-view-model.js').Coordinate[]> | undefined
 * } | undefined}
 */
export function enterBattleRoom(state, room) {
  if (!room.roomId || !room.playerId) {
    roomUnavailable(state);
    return undefined;
  }

  enterRoom(state, room.roomId, room.playerId, room.opponentId);
  const shipCoordinatesById = receiveGameState(
    state,
    room.game ?? state.pendingGame
  );
  state.pendingGame = undefined;
  return { shipCoordinatesById };
}

/**
 * @param {BattleRoomState} state
 * @param {unknown} game
 * @returns {Record<string, import('./game-view-model.js').Coordinate[]> | undefined}
 */
export function receiveGameState(state, game) {
  if (!state.playerId) {
    state.pendingGame = game;
    return undefined;
  }

  const viewModel = gameViewModelFromState(game, state.playerId);
  if (!viewModel) {
    return undefined;
  }

  state.isMyTurn = viewModel.isMyTurn;
  state.canStrike = state.phase === 'playing' && viewModel.isMyTurn;
  state.status = viewModel.status;
  state.fleetRows = rowsWithSunkState(
    rowsFromTiles('fleetboard', viewModel.fleetTiles),
    viewModel.sunkClusters
  );
  state.hitRows = rowsFromTiles('hitboard', viewModel.hitTiles);
  return viewModel.shipCoordinatesById;
}

/**
 * @param {BattleRoomState} state
 * @param {import('./board-rows.js').BoardGridTile} tile
 * @returns {{ row: number, column: number } | undefined}
 */
export function moveForStrike(state, tile) {
  if (
    state.phase !== 'playing' ||
    !state.isMyTurn ||
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
 * @param {BattleRoomState} state
 */
export function roomUnavailable(state) {
  state.status = 'Room unavailable';
}

/**
 * @param {BattleRoomState} state
 */
export function opponentDisconnected(state) {
  state.status = 'Opponent disconnected';
}

/**
 * @param {BattleRoomState} state
 */
export function connectionError(state) {
  state.status = 'connection_error';
}

/**
 * @param {BattleRoomState} state
 * @param {string} reason
 */
export function serverError(state, reason) {
  state.status = reason;
}
