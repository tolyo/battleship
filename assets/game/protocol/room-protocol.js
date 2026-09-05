import { decodeServerMessage } from './server-message.js';

export const ROOM_EVENT = Object.freeze({
  WAITING: 'waiting',
  ROOM_ENTERED: 'room_entered',
  STATE_RECEIVED: 'state_received',
  OPPONENT_LEFT: 'opponent_left',
  ROOM_UNAVAILABLE: 'room_unavailable',
  SERVER_ERROR: 'server_error',
  IGNORED: 'ignored',
});

export const ROOM_SERVER_MESSAGE = Object.freeze({
  LOBBY_WAITING: 'lobby_waiting',
  MATCH_FOUND: 'match_found',
  ROOM_JOINED: 'room_joined',
  ROOM_UPDATE: 'room_update',
  ROOM_STATE: 'room_state',
  OPPONENT_LEFT: 'opponent_left',
  ERROR: 'error',
});

export const CLIENT_MESSAGE = Object.freeze({
  MOVE: 'move',
});

/**
 * @param {LobbyEntry} entry
 * @returns {string}
 */
export function lobbyUrl(entry) {
  const playerParam = encodeURIComponent(entry.player);
  const boardParam = encodeURIComponent(JSON.stringify(entry.boardState));

  return `/ws?player=${playerParam}&board=${boardParam}`;
}

/**
 * @param {RestoreEntry} entry
 * @returns {string}
 */
export function restoreUrl(entry) {
  return `/ws?room_id=${encodeURIComponent(entry.roomId)}&player_id=${encodeURIComponent(entry.playerId)}`;
}

/**
 * @param {{ row: number, column: number }} coordinate
 * @returns {{ type: string, row: number, column: number }}
 */
export function moveMessage(coordinate) {
  return {
    type: CLIENT_MESSAGE.MOVE,
    row: coordinate.row,
    column: coordinate.column,
  };
}

/**
 * @param {unknown} data
 * @returns {RoomEvent}
 */
export function decodeRoomEvent(data) {
  return roomEventFromServerMessage(decodeServerMessage(data));
}

/**
 * @param {string} reason
 * @returns {boolean}
 */
function isRoomUnavailableReason(reason) {
  return reason === 'room_not_found' || reason === 'unknown_player';
}

/**
 * @typedef {{
 *   player: string,
 *   boardState: string[][],
 * }} LobbyEntry
 */

/**
 * @typedef {{
 *   roomId: string,
 *   playerId: string,
 * }} RestoreEntry
 */

/**
 * @typedef {{
 *   roomId: string,
 *   playerId: string,
 *   opponentId: string | undefined,
 *   view?: unknown
 * }} RoomEntry
 */

/**
 * @typedef {{
 *   type: 'waiting'
 * } | {
 *   type: 'room_entered',
 *   entry: RoomEntry,
 *   updateUrl: boolean
 * } | {
 *   type: 'state_received',
 *   view: unknown
 * } | {
 *   type: 'opponent_left'
 * } | {
 *   type: 'room_unavailable',
 *   reason: string
 * } | {
 *   type: 'server_error',
 *   reason: string
 * } | {
 *   type: 'ignored'
 * }} RoomEvent
 */

/**
 * @param {import('./server-message.js').ServerMessage} message
 * @returns {RoomEvent}
 */
export function roomEventFromServerMessage(message) {
  if (message.type === ROOM_SERVER_MESSAGE.LOBBY_WAITING) {
    return { type: ROOM_EVENT.WAITING };
  }

  if (message.type === ROOM_SERVER_MESSAGE.MATCH_FOUND) {
    return roomEnteredEvent(message, true);
  }

  if (message.type === ROOM_SERVER_MESSAGE.ROOM_JOINED) {
    return roomEnteredEvent(message, false);
  }

  if (
    message.type === ROOM_SERVER_MESSAGE.ROOM_UPDATE ||
    message.type === ROOM_SERVER_MESSAGE.ROOM_STATE
  ) {
    return { type: ROOM_EVENT.STATE_RECEIVED, view: message.view };
  }

  if (message.type === ROOM_SERVER_MESSAGE.OPPONENT_LEFT) {
    return { type: ROOM_EVENT.OPPONENT_LEFT };
  }

  if (message.type === ROOM_SERVER_MESSAGE.ERROR) {
    const reason = message.reason || 'server_error';
    if (isRoomUnavailableReason(reason)) {
      return { type: ROOM_EVENT.ROOM_UNAVAILABLE, reason };
    }

    return { type: ROOM_EVENT.SERVER_ERROR, reason };
  }

  return { type: ROOM_EVENT.IGNORED };
}

/**
 * @param {import('./server-message.js').ServerMessage} message
 * @param {boolean} updateUrl
 * @returns {RoomEvent}
 */
function roomEnteredEvent(message, updateUrl) {
  if (!message.room_id || !message.player_id) {
    return {
      type: ROOM_EVENT.ROOM_UNAVAILABLE,
      reason: 'invalid_room_entry',
    };
  }

  return {
    type: ROOM_EVENT.ROOM_ENTERED,
    entry: {
      roomId: message.room_id,
      playerId: message.player_id,
      opponentId: message.opponent_id,
      view: message.view,
    },
    updateUrl,
  };
}
