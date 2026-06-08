import { decodeServerMessage } from './server-message.js';

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
 * @returns {{ type: 'move', row: number, column: number }}
 */
export function moveMessage(coordinate) {
  return {
    type: 'move',
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
  if (message.type === 'lobby_waiting') {
    return { type: 'waiting' };
  }

  if (message.type === 'match_found') {
    return roomEnteredEvent(message, true);
  }

  if (message.type === 'room_joined') {
    return roomEnteredEvent(message, false);
  }

  if (message.type === 'room_update' || message.type === 'room_state') {
    return { type: 'state_received', view: message.view };
  }

  if (message.type === 'opponent_left') {
    return { type: 'opponent_left' };
  }

  if (message.type === 'error') {
    const reason = message.reason || 'server_error';
    if (isRoomUnavailableReason(reason)) {
      return { type: 'room_unavailable', reason };
    }

    return { type: 'server_error', reason };
  }

  return { type: 'ignored' };
}

/**
 * @param {import('./server-message.js').ServerMessage} message
 * @param {boolean} updateUrl
 * @returns {RoomEvent}
 */
function roomEnteredEvent(message, updateUrl) {
  if (!message.room_id || !message.player_id) {
    return { type: 'room_unavailable', reason: 'invalid_room_entry' };
  }

  return {
    type: 'room_entered',
    entry: {
      roomId: message.room_id,
      playerId: message.player_id,
      opponentId: message.opponent_id,
      view: message.view,
    },
    updateUrl,
  };
}
