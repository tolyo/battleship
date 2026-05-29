/**
 * @param {string} player
 * @param {string[][]} boardState
 * @returns {string}
 */
export function lobbyUrl(player, boardState) {
  const playerParam = encodeURIComponent(player);
  const boardParam = encodeURIComponent(JSON.stringify(boardState));

  return `/ws?player=${playerParam}&board=${boardParam}`;
}

/**
 * @param {string | undefined} roomId
 * @param {string | undefined} playerId
 * @returns {string}
 */
export function restoreUrl(roomId, playerId) {
  return `/ws?room_id=${encodeURIComponent(roomId ?? '')}&player_id=${encodeURIComponent(playerId ?? '')}`;
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
 * @param {string} reason
 * @returns {boolean}
 */
export function isRoomUnavailableReason(reason) {
  return reason === 'room_not_found' || reason === 'unknown_player';
}

/**
 * @typedef {{
 *   type: 'waiting'
 * } | {
 *   type: 'room_entered',
 *   message: import('../map/server-message.js').ServerMessage,
 *   updateUrl: boolean
 * } | {
 *   type: 'game_received',
 *   game: unknown
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
 * }} BattleRoomEvent
 */

/**
 * @param {import('../map/server-message.js').ServerMessage} message
 * @returns {BattleRoomEvent}
 */
export function battleRoomEvent(message) {
  if (message.type === 'lobby_waiting') {
    return { type: 'waiting' };
  }

  if (message.type === 'match_found') {
    return { type: 'room_entered', message, updateUrl: true };
  }

  if (message.type === 'room_joined') {
    return { type: 'room_entered', message, updateUrl: false };
  }

  if (message.type === 'game_update' || message.type === 'game_state') {
    return { type: 'game_received', game: message.game };
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
