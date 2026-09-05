export const CONNECTION_MODE = Object.freeze({
  LOBBY: 'lobby',
  RESTORE: 'restore',
});

/**
 * @typedef {{
 *   mode: (typeof CONNECTION_MODE.LOBBY)
 * }} LobbyConnectionContext
 */

/**
 * @typedef {{
 *   mode: (typeof CONNECTION_MODE.RESTORE),
 *   roomId: string,
 *   playerId: string
 * }} RestoreConnectionContext
 */

/**
 * @typedef {LobbyConnectionContext | RestoreConnectionContext} ConnectionContext
 */

/**
 * @param {ConnectionContext} context
 * @returns {context is RestoreConnectionContext}
 */
export function isRestoreContext(context) {
  return context.mode === CONNECTION_MODE.RESTORE;
}

/**
 * @param {ConnectionContext} context
 * @returns {context is LobbyConnectionContext}
 */
export function isLobbyContext(context) {
  return context.mode === CONNECTION_MODE.LOBBY;
}
