const ANONYMOUS_PLAYER_KEY = 'battleship.anonymousPlayerName';
const ROOM_PLAYER_PREFIX = 'battleship.roomPlayer.';

/**
 * @returns {string}
 */
function anonymousSuffix() {
  return Math.floor(Math.random() * 2176782336)
    .toString(36)
    .padStart(6, '0')
    .toUpperCase();
}

/**
 * @returns {string}
 */
export function anonymousPlayerName() {
  const fallback = `Anonymous ${anonymousSuffix()}`;

  try {
    const storedName = window.localStorage.getItem(ANONYMOUS_PLAYER_KEY);
    if (storedName) {
      return storedName;
    }

    window.localStorage.setItem(ANONYMOUS_PLAYER_KEY, fallback);
  } catch {
    return fallback;
  }

  return fallback;
}

/**
 * @returns {string | undefined}
 */
export function roomIdFromPath() {
  const match = /^\/room\/([^/?#]+)/.exec(window.location.pathname);
  if (!match) {
    return undefined;
  }

  return decodeURIComponent(match[1]);
}

/**
 * @param {string} roomId
 * @returns {string}
 */
function roomPlayerKey(roomId) {
  return `${ROOM_PLAYER_PREFIX}${roomId}`;
}

/**
 * @param {string} roomId
 * @returns {string | undefined}
 */
export function storedPlayerId(roomId) {
  try {
    return window.localStorage.getItem(roomPlayerKey(roomId)) || undefined;
  } catch {
    return undefined;
  }
}

/**
 * @param {string} roomId
 * @param {string} playerId
 */
export function rememberRoomPlayer(roomId, playerId) {
  try {
    window.localStorage.setItem(roomPlayerKey(roomId), playerId);
  } catch {
    // A reload can only restore automatically when local storage is available.
  }
}

/**
 * @param {string} roomId
 */
export function replaceUrlWithRoom(roomId) {
  const roomPath = `/room/${encodeURIComponent(roomId)}`;
  if (window.location.pathname !== roomPath) {
    window.history.replaceState(null, '', roomPath);
  }
}
