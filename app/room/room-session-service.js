const ANONYMOUS_PLAYER_KEY = 'battleship.anonymousPlayerName';
const ROOM_PLAYER_PREFIX = 'battleship.roomPlayer.';

export class RoomSessionService {
  /**
   * @returns {string}
   */
  anonymousPlayerName() {
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
  currentRoomId() {
    const match = /^\/room\/([^/?#]+)/.exec(window.location.pathname);
    if (!match) {
      return undefined;
    }

    return decodeURIComponent(match[1]);
  }

  /**
   * @param {string} roomId
   * @returns {string | undefined}
   */
  playerId(roomId) {
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
  rememberPlayer(roomId, playerId) {
    try {
      window.localStorage.setItem(roomPlayerKey(roomId), playerId);
    } catch {
      // A reload can only restore automatically when local storage is available.
    }
  }

  /**
   * @param {string} roomId
   */
  forgetPlayer(roomId) {
    try {
      window.localStorage.removeItem(roomPlayerKey(roomId));
    } catch {
      // Nothing to clear when local storage is unavailable.
    }
  }

  /**
   * @param {string} roomId
   */
  showRoom(roomId) {
    const roomPath = `/room/${encodeURIComponent(roomId)}`;
    if (window.location.pathname !== roomPath) {
      window.history.replaceState(null, '', roomPath);
    }
  }

  showHome() {
    if (window.location.pathname !== '/') {
      window.history.replaceState(null, '', '/');
    }
  }
}

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
 * @param {string} roomId
 * @returns {string}
 */
function roomPlayerKey(roomId) {
  return `${ROOM_PLAYER_PREFIX}${roomId}`;
}
