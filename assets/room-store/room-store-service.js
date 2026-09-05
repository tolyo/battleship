const ANONYMOUS_PLAYER_KEY = 'battleship.anonymousPlayerName';
const ROOM_PLAYER_PREFIX = 'battleship.roomPlayer.';

export const RESTORE_REQUEST = Object.freeze({
  NONE: 'none',
  MISSING_PLAYER: 'missing_player',
  RESTORE: 'restore',
});

export class RoomStoreService {
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
   * @returns {CurrentRestoreRequest}
   */
  currentRestoreRequest() {
    const roomId = this.currentRoomId();
    if (!roomId) {
      return { type: RESTORE_REQUEST.NONE };
    }

    const entry = this.restoreEntry(roomId);
    if (!entry) {
      return { type: RESTORE_REQUEST.MISSING_PLAYER, roomId };
    }

    return {
      type: RESTORE_REQUEST.RESTORE,
      ...entry,
    };
  }

  /**
   * @param {string} roomId
   * @returns {StoredRoomEntry | undefined}
   */
  restoreEntry(roomId) {
    const playerId = this.playerId(roomId);
    if (!playerId) {
      return undefined;
    }

    return {
      roomId,
      playerId,
    };
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
   * @param {string} playerId
   * @param {{ updateUrl?: boolean }} options
   */
  enterRoom(roomId, playerId, options = {}) {
    this.rememberPlayer(roomId, playerId);
    if (options.updateUrl) {
      this.showRoom(roomId);
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
  leaveRoom(roomId) {
    this.forgetPlayer(roomId);
    this.showHome();
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

/**
 * @typedef {{
 *   roomId: string,
 *   playerId: string
 * }} StoredRoomEntry
 */

/**
 * @typedef {{
 *   type: 'none'
 * } | {
 *   type: 'missing_player',
 *   roomId: string
 * } | ({
 *   type: 'restore'
 * } & StoredRoomEntry)} CurrentRestoreRequest
 */
