import { OperationTimeout } from '../../transport/operation-timeout.js';
import {
  DEFAULT_RESTORE_TIMEOUT_MS,
  ROOM_UNAVAILABLE_STATUS,
} from '../domain/room-state.js';
import { RESTORE_REQUEST } from '../../room-store/room-store-service.js';
import { isRestoreContext } from './connection-context.js';

export class RoomRestoreService {
  static $inject = ['roomStore', 'matchState', 'roomRecovery'];

  /**
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {import('./room-recovery-service.js').RoomRecoveryService} roomRecovery
   */
  constructor(roomStore, matchState, roomRecovery) {
    this.roomStore = roomStore;
    this.matchState = matchState;
    this.roomRecovery = roomRecovery;
    this.restoreTimeout = new OperationTimeout();
  }

  /**
   * @returns {boolean}
   */
  restoreCurrentRequest() {
    const request = this.roomStore.currentRestoreRequest();

    if (request.type === RESTORE_REQUEST.RESTORE) {
      return this.startRestore(request);
    }

    if (request.type === RESTORE_REQUEST.MISSING_PLAYER) {
      this.matchState.roomUnavailable();
    }

    return false;
  }

  /**
   * @param {import('../../room-store/room-store-service.js').StoredRoomEntry} entry
   * @param {{ timeoutMs?: number }} options
   * @returns {boolean}
   */
  startRestore(entry, options = {}) {
    if (!this.matchState.startRestore(entry.roomId, entry.playerId)) {
      return false;
    }

    this.restoreTimeout.start(() => {
      this.roomRecovery.restoreFailed(entry.roomId, ROOM_UNAVAILABLE_STATUS);
    }, options.timeoutMs ?? DEFAULT_RESTORE_TIMEOUT_MS);

    return true;
  }

  clearRestoreTimeout() {
    this.restoreTimeout.clear();
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   * @param {string} reason
   * @param {{ requiresActiveRestore?: boolean }} options
   * @returns {boolean}
   */
  handleConnectionDisconnect(
    context,
    reason = ROOM_UNAVAILABLE_STATUS,
    options = {}
  ) {
    if (!isRestoreContext(context)) {
      return false;
    }

    const { requiresActiveRestore = true } = options;

    if (requiresActiveRestore && !this.matchState.isRestoring) {
      return false;
    }

    this.failRestore(context.roomId, reason);
    return true;
  }

  /**
   * @param {string} roomId
   * @param {string} status
   */
  failRestore(roomId, status) {
    this.restoreTimeout.clear();
    this.roomRecovery.restoreFailed(roomId, status);
  }
}
