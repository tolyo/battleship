import Units from '../domain/unit-catalog.js';
import { emptyBoardState } from '../domain/board-state.js';
import {
  roomSessionConfig,
  RoomStateEvent,
  SessionPhase,
} from '../domain/room-state.js';

export class MatchStateService {
  static $inject = ['roomStore', '$machine', '$rootScope'];

  /**
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {ng.MachineService} $machine
   * @param {ng.Scope=} $rootScope
   */
  constructor(roomStore, $machine, $rootScope) {
    this.units = Units;
    this.$rootScope = $rootScope;
    /** @type {string[][]} */
    this.boardState = emptyBoardState();
    /** @type {string} */
    this.player = roomStore.anonymousPlayerName();
    /** @type {string} */
    this.playerLabel = 'Anonymous';
    this.roomMachine = $machine(roomSessionConfig());
  }

  /**
   * @returns {import('../domain/room-state.js').SessionPhaseValue}
   */
  get phase() {
    return /** @type {import('../domain/room-state.js').SessionPhaseValue} */ (
      this.roomMachine.current
    );
  }

  get data() {
    return this.roomMachine.data;
  }

  get isSetup() {
    return this.roomMachine.matches(SessionPhase.SETUP);
  }

  get isPlaying() {
    return this.roomMachine.matches(SessionPhase.PLAYING);
  }

  get isRestoring() {
    return this.roomMachine.matches(SessionPhase.RESTORING);
  }

  get status() {
    return this.data.status;
  }

  get boardReady() {
    return this.data.boardReady;
  }

  get roomId() {
    return this.data.roomId;
  }

  get playerId() {
    return this.data.playerId;
  }

  get opponentId() {
    return this.data.opponentId;
  }

  get canSubmitMove() {
    return this.data.canSubmitMove;
  }

  get targetVisible() {
    return this.data.targetVisible;
  }

  get targetDisabled() {
    return this.data.targetDisabled;
  }

  get unitPlacements() {
    return this.data.unitPlacements;
  }

  get ownRows() {
    return this.data.ownRows;
  }

  get targetRows() {
    return this.data.targetRows;
  }

  get unitsLocked() {
    return this.data.unitsLocked;
  }

  /**
   * @returns {LobbyEntry}
   */
  get lobbyEntry() {
    return {
      player: this.player,
      boardState: this.boardState,
    };
  }

  /**
   * @returns {boolean}
   */
  tryEnterWaiting() {
    if (!this.isSetup) {
      return false;
    }

    if (!this.boardReady) {
      this.transition(RoomStateEvent.SETUP_INCOMPLETE);
      return false;
    }

    return this.transition(RoomStateEvent.JOIN_REQUESTED);
  }

  socketOpened() {
    return this.transition(RoomStateEvent.SOCKET_OPENED);
  }

  roomUnavailable() {
    return this.transition(RoomStateEvent.ROOM_UNAVAILABLE);
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   */
  startRestore(roomId, playerId) {
    return this.transition(RoomStateEvent.RESTORE_STARTED, {
      roomId,
      playerId,
    });
  }

  /**
   * @param {{
   *   roomId: string,
   *   playerId: string,
   *   opponentId?: string,
   * }} entry
   */
  enterRoom(entry) {
    return this.transition(RoomStateEvent.ROOM_ENTERED, entry);
  }

  opponentDisconnected() {
    return this.transition(RoomStateEvent.OPPONENT_DISCONNECTED);
  }

  connectionClosed() {
    return this.transition(RoomStateEvent.CONNECTION_CLOSED);
  }

  connectionError() {
    return this.transition(RoomStateEvent.CONNECTION_ERROR);
  }

  /**
   * @param {string} reason
   */
  serverError(reason) {
    return this.transition(RoomStateEvent.SERVER_ERROR, reason);
  }

  /**
   * @param {string} status
   */
  returnToSetup(status) {
    return this.transition(RoomStateEvent.RETURN_TO_SETUP, status);
  }

  /**
   * @param {import('../domain/match-view-model.js').MatchViewModel} viewModel
   * @returns {boolean}
   */
  applyViewModel(viewModel) {
    return this.transition(RoomStateEvent.STATE_RECEIVED, { viewModel });
  }

  /**
   * @param {string} type
   * @param {unknown=} payload
   * @returns {boolean}
   */
  transition(type, payload) {
    const changed = this.roomMachine.send(type, payload);
    if (changed) {
      this.refresh();
    }

    return changed;
  }

  refresh() {
    const handler = /** @type {ScopeRefreshHandler | undefined} */ (
      this.$rootScope?.$handler
    );
    handler?._checkListenersForAllKeys?.(this);
    handler?._checkListenersForAllKeys?.(this.data);
    handler?._flushScheduledTasks?.();
    this.syncTargetBoardState();
  }

  syncTargetBoardState() {
    const targetBoard = globalThis.document?.querySelector?.('#target-board');
    targetBoard?.classList?.toggle?.('disabled', this.targetDisabled);
  }
}

/**
 * @typedef {{
 *   player: string,
 *   boardState: string[][],
 * }} LobbyEntry
 */

/**
 * @typedef {{
 *   _checkListenersForAllKeys?: (value: unknown) => void,
 *   _flushScheduledTasks?: () => void
 * }} ScopeRefreshHandler
 */
