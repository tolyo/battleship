import Units from '../domain/unit-catalog.js';
import { emptyBoardState } from '../domain/board-state.js';
import {
  roomSessionConfig,
  RoomStateEvent,
  SessionPhase,
} from '../domain/room-state.js';

export class MatchStateService {
  static $inject = ['roomStore', '$machine'];

  /**
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {ng.MachineService} $machine
   */
  constructor(roomStore, $machine) {
    this.units = Units;
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
      this.roomMachine.send(RoomStateEvent.SETUP_INCOMPLETE);
      return false;
    }

    return this.roomMachine.send(RoomStateEvent.JOIN_REQUESTED);
  }

  socketOpened() {
    return this.roomMachine.send(RoomStateEvent.SOCKET_OPENED);
  }

  roomUnavailable() {
    return this.roomMachine.send(RoomStateEvent.ROOM_UNAVAILABLE);
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   */
  startRestore(roomId, playerId) {
    return this.roomMachine.send(RoomStateEvent.RESTORE_STARTED, {
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
    return this.roomMachine.send(RoomStateEvent.ROOM_ENTERED, entry);
  }

  opponentDisconnected() {
    return this.roomMachine.send(RoomStateEvent.OPPONENT_DISCONNECTED);
  }

  connectionClosed() {
    return this.roomMachine.send(RoomStateEvent.CONNECTION_CLOSED);
  }

  connectionError() {
    return this.roomMachine.send(RoomStateEvent.CONNECTION_ERROR);
  }

  /**
   * @param {string} reason
   */
  serverError(reason) {
    return this.roomMachine.send(RoomStateEvent.SERVER_ERROR, reason);
  }

  /**
   * @param {string} status
   */
  returnToSetup(status) {
    return this.roomMachine.send(RoomStateEvent.RETURN_TO_SETUP, status);
  }

  /**
   * @param {import('../domain/match-view-model.js').MatchViewModel} viewModel
   * @returns {boolean}
   */
  applyViewModel(viewModel) {
    return this.roomMachine.send(RoomStateEvent.STATE_RECEIVED, { viewModel });
  }

}

/**
 * @typedef {{
 *   player: string,
 *   boardState: string[][],
 * }} LobbyEntry
 */
