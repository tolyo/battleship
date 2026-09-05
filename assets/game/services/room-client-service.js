import {
  decodeRoomEvent,
  lobbyUrl,
  moveMessage,
  restoreUrl,
} from '../protocol/room-protocol.js';
import { CONNECTION_MODE, isLobbyContext } from './connection-context.js';

export class RoomClientService {
  static $inject = [
    'connection',
    'matchState',
    'target',
    'roomRestore',
    'roomEventDispatcher',
    'roomConnectionLifecycle',
  ];

  /**
   * @param {import('../../transport/connection-service.js').ConnectionService} connection
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {import('./target-service.js').TargetService} target
   * @param {import('./room-restore-service.js').RoomRestoreService} roomRestore
   * @param {import('./room-event-dispatcher-service.js').RoomEventDispatcherService} roomEventDispatcher
   * @param {import('./room-connection-lifecycle-service.js').RoomConnectionLifecycleService} roomConnectionLifecycle
   */
  constructor(
    connection,
    matchState,
    target,
    roomRestore,
    roomEventDispatcher,
    roomConnectionLifecycle
  ) {
    this.connection = connection;
    this.matchState = matchState;
    this.target = target;
    this.roomRestore = roomRestore;
    this.roomEventDispatcher = roomEventDispatcher;
    this.roomConnectionLifecycle = roomConnectionLifecycle;
  }

  joinLobby() {
    if (this.connection.isActive() || !this.matchState.tryEnterWaiting()) {
      return;
    }

    this.connect({ mode: CONNECTION_MODE.LOBBY });
  }

  restoreRoomFromCurrentUrl() {
    return this.roomRestore.restoreCurrentRequest();
  }

  /**
   * @param {import('../../room-store/room-store-service.js').StoredRoomEntry} entry
   * @param {{ timeoutMs?: number }} options
   * @returns {boolean}
   */
  restoreRoom(entry, options = {}) {
    if (!this.roomRestore.startRestore(entry, options)) {
      return false;
    }

    this.connect({
      mode: CONNECTION_MODE.RESTORE,
      roomId: entry.roomId,
      playerId: entry.playerId,
    });

    return true;
  }

  /**
   * @param {import('../domain/board-rows.js').BoardGridTile} tile
   */
  submitMove(tile) {
    if (!this.connection.isOpen()) {
      return;
    }

    const move = this.target.moveForTile(tile);
    if (!move) {
      return;
    }

    this.connection.send(moveMessage(move));
  }

  close() {
    this.roomRestore.clearRestoreTimeout();
    this.connection.close();
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   */
  connect(context) {
    this.connection.connect(this.urlForContext(context), {
      onOpen: () => {
        if (isLobbyContext(context)) {
          this.matchState.socketOpened();
        }
      },
      onMessage: (data) => {
        this.roomRestore.clearRestoreTimeout();
        this.roomEventDispatcher.dispatch(decodeRoomEvent(data), context);
      },
      onClose: () =>
        this.roomConnectionLifecycle.onClose(context, () => {
          this.close();
        }),
      onError: () =>
        this.roomConnectionLifecycle.onError(context, () => {
          this.close();
        }),
    });
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   * @returns {string}
   */
  urlForContext(context) {
    if (context.mode === CONNECTION_MODE.LOBBY) {
      return lobbyUrl(this.matchState.lobbyEntry);
    }

    return restoreUrl(context);
  }
}
