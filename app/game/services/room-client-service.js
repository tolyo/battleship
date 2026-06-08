import {
  decodeRoomEvent,
  lobbyUrl,
  moveMessage,
  restoreUrl,
} from '../protocol/room-protocol.js';
import { OperationTimeout } from '../../transport/operation-timeout.js';

/**
 * @typedef {{
 *   mode: 'lobby'
 * }} LobbyConnectionContext
 */

/**
 * @typedef {{
 *   mode: 'restore',
 *   roomId: string,
 *   playerId: string
 * }} RestoreConnectionContext
 */

/**
 * @typedef {LobbyConnectionContext | RestoreConnectionContext} ConnectionContext
 */

/**
 * @typedef {import('../protocol/room-protocol.js').RoomEvent} RoomEvent
 */

/**
 * @typedef {(
 *   client: RoomClientService,
 *   event: RoomEvent,
 *   context: ConnectionContext
 * ) => void} RoomEventHandler
 */

/**
 * @type {Partial<Record<RoomEvent['type'], RoomEventHandler>>}
 */
const ROOM_EVENT_HANDLERS = Object.freeze({
  waiting(client) {
    client.matchState.socketOpened();
  },
  room_entered(client, event) {
    const roomEntered =
      /** @type {Extract<RoomEvent, { type: 'room_entered' }>} */ (event);

    client.roomEntry.enter(roomEntered.entry, {
      updateUrl: roomEntered.updateUrl,
    });
  },
  state_received(client, event) {
    const stateReceived =
      /** @type {Extract<RoomEvent, { type: 'state_received' }>} */ (event);

    client.matchView.receiveSnapshot(stateReceived.view);
  },
  opponent_left(client) {
    client.matchState.opponentDisconnected();
  },
  room_unavailable(client, _event, context) {
    client.handleUnavailable(context);
  },
  server_error(client, event) {
    const serverError =
      /** @type {Extract<RoomEvent, { type: 'server_error' }>} */ (event);

    client.matchState.serverError(serverError.reason);
  },
});

export class RoomClientService {
  static $inject = [
    'connection',
    'roomStore',
    'matchState',
    'target',
    'matchView',
    'roomEntry',
  ];

  /**
   * @param {import('../../transport/connection-service.js').ConnectionService} connection
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {import('./target-service.js').TargetService} target
   * @param {import('./match-view-service.js').MatchViewService} matchView
   * @param {import('./room-entry-service.js').RoomEntryService} roomEntry
   */
  constructor(connection, roomStore, matchState, target, matchView, roomEntry) {
    this.connection = connection;
    this.roomStore = roomStore;
    this.matchState = matchState;
    this.target = target;
    this.matchView = matchView;
    this.roomEntry = roomEntry;
    this.restoreTimeout = new OperationTimeout();
  }

  joinLobby() {
    if (this.connection.isActive() || !this.matchState.tryEnterWaiting()) {
      return;
    }

    this.connect({ mode: 'lobby' });
  }

  restoreRoomFromCurrentUrl() {
    const request = this.roomStore.currentRestoreRequest();
    if (request.type === 'restore') {
      this.restoreRoom({
        roomId: request.roomId,
        playerId: request.playerId,
      });
    } else if (request.type === 'missing_player') {
      this.matchState.roomUnavailable();
    }
  }

  /**
   * @param {import('../../room-store/room-store-service.js').StoredRoomEntry} entry
   * @param {{ timeoutMs?: number }} options
   * @returns {boolean}
   */
  restoreRoom(entry, options = {}) {
    if (!this.matchState.startRestore(entry.roomId, entry.playerId)) {
      return false;
    }

    this.connect({
      mode: 'restore',
      roomId: entry.roomId,
      playerId: entry.playerId,
    });

    this.restoreTimeout.start(
      () => this.restoreFailed(entry.roomId, 'Room unavailable'),
      options.timeoutMs ?? 5000
    );

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
    this.restoreTimeout.clear();
    this.connection.close();
  }

  /**
   * @param {ConnectionContext} context
   */
  connect(context) {
    this.connection.connect(this.urlForContext(context), {
      onOpen: () => {
        if (context.mode === 'lobby') {
          this.matchState.socketOpened();
        }
      },
      onMessage: (data) => {
        this.restoreTimeout.clear();
        this.handleEvent(decodeRoomEvent(data), context);
      },
      onClose: () => this.handleClose(context),
      onError: () => this.handleConnectionError(context),
    });
  }

  /**
   * @param {ConnectionContext} context
   * @returns {string}
   */
  urlForContext(context) {
    if (context.mode === 'lobby') {
      return lobbyUrl(this.matchState.lobbyEntry);
    }

    return restoreUrl(context);
  }

  /**
   * @param {import('../protocol/room-protocol.js').RoomEvent} event
   * @param {ConnectionContext} context
   */
  handleEvent(event, context) {
    const handler = ROOM_EVENT_HANDLERS[event.type];
    if (handler) {
      handler(this, event, context);
    }
  }

  /**
   * @param {ConnectionContext} context
   */
  handleClose(context) {
    if (
      context.mode === 'restore' &&
      this.matchState.isRestoring
    ) {
      this.restoreFailed(context.roomId, 'Room unavailable');
      return;
    }

    this.matchState.connectionClosed();
  }

  /**
   * @param {ConnectionContext} context
   */
  handleConnectionError(context) {
    if (context.mode === 'restore') {
      this.handleUnavailable(context);
      return;
    }

    this.matchState.connectionError();
  }

  /**
   * @param {ConnectionContext} context
   */
  handleUnavailable(context) {
    if (context.mode === 'restore') {
      this.restoreFailed(context.roomId, 'Room unavailable');
      return;
    }

    this.matchState.roomUnavailable();
  }

  /**
   * @param {string} roomId
   * @param {string} status
   */
  restoreFailed(roomId, status) {
    this.roomStore.leaveRoom(roomId);
    this.matchState.returnToSetup(status);
    this.close();
  }
}
