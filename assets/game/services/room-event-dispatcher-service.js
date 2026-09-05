import { ROOM_UNAVAILABLE_STATUS } from '../domain/room-state.js';
import { ROOM_EVENT } from '../protocol/room-protocol.js';

/**
 * @typedef {import('../protocol/room-protocol.js').RoomEvent} RoomEvent
 */

/**
 * @typedef {(
 *   service: RoomEventDispatcherService,
 *   event: RoomEvent,
 *   context: import('./connection-context.js').ConnectionContext
 * ) => void} RoomEventHandler
 */

/** @type {Partial<Record<RoomEvent['type'], RoomEventHandler>>} */
const ROOM_EVENT_HANDLERS = Object.freeze({
  [ROOM_EVENT.WAITING](service) {
    service.matchState.socketOpened();
  },
  [ROOM_EVENT.ROOM_ENTERED](service, event) {
    const roomEntered =
      /** @type {Extract<RoomEvent, { type: 'room_entered' }>} */ (event);

    service.roomEntry.enter(roomEntered.entry, {
      updateUrl: roomEntered.updateUrl,
    });
  },
  [ROOM_EVENT.STATE_RECEIVED](service, event) {
    const stateReceived =
      /** @type {Extract<RoomEvent, { type: 'state_received' }>} */ (event);

    service.matchView.receiveSnapshot(stateReceived.view);
  },
  [ROOM_EVENT.OPPONENT_LEFT](service) {
    service.matchState.opponentDisconnected();
  },
  [ROOM_EVENT.ROOM_UNAVAILABLE](service, _event, context) {
    if (
      service.roomRestore.handleConnectionDisconnect(
        context,
        ROOM_UNAVAILABLE_STATUS
      )
    ) {
      return;
    }

    service.matchState.roomUnavailable();
  },
  [ROOM_EVENT.SERVER_ERROR](service, event) {
    const serverError =
      /** @type {Extract<RoomEvent, { type: 'server_error' }>} */ (event);

    service.matchState.serverError(serverError.reason);
  },
});

export class RoomEventDispatcherService {
  static $inject = ['matchState', 'matchView', 'roomEntry', 'roomRestore'];

  /**
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {import('./match-view-service.js').MatchViewService} matchView
   * @param {import('./room-entry-service.js').RoomEntryService} roomEntry
   * @param {import('./room-restore-service.js').RoomRestoreService} roomRestore
   */
  constructor(matchState, matchView, roomEntry, roomRestore) {
    this.matchState = matchState;
    this.matchView = matchView;
    this.roomEntry = roomEntry;
    this.roomRestore = roomRestore;
  }

  /**
   * @param {RoomEvent} event
   * @param {import('./connection-context.js').ConnectionContext} context
   */
  dispatch(event, context) {
    const handler = ROOM_EVENT_HANDLERS[event.type];
    if (handler) {
      handler(this, event, context);
    }
  }
}
