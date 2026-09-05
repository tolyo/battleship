import { ROOM_UNAVAILABLE_STATUS } from '../domain/room-state.js';
import { RoomEventDispatcherService } from './room-event-dispatcher-service.js';
import { ROOM_EVENT } from '../protocol/room-protocol.js';
import { CONNECTION_MODE } from './connection-context.js';

describe('RoomEventDispatcherService', () => {
  it('opens lobby socket on waiting event', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.WAITING },
      { mode: CONNECTION_MODE.LOBBY }
    );

    expect(matchState.socketOpened).toHaveBeenCalledOnceWith();
  });

  it('forwards room entered events to room entry service', () => {
    const { dispatcher, roomEntry } = roomEventDispatcher();
    const entry = {
      roomId: 'room-1',
      playerId: 'player-1',
      opponentId: 'player-2',
    };

    dispatcher.dispatch(
      {
        type: ROOM_EVENT.ROOM_ENTERED,
        entry,
        updateUrl: true,
      },
      { mode: CONNECTION_MODE.LOBBY }
    );

    expect(roomEntry.enter).toHaveBeenCalledOnceWith(entry, {
      updateUrl: true,
    });
  });

  it('forwards state snapshots to match view service', () => {
    const { dispatcher, matchView } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.STATE_RECEIVED, view: { id: 1 } },
      { mode: CONNECTION_MODE.LOBBY }
    );

    expect(matchView.receiveSnapshot).toHaveBeenCalledOnceWith({ id: 1 });
  });

  it('marks opponent left in match state', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.OPPONENT_LEFT },
      { mode: CONNECTION_MODE.LOBBY }
    );

    expect(matchState.opponentDisconnected).toHaveBeenCalledOnceWith();
  });

  it('maps server errors to match state server errors', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.SERVER_ERROR, reason: 'something' },
      { mode: CONNECTION_MODE.LOBBY }
    );

    expect(matchState.serverError).toHaveBeenCalledOnceWith('something');
  });

  it('maps unavailable room events to restore flow during restore mode', () => {
    const { dispatcher, roomRestore } = roomEventDispatcher({
      matchStateOverrides: { isRestoring: true },
    });

    dispatcher.dispatch(
      { type: ROOM_EVENT.ROOM_UNAVAILABLE },
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      }
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      ROOM_UNAVAILABLE_STATUS
    );
  });

  it('maps unavailable room events to match state when restore does not handle it', () => {
    const { dispatcher, roomRestore, matchState } = roomEventDispatcher({
      matchStateOverrides: { isRestoring: false },
    });

    dispatcher.dispatch(
      { type: ROOM_EVENT.ROOM_UNAVAILABLE },
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      }
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      ROOM_UNAVAILABLE_STATUS
    );
    expect(matchState.roomUnavailable).toHaveBeenCalledOnceWith();
  });
});

function roomEventDispatcher({
  matchStateOverrides = {},
  matchViewOverrides = {},
  roomEntryOverrides = {},
  roomRestoreOverrides = {},
} = {}) {
  const matchState = {
    isRestoring: false,
    socketOpened: jasmine.createSpy('socketOpened'),
    roomUnavailable: jasmine.createSpy('roomUnavailable'),
    serverError: jasmine.createSpy('serverError'),
    opponentDisconnected: jasmine.createSpy('opponentDisconnected'),
    ...matchStateOverrides,
  };
  const roomRestore = {
    handleConnectionDisconnect: jasmine
      .createSpy('handleConnectionDisconnect')
      .and.callFake((context) => {
        if (
          context.mode !== CONNECTION_MODE.RESTORE ||
          !matchState.isRestoring
        ) {
          return false;
        }

        return true;
      }),
    ...roomRestoreOverrides,
  };
  const matchView = {
    receiveSnapshot: jasmine.createSpy('receiveSnapshot'),
    ...matchViewOverrides,
  };
  const roomEntry = {
    enter: jasmine.createSpy('enter').and.returnValue(true),
    ...roomEntryOverrides,
  };
  const dispatcher = new RoomEventDispatcherService(
    matchState,
    matchView,
    roomEntry,
    roomRestore
  );

  return {
    dispatcher,
    matchState,
    matchView,
    roomEntry,
    roomRestore,
  };
}
