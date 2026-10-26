import { ROOM_UNAVAILABLE_STATUS } from '../../../assets/game/domain/room-state.js';
import { create } from '../../../_build/browser/js/modules/battleship/client/RoomEvents.js';
import { ROOM_EVENT } from '../../../assets/game/protocol/room-protocol.js';
import {
  lobby,
  restore,
  is_restore as isRestoreContext,
} from '../../../_build/browser/js/modules/battleship/browser/ConnectionContext.js';

describe('RoomEventDispatcherService', () => {
  it('opens lobby socket on waiting event', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch({ type: ROOM_EVENT.WAITING }, lobby());

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
      lobby()
    );

    expect(roomEntry.enter).toHaveBeenCalledOnceWith(entry, {
      updateUrl: true,
    });
  });

  it('forwards state snapshots to match view service', () => {
    const { dispatcher, matchView } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.STATE_RECEIVED, view: { id: 1 } },
      lobby()
    );

    expect(matchView.receiveSnapshot).toHaveBeenCalledOnceWith({ id: 1 });
  });

  it('preserves missing and null snapshot payloads', () => {
    const { dispatcher, matchView } = roomEventDispatcher();
    dispatcher.dispatch({ type: ROOM_EVENT.STATE_RECEIVED }, lobby());
    dispatcher.dispatch(
      { type: ROOM_EVENT.STATE_RECEIVED, view: null },
      lobby()
    );
    expect(matchView.receiveSnapshot.calls.allArgs()).toEqual([
      [undefined],
      [null],
    ]);
  });

  it('ignores room entry events without identity and defaults error reasons', () => {
    const { dispatcher, roomEntry, matchState } = roomEventDispatcher();
    dispatcher.dispatch({ type: ROOM_EVENT.ROOM_ENTERED }, lobby());
    dispatcher.dispatch({ type: ROOM_EVENT.SERVER_ERROR }, lobby());
    dispatcher.dispatch({ type: ROOM_EVENT.SERVER_ERROR, reason: '' }, lobby());
    expect(roomEntry.enter).not.toHaveBeenCalled();
    expect(matchState.serverError.calls.allArgs()).toEqual([
      ['server_error'],
      [''],
    ]);
  });

  it('marks opponent left in match state', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch({ type: ROOM_EVENT.OPPONENT_LEFT }, lobby());

    expect(matchState.opponentDisconnected).toHaveBeenCalledOnceWith();
  });

  it('maps server errors to match state server errors', () => {
    const { dispatcher, matchState } = roomEventDispatcher();

    dispatcher.dispatch(
      { type: ROOM_EVENT.SERVER_ERROR, reason: 'something' },
      lobby()
    );

    expect(matchState.serverError).toHaveBeenCalledOnceWith('something');
  });

  it('maps unavailable room events to restore flow during restore mode', () => {
    const { dispatcher, roomRestore } = roomEventDispatcher({
      matchStateOverrides: { isRestoring: true },
    });

    dispatcher.dispatch(
      { type: ROOM_EVENT.ROOM_UNAVAILABLE },
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );
  });

  it('maps unavailable room events to match state when restore does not handle it', () => {
    const { dispatcher, roomRestore, matchState } = roomEventDispatcher({
      matchStateOverrides: { isRestoring: false },
    });

    dispatcher.dispatch(
      { type: ROOM_EVENT.ROOM_UNAVAILABLE },
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      restore({ roomId: 'room-1', playerId: 'player-1' })
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
        if (!isRestoreContext(context) || !matchState.isRestoring) {
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
  matchView.receiveOptionalSnapshot = (value) =>
    matchView.receiveSnapshot(value === 'none' ? undefined : value[1]);
  const roomEntry = {
    enter: jasmine.createSpy('enter').and.returnValue(true),
    ...roomEntryOverrides,
  };
  const dispatcher = create(matchState, matchView, roomEntry, roomRestore);

  return {
    dispatcher,
    matchState,
    matchView,
    roomEntry,
    roomRestore,
  };
}
