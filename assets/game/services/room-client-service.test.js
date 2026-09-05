import { RoomClientService } from './room-client-service.js';
import {
  ROOM_EVENT,
  CLIENT_MESSAGE,
  ROOM_SERVER_MESSAGE,
} from '../protocol/room-protocol.js';
import { ROOM_UNAVAILABLE_STATUS } from '../domain/room-state.js';
import { CONNECTION_MODE } from './connection-context.js';

describe('RoomClientService', () => {
  it('joins the lobby when the connection is inactive and setup can enter waiting', () => {
    const { client, connection, matchState } = roomClient();

    client.joinLobby();

    expect(matchState.tryEnterWaiting).toHaveBeenCalledOnceWith();
    expect(connection.connect).toHaveBeenCalledOnceWith(
      '/ws?player=Anonymous&board=%5B%5B%22_%22%5D%5D',
      jasmine.any(Object)
    );

    connection.lastHandlers.onOpen();

    expect(matchState.socketOpened).toHaveBeenCalledOnceWith();
  });

  it('does not join when the connection is already active', () => {
    const { client, connection, matchState } = roomClient({
      connectionOverrides: { active: true },
    });

    client.joinLobby();

    expect(matchState.tryEnterWaiting).not.toHaveBeenCalled();
    expect(connection.connect).not.toHaveBeenCalled();
  });

  it('does not join when setup cannot enter waiting', () => {
    const { client, connection } = roomClient({
      matchStateOverrides: {
        tryEnterWaiting: jasmine
          .createSpy('tryEnterWaiting')
          .and.returnValue(false),
      },
    });

    client.joinLobby();

    expect(connection.connect).not.toHaveBeenCalled();
  });

  it('restores a room with a remembered player', () => {
    const { client, connection, roomRestore } = roomClient();

    expect(
      client.restoreRoom(
        { roomId: 'room-1', playerId: 'player-1' },
        { timeoutMs: 123 }
      )
    ).toBe(true);

    expect(roomRestore.startRestore).toHaveBeenCalledOnceWith(
      {
        roomId: 'room-1',
        playerId: 'player-1',
      },
      { timeoutMs: 123 }
    );
    expect(connection.connect).toHaveBeenCalledOnceWith(
      '/ws?room_id=room-1&player_id=player-1',
      jasmine.any(Object)
    );
  });

  it('does not connect when match state rejects restore', () => {
    const { client, connection, roomRestore } = roomClient({
      roomRestoreOverrides: {
        startRestore: jasmine.createSpy('startRestore').and.returnValue(false),
      },
    });

    expect(client.restoreRoom({ roomId: 'room-1', playerId: 'player-1' })).toBe(
      false
    );

    expect(connection.connect).not.toHaveBeenCalled();
    expect(roomRestore.startRestore).toHaveBeenCalledOnceWith(
      { roomId: 'room-1', playerId: 'player-1' },
      {}
    );
  });

  it('restores from the current room url when present', () => {
    const { client, roomRestore } = roomClient();

    expect(client.restoreRoomFromCurrentUrl()).toBeTrue();

    expect(roomRestore.restoreCurrentRequest).toHaveBeenCalledOnceWith();
  });

  it('reports unavailable rooms when the current room has no remembered player', () => {
    const { client, roomRestore } = roomClient({
      roomRestoreOverrides: {
        restoreCurrentRequest: jasmine
          .createSpy('restoreCurrentRequest')
          .and.returnValue(false),
      },
    });

    expect(client.restoreRoomFromCurrentUrl()).toBeFalse();

    expect(roomRestore.restoreCurrentRequest).toHaveBeenCalledOnceWith();
  });

  it('sends target moves only for open connections and valid moves', () => {
    const { client, connection, target } = roomClient({
      connectionOverrides: { open: true },
      targetOverrides: {
        moveForTile: jasmine
          .createSpy('moveForTile')
          .and.returnValue({ row: 4, column: 5 }),
      },
    });

    client.submitMove({ row: 4, column: 5, state: 'empty' });

    expect(connection.send).toHaveBeenCalledOnceWith({
      type: CLIENT_MESSAGE.MOVE,
      row: 4,
      column: 5,
    });

    connection.open = false;
    client.submitMove({ row: 1, column: 1, state: 'empty' });

    expect(connection.send).toHaveBeenCalledTimes(1);
    expect(target.moveForTile).toHaveBeenCalledTimes(1);
  });

  it('does not send target moves rejected by target service', () => {
    const { client, connection } = roomClient({
      connectionOverrides: { open: true },
      targetOverrides: {
        moveForTile: jasmine
          .createSpy('moveForTile')
          .and.returnValue(undefined),
      },
    });

    client.submitMove({ row: 4, column: 5, state: 'hit' });

    expect(connection.send).not.toHaveBeenCalled();
  });

  it('forwards socket messages to room event dispatcher', () => {
    const { client, roomEventDispatcher, connection } = roomClient({
      connectionOverrides: { open: true },
    });

    client.joinLobby();

    connection.lastHandlers.onMessage({
      type: ROOM_SERVER_MESSAGE.LOBBY_WAITING,
    });

    expect(roomEventDispatcher.dispatch).toHaveBeenCalledOnceWith(
      { type: ROOM_EVENT.WAITING },
      { mode: CONNECTION_MODE.LOBBY }
    );
  });

  it('delegates close lifecycle events to room lifecycle service', () => {
    const { client, roomConnectionLifecycle, connection } = roomClient();

    client.connect({
      mode: CONNECTION_MODE.RESTORE,
      roomId: 'room-1',
      playerId: 'player-1',
    });

    connection.lastHandlers.onClose();

    expect(roomConnectionLifecycle.onClose).toHaveBeenCalledOnceWith(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      jasmine.any(Function)
    );
  });

  it('delegates error lifecycle events to room lifecycle service', () => {
    const { client, roomConnectionLifecycle, connection } = roomClient();

    client.connect({ mode: CONNECTION_MODE.LOBBY });

    connection.lastHandlers.onError();

    expect(roomConnectionLifecycle.onError).toHaveBeenCalledOnceWith(
      { mode: CONNECTION_MODE.LOBBY },
      jasmine.any(Function)
    );
  });

  it('closes transport through lifecycle completion callbacks', () => {
    const { client, roomConnectionLifecycle, connection } = roomClient();

    client.connect({ mode: CONNECTION_MODE.LOBBY });
    connection.lastHandlers.onClose();
    roomConnectionLifecycle.onClose.calls.mostRecent().args[1]();
    connection.lastHandlers.onError();
    roomConnectionLifecycle.onError.calls.mostRecent().args[1]();

    expect(connection.close).toHaveBeenCalledTimes(2);
  });

  it('forwards close lifecycle to room restore timeout cleanup and transport', () => {
    const { client, roomRestore, connection } = roomClient();

    client.close();

    expect(roomRestore.clearRestoreTimeout).toHaveBeenCalledOnceWith();
    expect(connection.close).toHaveBeenCalledOnceWith();
  });
});

function roomClient({
  connectionOverrides = {},
  matchStateOverrides = {},
  targetOverrides = {},
  roomRestoreOverrides = {},
  roomEventDispatcherOverrides = {},
  roomConnectionLifecycleOverrides = {},
} = {}) {
  const connection = {
    active: false,
    open: false,
    lastHandlers: undefined,
    isActive: jasmine
      .createSpy('isActive')
      .and.callFake(() => connection.active),
    isOpen: jasmine.createSpy('isOpen').and.callFake(() => connection.open),
    connect: jasmine.createSpy('connect').and.callFake((_url, handlers) => {
      connection.lastHandlers = handlers;
    }),
    send: jasmine.createSpy('send'),
    close: jasmine.createSpy('close'),
    ...connectionOverrides,
  };
  const matchState = {
    lobbyEntry: {
      player: 'Anonymous',
      boardState: [['_']],
    },
    tryEnterWaiting: jasmine.createSpy('tryEnterWaiting').and.returnValue(true),
    isRestoring: false,
    socketOpened: jasmine.createSpy('socketOpened'),
    roomUnavailable: jasmine.createSpy('roomUnavailable'),
    startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    enterRoom: jasmine.createSpy('enterRoom').and.returnValue(true),
    opponentDisconnected: jasmine.createSpy('opponentDisconnected'),
    serverError: jasmine.createSpy('serverError'),
    connectionClosed: jasmine.createSpy('connectionClosed'),
    connectionError: jasmine.createSpy('connectionError'),
    returnToSetup: jasmine.createSpy('returnToSetup'),
    ...matchStateOverrides,
  };
  const target = {
    moveForTile: jasmine
      .createSpy('moveForTile')
      .and.returnValue({ row: 1, column: 2 }),
    ...targetOverrides,
  };
  const roomRestore = {
    restoreCurrentRequest: jasmine
      .createSpy('restoreCurrentRequest')
      .and.callFake(() =>
        roomRestore.startRestore({ roomId: 'room-1', playerId: 'player-1' }, {})
      ),
    startRestore: jasmine
      .createSpy('startRestore')
      .and.callFake((_entry, options) =>
        matchState.startRestore(_entry.roomId, _entry.playerId, options)
      ),
    failRestore: jasmine.createSpy('failRestore'),
    handleConnectionDisconnect: jasmine
      .createSpy('handleConnectionDisconnect')
      .and.callFake((context, reason = ROOM_UNAVAILABLE_STATUS) => {
        if (
          context.mode !== CONNECTION_MODE.RESTORE ||
          !matchState.isRestoring
        ) {
          return false;
        }

        roomRestore.failRestore(context.roomId, reason);
        return true;
      }),
    clearRestoreTimeout: jasmine.createSpy('clearRestoreTimeout'),
    ...roomRestoreOverrides,
  };
  const roomEventDispatcher = {
    dispatch: jasmine.createSpy('dispatch'),
    ...roomEventDispatcherOverrides,
  };
  const roomConnectionLifecycle = {
    onClose: jasmine.createSpy('onClose'),
    onError: jasmine.createSpy('onError'),
    ...roomConnectionLifecycleOverrides,
  };
  const client = new RoomClientService(
    connection,
    matchState,
    target,
    roomRestore,
    roomEventDispatcher,
    roomConnectionLifecycle
  );

  return {
    client,
    connection,
    matchState,
    target,
    roomRestore,
    roomEventDispatcher,
    roomConnectionLifecycle,
  };
}
