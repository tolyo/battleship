import { RoomClientService } from './room-client-service.js';

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
    const { client, connection, matchState, restoreTimeout } = roomClient();

    expect(
      client.restoreRoom(
        { roomId: 'room-1', playerId: 'player-1' },
        { timeoutMs: 123 }
      )
    ).toBe(true);

    expect(matchState.startRestore).toHaveBeenCalledOnceWith(
      'room-1',
      'player-1'
    );
    expect(connection.connect).toHaveBeenCalledOnceWith(
      '/ws?room_id=room-1&player_id=player-1',
      jasmine.any(Object)
    );
    expect(restoreTimeout.start.calls.mostRecent().args[1]).toBe(123);
  });

  it('does not connect when match state rejects restore', () => {
    const { client, connection, restoreTimeout } = roomClient({
      matchStateOverrides: {
        startRestore: jasmine.createSpy('startRestore').and.returnValue(false),
      },
    });

    expect(
      client.restoreRoom({ roomId: 'room-1', playerId: 'player-1' })
    ).toBe(false);

    expect(connection.connect).not.toHaveBeenCalled();
    expect(restoreTimeout.start).not.toHaveBeenCalled();
  });

  it('restores from the current room url when present', () => {
    const { client } = roomClient({
      roomStoreOverrides: {
        currentRestoreRequest: jasmine
          .createSpy('currentRestoreRequest')
          .and.returnValue({
            type: 'restore',
            roomId: 'room-1',
            playerId: 'player-1',
          }),
      },
    });
    spyOn(client, 'restoreRoom');

    client.restoreRoomFromCurrentUrl();

    expect(client.restoreRoom).toHaveBeenCalledOnceWith({
      roomId: 'room-1',
      playerId: 'player-1',
    });
  });

  it('reports unavailable rooms when the current room has no remembered player', () => {
    const { client, connection, matchState } = roomClient({
      roomStoreOverrides: {
        currentRestoreRequest: jasmine
          .createSpy('currentRestoreRequest')
          .and.returnValue({ type: 'missing_player', roomId: 'room-1' }),
      },
    });

    client.restoreRoomFromCurrentUrl();

    expect(matchState.roomUnavailable).toHaveBeenCalledOnceWith();
    expect(connection.connect).not.toHaveBeenCalled();
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
      type: 'move',
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

  it('handles room entries through room entry service', () => {
    const { client, roomEntry } = roomClient();
    const entry = {
      roomId: 'room-1',
      playerId: 'player-1',
      opponentId: 'player-2',
      view: { id: 1 },
    };

    client.handleEvent(
      {
        type: 'room_entered',
        entry,
        updateUrl: true,
      },
      { mode: 'lobby' }
    );

    expect(roomEntry.enter).toHaveBeenCalledOnceWith(entry, {
      updateUrl: true,
    });
  });

  it('handles view update messages by updating match views', () => {
    const { client, matchView } = roomClient();

    client.handleEvent(
      { type: 'state_received', view: { id: 1 } },
      { mode: 'lobby' }
    );

    expect(matchView.receiveSnapshot).toHaveBeenCalledOnceWith({ id: 1 });
  });

  it('fails restoring rooms on restore close while reconnecting', () => {
    const { client, roomStore, matchState, connection, restoreTimeout } =
      roomClient({
        matchStateOverrides: { isRestoring: true },
      });

    client.handleClose({
      mode: 'restore',
      roomId: 'room-1',
      playerId: 'player-1',
    });

    expect(roomStore.leaveRoom).toHaveBeenCalledOnceWith('room-1');
    expect(matchState.returnToSetup).toHaveBeenCalledOnceWith(
      'Room unavailable'
    );
    expect(restoreTimeout.clear).toHaveBeenCalledOnceWith();
    expect(connection.close).toHaveBeenCalledOnceWith();
  });

  it('disconnects waiting on non-restore close', () => {
    const { client, matchState } = roomClient();

    client.handleClose({ mode: 'lobby' });

    expect(matchState.connectionClosed).toHaveBeenCalledOnceWith();
  });

  it('maps restore connection errors to unavailable rooms', () => {
    const { client, roomStore, matchState } = roomClient();

    client.handleConnectionError({
      mode: 'restore',
      roomId: 'room-1',
      playerId: 'player-1',
    });

    expect(roomStore.leaveRoom).toHaveBeenCalledOnceWith('room-1');
    expect(matchState.returnToSetup).toHaveBeenCalledOnceWith(
      'Room unavailable'
    );
  });

  it('maps lobby connection errors to match state errors', () => {
    const { client, matchState } = roomClient();

    client.handleConnectionError({ mode: 'lobby' });

    expect(matchState.connectionError).toHaveBeenCalledOnceWith();
  });
});

function roomClient({
  connectionOverrides = {},
  roomStoreOverrides = {},
  matchStateOverrides = {},
  targetOverrides = {},
  matchViewOverrides = {},
  roomEntryOverrides = {},
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
  const roomStore = {
    currentRestoreRequest: jasmine
      .createSpy('currentRestoreRequest')
      .and.returnValue({ type: 'none' }),
    enterRoom: jasmine.createSpy('enterRoom'),
    leaveRoom: jasmine.createSpy('leaveRoom'),
    ...roomStoreOverrides,
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
  const matchView = {
    receiveSnapshot: jasmine.createSpy('receiveSnapshot'),
    ...matchViewOverrides,
  };
  const roomEntry = {
    enter: jasmine.createSpy('enter').and.returnValue(true),
    ...roomEntryOverrides,
  };
  const restoreTimeout = {
    start: jasmine.createSpy('start'),
    clear: jasmine.createSpy('clear'),
  };
  const client = new RoomClientService(
    connection,
    roomStore,
    matchState,
    target,
    matchView,
    roomEntry
  );
  client.restoreTimeout = restoreTimeout;

  return {
    client,
    connection,
    roomStore,
    matchState,
    target,
    matchView,
    roomEntry,
    restoreTimeout,
  };
}
