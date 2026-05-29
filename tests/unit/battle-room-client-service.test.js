import { BattleRoomClientService } from '../../app/game/battle-room-client-service.js';

describe('BattleRoomClientService', () => {
  it('joins the lobby when the session is inactive and setup can enter waiting', () => {
    const { client, session, gameState } = battleRoomClient();

    client.joinLobby();

    expect(gameState.tryEnterWaiting).toHaveBeenCalledOnceWith();
    expect(session.connect).toHaveBeenCalledOnceWith(
      '/ws?player=Anonymous&board=%5B%5B%22_%22%5D%5D',
      jasmine.any(Object)
    );

    session.lastHandlers.onOpen();

    expect(gameState.waitingForOpponent).toHaveBeenCalledOnceWith();
  });

  it('does not join when the session is already active', () => {
    const { client, session, gameState } = battleRoomClient({
      sessionOverrides: { active: true },
    });

    client.joinLobby();

    expect(gameState.tryEnterWaiting).not.toHaveBeenCalled();
    expect(session.connect).not.toHaveBeenCalled();
  });

  it('does not join when setup cannot enter waiting', () => {
    const { client, session } = battleRoomClient({
      gameStateOverrides: {
        tryEnterWaiting: jasmine.createSpy('tryEnterWaiting').and.returnValue(false),
      },
    });

    client.joinLobby();

    expect(session.connect).not.toHaveBeenCalled();
  });

  it('restores a room with a remembered player', () => {
    const { client, session, roomSession, gameState, restoreTimeout } =
      battleRoomClient({
        roomSessionOverrides: {
          playerId: jasmine.createSpy('playerId').and.returnValue('player-1'),
        },
      });

    expect(client.restoreRoom('room-1', { timeoutMs: 123 })).toBe(true);

    expect(roomSession.playerId).toHaveBeenCalledOnceWith('room-1');
    expect(gameState.startRestore).toHaveBeenCalledOnceWith('room-1', 'player-1');
    expect(session.connect).toHaveBeenCalledOnceWith(
      '/ws?room_id=room-1&player_id=player-1',
      jasmine.any(Object)
    );
    expect(restoreTimeout.start.calls.mostRecent().args[1]).toBe(123);
  });

  it('rejects restore when the player is unknown', () => {
    const { client, session, gameState } = battleRoomClient({
      roomSessionOverrides: {
        playerId: jasmine.createSpy('playerId').and.returnValue(undefined),
      },
    });

    expect(client.restoreRoom('room-1')).toBe(false);

    expect(gameState.roomUnavailable).toHaveBeenCalledOnceWith();
    expect(session.connect).not.toHaveBeenCalled();
  });

  it('restores from the current room url when present', () => {
    const { client } = battleRoomClient({
      roomSessionOverrides: {
        currentRoomId: jasmine.createSpy('currentRoomId').and.returnValue('room-1'),
        playerId: jasmine.createSpy('playerId').and.returnValue('player-1'),
      },
    });
    spyOn(client, 'restoreRoom');

    client.restoreRoomFromCurrentUrl();

    expect(client.restoreRoom).toHaveBeenCalledOnceWith('room-1');
  });

  it('sends strikes only for open sessions and valid moves', () => {
    const { client, session, gameState } = battleRoomClient({
      sessionOverrides: { open: true },
      gameStateOverrides: {
        moveForStrike: jasmine
          .createSpy('moveForStrike')
          .and.returnValue({ row: 4, column: 5 }),
      },
    });

    client.strike({ row: 4, column: 5, state: 'empty' });

    expect(session.send).toHaveBeenCalledOnceWith({
      type: 'move',
      row: 4,
      column: 5,
    });

    session.open = false;
    client.strike({ row: 1, column: 1, state: 'empty' });

    expect(session.send).toHaveBeenCalledTimes(1);
    expect(gameState.moveForStrike).toHaveBeenCalledTimes(1);
  });

  it('does not send strikes rejected by game state', () => {
    const { client, session } = battleRoomClient({
      sessionOverrides: { open: true },
      gameStateOverrides: {
        moveForStrike: jasmine.createSpy('moveForStrike').and.returnValue(undefined),
      },
    });

    client.strike({ row: 4, column: 5, state: 'hit' });

    expect(session.send).not.toHaveBeenCalled();
  });

  it('enters rooms, persists player identity, updates url, and places ships', () => {
    const { client, roomSession, gameState, fleetLayout } = battleRoomClient({
      gameStateOverrides: {
        enterBattleRoom: jasmine
          .createSpy('enterBattleRoom')
          .and.returnValue({ shipCoordinatesById: { 0: [{ row: 1, column: 2 }] } }),
      },
    });

    client.enterRoom(
      {
        type: 'match_found',
        room_id: 'room-1',
        player_id: 'player-1',
        opponent_id: 'player-2',
        game: { id: 1 },
      },
      true
    );

    expect(gameState.enterBattleRoom).toHaveBeenCalledOnceWith({
      roomId: 'room-1',
      playerId: 'player-1',
      opponentId: 'player-2',
      game: { id: 1 },
    });
    expect(roomSession.rememberPlayer).toHaveBeenCalledOnceWith(
      'room-1',
      'player-1'
    );
    expect(roomSession.showRoom).toHaveBeenCalledOnceWith('room-1');
    expect(fleetLayout.placeFleetShips).toHaveBeenCalledOnceWith({
      0: [{ row: 1, column: 2 }],
    });
  });

  it('does not persist room entry when game state rejects it', () => {
    const { client, roomSession, fleetLayout } = battleRoomClient({
      gameStateOverrides: {
        enterBattleRoom: jasmine.createSpy('enterBattleRoom').and.returnValue(undefined),
      },
    });

    client.enterRoom({ type: 'match_found', room_id: 'room-1', player_id: 'p1' }, true);

    expect(roomSession.rememberPlayer).not.toHaveBeenCalled();
    expect(fleetLayout.placeFleetShips).not.toHaveBeenCalled();
  });

  it('handles game update messages by updating state and layout', () => {
    const { client, gameState, fleetLayout } = battleRoomClient({
      gameStateOverrides: {
        receiveGameState: jasmine
          .createSpy('receiveGameState')
          .and.returnValue({ 0: [{ row: 1, column: 2 }] }),
      },
    });

    client.handleEvent(
      { type: 'game_received', game: { id: 1 } },
      { mode: 'lobby' }
    );

    expect(gameState.receiveGameState).toHaveBeenCalledOnceWith({ id: 1 });
    expect(fleetLayout.placeFleetShips).toHaveBeenCalledOnceWith({
      0: [{ row: 1, column: 2 }],
    });
  });

  it('fails restoring rooms on restore close while reconnecting', () => {
    const { client, roomSession, gameState, session, restoreTimeout } =
      battleRoomClient({
        gameStateOverrides: {
          isRestoring: jasmine.createSpy('isRestoring').and.returnValue(true),
        },
      });

    client.handleClose({ mode: 'restore', roomId: 'room-1', playerId: 'player-1' });

    expect(roomSession.forgetPlayer).toHaveBeenCalledOnceWith('room-1');
    expect(roomSession.showHome).toHaveBeenCalledOnceWith();
    expect(gameState.returnToSetup).toHaveBeenCalledOnceWith(
      'Room unavailable'
    );
    expect(restoreTimeout.clear).toHaveBeenCalledOnceWith();
    expect(session.close).toHaveBeenCalledOnceWith();
  });

  it('disconnects waiting on non-restore close', () => {
    const { client, gameState } = battleRoomClient();

    client.handleClose({ mode: 'lobby' });

    expect(gameState.disconnectWaiting).toHaveBeenCalledOnceWith();
  });

  it('maps restore connection errors to unavailable rooms', () => {
    const { client, roomSession, gameState } = battleRoomClient();

    client.handleConnectionError({
      mode: 'restore',
      roomId: 'room-1',
      playerId: 'player-1',
    });

    expect(roomSession.forgetPlayer).toHaveBeenCalledOnceWith('room-1');
    expect(gameState.returnToSetup).toHaveBeenCalledOnceWith(
      'Room unavailable'
    );
  });

  it('maps lobby connection errors to game state errors', () => {
    const { client, gameState } = battleRoomClient();

    client.handleConnectionError({ mode: 'lobby' });

    expect(gameState.connectionError).toHaveBeenCalledOnceWith();
  });
});

function battleRoomClient({
  sessionOverrides = {},
  roomSessionOverrides = {},
  gameStateOverrides = {},
  fleetLayoutOverrides = {},
} = {}) {
  const session = {
    active: false,
    open: false,
    lastHandlers: undefined,
    isActive: jasmine.createSpy('isActive').and.callFake(() => session.active),
    isOpen: jasmine.createSpy('isOpen').and.callFake(() => session.open),
    connect: jasmine.createSpy('connect').and.callFake((_url, handlers) => {
      session.lastHandlers = handlers;
    }),
    send: jasmine.createSpy('send'),
    close: jasmine.createSpy('close'),
    ...sessionOverrides,
  };
  const roomSession = {
    currentRoomId: jasmine.createSpy('currentRoomId').and.returnValue(undefined),
    playerId: jasmine.createSpy('playerId').and.returnValue(undefined),
    rememberPlayer: jasmine.createSpy('rememberPlayer'),
    showRoom: jasmine.createSpy('showRoom'),
    forgetPlayer: jasmine.createSpy('forgetPlayer'),
    showHome: jasmine.createSpy('showHome'),
    ...roomSessionOverrides,
  };
  const gameState = {
    player: 'Anonymous',
    boardState: [['_']],
    tryEnterWaiting: jasmine.createSpy('tryEnterWaiting').and.returnValue(true),
    waitingForOpponent: jasmine.createSpy('waitingForOpponent'),
    roomUnavailable: jasmine.createSpy('roomUnavailable'),
    startRestore: jasmine.createSpy('startRestore'),
    moveForStrike: jasmine
      .createSpy('moveForStrike')
      .and.returnValue({ row: 1, column: 2 }),
    enterBattleRoom: jasmine
      .createSpy('enterBattleRoom')
      .and.returnValue({ shipCoordinatesById: undefined }),
    receiveGameState: jasmine.createSpy('receiveGameState').and.returnValue(undefined),
    opponentDisconnected: jasmine.createSpy('opponentDisconnected'),
    serverError: jasmine.createSpy('serverError'),
    isRestoring: jasmine.createSpy('isRestoring').and.returnValue(false),
    disconnectWaiting: jasmine.createSpy('disconnectWaiting'),
    connectionError: jasmine.createSpy('connectionError'),
    returnToSetup: jasmine.createSpy('returnToSetup'),
    ...gameStateOverrides,
  };
  const fleetLayout = {
    placeFleetShips: jasmine.createSpy('placeFleetShips'),
    ...fleetLayoutOverrides,
  };
  const restoreTimeout = {
    start: jasmine.createSpy('start'),
    clear: jasmine.createSpy('clear'),
  };
  const client = new BattleRoomClientService(
    session,
    roomSession,
    gameState,
    fleetLayout
  );
  client.restoreTimeout = restoreTimeout;

  return {
    client,
    session,
    roomSession,
    gameState,
    fleetLayout,
    restoreTimeout,
  };
}
