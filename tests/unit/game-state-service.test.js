import { GameStateService } from '../../app/game/game-state-service.js';
import { MapTile } from '../../app/game/constants.js';
import { shipCoordinatesFromStart } from '../../app/game/fleet-placement.js';

describe('GameStateService', () => {
  it('initializes setup state with an anonymous player', () => {
    const state = new GameStateService(roomSessionFake('Anonymous TEST'));

    expect(state.player).toBe('Anonymous TEST');
    expect(state.playerLabel).toBe('Anonymous');
    expect(state.status).toBe('Place your fleet');
    expect(state.phase).toBe('setup');
    expect(state.fleet.length).toBe(10);
    expect(state.fleetRows.length).toBe(10);
    expect(state.hitRows.length).toBe(10);
  });

  it('updates setup board through facade methods', () => {
    const state = new GameStateService(roomSessionFake());
    const coordinates = shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4);

    expect(state.canPlaceSetupShip('0', coordinates)).toBe(true);

    state.placeSetupShip('0', coordinates);

    expect(state.shipPlacements[0]).toEqual(coordinates);
    expect(state.boardState[0].slice(0, 4)).toEqual(['0', '0', '0', '0']);

    state.clearSetupShip('0');

    expect(state.shipPlacements[0]).toBeUndefined();
    expect(state.boardState[0][0]).toBe(MapTile.EMPTY);
  });

  it('locks and unlocks through battle phase transitions', () => {
    const state = new GameStateService(roomSessionFake());

    state.enterWaiting();

    expect(state.phase).toBe('waiting');
    expect(state.fleetLocked).toBe(true);

    state.returnToSetup('Back to setup');

    expect(state.phase).toBe('setup');
    expect(state.fleetLocked).toBe(false);
    expect(state.status).toBe('Back to setup');
  });

  it('stores game state until player id is known', () => {
    const state = new GameStateService(roomSessionFake());
    const game = { id: 1 };

    expect(state.receiveGameState(game)).toBeUndefined();
    expect(state.pendingGame).toBe(game);
  });

  it('enters battle rooms and projects game view state', () => {
    const state = new GameStateService(roomSessionFake());
    const game = gameStateFixture();

    const entered = state.enterBattleRoom({
      roomId: 'room-1',
      playerId: 'p1',
      opponentId: 'p2',
      game,
    });

    expect(entered?.shipCoordinatesById).toEqual({
      0: [{ row: 0, column: 0 }],
    });
    expect(state.phase).toBe('playing');
    expect(state.roomId).toBe('room-1');
    expect(state.playerId).toBe('p1');
    expect(state.opponentId).toBe('p2');
    expect(state.status).toBe('Your turn');
    expect(state.canStrike).toBe(true);
    expect(state.fleetLocked).toBe(true);
  });

  it('rejects invalid strike moves through the facade', () => {
    const state = new GameStateService(roomSessionFake());

    expect(
      state.moveForStrike({
        row: 1,
        column: 2,
        state: 'empty',
      })
    ).toBeUndefined();
  });

  it('sets simple status transitions', () => {
    const state = new GameStateService(roomSessionFake());

    state.waitingForOpponent();
    expect(state.status).toBe('Waiting for opponent...');

    state.opponentDisconnected();
    expect(state.status).toBe('Opponent disconnected');

    state.connectionError();
    expect(state.status).toBe('connection_error');

    state.serverError('bad_request');
    expect(state.status).toBe('bad_request');

    state.roomUnavailable();
    expect(state.status).toBe('Room unavailable');
  });
});

function roomSessionFake(player = 'Anonymous PLAYER') {
  return {
    anonymousPlayerName: jasmine
      .createSpy('anonymousPlayerName')
      .and.returnValue(player),
  };
}

function gameStateFixture() {
  const playerBoard = emptyBoard();
  playerBoard[0][0] = '0';
  return {
    player_one: { id: 'p1', board: playerBoard },
    player_two: { id: 'p2', board: emptyBoard() },
    current_turn: 'p1',
    turns: [],
  };
}

function emptyBoard() {
  return Array.from({ length: 10 }, () =>
    Array.from({ length: 10 }, () => MapTile.EMPTY)
  );
}
