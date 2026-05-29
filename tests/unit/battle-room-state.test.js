import {
  moveForStrike,
  returnToSetup,
  tryEnterWaiting,
} from '../../app/game/battle-room-state.js';

describe('tryEnterWaiting', () => {
  it('rejects a player without a ready board', () => {
    const state = battleState({ boardReady: false });

    expect(tryEnterWaiting(state)).toBe(false);
    expect(state.status).toBe('Place your fleet first');
    expect(state.fleetLocked).toBe(false);
  });

  it('moves a ready setup into waiting and locks the fleet', () => {
    const state = battleState({ boardReady: true });

    expect(tryEnterWaiting(state)).toBe(true);
    expect(state.phase).toBe('waiting');
    expect(state.status).toBe('Connecting...');
    expect(state.fleetLocked).toBe(true);
  });
});

describe('moveForStrike', () => {
  it('returns a move for an empty tile on the current player turn', () => {
    const state = battleState({ phase: 'playing', isMyTurn: true });

    expect(moveForStrike(state, tile({ row: 4, column: 5 }))).toEqual({
      row: 4,
      column: 5,
    });
  });

  it('rejects already resolved tiles', () => {
    const state = battleState({ phase: 'playing', isMyTurn: true });

    expect(moveForStrike(state, tile({ state: 'hit' }))).toBeUndefined();
    expect(moveForStrike(state, tile({ state: 'miss' }))).toBeUndefined();
  });

  it('rejects moves outside the current player turn', () => {
    const state = battleState({ phase: 'playing', isMyTurn: false });

    expect(moveForStrike(state, tile())).toBeUndefined();
  });
});

describe('returnToSetup', () => {
  it('clears room identity and unlocks the fleet', () => {
    const state = battleState({
      phase: 'playing',
      roomId: 'room-1',
      playerId: 'p1',
      opponentId: 'p2',
      isMyTurn: true,
      canStrike: true,
      pendingGame: { type: 'game_state' },
      fleetLocked: true,
    });

    returnToSetup(state, 'Room unavailable');

    expect(state.phase).toBe('setup');
    expect(state.roomId).toBeUndefined();
    expect(state.playerId).toBeUndefined();
    expect(state.opponentId).toBeUndefined();
    expect(state.isMyTurn).toBe(false);
    expect(state.canStrike).toBe(false);
    expect(state.pendingGame).toBeUndefined();
    expect(state.fleetLocked).toBe(false);
    expect(state.status).toBe('Room unavailable');
  });
});

function battleState(overrides = {}) {
  return {
    phase: 'setup',
    status: 'Place your fleet',
    boardReady: false,
    roomId: undefined,
    playerId: undefined,
    opponentId: undefined,
    isMyTurn: false,
    canStrike: false,
    pendingGame: undefined,
    fleetRows: [],
    hitRows: [],
    fleetLocked: false,
    setFleetLocked(locked) {
      this.fleetLocked = locked;
    },
    ...overrides,
  };
}

function tile(overrides = {}) {
  return {
    row: 0,
    column: 0,
    state: 'empty',
    boardName: 'hitboard',
    dataState: '_',
    sunk: false,
    sunkHorizontal: false,
    sunkVertical: false,
    sunkSingle: false,
    sunkStart: false,
    sunkEnd: false,
    ...overrides,
  };
}
