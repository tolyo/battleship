import {
  moveForTarget,
  roomSessionConfig,
  RoomStateEvent,
} from './room-state.js';

describe('roomSessionConfig', () => {
  it('keeps setup mode and reports incomplete setup before joining', () => {
    const session = testMachine(roomSessionConfig());

    expect(session.send(RoomStateEvent.SETUP_INCOMPLETE)).toBe(true);
    expect(session.current).toBe('setup');
    expect(session.data.status).toBe('Place your fleet first');
    expect(session.data.unitsLocked).toBe(false);
  });

  it('moves a setup session into connecting and locks units', () => {
    const session = testMachine(roomSessionConfig());

    expect(session.send(RoomStateEvent.JOIN_REQUESTED)).toBe(true);
    expect(session.current).toBe('connecting');
    expect(session.data.status).toBe('Connecting...');
    expect(session.data.unitsLocked).toBe(true);
  });

  it('moves an open lobby socket into waiting', () => {
    const session = testMachine(roomSessionConfig());

    session.send(RoomStateEvent.JOIN_REQUESTED);
    session.send(RoomStateEvent.SOCKET_OPENED);

    expect(session.current).toBe('waiting');
    expect(session.data.status).toBe('Waiting for opponent...');
  });

  it('tracks restoring as its own frontend mode', () => {
    const session = testMachine(roomSessionConfig());

    session.send(RoomStateEvent.RESTORE_STARTED, {
      roomId: 'room-1',
      playerId: 'player-1',
    });

    expect(session.current).toBe('restoring');
    expect(session.data.roomId).toBe('room-1');
    expect(session.data.playerId).toBe('player-1');
    expect(session.data.unitsLocked).toBe(true);
    expect(session.data.status).toBe('Reconnecting...');
  });

  it('clears room identity and unlocks units when returning to setup', () => {
    const session = testMachine(roomSessionConfig());

    session.send(RoomStateEvent.RESTORE_STARTED, {
      roomId: 'room-1',
      playerId: 'player-1',
    });
    session.data.opponentId = 'player-2';
    session.data.isMyTurn = true;
    session.data.canSubmitMove = true;
    session.data.pendingView = { type: 'room_state' };

    session.send(RoomStateEvent.RETURN_TO_SETUP, 'Room unavailable');

    expect(session.current).toBe('setup');
    expect(session.data.roomId).toBeUndefined();
    expect(session.data.playerId).toBeUndefined();
    expect(session.data.opponentId).toBeUndefined();
    expect(session.data.isMyTurn).toBe(false);
    expect(session.data.canSubmitMove).toBe(false);
    expect(session.data.pendingView).toBeUndefined();
    expect(session.data.unitsLocked).toBe(false);
    expect(session.data.status).toBe('Room unavailable');
  });
});

describe('moveForTarget', () => {
  it('returns a move for an empty tile when the server allows moves', () => {
    expect(moveForTarget('playing', true, tile({ row: 4, column: 5 }))).toEqual({
      row: 4,
      column: 5,
    });
  });

  it('rejects already resolved tiles', () => {
    expect(moveForTarget('playing', true, tile({ state: 'hit' }))).toBeUndefined();
    expect(
      moveForTarget('playing', true, tile({ state: 'miss' }))
    ).toBeUndefined();
  });

  it('rejects moves when the server does not allow moves', () => {
    expect(moveForTarget('playing', false, tile())).toBeUndefined();
  });
});

/**
 * @param {ng.MachineConfig} config
 * @returns {ng.Machine}
 */
function testMachine(config) {
  return {
    current: config.initial,
    data: config.data,
    send(type, payload) {
      const transition = config.transitions[this.current]?.[type];

      if (!transition) {
        return false;
      }

      const nextMode = transition(this.data, payload, this);
      if (typeof nextMode === 'string' && nextMode) {
        this.current = nextMode;
      }

      return true;
    },
    can(type) {
      return !!config.transitions[this.current]?.[type];
    },
    matches(mode) {
      return this.current === mode;
    },
    snapshot() {
      return structuredClone({
        current: this.current,
        data: this.data,
      });
    },
    restore(snapshot) {
      this.current = snapshot.current;
      this.data = structuredClone(snapshot.data);
    },
  };
}

function tile(overrides = {}) {
  return {
    row: 0,
    column: 0,
    state: 'empty',
    boardName: 'target-board',
    dataState: '_',
    sunk: false,
    sunkHorizontal: false,
    sunkVertical: false,
    sunkSingle: false,
    sunkStart: false,
    sunkEnd: false,
    classes: {},
    ...overrides,
  };
}
