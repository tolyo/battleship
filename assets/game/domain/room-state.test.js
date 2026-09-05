import {
  moveForTarget,
  roomSessionConfig,
  RoomStateEvent,
  ROOM_UNAVAILABLE_STATUS,
  SessionPhase,
} from './room-state.js';
import { SessionStatus } from './constants.js';

describe('roomSessionConfig', () => {
  it('keeps setup mode and reports incomplete setup before joining', () => {
    const session = testMachine(roomSessionConfig());

    expect(session.send(RoomStateEvent.SETUP_INCOMPLETE)).toBe(true);
    expect(session.current).toBe(SessionPhase.SETUP);
    expect(session.data.status).toBe(SessionStatus.PLACE_FLEET_FIRST);
    expect(session.data.unitsLocked).toBe(false);
  });

  it('moves a setup session into connecting and locks units', () => {
    const session = testMachine(roomSessionConfig());

    expect(session.send(RoomStateEvent.JOIN_REQUESTED)).toBe(true);
    expect(session.current).toBe(SessionPhase.CONNECTING);
    expect(session.data.status).toBe(SessionStatus.CONNECTING);
    expect(session.data.unitsLocked).toBe(true);
  });

  it('moves an open lobby socket into waiting', () => {
    const session = testMachine(roomSessionConfig());

    session.send(RoomStateEvent.JOIN_REQUESTED);
    session.send(RoomStateEvent.SOCKET_OPENED);

    expect(session.current).toBe(SessionPhase.WAITING);
    expect(session.data.status).toBe(SessionStatus.WAITING_FOR_OPPONENT);
  });

  it('tracks restoring as its own frontend mode', () => {
    const session = testMachine(roomSessionConfig());

    session.send(RoomStateEvent.RESTORE_STARTED, {
      roomId: 'room-1',
      playerId: 'player-1',
    });

    expect(session.current).toBe(SessionPhase.RESTORING);
    expect(session.data.roomId).toBe('room-1');
    expect(session.data.playerId).toBe('player-1');
    expect(session.data.unitsLocked).toBe(true);
    expect(session.data.status).toBe(SessionStatus.RECONNECTING);
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

    session.send(RoomStateEvent.RETURN_TO_SETUP, ROOM_UNAVAILABLE_STATUS);

    expect(session.current).toBe(SessionPhase.SETUP);
    expect(session.data.roomId).toBeUndefined();
    expect(session.data.playerId).toBeUndefined();
    expect(session.data.opponentId).toBeUndefined();
    expect(session.data.isMyTurn).toBe(false);
    expect(session.data.canSubmitMove).toBe(false);
    expect(session.data.pendingView).toBeUndefined();
    expect(session.data.unitsLocked).toBe(false);
    expect(session.data.status).toBe(ROOM_UNAVAILABLE_STATUS);
  });

  it('executes every declared transition with its boundary payload', () => {
    const config = roomSessionConfig();

    Object.entries(config.transitions).forEach(([phase, transitions]) => {
      Object.entries(transitions).forEach(([event, transition]) => {
        const { data } = roomSessionConfig();
        const nextPhase = transition(data, transitionPayload(event));

        expect(nextPhase).toBe(expectedNextPhase(phase, event));
        expect(data.status).toBe(expectedStatus(event));
      });
    });
  });
});

describe('moveForTarget', () => {
  it('returns a move for an empty tile when the server allows moves', () => {
    expect(
      moveForTarget(SessionPhase.PLAYING, true, tile({ row: 4, column: 5 }))
    ).toEqual({
      row: 4,
      column: 5,
    });
  });

  it('rejects already resolved tiles', () => {
    expect(
      moveForTarget(SessionPhase.PLAYING, true, tile({ state: 'hit' }))
    ).toBeUndefined();
    expect(
      moveForTarget(SessionPhase.PLAYING, true, tile({ state: 'miss' }))
    ).toBeUndefined();
  });

  it('rejects moves when the server does not allow moves', () => {
    expect(moveForTarget(SessionPhase.PLAYING, false, tile())).toBeUndefined();
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

function transitionPayload(event) {
  if (event === RoomStateEvent.ROOM_ENTERED) {
    return {
      roomId: 'room-1',
      playerId: 'player-1',
      opponentId: 'player-2',
    };
  }

  if (event === RoomStateEvent.RESTORE_STARTED) {
    return { roomId: 'room-1', playerId: 'player-1' };
  }

  if (event === RoomStateEvent.STATE_RECEIVED) {
    return {
      viewModel: {
        isMyTurn: true,
        status: 'Your turn',
        ownTiles: [],
        targetTiles: [],
        unitCoordinatesById: {},
        sunkClusters: [],
      },
    };
  }

  if (event === RoomStateEvent.RETURN_TO_SETUP) {
    return 'Back to setup';
  }

  if (event === RoomStateEvent.SERVER_ERROR) {
    return 'server_error';
  }

  return undefined;
}

function expectedStatus(event) {
  return {
    [RoomStateEvent.SETUP_INCOMPLETE]: SessionStatus.PLACE_FLEET_FIRST,
    [RoomStateEvent.JOIN_REQUESTED]: SessionStatus.CONNECTING,
    [RoomStateEvent.SOCKET_OPENED]: SessionStatus.WAITING_FOR_OPPONENT,
    [RoomStateEvent.CONNECTION_CLOSED]: SessionStatus.DISCONNECTED,
    [RoomStateEvent.RETURN_TO_SETUP]: 'Back to setup',
    [RoomStateEvent.ROOM_ENTERED]: SessionStatus.IN_ROOM,
    [RoomStateEvent.RESTORE_STARTED]: SessionStatus.RECONNECTING,
    [RoomStateEvent.STATE_RECEIVED]: 'Your turn',
    [RoomStateEvent.ROOM_UNAVAILABLE]: SessionStatus.ROOM_UNAVAILABLE,
    [RoomStateEvent.OPPONENT_DISCONNECTED]: SessionStatus.OPPONENT_DISCONNECTED,
    [RoomStateEvent.CONNECTION_ERROR]: SessionStatus.CONNECTION_ERROR,
    [RoomStateEvent.SERVER_ERROR]: 'server_error',
  }[event];
}

function expectedNextPhase(phase, event) {
  return {
    [`${SessionPhase.SETUP}:${RoomStateEvent.JOIN_REQUESTED}`]:
      SessionPhase.CONNECTING,
    [`${SessionPhase.SETUP}:${RoomStateEvent.ROOM_ENTERED}`]:
      SessionPhase.PLAYING,
    [`${SessionPhase.SETUP}:${RoomStateEvent.RESTORE_STARTED}`]:
      SessionPhase.RESTORING,
    [`${SessionPhase.CONNECTING}:${RoomStateEvent.SOCKET_OPENED}`]:
      SessionPhase.WAITING,
    [`${SessionPhase.CONNECTING}:${RoomStateEvent.CONNECTION_CLOSED}`]:
      SessionPhase.SETUP,
    [`${SessionPhase.CONNECTING}:${RoomStateEvent.ROOM_ENTERED}`]:
      SessionPhase.PLAYING,
    [`${SessionPhase.CONNECTING}:${RoomStateEvent.RETURN_TO_SETUP}`]:
      SessionPhase.SETUP,
    [`${SessionPhase.WAITING}:${RoomStateEvent.CONNECTION_CLOSED}`]:
      SessionPhase.SETUP,
    [`${SessionPhase.WAITING}:${RoomStateEvent.ROOM_ENTERED}`]:
      SessionPhase.PLAYING,
    [`${SessionPhase.WAITING}:${RoomStateEvent.RETURN_TO_SETUP}`]:
      SessionPhase.SETUP,
    [`${SessionPhase.RESTORING}:${RoomStateEvent.ROOM_ENTERED}`]:
      SessionPhase.PLAYING,
    [`${SessionPhase.RESTORING}:${RoomStateEvent.STATE_RECEIVED}`]:
      SessionPhase.PLAYING,
    [`${SessionPhase.RESTORING}:${RoomStateEvent.RETURN_TO_SETUP}`]:
      SessionPhase.SETUP,
    [`${SessionPhase.PLAYING}:${RoomStateEvent.RETURN_TO_SETUP}`]:
      SessionPhase.SETUP,
  }[`${phase}:${event}`];
}
