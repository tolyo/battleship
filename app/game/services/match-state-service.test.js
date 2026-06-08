import { MatchStateService } from './match-state-service.js';

describe('MatchStateService', () => {
  it('initializes setup state with an anonymous player', () => {
    const state = matchState('Anonymous TEST');

    expect(state.player).toBe('Anonymous TEST');
    expect(state.lobbyEntry).toEqual({
      player: 'Anonymous TEST',
      boardState: state.boardState,
    });
    expect(state.playerLabel).toBe('Anonymous');
    expect(state.status).toBe('Place your fleet');
    expect(state.phase).toBe('setup');
    expect(state.isSetup).toBe(true);
    expect(state.isPlaying).toBe(false);
    expect(state.units.length).toBe(10);
    expect(state.ownRows.length).toBe(10);
    expect(state.targetRows.length).toBe(10);
  });

  it('locks and unlocks through room phase transitions', () => {
    const state = matchState();
    state.data.boardReady = true;

    state.tryEnterWaiting();

    expect(state.phase).toBe('connecting');
    expect(state.unitsLocked).toBe(true);

    state.returnToSetup('Back to setup');

    expect(state.phase).toBe('setup');
    expect(state.unitsLocked).toBe(false);
    expect(state.status).toBe('Back to setup');
  });

  it('keeps setup active and reports incomplete setup before joining', () => {
    const state = matchState();

    expect(state.tryEnterWaiting()).toBe(false);

    expect(state.phase).toBe('setup');
    expect(state.unitsLocked).toBe(false);
    expect(state.status).toBe('Place your fleet first');
  });

  it('enters active rooms and applies match view state', () => {
    const state = matchState();

    expect(state.enterRoom({
      roomId: 'room-1',
      playerId: 'p1',
      opponentId: 'p2',
    })).toBe(true);
    expect(state.applyViewModel(matchViewModelFixture())).toBe(true);

    expect(state.unitPlacements).toEqual({
      0: [{ row: 0, column: 0 }],
    });
    expect(state.phase).toBe('playing');
    expect(state.isSetup).toBe(false);
    expect(state.isPlaying).toBe(true);
    expect(state.roomId).toBe('room-1');
    expect(state.playerId).toBe('p1');
    expect(state.opponentId).toBe('p2');
    expect(state.status).toBe('Your turn');
    expect(state.canSubmitMove).toBe(true);
    expect(state.unitsLocked).toBe(true);
  });

  it('sets simple status transitions', () => {
    const state = matchState();
    state.data.boardReady = true;
    state.tryEnterWaiting();

    state.socketOpened();
    expect(state.status).toBe('Waiting for opponent...');

    state.enterRoom({
      roomId: 'room-1',
      playerId: 'p1',
      opponentId: 'p2',
    });

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

function matchState(player) {
  return new MatchStateService(roomStoreFake(player), machineServiceFake);
}

function roomStoreFake(player = 'Anonymous PLAYER') {
  return {
    anonymousPlayerName: jasmine
      .createSpy('anonymousPlayerName')
      .and.returnValue(player),
  };
}

/**
 * @param {ng.MachineConfig} config
 * @returns {ng.Machine}
 */
function machineServiceFake(config) {
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

function matchViewModelFixture() {
  return {
    isMyTurn: true,
    status: 'Your turn',
    ownTiles: [
      {
        row: 0,
        column: 0,
        state: 'unit',
        unitId: '0',
      },
    ],
    targetTiles: [],
    unitCoordinatesById: {
      0: [{ row: 0, column: 0 }],
    },
    sunkClusters: [],
  };
}
