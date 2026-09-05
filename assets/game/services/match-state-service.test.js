import { MatchStateService } from './match-state-service.js';
import { SessionStatus } from '../domain/constants.js';
import { SessionPhase } from '../domain/room-state.js';

describe('MatchStateService', () => {
  it('initializes setup state with an anonymous player', () => {
    const state = matchState('Anonymous TEST');

    expect(state.player).toBe('Anonymous TEST');
    expect(state.lobbyEntry).toEqual({
      player: 'Anonymous TEST',
      boardState: state.boardState,
    });
    expect(state.playerLabel).toBe('Anonymous');
    expect(state.status).toBe(SessionStatus.PLACE_FLEET);
    expect(state.phase).toBe(SessionPhase.SETUP);
    expect(state.isSetup).toBe(true);
    expect(state.isPlaying).toBe(false);
    expect(state.targetVisible).toBe(false);
    expect(state.targetDisabled).toBe(true);
    expect(state.units.length).toBe(10);
    expect(state.ownRows.length).toBe(10);
    expect(state.targetRows.length).toBe(10);
  });

  it('locks and unlocks through room phase transitions', () => {
    const state = matchState();
    state.data.boardReady = true;

    state.tryEnterWaiting();

    expect(state.phase).toBe(SessionPhase.CONNECTING);
    expect(state.unitsLocked).toBe(true);

    state.returnToSetup('Back to setup');

    expect(state.phase).toBe(SessionPhase.SETUP);
    expect(state.unitsLocked).toBe(false);
    expect(state.status).toBe('Back to setup');
  });

  it('keeps setup active and reports incomplete setup before joining', () => {
    const state = matchState();

    expect(state.tryEnterWaiting()).toBe(false);

    expect(state.phase).toBe(SessionPhase.SETUP);
    expect(state.unitsLocked).toBe(false);
    expect(state.status).toBe(SessionStatus.PLACE_FLEET_FIRST);
  });

  it('refreshes Angular bindings after room state transitions', () => {
    const $rootScope = rootScopeFake();
    const state = matchState('Anonymous PLAYER', $rootScope);
    state.data.boardReady = true;

    state.tryEnterWaiting();

    expect($rootScope.$handler._checkListenersForAllKeys).toHaveBeenCalledWith(
      state
    );
    expect($rootScope.$handler._checkListenersForAllKeys).toHaveBeenCalledWith(
      state.data
    );
    expect($rootScope.$handler._flushScheduledTasks).toHaveBeenCalledOnceWith();
  });

  it('syncs the target board disabled class after match view changes', () => {
    const targetBoard = {
      classList: {
        toggle: jasmine.createSpy('toggle'),
      },
    };
    const originalDocument = globalThis.document;
    globalThis.document = {
      querySelector: jasmine
        .createSpy('querySelector')
        .and.returnValue(targetBoard),
    };

    try {
      const state = matchState();
      state.enterRoom({
        roomId: 'room-1',
        playerId: 'p1',
        opponentId: 'p2',
      });

      state.applyViewModel(matchViewModelFixture());

      expect(globalThis.document.querySelector).toHaveBeenCalledWith(
        '#target-board'
      );
      expect(targetBoard.classList.toggle).toHaveBeenCalledWith(
        'disabled',
        false
      );
    } finally {
      globalThis.document = originalDocument;
    }
  });

  it('enters active rooms and applies match view state', () => {
    const state = matchState();

    expect(
      state.enterRoom({
        roomId: 'room-1',
        playerId: 'p1',
        opponentId: 'p2',
      })
    ).toBe(true);
    expect(state.applyViewModel(matchViewModelFixture())).toBe(true);

    expect(state.unitPlacements).toEqual({
      0: [{ row: 0, column: 0 }],
    });
    expect(state.phase).toBe(SessionPhase.PLAYING);
    expect(state.isSetup).toBe(false);
    expect(state.isPlaying).toBe(true);
    expect(state.roomId).toBe('room-1');
    expect(state.playerId).toBe('p1');
    expect(state.opponentId).toBe('p2');
    expect(state.status).toBe('Your turn');
    expect(state.canSubmitMove).toBe(true);
    expect(state.targetVisible).toBe(true);
    expect(state.targetDisabled).toBe(false);
    expect(state.unitsLocked).toBe(true);
  });

  it('sets simple status transitions', () => {
    const state = matchState();
    state.data.boardReady = true;
    state.tryEnterWaiting();

    state.socketOpened();
    expect(state.status).toBe(SessionStatus.WAITING_FOR_OPPONENT);

    state.enterRoom({
      roomId: 'room-1',
      playerId: 'p1',
      opponentId: 'p2',
    });

    state.opponentDisconnected();
    expect(state.status).toBe(SessionStatus.OPPONENT_DISCONNECTED);

    state.connectionError();
    expect(state.status).toBe(SessionStatus.CONNECTION_ERROR);

    state.serverError('bad_request');
    expect(state.status).toBe('bad_request');

    state.roomUnavailable();
    expect(state.status).toBe(SessionStatus.ROOM_UNAVAILABLE);
  });

  it('starts restoration with room identity', () => {
    const state = matchState();

    expect(state.startRestore('room-1', 'p1')).toBe(true);
    expect(state.phase).toBe(SessionPhase.RESTORING);
    expect(state.isRestoring).toBe(true);
    expect(state.roomId).toBe('room-1');
    expect(state.playerId).toBe('p1');
  });

  it('returns to setup when a connecting socket closes', () => {
    const state = matchState();
    state.data.boardReady = true;
    state.tryEnterWaiting();

    expect(state.connectionClosed()).toBe(true);
    expect(state.phase).toBe(SessionPhase.SETUP);
    expect(state.status).toBe(SessionStatus.DISCONNECTED);
  });

  it('does not restart joining after leaving setup', () => {
    const state = matchState();
    state.enterRoom({ roomId: 'room-1', playerId: 'p1' });

    expect(state.tryEnterWaiting()).toBe(false);
    expect(state.phase).toBe(SessionPhase.PLAYING);
  });
});

function matchState(player, $rootScope) {
  return new MatchStateService(
    roomStoreFake(player),
    machineServiceFake,
    $rootScope
  );
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

function rootScopeFake() {
  return {
    $handler: {
      _checkListenersForAllKeys: jasmine.createSpy('checkListenersForAllKeys'),
      _flushScheduledTasks: jasmine.createSpy('flushScheduledTasks'),
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
