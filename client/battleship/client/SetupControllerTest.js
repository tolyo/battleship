import { create } from '../../../_build/browser/js/modules/battleship/client/SetupController.js';
import {
  canPlaceSetupUnit,
  clearSetupPreview,
  clearSetupUnit,
  placeSetupUnit,
  placeSetupUnitAt,
  previewSetupUnitAt,
  randomizeUnits,
  resetSetupUnits,
} from '../../../assets/game/domain/setup-state.js';
import { CellState } from '../../../assets/game/domain/constants.js';
import { empty_board as emptyBoardState } from '../../../_build/browser/js/modules/battleship/browser/Fleet.js';
import { initialRoomSessionData } from '../../../assets/game/domain/room-state.js';

describe('SetupController', () => {
  it('randomizes setup state', () => {
    const matchState = matchStateFake();

    setupController(matchState).randomize();

    expect(matchState.data.boardReady).toBe(true);
    expect(Object.keys(matchState.data.unitPlacements).length).toBe(10);
  });

  it('refreshes Angular bindings after setup mutations', () => {
    const matchState = matchStateFake();
    const $rootScope = rootScopeFake();

    setupController(matchState, $rootScope).randomize();

    expect($rootScope.$handler._checkListenersForAllKeys).toHaveBeenCalledWith(
      matchState.data
    );
    expect($rootScope.$handler._flushScheduledTasks).toHaveBeenCalledOnceWith();
  });

  it('resets setup state', () => {
    const matchState = matchStateFake();
    const setup = setupController(matchState);

    setup.randomize();
    setup.reset();

    expect(matchState.data.boardReady).toBe(false);
    expect(matchState.data.unitPlacements).toEqual({});
    expect(matchState.boardState[0][0]).toBe(CellState.EMPTY);
  });

  it('clears setup units and preview state', () => {
    const matchState = matchStateFake();
    const setup = setupController(matchState);
    const coordinates = [
      { row: 1, column: 0 },
      { row: 1, column: 1 },
      { row: 1, column: 2 },
    ];

    setup.placeUnit('2', coordinates);
    setup.previewUnitAt('3', '4', '0', 'HORIZONTAL', 3);
    setup.clearUnit('2');
    setup.clearPreview();

    expect(matchState.data.unitPlacements[2]).toBeUndefined();
    expect(matchState.boardState[1][0]).toBe(CellState.EMPTY);
    expect(matchState.data.setupPreviewCoordinates).toEqual([]);
  });

  it('places setup units', () => {
    const matchState = matchStateFake();
    const setup = setupController(matchState);
    const coordinates = [{ row: 1, column: 2 }];

    setup.placeUnit('2', coordinates);

    expect(matchState.data.unitPlacements[2]).toEqual(coordinates);
    expect(matchState.boardState[1][2]).toBe('2');

    setup.placeUnitAt('2', '4', '5', 'HORIZONTAL', 3);

    expect(matchState.data.unitPlacements[2]).toEqual([
      { row: 4, column: 5 },
      { row: 4, column: 6 },
      { row: 4, column: 7 },
    ]);
    expect(matchState.boardState[4].slice(5, 8)).toEqual(['2', '2', '2']);
  });

  it('previews setup unit placement', () => {
    const matchState = matchStateFake();
    const setup = setupController(matchState);

    expect(setup.previewUnitAt('2', '4', '5', 'VERTICAL', 3)).toEqual([
      { row: 4, column: 5 },
      { row: 5, column: 5 },
      { row: 6, column: 5 },
    ]);

    expect(matchState.data.setupPreviewCoordinates).toEqual([
      { row: 4, column: 5 },
      { row: 5, column: 5 },
      { row: 6, column: 5 },
    ]);
    expect(matchState.data.ownRows[4][5].classes['droppable-target']).toBe(
      true
    );
  });

  it('reports setup placement validity', () => {
    const setup = setupController(matchStateFake());

    expect(
      setup.canPlaceUnit('2', [
        { row: 4, column: 5 },
        { row: 4, column: 6 },
        { row: 4, column: 7 },
      ])
    ).toBe(true);
  });

  it('uses the original state and refreshes the latest data after mutation', () => {
    const state = matchStateFake();
    const nextData = { ...state.data, status: 'updated' };
    const calls = [];
    const handler = {
      _checkListenersForAllKeys(data) {
        expect(this).toBe(handler);
        expect(data).toBe(nextData);
        calls.push('check');
      },
      _flushScheduledTasks() {
        expect(this).toBe(handler);
        calls.push('flush');
      },
    };
    const setup = setupController(
      state,
      { $handler: handler },
      {
        randomize(received) {
          expect(received).toBe(state);
          state.data = nextData;
          calls.push('mutate');
        },
      }
    );
    setup.randomize();
    expect(setup.setupState).toBe(state);
    expect(calls).toEqual(['mutate', 'check', 'flush']);
  });

  it('supports scopes with missing or partial refresh callbacks', () => {
    const checked = jasmine.createSpy('checked');
    const flushed = jasmine.createSpy('flushed');
    for (const scope of [
      {},
      { $handler: {} },
      { $handler: { _checkListenersForAllKeys: checked } },
      { $handler: { _flushScheduledTasks: flushed } },
    ]) {
      setupController(matchStateFake(), scope).refresh();
    }
    expect(checked).toHaveBeenCalledTimes(1);
    expect(flushed).toHaveBeenCalledOnceWith();
  });

  it('preserves accepted and rejected placement results and then refreshes', () => {
    const state = matchStateFake();
    const scope = rootScopeFake();
    const result = Object.freeze([{ row: 1, column: 2 }]);
    const placed = jasmine.createSpy('placed').and.returnValue(result);
    const previewed = jasmine.createSpy('previewed').and.returnValue(undefined);
    const setup = setupController(state, scope, {
      placeUnitAt: placed,
      previewUnitAt: previewed,
    });
    expect(setup.placeUnitAt('9', '1', '2', 'HORIZONTAL', 1)).toBe(result);
    expect(setup.previewUnitAt('9', '9', '9', 'VERTICAL', 1)).toBeUndefined();
    expect(placed).toHaveBeenCalledOnceWith(
      state,
      '9',
      '1',
      '2',
      'HORIZONTAL',
      1
    );
    expect(previewed).toHaveBeenCalledOnceWith(
      state,
      '9',
      '9',
      '9',
      'VERTICAL',
      1
    );
    expect(scope.$handler._flushScheduledTasks).toHaveBeenCalledTimes(2);
  });

  it('checks placement without refreshing and propagates mutation failures', () => {
    const state = matchStateFake();
    const scope = rootScopeFake();
    const failure = new Error('placement failed');
    const check = jasmine.createSpy('check').and.returnValue(false);
    const setup = setupController(state, scope, {
      canPlaceUnit: check,
      reset() {
        throw failure;
      },
    });
    const points = [];
    expect(setup.canPlaceUnit('9', points)).toBeFalse();
    expect(check).toHaveBeenCalledOnceWith(state, '9', points);
    expect(() => setup.reset()).toThrow(failure);
    expect(scope.$handler._flushScheduledTasks).not.toHaveBeenCalled();
  });

  it('adapts the match board state for setup operations', () => {
    const matchState = matchStateFake();
    const setup = setupController(matchState);

    expect(setup.setupState.boardState).toBe(matchState.boardState);
  });
});

function matchStateFake() {
  let boardState = emptyBoardState();

  return {
    isSetup: true,
    data: initialRoomSessionData(),
    get boardState() {
      return boardState;
    },
    set boardState(nextBoardState) {
      boardState = nextBoardState;
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

function setupController(state, scope = {}, overrides = {}) {
  return Reflect.construct(create, [
    state,
    scope,
    {
      randomize: randomizeUnits,
      reset: resetSetupUnits,
      clearUnit: clearSetupUnit,
      clearPreview: clearSetupPreview,
      placeUnit: placeSetupUnit,
      placeUnitAt: placeSetupUnitAt,
      previewUnitAt: previewSetupUnitAt,
      canPlaceUnit: canPlaceSetupUnit,
      ...overrides,
    },
  ]);
}
