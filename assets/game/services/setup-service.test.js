import { SetupService } from './setup-service.js';
import { CellState } from '../domain/constants.js';
import { emptyBoardState } from '../domain/board-state.js';
import { initialRoomSessionData } from '../domain/room-state.js';

describe('SetupService', () => {
  it('randomizes setup state', () => {
    const matchState = matchStateFake();

    new SetupService(matchState).randomize();

    expect(matchState.data.boardReady).toBe(true);
    expect(Object.keys(matchState.data.unitPlacements).length).toBe(10);
  });

  it('refreshes Angular bindings after setup mutations', () => {
    const matchState = matchStateFake();
    const $rootScope = rootScopeFake();

    new SetupService(matchState, $rootScope).randomize();

    expect($rootScope.$handler._checkListenersForAllKeys).toHaveBeenCalledWith(
      matchState.data
    );
    expect($rootScope.$handler._flushScheduledTasks).toHaveBeenCalledOnceWith();
  });

  it('resets setup state', () => {
    const matchState = matchStateFake();
    const setup = new SetupService(matchState);

    setup.randomize();
    setup.reset();

    expect(matchState.data.boardReady).toBe(false);
    expect(matchState.data.unitPlacements).toEqual({});
    expect(matchState.boardState[0][0]).toBe(CellState.EMPTY);
  });

  it('clears setup units and preview state', () => {
    const matchState = matchStateFake();
    const setup = new SetupService(matchState);
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
    const setup = new SetupService(matchState);
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
    const setup = new SetupService(matchState);

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
    const setup = new SetupService(matchStateFake());

    expect(
      setup.canPlaceUnit('2', [
        { row: 4, column: 5 },
        { row: 4, column: 6 },
        { row: 4, column: 7 },
      ])
    ).toBe(true);
  });

  it('adapts the match board state for setup operations', () => {
    const matchState = matchStateFake();
    const setup = new SetupService(matchState);

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
