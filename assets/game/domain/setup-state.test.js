import { emptyBoardState } from './board-state.js';
import { emptyTileRows } from './board-rows.js';
import { CellState, SessionStatus } from './constants.js';
import { initialRoomSessionData, SessionPhase } from './room-state.js';
import {
  canPlaceSetupUnit,
  clearSetupPreview,
  clearSetupUnit,
  placeSetupUnit,
  placeSetupUnitAt,
  previewSetupUnit,
  previewSetupUnitAt,
  randomizeUnits,
  resetSetupUnits,
} from './setup-state.js';
import { unitCoordinatesFromStart } from './unit-placement.js';

describe('setup state', () => {
  it('places a setup unit and rebuilds the visible board', () => {
    const state = setupState();

    placeSetupUnit(
      state,
      '0',
      unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4)
    );

    expect(state.data.unitPlacements[0]).toEqual([
      { row: 0, column: 0 },
      { row: 0, column: 1 },
      { row: 0, column: 2 },
      { row: 0, column: 3 },
    ]);
    expect(state.boardState[0].slice(0, 4)).toEqual(['0', '0', '0', '0']);
    expect(state.data.ownRows[0][0].state).toBe('unit');
    expect(state.data.ownRows[1][0].dataState).toBe(CellState.BLOCKED);
    expect(state.data.boardReady).toBe(false);
    expect(state.data.status).toBe(SessionStatus.PLACE_FLEET);
  });

  it('clears a setup unit and rebuilds readiness', () => {
    const state = setupState({
      unitPlacements: {
        0: unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
      },
    });
    placeSetupUnit(
      state,
      '1',
      unitCoordinatesFromStart('2', '0', 'HORIZONTAL', 3)
    );

    clearSetupUnit(state, '0');

    expect(state.data.unitPlacements[0]).toBeUndefined();
    expect(state.data.unitPlacements[1]).toEqual([
      { row: 2, column: 0 },
      { row: 2, column: 1 },
      { row: 2, column: 2 },
    ]);
    expect(state.boardState[0][0]).toBe(CellState.EMPTY);
    expect(state.boardState[2].slice(0, 3)).toEqual(['1', '1', '1']);
  });

  it('rejects placement checks outside setup phase', () => {
    const state = setupState({ current: SessionPhase.PLAYING });

    expect(
      canPlaceSetupUnit(
        state,
        '0',
        unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4)
      )
    ).toBe(false);
  });

  it('ignores setup mutations outside setup phase', () => {
    const state = setupState({
      current: SessionPhase.PLAYING,
      unitPlacements: {
        0: unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
      },
      status: 'Your turn',
    });

    resetSetupUnits(state);
    placeSetupUnit(
      state,
      '1',
      unitCoordinatesFromStart('2', '0', 'HORIZONTAL', 3)
    );
    clearSetupUnit(state, '0');

    expect(state.data.unitPlacements).toEqual({
      0: unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
    });
    expect(state.data.status).toBe('Your turn');
  });

  it('randomizes to a complete ready unit placement', () => {
    const state = setupState();

    randomizeUnits(state);

    expect(Object.values(state.data.unitPlacements).flat().length).toBe(20);
    expect(state.data.boardReady).toBe(true);
    expect(state.data.status).toBe(SessionStatus.FLEET_READY);
  });

  it('projects setup placement previews into visible board classes', () => {
    const state = setupState();

    previewSetupUnit(
      state,
      unitCoordinatesFromStart('3', '2', 'HORIZONTAL', 3)
    );

    expect(state.data.setupPreviewCoordinates).toEqual([
      { row: 3, column: 2 },
      { row: 3, column: 3 },
      { row: 3, column: 4 },
    ]);
    expect(state.data.ownRows[3][2].classes['droppable-target']).toBe(true);

    clearSetupPreview(state);

    expect(state.data.setupPreviewCoordinates).toEqual([]);
    expect(state.data.ownRows[3][2].classes['droppable-target']).toBe(false);
  });

  it('previews valid setup placements from start tile and orientation', () => {
    const state = setupState();

    expect(previewSetupUnitAt(state, '1', '2', '0', 'HORIZONTAL', 3)).toEqual([
      { row: 2, column: 0 },
      { row: 2, column: 1 },
      { row: 2, column: 2 },
    ]);
    expect(state.data.ownRows[2][0].classes['droppable-target']).toBe(true);
  });

  it('does not rebuild unchanged setup previews', () => {
    const state = setupState();

    previewSetupUnitAt(state, '1', '2', '0', 'HORIZONTAL', 3);
    const { ownRows } = state.data;

    previewSetupUnitAt(state, '1', '2', '0', 'HORIZONTAL', 3);

    expect(state.data.ownRows).toBe(ownRows);
  });

  it('rejects invalid setup placement previews and clears stale preview', () => {
    const state = setupState();
    placeSetupUnit(
      state,
      '0',
      unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4)
    );
    previewSetupUnit(
      state,
      unitCoordinatesFromStart('3', '0', 'HORIZONTAL', 3)
    );

    expect(
      previewSetupUnitAt(state, '1', '0', '1', 'HORIZONTAL', 3)
    ).toBeUndefined();
    expect(state.data.setupPreviewCoordinates).toEqual([]);
  });

  it('places valid setup units from start tile and orientation', () => {
    const state = setupState();

    expect(placeSetupUnitAt(state, '1', '2', '0', 'HORIZONTAL', 3)).toEqual([
      { row: 2, column: 0 },
      { row: 2, column: 1 },
      { row: 2, column: 2 },
    ]);
    expect(state.data.unitPlacements[1]).toEqual([
      { row: 2, column: 0 },
      { row: 2, column: 1 },
      { row: 2, column: 2 },
    ]);
  });

  it('rejects invalid placement coordinates without changing setup state', () => {
    const state = setupState();

    expect(
      placeSetupUnitAt(state, '1', 'outside', '0', 'HORIZONTAL', 3)
    ).toBeUndefined();
    expect(state.data.unitPlacements).toEqual({});
  });

  it('ignores preview and randomization operations outside setup phase', () => {
    const state = setupState({ current: SessionPhase.PLAYING });
    const initialRows = state.data.ownRows;

    randomizeUnits(state);
    previewSetupUnit(state, [{ row: 1, column: 1 }]);
    clearSetupPreview(state);

    expect(state.data.unitPlacements).toEqual({});
    expect(state.data.setupPreviewCoordinates).toEqual([]);
    expect(state.data.ownRows).toBe(initialRows);
  });
});

function setupState(overrides = {}) {
  const { current = SessionPhase.SETUP, ...dataOverrides } = overrides;
  const data = {
    ...initialRoomSessionData(),
    ownRows: emptyTileRows('own-board'),
    ...dataOverrides,
  };

  return {
    isSetup: current === SessionPhase.SETUP,
    data,
    boardState: emptyBoardState(),
  };
}
