import { emptyBoardState } from '../../app/game/board-state.js';
import { emptyTileRows } from '../../app/game/board-rows.js';
import { MapTile } from '../../app/game/constants.js';
import {
  canPlaceSetupShip,
  clearSetupShip,
  placeSetupShip,
  randomizeFleet,
  resetSetupFleet,
} from '../../app/game/fleet-setup-state.js';
import { shipCoordinatesFromStart } from '../../app/game/fleet-placement.js';

describe('fleet setup state', () => {
  it('places a setup ship and rebuilds the visible board', () => {
    const state = setupState();

    placeSetupShip(
      state,
      '0',
      shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4)
    );

    expect(state.shipPlacements[0]).toEqual([
      { row: 0, column: 0 },
      { row: 0, column: 1 },
      { row: 0, column: 2 },
      { row: 0, column: 3 },
    ]);
    expect(state.boardState[0].slice(0, 4)).toEqual(['0', '0', '0', '0']);
    expect(state.fleetRows[0][0].state).toBe('ship');
    expect(state.fleetRows[1][0].dataState).toBe(MapTile.BLOCKED);
    expect(state.boardReady).toBe(false);
    expect(state.status).toBe('Place your fleet');
  });

  it('clears a setup ship and rebuilds readiness', () => {
    const state = setupState({
      shipPlacements: {
        0: shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
      },
    });
    placeSetupShip(state, '1', shipCoordinatesFromStart('2', '0', 'HORIZONTAL', 3));

    clearSetupShip(state, '0');

    expect(state.shipPlacements[0]).toBeUndefined();
    expect(state.shipPlacements[1]).toEqual([
      { row: 2, column: 0 },
      { row: 2, column: 1 },
      { row: 2, column: 2 },
    ]);
    expect(state.boardState[0][0]).toBe(MapTile.EMPTY);
    expect(state.boardState[2].slice(0, 3)).toEqual(['1', '1', '1']);
  });

  it('rejects placement checks outside setup phase', () => {
    const state = setupState({ phase: 'playing' });

    expect(
      canPlaceSetupShip(
        state,
        '0',
        shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4)
      )
    ).toBe(false);
  });

  it('ignores setup mutations outside setup phase', () => {
    const state = setupState({
      phase: 'playing',
      shipPlacements: {
        0: shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
      },
      status: 'Your turn',
    });

    resetSetupFleet(state);
    placeSetupShip(state, '1', shipCoordinatesFromStart('2', '0', 'HORIZONTAL', 3));
    clearSetupShip(state, '0');

    expect(state.shipPlacements).toEqual({
      0: shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
    });
    expect(state.status).toBe('Your turn');
  });

  it('randomizes to a complete ready fleet', () => {
    const state = setupState();

    randomizeFleet(state);

    expect(Object.values(state.shipPlacements).flat().length).toBe(20);
    expect(state.boardReady).toBe(true);
    expect(state.status).toBe('Ready to join');
  });
});

function setupState(overrides = {}) {
  return {
    phase: 'setup',
    boardReady: false,
    boardState: emptyBoardState(),
    shipPlacements: {},
    status: 'Place your fleet',
    fleetRows: emptyTileRows('fleetboard'),
    ...overrides,
  };
}
