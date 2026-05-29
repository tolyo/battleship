import {
  allFleetShipsPlaced,
  canPlaceSetupShip,
  randomFleetPlacements,
  shipCoordinatesFromStart,
} from '../../app/game/fleet-placement.js';

describe('shipCoordinatesFromStart', () => {
  it('builds horizontal coordinates', () => {
    expect(shipCoordinatesFromStart('2', '3', 'HORIZONTAL', 3)).toEqual([
      { row: 2, column: 3 },
      { row: 2, column: 4 },
      { row: 2, column: 5 },
    ]);
  });

  it('builds vertical coordinates', () => {
    expect(shipCoordinatesFromStart('2', '3', 'VERTICAL', 3)).toEqual([
      { row: 2, column: 3 },
      { row: 3, column: 3 },
      { row: 4, column: 3 },
    ]);
  });
});

describe('canPlaceSetupShip', () => {
  const placements = {
    0: shipCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
  };

  it('rejects overlapping ships', () => {
    expect(
      canPlaceSetupShip(
        placements,
        '1',
        shipCoordinatesFromStart('0', '2', 'HORIZONTAL', 3)
      )
    ).toBe(false);
  });

  it('rejects adjacent ships', () => {
    expect(
      canPlaceSetupShip(
        placements,
        '1',
        shipCoordinatesFromStart('1', '0', 'HORIZONTAL', 3)
      )
    ).toBe(false);
  });

  it('accepts separated ships', () => {
    expect(
      canPlaceSetupShip(
        placements,
        '1',
        shipCoordinatesFromStart('2', '0', 'HORIZONTAL', 3)
      )
    ).toBe(true);
  });
});

describe('randomFleetPlacements', () => {
  it('produces a complete legal fleet', () => {
    const placements = randomFleetPlacements();

    expect(allFleetShipsPlaced(placements)).toBe(true);
    expect(Object.values(placements).flat().length).toBe(20);
  });
});
