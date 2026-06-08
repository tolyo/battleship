import {
  allUnitsPlaced,
  canPlaceSetupUnit,
  randomUnitPlacements,
  unitCoordinatesFromStart,
} from './unit-placement.js';

describe('unitCoordinatesFromStart', () => {
  it('builds horizontal coordinates', () => {
    expect(unitCoordinatesFromStart('2', '3', 'HORIZONTAL', 3)).toEqual([
      { row: 2, column: 3 },
      { row: 2, column: 4 },
      { row: 2, column: 5 },
    ]);
  });

  it('builds vertical coordinates', () => {
    expect(unitCoordinatesFromStart('2', '3', 'VERTICAL', 3)).toEqual([
      { row: 2, column: 3 },
      { row: 3, column: 3 },
      { row: 4, column: 3 },
    ]);
  });
});

describe('canPlaceSetupUnit', () => {
  const placements = {
    0: unitCoordinatesFromStart('0', '0', 'HORIZONTAL', 4),
  };

  it('rejects overlapping units', () => {
    expect(
      canPlaceSetupUnit(
        placements,
        '1',
        unitCoordinatesFromStart('0', '2', 'HORIZONTAL', 3)
      )
    ).toBe(false);
  });

  it('rejects adjacent units', () => {
    expect(
      canPlaceSetupUnit(
        placements,
        '1',
        unitCoordinatesFromStart('1', '0', 'HORIZONTAL', 3)
      )
    ).toBe(false);
  });

  it('accepts separated units', () => {
    expect(
      canPlaceSetupUnit(
        placements,
        '1',
        unitCoordinatesFromStart('2', '0', 'HORIZONTAL', 3)
      )
    ).toBe(true);
  });
});

describe('randomUnitPlacements', () => {
  it('produces a complete legal unit layout', () => {
    const placements = randomUnitPlacements();

    expect(allUnitsPlaced(placements)).toBe(true);
    expect(Object.values(placements).flat().length).toBe(20);
  });
});
