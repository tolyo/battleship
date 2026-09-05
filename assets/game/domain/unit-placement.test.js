import {
  allUnitsPlaced,
  boardStateFromPlacements,
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

  it('rejects non-numeric starting coordinates', () => {
    expect(unitCoordinatesFromStart('row', '3', 'VERTICAL', 3)).toEqual([]);
    expect(unitCoordinatesFromStart('2', 'column', 'HORIZONTAL', 3)).toEqual(
      []
    );
  });
});

describe('boardStateFromPlacements', () => {
  it('marks invalid and duplicate coordinates as incomplete', () => {
    const result = boardStateFromPlacements({
      0: [
        { row: 0, column: 0 },
        { row: 0, column: 0 },
        { row: 0, column: 1 },
        { row: 10, column: 1 },
      ],
    });

    expect(result.complete).toBe(false);
    expect(result.placedUnitCells).toBe(2);
    expect(result.boardState[0].slice(0, 2)).toEqual(['0', '0']);
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

  it('rejects unknown units, wrong sizes, and invalid coordinates', () => {
    expect(canPlaceSetupUnit({}, 'unknown', [])).toBe(false);
    expect(canPlaceSetupUnit({}, '1', [])).toBe(false);
    expect(
      canPlaceSetupUnit({}, '1', [
        { row: 0, column: 0 },
        { row: 0, column: 1 },
        { row: 0, column: 10 },
      ])
    ).toBe(false);
  });
});

describe('randomUnitPlacements', () => {
  it('produces a complete legal unit layout', () => {
    const placements = randomUnitPlacements();

    expect(allUnitsPlaced(placements)).toBe(true);
    expect(Object.values(placements).flat().length).toBe(20);
  });

  it('fails explicitly when random choices cannot place the next unit', () => {
    spyOn(Math, 'random').and.returnValue(0);

    expect(() => randomUnitPlacements()).toThrowError(
      Error,
      'Unable to place unit 1'
    );
  });
});
