import {
  alignElementTo,
  coordinatesFromElements,
  elementPagePosition,
  fleetBoardTileAt,
  fleetBoardTileFromPoint,
  fleetBoardTilesForShip,
  orientationFromCoordinates,
  setElementPosition,
  shipElementSize,
  sortedCoordinates,
  tileDataset,
} from '../../app/game/fleet-ship-geometry.js';

describe('fleet ship geometry', () => {
  let originalWindow;
  let originalDocument;
  let originalHTMLElement;

  beforeEach(() => {
    originalWindow = globalThis.window;
    originalDocument = globalThis.document;
    originalHTMLElement = globalThis.HTMLElement;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
    globalThis.document = originalDocument;
    globalThis.HTMLElement = originalHTMLElement;
  });

  it('computes element page positions from viewport box and scroll', () => {
    globalThis.window = { scrollX: 10, scrollY: 20 };
    const element = elementFake({ box: { left: 3, top: 4 } });

    expect(elementPagePosition(element)).toEqual({ left: 13, top: 24 });
  });

  it('sets and aligns element positions', () => {
    globalThis.window = { scrollX: 5, scrollY: 6 };
    const ship = elementFake();
    const target = elementFake({ box: { left: 7, top: 8 } });

    setElementPosition(ship, { left: 1, top: 2 });
    expect(ship.style).toEqual({ left: '1px', top: '2px' });

    alignElementTo(ship, target);
    expect(ship.style).toEqual({ left: '12px', top: '14px' });
  });

  it('reads and validates tile datasets', () => {
    expect(tileDataset(elementFake({ dataset: { row: '1', column: '2' } }))).toEqual({
      row: '1',
      column: '2',
    });
    expect(tileDataset(elementFake({ dataset: { row: '1' } }))).toBeUndefined();
  });

  it('extracts numeric coordinates from elements', () => {
    const coordinates = coordinatesFromElements([
      elementFake({ dataset: { row: '1', column: '2' } }),
      elementFake({ dataset: { row: 'x', column: '3' } }),
      elementFake({ dataset: { row: '4', column: '5' } }),
    ]);

    expect(coordinates).toEqual([
      { row: 1, column: 2 },
      { row: 4, column: 5 },
    ]);
  });

  it('computes ship element sizes', () => {
    expect(shipElementSize(undefined, 'HORIZONTAL')).toEqual({
      width: '30px',
      height: '30px',
    });
    expect(shipElementSize({ size: 4 }, 'HORIZONTAL')).toEqual({
      width: '120px',
      height: '30px',
    });
    expect(shipElementSize({ size: 4 }, 'VERTICAL')).toEqual({
      width: '30px',
      height: '120px',
    });
  });

  it('derives orientation and sorted coordinates', () => {
    expect(
      orientationFromCoordinates([
        { row: 1, column: 1 },
        { row: 1, column: 2 },
      ])
    ).toBe('HORIZONTAL');
    expect(
      orientationFromCoordinates([
        { row: 1, column: 1 },
        { row: 2, column: 1 },
      ])
    ).toBe('VERTICAL');
    expect(orientationFromCoordinates([])).toBe('HORIZONTAL');
    expect(
      sortedCoordinates([
        { row: 2, column: 3 },
        { row: 1, column: 8 },
        { row: 2, column: 1 },
      ])
    ).toEqual([
      { row: 1, column: 8 },
      { row: 2, column: 1 },
      { row: 2, column: 3 },
    ]);
  });

  it('finds fleet board tiles by coordinate and ship span', () => {
    const tiles = new Map([
      ['fleetboard-2-3', elementFake({ id: 'fleetboard-2-3' })],
      ['fleetboard-2-4', elementFake({ id: 'fleetboard-2-4' })],
      ['fleetboard-2-5', elementFake({ id: 'fleetboard-2-5' })],
      ['fleetboard-3-3', elementFake({ id: 'fleetboard-3-3' })],
    ]);
    installDom({ tiles });

    expect(fleetBoardTileAt({ row: 2, column: 3 })).toBe(
      tiles.get('fleetboard-2-3')
    );
    expect(fleetBoardTileAt({ row: 9, column: 9 })).toBeUndefined();
    expect(fleetBoardTilesForShip('2', '3', 'HORIZONTAL', 3)).toEqual([
      tiles.get('fleetboard-2-3'),
      tiles.get('fleetboard-2-4'),
      tiles.get('fleetboard-2-5'),
    ]);
    expect(fleetBoardTilesForShip('2', '3', 'VERTICAL', 2)).toEqual([
      tiles.get('fleetboard-2-3'),
      tiles.get('fleetboard-3-3'),
    ]);
    expect(fleetBoardTilesForShip('x', '3', 'VERTICAL', 2)).toEqual([]);
  });

  it('finds fleet board tile under a point', () => {
    const fleetTile = elementFake({ classNames: ['fleetboard-tile'] });
    const otherTile = elementFake({ classNames: ['other'] });
    installDom({ pointElement: fleetTile });

    expect(fleetBoardTileFromPoint(10, 20)).toBe(fleetTile);

    installDom({ pointElement: otherTile });

    expect(fleetBoardTileFromPoint(10, 20)).toBeUndefined();
  });
});

class FakeElement {
  constructor({ id, dataset, box, classNames }) {
    this.id = id;
    this.dataset = dataset;
    this.box = box;
    this.style = {};
    this.classList = {
      contains: (className) => classNames.includes(className),
    };
  }

  getBoundingClientRect() {
    return this.box;
  }
}

function installDom({ tiles = new Map(), pointElement = undefined } = {}) {
  globalThis.HTMLElement = FakeElement;
  globalThis.document = {
    getElementById(id) {
      return tiles.get(id) ?? null;
    },
    elementFromPoint(x, y) {
      return pointElement
        ? Object.assign(pointElement, { lastPoint: { x, y } })
        : null;
    },
  };
}

function elementFake({
  id = undefined,
  dataset = {},
  box = { left: 0, top: 0 },
  classNames = [],
} = {}) {
  return new FakeElement({ id, dataset, box, classNames });
}
