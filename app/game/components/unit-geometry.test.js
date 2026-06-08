import {
  elementPagePosition,
  boardTileAt,
  boardTileFromPoint,
  orientationFromCoordinates,
  elementSize,
  sortedCoordinates,
  tileDataset,
} from './unit-geometry.js';

describe('unit geometry', () => {
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

  it('reads and validates tile datasets', () => {
    expect(tileDataset(elementFake({ dataset: { row: '1', column: '2' } }))).toEqual({
      row: '1',
      column: '2',
    });
    expect(tileDataset(elementFake({ dataset: { row: '1' } }))).toBeUndefined();
  });

  it('computes unit element sizes', () => {
    expect(elementSize(undefined, 'HORIZONTAL')).toEqual({
      width: '30px',
      height: '30px',
    });
    expect(elementSize({ size: 4 }, 'HORIZONTAL')).toEqual({
      width: '120px',
      height: '30px',
    });
    expect(elementSize({ size: 4 }, 'VERTICAL')).toEqual({
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

  it('finds own board tile by coordinate', () => {
    const tiles = new Map([
      ['own-board-2-3', elementFake({ id: 'own-board-2-3' })],
    ]);
    installDom({ tiles });

    expect(boardTileAt({ row: 2, column: 3 })).toBe(
      tiles.get('own-board-2-3')
    );
    expect(boardTileAt({ row: 9, column: 9 })).toBeUndefined();
  });

  it('finds own board tile under a point', () => {
    const unitTile = elementFake({ classNames: ['own-board-tile'] });
    const otherTile = elementFake({ classNames: ['other'] });
    installDom({ pointElement: unitTile });

    expect(boardTileFromPoint(10, 20)).toBe(unitTile);

    installDom({ pointElement: otherTile });

    expect(boardTileFromPoint(10, 20)).toBeUndefined();
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
