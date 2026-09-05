import { UnitLayoutState } from './unit-layout-state.js';

describe('UnitLayoutState', () => {
  let originalDocument;
  let originalHTMLElement;

  beforeEach(() => {
    originalDocument = globalThis.document;
    originalHTMLElement = globalThis.HTMLElement;
  });

  afterEach(() => {
    globalThis.document = originalDocument;
    globalThis.HTMLElement = originalHTMLElement;
  });

  it('syncs orientation, anchor tile, and visibility from coordinates', () => {
    const tile = elementFake();
    installDom({
      tiles: new Map([['own-board-2-3', tile]]),
    });
    const state = new UnitLayoutState();

    state.syncCoordinates([
      { row: 2, column: 3 },
      { row: 2, column: 4 },
    ]);

    expect(state.orientation).toBe('HORIZONTAL');
    expect(state.anchorTile).toBe(tile);
    expect(state.visible).toBe(true);
    expect(state.previewCoordinates).toEqual([
      { row: 2, column: 3 },
      { row: 2, column: 4 },
    ]);
  });

  it('hides when synced coordinates do not have an anchor tile', () => {
    installDom({ tiles: new Map() });
    const state = new UnitLayoutState();

    state.syncCoordinates([{ row: 2, column: 3 }]);

    expect(state.anchorTile).toBeUndefined();
    expect(state.visible).toBe(false);
  });

  it('resets to placeholder defaults', () => {
    const state = new UnitLayoutState();
    state.orientation = 'VERTICAL';
    state.visible = false;
    state.previewCoordinates = [{ row: 1, column: 2 }];
    state.anchorTile = elementFake();

    state.resetToPlaceholder();

    expect(state.orientation).toBe('HORIZONTAL');
    expect(state.visible).toBe(true);
    expect(state.previewCoordinates).toEqual([]);
    expect(state.anchorTile).toBeUndefined();
  });

  it('reports opposite orientation and placement availability', () => {
    const state = new UnitLayoutState();

    expect(state.oppositeOrientation()).toBe('VERTICAL');
    expect(state.hasPlacement()).toBe(false);

    state.orientation = 'VERTICAL';
    state.anchorTile = elementFake();
    state.previewCoordinates = [{ row: 1, column: 2 }];

    expect(state.oppositeOrientation()).toBe('HORIZONTAL');
    expect(state.hasPlacement()).toBe(true);
  });

  it('can hide a unit without changing its placement', () => {
    const state = new UnitLayoutState();
    state.previewCoordinates = [{ row: 1, column: 2 }];

    state.hide();

    expect(state.visible).toBe(false);
    expect(state.previewCoordinates).toEqual([{ row: 1, column: 2 }]);
  });

  it('clears the anchor when preview coordinates are empty', () => {
    const state = new UnitLayoutState();
    state.anchorTile = elementFake();

    state.setPreview([]);

    expect(state.anchorTile).toBeUndefined();
    expect(state.previewCoordinates).toEqual([]);
  });
});

class FakeElement {}

function installDom({ tiles }) {
  globalThis.HTMLElement = FakeElement;
  globalThis.document = {
    getElementById(id) {
      return tiles.get(id);
    },
  };
}

function elementFake() {
  return new FakeElement();
}
