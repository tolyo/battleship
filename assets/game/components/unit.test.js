import { UnitController } from './unit.js';

class FakeElement {
  constructor() {
    this.parentElement = undefined;
    this.dataset = {};
    this.classList = {
      contains: jasmine.createSpy('contains').and.returnValue(false),
    };
    this.getBoundingClientRect = jasmine
      .createSpy('getBoundingClientRect')
      .and.returnValue({ left: 0, top: 0 });
  }

  setPointerCapture() {}

  releasePointerCapture() {}
}

describe('UnitController', () => {
  let originalDocument;
  let originalWindow;
  let originalHTMLElement;

  beforeEach(() => {
    originalDocument = globalThis.document;
    originalWindow = globalThis.window;
    originalHTMLElement = globalThis.HTMLElement;
    globalThis.HTMLElement = FakeElement;
    globalThis.window = { scrollX: 0, scrollY: 0 };
    globalThis.document = {
      elementFromPoint: () => undefined,
      getElementById: () => undefined,
    };
  });

  afterEach(() => {
    globalThis.document = originalDocument;
    globalThis.window = originalWindow;
    globalThis.HTMLElement = originalHTMLElement;
  });

  it('links only complete unit elements and synchronizes later changes', () => {
    const missingUnit = controller();
    spyOn(missingUnit, 'syncCoordinates');
    missingUnit.$postLink();
    expect(missingUnit.syncCoordinates).not.toHaveBeenCalled();

    const missingPlaceholder = controller();
    missingPlaceholder.unit = unit();
    missingPlaceholder.$element.parentElement = {};
    missingPlaceholder.$postLink();
    expect(missingPlaceholder.placeHolder).toBeUndefined();

    const missingElement = controller();
    missingElement.unit = unit();
    missingElement.$postLink();
    expect(missingElement.placeHolder).toBe(
      missingElement.$element.parentElement
    );

    const linked = linkedController();
    spyOn(linked, 'setRotation');
    spyOn(linked, 'syncCoordinates');
    linked.$postLink();
    linked.$onChanges();

    expect(linked.setRotation).toHaveBeenCalledOnceWith();
    expect(linked.syncCoordinates).toHaveBeenCalledTimes(2);

    const unlinked = controller();
    spyOn(unlinked, 'syncCoordinates');
    unlinked.$onChanges();
    expect(unlinked.syncCoordinates).not.toHaveBeenCalled();
  });

  it('requires linked unit and placeholder elements', () => {
    const state = controller();

    expect(() => state.getUnitElement()).toThrowError(
      'Unit element has not been linked.'
    );
    expect(() => state.getPlaceHolder()).toThrowError(
      'Unit placeholder has not been linked.'
    );

    state.unitElement = new FakeElement();
    state.placeHolder = new FakeElement();
    expect(state.getUnitElement()).toBe(state.unitElement);
    expect(state.getPlaceHolder()).toBe(state.placeHolder);
  });

  it('aligns units to placeholders and active layout anchors', () => {
    const state = linkedController();
    spyOn(state, 'clearMapBlocks');
    spyOn(state, 'alignToPlaceholder');
    state.setOnPlaceholder();
    expect(state.clearMapBlocks).toHaveBeenCalledOnceWith();
    expect(state.alignToPlaceholder).toHaveBeenCalledOnceWith();

    state.alignToPlaceholder.and.callThrough();
    spyOn(state, 'resetPreviewState');
    spyOn(state.layout, 'resetToPlaceholder');
    spyOn(state, 'setRotation');
    spyOn(state, 'alignToElement');
    state.alignToPlaceholder();
    expect(state.alignToElement).toHaveBeenCalledWith(state.placeHolder);

    state.setRotation.calls.reset();
    state.drag.active = true;
    state.realignToLayout();
    expect(state.setRotation).not.toHaveBeenCalled();

    state.drag.active = false;
    state.layout.anchorTile = new FakeElement();
    state.realignToLayout();
    expect(state.alignToElement).toHaveBeenCalledWith(state.layout.anchorTile);

    state.layout.anchorTile = undefined;
    state.realignToLayout();
    expect(state.alignToElement).toHaveBeenCalledWith(state.placeHolder);
  });

  it('realigns only after both rendered elements exist', () => {
    const state = controller();
    spyOn(state, 'realignToLayout');

    state.$afterRender();
    state.unitElement = new FakeElement();
    state.$afterRender();
    state.placeHolder = new FakeElement();
    state.$afterRender();

    expect(state.realignToLayout).toHaveBeenCalledOnceWith();
  });

  it('syncs placed coordinates and handles unplaced setup and play units', () => {
    const state = linkedController();
    spyOn(state, 'alignToPlaceholder');
    spyOn(state.layout, 'hide');

    state.unit = undefined;
    state.setupActive = true;
    state.syncCoordinates();
    expect(state.alignToPlaceholder).toHaveBeenCalledOnceWith();

    state.setupActive = false;
    state.syncCoordinates();
    expect(state.layout.hide).toHaveBeenCalledOnceWith();

    state.unit = unit();
    state.coordinates = [];
    state.syncCoordinates();
    expect(state.layout.hide).toHaveBeenCalledTimes(2);

    state.coordinates = [{ row: 2, column: 3 }];
    spyOn(state.layout, 'syncCoordinates').and.callFake(() => {
      state.layout.anchorTile = undefined;
    });
    spyOn(state, 'setRotation');
    spyOn(state, 'alignToElement');
    state.syncCoordinates();
    expect(state.alignToElement).not.toHaveBeenCalled();

    const anchor = new FakeElement();
    state.layout.syncCoordinates.and.callFake(() => {
      state.layout.anchorTile = anchor;
    });
    state.syncCoordinates();
    expect(state.alignToElement).toHaveBeenCalledWith(anchor);
  });

  it('rotates only eligible units onto valid placements', () => {
    const state = linkedController();
    spyOn(state, 'placeAt').and.returnValue([{ row: 1, column: 2 }]);
    spyOn(state, 'setRotation');
    spyOn(state.layout, 'setPreview');

    state.unit = undefined;
    state.onDoubleClick();
    state.unit = unit();
    state.locked = true;
    state.onDoubleClick();
    state.locked = false;
    state.unit = unit({ size: 1 });
    state.onDoubleClick();
    state.unit = unit();
    state.layout.anchorTile = undefined;
    state.onDoubleClick();
    expect(state.placeAt).not.toHaveBeenCalled();

    state.layout.anchorTile = new FakeElement();
    state.onDoubleClick();
    expect(state.placeAt).not.toHaveBeenCalled();

    state.layout.anchorTile.dataset = { row: '1', column: '2' };
    state.onDoubleClick();
    expect(state.layout.orientation).toBe('VERTICAL');
    expect(state.setRotation).toHaveBeenCalledOnceWith();
    expect(state.layout.setPreview).toHaveBeenCalledWith([
      { row: 1, column: 2 },
    ]);

    state.placeAt.and.returnValue(undefined);
    state.onDoubleClick();
    expect(state.setRotation).toHaveBeenCalledOnceWith();
  });

  it('starts pointer drags only for unlocked primary input', () => {
    const state = linkedController();
    spyOn(state, 'getUnitCoordinates').and.returnValue({ left: 10, top: 20 });
    spyOn(state.drag, 'start');

    state.unit = undefined;
    expect(state.startDrag(pointer({ button: 0 }))).toBe(false);
    state.unit = unit();
    state.locked = true;
    expect(state.startDrag(pointer({ button: 0 }))).toBe(false);
    state.locked = false;
    expect(state.startDrag(pointer({ button: 1 }))).toBe(false);
    expect(state.startDrag(pointer({ button: undefined }))).toBe(true);
    expect(state.drag.start).toHaveBeenCalled();

    spyOn(state, 'startDrag').and.returnValue(false);
    state.onPointerDown(pointer({ isPrimary: false }));
    state.onPointerDown(pointer());
    expect(state.startDrag).toHaveBeenCalledOnceWith(jasmine.any(Object));

    state.startDrag.and.returnValue(true);
    const target = new FakeElement();
    spyOn(target, 'setPointerCapture');
    state.onPointerDown(pointer({ currentTarget: target }));
    state.onPointerDown(pointer({ currentTarget: null }));
    expect(target.setPointerCapture).toHaveBeenCalledWith(7);
  });

  it('routes owned pointer movement and completion', () => {
    const state = linkedController();
    spyOn(state.drag, 'owns').and.returnValue(false);
    spyOn(state, 'moveUnit');
    spyOn(state, 'finishDrag');
    const target = new FakeElement();
    spyOn(target, 'releasePointerCapture');

    state.onPointerMove(pointer());
    state.onPointerUp(pointer());
    state.onPointerCancel(pointer());
    expect(state.moveUnit).not.toHaveBeenCalled();
    expect(state.finishDrag).not.toHaveBeenCalled();

    state.drag.owns.and.returnValue(true);
    state.onPointerMove(pointer());
    state.onPointerUp(pointer({ currentTarget: target }));
    state.onPointerUp(pointer({ currentTarget: null }));
    state.onPointerCancel(pointer({ currentTarget: target }));
    state.onPointerCancel(pointer({ currentTarget: null }));
    expect(state.moveUnit).toHaveBeenCalledOnceWith(jasmine.any(Object));
    expect(target.releasePointerCapture).toHaveBeenCalledTimes(2);
    expect(target.releasePointerCapture).toHaveBeenCalledWith(7);
    expect(state.finishDrag).toHaveBeenCalledTimes(4);
  });

  it('moves units over valid board previews and clears invalid previews', () => {
    const state = linkedController();
    spyOn(state.drag, 'consumeFirstMove').and.returnValues(true, false, false);
    spyOn(state.drag, 'position').and.returnValue({ left: 20, top: 30 });
    spyOn(state, 'clearMapBlocks');
    spyOn(state, 'setPosition');
    spyOn(state, 'previewAt').and.returnValue(undefined);
    spyOn(state, 'resetPreviewState');
    spyOn(state.layout, 'setPreview');

    state.moveUnit(pointer());
    expect(state.clearMapBlocks).toHaveBeenCalledOnceWith();
    expect(state.resetPreviewState).toHaveBeenCalledOnceWith();

    const tile = new FakeElement();
    tile.dataset = { row: '2', column: '3' };
    tile.classList.contains.and.returnValue(true);
    globalThis.document.elementFromPoint = () => tile;
    state.moveUnit(pointer());
    expect(state.resetPreviewState).toHaveBeenCalledTimes(2);

    const coordinates = [{ row: 2, column: 3 }];
    state.previewAt.and.returnValue(coordinates);
    state.moveUnit(pointer());
    expect(state.layout.setPreview).toHaveBeenCalledWith(coordinates);
  });

  it('commits valid drags and restores invalid drags', () => {
    const state = linkedController();
    spyOn(state.drag, 'stop');
    spyOn(state.layout, 'hasPlacement').and.returnValues(true, true, false);
    spyOn(state, 'alignToElement');
    spyOn(state, 'claimTiles');
    spyOn(state, 'setOnPlaceholder');

    state.layout.anchorTile = new FakeElement();
    state.finishDrag();
    expect(state.claimTiles).toHaveBeenCalledOnceWith();

    state.layout.anchorTile = undefined;
    state.finishDrag();
    state.finishDrag();
    expect(state.setOnPlaceholder).toHaveBeenCalledTimes(2);
  });

  it('delegates style, placement, cleanup, and view state', () => {
    const state = linkedController();
    const position = { left: 12, top: 34 };
    const coordinates = [{ row: 1, column: 2 }];
    spyOn(state.style, 'setSize');
    spyOn(state.style, 'setPosition');
    spyOn(state.setup, 'clearUnit');
    spyOn(state.setup, 'clearPreview');
    spyOn(state.setup, 'commit');
    spyOn(state.setup, 'previewAt').and.returnValue(coordinates);
    spyOn(state.setup, 'placeAt').and.returnValue(coordinates);
    spyOn(state.layout, 'resetPreview');
    spyOn(state.drag, 'stop');
    state.layout.previewCoordinates = coordinates;

    state.setRotation();
    state.setPosition(position);
    state.clearMapBlocks();
    state.resetPreviewState();
    state.claimTiles();
    expect(state.previewAt('1', '2', 'VERTICAL')).toBe(coordinates);
    expect(state.placeAt('1', '2', 'VERTICAL')).toBe(coordinates);
    state.$onDestroy();

    expect(state.style.setSize).toHaveBeenCalledWith(
      state.unit,
      state.layout.orientation
    );
    expect(state.setup.commit).toHaveBeenCalledWith(state.unit, coordinates);

    state.locked = false;
    state.drag.active = true;
    expect(state.ariaDisabled).toBe('false');
    expect(state.cssClasses).toEqual({ locked: false, dragged: true });
    state.locked = true;
    expect(state.ariaDisabled).toBe('true');
  });

  it('reads element positions when aligning and dragging', () => {
    const state = linkedController();
    const element = new FakeElement();
    element.getBoundingClientRect.and.returnValue({ left: 4, top: 5 });
    globalThis.window = { scrollX: 6, scrollY: 7 };
    spyOn(state, 'setPosition');

    state.alignToElement(element);
    expect(state.setPosition).toHaveBeenCalledWith({ left: 10, top: 12 });
    state.unitElement = element;
    expect(state.getUnitCoordinates()).toEqual({ left: 10, top: 12 });
  });
});

function controller() {
  const root = new FakeElement();
  root.parentElement = new FakeElement();
  return new UnitController(root, setupFake());
}

function linkedController() {
  const state = controller();
  state.unit = unit();
  state.coordinates = [{ row: 1, column: 2 }];
  state.placeHolder = state.$element.parentElement;
  state.unitElement = new FakeElement();
  return state;
}

function unit(overrides = {}) {
  return { id: '2', size: 3, placeholderWidth: '90px', ...overrides };
}

function setupFake() {
  return {
    clearUnit: jasmine.createSpy('clearUnit'),
    clearPreview: jasmine.createSpy('clearPreview'),
    placeUnit: jasmine.createSpy('placeUnit'),
    placeUnitAt: jasmine.createSpy('placeUnitAt'),
    previewUnitAt: jasmine.createSpy('previewUnitAt'),
  };
}

function pointer(overrides = {}) {
  return {
    isPrimary: true,
    pointerId: 7,
    button: 0,
    pageX: 50,
    pageY: 60,
    currentTarget: null,
    ...overrides,
  };
}
