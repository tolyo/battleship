import { UnitController } from './unit.js';
import {
  started,
  stopped,
} from '../../../_build/browser/js/modules/battleship/browser/UnitDrag.js';

class FakeElement {
  constructor() {
    this.parentElement = undefined;
    this.dataset = {};
    this.style = {};
    this.classList = {
      add: jasmine.createSpy('add'),
      remove: jasmine.createSpy('remove'),
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
  let originalRequestFrame;
  let originalCancelFrame;
  let frames;

  beforeEach(() => {
    originalDocument = globalThis.document;
    originalWindow = globalThis.window;
    originalHTMLElement = globalThis.HTMLElement;
    originalRequestFrame = globalThis.requestAnimationFrame;
    originalCancelFrame = globalThis.cancelAnimationFrame;
    frames = new Map();
    let nextFrame = 0;
    globalThis.requestAnimationFrame = (callback) => {
      const id = nextFrame++;
      frames.set(id, callback);
      return id;
    };
    globalThis.cancelAnimationFrame = (id) => frames.delete(id);
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
    globalThis.requestAnimationFrame = originalRequestFrame;
    globalThis.cancelAnimationFrame = originalCancelFrame;
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

  it('preserves the active preview during parent coordinate updates', () => {
    const state = linkedController();
    state.drag.active = true;
    state.layout.previewCoordinates = [{ row: 2, column: 3 }];
    spyOn(state, 'alignToPlaceholder');
    spyOn(state, 'syncLayoutCoordinates');
    state.$onChanges();
    expect(state.alignToPlaceholder).not.toHaveBeenCalled();
    expect(state.syncLayoutCoordinates).not.toHaveBeenCalled();
    expect(state.layout.previewCoordinates).toEqual([{ row: 2, column: 3 }]);
  });

  it('does not clear another ship preview while aligning an unplaced unit', () => {
    const state = linkedController();
    spyOn(state.setup, 'clearPreview');
    state.alignToPlaceholder();
    expect(state.setup.clearPreview).not.toHaveBeenCalled();
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
    spyOn(state, 'setRotation');
    spyOn(state, 'alignToElement');
    state.alignToPlaceholder();
    expect(state.layout.orientation).toBe('HORIZONTAL');
    expect(state.layout.previewCoordinates).toEqual([]);
    expect(state.anchorTile).toBeUndefined();
    expect(state.alignToElement).toHaveBeenCalledWith(state.placeHolder);

    state.setRotation.calls.reset();
    state.drag.active = true;
    state.realignToLayout();
    expect(state.setRotation).not.toHaveBeenCalled();

    state.drag.active = false;
    state.anchorTile = new FakeElement();
    state.realignToLayout();
    expect(state.alignToElement).toHaveBeenCalledWith(state.anchorTile);

    state.anchorTile = undefined;
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

    state.unit = undefined;
    state.setupActive = true;
    state.syncCoordinates();
    expect(state.alignToPlaceholder).toHaveBeenCalledOnceWith();

    state.setupActive = false;
    state.syncCoordinates();
    expect(state.layout.visible).toBe(false);

    state.unit = unit();
    state.coordinates = [];
    state.syncCoordinates();
    expect(state.layout.visible).toBe(false);

    state.coordinates = [{ row: 2, column: 3 }];
    spyOn(state, 'syncLayoutCoordinates').and.callFake(() => {
      state.anchorTile = undefined;
    });
    spyOn(state, 'setRotation');
    spyOn(state, 'alignToElement');
    state.syncCoordinates();
    expect(state.alignToElement).not.toHaveBeenCalled();

    const anchor = new FakeElement();
    state.syncLayoutCoordinates.and.callFake(() => {
      state.anchorTile = anchor;
    });
    state.syncCoordinates();
    expect(state.alignToElement).toHaveBeenCalledWith(anchor);
  });

  it('rotates only eligible units onto valid placements', () => {
    const state = linkedController();
    spyOn(state, 'placeAt').and.returnValue([{ row: 1, column: 2 }]);
    spyOn(state, 'setRotation');
    spyOn(state, 'setLayoutPreview');

    state.unit = undefined;
    state.onDoubleClick();
    state.unit = unit();
    state.locked = true;
    state.onDoubleClick();
    state.locked = false;
    state.unit = unit({ size: 1 });
    state.onDoubleClick();
    state.unit = unit();
    state.anchorTile = undefined;
    state.onDoubleClick();
    expect(state.placeAt).not.toHaveBeenCalled();

    state.anchorTile = new FakeElement();
    state.onDoubleClick();
    expect(state.placeAt).not.toHaveBeenCalled();

    state.anchorTile.dataset = { row: '1', column: '2' };
    state.onDoubleClick();
    expect(state.layout.orientation).toBe('VERTICAL');
    expect(state.setRotation).toHaveBeenCalledOnceWith();
    expect(state.setLayoutPreview).toHaveBeenCalledWith([
      { row: 1, column: 2 },
    ]);

    state.placeAt.and.returnValue(undefined);
    state.onDoubleClick();
    expect(state.setRotation).toHaveBeenCalledOnceWith();
  });

  it('resolves native layout anchors from rendered board tiles', () => {
    const state = linkedController();
    const tile = new FakeElement();
    globalThis.document.getElementById = (id) =>
      id === 'own-board-2-3' ? tile : undefined;
    const coordinates = [
      { row: 3, column: 3 },
      { row: 2, column: 3 },
    ];
    state.syncLayoutCoordinates(coordinates);
    expect(state.layout.orientation).toBe('VERTICAL');
    expect(state.anchorTile).toBe(tile);
    expect(state.layout.visible).toBe(true);
    state.setLayoutPreview([{ row: 2, column: 3 }]);
    expect(state.anchorTile).toBe(tile);
    state.setLayoutPreview([]);
    expect(state.anchorTile).toBeUndefined();
    state.syncLayoutCoordinates([]);
    expect(state.layout.visible).toBe(false);
    state.clearLayoutPreview();
    expect(state.layout.previewCoordinates).toEqual([]);
  });

  it('starts pointer drags only for unlocked primary input', () => {
    const state = linkedController();
    spyOn(state, 'getUnitCoordinates').and.returnValue({ left: 10, top: 20 });

    state.unit = undefined;
    expect(state.startDrag(pointer({ button: 0 }))).toBe(false);
    state.unit = unit();
    state.locked = true;
    expect(state.startDrag(pointer({ button: 0 }))).toBe(false);
    state.locked = false;
    expect(state.startDrag(pointer({ button: 1 }))).toBe(false);
    expect(state.startDrag(pointer({ button: undefined }))).toBe(true);
    expect(state.drag.active).toBe(true);
    expect(state.drag.pointerId).toBe(7);
    expect(state.drag.shiftX).toBe(40);
    expect(state.drag.shiftY).toBe(40);

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
    spyOn(state, 'moveUnit');
    spyOn(state, 'finishDrag');
    const target = new FakeElement();
    spyOn(target, 'releasePointerCapture');

    state.onPointerMove(pointer());
    state.onPointerUp(pointer());
    state.onPointerCancel(pointer());
    expect(state.moveUnit).not.toHaveBeenCalled();
    expect(state.finishDrag).not.toHaveBeenCalled();

    state.drag = started(pointer(), { left: 0, top: 0 });
    state.onPointerMove(pointer());
    state.onPointerUp(pointer({ currentTarget: target }));
    state.onPointerUp(pointer({ currentTarget: null }));
    state.onPointerCancel(pointer({ currentTarget: target }));
    state.onPointerCancel(pointer({ currentTarget: null }));
    expect(state.moveUnit).toHaveBeenCalledTimes(2);
    expect(target.releasePointerCapture).toHaveBeenCalledTimes(2);
    expect(target.releasePointerCapture).toHaveBeenCalledWith(7);
    expect(state.finishDrag).toHaveBeenCalledTimes(4);
  });

  it('batches pointer movement and uses release coordinates when the final move is coalesced', () => {
    const state = linkedController();
    state.drag = started(pointer(), { left: 0, top: 0 });
    spyOn(state, 'moveUnit');
    const first = pointer({ pageX: 100 });
    const last = pointer({ pageX: 110 });
    state.onPointerMove(first);
    state.onPointerMove(last);
    expect(frames.size).toBe(1);
    expect(state.moveUnit).not.toHaveBeenCalled();
    frames.values().next().value();
    expect(state.moveUnit).toHaveBeenCalledOnceWith(last);
    state.onPointerMove(first);
    spyOn(state, 'finishDrag');
    const released = pointer({ pageX: 130 });
    state.onPointerUp(released);
    expect(state.moveUnit).toHaveBeenCalledWith(released);
    expect(state.moveUnit).not.toHaveBeenCalledWith(first);
    expect(frames.size).toBe(0);
    state.flushPointerMove();
    state.onPointerMove(last);
    state.drag = stopped(state.drag);
    state.flushPointerMove();
    expect(state.moveUnit).toHaveBeenCalledTimes(2);
    state.drag = started(pointer(), { left: 0, top: 0 });
    state.onPointerMove(last);
    state.$onDestroy();
    expect(frames.size).toBe(0);
    expect(state.pendingPointer).toBeUndefined();
  });

  it('moves the drag element without publishing pixel positions to reactive state', () => {
    const state = linkedController();
    const originalStyle = state.style.value;
    state.drag.active = true;
    state.setPosition({ left: 45, top: 67 });
    expect(state.unitElement.style).toEqual({ left: '45px', top: '67px' });
    expect(state.style.value).toBe(originalStyle);
  });

  it('moves units over valid board previews and clears invalid previews', () => {
    const state = linkedController();
    state.drag = started(pointer(), { left: 20, top: 30 });
    spyOn(state, 'clearMapBlocks');
    spyOn(state, 'setPosition');
    spyOn(state, 'previewAt').and.returnValue(undefined);
    spyOn(state, 'resetPreviewState');
    spyOn(state, 'setLayoutPreview');

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
    tile.dataset.column = '4';
    state.moveUnit(pointer());
    state.moveUnit(pointer());
    expect(state.previewAt).toHaveBeenCalledTimes(2);
    expect(state.setLayoutPreview).toHaveBeenCalledWith(coordinates);
  });

  it('commits valid drags and restores invalid drags', () => {
    const state = linkedController();
    state.layout.previewCoordinates = [{ row: 1, column: 2 }];
    spyOn(state, 'alignToElement');
    spyOn(state, 'claimTiles');
    spyOn(state, 'setOnPlaceholder');

    state.anchorTile = new FakeElement();
    state.finishDrag();
    expect(state.claimTiles).toHaveBeenCalledOnceWith();

    state.anchorTile = undefined;
    state.finishDrag();
    state.finishDrag();
    expect(state.setOnPlaceholder).toHaveBeenCalledTimes(2);
  });

  it('delegates style, placement, cleanup, and view state', () => {
    const state = linkedController();
    const position = { left: 12, top: 34 };
    const coordinates = [{ row: 1, column: 2 }];
    spyOn(state.setup, 'clearUnit');
    spyOn(state.setup, 'clearPreview');
    spyOn(state.setup, 'commit');
    spyOn(state.setup, 'previewAt').and.returnValue(coordinates);
    spyOn(state.setup, 'placeAt').and.returnValue(coordinates);
    spyOn(state, 'clearLayoutPreview');
    state.layout.previewCoordinates = coordinates;

    state.setRotation();
    state.setPosition(position);
    state.clearMapBlocks();
    state.resetPreviewState();
    state.claimTiles();
    expect(state.previewAt('1', '2', 'VERTICAL')).toBe(coordinates);
    expect(state.placeAt('1', '2', 'VERTICAL')).toBe(coordinates);
    state.$onDestroy();

    expect(state.style.value.width).toBe('90px');
    expect(state.style.value.left).toBe('12px');
    expect(state.style.value.top).toBe('34px');
    expect(state.setup.commit).toHaveBeenCalledWith(state.unit, coordinates);

    state.locked = false;
    state.drag.active = true;
    expect(state.ariaDisabled).toBe('false');
    expect(state.cssClasses).toEqual({ locked: false, dragged: true });
    state.locked = true;
    expect(state.ariaDisabled).toBe('true');
  });

  it('uses a single-cell size before the unit binding arrives', () => {
    const state = controller();
    state.setRotation();
    expect(state.style.value.width).toBe('30px');
    expect(state.style.value.height).toBe('30px');
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
