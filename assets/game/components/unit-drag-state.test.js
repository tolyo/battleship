import { UnitDragState } from './unit-drag-state.js';

describe('UnitDragState', () => {
  it('tracks the active pointer and first move', () => {
    const state = new UnitDragState();
    const event = pointerEvent({ pointerId: 7, pageX: 40, pageY: 60 });

    state.start(event, { left: 10, top: 20 });

    expect(state.active).toBe(true);
    expect(state.pointerId).toBe(7);
    expect(state.owns(event)).toBe(true);
    expect(state.owns(pointerEvent({ pointerId: 8 }))).toBe(false);
    expect(state.consumeFirstMove()).toBe(true);
    expect(state.consumeFirstMove()).toBe(false);
  });

  it('computes dragged element positions from pointer offsets', () => {
    const state = new UnitDragState();

    state.start(pointerEvent({ pageX: 40, pageY: 60 }), { left: 10, top: 20 });

    expect(state.position(pointerEvent({ pageX: 45.8, pageY: 67.2 }))).toEqual({
      left: 15,
      top: 27,
    });
  });

  it('clears active state on stop', () => {
    const state = new UnitDragState();

    state.start(pointerEvent({ pointerId: 7 }), { left: 0, top: 0 });
    state.stop();

    expect(state.active).toBe(false);
    expect(state.pointerId).toBeUndefined();
    expect(state.consumeFirstMove()).toBe(false);
  });
});

function pointerEvent(overrides = {}) {
  return {
    pointerId: 1,
    pageX: 0,
    pageY: 0,
    ...overrides,
  };
}
