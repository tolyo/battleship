import { UnitStyleState } from './unit-style-state.js';

describe('UnitStyleState', () => {
  it('tracks unit position as css lengths', () => {
    const state = new UnitStyleState();

    state.setPosition({ left: 12, top: 34 });

    expect(state.value).toEqual({
      left: '12px',
      top: '34px',
      width: '30px',
      height: '30px',
    });
  });

  it('tracks horizontal and vertical unit sizes', () => {
    const state = new UnitStyleState();

    state.setSize({ size: 4 }, 'HORIZONTAL');

    expect(state.value).toEqual({
      left: '0px',
      top: '0px',
      width: '120px',
      height: '30px',
    });

    state.setSize({ size: 4 }, 'VERTICAL');

    expect(state.value).toEqual({
      left: '0px',
      top: '0px',
      width: '30px',
      height: '120px',
    });
  });
});
