import { elementSize } from './unit-geometry.js';

export class UnitStyleState {
  constructor() {
    this.value = {
      left: '0px',
      top: '0px',
      width: '30px',
      height: '30px',
    };
  }

  /**
   * @param {{ size: number } | undefined} unit
   * @param {'VERTICAL' | 'HORIZONTAL'} orientation
   */
  setSize(unit, orientation) {
    const { width, height } = elementSize(unit, orientation);
    this.value = {
      ...this.value,
      width,
      height,
    };
  }

  /**
   * @param {{ left: number, top: number }} position
   */
  setPosition(position) {
    this.value = {
      ...this.value,
      left: `${position.left}px`,
      top: `${position.top}px`,
    };
  }
}
