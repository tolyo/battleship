export class UnitDragState {
  constructor() {
    this.active = false;
    /** @type {number | undefined} */
    this.pointerId = undefined;
    this.firstMove = false;
    this.shiftX = 0;
    this.shiftY = 0;
  }

  /**
   * @param {PointerEvent} event
   * @param {{ left: number, top: number }} origin
   */
  start(event, origin) {
    this.active = true;
    this.pointerId = event.pointerId;
    this.firstMove = true;
    this.shiftX = event.pageX - origin.left;
    this.shiftY = event.pageY - origin.top;
  }

  /**
   * @param {PointerEvent} event
   * @returns {boolean}
   */
  owns(event) {
    return this.active && event.pointerId === this.pointerId;
  }

  /**
   * @param {PointerEvent} event
   * @returns {{ left: number, top: number }}
   */
  position(event) {
    return {
      left: Math.floor(event.pageX - this.shiftX),
      top: Math.floor(event.pageY - this.shiftY),
    };
  }

  consumeFirstMove() {
    const { firstMove } = this;
    this.firstMove = false;
    return firstMove;
  }

  stop() {
    this.active = false;
    this.pointerId = undefined;
    this.firstMove = false;
  }
}
