export class OperationTimeout {
  constructor() {
    /** @type {number | undefined} */
    this.timerId = undefined;
  }

  /**
   * @param {() => void} callback
   * @param {number} delay
   */
  start(callback, delay) {
    this.clear();
    this.timerId = window.setTimeout(callback, delay);
  }

  clear() {
    if (this.timerId !== undefined) {
      window.clearTimeout(this.timerId);
      this.timerId = undefined;
    }
  }
}
