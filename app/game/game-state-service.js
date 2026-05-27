export class GameStateService {
  constructor() {
    this.fleetLocked = false;
    /** @type {Set<(locked: boolean) => void>} */
    this.fleetLockListeners = new Set();
    /** @type {Set<import('./fleet-ship.js').FleetShipController>} */
    this.fleetShipControllers = new Set();
  }

  /**
   * @param {boolean} locked
   */
  setFleetLocked(locked) {
    if (this.fleetLocked === locked) {
      return;
    }

    this.fleetLocked = locked;
    this.fleetLockListeners.forEach((listener) => listener(locked));
  }

  /**
   * @param {(locked: boolean) => void} listener
   * @returns {() => void}
   */
  subscribeFleetLock(listener) {
    this.fleetLockListeners.add(listener);
    listener(this.fleetLocked);

    return () => {
      this.fleetLockListeners.delete(listener);
    };
  }

  /**
   * @param {import('./fleet-ship.js').FleetShipController} controller
   * @returns {() => void}
   */
  registerFleetShip(controller) {
    this.fleetShipControllers.add(controller);

    return () => {
      this.fleetShipControllers.delete(controller);
    };
  }

  /**
   * @returns {import('./fleet-ship.js').FleetShipController[]}
   */
  fleetShips() {
    return Array.from(this.fleetShipControllers).sort(
      (left, right) => Number(left.ship?.id ?? 0) - Number(right.ship?.id ?? 0)
    );
  }
}
