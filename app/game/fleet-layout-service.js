export class FleetLayoutService {
  constructor() {
    /** @type {Set<import('./fleet-ship.js').FleetShipController>} */
    this.fleetShipControllers = new Set();
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

  resetFleetToPlaceholders() {
    this.fleetShips().forEach((fleetShip) => {
      fleetShip.setOnPlaceholder();
    });
  }

  /**
   * @param {Record<string, import('./game-view-model.js').Coordinate[]>} shipCoordinatesById
   */
  placeFleetShips(shipCoordinatesById) {
    this.fleetShips().forEach((fleetShip) => {
      if (!fleetShip.ship) {
        return;
      }

      fleetShip.placeOnBoardCoordinates(
        (shipCoordinatesById[fleetShip.ship.id] ?? []).filter(
          (coordinate) =>
            Number.isInteger(coordinate.row) &&
            Number.isInteger(coordinate.column)
        )
      );
    });
  }
}
