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
   * @param {Record<string, import('../map/game-view-model.js').Coordinate[]>} shipCoordinatesById
   */
  placeFleetShips(shipCoordinatesById) {
    this.fleetShips().forEach((fleetShip) => {
      if (!fleetShip.ship) {
        return;
      }

      const liveCoordinates = shipCoordinatesById[fleetShip.ship.id] ?? [];
      const coordinates =
        liveCoordinates.length > 0
          ? liveCoordinates
          : fleetShip.elementsBelow.map((tile) => ({
              row: Number(tile.dataset.row),
              column: Number(tile.dataset.column),
            }));

      fleetShip.placeOnBoardCoordinates(
        coordinates.filter(
          (coordinate) =>
            Number.isInteger(coordinate.row) &&
            Number.isInteger(coordinate.column)
        )
      );
    });
  }
}
