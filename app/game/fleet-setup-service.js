export class FleetSetupService {
  static $inject = ['gameState', 'fleetLayout'];

  /**
   * @param {import('./game-state-service.js').GameStateService} gameState
   * @param {import('./fleet-layout-service.js').FleetLayoutService} fleetLayout
   */
  constructor(gameState, fleetLayout) {
    this.gameState = gameState;
    this.fleetLayout = fleetLayout;
  }

  randomize() {
    this.gameState.randomizeFleet();
    this.fleetLayout.placeFleetShips(this.gameState.shipPlacements);
  }

  reset() {
    this.gameState.resetSetupFleet();
    this.fleetLayout.resetFleetToPlaceholders();
  }
}
