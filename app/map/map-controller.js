class MapController {
  static $inject = ['gameState', 'battleRoomClient', 'fleetSetup'];

  /**
   * @param {import('../game/game-state-service.js').GameStateService} gameState
   * @param {import('../game/battle-room-client-service.js').BattleRoomClientService} battleRoomClient
   * @param {import('../game/fleet-setup-service.js').FleetSetupService} fleetSetup
   */
  constructor(gameState, battleRoomClient, fleetSetup) {
    this.gameState = gameState;
    this.battleRoomClient = battleRoomClient;
    this.fleetSetup = fleetSetup;
  }

  $onInit() {
    this.battleRoomClient.restoreRoomFromCurrentUrl();
  }

  $onDestroy() {
    this.battleRoomClient.close();
  }

  random() {
    this.fleetSetup.randomize();
  }

  reset() {
    this.fleetSetup.reset();
  }

  join() {
    this.battleRoomClient.joinLobby();
  }

  /**
   * @param {import('../game/game-state-service.js').BoardGridTile} tile
   */
  strike(tile) {
    this.battleRoomClient.strike(tile);
  }
}

export default {
  templateUrl: '/static/map/map.html',
  controller: MapController,
};
