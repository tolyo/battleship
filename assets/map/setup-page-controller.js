export class SetupPageController {
  static $inject = ['matchState', 'roomClient', 'setup'];

  /**
   * @param {import('../game/services/match-state-service.js').MatchStateService} matchState
   * @param {import('../game/services/room-client-service.js').RoomClientService} roomClient
   * @param {import('../game/services/setup-service.js').SetupService} setup
   */
  constructor(matchState, roomClient, setup) {
    this.state = matchState;
    this.roomClient = roomClient;
    this.setup = setup;
  }

  $onInit() {
    this.roomClient.restoreRoomFromCurrentUrl();
  }

  $onDestroy() {
    this.roomClient.close();
  }
}

export default {
  templateUrl: '/static/map/map.html',
  controller: SetupPageController,
};
