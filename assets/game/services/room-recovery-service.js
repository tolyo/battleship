export class RoomRecoveryService {
  static $inject = ['roomStore', 'matchState'];

  /**
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {import('./match-state-service.js').MatchStateService} matchState
   */
  constructor(roomStore, matchState) {
    this.roomStore = roomStore;
    this.matchState = matchState;
  }

  /**
   * @param {string} roomId
   * @param {string} status
   */
  restoreFailed(roomId, status) {
    this.roomStore.leaveRoom(roomId);
    this.matchState.returnToSetup(status);
  }
}
