export class RoomEntryService {
  static $inject = ['roomStore', 'matchState', 'matchView'];

  /**
   * @param {import('../../room-store/room-store-service.js').RoomStoreService} roomStore
   * @param {import('./match-state-service.js').MatchStateService} matchState
   * @param {import('./match-view-service.js').MatchViewService} matchView
   */
  constructor(roomStore, matchState, matchView) {
    this.roomStore = roomStore;
    this.matchState = matchState;
    this.matchView = matchView;
  }

  /**
   * @param {import('../protocol/room-protocol.js').RoomEntry} entry
   * @param {{ updateUrl: boolean }} options
   * @returns {boolean}
   */
  enter(entry, options) {
    if (!this.matchState.enterRoom(entry)) {
      return false;
    }

    if (entry.view !== undefined) {
      this.matchView.receiveSnapshot(entry.view);
    }

    this.roomStore.enterRoom(entry.roomId, entry.playerId, {
      updateUrl: options.updateUrl,
    });

    return true;
  }
}
