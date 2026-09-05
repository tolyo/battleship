export class RoomConnectionLifecycleService {
  static $inject = ['roomRestore', 'matchState'];

  /**
   * @param {import('./room-restore-service.js').RoomRestoreService} roomRestore
   * @param {import('./match-state-service.js').MatchStateService} matchState
   */
  constructor(roomRestore, matchState) {
    this.roomRestore = roomRestore;
    this.matchState = matchState;
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   * @param {() => void} onRestoreClose
   */
  onClose(context, onRestoreClose) {
    this.handleDisconnect(context, onRestoreClose, () => {
      this.matchState.connectionClosed();
    });
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   * @param {() => void} onRestoreClose
   */
  onError(context, onRestoreClose) {
    this.handleDisconnect(context, onRestoreClose, () => {
      this.matchState.connectionError();
    });
  }

  /**
   * @param {import('./connection-context.js').ConnectionContext} context
   * @param {() => void} onRestoreClose
   * @param {() => void} fallback
   */
  handleDisconnect(context, onRestoreClose, fallback) {
    if (this.roomRestore.handleConnectionDisconnect(context)) {
      onRestoreClose();
      return;
    }

    fallback();
  }
}
