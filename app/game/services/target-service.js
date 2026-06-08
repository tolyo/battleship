import { moveForTarget } from '../domain/room-state.js';

export class TargetService {
  static $inject = ['matchState'];

  /**
   * @param {import('./match-state-service.js').MatchStateService} matchState
   */
  constructor(matchState) {
    this.matchState = matchState;
  }

  /**
   * @param {import('../domain/board-rows.js').BoardGridTile} tile
   * @returns {{ row: number, column: number } | undefined}
   */
  moveForTile(tile) {
    return moveForTarget(
      this.matchState.phase,
      this.matchState.canSubmitMove,
      tile
    );
  }
}
