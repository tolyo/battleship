import { matchViewModelFromView } from '../domain/match-view-model.js';

export class MatchViewService {
  static $inject = ['matchState'];

  /**
   * @param {import('./match-state-service.js').MatchStateService} matchState
   */
  constructor(matchState) {
    this.matchState = matchState;
  }

  /**
   * @param {unknown} view
   * @returns {boolean}
   */
  receiveSnapshot(view) {
    if (!this.matchState.playerId) {
      this.matchState.data.pendingView = view;
      return false;
    }

    const viewModel = matchViewModelFromView(view);
    if (!viewModel) {
      return false;
    }

    return this.matchState.applyViewModel(viewModel);
  }
}
