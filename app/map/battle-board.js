class BattleBoardController {
  constructor() {
    /** @type {((locals: { tile: import('../game/board-rows.js').BoardGridTile }) => void) | undefined} */
    this.onStrike = undefined;
  }

  /**
   * @param {import('../game/board-rows.js').BoardGridTile} tile
   */
  strike(tile) {
    this.onStrike?.({ tile });
  }
}

export default {
  bindings: {
    state: '<',
    onStrike: '&',
  },
  template: `
    <board-grid
      board-id="hitboard"
      tile-class="hitboard-tile"
      rows="$ctrl.state.hitRows"
      hidden="$ctrl.state.phase !== 'playing'"
      disabled="!$ctrl.state.canStrike"
      on-tile-click="$ctrl.strike(tile)"
    ></board-grid>
  `,
  controller: BattleBoardController,
};
