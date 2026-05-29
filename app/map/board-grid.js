export class BoardGridController {
  constructor() {
    /** @type {((locals: { tile: import('../game/game-state-service.js').BoardGridTile }) => void) | undefined} */
    this.onTileClick = undefined;
  }

  /**
   * @param {import('../game/game-state-service.js').BoardGridTile} tile
   */
  tileClicked(tile) {
    if (this.onTileClick) {
      this.onTileClick({ tile });
    }
  }
}

export default {
  bindings: {
    boardId: '@',
    tileClass: '@',
    rows: '<',
    disabled: '<',
    hidden: '<',
    onTileClick: '&?',
  },
  template: `
    <div
      ng-attr-id="{{$ctrl.boardId}}"
      ng-show="!$ctrl.hidden"
      ng-class="{ disabled: $ctrl.disabled }">
      <div class="board-row" ng-repeat="row in $ctrl.rows">
        <div
          ng-repeat="tile in row"
          class="{{$ctrl.tileClass}}"
          ng-attr-id="{{$ctrl.boardId}}-{{tile.row}}-{{tile.column}}"
          ng-attr-data-row="{{tile.row}}"
          ng-attr-data-column="{{tile.column}}"
          ng-attr-data-state="{{tile.dataState}}"
          ng-click="$ctrl.tileClicked(tile)"
          ng-class="{
            placed: tile.state === 'ship',
            hit: tile.state === 'hit',
            miss: tile.state === 'miss',
            sunk: tile.sunk,
            'sunk-horizontal': tile.sunkHorizontal,
            'sunk-vertical': tile.sunkVertical,
            'sunk-single': tile.sunkSingle,
            'sunk-start': tile.sunkStart,
            'sunk-end': tile.sunkEnd
          }">
        </div>
      </div>
    </div>
  `,
  controller: BoardGridController,
};
