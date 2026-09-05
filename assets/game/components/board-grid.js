export class BoardGridController {
  constructor() {
    /** @type {boolean | undefined} */
    this.visible = undefined;
    this.hidden = false;
    this.disabled = false;
    /** @type {((locals: { tile: import('../domain/board-rows.js').BoardGridTile }) => void) | undefined} */
    this.onTileClick = undefined;
  }

  get isVisible() {
    if (this.visible !== undefined) {
      return this.visible;
    }

    return !this.hidden;
  }

  get cssClasses() {
    return {
      disabled: this.disabled,
    };
  }

  /**
   * @param {import('../domain/board-rows.js').BoardGridTile} tile
   */
  submitTile(tile) {
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
    visible: '<',
    onTileClick: '&?',
  },
  template: `
    <div
      ng-attr-id="{{$ctrl.boardId}}">
      <div class="board-row" ng-repeat="row in $ctrl.rows">
        <div
          ng-repeat="tile in row"
          class="{{$ctrl.tileClass}}"
          ng-attr-id="{{tile.id}}"
          ng-attr-data-row="{{tile.row}}"
          ng-attr-data-column="{{tile.column}}"
          ng-attr-data-state="{{tile.dataState}}"
          ng-click="$ctrl.submitTile(tile)"
          ng-class="tile.classes">
        </div>
      </div>
    </div>
  `,
  controller: BoardGridController,
};
