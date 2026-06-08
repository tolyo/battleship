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
      <div class="board-row" ng-repeat="row in $ctrl.rows" data-index="id">
        <div
          ng-repeat="tile in row"
          data-index="id"
          class="{{$ctrl.tileClass}}"
          ng-attr-id="{{tile.id}}"
          ng-attr-data-row="{{tile.row}}"
          ng-attr-data-column="{{tile.column}}"
          ng-attr-data-state="{{tile.dataState}}"
          ng-click="$ctrl.onTileClick && $ctrl.onTileClick({ tile: tile })"
          ng-class="tile.classes">
        </div>
      </div>
    </div>
  `,
};
