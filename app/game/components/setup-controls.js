export default {
  bindings: {
    setupActive: '<',
    boardReady: '<',
    onReady: '&',
    onRandom: '&',
    onReset: '&',
  },
  template: `
    <div id="setup-controls" ng-show="$ctrl.setupActive">
      <button
        id="ready"
        ng-click="$ctrl.onReady()"
        ng-disabled="!$ctrl.boardReady || !$ctrl.setupActive"
      >
        Ready
      </button>

      <button
        id="random"
        ng-disabled="!$ctrl.setupActive"
        ng-click="$ctrl.onRandom()"
      >
        Random
      </button>

      <button
        id="reset"
        ng-disabled="!$ctrl.setupActive"
        ng-click="$ctrl.onReset()"
      >
        Reset
      </button>
    </div>
  `,
};
