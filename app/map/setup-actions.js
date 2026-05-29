class SetupActionsController {
  constructor() {
    /** @type {(() => void) | undefined} */
    this.onReady = undefined;
    /** @type {(() => void) | undefined} */
    this.onRandom = undefined;
    /** @type {(() => void) | undefined} */
    this.onReset = undefined;
  }

  ready() {
    this.onReady?.();
  }

  random() {
    this.onRandom?.();
  }

  reset() {
    this.onReset?.();
  }
}

export default {
  bindings: {
    state: '<',
    onReady: '&',
    onRandom: '&',
    onReset: '&',
  },
  template: `
    <div id="setup-actions" ng-show="$ctrl.state.phase === 'setup'">
      <button
        id="ready"
        ng-click="$ctrl.ready()"
        ng-disabled="!$ctrl.state.boardReady || $ctrl.state.phase !== 'setup'"
      >
        Ready
      </button>

      <button
        id="random"
        ng-disabled="$ctrl.state.phase !== 'setup'"
        ng-click="$ctrl.random()"
      >
        Random
      </button>

      <button
        id="reset"
        ng-disabled="$ctrl.state.phase !== 'setup'"
        ng-click="$ctrl.reset()"
      >
        Reset
      </button>
    </div>
  `,
  controller: SetupActionsController,
};
