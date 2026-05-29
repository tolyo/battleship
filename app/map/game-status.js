export default {
  bindings: {
    state: '<',
  },
  template: `
    <section class="player-panel">
      <span ng-bind="$ctrl.state.playerLabel"></span>
      <strong id="match-status">{{$ctrl.state.status}}</strong>
    </section>
  `,
};
