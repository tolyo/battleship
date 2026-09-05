export default {
  bindings: {
    playerLabel: '<',
    status: '<',
  },
  template: `
    <section class="player-panel">
      <span ng-bind="$ctrl.playerLabel"></span>
      <strong id="match-status">{{$ctrl.status}}</strong>
    </section>
  `,
};
