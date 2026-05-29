export default {
  bindings: {
    state: '<',
  },
  template: `
    <board-grid
      board-id="fleetboard"
      tile-class="fleetboard-tile"
      rows="$ctrl.state.fleetRows"
      disabled="$ctrl.state.phase === 'playing'"
    ></board-grid>
    <div
      id="fleet"
      ng-class="{ 'room-active': $ctrl.state.phase === 'playing' }"
    >
      <div
        class="place-holder"
        ng-repeat="ship in $ctrl.state.fleet"
        ng-attr-id="placeholder-{{ship.id}}"
        ng-style="{ width: ship.placeholderWidth }"
      >
        <fleet-ship ship="ship"></fleet-ship>
      </div>
    </div>
  `,
};
