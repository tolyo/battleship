export default {
  bindings: {
    rows: '<',
    disabled: '<',
    active: '<',
    units: '<',
    unitPlacements: '<',
    setupActive: '<',
  },
  template: `
    <board-grid
      board-id="own-board"
      tile-class="own-board-tile"
      rows="$ctrl.rows"
      disabled="$ctrl.disabled"
    ></board-grid>
    <div
      id="units"
      ng-class="{ 'active-room': $ctrl.active }"
    >
      <div
        class="place-holder"
        ng-repeat="unit in $ctrl.units"
        ng-attr-id="placeholder-{{unit.id}}"
        ng-style="{ width: unit.placeholderWidth }"
      >
        <unit
          unit="unit"
          coordinates="$ctrl.unitPlacements[unit.id]"
          locked="$ctrl.disabled"
          setup-active="$ctrl.setupActive"
        ></unit>
      </div>
    </div>
  `,
};
