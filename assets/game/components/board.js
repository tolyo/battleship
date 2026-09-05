export class BoardController {
  constructor() {
    this.active = false;
    /** @type {import('../domain/unit-catalog.js').Unit[]} */
    this.units = [];
    /** @type {Record<string, import('../domain/match-view-model.js').Coordinate[]>} */
    this.unitPlacements = {};
    /** @type {BoardUnitView[]} */
    this.unitViews = [];
  }

  $onChanges() {
    const placements = this.unitPlacements ?? {};
    this.unitViews = (this.units ?? []).map((unit) => ({
      id: unit.id,
      unit,
      coordinates: placements[unit.id],
      placeholderStyle: { width: unit.placeholderWidth },
    }));
  }

  get cssClasses() {
    return {
      'active-room': this.active,
    };
  }
}

/**
 * @typedef {{
 *   id: string,
 *   unit: import('../domain/unit-catalog.js').Unit,
 *   coordinates: import('../domain/match-view-model.js').Coordinate[] | undefined,
 *   placeholderStyle: { width: string }
 * }} BoardUnitView
 */

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
    <div id="units" ng-class="$ctrl.cssClasses">
      <div
        class="place-holder"
        ng-repeat="unitView in $ctrl.unitViews"
        ng-attr-id="placeholder-{{unitView.id}}"
        ng-style="unitView.placeholderStyle"
      >
        <unit
          unit="unitView.unit"
          coordinates="unitView.coordinates"
          locked="$ctrl.disabled"
          setup-active="$ctrl.setupActive"
        ></unit>
      </div>
    </div>
  `,
  controller: BoardController,
};
