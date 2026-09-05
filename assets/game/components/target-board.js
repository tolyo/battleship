export default {
  bindings: {
    rows: '<',
    hidden: '<',
    visible: '<',
    disabled: '<',
    onSubmit: '&',
  },
  template: `
    <board-grid
      board-id="target-board"
      tile-class="target-board-tile"
      rows="$ctrl.rows"
      hidden="$ctrl.hidden"
      visible="$ctrl.visible"
      disabled="$ctrl.disabled"
      on-tile-click="$ctrl.onSubmit({ tile: tile })"
    ></board-grid>
  `,
};
