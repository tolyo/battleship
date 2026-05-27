import Fleet from '../game/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';
import { FLEET_SIZE, GRID } from '../game/constants.js';

class MapController {
  static $inject = ['$scope'];

  /**
   * @param {ng.RootScopeService} $scope
   */
  constructor($scope) {
    this.$scope = $scope;
    this.boardReady = false;
    /** @type {HTMLDivElement} */
    this.board = /** @type {HTMLDivElement} */ (
      document.getElementById('fleetboard')
    );
    /** @type {HTMLButtonElement} */
    this.button = /** @type {HTMLButtonElement} */ (
      document.getElementById('ready')
    );
    addTilesToBoard(this.board, 'fleetboard');
    // Add placeholders
    /** @type {HTMLDivElement} */
    const fleetPlaceholder = /** @type {HTMLDivElement} */ (
      document.getElementById('fleet')
    );
    Fleet.forEach((ship) => ship.createPlaceHolder(fleetPlaceholder));
    // Attach ships to them
    Fleet.forEach((ship) => ship.createOnPlaceholder());

    /** @type {string[][]} */
    this.boardState = GRID.map(() => GRID.map(() => '_'));
    /** @type {string | undefined} */
    this.player = undefined;

    const observer = new MutationObserver(() => this.handleChildChanges());
    // Start observing the parent node for childList mutations
    observer.observe(this.board, {
      attributes: true,
      childList: true,
      subtree: true,
    });
  }

  handleChildChanges() {
    let count = 0;
    // We reset the board state each time. Maybe there is a more efficient way of doing this
    this.boardState = GRID.map(() => GRID.map(() => '_'));
    Fleet.forEach((ship) => {
      ship.elementsBelow.forEach((elem) => {
        const y = Number(elem.dataset.row);
        const x = Number(elem.dataset.column);
        if (!Number.isInteger(y) || !Number.isInteger(x)) {
          return;
        }
        this.boardState[y][x] = ship.id;
      });
      count += ship.size;
    });
    if (count === FLEET_SIZE) {
      this.boardReady = true;
    } else {
      this.boardReady = false;
    }
  }

  random() {
    try {
      this.tryPlacingShips();
    } catch (e) {
      this.random();
    }
  }

  tryPlacingShips() {
    this.reset();
    Fleet.forEach((ship) => {
      let count = 100; // safety to prevent runaway cycle
      let res = ship.tryRandomLocation();
      while (res === false) {
        res = ship.tryRandomLocation();
        count -= 1;
        if (count === 0) {
          throw new Error('Count exceeded');
        }
      }
    });
  }

  reset() {
    Fleet.forEach((ship) => ship.setOnPlaceholder());
  }

  join() {
    // Send a JSON-encoded board snapshot for matchmaking.
    const boardParam = encodeURIComponent(JSON.stringify(this.boardState));
    const playerParam = encodeURIComponent(this.player || 'player');
    const socket = new WebSocket(
      `/ws?player=${playerParam}&board=${boardParam}`
    );
    socket.addEventListener('open', (ev) => {
      // eslint-disable-next-line no-console
      console.log(ev);
    });
    socket.addEventListener('message', (ev) => {
      // eslint-disable-next-line no-console
      console.log(ev.data);
    });
  }
}

export default {
  templateUrl: '/static/map/map.html',
  controller: MapController,
};
