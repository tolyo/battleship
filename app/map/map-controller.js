/* eslint-disable class-methods-use-this */
import Fleet from '../model/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';
import { FLEET_SIZE, GRID } from '../constants.js';

class MapController {
  static $inject = ['$scope'];

  constructor($scope) {
    this.$scope = $scope;
    this.boardReady = false;
    this.board = document.getElementById('fleetboard');
    this.button = document.getElementById('ready');
    addTilesToBoard(this.board, 'fleetboard');
    // Add placeholders
    const fleetPlaceholder = document.getElementById('fleet');
    Fleet.forEach((ship) => ship.createPlaceHolder(fleetPlaceholder));
    // Attach ships to them
    Fleet.forEach((ship) => ship.createOnPlaceholder());

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
        const y = elem.dataset.row;
        const x = elem.dataset.column;
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
    console.log(this.player);
    console.log(this.board);
    const socket = new WebSocket(
      `/ws?player=${this.player}&board=${this.boardState}`
    );
    socket.addEventListener('open', (ev, data) => {
      console.log(ev);
      console.log(data);
    });
    socket.addEventListener('message', (ev, data) => {
      console.log(ev);
      console.log(data);
    });
  }
}

export default {
  templateUrl: '/static/map/map.html',
  controller: MapController,
};
