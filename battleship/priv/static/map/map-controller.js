/* eslint-disable class-methods-use-this */
import Fleet from '../model/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';
import { FLEET_SIZE } from '../constants.js';

export default class MapController {
  constructor() {
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

    window.Fleet = Fleet;
  }

  handleChildChanges() {
    const count = Fleet.reduce(
      (accumulator, ship) => accumulator + ship.elementsBelow.length,
      0
    );
    if (count === FLEET_SIZE) {
      this.button.removeAttribute('hidden');
    } else {
      this.button.setAttribute('hidden', 'true');
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
}
