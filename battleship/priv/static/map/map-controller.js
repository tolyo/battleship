import { Fleet } from '../model/fleet.js';
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
}
