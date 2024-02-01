import { Fleet } from '../model/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';

export default class MapController {
  constructor() {
    this.board = document.getElementById('fleetboard');
    addTilesToBoard(this.board, 'fleetboard');
    // Add placeholders
    const fleetPlaceholder = document.getElementById('fleet');
    Fleet.forEach((ship) => ship.createPlaceHolder(fleetPlaceholder));
    // Attach ships to them
    Fleet.forEach((ship) => ship.createOnPlaceholder());
    window.Fleet = Fleet;
  }

}
