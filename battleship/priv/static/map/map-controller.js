import { Fleet } from '../model/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';

export default class MapController {
  constructor() {
    this.board = document.getElementById('fleetboard');
    addTilesToBoard(this.board, 'fleetboard');
    this.addFleetPlaceholders();
    this.addFleet();
  }

  addFleetPlaceholders() {
    const fleetPlaceholder = document.getElementById('fleet');
    Fleet.forEach((ship) => ship.createPlaceHolder(fleetPlaceholder));
  }

  addFleet() {
    Fleet.forEach((ship) => ship.createOnPlaceholder());
  }
}
