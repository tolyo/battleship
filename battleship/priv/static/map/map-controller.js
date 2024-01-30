import { Fleet } from "../fleet.js";
import { addTilesToBoard } from "../fleetboard.js";

export default class MapController {
    constructor() {
        this.board = document.getElementById("fleetboard");
        addTilesToBoard(this.board, "fleetboard");
        addFleetPlaceholders();
    }
}

function addFleetPlaceholders() {

    const fleetPlaceholder = document.getElementById("fleet");

    Fleet.forEach((ship) => {
        const shipPlaceholder = document.createElement('div');
        shipPlaceholder.className = `place-holder`;
        shipPlaceholder.id = ship.id;
        shipPlaceholder.style.width = `${ship.size * 30}px`;
        fleetPlaceholder.appendChild(shipPlaceholder);
    })
}
