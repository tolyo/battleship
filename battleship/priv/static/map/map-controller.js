import { Fleet } from "../fleet.js";
import { addTilesToBoard } from "../fleetboard.js";
import { createDomElement } from "../fleetdom.js";

export default class MapController {
    constructor() {
        this.board = document.getElementById("fleetboard");
        addTilesToBoard(this.board, "fleetboard");
        this.addFleetPlaceholders();
        this.addFleet();
    }

    addFleetPlaceholders() {
        const fleetPlaceholder = document.getElementById("fleet");
    
        Fleet.forEach((ship) => {
            const shipPlaceholder = document.createElement('div');
            shipPlaceholder.className = `place-holder`;
            shipPlaceholder.id = `placeholder-${ship.id}`;
            shipPlaceholder.style.width = `${ship.size * 30}px`;
            fleetPlaceholder.appendChild(shipPlaceholder);
        })
    }
  

    addFleet() {
        Fleet.forEach((ship) => {
            const element = createDomElement(ship);
            this.board.appendChild(element);
            this.attachShipElelmentToPlaceHolder(ship);
        })
    }

    attachShipElelmentToPlaceHolder(ship) {
        const tile = document.getElementById(`placeholder-${ship.id}`);
        const shipDom = document.getElementById(ship.id);
        shipDom.style.left = `${tile.getBoundingClientRect().left + window.scrollX}px`;
        shipDom.style.top = `${tile.getBoundingClientRect().top + window.scrollY}px`;
        // //     // set event handlers
        // shipDom.onmousedown = (e) => onmousedown(e, shipDom);
        // override default browser behavior
        shipDom.ondragstart = () => false;
        shipDom.onmouseup = () => false;
      }
      
}
