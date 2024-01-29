import { addTilesToBoard } from "./fleetboard.js";

export default class MapController {
    constructor() {
        this.board = document.getElementById("fleetboard");
        addTilesToBoard(this.board, "fleetboard");
    }
}