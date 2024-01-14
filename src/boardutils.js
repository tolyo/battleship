import { GRID } from "./constants";

/**
 * @function addTilesToBoard
 * @param {HTMLElement} elem
 * @param {string} boardname
 * @return
 */
export function addTilesToBoard(elem, boardname) {
  GRID.forEach((y) => {
    // create row
    const tileRow = document.createElement("div");
    tileRow.className = "board-row";
    elem.appendChild(tileRow);

    // create tiles
    GRID.forEach((x) => {
      const tile = document.createElement("div");
      tile.className = `${boardname}-tile`;
      tile.id = `${boardname}-${y}-${x}`;
      tile.setAttribute("data-column", y.toString());
      tile.setAttribute("data-row", x.toString());
      tileRow.appendChild(tile);
    });
  });
}
