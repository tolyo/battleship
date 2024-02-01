import { GRID } from './constants.js';
import { Fleet } from './model/fleet.js';
import Ship from './model/ship.js';
import { MapTile } from './strikemap.js';

export function clearPlacedGrids() {
  const htmlList = window.document.getElementsByClassName('placed');
  const elemList = Array.from(htmlList);
  elemList.forEach((elem) => elem.classList.remove('placed'));
}

export function placeFleet() {
  clearPlacedGrids();
  Fleet.forEach((ship) => placeShip(ship));
}

/**
 * Marks
 * @param {Ship} ship to place
 */
export function placeShip(ship) {
  ship.getShipMapCoordinates().forEach(({ row, column }) => {
    applyClassToGrid(row, column, 'placed');
  });
}

/**
 * Marks
 * @param {Ship} ship to place
 */
export function removeShip(ship) {
  ship.getShipMapCoordinates().forEach(({ row, column }) => {
    removeClassFromGrid(row, column, 'placed');
  });
}

export function applyClassToGrid(row, column, className) {
  const elem = window.document.getElementById(`fleetboard-${row}-${column}`);
  elem.classList.add(className);
}

export function removeClassFromGrid(row, column, className) {
  const elem = window.document.getElementById(`fleetboard-${row}-${column}`);
  elem.classList.remove(className);
}

export function reset() {
  clearPlacedGrids();
  Fleet.forEach((ship) => ship.reset()); // reset every chpm
}

/**
 * @function addTilesToBoard
 * @param {HTMLElement} elem
 * @param {string} boardname
 * @return
 */
export function addTilesToBoard(elem, boardname) {
  GRID.forEach((y) => {
    // create row
    const tileRow = document.createElement('div');
    tileRow.className = 'board-row';
    elem.appendChild(tileRow);

    // create tiles
    GRID.forEach((x) => {
      const tile = document.createElement('div');
      tile.className = `${boardname}-tile`;
      tile.id = `${boardname}-${y}-${x}`;
      tile.dataset.row = y.toString();
      tile.dataset.column = x.toString();
      tile.dataset.state = MapTile.EMPTY;
      tileRow.appendChild(tile);
    });
  });
}
