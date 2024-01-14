import { GRID } from './constants';
import Fleet from './fleet';
import { Ship } from './ship';

export default (() => {
  const createBoard = (id) => {
    if (id === undefined) throw new Error('board id not provided');
    const elem = window.document.getElementById(id);
    if (elem === null) throw new Error('board element not found');
    addTilesToBoard(elem, 'fleetboard');

    // pubsub.subscribe(BOARD_EVENTS.REMOVE_SHIP, (shipDom) => {
    //   removeShip(Fleet[parseInt(shipDom.id)]);
    // });
  };

  const clearPlacedGrids = () => {
    const htmlList = window.document.getElementsByClassName('placed');
    const elemList = Array.from(htmlList);
    elemList.forEach((elem) => elem.classList.remove('placed'));
  };

  const placeFleet = () => {
    clearPlacedGrids();
    Fleet.forEach((ship) => placeShip(ship));
  };

  /**
   * Marks
   * @param {Ship} ship to place
   */
  const placeShip = (ship) => {
    ship.getShipMapCoordinates().forEach(({ row, column }) => {
      applyClassToGrid(row, column, 'placed');
    });
  };

  /**
   * Marks
   * @param {Ship} ship to place
   */
  const removeShip = (ship) => {
    ship.getShipMapCoordinates().forEach(({ row, column }) => {
      removeClassFromGrid(row, column, 'placed');
    });
  };

  const applyClassToGrid = (row, column, className) => {
    const elem = window.document.getElementById(`fleetboard-${row}-${column}`);
    elem.classList.add(className);
  };

  const removeClassFromGrid = (row, column, className) => {
    const elem = window.document.getElementById(`fleetboard-${row}-${column}`);
    elem.classList.remove(className);
  };

  const reset = () => {
    clearPlacedGrids();
    Fleet.forEach((ship) => ship.reset()); // reset every chpm
  };

  return {
    createBoard,
    placeFleet,
    reset,
  };
})();

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
      tile.setAttribute('data-column', y.toString());
      tile.setAttribute('data-row', x.toString());
      tileRow.appendChild(tile);
    });
  });
}
