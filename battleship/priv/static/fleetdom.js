import { FLEET_BOARD_ID } from './constants.js';

function reset() {
  const htmlList = window.document.getElementsByClassName('ship');
  Array.from(htmlList).forEach((elem) => elem.parentNode.removeChild(elem));
}

const fleetBoardElem = document.getElementById(FLEET_BOARD_ID);
