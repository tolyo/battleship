import { FLEET_BOARD_ID, GRID_SIZE } from './constants.js';

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} ShipOrientation
 */

/**
 * @typedef {{ row: number, column: number }} ShipCoordinate
 */

/**
 * @typedef {{ width: string, height: string }} ShipElementSize
 */

/**
 * @param {HTMLElement} element
 * @returns {{ left: number, top: number }}
 */
export function elementPagePosition(element) {
  const box = element.getBoundingClientRect();
  return {
    left: box.left + window.scrollX,
    top: box.top + window.scrollY,
  };
}

/**
 * @param {HTMLElement} element
 * @param {{ left: number, top: number }} position
 */
export function setElementPosition(element, position) {
  element.style.left = `${position.left}px`;
  element.style.top = `${position.top}px`;
}

/**
 * @param {HTMLElement} shipElement
 * @param {HTMLElement} target
 */
export function alignElementTo(shipElement, target) {
  setElementPosition(shipElement, elementPagePosition(target));
}

/**
 * @param {HTMLElement} element
 * @returns {{ row: string, column: string } | undefined}
 */
export function tileDataset(element) {
  const { row, column } = element.dataset;
  if (!row || !column) {
    return undefined;
  }

  return { row, column };
}

/**
 * @param {HTMLElement[]} elements
 * @returns {ShipCoordinate[]}
 */
export function coordinatesFromElements(elements) {
  return elements
    .map((element) => ({
      row: Number(element.dataset.row),
      column: Number(element.dataset.column),
    }))
    .filter(
      (coordinate) =>
        Number.isInteger(coordinate.row) &&
        Number.isInteger(coordinate.column)
    );
}

/**
 * @param {{ size: number } | undefined} ship
 * @param {ShipOrientation} orientation
 * @returns {ShipElementSize}
 */
export function shipElementSize(ship, orientation) {
  if (!ship) {
    return { width: `${GRID_SIZE}px`, height: `${GRID_SIZE}px` };
  }

  return {
    width:
      orientation === 'HORIZONTAL'
        ? `${GRID_SIZE * ship.size}px`
        : `${GRID_SIZE}px`,
    height:
      orientation === 'VERTICAL'
        ? `${GRID_SIZE * ship.size}px`
        : `${GRID_SIZE}px`,
  };
}

/**
 * @param {ShipCoordinate[]} coordinates
 * @returns {ShipOrientation}
 */
export function orientationFromCoordinates(coordinates) {
  if (coordinates.length === 0) {
    return 'HORIZONTAL';
  }

  return coordinates.every((coordinate) => coordinate.row === coordinates[0].row)
    ? 'HORIZONTAL'
    : 'VERTICAL';
}

/**
 * @param {ShipCoordinate[]} coordinates
 * @returns {ShipCoordinate[]}
 */
export function sortedCoordinates(coordinates) {
  return [...coordinates].sort((left, right) =>
    left.row === right.row ? left.column - right.column : left.row - right.row
  );
}

/**
 * @param {ShipCoordinate} coordinate
 * @returns {HTMLElement | undefined}
 */
export function fleetBoardTileAt(coordinate) {
  const tile = document.getElementById(
    `${FLEET_BOARD_ID}-${coordinate.row}-${coordinate.column}`
  );

  return tile instanceof HTMLElement ? tile : undefined;
}

/**
 * @param {string} row
 * @param {string} column
 * @param {ShipOrientation} orientation
 * @param {number} size
 * @returns {HTMLElement[]}
 */
export function fleetBoardTilesForShip(row, column, orientation, size) {
  const y = parseInt(row, 10);
  const x = parseInt(column, 10);
  if (!Number.isInteger(y) || !Number.isInteger(x)) {
    return [];
  }

  return Array.from({ length: size }, (_, offset) =>
    fleetBoardTileAt({
      row: orientation === 'VERTICAL' ? y + offset : y,
      column: orientation === 'HORIZONTAL' ? x + offset : x,
    })
  ).filter((tile) => tile instanceof HTMLElement);
}

/**
 * @param {number} x
 * @param {number} y
 * @returns {HTMLElement | undefined}
 */
export function fleetBoardTileFromPoint(x, y) {
  const elementBelow = document.elementFromPoint(x + 15, y + 15);
  if (
    elementBelow instanceof HTMLElement &&
    elementBelow.classList.contains('fleetboard-tile')
  ) {
    return elementBelow;
  }

  return undefined;
}
