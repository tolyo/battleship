import { OWN_BOARD_ID } from '../domain/constants.js';
import {
  element_size as elementSizeModel,
  orientation,
  sorted_coordinates as sortedCoordinatesModel,
} from '../../../_build/browser/js/modules/battleship/browser/UnitGeometry.js';

/**
 * @typedef {'VERTICAL' | 'HORIZONTAL'} Orientation
 */

/**
 * @typedef {{ row: number, column: number }} Coordinate
 */

/**
 * @typedef {{ width: string, height: string }} ElementSize
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
 * @param {{ size: number } | undefined} unit
 * @param {Orientation} direction
 * @returns {ElementSize}
 */
export function elementSize(unit, direction) {
  return elementSizeModel(unit?.size ?? 1, direction);
}

/**
 * @param {Coordinate[]} coordinates
 * @returns {Orientation}
 */
export function orientationFromCoordinates(coordinates) {
  return /** @type {Orientation} */ (orientation(coordinates));
}

/**
 * @param {Coordinate[]} coordinates
 * @returns {Coordinate[]}
 */
export function sortedCoordinates(coordinates) {
  return sortedCoordinatesModel(coordinates);
}

/**
 * @param {Coordinate} coordinate
 * @returns {HTMLElement | undefined}
 */
export function boardTileAt(coordinate) {
  const tile = document.getElementById(
    `${OWN_BOARD_ID}-${coordinate.row}-${coordinate.column}`
  );

  return tile instanceof HTMLElement ? tile : undefined;
}

/**
 * @param {number} x
 * @param {number} y
 * @returns {HTMLElement | undefined}
 */
export function boardTileFromPoint(x, y) {
  const elementBelow = document.elementFromPoint(x + 15 - window.scrollX, y + 15 - window.scrollY);
  if (
    elementBelow instanceof HTMLElement &&
    elementBelow.classList.contains('own-board-tile')
  ) {
    return elementBelow;
  }

  return undefined;
}
