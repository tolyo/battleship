import { OWN_BOARD_ID, GRID_SIZE } from '../domain/constants.js';

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
 * @param {Orientation} orientation
 * @returns {ElementSize}
 */
export function elementSize(unit, orientation) {
  if (!unit) {
    return { width: `${GRID_SIZE}px`, height: `${GRID_SIZE}px` };
  }

  return {
    width:
      orientation === 'HORIZONTAL'
        ? `${GRID_SIZE * unit.size}px`
        : `${GRID_SIZE}px`,
    height:
      orientation === 'VERTICAL'
        ? `${GRID_SIZE * unit.size}px`
        : `${GRID_SIZE}px`,
  };
}

/**
 * @param {Coordinate[]} coordinates
 * @returns {Orientation}
 */
export function orientationFromCoordinates(coordinates) {
  if (coordinates.length === 0) {
    return 'HORIZONTAL';
  }

  return coordinates.every(
    (coordinate) => coordinate.row === coordinates[0].row
  )
    ? 'HORIZONTAL'
    : 'VERTICAL';
}

/**
 * @param {Coordinate[]} coordinates
 * @returns {Coordinate[]}
 */
export function sortedCoordinates(coordinates) {
  return [...coordinates].sort((left, right) =>
    left.row === right.row ? left.column - right.column : left.row - right.row
  );
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
  const elementBelow = document.elementFromPoint(x + 15, y + 15);
  if (
    elementBelow instanceof HTMLElement &&
    elementBelow.classList.contains('own-board-tile')
  ) {
    return elementBelow;
  }

  return undefined;
}
