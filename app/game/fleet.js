import { GRID_SIZE } from './constants.js';

/**
 * @typedef {{ id: string, size: number, placeholderWidth: string }} FleetShip
 */

/**
 * @type {FleetShip[]}
 */
export default [
  ship('0', 4),

  ship('1', 3),
  ship('2', 3),

  // 3 destroyers
  ship('3', 2),
  ship('4', 2),
  ship('5', 2),

  // 4 torpedo boats
  ship('6', 1),
  ship('7', 1),
  ship('8', 1),
  ship('9', 1),
];

/**
 * @param {string} id
 * @param {number} size
 * @returns {FleetShip}
 */
function ship(id, size) {
  return Object.freeze({
    id,
    size,
    placeholderWidth: `${size * GRID_SIZE}px`,
  });
}
