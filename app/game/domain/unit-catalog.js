import { GRID_SIZE } from './constants.js';

/**
 * @typedef {{ id: string, size: number, placeholderWidth: string }} Unit
 */

/**
 * @type {Unit[]}
 */
export default [
  unit('0', 4),

  unit('1', 3),
  unit('2', 3),

  unit('3', 2),
  unit('4', 2),
  unit('5', 2),

  unit('6', 1),
  unit('7', 1),
  unit('8', 1),
  unit('9', 1),
];

/**
 * @param {string} id
 * @param {number} size
 * @returns {Unit}
 */
function unit(id, size) {
  return Object.freeze({
    id,
    size,
    placeholderWidth: `${size * GRID_SIZE}px`,
  });
}
