import Ship from './ship.js';

/**
 * @type {Ship[]}
 */
export default [
  new Ship('0', 4),

  new Ship('1', 3),
  new Ship('2', 3),

  // 3 destroyers
  new Ship('3', 2),
  new Ship('4', 2),
  new Ship('5', 2),

  // 4 torpedo boats
  new Ship('6', 1),
  new Ship('7', 1),
  new Ship('8', 1),
  new Ship('9', 1),
];
