import Ship from './ship';
// Fleet representation
export const Fleet = [
  new Ship('0', 4),

  new Ship('1', 3),
  new Ship('2', 3),

  // 3 destroyers
  new Ship('3', 2),
  new Ship('4', 2),
  new Ship('5', 3),

  // 4 torpedo boats
  new Ship('6', 2),
  new Ship('7', 2),
  new Ship('8', 2),
  new Ship('9', 2),
];

export const FLEET_SIZE = Fleet.map((x) => x.size).reduce(
  (acc, a) => acc + a,
  0
);
