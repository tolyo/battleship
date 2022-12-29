import {ShipOrientation} from './ship';

export const getRandomShipCoordinate = () => {
  return {
    row: getRandomTile(),
    column: getRandomTile(),
    orientation: getRandomOrientation(),
  };
};

const getRandomTile = () => Math.floor(Math.random() * 9);

export const getRandomOrientation = () =>
  [ShipOrientation.VERTICAL, ShipOrientation.HORIZONTAL][
    Math.round(Math.random())
  ];
