function getRandomTile() {
  return Math.floor(Math.random() * 9);
}

function getRandomOrientation() {
  return ["VERTICAL", "HORIZONTAL"][Math.round(Math.random())];
}

function getRandomShipCoordinate() {
  return {
    row: getRandomTile(),
    column: getRandomTile(),
    orientation: getRandomOrientation(),
  };
}
