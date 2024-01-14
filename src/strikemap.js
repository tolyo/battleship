import { GRID } from "./constants";
import { FLEET_SIZE } from "./fleet";

/**
 * @enum {string}
 */
export const MapTile = {
  EMPTY: "_",
  FILLED: "X",
  BLOCKED: "o",
  HIT: "+",
  MISS: "m",
};

/**
 * @function strikemap
 * @param {function(int, int)} strikeCallback
 * @param {function} victoryCallback
 * @return {{showMap: function(), attemptStrike: function(number, number)}} strikemap
 */
export default ({ strikeCallback, victoryCallback }) => {
  const map = [];
  let strikeCount = 0;

  // initialize map
  GRID.forEach((col) => {
    map.push([]);
    GRID.forEach(() => map[col].push(MapTile.EMPTY));
  });

  /**
   * @return {null}
   */
  const showMap = () => {
    let grid = ``;
    map.forEach((column) => {
      column.forEach((row) => {
        grid += `${row} `;
      });
      grid += `\n`;
    });
    console.log(grid);
  };

  /**
   *
   * @param {number} row
   * @param {number} column
   * @return {null}
   */
  const attemptStrike = (row, column) => {
    if (map[row][column] === MapTile.MISS) {
      throw new Error("Illegal state. Already struck with MISS");
    }

    if (map[row][column] === MapTile.HIT) {
      throw new Error("Illegal state. Already struck with HIT");
    }

    const result = strikeCallback(row, column);
    if (result === true) {
      map[row][column] = MapTile.HIT;
      ++strikeCount;
      if (strikeCount === FLEET_SIZE) {
        victoryCallback();
      }
    } else if (result === false) {
      map[row][column] = MapTile.MISS;
    } else {
      throw new Error("Callback must return boolean");
    }
  };

  return {
    showMap,
    attemptStrike,
  };
};
