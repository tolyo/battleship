import { GRID } from './constants'
import { MapTile } from './state'
import { FLEET_SIZE } from './Player'

export default ({
  strikeCallback,
  victoryCallback
}) => {

  const map = []
  let strikeCount = 0

  // initialize map
  GRID.forEach(col => {
    map.push([])
    GRID.forEach(() => map[col].push(MapTile.EMPTY))
  })

  const showMap = () => {
    let grid = ``
    map.forEach(column => {
      column.forEach(row => {
        grid = grid + `${row} `
      })
      grid = grid + `\n`
    })
    console.log(grid)
  }

  const attemptStrike = (row, column) => {
    if (map[row][column] === MapTile.MISS) {
      throw new Error("Illegal state. Already struck with MISS")
    }

    if (map[row][column] === MapTile.HIT) {
      throw new Error("Illegal state. Already struck with HIT")
    }

    const result = strikeCallback(row, column)
    if (result === true) {
      map[row][column] = MapTile.HIT
      ++strikeCount;
      if (strikeCount === FLEET_SIZE) {
        victoryCallback()
      }
    } else if (result === false) {
      map[row][column] = MapTile.MISS
    } else {
      throw new Error("Callback must return boolean")
    }
  }

  return {
    showMap,
    attemptStrike
  }

}


