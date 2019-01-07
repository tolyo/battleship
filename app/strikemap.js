import { GRID } from './constants'
import { MapTile } from './state'
import { FLEET_SIZE } from './Player'

export default ({
  strikeCallback,
  victoryCallback
}) => {

  const strikeMap = []
  let strikeCount = 0

  // initialize map
  GRID.forEach(col => {
    strikeMap.push([])
    GRID.forEach(() => strikeMap[col].push(MapTile.EMPTY))
  })

  const showMap = () => {
    let grid = ``
    strikeMap.forEach(column => {
      column.forEach(row => {
        grid = grid + `${row} `
      })
      grid = grid + `\n`
    })
    console.log(grid)
  }

  const attemptStrike = (row, column) => {
    if (strikeMap[row][column] === MapTile.MISS) {
      throw new Error("Illegal state. Already struck with MISS")
    }

    if (strikeMap[row][column] === MapTile.HIT) {
      throw new Error("Illegal state. Already struck with HIT")
    }

    const result = strikeCallback(row, column)
    if (result === true) {
      strikeMap[row][column] = MapTile.HIT
      ++strikeCount;
      if (strikeCount === FLEET_SIZE) {
        victoryCallback()
      }
    } else if (result === false) {
      strikeMap[row][column] = MapTile.MISS
    } else {
      throw new Error("Callback must return boolean")
    }
  }

  return {
    showMap,
    attemptStrike
  }

}


