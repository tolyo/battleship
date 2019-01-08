import { GRID } from './constants'
import { GridSquare, State } from './state'

/**
 * @function addTilesToBoard
 * @param {HTMLElement} elem
 * @param {string} boardname
 * @return
 */
export const addTilesToBoard = (elem, boardname) => {
  GRID.forEach((y) => {
    // create row
    const tileRow = document.createElement('div')
    tileRow.className = 'board-row'
    elem.appendChild(tileRow)

    State.grid.push([])

    // create tiles
    GRID.forEach((x) => {
      const tile = document.createElement('div')
      tile.className = boardname + '-tile'
      tile.id = boardname + '-' + y + '-' + x
      tile.setAttribute('data-column', y)
      tile.setAttribute('data-row', x)
      //
      // tile.addEventListener('dragEnter', () => tile.className = 'fleetboard-tile droppable-target')
      tile.addEventListener('dragLeave', () => {
        tile.className = boardname + '-tile'
      })

      if (boardname === 'hitboard') {
        this.initHitboardTile(tile)
      }

      tileRow.appendChild(tile)
      State.grid[y].push(new GridSquare(x, y, tile))
    })
  })
}