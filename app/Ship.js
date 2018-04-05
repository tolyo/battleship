import { State } from './state'
import BoardMap from './BoardMap'

export const ShipState = Object.freeze({
  ACTIVE:   'ACTIVE',
  WOUNDED:  'WOUNDED',
  KILLED:   'KILLED'
});

export const ShipOrientation = Object.freeze({
  VERTICAL:   'VERTICAL',
  HORIZONTAL: 'HORIZONTAL'
});

const SQUARE_SIZE = 30;

export default class Ship {

  constructor(id, size) {
    this.id = id
    this.health = ShipState.ACTIVE
    this.gridState = []
    this.placed = false

    this.hitcount = 0
    this.size = size
    this.domState = [] // reference to dom elements occupied by a ship

    this.shiftX = 0     // offset holders
    this.shiftY = 0
  }

  setLocation(column, row, orientation = ShipOrientation.HORIZONTAL) {
    this.column = column
    this.row = row
    this.orientation = orientation
  }

  attachToBoard() {
    console.log('attach to board')
    this.shipElement = document.createElement('div')

    // set ship size
    this.setShipSize()

    const board = document.getElementById('board')
    board.appendChild(this.shipElement)
    this.shipElement.id = 'ship'
    this.shipElement.position = 'absolute'
    this.placed = true
    this.shipElement.appendChild(document.createElement('span'))
    this.attachShipToTile(this.column, this.row)
    this.updateDomState()
    console.log(this.domState)
    this.attachShipToClosestTile()
    console.log(this.domState)
    Array.from(this.domState).forEach(tile => tile.className = 'tile hit')

    // set event handlers
    this.shipElement.onmousedown = (e) => this.onmousedown(e)

    // override default browser behavior
    this.shipElement.ondragstart = () => false
    document.dispatchEvent(new Event('hello'))

  }

  removeFromBoard() {
    if (this.placed == false) return
    const board = document.getElementById('board')
    board.removeChild(this.shipElement)
    this.domState.forEach(elem => {
      elem.className = 'tile'
      getAdjacentForTile(elem).forEach(elem => elem.className = 'tile')
    })
    this.placed = false
  }

  setShipSize() {
    this.shipElement.style.width = (this.orientation === ShipOrientation.HORIZONTAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'
    this.shipElement.style.height = (this.orientation === ShipOrientation.VERTICAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'
  }

  onmousedown (e) {
    console.log('onmousedown')
    this.moved = false
    document.onmousemove = (e) => this.onmousemove(e)
    this.shipElement.onmouseup = (e) => this.onmouseup(e)
    const shipCoordinates = this.getShipCoordinates()
    // set offsets for the click event
    this.shiftX = e.pageX - shipCoordinates.left
    this.shiftY = e.pageY - shipCoordinates.top
    this.shipElement.classList.add('dragged')
  }

  onmousemove (e) {
    console.log('onmousemove')
    this.moved = true
    this.domState.forEach(elem => elem.dispatchEvent(new Event('dragLeave')))
    this.shipElement.style.left = e.pageX - this.shiftX + 'px';
    this.shipElement.style.top = e.pageY - this.shiftY + 'px';
    this.triggerDragEvent()
    window.BattleShipBoard.map.remove(this)
    window.BattleShipBoard.map.clearBlocked()
    console.log(window.BattleShipBoard.map.showGrid())
  }

  onmouseup (e) {
    console.log('onmouseup')
    // clear event bindings
    document.onmousemove = null
    this.shipElement.onmouseup = null
    this.shipElement.classList.remove('dragged')
    if (this.moved === false) {
      console.log('rotate ship')
      this.domState.forEach(elem => elem.dispatchEvent(new Event('dragLeave')))
      window.BattleShipBoard.map.remove(this)
      window.BattleShipBoard.map.clearBlocked()
      this.triggerDragEvent()
      if (this.orientation === ShipOrientation.VERTICAL) {
        this.orientation = ShipOrientation.HORIZONTAL
      } else {
        this.orientation = ShipOrientation.VERTICAL
      }
      this.setShipSize()
      this.triggerDragEvent()
    }
    this.attachShipToClosestTile()
    Array.from(this.domState).forEach(tile => tile.className = 'tile hit')
    window.BattleShipBoard.map.add(this)
    console.log(window.BattleShipBoard.map.showGrid())
  }

  getEventCoordinates(e) {
    const ship = this.getShipCoordinates()
    //console.log(ship)
    return {
      left: e.pageX - ship.left,
      top: e.pageY - ship.top
    }
  }

  getShipCoordinates() {
    const box = this.shipElement.getBoundingClientRect()
    return {
      left: box.left + window.pageXOffset,
      top: box.top + window.pageYOffset
    }
  }

  getShipCenterCoordinates() {
    const box = this.shipElement.getBoundingClientRect()
    const width = (this.orientation === ShipOrientation.HORIZONTAL) ? this.shipElement.offsetWidth / this.size : this.shipElement.offsetWidth
    const height = (this.orientation === ShipOrientation.VERTICAL) ? this.shipElement.offsetHeight / this.size : this.shipElement.offsetHeight
    return {
      left: box.left + width / 2,
      top: box.top + height / 2
    }
  }

  isKilled() {
    return this.hitcount === this.size
  }

  attachShipToLastTile() {
    console.log(this.getShipTileId())
    const tile = document.getElementById(this.getShipTileId())
    tile.className = 'tile hit'
    this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
    this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
  }

  attachShipToTile(column, row) {
    const tile = document.getElementById(`${column}-${row}`)
    this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
    this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
  }


  getShipTileId () {
    return `${this.column}-${this.row}`
  }

  attachShipToClosestTile () {
    const tile = this.findClosestTile()
    this.getAdjacentTiles().forEach(tile => tile.dispatchEvent(new Event('dragEnter')))

    console.log(this.getAdjacentTiles())
    if (tile.className.split(' ').indexOf('tile') != -1) {
      //tile.className = 'tile hit'
      this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
      this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';

      // ship should remember its coordinates
      const coordinates = getTileCoordinates(tile)
      this.row = coordinates.row
      this.column = coordinates.column

      this.updateDomState() // always update dom state on ship attachment
    } else {
      this.attachShipToLastTile()
    }
  }

  findClosestTile() {
    const shipCenter = this.getShipCenterCoordinates()
    this.shipElement.hidden = true
    const tile = document.elementFromPoint(shipCenter.left, shipCenter.top)
    this.shipElement.hidden = false
    return tile
  }

  triggerDragEvent() {
    this.getShipTiles().forEach(tile =>tile.dispatchEvent(new Event('dragEnter')))
  }

  getShipTiles() {
    const tiles = []
    const tile = this.findClosestTile()
    const coordinates = getTileCoordinates(tile)
    this.gridState.forEach((val, indx) => {
      if (this.orientation === ShipOrientation.HORIZONTAL) {
        tiles.push(State.grid[coordinates.column][coordinates.row + indx].elem)
      } else {
        tiles.push(State.grid[coordinates.column + indx][coordinates.row].elem)
      }
    })
    return tiles
  }

  getAdjacentTiles() {
    const tiles = []
    this.getShipTiles().forEach(tile => {
      getAdjacentForTile(tile).forEach(adjacentTile => {
        if (tiles.map(x => x.id).indexOf(adjacentTile.id) === -1) {
          tiles.push(adjacentTile)
        }
      })
    })

    return tiles
  }

  updateDomState() {
    console.log('updateDomState')
    this.domState = []
    // domState must always be the same size as the grid state
    this.getShipTiles().forEach(el => this.domState.push(el))
  }

  // Reset all tiles to initial state
  clear() {
    console.log(`crearing ship ${this}`)

    // notify tiles
    this.getShipTiles().forEach(el => {
      el.className = 'tile'
      getAdjacentForTile(el).forEach(el => el.className = 'tile')
    })

    window.document.getElementById('board').removeChild(this.shipElement)
    this.placed = false
  }

  getShipMapCoordinates() {
    const coordinates = []
    for (let i = 0; i < this.size; i++) {
      if (this.orientation === ShipOrientation.HORIZONTAL) {
        coordinates.push({y : this.column, x: this.row + i})
      } else {
        coordinates.push({y : this.column + i, x: this.row})
      }
    }
    return coordinates
  }
}

const getTileCoordinates = (tile) => {
  return {
    row: parseInt(tile.getAttribute('data-row')),
    column: parseInt(tile.getAttribute('data-column'))
  }
}



const getAdjacentForTile = (tile) => {

  const tiles = []
  const coordinates = getTileCoordinates(tile)
  const y = coordinates.column
  const x = coordinates.row

  // top left
  if (y !== 0 && x !== 0) {
    tiles.push(getStateElement(y - 1, x - 1))
  }

  // top mid
  if (y !== 0) {
    tiles.push(getStateElement(y - 1, x))
  }

  // top right
  if (y !== 0 && x !== 9) {
      tiles.push(getStateElement(y - 1,x + 1))
  }

  // mid left
  if (x !== 0) {
    tiles.push(getStateElement(y,x - 1))
  }

  // mid right
  if (x !== 9) {
    tiles.push(getStateElement(y,x + 1))
  }

  // bot left
  if (y !== 9 && x !== 0) {
      tiles.push(getStateElement(y + 1,x - 1))
  }

  // bot mid
  if (y !== 9) {
    tiles.push(getStateElement(y + 1,x))
  }

  // bot right
  if (y !== 9 && x !== 9) {
    tiles.push(getStateElement(y + 1,x + 1))
  }

  return tiles
}



const getStateElement = (y, x) => {
  return getGridElement(y, x).elem
}

const getGridElement = (y, x) => {
  return State.grid[y][x]
}