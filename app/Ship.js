import { State } from './state'

export const ShipState = Object.freeze({
  ACTIVE:   Symbol('ACTIVE'),
  WOUNDED:  Symbol('WOUNDED'),
  KILLED:   Symbol('KILLED')
});

export const ShipOrientation = Object.freeze({
  VERTICAL:   Symbol('VERTICAL'),
  HORIZONTAL:  Symbol('HORIZONTAL')
});

const SQUARE_SIZE = 30;

export default class Ship {

  constructor(id, size = 1, orientation = ShipOrientation.HORIZONTAL) {
    this.health = ShipState.ACTIVE
    this.gridState = []
    this.row = null
    this.column = null
    this.orientation = orientation
    this.hitcount = 0
    this.size = size
    this.domState = [] // reference to dom elements occupied by a ship

    this.shiftX = 0     // offset holders
    this.shiftY = 0
    this.shipElement = document.createElement('div')

    // set ship size
    this.shipElement.style.width = (this.orientation == ShipOrientation.HORIZONTAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'
    this.shipElement.style.height = (this.orientation == ShipOrientation.VERTICAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'

    const board = document.getElementById('board')
    board.appendChild(this.shipElement)
    this.shipElement.id = 'ship'
    this.shipElement.position = 'absolute'
    this.placed = false
    this.attachShipToClosestTile()

    // set event handlers
    this.shipElement.onmousedown = (e) => this.onmousedown(e)

    // override default browser behavior
    this.shipElement.ondragstart = () => false
  }

  onmousedown (e) {
    console.log('onmousedown')
    if (this.placed) return
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
    this.shipElement.style.left = e.pageX - this.shiftX + 'px';
    this.shipElement.style.top = e.pageY - this.shiftY + 'px';
    this.notifyClosestTiles()
  }

  onmouseup (e) {
    console.log('onmouseup')
    // clear event bindings
    document.onmousemove = null
    this.shipElement.onmouseup = null
    this.shipElement.classList.remove('dragged')
    this.attachShipToClosestTile()
    console.log(this.domState)
    Array.from(this.domState).forEach(tile => tile.className = 'tile hit')
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
    const width = (this.orientation == ShipOrientation.HORIZONTAL) ? this.shipElement.offsetWidth / this.size : this.shipElement.offsetWidth
    const height = (this.orientation == ShipOrientation.VERTICAL) ? this.shipElement.offsetHeight/ this.size : this.shipElement.offsetHeight
    return {
      left: box.left + width / 2,
      top: box.top + height / 2
    }
  }

  isKilled() {
    return this.hitcount == this.size
  }

  attachShipToLastTile() {
    console.log(this.getShipTileId())
    const tile = document.getElementById(this.getShipTileId())
    tile.className = 'tile hit'
    this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
    this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
  }

  getShipTileId () {
    return `${this.column}-${this.row}`
  }

  attachShipToClosestTile () {
    const tile = this.findClosestTile()
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

  notifyClosestTiles() {
    const elements = document.getElementsByClassName('droppable-target')
    console.log(elements)
    Array.from(elements).forEach((el) => el.dispatchEvent(new Event('dragLeave')))
    this.getShipTiles().forEach(tile =>tile.dispatchEvent(new Event('dragEnter')))
  }

  getShipTiles() {
    const tiles = []
    const tile = this.findClosestTile()
    tiles.push(tile)
    const coordinates = getTileCoordinates(tile)
    this.gridState.forEach((val, indx) => {
      if (this.orientation == ShipOrientation.HORIZONTAL) {
        tiles.push(State.grid[coordinates.column - 1][coordinates.row - 1 + indx].elem)
      } else {
        tiles.push(State.grid[coordinates.column - 1 + indx][coordinates.row - 1].elem)
      }
    })
    tiles.push(tile)
    return tiles
  }

  updateDomState() {
    this.domState = []
    // domState must always be the same size as the grid state
    this.getShipTiles().forEach(el => this.domState.push(el))
  }

}

const getTileCoordinates = (tile) => {
  return {
    row: parseInt(tile.getAttribute('data-row')),
    column: parseInt(tile.getAttribute('data-column'))
  }
}