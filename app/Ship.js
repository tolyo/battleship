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
    this.state = ShipState.ACTIVE
    this.row = null
    this.column = null
    this.orientation = orientation
    this.hitcount = 0
    this.size = size
    this.shiftX = 0
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
    this.moved = false
    //console.log(e)
    if (this.placed) return
    document.onmousemove = (e) => this.onmousemove(e)
    this.shipElement.onmouseup = (e) => this.onmouseup(e)
    const shipCoordinates = this.getShipCoordinates()

    // set offsets for the click event
    this.shiftX = e.pageX - shipCoordinates.left
    this.shiftY = e.pageY - shipCoordinates.top

    const shipCenterCoordinates = this.getShipCenterCoordinates()
    console.log(shipCenterCoordinates)
    this.originalX = shipCoordinates.left
    this.originalY = shipCoordinates.top
  }

  onmousemove (e) {
    //console.log(e)
    this.moved = true
    this.shipElement.style.left = e.pageX - this.shiftX + 'px';
    this.shipElement.style.top = e.pageY - this.shiftY + 'px';
  }

  onmouseup (e) {
    //console.log(e)
    // prevent firing on clicks
    if (!this.moved) return
    // clear event bindings
    document.onmousemove = null
    this.shipElement.onmouseup = null
    this.attachShipToClosestTile()
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
    const shipCenter = this.getShipCenterCoordinates()
    this.shipElement.hidden = true
    const tile = document.elementFromPoint(shipCenter.left, shipCenter.top)
    this.shipElement.hidden = false
    if (tile.className.split(' ').indexOf('tile') != -1) {
      tile.className = 'tile hit'
      this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
      this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';

      this.row = tile.getAttribute('data-row')
      this.column = tile.getAttribute('data-column')

      console.log(this)

      this.originalX = this.shipElement.style.left
      this.originalY = this.shipElement.style.top
    } else {
      this.attachShipToLastTile()
    }
  }
}
