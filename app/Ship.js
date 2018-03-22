export const ShipState = Object.freeze({
  ACTIVE:   Symbol("ACTIVE"),
  WOUNDED:  Symbol("WOUNDED"),
  KILLED:   Symbol("KILLED")
});

export const ShipOrientation = Object.freeze({
  VERTICAL:   Symbol("VERTICAL"),
  HORIZONTAL:  Symbol("HORIZONTAL")
});

export default class Ship {

  constructor(id, size = 1, orientation = ShipOrientation.HORIZONTAL) {
    this.state = ShipState.ACTIVE
    this.orientation = orientation
    this.hitcount = 0
    this.size = size
    this.shiftX = 0
    this.shiftY = 0
    this.shipElement = document.getElementById(id)
    this.shipElement.position = 'absolute'

    // set event hadlers
    this.shipElement.onmousedown = (e) => this.onmousedown(e)

    // override default browser behavior
    this.shipElement.ondragstart = () => false
  }


  onmousedown (e) {
    document.onmousemove = (e) => this.onmousemove(e)
    this.shipElement.onmouseup = (e) => this.onmouseup(e)
    const shipCoordinates = this.getShipCoordinates()

    // set offsets for the click event
    this.shiftX = e.pageX - shipCoordinates.left
    this.shiftY = e.pageY - shipCoordinates.top
  }

  onmousemove (e) {
    this.shipElement.style.left = e.pageX - this.shiftX + 'px';
    this.shipElement.style.top = e.pageY - this.shiftY + 'px';
  }

  onmouseup (e) {
    // clear event bindings
    document.onmousemove = null
    this.shipElement.onmouseup = null
    const shipCenter = this.getShipCenterCoordinates()
    this.shipElement.hidden = true
    const tile = document.elementFromPoint(shipCenter.left, shipCenter.top)
    this.shipElement.hidden = false
    if (tile.className.split(' ').indexOf('tile') != -1) {
      tile.className = "tile hit"
      this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
      this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
    }
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
    const width = this.shipElement.offsetWidth
    const height = this.shipElement.offsetHeight
    return {
      left: box.left + width / 2,
      top: box.top + height / 2
    }
  }

  isKilled() {
    return this.hitcount == this.size
  }

}
