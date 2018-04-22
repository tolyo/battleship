import { State } from './state'
import pubsubService from './pubsubservice'
import boardmap from './BoardMap'
import { TOPIC } from './constants'

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

let DRAGGED = false

export default class Ship {

  constructor(id, size) {
    this.id = id
    this.health = ShipState.ACTIVE
    this.gridState = []
    this.placed = false
    DRAGGED = false

    this.hitcount = 0
    this.size = size
    this.domState = [] // reference to dom elements occupied by a ship

    this.shiftX = 0     // offset holders
    this.shiftY = 0
    this.locked = false
    pubsubService.subscribe(TOPIC.HIT, coordinates => {
      const { column, row } = coordinates
      const index = this.domState.map(x => x.id).indexOf(`fleetboard-${column}-${row}`)
      if (index !== -1) {
        this.gridState[index] = ShipGrid.KILLED
        this.hitcount += 1
        if (this.isKilled()) {
          this.health = ShipState.KILLED
          document.getElementById(this.id).classList.add('killed')
          console.log(this)
        }
      }
    })
  }

  reset() {
    this.health = ShipState.ACTIVE
    this.hitcount = 0
    let newgridstate = []
    this.gridState.forEach(e => newgridstate.push(ShipGrid.ALIVE))
    this.gridState = newgridstate
  }

  setLocation(column, row, orientation = ShipOrientation.HORIZONTAL) {
    this.column = column
    this.row = row
    this.orientation = orientation
  }

  attachToBoard() {
    // console.log('attach to board')
    this.shipElement = document.createElement('div')

    // set ship size
    this.setShipSize()

    const board = document.getElementById('board')
    board.appendChild(this.shipElement)
    this.shipElement.id = this.id
    this.shipElement.classList += 'ship'
    this.shipElement.position = 'absolute'
    this.placed = true
    this.shipElement.appendChild(document.createElement('span'))
    this.attachShipToTile(this.column, this.row)
    this.updateDomState()
    // console.log(this.domState)
    this.attachShipToClosestTile()
    // console.log(this.domState)
    Array.from(this.domState).forEach(tile => tile.className = 'fleetboard-tile placed')

    // set event handlers
    this.shipElement.onmousedown = (e) => this.onmousedown(e)

    // override default browser behavior
    this.shipElement.ondragstart = () => false
    this.shipElement.onmouseup = (e) => console.log(this.shipElement.id)

  }

  removeFromBoard() {
    if (this.placed == false) return
    const board = document.getElementById('board')
    board.removeChild(this.shipElement)
    this.domState.forEach(elem => {
      elem.className = 'fleetboard-tile'
      getAdjacentForTile(elem).forEach(elem => elem.className = 'fleetboard-tile')
    })
    this.placed = false
    this.reset()
  }

  setShipSize() {
    this.shipElement.style.width = (this.orientation === ShipOrientation.HORIZONTAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'
    this.shipElement.style.height = (this.orientation === ShipOrientation.VERTICAL) ? SQUARE_SIZE * this.size + 'px' : SQUARE_SIZE + 'px'
  }

  onmousedown (e) {
    if (e.button !== undefined && e.button !== 0) return // only touch or left click
    if (e.touches && e.touches.length > 1)        return // support one finger touch only
    if (DRAGGED === true) return
    if (this.locked === true) return

    e.preventDefault()
    // console.log('onmousedown')
    DRAGGED = true
    this.moved = false
    document.onmousemove = (e) => this.onmousemove(e)
    document.onmouseup = (e) => this.onmouseup(e)
    const shipCoordinates = this.getShipCoordinates()
    // set offsets for the click event
    this.shiftX = e.pageX - shipCoordinates.left
    this.shiftY = e.pageY - shipCoordinates.top
    this.shipElement.classList.add('dragged')
    //// console.log('finished')
  }

  onmousemove (e) {
    e.preventDefault()
    // console.log('onmousemove')
    this.moved = true
    this.domState.forEach(elem => elem.dispatchEvent(new Event('dragLeave')))
    this.shipElement.style.left = e.pageX - this.shiftX + 'px';
    this.shipElement.style.top = e.pageY - this.shiftY + 'px';
    //this.triggerDragEvent()
    boardmap.remove(this)
    boardmap.clearBlocked()
  }

  onmouseup (e) {
    console.log('onmouseup')
    console.log(e)
    e.preventDefault()
    DRAGGED = false
    // clear event bindings
    document.onmousemove = null
    document.onmouseup = null
    this.shipElement.classList.remove('dragged')
    if (this.moved === false) {
      this.domState.forEach(elem => elem.dispatchEvent(new Event('dragLeave')))
      boardmap.remove(this)
      boardmap.clearBlocked()

      // console.log('rotate ship')
      const isLegal = boardmap.isLegal(this.column, this.row, this.size, this.alternateShipOrientation())

      if (isLegal) {
        this.triggerDragEvent()
        this.orientation = this.alternateShipOrientation()
        this.setShipSize()
        this.triggerDragEvent()
      } else {
        this.shipElement.classList.add('error')
        setTimeout(() => this.shipElement.classList.remove('error'), 500)
        //this.shipElement.classList.remove('error')
      }
    }
    this.attachShipToClosestTile()
    Array.from(this.domState).forEach(tile => tile.className += ' placed')
    boardmap.add(this)
    //Fleet.forEach(ship => ship.getAdjacentTiles().forEach(tile => tile.dispatchEvent(new Event('dragEnter'))))
    pubsubService.publish("markAdjacent", null)
    // console.log(boardmap.showGrid())

  }

  alternateShipOrientation() {
    if (this.orientation === ShipOrientation.VERTICAL) {
      return ShipOrientation.HORIZONTAL
    } else {
      return ShipOrientation.VERTICAL
    }
  }

  getEventCoordinates(e) {
    const ship = this.getShipCoordinates()
    //// console.log(ship)
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
    // console.log("attachShipToLastTile")
    const tile = document.getElementById(this.getShipTileId())
    tile.className = 'fleetboard-tile placed'
    this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
    this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
  }

  attachShipToTile(column, row) {
    const tile = document.getElementById(`fleetboard-${column}-${row}`)
    this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
    this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';
  }


  getShipTileId () {
    // console.log("getShipTileId " + `${this.column}-${this.row}`)
    return `fleetboard-${this.column}-${this.row}`
  }

  lockShip() {
    this.locked = !this.locked
  }


  attachShipToClosestTile () {
    const tile = this.findClosestTile()
    this.getAdjacentTiles().forEach(tile => tile.dispatchEvent(new Event('dragEnter')))
    //boardmap.isLegal(tile.elem.getAttribute(data-x))
    if (!tile) {
      this.attachShipToLastTile()
      return
    }
    const {row, column} = getTileCoordinates(tile)
    // console.log(window.BattleShipBoard)
    console.log("isLegal " + boardmap.isLegal(column, row, this.size, this.orientation))
    if (!boardmap.isLegal(column, row, this.size, this.orientation)) {
      this.attachShipToLastTile()
    } else {
      if (tile.className.split(' ').indexOf('fleetboard-tile') != -1) {
        //tile.className = 'fleetboard-tile hit'
        this.shipElement.style.left = tile.getBoundingClientRect().left + 'px';
        this.shipElement.style.top = tile.getBoundingClientRect().top + 'px';

        // ship should remember its coordinates
        const {row, column} = getTileCoordinates(tile)
        this.row = row
        this.column = column

        this.updateDomState() // always update dom state on ship attachment
      } else {
        this.attachShipToLastTile()
      }
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
    let tiles = []
    const tile = this.findClosestTile()
    const coordinates = getTileCoordinates(tile)
    // console.log(coordinates)
    if (!isNaN(coordinates.row) && !isNaN(coordinates.column)) {
      this.gridState.forEach((val, indx) => {
        try {
          if (this.orientation === ShipOrientation.HORIZONTAL) {
            tiles.push(State.grid[coordinates.column][coordinates.row + indx].elem)
          } else {
            tiles.push(State.grid[coordinates.column + indx][coordinates.row].elem)
          }
        } catch (e) {
          tiles = []
        }
      })
    }
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
    // console.log('updateDomState')
    this.domState = []
    // domState must always be the same size as the grid state
    this.getShipTiles().forEach(el => {
      const size = this.domState.push(el)
    })
  }

  // Reset all tiles to initial state
  clear() {
    // console.log(`crearing ship ${this}`)

    // notify tiles
    this.getShipTiles().forEach(el => {
      el.className = 'fleetboard-tile'
      getAdjacentForTile(el).forEach(el => el.className = 'fleetboard-tile')
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
  //// console.log(tile)
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


export const ShipGrid = {
  ALIVE   : 0,
  KILLED  : 1
}

export class Carrier extends Ship {

  constructor(id) {
    super(id, 4)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Cruiser extends Ship {

  constructor(id) {
    super(id, 3)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class Destroyer extends Ship {

  constructor(id) {
    super(id, 2)
    this.gridState = [ShipGrid.ALIVE, ShipGrid.ALIVE]
  }

}

export class TorpedoBoat extends Ship {

  constructor(id) {
    super(id, 1)
    this.gridState = [ShipGrid.ALIVE]
  }

}

