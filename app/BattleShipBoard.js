
export default class BattleShipBoard {

  constructor(id) {
    if (!id) throw new Error("Board id required");
    this.fleetboard = window.document.getElementById(id);
    if (!this.fleetboard) throw new Error("Board id not found");
    this.fleetboard.setAttribute('class', 'battleshipboard')
    addTiles(this.fleetboard)
  }

}

const addTiles = (board) => {
  const rows = [1,2,3,4,5,6,7,8,9,10]
  const columns = [1,2,3,4,5,6,7,8,9,10]

  rows.forEach(() => {
    // create row
    const tileRow = document.createElement("div")
    tileRow.className = "tileRow"
    board.appendChild(tileRow)

    // create tiles
    columns.forEach(() => {
      const tile = document.createElement("div")
      tile.className = "tile droppable-target"
      tileRow.appendChild(tile)
    })

  })
}
