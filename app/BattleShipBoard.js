
export default class BattleShipBoard {

  constructor(id) {
    if (!id) throw new Error("Board id required");
    this.board = window.document.getElementById(id);
    if (!this.board) throw new Error("Board id not found");
    this.board.setAttribute('class', 'battleshipboard')

    this.addTiles()
  }

  addTiles () {
    const rows = [1,2,3,4,5,6,7,8,9,10]
    const columns = [1,2,3,4,5,6,7,8,9,10]

    rows.forEach(() => {
      // create row
      const tileRow = document.createElement("div")
      tileRow.setAttribute("class", "tileRow")
      this.board.appendChild(tileRow)

      // create tiles
      columns.forEach(() => {
        const tile = document.createElement("div")
        tile.setAttribute("class", "tile")
        tile.addEventListener("click",() => {
          tile.setAttribute("class", "tile miss")
        })
        tileRow.appendChild(tile)
      })

    })
  }
}