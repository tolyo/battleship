import { addTilesToBoard } from './boardutils'

export default (() => {
  const createBoard = (id) => {
    if (id === undefined) throw new Error("board id not provided")
    const elem = window.document.getElementById(id)
    if (elem === null) throw new Error("board element not found")
    debugger
    addTilesToBoard(elem, "fleetboard")
  }

  return {
    createBoard
  }
})()
