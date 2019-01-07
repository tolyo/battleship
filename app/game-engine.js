
const GameState = Object.freeze({
  PREPARING: 0,
  PLAYING: 1,
  ENDED: 2
})

/**
 * @function gameEngine
 * @return {{nextState: function()}}
 */
export default () => {

  let gamestate = GameState.PREPARING

  const nextState = () => {
    switch (gamestate) {
      case GameState.PREPARING:
        gamestate = GameState.PLAYING
        break
      case GameState.PLAYING:
        gamestate = GameState.ENDED
        break
    }
  }

  return {
    nextState
  }

}