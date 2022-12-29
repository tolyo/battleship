/**
 * @function gameEngine
 * @return {{nextState: function()}}
 */
import {GameState} from './constants';

export default (() => {
  let gamestate: GameState = GameState.Preparing;

  const getState = () => gamestate;

  const nextState = () => {
    switch (gamestate) {
      case GameState.Preparing:
        gamestate = GameState.Playing;
        break;
      case GameState.Playing:
        gamestate = GameState.Ended;
        break;
    }
  };

  const init = () => {
    gamestate = GameState.Preparing;
  };

  return {
    getState,
    nextState,
    init,
  };
})();
