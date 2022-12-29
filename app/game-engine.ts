/**
 * @function gameEngine
 * @return {{nextState: function()}}
 */
import {GameState} from './constants';

export default (() => {
  let gamestate = GameState.PREPARING;

  const getState = () => gamestate;

  const nextState = () => {
    switch (gamestate) {
      case GameState.PREPARING:
        gamestate = GameState.PLAYING;
        break;
      case GameState.PLAYING:
        gamestate = GameState.ENDED;
        break;
    }
  };

  const init = () => {
    gamestate = GameState.PREPARING;
  };

  return {
    getState,
    nextState,
    init,
  };
})();
