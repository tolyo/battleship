/**
 * @function gameEngine
 * @return {{nextState: function()}}
 */
export default (() => {
  /**
   * @type {"PREPARING" | "PLAYING" | "ENDED"}
   */
  let gamestate = "PREPARING";

  const getState = () => gamestate;

  const nextState = () => {
    switch (gamestate) {
      case "PREPARING":
        gamestate = "PLAYING";
        break;
      case "PLAYING":
        gamestate = "ENDED";
        break;
    }
  };

  const init = () => {
    gamestate = "PREPARING";
  };

  return {
    getState,
    nextState,
    init,
  };
})();
