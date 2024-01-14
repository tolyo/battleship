export default class GameState {
  constructor() {
    /**
     * @type {"PREPARING" | "PLAYING" | "ENDED"}
     */
    this.gamestate = 'PREPARING';
  }

  nextState() {
    switch (this.gamestate) {
      case 'PREPARING':
        this.gamestate = 'PLAYING';
        break;
      case 'PLAYING':
        this.gamestate = 'ENDED';
        break;
      default:
        throw new Error('Game already ended');
    }
  }

  init() {
    this.gamestate = 'PREPARING';
  }
}
