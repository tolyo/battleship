import GameState from './game.js';

describe('GameState', () => {
  it('moves through preparing, playing, and ended states', () => {
    const state = new GameState();

    expect(state.gamestate).toBe('PREPARING');

    state.nextState();
    expect(state.gamestate).toBe('PLAYING');

    state.nextState();
    expect(state.gamestate).toBe('ENDED');

    expect(() => state.nextState()).toThrowError('Game already ended');
  });

  it('can be reset to preparing', () => {
    const state = new GameState();
    state.nextState();
    state.init();

    expect(state.gamestate).toBe('PREPARING');
  });
});
