import {
  lobby,
  restore,
  is_lobby,
  is_restore,
  entry,
} from '../../../_build/browser/js/modules/battleship/browser/ConnectionContext.js';

describe('native connection context', () => {
  it('keeps lobby and restored seat contexts distinct', () => {
    const seat = Object.freeze({ roomId: 'room-1', playerId: 'player-1' });
    const context = restore(seat);
    expect(is_lobby(lobby())).toBeTrue();
    expect(is_restore(lobby())).toBeFalse();
    expect(entry(lobby())).toBe('none');
    expect(is_lobby(context)).toBeFalse();
    expect(is_restore(context)).toBeTrue();
    expect(entry(context)).toEqual(['some', seat]);
    expect(entry(context)[1]).toBe(seat);
  });
});
