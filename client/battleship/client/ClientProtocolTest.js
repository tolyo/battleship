import {
  move,
  sync,
} from '../../../_build/browser/js/modules/battleship/client/ClientProtocol.js';

describe('client protocol messages', () => {
  it('constructs a fresh wire object with the original type and coordinate keys', () => {
    const point = Object.freeze({ row: 4, column: 7, allowed: true });
    const message = move(point);
    expect(message).not.toBe(point);
    expect(message).toEqual({ type: 'move', row: 4, column: 7 });
    expect(JSON.parse(JSON.stringify(message))).toEqual({
      type: 'move',
      row: 4,
      column: 7,
    });
  });

  it('serializes sync requests with the shared native protocol name', () => {
    expect(sync()).toEqual({ type: 'sync' });
    expect(JSON.stringify(sync())).toBe('{"type":"sync"}');
  });
});
