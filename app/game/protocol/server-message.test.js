import { decodeServerMessage } from './server-message.js';

describe('server message decoder', () => {
  it('keeps already decoded server messages', () => {
    const message = { type: 'room_state', view: { id: 1 } };

    expect(decodeServerMessage(message)).toBe(message);
  });

  it('decodes json server messages', () => {
    expect(
      decodeServerMessage('{"type":"room_update","view":{"id":1}}')
    ).toEqual({
      type: 'room_update',
      view: { id: 1 },
    });
  });

  it('returns invalid_payload for malformed JSON', () => {
    expect(decodeServerMessage('{')).toEqual({ type: 'invalid_payload' });
  });

  it('returns unknown for parsed payloads without a type', () => {
    expect(decodeServerMessage('{"view":{}}')).toEqual({ type: 'unknown' });
  });
});
