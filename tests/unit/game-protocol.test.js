import { decodeServerMessage } from '../../app/game/server-message.js';
import {
  battleRoomEvent,
  lobbyUrl,
  moveMessage,
  restoreUrl,
} from '../../app/game/game-protocol.js';

describe('game protocol URLs', () => {
  it('encodes lobby player and board state', () => {
    const url = lobbyUrl('Anonymous A B', [['_']]);

    expect(url).toBe(
      `/ws?player=Anonymous%20A%20B&board=${encodeURIComponent('[["_"]]')}`
    );
  });

  it('encodes restore identifiers', () => {
    expect(restoreUrl('room 1', 'player/1')).toBe(
      '/ws?room_id=room%201&player_id=player%2F1'
    );
  });

  it('builds move messages', () => {
    expect(moveMessage({ row: 3, column: 7 })).toEqual({
      type: 'move',
      row: 3,
      column: 7,
    });
  });
});

describe('server messages', () => {
  it('keeps already decoded server messages', () => {
    const message = { type: 'game_state', game: { id: 1 } };

    expect(decodeServerMessage(message)).toBe(message);
  });

  it('decodes json server messages', () => {
    expect(decodeServerMessage('{"type":"game_update","game":{"id":1}}')).toEqual(
      {
        type: 'game_update',
        game: { id: 1 },
      }
    );
  });

  it('returns invalid_payload for malformed JSON', () => {
    expect(decodeServerMessage('{')).toEqual({ type: 'invalid_payload' });
  });

  it('returns unknown for parsed payloads without a type', () => {
    expect(decodeServerMessage('{"game":{}}')).toEqual({ type: 'unknown' });
  });

  it('maps lobby waiting messages', () => {
    expect(battleRoomEvent({ type: 'lobby_waiting' })).toEqual({
      type: 'waiting',
    });
  });

  it('maps game updates to game received events', () => {
    const game = { id: 1 };

    expect(battleRoomEvent({ type: 'game_update', game })).toEqual({
      type: 'game_received',
      game,
    });
    expect(battleRoomEvent({ type: 'game_state', game })).toEqual({
      type: 'game_received',
      game,
    });
  });

  it('maps opponent left messages', () => {
    expect(battleRoomEvent({ type: 'opponent_left' })).toEqual({
      type: 'opponent_left',
    });
  });

  it('maps room errors to unavailable events', () => {
    expect(
      battleRoomEvent({ type: 'error', reason: 'room_not_found' })
    ).toEqual({
      type: 'room_unavailable',
      reason: 'room_not_found',
    });
  });

  it('maps unknown player errors to unavailable events', () => {
    expect(battleRoomEvent({ type: 'error', reason: 'unknown_player' })).toEqual({
      type: 'room_unavailable',
      reason: 'unknown_player',
    });
  });

  it('maps other errors to server errors', () => {
    expect(battleRoomEvent({ type: 'error', reason: 'bad_request' })).toEqual({
      type: 'server_error',
      reason: 'bad_request',
    });
  });

  it('maps match_found to a room entry that updates the url', () => {
    const message = { type: 'match_found', room_id: 'r1', player_id: 'p1' };

    expect(battleRoomEvent(message)).toEqual({
      type: 'room_entered',
      message,
      updateUrl: true,
    });
  });

  it('maps room_joined to a room entry without updating the url', () => {
    const message = { type: 'room_joined', room_id: 'r1', player_id: 'p1' };

    expect(battleRoomEvent(message)).toEqual({
      type: 'room_entered',
      message,
      updateUrl: false,
    });
  });

  it('ignores unknown messages', () => {
    expect(battleRoomEvent({ type: 'something_else' })).toEqual({
      type: 'ignored',
    });
  });
});
