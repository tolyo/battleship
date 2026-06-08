import {
  decodeRoomEvent,
  roomEventFromServerMessage,
  lobbyUrl,
  moveMessage,
  restoreUrl,
} from './room-protocol.js';

describe('room protocol URLs', () => {
  it('encodes lobby player and board state', () => {
    const url = lobbyUrl({ player: 'Anonymous A B', boardState: [['_']] });

    expect(url).toBe(
      `/ws?player=Anonymous%20A%20B&board=${encodeURIComponent('[["_"]]')}`
    );
  });

  it('encodes restore identifiers', () => {
    expect(restoreUrl({ roomId: 'room 1', playerId: 'player/1' })).toBe(
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

describe('room events', () => {
  it('decodes websocket payloads directly to room events', () => {
    expect(decodeRoomEvent('{"type":"lobby_waiting"}')).toEqual({
      type: 'waiting',
    });
  });

  it('maps lobby waiting messages', () => {
    expect(roomEventFromServerMessage({ type: 'lobby_waiting' })).toEqual({
      type: 'waiting',
    });
  });

  it('maps room view snapshots to room events', () => {
    const view = { id: 1 };

    expect(roomEventFromServerMessage({ type: 'room_update', view })).toEqual({
      type: 'state_received',
      view,
    });
    expect(roomEventFromServerMessage({ type: 'room_state', view })).toEqual({
      type: 'state_received',
      view,
    });
  });

  it('maps opponent left messages', () => {
    expect(roomEventFromServerMessage({ type: 'opponent_left' })).toEqual({
      type: 'opponent_left',
    });
  });

  it('maps room errors to unavailable events', () => {
    expect(
      roomEventFromServerMessage({ type: 'error', reason: 'room_not_found' })
    ).toEqual({
      type: 'room_unavailable',
      reason: 'room_not_found',
    });
  });

  it('maps unknown player errors to unavailable events', () => {
    expect(
      roomEventFromServerMessage({ type: 'error', reason: 'unknown_player' })
    ).toEqual({
      type: 'room_unavailable',
      reason: 'unknown_player',
    });
  });

  it('maps other errors to server errors', () => {
    expect(
      roomEventFromServerMessage({ type: 'error', reason: 'bad_request' })
    ).toEqual({
      type: 'server_error',
      reason: 'bad_request',
    });
  });

  it('maps match_found to a room entry that updates the url', () => {
    expect(
      roomEventFromServerMessage({
        type: 'match_found',
        room_id: 'r1',
        player_id: 'p1',
        opponent_id: 'p2',
        view: { id: 1 },
      })
    ).toEqual({
      type: 'room_entered',
      entry: {
        roomId: 'r1',
        playerId: 'p1',
        opponentId: 'p2',
        view: { id: 1 },
      },
      updateUrl: true,
    });
  });

  it('maps room_joined to a room entry without updating the url', () => {
    expect(
      roomEventFromServerMessage({
        type: 'room_joined',
        room_id: 'r1',
        player_id: 'p1',
      })
    ).toEqual({
      type: 'room_entered',
      entry: {
        roomId: 'r1',
        playerId: 'p1',
        opponentId: undefined,
        view: undefined,
      },
      updateUrl: false,
    });
  });

  it('maps malformed room entries to unavailable events', () => {
    expect(
      roomEventFromServerMessage({ type: 'match_found', player_id: 'p1' })
    ).toEqual({
      type: 'room_unavailable',
      reason: 'invalid_room_entry',
    });
  });

  it('ignores unknown messages', () => {
    expect(roomEventFromServerMessage({ type: 'something_else' })).toEqual({
      type: 'ignored',
    });
  });
});
