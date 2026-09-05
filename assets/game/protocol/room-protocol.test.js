import {
  decodeRoomEvent,
  roomEventFromServerMessage,
  ROOM_EVENT,
  ROOM_SERVER_MESSAGE,
  CLIENT_MESSAGE,
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
      type: CLIENT_MESSAGE.MOVE,
      row: 3,
      column: 7,
    });
  });
});

describe('room events', () => {
  it('decodes websocket payloads directly to room events', () => {
    expect(decodeRoomEvent('{"type":"lobby_waiting"}')).toEqual({
      type: ROOM_EVENT.WAITING,
    });
  });

  it('maps lobby waiting messages', () => {
    expect(
      roomEventFromServerMessage({ type: ROOM_SERVER_MESSAGE.LOBBY_WAITING })
    ).toEqual({
      type: ROOM_EVENT.WAITING,
    });
  });

  it('maps room view snapshots to room events', () => {
    const view = { id: 1 };

    expect(
      roomEventFromServerMessage({
        type: ROOM_SERVER_MESSAGE.ROOM_UPDATE,
        view,
      })
    ).toEqual({
      type: ROOM_EVENT.STATE_RECEIVED,
      view,
    });
    expect(
      roomEventFromServerMessage({ type: ROOM_SERVER_MESSAGE.ROOM_STATE, view })
    ).toEqual({
      type: ROOM_EVENT.STATE_RECEIVED,
      view,
    });
  });

  it('maps opponent left messages', () => {
    expect(
      roomEventFromServerMessage({ type: ROOM_SERVER_MESSAGE.OPPONENT_LEFT })
    ).toEqual({
      type: ROOM_EVENT.OPPONENT_LEFT,
    });
  });

  it('maps room errors to unavailable events', () => {
    expect(
      roomEventFromServerMessage({
        type: ROOM_SERVER_MESSAGE.ERROR,
        reason: 'room_not_found',
      })
    ).toEqual({
      type: ROOM_EVENT.ROOM_UNAVAILABLE,
      reason: 'room_not_found',
    });
  });

  it('maps unknown player errors to unavailable events', () => {
    expect(
      roomEventFromServerMessage({
        type: ROOM_SERVER_MESSAGE.ERROR,
        reason: 'unknown_player',
      })
    ).toEqual({
      type: ROOM_EVENT.ROOM_UNAVAILABLE,
      reason: 'unknown_player',
    });
  });

  it('maps other errors to server errors', () => {
    expect(
      roomEventFromServerMessage({
        type: ROOM_SERVER_MESSAGE.ERROR,
        reason: 'bad_request',
      })
    ).toEqual({
      type: ROOM_EVENT.SERVER_ERROR,
      reason: 'bad_request',
    });
  });

  it('uses a stable reason when an error omits one', () => {
    expect(
      roomEventFromServerMessage({ type: ROOM_SERVER_MESSAGE.ERROR })
    ).toEqual({
      type: ROOM_EVENT.SERVER_ERROR,
      reason: 'server_error',
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
      type: ROOM_EVENT.ROOM_ENTERED,
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
        type: ROOM_SERVER_MESSAGE.ROOM_JOINED,
        room_id: 'r1',
        player_id: 'p1',
      })
    ).toEqual({
      type: ROOM_EVENT.ROOM_ENTERED,
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
      roomEventFromServerMessage({
        type: ROOM_SERVER_MESSAGE.MATCH_FOUND,
        player_id: 'p1',
      })
    ).toEqual({
      type: ROOM_EVENT.ROOM_UNAVAILABLE,
      reason: 'invalid_room_entry',
    });
  });

  it('ignores unknown messages', () => {
    expect(roomEventFromServerMessage({ type: 'something_else' })).toEqual({
      type: ROOM_EVENT.IGNORED,
    });
  });
});
