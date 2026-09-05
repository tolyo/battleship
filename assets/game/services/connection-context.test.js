import {
  CONNECTION_MODE,
  isLobbyContext,
  isRestoreContext,
} from './connection-context.js';

describe('connection context predicates', () => {
  it('identifies lobby context', () => {
    expect(
      isLobbyContext({
        mode: CONNECTION_MODE.LOBBY,
      })
    ).toBeTrue();

    expect(
      isLobbyContext({
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      })
    ).toBeFalse();
  });

  it('identifies restore context', () => {
    expect(
      isRestoreContext({
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      })
    ).toBeTrue();

    expect(
      isRestoreContext({
        mode: CONNECTION_MODE.LOBBY,
      })
    ).toBeFalse();
  });
});
