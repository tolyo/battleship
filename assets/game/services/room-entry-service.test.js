import { RoomEntryService } from './room-entry-service.js';

describe('RoomEntryService', () => {
  it('enters rooms, applies initial view snapshots, and persists room identity', () => {
    const { service, roomStore, matchState, matchView } = roomEntryService();
    const entry = {
      roomId: 'room-1',
      playerId: 'player-1',
      opponentId: 'player-2',
      view: { id: 1 },
    };

    expect(service.enter(entry, { updateUrl: true })).toBe(true);

    expect(matchState.enterRoom).toHaveBeenCalledOnceWith(entry);
    expect(matchView.receiveSnapshot).toHaveBeenCalledOnceWith({ id: 1 });
    expect(roomStore.enterRoom).toHaveBeenCalledOnceWith('room-1', 'player-1', {
      updateUrl: true,
    });
  });

  it('persists room identity without applying missing view snapshots', () => {
    const { service, roomStore, matchView } = roomEntryService();

    expect(
      service.enter(
        {
          roomId: 'room-1',
          playerId: 'player-1',
          opponentId: undefined,
        },
        { updateUrl: false }
      )
    ).toBe(true);

    expect(matchView.receiveSnapshot).not.toHaveBeenCalled();
    expect(roomStore.enterRoom).toHaveBeenCalledOnceWith('room-1', 'player-1', {
      updateUrl: false,
    });
  });

  it('does not persist room identity when match state rejects room entry', () => {
    const { service, roomStore, matchView } = roomEntryService({
      matchStateOverrides: {
        enterRoom: jasmine.createSpy('enterRoom').and.returnValue(false),
      },
    });

    expect(
      service.enter(
        {
          roomId: 'room-1',
          playerId: 'player-1',
          opponentId: 'player-2',
          view: { id: 1 },
        },
        { updateUrl: true }
      )
    ).toBe(false);

    expect(roomStore.enterRoom).not.toHaveBeenCalled();
    expect(matchView.receiveSnapshot).not.toHaveBeenCalled();
  });
});

function roomEntryService({ matchStateOverrides = {} } = {}) {
  const roomStore = {
    enterRoom: jasmine.createSpy('enterRoom'),
  };
  const matchState = {
    enterRoom: jasmine.createSpy('enterRoom').and.returnValue(true),
    ...matchStateOverrides,
  };
  const matchView = {
    receiveSnapshot: jasmine.createSpy('receiveSnapshot'),
  };
  const service = new RoomEntryService(roomStore, matchState, matchView);

  return { service, roomStore, matchState, matchView };
}
