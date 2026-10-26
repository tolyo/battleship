import { create } from '../../../_build/browser/js/modules/battleship/client/RoomEntry.js';

describe('RoomEntryService', () => {
  it('preserves snapshot values, receiver identity, and state-view-store ordering', () => {
    for (const snapshot of [undefined, null, { phase: 'setup' }]) {
      const calls = [];
      const entry = { roomId: 'room-1', playerId: 'player-1', view: snapshot };
      const options = { updateUrl: true, unrelated: 'discard' };
      const store = {
        enterRoom(roomId, playerId, persisted) {
          expect(this).toBe(store);
          expect([roomId, playerId]).toEqual(['room-1', 'player-1']);
          expect(persisted).toEqual({ updateUrl: true });
          expect(persisted).not.toBe(options);
          calls.push('store');
        },
      };
      const state = {
        enterRoom(value) {
          expect(this).toBe(state);
          expect(value).toBe(entry);
          calls.push('state');
          return true;
        },
      };
      const view = {
        receiveSnapshot(value) {
          expect(this).toBe(view);
          expect(value).toBe(snapshot);
          calls.push('view');
          return false;
        },
      };
      expect(create(store, state, view).enter(entry, options)).toBe(true);
      expect(calls).toEqual(
        snapshot === undefined ? ['state', 'store'] : ['state', 'view', 'store']
      );
    }
  });

  it('does not read snapshots or identity fields when entry is rejected', () => {
    const { service } = roomEntryService({
      matchStateOverrides: { enterRoom: () => false },
    });
    const entry = new Proxy(
      {},
      {
        get() {
          throw new Error('rejected entry was read');
        },
      }
    );
    expect(service.enter(entry, { updateUrl: false })).toBe(false);
  });

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
    receiveSnapshot: jasmine.createSpy('receiveSnapshot').and.returnValue(true),
  };
  const service = Reflect.construct(create, [roomStore, matchState, matchView]);

  return { service, roomStore, matchState, matchView };
}
