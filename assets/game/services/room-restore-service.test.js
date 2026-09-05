import {
  DEFAULT_RESTORE_TIMEOUT_MS,
  ROOM_UNAVAILABLE_STATUS,
} from '../domain/room-state.js';
import { RoomRestoreService } from './room-restore-service.js';
import { CONNECTION_MODE } from './connection-context.js';
import { RESTORE_REQUEST } from '../../room-store/room-store-service.js';

describe('RoomRestoreService', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('starts restore flow and schedules a timeout on success', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const entry = { roomId: 'room-1', playerId: 'player-1' };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(service.startRestore(entry, { timeoutMs: 750 })).toBe(true);

    expect(matchState.startRestore).toHaveBeenCalledOnceWith(
      'room-1',
      'player-1'
    );
    expect(timers.scheduled).toEqual([{ id: 1, delay: 750 }]);
  });

  it('uses default timeout when none is provided', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(
      service.startRestore({ roomId: 'room-1', playerId: 'player-1' })
    ).toBe(true);

    expect(timers.scheduled.map((timer) => timer.delay)).toEqual([
      DEFAULT_RESTORE_TIMEOUT_MS,
    ]);
  });

  it('does not schedule a timeout when restore is rejected', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(false),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(
      service.startRestore({ roomId: 'room-1', playerId: 'player-1' })
    ).toBe(false);
    expect(timers.scheduled).toEqual([]);
  });

  it('marks restore unavailable when restore timeout fires', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);
    service.startRestore({ roomId: 'room-1', playerId: 'player-1' });

    timers.execute();

    expect(roomRecovery.restoreFailed).toHaveBeenCalledOnceWith(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );
  });

  it('clears timeout and fails restore when requested', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    service.startRestore({ roomId: 'room-1', playerId: 'player-1' });
    service.failRestore('room-1', ROOM_UNAVAILABLE_STATUS);

    expect(roomRecovery.restoreFailed).toHaveBeenCalledOnceWith(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );
    expect(timers.cleared).toEqual([1]);
  });

  it('handles restore disconnects for restore mode', () => {
    const matchState = {
      isRestoring: true,
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = {
      restoreFailed: jasmine.createSpy('restoreFailed'),
    };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    const handled = service.handleConnectionDisconnect(
      { mode: CONNECTION_MODE.RESTORE, roomId: 'room-1', playerId: 'p1' },
      ROOM_UNAVAILABLE_STATUS
    );

    expect(handled).toBeTrue();
    expect(roomRecovery.restoreFailed).toHaveBeenCalledOnceWith(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );
  });

  it('can disable active restore session requirement', () => {
    const matchState = {
      isRestoring: false,
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = {
      restoreFailed: jasmine.createSpy('restoreFailed'),
    };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    const handled = service.handleConnectionDisconnect(
      { mode: CONNECTION_MODE.RESTORE, roomId: 'room-1', playerId: 'p1' },
      ROOM_UNAVAILABLE_STATUS,
      { requiresActiveRestore: false }
    );

    expect(handled).toBeTrue();
    expect(roomRecovery.restoreFailed).toHaveBeenCalledOnceWith(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );
  });

  it('ignores restore interruptions on non-restore context', () => {
    const matchState = {
      isRestoring: true,
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = {
      restoreFailed: jasmine.createSpy('restoreFailed'),
    };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    const handled = service.handleConnectionDisconnect(
      { mode: CONNECTION_MODE.LOBBY, roomId: 'room-1', playerId: 'p1' },
      ROOM_UNAVAILABLE_STATUS
    );

    expect(handled).toBeFalse();
    expect(roomRecovery.restoreFailed).not.toHaveBeenCalled();
  });

  it('does not handle restore disconnects when inactive restore session', () => {
    const matchState = {
      isRestoring: false,
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = {
      restoreFailed: jasmine.createSpy('restoreFailed'),
    };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    const handled = service.handleConnectionDisconnect(
      { mode: CONNECTION_MODE.RESTORE, roomId: 'room-1', playerId: 'p1' },
      ROOM_UNAVAILABLE_STATUS
    );

    expect(handled).toBeFalse();
    expect(roomRecovery.restoreFailed).not.toHaveBeenCalled();
  });

  it('maps connection disconnect handling to restore interruption behavior', () => {
    const matchState = {
      isRestoring: true,
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = {
      restoreFailed: jasmine.createSpy('restoreFailed'),
    };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    const handled = service.handleConnectionDisconnect({
      mode: CONNECTION_MODE.RESTORE,
      roomId: 'room-1',
      playerId: 'p1',
    });

    expect(handled).toBeTrue();
    expect(roomRecovery.restoreFailed).toHaveBeenCalledOnceWith(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );
  });

  it('forwards restore timeout clear calls', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    service.startRestore({ roomId: 'room-1', playerId: 'player-1' });
    service.clearRestoreTimeout();

    expect(timers.cleared).toEqual([1]);
  });

  it('restores room when current request has a remembered player', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;

    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake({
      currentRestoreRequest: jasmine
        .createSpy('currentRestoreRequest')
        .and.returnValue({
          type: RESTORE_REQUEST.RESTORE,
          roomId: 'room-1',
          playerId: 'player-1',
        }),
    });
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(service.restoreCurrentRequest()).toBe(true);

    expect(matchState.startRestore).toHaveBeenCalledOnceWith(
      'room-1',
      'player-1'
    );
  });

  it('reports unavailable state when current request lacks player id', () => {
    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
      roomUnavailable: jasmine.createSpy('roomUnavailable'),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake({
      currentRestoreRequest: jasmine
        .createSpy('currentRestoreRequest')
        .and.returnValue({
          type: RESTORE_REQUEST.MISSING_PLAYER,
          roomId: 'room-1',
        }),
    });
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(service.restoreCurrentRequest()).toBeFalse();

    expect(matchState.roomUnavailable).toHaveBeenCalledOnceWith();
    expect(roomRecovery.restoreFailed).not.toHaveBeenCalled();
  });

  it('is a no-op when no restore request is available', () => {
    const matchState = {
      startRestore: jasmine.createSpy('startRestore').and.returnValue(true),
      roomUnavailable: jasmine.createSpy('roomUnavailable'),
    };
    const roomRecovery = { restoreFailed: jasmine.createSpy('restoreFailed') };
    const roomStore = roomStoreFake();
    const service = new RoomRestoreService(roomStore, matchState, roomRecovery);

    expect(service.restoreCurrentRequest()).toBeFalse();

    expect(matchState.startRestore).not.toHaveBeenCalled();
    expect(matchState.roomUnavailable).not.toHaveBeenCalled();
    expect(roomRecovery.restoreFailed).not.toHaveBeenCalled();
  });
});

function roomStoreFake(overrides = {}) {
  return {
    currentRestoreRequest: jasmine
      .createSpy('currentRestoreRequest')
      .and.returnValue({ type: RESTORE_REQUEST.NONE }),
    ...overrides,
  };
}

function timerWindowFake() {
  let nextId = 1;
  const scheduled = [];
  const callbacks = [];
  const cleared = [];
  return {
    scheduled,
    cleared,
    window: {
      setTimeout(callback, delay) {
        const id = nextId;
        nextId += 1;
        scheduled.push({ id, delay });
        callbacks.push({ id, callback });
        return id;
      },
      clearTimeout(id) {
        cleared.push(id);
      },
    },
    execute() {
      const queue = [...callbacks];
      callbacks.length = 0;
      queue.forEach((entry) => entry.callback());
      return queue.map((entry) => entry.id);
    },
  };
}
