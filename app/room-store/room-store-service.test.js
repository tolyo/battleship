import { RoomStoreService } from './room-store-service.js';

describe('RoomStoreService', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('returns and reuses a stored anonymous player name', () => {
    const storage = storageFake({
      'battleship.anonymousPlayerName': 'Anonymous STORED',
    });
    globalThis.window = windowFake({ storage });

    expect(new RoomStoreService().anonymousPlayerName()).toBe(
      'Anonymous STORED'
    );
    expect(storage.get('battleship.anonymousPlayerName')).toBe(
      'Anonymous STORED'
    );
  });

  it('creates an anonymous player name when none is stored', () => {
    const storage = storageFake();
    globalThis.window = windowFake({ storage });

    const name = new RoomStoreService().anonymousPlayerName();

    expect(name).toMatch(/^Anonymous [0-9A-Z]{6}$/);
    expect(storage.get('battleship.anonymousPlayerName')).toBe(name);
  });

  it('parses the current room id from the path', () => {
    globalThis.window = windowFake({
      pathname: '/room/alpha%20beta',
    });

    expect(new RoomStoreService().currentRoomId()).toBe('alpha beta');
  });

  it('returns the current restore request with its remembered player', () => {
    const storage = storageFake({
      'battleship.roomPlayer.room-1': 'player-1',
    });
    globalThis.window = windowFake({
      pathname: '/room/room-1',
      storage,
    });

    expect(new RoomStoreService().currentRestoreRequest()).toEqual({
      type: 'restore',
      roomId: 'room-1',
      playerId: 'player-1',
    });
  });

  it('returns a missing player restore request for room urls without a remembered player', () => {
    globalThis.window = windowFake({
      pathname: '/room/room-1',
    });

    expect(new RoomStoreService().currentRestoreRequest()).toEqual({
      type: 'missing_player',
      roomId: 'room-1',
    });
  });

  it('returns undefined outside room paths', () => {
    globalThis.window = windowFake({
      pathname: '/dashboard',
    });

    expect(new RoomStoreService().currentRoomId()).toBeUndefined();
    expect(new RoomStoreService().currentRestoreRequest()).toEqual({
      type: 'none',
    });
  });

  it('remembers and forgets room players', () => {
    const storage = storageFake();
    globalThis.window = windowFake({ storage });
    const store = new RoomStoreService();

    store.rememberPlayer('room-1', 'player-1');
    expect(store.playerId('room-1')).toBe('player-1');

    store.forgetPlayer('room-1');
    expect(store.playerId('room-1')).toBeUndefined();
  });

  it('enters a room by remembering the player and optionally updating the url', () => {
    const storage = storageFake();
    const historyCalls = [];
    globalThis.window = windowFake({ storage, historyCalls });
    const store = new RoomStoreService();

    store.enterRoom('room 1', 'player-1', { updateUrl: true });

    expect(store.playerId('room 1')).toBe('player-1');
    expect(historyCalls).toEqual([[null, '', '/room/room%201']]);
  });

  it('leaves a room by forgetting the player and returning home', () => {
    const storage = storageFake();
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/room/room-1',
      storage,
      historyCalls,
    });
    const store = new RoomStoreService();

    store.rememberPlayer('room-1', 'player-1');
    store.leaveRoom('room-1');

    expect(store.playerId('room-1')).toBeUndefined();
    expect(historyCalls).toEqual([[null, '', '/']]);
  });

  it('updates browser history when showing a room', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/',
      historyCalls,
    });

    new RoomStoreService().showRoom('room 1');

    expect(historyCalls).toEqual([[null, '', '/room/room%201']]);
  });

  it('does not update history when already showing the same room', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/room/room%201',
      historyCalls,
    });

    new RoomStoreService().showRoom('room 1');

    expect(historyCalls).toEqual([]);
  });

  it('returns home from a room path', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/room/room-1',
      historyCalls,
    });

    new RoomStoreService().showHome();

    expect(historyCalls).toEqual([[null, '', '/']]);
  });

  it('survives unavailable local storage', () => {
    globalThis.window = windowFake({
      storage: {
        getItem() {
          throw new Error('blocked');
        },
        setItem() {
          throw new Error('blocked');
        },
        removeItem() {
          throw new Error('blocked');
        },
      },
    });
    const store = new RoomStoreService();

    expect(store.anonymousPlayerName()).toMatch(/^Anonymous [0-9A-Z]{6}$/);
    expect(store.playerId('room-1')).toBeUndefined();
    expect(() => store.rememberPlayer('room-1', 'player-1')).not.toThrow();
    expect(() => store.forgetPlayer('room-1')).not.toThrow();
  });
});

function storageFake(initial = {}) {
  const values = new Map(Object.entries(initial));
  return {
    get: (key) => values.get(key),
    getItem: (key) => values.get(key) ?? null,
    setItem: (key, value) => {
      values.set(key, value);
    },
    removeItem: (key) => {
      values.delete(key);
    },
  };
}

function windowFake({ pathname = '/', storage = storageFake(), historyCalls = [] } = {}) {
  return {
    localStorage: storage,
    location: {
      pathname,
    },
    history: {
      replaceState: (...args) => {
        historyCalls.push(args);
      },
    },
  };
}
