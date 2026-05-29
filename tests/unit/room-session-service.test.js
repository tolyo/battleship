import { RoomSessionService } from '../../app/room/room-session-service.js';

describe('RoomSessionService', () => {
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

    expect(new RoomSessionService().anonymousPlayerName()).toBe(
      'Anonymous STORED'
    );
    expect(storage.get('battleship.anonymousPlayerName')).toBe(
      'Anonymous STORED'
    );
  });

  it('creates an anonymous player name when none is stored', () => {
    const storage = storageFake();
    globalThis.window = windowFake({ storage });

    const name = new RoomSessionService().anonymousPlayerName();

    expect(name).toMatch(/^Anonymous [0-9A-Z]{6}$/);
    expect(storage.get('battleship.anonymousPlayerName')).toBe(name);
  });

  it('parses the current room id from the path', () => {
    globalThis.window = windowFake({
      pathname: '/room/alpha%20beta',
    });

    expect(new RoomSessionService().currentRoomId()).toBe('alpha beta');
  });

  it('returns undefined outside room paths', () => {
    globalThis.window = windowFake({
      pathname: '/dashboard',
    });

    expect(new RoomSessionService().currentRoomId()).toBeUndefined();
  });

  it('remembers and forgets room players', () => {
    const storage = storageFake();
    globalThis.window = windowFake({ storage });
    const session = new RoomSessionService();

    session.rememberPlayer('room-1', 'player-1');
    expect(session.playerId('room-1')).toBe('player-1');

    session.forgetPlayer('room-1');
    expect(session.playerId('room-1')).toBeUndefined();
  });

  it('updates browser history when showing a room', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/',
      historyCalls,
    });

    new RoomSessionService().showRoom('room 1');

    expect(historyCalls).toEqual([[null, '', '/room/room%201']]);
  });

  it('does not update history when already showing the same room', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/room/room%201',
      historyCalls,
    });

    new RoomSessionService().showRoom('room 1');

    expect(historyCalls).toEqual([]);
  });

  it('returns home from a room path', () => {
    const historyCalls = [];
    globalThis.window = windowFake({
      pathname: '/room/room-1',
      historyCalls,
    });

    new RoomSessionService().showHome();

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
    const session = new RoomSessionService();

    expect(session.anonymousPlayerName()).toMatch(/^Anonymous [0-9A-Z]{6}$/);
    expect(session.playerId('room-1')).toBeUndefined();
    expect(() => session.rememberPlayer('room-1', 'player-1')).not.toThrow();
    expect(() => session.forgetPlayer('room-1')).not.toThrow();
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
