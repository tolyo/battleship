import { SessionTimeout } from '../../app/session/session-timeout.js';

describe('SessionTimeout', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('starts a timeout with the requested delay', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;
    const callback = jasmine.createSpy('callback');

    new SessionTimeout().start(callback, 123);

    expect(timers.scheduled).toEqual([{ id: 1, callback, delay: 123 }]);
  });

  it('clears an existing timeout before starting another', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;
    const timeout = new SessionTimeout();

    timeout.start(() => {}, 100);
    timeout.start(() => {}, 200);

    expect(timers.cleared).toEqual([1]);
    expect(timers.scheduled.map((timer) => timer.delay)).toEqual([100, 200]);
  });

  it('clears the active timeout once', () => {
    const timers = timerWindowFake();
    globalThis.window = timers.window;
    const timeout = new SessionTimeout();

    timeout.start(() => {}, 100);
    timeout.clear();
    timeout.clear();

    expect(timers.cleared).toEqual([1]);
    expect(timeout.timerId).toBeUndefined();
  });
});

function timerWindowFake() {
  let nextId = 1;
  const scheduled = [];
  const cleared = [];
  return {
    scheduled,
    cleared,
    window: {
      setTimeout(callback, delay) {
        const id = nextId;
        nextId += 1;
        scheduled.push({ id, callback, delay });
        return id;
      },
      clearTimeout(id) {
        cleared.push(id);
      },
    },
  };
}
