import { create } from '../../../_build/browser/js/modules/battleship/client/OperationTimeout.js';

describe('OperationTimeout', () => {
  it('starts a timeout with the requested delay', () => {
    const timers = timerWindowFake();
    const callback = jasmine.createSpy('callback');

    create(timers.window).start(callback, 123);

    expect(timers.scheduled).toEqual([{ id: 1, callback, delay: 123 }]);
  });

  it('clears an existing timeout before starting another', () => {
    const timers = timerWindowFake();
    const timeout = create(timers.window);

    timeout.start(() => {}, 100);
    timeout.start(() => {}, 200);

    expect(timers.cleared).toEqual([1]);
    expect(timers.scheduled.map((timer) => timer.delay)).toEqual([100, 200]);
  });

  it('clears the active timeout once', () => {
    const timers = timerWindowFake();
    const timeout = create(timers.window);

    timeout.start(() => {}, 100);
    timeout.clear();
    timeout.clear();

    expect(timers.cleared).toEqual([1]);
  });

  it('keeps timer state per instance and clears zero handles with the original receiver', () => {
    let next = 0;
    const cleared = [];
    const host = {
      setTimeout() {
        expect(this).toBe(host);
        return next++;
      },
      clearTimeout(handle) {
        expect(this).toBe(host);
        cleared.push(handle);
      },
    };
    const first = create(host);
    const second = create(host);
    first.start(() => {}, 100);
    second.start(() => {}, 200);
    first.clear();
    first.clear();
    expect(cleared).toEqual([0]);
    second.clear();
    expect(cleared).toEqual([0, 1]);
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
