import { create } from '../../../_build/browser/js/modules/battleship/client/Dashboard.js';

describe('dashboard controller', () => {
  it('marks root scope as authenticated', () => {
    const rootScope = {};

    const controller = Reflect.construct(create, [rootScope]);

    expect(controller).toBeDefined();
    expect(rootScope.authenticated).toBe(true);
  });

  it('updates the injected scope through its original proxy exactly once', () => {
    const writes = [];
    const scope = new Proxy(
      {},
      {
        set(target, key, value, receiver) {
          writes.push({ key, value, receiver });
          return Reflect.set(target, key, value, receiver);
        },
      }
    );
    const controller = Reflect.construct(create, [scope]);
    expect(controller).not.toBe(scope);
    expect(writes).toEqual([
      { key: 'authenticated', value: true, receiver: scope },
    ]);
  });
});
