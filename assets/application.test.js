import { configureBattleship } from './application.js';

describe('configureBattleship', () => {
  it('registers routes, services, controllers, and components', () => {
    const module = moduleFake();
    const angular = {
      module: jasmine.createSpy('module').and.returnValue(module),
    };

    expect(configureBattleship(angular)).toBe(module);
    expect(angular.module).toHaveBeenCalledWith('battleship', []);

    expect(
      module.router.calls.argsFor(0)[0].map((state) => state.name)
    ).toEqual(['dashboard', 'room', 'activeRoom', 'register', 'login', 'home']);
    expect(module.service).toHaveBeenCalledTimes(13);
    expect(module.value).toHaveBeenCalledWith('snapshotProjection', {
      project: jasmine.any(Function),
    });
    expect(module.value).toHaveBeenCalledTimes(2);
    expect(module.controller).toHaveBeenCalledWith(
      'HeaderController',
      jasmine.any(Function)
    );
    expect(module.component).toHaveBeenCalledTimes(11);
  });
});

function moduleFake() {
  const module = {};
  ['config', 'router', 'service', 'value', 'controller', 'component'].forEach(
    (method) => {
      module[method] = jasmine.createSpy(method).and.callFake(() => module);
    }
  );
  return module;
}
