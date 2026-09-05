import { configureBattleship } from './application.js';

describe('configureBattleship', () => {
  it('registers routes, services, controllers, and components', () => {
    const module = moduleFake();
    const angular = {
      module: jasmine.createSpy('module').and.returnValue(module),
    };

    expect(configureBattleship(angular)).toBe(module);
    expect(angular.module).toHaveBeenCalledWith('battleship', []);

    const routeConfig = module.config.calls.allArgs()[1][0];
    const stateProvider = stateProviderFake();
    routeConfig.at(-1)(stateProvider);

    expect(stateProvider.states.map((state) => state.name)).toEqual([
      'dashboard',
      'room',
      'activeRoom',
      'register',
      'login',
      'home',
    ]);
    expect(module.service).toHaveBeenCalledTimes(12);
    expect(module.controller).toHaveBeenCalledWith(
      'HeaderController',
      jasmine.any(Function)
    );
    expect(module.component).toHaveBeenCalledTimes(11);
  });
});

function moduleFake() {
  const module = {};
  ['config', 'service', 'controller', 'component'].forEach((method) => {
    module[method] = jasmine.createSpy(method).and.callFake(() => module);
  });
  return module;
}

function stateProviderFake() {
  return {
    states: [],
    state(state) {
      this.states.push(state);
      return this;
    },
  };
}
