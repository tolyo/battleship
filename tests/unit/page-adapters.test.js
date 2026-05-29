import appConfig from '../../app/configuration.js';
import dashboardComponent from '../../app/dashboard/dashboard.js';
import GameState from '../../app/game.js';
import { HeaderController } from '../../app/layout/header-ctrl.js';
import { MapController } from '../../app/map/map-controller.js';
import registerComponent from '../../app/register/register.js';

const DashboardController = dashboardComponent.controller;
const RegisterController = registerComponent.controller;

describe('MapController', () => {
  it('restores rooms on init and closes the client on destroy', () => {
    const { controller, battleRoomClient } = mapController();

    controller.$onInit();
    controller.$onDestroy();

    expect(battleRoomClient.restoreRoomFromCurrentUrl).toHaveBeenCalledOnceWith();
    expect(battleRoomClient.close).toHaveBeenCalledOnceWith();
  });

  it('delegates setup and battle actions', () => {
    const { controller, battleRoomClient, fleetSetup } = mapController();
    const tile = { row: 1, column: 2, state: 'empty' };

    controller.random();
    controller.reset();
    controller.join();
    controller.strike(tile);

    expect(fleetSetup.randomize).toHaveBeenCalledOnceWith();
    expect(fleetSetup.reset).toHaveBeenCalledOnceWith();
    expect(battleRoomClient.joinLobby).toHaveBeenCalledOnceWith();
    expect(battleRoomClient.strike).toHaveBeenCalledOnceWith(tile);
  });
});

describe('appConfig', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('enables credentials and registers an unauthorized redirect interceptor', async () => {
    const provider = {
      defaults: {},
      interceptors: [],
    };
    globalThis.window = {
      location: {
        href: '/',
      },
    };

    appConfig(provider);

    expect(provider.defaults.withCredentials).toBe(true);
    expect(provider.interceptors.length).toBe(1);

    const interceptor = provider.interceptors[0][0]();
    await expectAsync(
      interceptor.responseError({ status: 401, data: 'unauthorized' })
    ).toBeRejectedWith({ status: 401, data: 'unauthorized' });
    expect(globalThis.window.location.href).toBe('/login');
  });

  it('rejects non-401 responses without redirecting', async () => {
    const provider = {
      defaults: {},
      interceptors: [],
    };
    globalThis.window = {
      location: {
        href: '/',
      },
    };

    appConfig(provider);

    const interceptor = provider.interceptors[0][0]();
    await expectAsync(
      interceptor.responseError({ status: 500, data: 'error' })
    ).toBeRejectedWith({ status: 500, data: 'error' });
    expect(globalThis.window.location.href).toBe('/');
  });
});

describe('HeaderController', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('removes the auth cookie and redirects to login on logout', () => {
    const cookieService = {
      remove: jasmine.createSpy('remove'),
    };
    const replace = jasmine.createSpy('replace');
    globalThis.window = {
      location: {
        replace,
      },
    };

    new HeaderController(cookieService).logout();

    expect(cookieService.remove).toHaveBeenCalledOnceWith('SEC_USER');
    expect(replace).toHaveBeenCalledOnceWith('/login');
  });
});

describe('simple component controllers', () => {
  it('dashboard marks root scope as authenticated', () => {
    const rootScope = {};

    const controller = new DashboardController(rootScope);

    expect(controller).toBeDefined();
    expect(rootScope.authenticated).toBe(true);
  });

  it('register initializes success state', () => {
    const controller = new RegisterController();

    expect(controller.success).toBe(false);
  });
});

describe('GameState', () => {
  it('moves through preparing, playing, and ended states', () => {
    const state = new GameState();

    expect(state.gamestate).toBe('PREPARING');

    state.nextState();
    expect(state.gamestate).toBe('PLAYING');

    state.nextState();
    expect(state.gamestate).toBe('ENDED');

    expect(() => state.nextState()).toThrowError('Game already ended');
  });

  it('can be reset to preparing', () => {
    const state = new GameState();
    state.nextState();
    state.init();

    expect(state.gamestate).toBe('PREPARING');
  });
});

function mapController() {
  const gameState = {};
  const battleRoomClient = {
    restoreRoomFromCurrentUrl: jasmine.createSpy('restoreRoomFromCurrentUrl'),
    close: jasmine.createSpy('close'),
    joinLobby: jasmine.createSpy('joinLobby'),
    strike: jasmine.createSpy('strike'),
  };
  const fleetSetup = {
    randomize: jasmine.createSpy('randomize'),
    reset: jasmine.createSpy('reset'),
  };

  return {
    controller: new MapController(gameState, battleRoomClient, fleetSetup),
    battleRoomClient,
    fleetSetup,
  };
}
