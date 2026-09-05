import appConfig from './configuration.js';

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
