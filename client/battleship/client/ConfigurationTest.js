import { create } from '../../../_build/browser/js/modules/battleship/client/Configuration.js';

describe('appConfig', () => {
  let originalWindow;

  beforeEach(() => {
    originalWindow = globalThis.window;
  });

  afterEach(() => {
    globalThis.window = originalWindow;
  });

  it('uses same-origin cookies and registers an unauthorized redirect interceptor', async () => {
    const provider = {
      defaults: {},
      interceptors: [],
    };
    globalThis.window = {
      location: {
        href: '/',
      },
    };

    Object.assign(provider, create(globalThis.window, Promise).$http);

    expect(provider.defaults.credentials).toBe('same-origin');
    expect(provider.interceptors.length).toBe(1);

    const interceptor = provider.interceptors[0]();
    await expectAsync(
      interceptor.responseError({ status: 401, data: 'unauthorized' })
    ).toBeRejectedWith({ status: 401, data: 'unauthorized' });
    expect(globalThis.window.location.href).toBe('/login');
  });

  it('keeps rejection identity and reads the current browser location', async () => {
    const browser = { location: { href: '/' } };
    const configuration = create(browser, Promise);
    const response = {
      status: 401,
      data: { reason: 'expired' },
      extra: 'retained',
    };
    browser.location = { href: '/room/1' };
    await configuration.$http.interceptors[0]()
      .responseError(response)
      .catch((rejected) => {
        expect(rejected).toBe(response);
      });
    expect(browser.location.href).toBe('/login');
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

    Object.assign(provider, create(globalThis.window, Promise).$http);

    const interceptor = provider.interceptors[0]();
    await expectAsync(
      interceptor.responseError({ status: 500, data: 'error' })
    ).toBeRejectedWith({ status: 500, data: 'error' });
    expect(globalThis.window.location.href).toBe('/');
  });
});
