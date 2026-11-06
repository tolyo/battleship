import { create } from '../../../_build/browser/js/modules/battleship/client/Header.js';

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

    Reflect.construct(create, [cookieService, globalThis.window]).logout();

    expect(cookieService.remove).toHaveBeenCalledOnceWith('SEC_USER');
    expect(replace).toHaveBeenCalledOnceWith('/login');
  });
});
