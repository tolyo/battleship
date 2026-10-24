import { create } from '../../../_build/browser/js/modules/battleship/client/SetupPage.js';

describe('Terlan setup page controller', () => {
  it('preserves its services and restores and closes the room through Angular hooks', () => {
    const state = {};
    const setup = {};
    const calls = [];
    const client = {
      restoreRoomFromCurrentUrl() {
        expect(this).toBe(client);
        calls.push('restore');
        return true;
      },
      close() {
        expect(this).toBe(client);
        calls.push('close');
      },
    };
    const controller = Reflect.construct(create, [state, client, setup]);

    expect(controller.state).toBe(state);
    expect(controller.setup).toBe(setup);
    expect(controller.roomClient).toBe(client);
    controller.$onInit();
    controller.$onDestroy();
    expect(calls).toEqual(['restore', 'close']);
  });
});
