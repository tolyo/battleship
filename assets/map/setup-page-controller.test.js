import setupPageComponent from './setup-page-controller.js';

const SetupPageController = setupPageComponent.controller;

describe('SetupPageController', () => {
  it('restores rooms on init and closes the client on destroy', () => {
    const { controller, roomClient } = mapController();

    controller.$onInit();
    controller.$onDestroy();

    expect(roomClient.restoreRoomFromCurrentUrl).toHaveBeenCalledOnceWith();
    expect(roomClient.close).toHaveBeenCalledOnceWith();
  });
});

function mapController() {
  const matchState = {};
  const roomClient = {
    restoreRoomFromCurrentUrl: jasmine.createSpy('restoreRoomFromCurrentUrl'),
    close: jasmine.createSpy('close'),
  };
  const setup = {};

  return {
    controller: new SetupPageController(matchState, roomClient, setup),
    roomClient,
    setup,
  };
}
