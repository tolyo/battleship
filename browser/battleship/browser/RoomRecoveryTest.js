import { create as createRoomRecovery } from '../../../_build/browser/js/modules/battleship/browser/RoomRecovery.js';
import { ROOM_UNAVAILABLE_STATUS } from '../../../assets/game/domain/room-state.js';

describe('RoomRecoveryService', () => {
  it('preserves method receivers while cleaning up before resetting the match', () => {
    const calls = [];
    const store = {
      id: 'room-1',
      leaveRoom(roomId) {
        calls.push([this.id, roomId]);
      },
    };
    const state = {
      status: 'reset',
      returnToSetup(status) {
        calls.push([this.status, status]);
        return true;
      },
    };
    expect(
      createRoomRecovery(store, state).restoreFailed('room-1', 'unavailable')
    ).toBe(true);
    expect(calls).toEqual([
      ['room-1', 'room-1'],
      ['reset', 'unavailable'],
    ]);
  });

  it('marks restore failure and routes user back to setup state', () => {
    const roomStore = {
      leaveRoom: jasmine.createSpy('leaveRoom'),
    };
    const matchState = {
      returnToSetup: jasmine.createSpy('returnToSetup').and.returnValue(true),
    };

    createRoomRecovery(roomStore, matchState).restoreFailed(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );

    expect(roomStore.leaveRoom).toHaveBeenCalledOnceWith('room-1');
    expect(matchState.returnToSetup).toHaveBeenCalledOnceWith(
      ROOM_UNAVAILABLE_STATUS
    );
  });
});
