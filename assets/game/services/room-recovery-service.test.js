import { RoomRecoveryService } from './room-recovery-service.js';
import { ROOM_UNAVAILABLE_STATUS } from '../domain/room-state.js';

describe('RoomRecoveryService', () => {
  it('marks restore failure and routes user back to setup state', () => {
    const roomStore = {
      leaveRoom: jasmine.createSpy('leaveRoom'),
    };
    const matchState = {
      returnToSetup: jasmine.createSpy('returnToSetup'),
    };

    new RoomRecoveryService(roomStore, matchState).restoreFailed(
      'room-1',
      ROOM_UNAVAILABLE_STATUS
    );

    expect(roomStore.leaveRoom).toHaveBeenCalledOnceWith('room-1');
    expect(matchState.returnToSetup).toHaveBeenCalledOnceWith(
      ROOM_UNAVAILABLE_STATUS
    );
  });
});
