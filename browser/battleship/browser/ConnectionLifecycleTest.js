import { create as createConnectionLifecycle } from '../../../_build/browser/js/modules/battleship/browser/ConnectionLifecycle.js';
import {
  lobby,
  restore,
  is_restore as isRestoreContext,
} from '../../../_build/browser/js/modules/battleship/browser/ConnectionContext.js';

describe('RoomConnectionLifecycleService', () => {
  it('maps restore-mode close events to restore flow and close callback', () => {
    const { service, matchState, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: true },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onClose(
      restore({ roomId: 'room-1', playerId: 'player-1' }),
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );
    expect(onRestoreClose).toHaveBeenCalledOnceWith();
    expect(matchState.connectionClosed).not.toHaveBeenCalled();
  });

  it('maps lobby close events to connection closed state', () => {
    const { service, matchState } = roomConnectionLifecycle();

    service.onClose(lobby(), jasmine.createSpy('onRestoreClose'));

    expect(matchState.connectionClosed).toHaveBeenCalledOnceWith();
  });

  it('maps restore-mode error events to restore flow and close callback', () => {
    const { service, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: true },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onError(
      restore({ roomId: 'room-1', playerId: 'player-1' }),
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );
    expect(onRestoreClose).toHaveBeenCalledOnceWith();
  });

  it('maps lobby error events to connection error state', () => {
    const { service, matchState } = roomConnectionLifecycle();

    service.onError(lobby(), jasmine.createSpy('onRestoreClose'));

    expect(matchState.connectionError).toHaveBeenCalledOnceWith();
  });

  it('does nothing extra when restore handling is inactive', () => {
    const { service, matchState, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: false },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onError(
      restore({ roomId: 'room-1', playerId: 'player-1' }),
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith(
      restore({ roomId: 'room-1', playerId: 'player-1' })
    );
    expect(onRestoreClose).not.toHaveBeenCalled();
    expect(matchState.connectionError).toHaveBeenCalledOnceWith();
  });
});

function roomConnectionLifecycle({ matchStateOverrides = {} } = {}) {
  const matchState = {
    isRestoring: false,
    connectionClosed: jasmine
      .createSpy('connectionClosed')
      .and.returnValue(true),
    connectionError: jasmine.createSpy('connectionError').and.returnValue(true),
    ...matchStateOverrides,
  };
  const roomRestore = {
    handleConnectionDisconnect: jasmine
      .createSpy('handleConnectionDisconnect')
      .and.callFake(
        (context) => isRestoreContext(context) && matchState.isRestoring
      ),
    failRestore: jasmine.createSpy('failRestore'),
  };
  const service = createConnectionLifecycle(roomRestore, matchState);

  return { service, matchState, roomRestore };
}
