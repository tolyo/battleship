import { RoomConnectionLifecycleService } from './room-connection-lifecycle-service.js';
import { CONNECTION_MODE } from './connection-context.js';

describe('RoomConnectionLifecycleService', () => {
  it('maps restore-mode close events to restore flow and close callback', () => {
    const { service, matchState, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: true },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onClose(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith({
      mode: CONNECTION_MODE.RESTORE,
      roomId: 'room-1',
      playerId: 'player-1',
    });
    expect(onRestoreClose).toHaveBeenCalledOnceWith();
    expect(matchState.connectionClosed).not.toHaveBeenCalled();
  });

  it('maps lobby close events to connection closed state', () => {
    const { service, matchState } = roomConnectionLifecycle();

    service.onClose(
      { mode: CONNECTION_MODE.LOBBY },
      jasmine.createSpy('onRestoreClose')
    );

    expect(matchState.connectionClosed).toHaveBeenCalledOnceWith();
  });

  it('maps restore-mode error events to restore flow and close callback', () => {
    const { service, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: true },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onError(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith({
      mode: CONNECTION_MODE.RESTORE,
      roomId: 'room-1',
      playerId: 'player-1',
    });
    expect(onRestoreClose).toHaveBeenCalledOnceWith();
  });

  it('maps lobby error events to connection error state', () => {
    const { service, matchState } = roomConnectionLifecycle();

    service.onError(
      { mode: CONNECTION_MODE.LOBBY },
      jasmine.createSpy('onRestoreClose')
    );

    expect(matchState.connectionError).toHaveBeenCalledOnceWith();
  });

  it('does nothing extra when restore handling is inactive', () => {
    const { service, matchState, roomRestore } = roomConnectionLifecycle({
      matchStateOverrides: { isRestoring: false },
    });
    const onRestoreClose = jasmine.createSpy('onRestoreClose');

    service.onError(
      {
        mode: CONNECTION_MODE.RESTORE,
        roomId: 'room-1',
        playerId: 'player-1',
      },
      onRestoreClose
    );

    expect(roomRestore.handleConnectionDisconnect).toHaveBeenCalledOnceWith({
      mode: CONNECTION_MODE.RESTORE,
      roomId: 'room-1',
      playerId: 'player-1',
    });
    expect(onRestoreClose).not.toHaveBeenCalled();
    expect(matchState.connectionError).toHaveBeenCalledOnceWith();
  });
});

function roomConnectionLifecycle({ matchStateOverrides = {} } = {}) {
  const matchState = {
    isRestoring: false,
    connectionClosed: jasmine.createSpy('connectionClosed'),
    connectionError: jasmine.createSpy('connectionError'),
    ...matchStateOverrides,
  };
  const roomRestore = {
    handleConnectionDisconnect: jasmine
      .createSpy('handleConnectionDisconnect')
      .and.callFake(
        (context) =>
          context.mode === CONNECTION_MODE.RESTORE && matchState.isRestoring
      ),
    failRestore: jasmine.createSpy('failRestore'),
  };
  const service = new RoomConnectionLifecycleService(roomRestore, matchState);

  return { service, matchState, roomRestore };
}
