import { SessionService } from '../../app/session/session-service.js';

describe('SessionService', () => {
  it('opens websocket connections and reports active/open state', () => {
    const websocket = websocketFake();
    const opened = jasmine.createSpy('opened');
    const session = new SessionService(websocket.connect);

    session.connect('/ws', { onOpen: opened });

    expect(session.isActive()).toBe(true);
    expect(session.isOpen()).toBe(false);
    expect(websocket.connections[0].url).toBe('/ws');
    expect(websocket.connections[0].protocols).toEqual([]);
    expect(websocket.connections[0].options).toEqual(
      jasmine.objectContaining({
        heartbeatTimeout: 0,
        maxRetries: 0,
      })
    );

    websocket.connections[0].options.onOpen();

    expect(session.isOpen()).toBe(true);
    expect(opened).toHaveBeenCalledOnceWith();
  });

  it('passes messages from the active connection', () => {
    const websocket = websocketFake();
    const onMessage = jasmine.createSpy('onMessage');
    const session = new SessionService(websocket.connect);

    session.connect('/ws', { onMessage });
    websocket.connections[0].options.onMessage({ type: 'game_update' });

    expect(onMessage).toHaveBeenCalledOnceWith({ type: 'game_update' });
  });

  it('ignores stale connection callbacks after reconnecting', () => {
    const websocket = websocketFake();
    const onOpen = jasmine.createSpy('onOpen');
    const onMessage = jasmine.createSpy('onMessage');
    const session = new SessionService(websocket.connect);

    session.connect('/first', { onOpen, onMessage });
    session.connect('/second', { onOpen, onMessage });

    websocket.connections[0].options.onOpen();
    websocket.connections[0].options.onMessage('stale');
    websocket.connections[1].options.onOpen();
    websocket.connections[1].options.onMessage('fresh');

    expect(onOpen).toHaveBeenCalledTimes(1);
    expect(onMessage).toHaveBeenCalledOnceWith('fresh');
    expect(websocket.connections[0].connection.close).toHaveBeenCalledOnceWith();
  });

  it('sends through the active connection and closes it', () => {
    const websocket = websocketFake();
    const onClose = jasmine.createSpy('onClose');
    const session = new SessionService(websocket.connect);

    session.connect('/ws', { onClose });
    session.send({ type: 'move', row: 1, column: 2 });
    session.close();
    websocket.connections[0].options.onClose();

    expect(websocket.connections[0].connection.send).toHaveBeenCalledOnceWith({
      type: 'move',
      row: 1,
      column: 2,
    });
    expect(websocket.connections[0].connection.close).toHaveBeenCalledOnceWith();
    expect(session.isActive()).toBe(false);
    expect(onClose).not.toHaveBeenCalled();
  });

  it('marks closed when the active connection closes', () => {
    const websocket = websocketFake();
    const onClose = jasmine.createSpy('onClose');
    const session = new SessionService(websocket.connect);

    session.connect('/ws', { onClose });
    websocket.connections[0].options.onClose();

    expect(session.isActive()).toBe(false);
    expect(onClose).toHaveBeenCalledOnceWith();
  });

  it('reports active connection errors', () => {
    const websocket = websocketFake();
    const onError = jasmine.createSpy('onError');
    const session = new SessionService(websocket.connect);

    session.connect('/ws', { onError });
    websocket.connections[0].options.onError();

    expect(onError).toHaveBeenCalledOnceWith();
  });
});

function websocketFake() {
  const connections = [];
  return {
    connections,
    connect(url, protocols, options) {
      const connection = {
        send: jasmine.createSpy('send'),
        close: jasmine.createSpy('close'),
      };
      connections.push({ url, protocols, options, connection });
      return connection;
    },
  };
}
