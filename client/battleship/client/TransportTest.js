import { create } from '../../../_build/browser/js/modules/battleship/client/Transport.js';

describe('ConnectionService', () => {
  it('opens websocket connections and reports active/open state', () => {
    const websocket = websocketFake();
    const opened = jasmine.createSpy('opened');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/ws', handlers({ onOpen: opened }));

    expect(connection.isActive()).toBe(true);
    expect(connection.isOpen()).toBe(false);
    expect(websocket.connections[0].url).toBe('/ws');
    expect(websocket.connections[0].protocols).toEqual([]);
    expect(websocket.connections[0].options).toEqual(
      jasmine.objectContaining({
        heartbeatTimeout: 0,
        maxRetries: 0,
      })
    );

    websocket.connections[0].options.onOpen();

    expect(connection.isOpen()).toBe(true);
    expect(opened).toHaveBeenCalledOnceWith();
  });

  it('passes messages from the active connection', () => {
    const websocket = websocketFake();
    const onMessage = jasmine.createSpy('onMessage');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/ws', handlers({ onMessage }));
    websocket.connections[0].options.onMessage({ type: 'room_update' });

    expect(onMessage).toHaveBeenCalledOnceWith({ type: 'room_update' });
  });

  it('ignores stale connection callbacks after reconnecting', () => {
    const websocket = websocketFake();
    const onOpen = jasmine.createSpy('onOpen');
    const onMessage = jasmine.createSpy('onMessage');
    const onError = jasmine.createSpy('onError');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/first', handlers({ onOpen, onMessage, onError }));
    connection.connect('/second', handlers({ onOpen, onMessage, onError }));

    websocket.connections[0].options.onOpen();
    websocket.connections[0].options.onMessage('stale');
    websocket.connections[0].options.onError();
    websocket.connections[1].options.onOpen();
    websocket.connections[1].options.onMessage('fresh');

    expect(onOpen).toHaveBeenCalledTimes(1);
    expect(onMessage).toHaveBeenCalledOnceWith('fresh');
    expect(onError).not.toHaveBeenCalled();
    expect(
      websocket.connections[0].connection.close
    ).toHaveBeenCalledOnceWith();
  });

  it('sends through the active connection and closes it', () => {
    const websocket = websocketFake();
    const onClose = jasmine.createSpy('onClose');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/ws', handlers({ onClose }));
    connection.send({ type: 'move', row: 1, column: 2 });
    connection.close();
    websocket.connections[0].options.onClose();

    expect(websocket.connections[0].connection.send).toHaveBeenCalledOnceWith({
      type: 'move',
      row: 1,
      column: 2,
    });
    expect(
      websocket.connections[0].connection.close
    ).toHaveBeenCalledOnceWith();
    expect(connection.isActive()).toBe(false);
    expect(onClose).not.toHaveBeenCalled();
  });

  it('marks closed when the active connection closes', () => {
    const websocket = websocketFake();
    const onClose = jasmine.createSpy('onClose');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/ws', handlers({ onClose }));
    websocket.connections[0].options.onClose();

    expect(connection.isActive()).toBe(false);
    expect(onClose).toHaveBeenCalledOnceWith();
  });

  it('ignores old close events without clearing the replacement socket', () => {
    const websocket = websocketFake();
    const closed = jasmine.createSpy('closed');
    const connection = create(websocket.connect);
    connection.connect('/first', handlers({ onClose: closed }));
    connection.connect('/second', handlers({ onClose: closed }));
    websocket.connections[0].options.onClose();
    websocket.connections[1].options.onOpen();
    connection.send('fresh');
    expect(connection.isOpen()).toBeTrue();
    expect(websocket.connections[1].connection.send).toHaveBeenCalledOnceWith(
      'fresh'
    );
    expect(closed).not.toHaveBeenCalled();
  });

  it('ignores every callback after the active socket closes', () => {
    const websocket = websocketFake();
    const callbacks = handlers({
      onOpen: jasmine.createSpy('open'),
      onMessage: jasmine.createSpy('message'),
      onClose: jasmine.createSpy('close'),
      onError: jasmine.createSpy('error'),
    });
    const connection = create(websocket.connect);
    connection.connect('/ws', callbacks);
    const socket = websocket.connections[0];
    socket.options.onClose();
    socket.options.onOpen();
    socket.options.onMessage('stale');
    socket.options.onError();
    socket.options.onClose();
    expect(connection.isActive()).toBeFalse();
    expect(connection.isOpen()).toBeFalse();
    expect(callbacks.onClose).toHaveBeenCalledOnceWith();
    expect(callbacks.onOpen).not.toHaveBeenCalled();
    expect(callbacks.onMessage).not.toHaveBeenCalled();
    expect(callbacks.onError).not.toHaveBeenCalled();
  });

  it('invalidates callbacks before closing the socket and isolates service instances', () => {
    const websocket = websocketFake();
    const closed = jasmine.createSpy('closed');
    const first = create(websocket.connect);
    const second = create(websocket.connect);
    first.connect('/first', handlers({ onClose: closed }));
    second.connect('/second', handlers());
    const socket = websocket.connections[0];
    socket.connection.close.and.callFake(() => socket.options.onClose());
    first.close();
    first.close();
    first.send('ignored');
    expect(first.isActive()).toBeFalse();
    expect(second.isActive()).toBeTrue();
    expect(socket.connection.close).toHaveBeenCalledOnceWith();
    expect(socket.connection.send).not.toHaveBeenCalled();
    expect(closed).not.toHaveBeenCalled();
    second.close();
  });

  it('reports active connection errors', () => {
    const websocket = websocketFake();
    const onError = jasmine.createSpy('onError');
    const connection = Reflect.construct(create, [websocket.connect]);

    connection.connect('/ws', handlers({ onError }));
    websocket.connections[0].options.onError();

    expect(onError).toHaveBeenCalledOnceWith();
  });
});

function websocketFake() {
  const connections = [];
  return {
    connections,
    connect(url, options) {
      const { protocols } = options;
      const connection = {
        send: jasmine.createSpy('send'),
        close: jasmine.createSpy('close'),
      };
      connections.push({ url, protocols, options, connection });
      return connection;
    },
  };
}

function handlers(overrides = {}) {
  return {
    onOpen() {},
    onMessage() {},
    onClose() {},
    onError() {},
    ...overrides,
  };
}
