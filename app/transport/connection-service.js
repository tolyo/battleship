/**
 * @typedef {object} SessionHandlers
 * @property {() => void=} onOpen
 * @property {(data: unknown) => void=} onMessage
 * @property {() => void=} onClose
 * @property {() => void=} onError
 */

export class ConnectionService {
  static $inject = ['$websocket'];

  /**
   * @param {ng.WebSocketService} $websocket
   */
  constructor($websocket) {
    this.$websocket = $websocket;
    /** @type {ng.WebSocketConnection | undefined} */
    this.connection = undefined;
    /** @type {'closed' | 'connecting' | 'open'} */
    this.state = 'closed';
  }

  /**
   * @param {string} url
   * @param {SessionHandlers} handlers
   */
  connect(url, handlers = {}) {
    this.close();
    this.state = 'connecting';

    const connection = this.$websocket(url, [], {
      heartbeatTimeout: 0,
      maxRetries: 0,
      onOpen: () => {
        if (this.connection !== connection) {
          return;
        }

        this.state = 'open';
        handlers.onOpen?.();
      },
      onMessage: (data) => {
        if (this.connection !== connection) {
          return;
        }

        handlers.onMessage?.(data);
      },
      onClose: () => {
        if (this.connection !== connection) {
          return;
        }

        this.connection = undefined;
        this.state = 'closed';
        handlers.onClose?.();
      },
      onError: () => {
        if (this.connection !== connection) {
          return;
        }

        handlers.onError?.();
      },
    });
    this.connection = connection;
  }

  /**
   * @param {unknown} message
   */
  send(message) {
    this.connection?.send(message);
  }

  close() {
    const { connection } = this;
    this.connection = undefined;
    this.state = 'closed';
    connection?.close();
  }

  /**
   * @returns {boolean}
   */
  isActive() {
    return this.state !== 'closed';
  }

  /**
   * @returns {boolean}
   */
  isOpen() {
    return this.state === 'open';
  }
}
