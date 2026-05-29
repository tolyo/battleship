import { decodeServerMessage } from '../map/server-message.js';
import {
  battleRoomEvent,
  lobbyUrl,
  moveMessage,
  restoreUrl,
} from './game-protocol.js';

/**
 * @typedef {{
 *   mode: 'lobby' | 'restore',
 *   roomId?: string,
 *   playerId?: string
 * }} BattleRoomContext
 */

export class BattleRoomClientService {
  static $inject = ['session', 'roomSession', 'gameState', 'fleetLayout'];

  /**
   * @param {import('../session/session-service.js').SessionService} session
   * @param {import('../room/room-session-service.js').RoomSessionService} roomSession
   * @param {import('./game-state-service.js').GameStateService} gameState
   * @param {import('./fleet-layout-service.js').FleetLayoutService} fleetLayout
   */
  constructor(session, roomSession, gameState, fleetLayout) {
    this.session = session;
    this.roomSession = roomSession;
    this.gameState = gameState;
    this.fleetLayout = fleetLayout;
    /** @type {number | undefined} */
    this.restoreTimer = undefined;
  }

  joinLobby() {
    if (this.session.isActive() || !this.gameState.tryEnterWaiting()) {
      return;
    }

    this.connect({ mode: 'lobby' });
  }

  restoreRoomFromCurrentUrl() {
    const roomId = this.roomSession.currentRoomId();
    if (roomId) {
      this.restoreRoom(roomId);
    }
  }

  /**
   * @param {string} roomId
   * @param {{ timeoutMs?: number }} options
   * @returns {boolean}
   */
  restoreRoom(roomId, options = {}) {
    const playerId = this.roomSession.playerId(roomId);
    if (!playerId) {
      this.gameState.roomUnavailable();
      return false;
    }

    this.gameState.startRestore(roomId, playerId);
    this.connect({
      mode: 'restore',
      roomId,
      playerId,
    });

    this.restoreTimer = window.setTimeout(() => {
      this.restoreFailed(roomId, 'Room unavailable');
    }, options.timeoutMs ?? 5000);

    return true;
  }

  /**
   * @param {import('./game-state-service.js').BoardGridTile} tile
   */
  strike(tile) {
    if (!this.session.isOpen()) {
      return;
    }

    const move = this.gameState.moveForStrike(tile);
    if (!move) {
      return;
    }

    this.session.send(moveMessage(move));
  }

  close() {
    this.clearRestoreTimer();
    this.session.close();
  }

  /**
   * @param {BattleRoomContext} context
   */
  connect(context) {
    this.session.connect(this.urlForContext(context), {
      onOpen: () => {
        if (context.mode === 'lobby') {
          this.gameState.waitingForOpponent();
        }
      },
      onMessage: (data) => {
        this.clearRestoreTimer();
        this.handleEvent(battleRoomEvent(decodeServerMessage(data)), context);
      },
      onClose: () => this.handleClose(context),
      onError: () => this.handleConnectionError(context),
    });
  }

  /**
   * @param {BattleRoomContext} context
   * @returns {string}
   */
  urlForContext(context) {
    if (context.mode === 'lobby') {
      return lobbyUrl(this.gameState.player, this.gameState.boardState);
    }

    return restoreUrl(context.roomId, context.playerId);
  }

  /**
   * @param {import('./game-protocol.js').BattleRoomEvent} event
   * @param {BattleRoomContext} context
   */
  handleEvent(event, context) {
    if (event.type === 'waiting') {
      this.gameState.waitingForOpponent();
    } else if (event.type === 'room_entered') {
      this.enterRoom(event.message, event.updateUrl);
    } else if (event.type === 'game_received') {
      this.placeFleetShips(this.gameState.receiveBattleRoomGame(event.game));
    } else if (event.type === 'opponent_left') {
      this.gameState.opponentDisconnected();
    } else if (event.type === 'room_unavailable') {
      this.handleUnavailable(context);
    } else if (event.type === 'server_error') {
      this.gameState.serverError(event.reason);
    }
  }

  /**
   * @param {import('../map/server-message.js').ServerMessage} message
   * @param {boolean} updateUrl
   */
  enterRoom(message, updateUrl) {
    const entered = this.gameState.enterBattleRoom({
      roomId: message.room_id,
      playerId: message.player_id,
      opponentId: message.opponent_id,
      game: message.game,
    });
    if (!entered || !message.room_id || !message.player_id) {
      return;
    }

    this.roomSession.rememberPlayer(message.room_id, message.player_id);
    if (updateUrl) {
      this.roomSession.showRoom(message.room_id);
    }
    this.placeFleetShips(entered.shipCoordinatesById);
  }

  /**
   * @param {Record<string, import('../map/game-view-model.js').Coordinate[]> | undefined} shipCoordinatesById
   */
  placeFleetShips(shipCoordinatesById) {
    if (shipCoordinatesById) {
      this.fleetLayout.placeFleetShips(shipCoordinatesById);
    }
  }

  /**
   * @param {BattleRoomContext} context
   */
  handleClose(context) {
    if (
      context.mode === 'restore' &&
      context.roomId &&
      this.gameState.isRestoring()
    ) {
      this.restoreFailed(context.roomId, 'Room unavailable');
      return;
    }

    this.gameState.disconnectWaiting();
  }

  /**
   * @param {BattleRoomContext} context
   */
  handleConnectionError(context) {
    if (context.mode === 'restore') {
      this.handleUnavailable(context);
      return;
    }

    this.gameState.connectionError();
  }

  /**
   * @param {BattleRoomContext} context
   */
  handleUnavailable(context) {
    if (context.roomId) {
      this.restoreFailed(context.roomId, 'Room unavailable');
      return;
    }

    this.gameState.roomUnavailable();
  }

  /**
   * @param {string} roomId
   * @param {string} status
   */
  restoreFailed(roomId, status) {
    this.roomSession.forgetPlayer(roomId);
    this.roomSession.showHome();
    this.gameState.returnToSetup(status);
    this.close();
  }

  clearRestoreTimer() {
    if (this.restoreTimer !== undefined) {
      window.clearTimeout(this.restoreTimer);
      this.restoreTimer = undefined;
    }
  }
}
