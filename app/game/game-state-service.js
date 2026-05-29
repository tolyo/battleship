import Fleet from './fleet.js';
import { emptyBoardState } from './board-state.js';
import {
  canPlaceSetupShip as canPlaceSetupShipState,
  clearSetupShip as clearSetupShipState,
  placeSetupShip as placeSetupShipState,
  randomizeFleet as randomizeFleetState,
  rebuildSetupBoard as rebuildSetupBoardState,
  resetSetupFleet as resetSetupFleetState,
} from './fleet-setup-state.js';
import { emptyTileRows } from './board-rows.js';
import {
  connectionError,
  disconnectWaiting,
  enterBattleRoom,
  enterWaiting as enterWaitingState,
  enterRoom,
  isRestoring,
  moveForStrike,
  opponentDisconnected,
  receiveGameState,
  returnToSetup,
  roomUnavailable,
  serverError,
  startRestore,
  tryEnterWaiting,
  waitingForOpponent,
} from './battle-room-state.js';

export class GameStateService {
  static $inject = ['roomSession'];

  /**
   * @param {import('../room/room-session-service.js').RoomSessionService} roomSession
   */
  constructor(roomSession) {
    this.fleet = Fleet;
    /** @type {boolean} */
    this.boardReady = false;
    /** @type {string[][]} */
    this.boardState = emptyBoardState();
    /** @type {Record<string, import('./game-view-model.js').Coordinate[]>} */
    this.shipPlacements = {};
    /** @type {string} */
    this.player = roomSession.anonymousPlayerName();
    /** @type {string} */
    this.playerLabel = 'Anonymous';
    /** @type {string} */
    this.status = 'Place your fleet';
    /** @type {'setup' | 'waiting' | 'playing'} */
    this.phase = 'setup';
    /** @type {string | undefined} */
    this.roomId = undefined;
    /** @type {string | undefined} */
    this.playerId = undefined;
    /** @type {string | undefined} */
    this.opponentId = undefined;
    this.isMyTurn = false;
    this.canStrike = false;
    /** @type {unknown} */
    this.pendingGame = undefined;
    this.fleetRows = emptyTileRows('fleetboard');
    this.hitRows = emptyTileRows('hitboard');
    this.fleetLocked = false;
  }

  /**
   * @param {boolean} locked
   */
  setFleetLocked(locked) {
    this.fleetLocked = locked;
  }

  resetSetupFleet() {
    resetSetupFleetState(this);
  }

  randomizeFleet() {
    randomizeFleetState(this);
  }

  /**
   * @param {string} shipId
   * @param {import('./game-view-model.js').Coordinate[]} coordinates
   */
  placeSetupShip(shipId, coordinates) {
    placeSetupShipState(this, shipId, coordinates);
  }

  /**
   * @param {string} shipId
   */
  clearSetupShip(shipId) {
    clearSetupShipState(this, shipId);
  }

  /**
   * @param {string} shipId
   * @param {import('./game-view-model.js').Coordinate[]} coordinates
   * @returns {boolean}
   */
  canPlaceSetupShip(shipId, coordinates) {
    return canPlaceSetupShipState(this, shipId, coordinates);
  }

  rebuildSetupBoard() {
    rebuildSetupBoardState(this);
  }

  enterWaiting() {
    enterWaitingState(this);
  }

  waitingForOpponent() {
    waitingForOpponent(this);
  }

  /**
   * @returns {boolean}
   */
  tryEnterWaiting() {
    return tryEnterWaiting(this);
  }

  disconnectWaiting() {
    disconnectWaiting(this);
  }

  /**
   * @param {string} status
   */
  returnToSetup(status) {
    returnToSetup(this, status);
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   * @param {string | undefined} opponentId
   */
  enterRoom(roomId, playerId, opponentId) {
    enterRoom(this, roomId, playerId, opponentId);
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   */
  startRestore(roomId, playerId) {
    startRestore(this, roomId, playerId);
  }

  /**
   * @returns {boolean}
   */
  isRestoring() {
    return isRestoring(this);
  }

  /**
   * @param {{
   *   roomId: string | undefined,
   *   playerId: string | undefined,
   *   opponentId: string | undefined,
   *   game: unknown
   * }} room
   * @returns {{
   *   shipCoordinatesById: Record<string, import('./game-view-model.js').Coordinate[]> | undefined
   * } | undefined}
   */
  enterBattleRoom(room) {
    return enterBattleRoom(this, room);
  }

  /**
   * @param {unknown} game
   * @returns {Record<string, import('./game-view-model.js').Coordinate[]> | undefined}
   */
  receiveGameState(game) {
    return receiveGameState(this, game);
  }

  /**
   * @param {BoardGridTile} tile
   * @returns {{ row: number, column: number } | undefined}
   */
  moveForStrike(tile) {
    return moveForStrike(this, tile);
  }

  roomUnavailable() {
    roomUnavailable(this);
  }

  opponentDisconnected() {
    opponentDisconnected(this);
  }

  connectionError() {
    connectionError(this);
  }

  /**
   * @param {string} reason
   */
  serverError(reason) {
    serverError(this, reason);
  }
}

/**
 * @typedef {import('./board-rows.js').BoardGridTile} BoardGridTile
 */
