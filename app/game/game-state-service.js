import Fleet from './fleet.js';
import { emptyBoardState } from '../map/board-state.js';
import { gameViewModelFromState } from '../map/game-view-model.js';
import {
  allFleetShipsPlaced,
  boardStateFromPlacements,
  canPlaceSetupShip,
  randomFleetPlacements,
} from './fleet-placement.js';
import {
  emptyTileRows,
  rowsFromTiles,
  rowsWithSunkState,
  setupRowsFromDataState,
} from './board-rows.js';

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
    /** @type {Record<string, import('../map/game-view-model.js').Coordinate[]>} */
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
    /** @type {Set<(locked: boolean) => void>} */
    this.fleetLockListeners = new Set();
  }

  /**
   * @param {boolean} locked
   */
  setFleetLocked(locked) {
    if (this.fleetLocked === locked) {
      return;
    }

    this.fleetLocked = locked;
    this.fleetLockListeners.forEach((listener) => listener(locked));
  }

  /**
   * @param {(locked: boolean) => void} listener
   * @returns {() => void}
   */
  subscribeFleetLock(listener) {
    this.fleetLockListeners.add(listener);
    listener(this.fleetLocked);

    return () => {
      this.fleetLockListeners.delete(listener);
    };
  }

  resetSetupFleet() {
    if (this.phase !== 'setup') {
      return;
    }

    this.shipPlacements = {};
    this.rebuildSetupBoard();
  }

  randomizeFleet() {
    if (this.phase !== 'setup') {
      return;
    }

    this.shipPlacements = randomFleetPlacements();
    this.rebuildSetupBoard();
  }

  /**
   * @param {string} shipId
   * @param {import('../map/game-view-model.js').Coordinate[]} coordinates
   */
  placeSetupShip(shipId, coordinates) {
    if (this.phase !== 'setup') {
      return;
    }

    this.shipPlacements = {
      ...this.shipPlacements,
      [shipId]: coordinates,
    };
    this.rebuildSetupBoard();
  }

  /**
   * @param {string} shipId
   */
  clearSetupShip(shipId) {
    if (this.phase !== 'setup') {
      return;
    }

    const { [shipId]: _removed, ...nextPlacements } = this.shipPlacements;
    this.shipPlacements = nextPlacements;
    this.rebuildSetupBoard();
  }

  /**
   * @param {string} shipId
   * @param {import('../map/game-view-model.js').Coordinate[]} coordinates
   * @returns {boolean}
   */
  canPlaceSetupShip(shipId, coordinates) {
    if (this.phase !== 'setup') {
      return false;
    }

    return canPlaceSetupShip(this.shipPlacements, shipId, coordinates);
  }

  rebuildSetupBoard() {
    const { boardState, tileDataState } = boardStateFromPlacements(
      this.shipPlacements
    );

    this.boardState = boardState;
    this.fleetRows = setupRowsFromDataState(tileDataState);
    if (allFleetShipsPlaced(this.shipPlacements)) {
      this.boardReady = true;
      this.status = 'Ready to join';
    } else {
      this.boardReady = false;
      this.status = 'Place your fleet';
    }
  }

  enterWaiting() {
    this.phase = 'waiting';
    this.setFleetLocked(true);
    this.status = 'Connecting...';
  }

  waitingForOpponent() {
    this.status = 'Waiting for opponent...';
  }

  /**
   * @returns {boolean}
   */
  tryEnterWaiting() {
    if (this.phase !== 'setup') {
      return false;
    }

    if (!this.boardReady) {
      this.status = 'Place your fleet first';
      return false;
    }

    this.enterWaiting();
    return true;
  }

  disconnectWaiting() {
    this.status = 'Disconnected';
    if (this.phase === 'waiting') {
      this.phase = 'setup';
      this.setFleetLocked(false);
    }
  }

  /**
   * @param {string} status
   */
  returnToSetup(status) {
    this.phase = 'setup';
    this.roomId = undefined;
    this.playerId = undefined;
    this.opponentId = undefined;
    this.isMyTurn = false;
    this.canStrike = false;
    this.pendingGame = undefined;
    this.setFleetLocked(false);
    this.status = status;
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   * @param {string | undefined} opponentId
   */
  enterRoom(roomId, playerId, opponentId) {
    this.phase = 'playing';
    this.setFleetLocked(true);
    this.roomId = roomId;
    this.playerId = playerId;
    this.opponentId = opponentId;
    this.status = 'In room';
  }

  /**
   * @param {string} roomId
   * @param {string} playerId
   */
  startRestore(roomId, playerId) {
    this.phase = 'playing';
    this.setFleetLocked(true);
    this.roomId = roomId;
    this.playerId = playerId;
    this.status = 'Reconnecting...';
  }

  /**
   * @returns {boolean}
   */
  isRestoring() {
    return this.status === 'Reconnecting...';
  }

  /**
   * @param {{
   *   roomId: string | undefined,
   *   playerId: string | undefined,
   *   opponentId: string | undefined,
   *   game: unknown
   * }} room
   * @returns {{
   *   shipCoordinatesById: Record<string, import('../map/game-view-model.js').Coordinate[]> | undefined
   * } | undefined}
   */
  enterBattleRoom(room) {
    if (!room.roomId || !room.playerId) {
      this.roomUnavailable();
      return undefined;
    }

    this.enterRoom(room.roomId, room.playerId, room.opponentId);
    const shipCoordinatesById = this.receiveGameState(
      room.game ?? this.pendingGame
    );
    this.pendingGame = undefined;
    return { shipCoordinatesById };
  }

  /**
   * @param {unknown} game
   * @returns {Record<string, import('../map/game-view-model.js').Coordinate[]> | undefined}
   */
  receiveGameState(game) {
    if (!this.playerId) {
      this.pendingGame = game;
      return undefined;
    }

    const viewModel = gameViewModelFromState(game, this.playerId);
    if (!viewModel) {
      return undefined;
    }

    this.isMyTurn = viewModel.isMyTurn;
    this.canStrike = this.phase === 'playing' && viewModel.isMyTurn;
    this.status = viewModel.status;
    this.fleetRows = rowsWithSunkState(
      rowsFromTiles('fleetboard', viewModel.fleetTiles),
      viewModel.sunkClusters
    );
    this.hitRows = rowsFromTiles('hitboard', viewModel.hitTiles);
    return viewModel.shipCoordinatesById;
  }

  /**
   * @param {unknown} game
   * @returns {Record<string, import('../map/game-view-model.js').Coordinate[]> | undefined}
   */
  receiveBattleRoomGame(game) {
    return this.receiveGameState(game);
  }

  /**
   * @param {BoardGridTile} tile
   * @returns {{ row: number, column: number } | undefined}
   */
  moveForStrike(tile) {
    if (
      this.phase !== 'playing' ||
      !this.isMyTurn ||
      tile.state === 'hit' ||
      tile.state === 'miss'
    ) {
      return undefined;
    }

    return {
      row: tile.row,
      column: tile.column,
    };
  }

  roomUnavailable() {
    this.status = 'Room unavailable';
  }

  opponentDisconnected() {
    this.status = 'Opponent disconnected';
  }

  connectionError() {
    this.status = 'connection_error';
  }

  /**
   * @param {string} reason
   */
  serverError(reason) {
    this.status = reason;
  }

}

/**
 * @typedef {import('./board-rows.js').BoardGridTile} BoardGridTile
 */
