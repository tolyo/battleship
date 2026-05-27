import Fleet from '../game/fleet.js';
import { addTilesToBoard } from '../fleetboard.js';
import { FLEET_SIZE } from '../game/constants.js';
import {
  emptyBoardState,
  isClaimedShipTile,
  tileCoordinates,
} from './board-state.js';
import { renderGameState } from './game-state-renderer.js';
import {
  anonymousPlayerName,
  rememberRoomPlayer,
  replaceUrlWithRoom,
  roomIdFromPath,
  storedPlayerId,
} from './room-session.js';
import { decodeServerMessage } from './server-message.js';

class MapController {
  static $inject = ['$scope'];

  /**
   * @param {ng.RootScopeService} $scope
   */
  constructor($scope) {
    this.$scope = $scope;
    this.boardReady = false;
    /** @type {HTMLDivElement} */
    this.board = /** @type {HTMLDivElement} */ (
      document.getElementById('fleetboard')
    );
    /** @type {HTMLButtonElement} */
    this.button = /** @type {HTMLButtonElement} */ (
      document.getElementById('ready')
    );
    /** @type {HTMLButtonElement} */
    this.randomButton = /** @type {HTMLButtonElement} */ (
      document.getElementById('random')
    );
    /** @type {HTMLButtonElement} */
    this.resetButton = /** @type {HTMLButtonElement} */ (
      document.getElementById('reset')
    );
    /** @type {HTMLElement} */
    this.statusElement = /** @type {HTMLElement} */ (
      document.getElementById('match-status')
    );
    /** @type {HTMLElement} */
    this.setupActions = /** @type {HTMLElement} */ (
      document.getElementById('setup-actions')
    );
    /** @type {HTMLDivElement} */
    this.hitboard = /** @type {HTMLDivElement} */ (
      document.getElementById('hitboard')
    );
    addTilesToBoard(this.board, 'fleetboard');
    // Add placeholders
    /** @type {HTMLDivElement} */
    this.fleetPlaceholder = /** @type {HTMLDivElement} */ (
      document.getElementById('fleet')
    );
    Fleet.forEach((ship) => ship.createPlaceHolder(this.fleetPlaceholder));
    // Attach ships to them
    Fleet.forEach((ship) => ship.createOnPlaceholder());

    /** @type {string[][]} */
    this.boardState = emptyBoardState();
    /** @type {string} */
    this.player = anonymousPlayerName();
    /** @type {string} */
    this.playerLabel = 'Anonymous';
    /** @type {string} */
    this.status = 'Place your fleet';
    /** @type {WebSocket | undefined} */
    this.socket = undefined;
    /** @type {'setup' | 'waiting' | 'playing'} */
    this.phase = 'setup';
    /** @type {string | undefined} */
    this.roomId = undefined;
    /** @type {string | undefined} */
    this.playerId = undefined;
    /** @type {string | undefined} */
    this.opponentId = undefined;
    this.hitboardReady = false;
    this.restoredFromUrl = false;
    this.isMyTurn = false;
    /** @type {unknown} */
    this.pendingGame = undefined;

    const observer = new MutationObserver(() => this.handleChildChanges());
    // Start observing the parent node for childList mutations
    observer.observe(this.board, {
      attributes: true,
      childList: true,
      subtree: true,
    });
    window.addEventListener('resize', () => this.scheduleShipRealign());
    this.scheduleShipRealign();

    const roomId = roomIdFromPath();
    if (roomId) {
      this.restoreRoom(roomId);
      return;
    }

    this.syncSetupUi();
  }

  handleChildChanges() {
    if (this.phase !== 'setup') {
      return;
    }

    let placedCells = 0;
    let allShipsPlaced = true;
    const occupiedCells = new Set();
    const nextBoardState = emptyBoardState();

    Fleet.forEach((ship) => {
      const shipTiles = ship.elementsBelow.filter(isClaimedShipTile);
      if (shipTiles.length !== ship.size) {
        allShipsPlaced = false;
      }

      shipTiles.forEach((elem) => {
        const coordinates = tileCoordinates(elem);
        if (!coordinates) {
          allShipsPlaced = false;
          return;
        }
        const occupiedKey = `${coordinates.row}:${coordinates.column}`;
        if (occupiedCells.has(occupiedKey)) {
          allShipsPlaced = false;
          return;
        }

        occupiedCells.add(occupiedKey);
        nextBoardState[coordinates.row][coordinates.column] = ship.id;
        placedCells += 1;
      });
    });

    this.boardState = nextBoardState;
    if (allShipsPlaced && placedCells === FLEET_SIZE) {
      this.boardReady = true;
      this.status = 'Ready to join';
    } else {
      this.boardReady = false;
      this.status = 'Place your fleet';
    }
    this.syncSetupUi();
  }

  syncSetupUi() {
    const setupActive = this.phase === 'setup';
    this.button.disabled = !this.boardReady || !setupActive;
    this.randomButton.disabled = !setupActive;
    this.resetButton.disabled = !setupActive;
    this.statusElement.textContent = this.status;
  }

  realignShipsToLayout() {
    Fleet.forEach((ship) => {
      ship.realignToLayout();
    });
  }

  scheduleShipRealign() {
    const realign = () => this.realignShipsToLayout();

    requestAnimationFrame(() => {
      realign();
      requestAnimationFrame(realign);
    });

    document.fonts?.ready.then(realign).catch(() => {});
  }

  random() {
    if (this.phase !== 'setup') {
      return;
    }

    try {
      this.tryPlacingShips();
      this.handleChildChanges();
    } catch (e) {
      this.random();
    }
  }

  tryPlacingShips() {
    this.reset();
    Fleet.forEach((ship) => {
      let count = 100; // safety to prevent runaway cycle
      let res = ship.tryRandomLocation();
      while (res === false) {
        res = ship.tryRandomLocation();
        count -= 1;
        if (count === 0) {
          throw new Error('Count exceeded');
        }
      }
    });
  }

  reset() {
    if (this.phase !== 'setup') {
      return;
    }

    Fleet.forEach((ship) => ship.setOnPlaceholder());
    this.handleChildChanges();
  }

  join() {
    if (this.phase !== 'setup') {
      return;
    }

    if (!this.boardReady) {
      this.status = 'Place your fleet first';
      this.syncSetupUi();
      return;
    }

    if (
      this.socket &&
      (this.socket.readyState === WebSocket.CONNECTING ||
        this.socket.readyState === WebSocket.OPEN)
    ) {
      return;
    }

    // Send a JSON-encoded board snapshot for matchmaking.
    const boardParam = encodeURIComponent(JSON.stringify(this.boardState));
    const playerParam = encodeURIComponent(this.player);
    const socket = new WebSocket(
      `/ws?player=${playerParam}&board=${boardParam}`
    );
    this.socket = socket;
    this.phase = 'waiting';
    this.status = 'Connecting...';
    this.syncSetupUi();

    socket.addEventListener('open', () => {
      this.status = 'Waiting for opponent...';
      this.syncSetupUi();
    });

    socket.addEventListener('message', (ev) => {
      const message = decodeServerMessage(ev.data);
      if (message.type === 'lobby_waiting') {
        this.status = 'Waiting for opponent...';
      } else if (message.type === 'match_found') {
        this.enterRoom(message, true);
      } else if (message.type === 'room_joined') {
        this.enterRoom(message, false);
      } else if (message.type === 'game_update') {
        this.status = 'Game in progress';
        this.receiveGameState(message.game);
      } else if (message.type === 'game_state') {
        this.status = 'In room';
        this.receiveGameState(message.game);
      } else if (message.type === 'error') {
        this.status = message.reason || 'Server error';
      }
      this.syncSetupUi();
    });

    socket.addEventListener('close', () => {
      this.status = 'Disconnected';
      this.socket = undefined;
      if (this.phase === 'waiting') {
        this.phase = 'setup';
      }
      this.syncSetupUi();
    });
  }

  /**
   * @param {import('./server-message.js').ServerMessage} message
   * @param {boolean} updateUrl
   */
  enterRoom(message, updateUrl) {
    if (!message.room_id || !message.player_id) {
      this.status = 'Room unavailable';
      this.syncSetupUi();
      return;
    }

    this.phase = 'playing';
    this.roomId = message.room_id;
    this.playerId = message.player_id;
    this.opponentId = message.opponent_id;
    rememberRoomPlayer(message.room_id, message.player_id);
    if (updateUrl) {
      replaceUrlWithRoom(message.room_id);
    }
    this.status = 'In room';
    this.showRoomUi();
    this.receiveGameState(message.game ?? this.pendingGame);
    this.pendingGame = undefined;
  }

  /**
   * @param {string} roomId
   */
  restoreRoom(roomId) {
    const playerId = storedPlayerId(roomId);
    if (!playerId) {
      this.status = 'Room unavailable';
      this.syncSetupUi();
      return;
    }

    this.phase = 'playing';
    this.roomId = roomId;
    this.playerId = playerId;
    this.restoredFromUrl = true;
    this.status = 'Reconnecting...';
    this.showRoomUi();

    const socket = new WebSocket(
      `/ws?room_id=${encodeURIComponent(roomId)}&player_id=${encodeURIComponent(playerId)}`
    );
    this.socket = socket;
    socket.addEventListener('message', (ev) => {
      const message = decodeServerMessage(ev.data);
      if (message.type === 'room_joined') {
        this.enterRoom(message, false);
      } else if (message.type === 'game_state') {
        this.status = 'In room';
        this.receiveGameState(message.game);
      } else if (message.type === 'game_update') {
        this.status = 'Game in progress';
        this.receiveGameState(message.game);
      } else if (message.type === 'error') {
        this.status = message.reason || 'Server error';
      }
      this.syncSetupUi();
    });
    socket.addEventListener('close', () => {
      this.status = 'Disconnected';
      this.socket = undefined;
      this.syncSetupUi();
    });
  }

  showRoomUi() {
    this.setupActions.hidden = true;
    this.hitboard.hidden = false;
    this.hitboard.classList.add('disabled');
    this.hitboard.setAttribute('aria-disabled', 'true');
    this.board.classList.add('disabled');
    this.fleetPlaceholder.classList.add('room-active');
    this.ensureHitboard();
    this.scheduleShipRealign();
    this.syncSetupUi();
  }

  /**
   * @param {unknown} game
   */
  receiveGameState(game) {
    if (!this.playerId) {
      this.pendingGame = game;
      return;
    }

    const result = renderGameState({
      game,
      playerId: this.playerId,
      hitboard: this.hitboard,
      fleetPlaceholder: this.fleetPlaceholder,
      restoredFromUrl: this.restoredFromUrl,
    });

    if (result) {
      this.isMyTurn = result.isMyTurn;
      this.status = result.status;
    }
  }

  ensureHitboard() {
    if (this.hitboardReady) {
      return;
    }

    addTilesToBoard(this.hitboard, 'hitboard');
    this.hitboard.addEventListener('click', (event) =>
      this.handleHitboardClick(event)
    );
    this.hitboardReady = true;
  }

  /**
   * @param {MouseEvent} event
   */
  handleHitboardClick(event) {
    if (
      !this.socket ||
      this.socket.readyState !== WebSocket.OPEN ||
      this.phase !== 'playing' ||
      !this.isMyTurn
    ) {
      return;
    }

    if (!(event.target instanceof Element)) {
      return;
    }

    const target = event.target.closest('.hitboard-tile');
    if (!(target instanceof HTMLElement)) {
      return;
    }

    if (target.classList.contains('hit') || target.classList.contains('miss')) {
      return;
    }

    const coordinates = tileCoordinates(target);
    if (!coordinates) {
      return;
    }

    this.socket.send(
      JSON.stringify({
        type: 'move',
        row: coordinates.row,
        column: coordinates.column,
      })
    );
  }
}

export default {
  templateUrl: '/static/map/map.html',
  controller: MapController,
};
