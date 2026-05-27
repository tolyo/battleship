import { GRID, MapTile } from '../game/constants.js';

/**
 * @typedef {object} RenderGameStateOptions
 * @property {unknown} game
 * @property {string} playerId
 * @property {HTMLDivElement} fleetPlaceholder
 * @property {boolean} restoredFromUrl
 */

/**
 * @typedef {object} RenderGameStateResult
 * @property {boolean} isMyTurn
 * @property {string} status
 */

/**
 * @param {unknown} value
 * @returns {value is Record<string, unknown>}
 */
function isRecord(value) {
  return typeof value === 'object' && value !== null;
}

/**
 * @param {unknown} value
 * @returns {boolean}
 */
function isShipCell(value) {
  return typeof value === 'string' && /^[0-9]$/.test(value);
}

/**
 * @param {unknown} value
 * @returns {value is unknown[][]}
 */
function isBoard(value) {
  return Array.isArray(value) && value.every((row) => Array.isArray(row));
}

/**
 * @param {RenderGameStateOptions} options
 * @returns {RenderGameStateResult | undefined}
 */
export function renderGameState({
  game,
  playerId,
  fleetPlaceholder,
  restoredFromUrl,
}) {
  if (!isRecord(game)) {
    return undefined;
  }

  const turnState = applyTurnState(game, playerId);
  const player = currentPlayerFromGame(game, playerId);
  if (player && isBoard(player.board)) {
    applyFleetBoard(player.board);
    if (restoredFromUrl) {
      hideShipElements(fleetPlaceholder);
    }
  }

  applyTurns(game.turns, playerId);
  return turnState;
}

/**
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {RenderGameStateResult}
 */
function applyTurnState(game, playerId) {
  const currentTurnId = currentTurnIdFromGame(game);
  const isMyTurn = currentTurnId === playerId;
  const finished = game.phase === 'finished' || game.state === 'FINISHED';

  if (finished) {
    return { isMyTurn, status: 'Game finished' };
  }

  if (isMyTurn) {
    return { isMyTurn, status: 'Your turn' };
  }

  return { isMyTurn, status: "Awaiting opponent's move" };
}

/**
 * @param {Record<string, unknown>} game
 * @returns {string | undefined}
 */
function currentTurnIdFromGame(game) {
  if (game.phase === 'finished' || game.state === 'FINISHED') {
    return undefined;
  }

  if (typeof game.current_turn === 'string') {
    return game.current_turn;
  }

  if (!Array.isArray(game.turns) || game.turns.length === 0) {
    return typeof game.first_turn === 'string' ? game.first_turn : undefined;
  }

  const lastTurn = game.turns[0];
  if (!isRecord(lastTurn) || typeof lastTurn.id !== 'string') {
    return undefined;
  }

  const playerOne = game.player_one;
  const playerTwo = game.player_two;
  if (!isRecord(playerOne) || !isRecord(playerTwo)) {
    return undefined;
  }

  if (lastTurn.res === 'HIT') {
    return lastTurn.id;
  }

  if (lastTurn.id === playerOne.id && typeof playerTwo.id === 'string') {
    return playerTwo.id;
  }

  if (lastTurn.id === playerTwo.id && typeof playerOne.id === 'string') {
    return playerOne.id;
  }

  return undefined;
}

/**
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {{ id: string, board: unknown } | undefined}
 */
function currentPlayerFromGame(game, playerId) {
  const players = [game.player_one, game.player_two];
  const player = players.find(
    (candidate) =>
      isRecord(candidate) &&
      candidate.id === playerId &&
      typeof candidate.id === 'string'
  );

  if (!isRecord(player) || typeof player.id !== 'string') {
    return undefined;
  }

  return { id: player.id, board: player.board };
}

/**
 * @param {unknown[][]} board
 */
function applyFleetBoard(board) {
  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const tile = document.getElementById(`fleetboard-${row}-${column}`);
      if (!(tile instanceof HTMLElement)) {
        return;
      }

      tile.classList.remove('placed', 'hit', 'miss');
      const cell = board[row]?.[column];
      if (isShipCell(cell)) {
        tile.classList.add('placed');
      } else if (cell === MapTile.HIT) {
        tile.classList.add('hit');
      } else if (cell === MapTile.MISS || cell === 'x') {
        tile.classList.add('miss');
      }
    });
  });
}

/**
 * @param {unknown} turns
 * @param {string} playerId
 */
function applyTurns(turns, playerId) {
  if (!Array.isArray(turns)) {
    return;
  }

  turns.forEach((turn) => {
    if (!isRecord(turn)) {
      return;
    }

    const row = Number(turn.y) - 1;
    const column = Number(turn.x) - 1;
    if (!Number.isInteger(row) || !Number.isInteger(column)) {
      return;
    }

    const boardName = turn.id === playerId ? 'hitboard' : 'fleetboard';
    const tile = document.getElementById(`${boardName}-${row}-${column}`);
    if (!(tile instanceof HTMLElement)) {
      return;
    }

    tile.classList.remove('hit', 'miss');
    if (turn.res === 'HIT') {
      tile.classList.add('hit');
    } else if (turn.res === 'MISS') {
      tile.classList.add('miss');
    }
  });
}

/**
 * @param {HTMLDivElement} fleetPlaceholder
 */
function hideShipElements(fleetPlaceholder) {
  fleetPlaceholder.querySelectorAll('.ship').forEach((ship) => {
    if (ship instanceof HTMLElement) {
      ship.hidden = true;
    }
  });
}
