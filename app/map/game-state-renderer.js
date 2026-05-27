import { GRID, MapTile } from '../game/constants.js';

/**
 * @typedef {object} RenderGameStateOptions
 * @property {unknown} game
 * @property {string} playerId
 * @property {import('../game/fleet-ship.js').FleetShipController[]} fleetShips
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
 * @returns {value is string}
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
  fleetShips,
}) {
  if (!isRecord(game)) {
    return undefined;
  }

  const turnState = applyTurnState(game, playerId);
  const player = currentPlayerFromGame(game, playerId);
  if (player && isBoard(player.board)) {
    applyFleetBoard(player.board, fleetShips);
  }

  const opponent = opponentPlayerFromGame(game, playerId);
  if (opponent && isBoard(opponent.board)) {
    applyHitBoard(opponent.board);
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
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {{ id: string, board: unknown } | undefined}
 */
function opponentPlayerFromGame(game, playerId) {
  const players = [game.player_one, game.player_two];
  const player = players.find(
    (candidate) =>
      isRecord(candidate) &&
      candidate.id !== playerId &&
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
function applyHitBoard(board) {
  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const tile = document.getElementById(`hitboard-${row}-${column}`);
      if (!(tile instanceof HTMLElement)) {
        return;
      }

      tile.classList.remove('hit', 'miss');
      const cell = board[row]?.[column];
      if (cell === MapTile.HIT || cell === MapTile.BLOCKED) {
        tile.classList.add('hit');
      } else if (cell === MapTile.MISS || cell === 'x') {
        tile.classList.add('miss');
      }
    });
  });
}

/**
 * @param {unknown[][]} board
 * @param {import('../game/fleet-ship.js').FleetShipController[]} fleetShips
 */
function applyFleetBoard(board, fleetShips) {
  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const tile = document.getElementById(`fleetboard-${row}-${column}`);
      if (!(tile instanceof HTMLElement)) {
        return;
      }

      tile.classList.remove(
        'placed',
        'hit',
        'miss',
        'sunk',
        'sunk-horizontal',
        'sunk-vertical',
        'sunk-single',
        'sunk-start',
        'sunk-end'
      );
      const cell = board[row]?.[column];
      if (isShipCell(cell)) {
        tile.classList.add('placed');
      } else if (cell === MapTile.HIT || cell === MapTile.BLOCKED) {
        tile.classList.add('hit');
      } else if (cell === MapTile.MISS || cell === 'x') {
        tile.classList.add('miss');
      }
    });
  });

  placeFleetShips(board, fleetShips);
  markSunkShips(board);
}

/**
 * @param {unknown[][]} board
 * @param {import('../game/fleet-ship.js').FleetShipController[]} fleetShips
 */
function placeFleetShips(board, fleetShips) {
  const coordinatesByShipId = fleetShips.reduce(
    (coordinates, fleetShip) => {
      if (fleetShip.ship) {
        coordinates[fleetShip.ship.id] = [];
      }
      return coordinates;
    },
    /** @type {Record<string, { row: number, column: number }[]>} */ ({})
  );

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const cell = board[row]?.[column];
      if (isShipCell(cell) && coordinatesByShipId[cell]) {
        coordinatesByShipId[cell].push({ row, column });
      }
    });
  });

  fleetShips.forEach((fleetShip) => {
    if (!fleetShip.ship) {
      return;
    }

    const liveCoordinates = coordinatesByShipId[fleetShip.ship.id];
    const coordinates =
      liveCoordinates.length > 0
        ? liveCoordinates
        : fleetShip.elementsBelow.map((tile) => ({
            row: Number(tile.dataset.row),
            column: Number(tile.dataset.column),
          }));
    fleetShip.placeOnBoardCoordinates(
      coordinates.filter(
        (coordinate) =>
          Number.isInteger(coordinate.row) &&
          Number.isInteger(coordinate.column)
      )
    );
  });
}

/**
 * @param {unknown[][]} board
 */
function markSunkShips(board) {
  /** @type {Set<string>} */
  const visited = new Set();

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      if (
        visited.has(tileKey(row, column)) ||
        board[row]?.[column] !== MapTile.HIT
      ) {
        return;
      }

      const cluster = collectHitCluster(board, row, column, visited);
      if (hasAdjacentShipCell(board, cluster)) {
        return;
      }

      markSunkCluster(cluster);
    });
  });
}

/**
 * @param {unknown[][]} board
 * @param {number} startRow
 * @param {number} startColumn
 * @param {Set<string>} visited
 * @returns {{ row: number, column: number }[]}
 */
function collectHitCluster(board, startRow, startColumn, visited) {
  const cluster = [];
  const pending = [{ row: startRow, column: startColumn }];

  while (pending.length > 0) {
    const coordinate = pending.pop();
    if (!coordinate) {
      continue;
    }

    const key = tileKey(coordinate.row, coordinate.column);
    if (
      visited.has(key) ||
      board[coordinate.row]?.[coordinate.column] !== MapTile.HIT
    ) {
      continue;
    }

    visited.add(key);
    cluster.push(coordinate);
    adjacentCoordinates(coordinate.row, coordinate.column).forEach(
      (adjacent) => {
        if (board[adjacent.row]?.[adjacent.column] === MapTile.HIT) {
          pending.push(adjacent);
        }
      }
    );
  }

  return cluster;
}

/**
 * @param {unknown[][]} board
 * @param {{ row: number, column: number }[]} cluster
 * @returns {boolean}
 */
function hasAdjacentShipCell(board, cluster) {
  return cluster.some((coordinate) =>
    adjacentCoordinates(coordinate.row, coordinate.column).some((adjacent) =>
      isShipCell(board[adjacent.row]?.[adjacent.column])
    )
  );
}

/**
 * @param {{ row: number, column: number }[]} cluster
 */
function markSunkCluster(cluster) {
  const orientation =
    cluster.length > 1 &&
    cluster.every((coordinate) => coordinate.row === cluster[0].row)
      ? 'horizontal'
      : 'vertical';
  const sorted = [...cluster].sort((left, right) =>
    orientation === 'horizontal'
      ? left.column - right.column
      : left.row - right.row
  );

  sorted.forEach((coordinate, index) => {
    const tile = document.getElementById(
      `fleetboard-${coordinate.row}-${coordinate.column}`
    );
    if (!(tile instanceof HTMLElement)) {
      return;
    }

    tile.classList.add('sunk');
    if (sorted.length === 1) {
      tile.classList.add('sunk-single');
      return;
    }

    tile.classList.add(`sunk-${orientation}`);
    if (index === 0) {
      tile.classList.add('sunk-start');
    }
    if (index === sorted.length - 1) {
      tile.classList.add('sunk-end');
    }
  });
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {{ row: number, column: number }[]}
 */
function adjacentCoordinates(row, column) {
  return [
    { row: row - 1, column },
    { row: row + 1, column },
    { row, column: column - 1 },
    { row, column: column + 1 },
  ].filter(
    (coordinate) =>
      coordinate.row >= 0 &&
      coordinate.row <= 9 &&
      coordinate.column >= 0 &&
      coordinate.column <= 9
  );
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {string}
 */
function tileKey(row, column) {
  return `${row}:${column}`;
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
