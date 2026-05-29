import { GRID, MapTile } from '../game/constants.js';

/**
 * @typedef {'empty' | 'ship' | 'hit' | 'miss'} TileState
 */

/**
 * @typedef {object} Coordinate
 * @property {number} row
 * @property {number} column
 */

/**
 * @typedef {object} BoardTileView
 * @property {number} row
 * @property {number} column
 * @property {TileState} state
 * @property {string=} shipId
 */

/**
 * @typedef {object} SunkClusterView
 * @property {Coordinate[]} coordinates
 * @property {'horizontal' | 'vertical'} orientation
 */

/**
 * @typedef {object} GameViewModel
 * @property {boolean} isMyTurn
 * @property {string} status
 * @property {BoardTileView[]} fleetTiles
 * @property {BoardTileView[]} hitTiles
 * @property {Record<string, Coordinate[]>} shipCoordinatesById
 * @property {SunkClusterView[]} sunkClusters
 */

/**
 * @param {unknown} game
 * @param {string} playerId
 * @returns {GameViewModel | undefined}
 */
export function gameViewModelFromState(game, playerId) {
  if (!isRecord(game)) {
    return undefined;
  }

  const currentPlayer = playerFromGame(game, playerId);
  const opponent = opponentFromGame(game, playerId);
  const playerBoard = isBoard(currentPlayer?.board)
    ? currentPlayer.board
    : emptyBoard();
  const opponentBoard = isBoard(opponent?.board)
    ? opponent.board
    : emptyBoard();
  const turnState = turnStateFromGame(game, playerId);

  return {
    ...turnState,
    fleetTiles: fleetTileViews(playerBoard),
    hitTiles: hitTileViews(opponentBoard),
    shipCoordinatesById: shipCoordinatesById(playerBoard),
    sunkClusters: sunkClusters(playerBoard),
  };
}

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
 * @returns {unknown[][]}
 */
function emptyBoard() {
  return GRID.map(() => GRID.map(() => MapTile.EMPTY));
}

/**
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {{ id: string, board: unknown } | undefined}
 */
function playerFromGame(game, playerId) {
  return playerMatching(game, (id) => id === playerId);
}

/**
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {{ id: string, board: unknown } | undefined}
 */
function opponentFromGame(game, playerId) {
  return playerMatching(game, (id) => id !== playerId);
}

/**
 * @param {Record<string, unknown>} game
 * @param {(id: string) => boolean} predicate
 * @returns {{ id: string, board: unknown } | undefined}
 */
function playerMatching(game, predicate) {
  const players = [game.player_one, game.player_two];
  const player = players.find(
    (candidate) =>
      isRecord(candidate) &&
      typeof candidate.id === 'string' &&
      predicate(candidate.id)
  );

  if (!isRecord(player) || typeof player.id !== 'string') {
    return undefined;
  }

  return { id: player.id, board: player.board };
}

/**
 * @param {Record<string, unknown>} game
 * @param {string} playerId
 * @returns {{ isMyTurn: boolean, status: string }}
 */
function turnStateFromGame(game, playerId) {
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
 * @param {unknown[][]} board
 * @returns {BoardTileView[]}
 */
function fleetTileViews(board) {
  return tileViews(board, true);
}

/**
 * @param {unknown[][]} board
 * @returns {BoardTileView[]}
 */
function hitTileViews(board) {
  return tileViews(board, false);
}

/**
 * @param {unknown[][]} board
 * @param {boolean} revealShips
 * @returns {BoardTileView[]}
 */
function tileViews(board, revealShips) {
  return GRID.flatMap((row) =>
    GRID.map((column) => {
      const cell = board[row]?.[column];
      if (isShipCell(cell) && revealShips) {
        return {
          row,
          column,
          state: /** @type {const} */ ('ship'),
          shipId: cell,
        };
      }
      if (cell === MapTile.HIT || cell === MapTile.BLOCKED) {
        return { row, column, state: /** @type {const} */ ('hit') };
      }
      if (cell === MapTile.MISS || cell === 'x') {
        return { row, column, state: /** @type {const} */ ('miss') };
      }
      return { row, column, state: /** @type {const} */ ('empty') };
    })
  );
}

/**
 * @param {unknown[][]} board
 * @returns {Record<string, Coordinate[]>}
 */
function shipCoordinatesById(board) {
  const coordinatesByShipId = /** @type {Record<string, Coordinate[]>} */ ({});

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      const cell = board[row]?.[column];
      if (!isShipCell(cell)) {
        return;
      }

      coordinatesByShipId[cell] ??= [];
      coordinatesByShipId[cell].push({ row, column });
    });
  });

  return coordinatesByShipId;
}

/**
 * @param {unknown[][]} board
 * @returns {SunkClusterView[]}
 */
function sunkClusters(board) {
  /** @type {Set<string>} */
  const visited = new Set();
  /** @type {SunkClusterView[]} */
  const clusters = [];

  GRID.forEach((row) => {
    GRID.forEach((column) => {
      if (
        visited.has(tileKey(row, column)) ||
        board[row]?.[column] !== MapTile.HIT
      ) {
        return;
      }

      const coordinates = collectHitCluster(board, row, column, visited);
      if (!hasAdjacentShipCell(board, coordinates)) {
        clusters.push({
          coordinates,
          orientation: clusterOrientation(coordinates),
        });
      }
    });
  });

  return clusters;
}

/**
 * @param {unknown[][]} board
 * @param {number} startRow
 * @param {number} startColumn
 * @param {Set<string>} visited
 * @returns {Coordinate[]}
 */
function collectHitCluster(board, startRow, startColumn, visited) {
  /** @type {Coordinate[]} */
  const cluster = [];
  /** @type {Coordinate[]} */
  const pending = [{ row: startRow, column: startColumn }];

  while (pending.length > 0) {
    const coordinate = pending.pop();
    if (
      coordinate &&
      !visited.has(tileKey(coordinate.row, coordinate.column)) &&
      board[coordinate.row]?.[coordinate.column] === MapTile.HIT
    ) {
      visited.add(tileKey(coordinate.row, coordinate.column));
      cluster.push(coordinate);
      adjacentCoordinates(coordinate.row, coordinate.column).forEach(
        (adjacent) => {
          if (board[adjacent.row]?.[adjacent.column] === MapTile.HIT) {
            pending.push(adjacent);
          }
        }
      );
    }
  }

  return cluster;
}

/**
 * @param {unknown[][]} board
 * @param {Coordinate[]} cluster
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
 * @param {Coordinate[]} cluster
 * @returns {'horizontal' | 'vertical'}
 */
function clusterOrientation(cluster) {
  return cluster.length > 1 &&
    cluster.every((coordinate) => coordinate.row === cluster[0].row)
    ? 'horizontal'
    : 'vertical';
}

/**
 * @param {number} row
 * @param {number} column
 * @returns {Coordinate[]}
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
