import { GRID, MapTile } from './constants.js';
import {
  fleetTileViews,
  hitTileViews,
  shipCoordinatesById,
  sunkClusters,
} from './board-view-model.js';

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
