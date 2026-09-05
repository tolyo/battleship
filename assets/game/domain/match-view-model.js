import { GRID, CellState } from './constants.js';
import {
  ownTileViews,
  targetTileViews,
  unitCoordinatesById,
  sunkClusters,
} from './tile-view-model.js';

/**
 * @typedef {'empty' | 'unit' | 'hit' | 'miss'} TileState
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
 * @property {string=} unitId
 */

/**
 * @typedef {object} SunkClusterView
 * @property {Coordinate[]} coordinates
 * @property {'horizontal' | 'vertical'} orientation
 */

/**
 * @typedef {object} MatchViewModel
 * @property {boolean} isMyTurn
 * @property {string} status
 * @property {BoardTileView[]} ownTiles
 * @property {BoardTileView[]} targetTiles
 * @property {Record<string, Coordinate[]>} unitCoordinatesById
 * @property {SunkClusterView[]} sunkClusters
 */

/**
 * @param {unknown} view
 * @returns {MatchViewModel | undefined}
 */
export function matchViewModelFromView(view) {
  if (!isRecord(view)) {
    return undefined;
  }

  const currentPlayer = ownPlayerFromView(view);
  const opponent = opponentFromView(view);
  const ownBoard = isBoard(currentPlayer?.board)
    ? currentPlayer.board
    : emptyBoard();
  const targetBoard = isBoard(opponent?.board) ? opponent.board : emptyBoard();
  const turnState = turnStateFromView(view);

  return {
    ...turnState,
    ownTiles: ownTileViews(ownBoard),
    targetTiles: targetTileViews(targetBoard),
    unitCoordinatesById: unitCoordinatesById(ownBoard),
    sunkClusters: sunkClusters(ownBoard),
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
  return GRID.map(() => GRID.map(() => CellState.EMPTY));
}

/**
 * @param {Record<string, unknown>} view
 * @returns {{ id: string, board: unknown } | undefined}
 */
function ownPlayerFromView(view) {
  return playerFromNamedView(view.own_player);
}

/**
 * @param {Record<string, unknown>} view
 * @returns {{ id: string, board: unknown } | undefined}
 */
function opponentFromView(view) {
  return playerFromNamedView(view.opponent);
}

/**
 * @param {unknown} player
 * @returns {{ id: string, board: unknown } | undefined}
 */
function playerFromNamedView(player) {
  if (!isRecord(player) || typeof player.id !== 'string') {
    return undefined;
  }

  return { id: player.id, board: player.board };
}

/**
 * @param {Record<string, unknown>} view
 * @returns {{ isMyTurn: boolean, status: string }}
 */
function turnStateFromView(view) {
  const allowedMove = allowedMoveFromView(view);
  const finished = view.phase === 'finished' || view.state === 'FINISHED';
  const isMyTurn = allowedMove && !finished;

  if (finished) {
    return { isMyTurn, status: 'Game finished' };
  }

  if (isMyTurn) {
    return { isMyTurn, status: 'Your turn' };
  }

  return { isMyTurn, status: "Awaiting opponent's move" };
}

/**
 * @param {Record<string, unknown>} view
 * @returns {boolean}
 */
function allowedMoveFromView(view) {
  if (!Array.isArray(view.allowed_actions)) {
    return false;
  }

  return view.allowed_actions.some(
    (action) => isRecord(action) && action.action === 'move'
  );
}
