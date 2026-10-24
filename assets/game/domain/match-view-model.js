import { project } from '../../../_build/browser/js/modules/battleship/browser/MatchView.js';
import { checkedBoard, tileViewsFromModel } from './tile-view-model.js';

/**
 * @typedef {'empty' | 'unit' | 'hit' | 'miss'} TileState
 */

/** @typedef {import('../../../_build/browser/js/modules/battleship/browser/UnitGeometry.js').Coordinate} Coordinate */

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
  const model = project(
    playerBoard(view.own_player),
    playerBoard(view.opponent),
    stringField(view.phase),
    stringField(view.state),
    Array.isArray(view.allowed_actions)
      ? Array.from(view.allowed_actions, (action) =>
          stringField(isRecord(action) ? action.action : undefined)
        )
      : []
  );
  return {
    isMyTurn: model.isMyTurn,
    status: model.status,
    ownTiles: tileViewsFromModel(model.ownTiles),
    targetTiles: tileViewsFromModel(model.targetTiles),
    unitCoordinatesById: Object.fromEntries(
      model.unitCoordinates.map(({ id, coordinates }) => [id, coordinates])
    ),
    sunkClusters: /** @type {SunkClusterView[]} */ (model.sunkClusters),
  };
}

/**
 * @param {unknown} value
 * @returns {value is Record<string, unknown>}
 */
function isRecord(value) {
  return typeof value === 'object' && value !== null;
}

/** @param {unknown} value */
function stringField(value) {
  return typeof value === 'string' ? value : '';
}

/**
 * @param {unknown} player
 * @returns {string[][]}
 */
function playerBoard(player) {
  if (
    !isRecord(player) ||
    typeof player.id !== 'string' ||
    !Array.isArray(player.board) ||
    !player.board.every(Array.isArray)
  ) {
    return [];
  }
  return checkedBoard(player.board);
}
