import {
  tile_views as tileViewsModel,
  unit_coordinates as unitCoordinatesModel,
  sunk_clusters as sunkClustersModel,
} from '../../../_build/browser/js/modules/battleship/browser/BoardView.js';

/** @param {unknown[][]} board */
export function checkedBoard(board) {
  return Array.from(board, (row) =>
    Array.from(row ?? [], (value) => (typeof value === 'string' ? value : ''))
  );
}

/** @param {unknown[][]} board */
export function ownTileViews(board) {
  return tileViews(board, true);
}

/** @param {unknown[][]} board */
export function targetTileViews(board) {
  return tileViews(board, false);
}

/** @param {unknown[][]} board */
export function unitCoordinatesById(board) {
  return Object.fromEntries(
    unitCoordinatesModel(checkedBoard(board)).map(({ id, coordinates }) => [
      id,
      coordinates,
    ])
  );
}

/** @param {unknown[][]} board */
export function sunkClusters(board) {
  return /** @type {import('./match-view-model.js').SunkClusterView[]} */ (
    sunkClustersModel(checkedBoard(board))
  );
}

/**
 * @param {unknown[][]} board
 * @param {boolean} revealUnits
 * @returns {import('./match-view-model.js').BoardTileView[]}
 */
function tileViews(board, revealUnits) {
  return tileViewsFromModel(tileViewsModel(checkedBoard(board), revealUnits));
}

/**
 * @param {import('../../../_build/browser/js/modules/battleship/browser/BoardView.js').Tile[]} tiles
 * @returns {import('./match-view-model.js').BoardTileView[]}
 */
export function tileViewsFromModel(tiles) {
  return tiles.map(
    ({ unit_id: unitId, ...tile }) =>
      /** @type {import('./match-view-model.js').BoardTileView} */ (
        unitId ? { ...tile, unitId } : tile
      )
  );
}
