import {
  empty_rows as emptyRows,
  rows_from_tiles as viewRows,
  setup_rows as setupRows,
  sunk_updates as sunkUpdates,
} from '../../../_build/browser/js/modules/battleship/browser/BoardRows.js';

/**
 * @typedef {import('../../../_build/browser/js/modules/battleship/browser/BoardRows.js').GridTile} ModelTile
 * @typedef {import('../../../_build/browser/js/modules/battleship/browser/BoardRows.js').GridRow} ModelRow
 * @typedef {Omit<ModelTile, 'classes' | 'unitId' | 'state'> &
 *   import('./match-view-model.js').BoardTileView & {
 *     classes: Record<string, boolean>
 *   }} BoardGridTile
 * @typedef {BoardGridTile[] & { id: string }} BoardGridRow
 */

/**
 * @param {ModelTile} model
 * @returns {BoardGridTile}
 */
function hostTile({ classes, unitId, ...tile }) {
  return /** @type {BoardGridTile} */ ({
    ...tile,
    ...(unitId ? { unitId } : {}),
    classes: Object.fromEntries(
      classes.map(({ name, enabled }) => [name, enabled])
    ),
  });
}

/**
 * @param {ModelRow[]} rows
 * @returns {BoardGridRow[]}
 */
export function rowsFromModel(rows) {
  return rows.map(({ id, tiles }) =>
    Object.assign(tiles.map(hostTile), { id })
  );
}

/**
 * @param {string} boardName
 * @returns {BoardGridRow[]}
 */
export function emptyTileRows(boardName) {
  return rowsFromModel(emptyRows(boardName));
}

/**
 * @param {string[][]} dataState
 * @param {import('./match-view-model.js').Coordinate[]=} previewCoordinates
 * @returns {BoardGridRow[]}
 */
export function setupRowsFromDataState(dataState, previewCoordinates = []) {
  const board = Array.from(dataState, (row) =>
    Array.from(row ?? [], (value) => value ?? '_')
  );
  return rowsFromModel(setupRows(board, previewCoordinates));
}

/**
 * @param {string} boardName
 * @param {import('./match-view-model.js').BoardTileView[]} tiles
 * @returns {BoardGridRow[]}
 */
export function rowsFromTiles(boardName, tiles) {
  return rowsFromModel(
    viewRows(
      boardName,
      tiles.map(({ unitId = '', ...tile }) => ({
        ...tile,
        unit_id: unitId,
      }))
    )
  );
}

/**
 * @param {BoardGridRow[]} rows
 * @param {import('./match-view-model.js').SunkClusterView[]} clusters
 * @returns {BoardGridRow[]}
 */
export function rowsWithSunkState(rows, clusters) {
  const tiles = rows.flat();
  const tileByKey = new Map(
    tiles.map((tile) => [`${tile.row}:${tile.column}`, tile])
  );
  const models = tiles.map((tile) => ({
    ...tile,
    unitId: tile.unitId ?? '',
    classes: [],
  }));
  sunkUpdates(models, clusters).forEach((model) => {
    const tile = /** @type {BoardGridTile} */ (
      tileByKey.get(`${model.row}:${model.column}`)
    );
    Object.assign(tile, hostTile(model));
  });
  return rows;
}
