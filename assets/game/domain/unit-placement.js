import {
  all_placed as allPlaced,
  can_place_setup as canPlaceSetup,
  layout,
  random_layout as randomLayout,
} from '../../../_build/browser/js/modules/battleship/browser/Placement.js';
import { coordinates_from_start as coordinatesFromStart } from '../../../_build/browser/js/modules/battleship/browser/UnitGeometry.js';

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 */
export function checkedPlacements(placements) {
  return Object.entries(placements).map(([id, coordinates]) => ({
    id,
    coordinates: checkedCoordinates(
      Array.isArray(coordinates) ? coordinates : []
    ),
  }));
}

/**
 * @param {unknown[]} coordinates
 * @returns {import('./match-view-model.js').Coordinate[]}
 */
export function checkedCoordinates(coordinates) {
  const normalized = Array.from(coordinates, (coordinate) => {
    const point = /** @type {{ row?: unknown, column?: unknown } | null} */ (
      coordinate
    );
    return typeof point === 'object' &&
      point !== null &&
      Number.isInteger(point.row) &&
      Number.isInteger(point.column)
      ? /** @type {import('./match-view-model.js').Coordinate} */ (point)
      : { row: -1, column: -1 };
  });
  return normalized.every((point, index) => point === coordinates[index])
    ? /** @type {import('./match-view-model.js').Coordinate[]} */ (coordinates)
    : normalized;
}

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 */
export function boardStateFromPlacements(placements) {
  return layout(checkedPlacements(placements));
}

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 */
export function allUnitsPlaced(placements) {
  return allPlaced(checkedPlacements(placements));
}

/**
 * @param {number=} seed
 * @returns {Record<string, import('./match-view-model.js').Coordinate[]>}
 */
export function randomUnitPlacements(
  seed = Math.floor(Math.random() * 2147483646) + 1
) {
  const result = randomLayout(seed);
  if (result.error) {
    throw new Error(result.error);
  }
  return Object.fromEntries(
    result.placements.map(({ id, coordinates }) => [id, coordinates])
  );
}

/**
 * @param {Record<string, import('./match-view-model.js').Coordinate[]>} placements
 * @param {string} unitId
 * @param {import('./match-view-model.js').Coordinate[]} coordinates
 */
export function canPlaceSetupUnit(placements, unitId, coordinates) {
  return canPlaceSetup(
    checkedPlacements(placements),
    unitId,
    checkedCoordinates(coordinates)
  );
}

/**
 * @param {string} row
 * @param {string} column
 * @param {'VERTICAL' | 'HORIZONTAL'} orientation
 * @param {number} size
 * @returns {import('./match-view-model.js').Coordinate[]}
 */
export function unitCoordinatesFromStart(row, column, orientation, size) {
  const y = parseInt(row, 10);
  const x = parseInt(column, 10);
  if (!Number.isInteger(y) || !Number.isInteger(x)) {
    return [];
  }

  return coordinatesFromStart(y, x, orientation, size);
}
