

import { ShipOrientation } from './Ship'

export const getRandomTileCoordinate = () => {
  return {
    row: getRandomTile(),
    column : getRandomTile()
  }
}

const getRandomTile = () => Math.floor(Math.random() * 9)

export const getRandomOrientation = () => [ShipOrientation.VERTICAL, ShipOrientation.HORIZONTAL][Math.round(Math.random())]
