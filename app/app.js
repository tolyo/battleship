import BattleShipBoard from './BattleShipBoard'
import Ship from './Ship'
import { Destroyer } from './ships'
import { Fleet } from './Fleet'

document.addEventListener('DOMContentLoaded', () => {
  // do your setup here
  console.log('Initialized board');
  new BattleShipBoard('board');

});


