import BattleShipBoard from './BattleShipBoard'
import Ship from './Ship'

document.addEventListener('DOMContentLoaded', () => {
  // do your setup here
  console.log('Initialized board');
  new BattleShipBoard('board');
  new Ship('ship')

});


