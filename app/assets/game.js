var BattleShipBoard = require('BattleShipBoard').default

document.addEventListener('DOMContentLoaded', function () {
  // do your setup here
  console.log(BattleShipBoard);
  window.BattleShipBoard = new BattleShipBoard('board');

});
