//
// document.addEventListener('DOMContentLoaded', function () {
//   window.BattleShipBoard = new BattleShipBoard('board')
// })
var fleetMap = require('BoardMap').default
var fleet = require('fleet').default
var strikemap = require('strikemap').default({
  strikeCallback: function (y, x) {
    return false
  }
})

window.fleetMap = fleetMap
window.fleet = fleet
window.strikemap = strikemap