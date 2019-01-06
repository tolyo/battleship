import { Carrier, Cruiser, Destroyer, TorpedoBoat } from './ship'

// Fleet representation
const Fleet = [
  // 1 carrier
  new Carrier('0'),

  // // 2 cruisers
  new Cruiser('1'),
  new Cruiser('2'),
  //
  // // 3 destroyers
  new Destroyer('3'),
  new Destroyer('4'),
  new Destroyer('5'),

  // // 4 torpedo boats
  new TorpedoBoat('6'),
  new TorpedoBoat('7'),
  new TorpedoBoat('8'),
  new TorpedoBoat('9')
]

export default Fleet
