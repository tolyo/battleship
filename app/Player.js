
const FLEET_SIZE = 4 + 3 * 2 + 2 * 3 + 4 * 1

export default class Player {

  constructor(id) {
    this.id = id
    this.health = FLEET_SIZE
  }

  decreaseHealth() {
    this.health = this.health - 1
  }

  isDead() {
    return this.health === 0
  }

}