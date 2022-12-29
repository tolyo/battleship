export enum ShipState {
  ACTIVE,
  WOUNDED,
  KILLED,
}

export enum ShipOrientation {
  VERTICAL,
  HORIZONTAL,
}

export enum GridHealth {
  ALIVE,
  KILLED,
}

abstract class Ship {
  private id: string;
  private health: ShipState;
  private hitcount: number;
  private size: number;
  private orientation: ShipOrientation;
  protected gridState: GridHealth[] = [];

  constructor(id: string, size: number) {
    this.id = id;
    this.health = ShipState.ACTIVE;
    this.gridState = [];
    this.hitcount = 0;
    this.size = size;
    this.orientation = ShipOrientation.HORIZONTAL;
  }

  reset() {
    this.coordinates = undefined;
    this.health = ShipState.ACTIVE;
    this.gridState.forEach((e, index) => {
      this.gridState[index] = GridHealth.ALIVE;
    });
    this.hitcount = 0;
    return this;
  }

  setColumn(column: number): void {
    this.column = column;
    return this;
  }

  setRow(row: number): void {
    this.row = row;
    return this;
  }

  setOrienation(orientation = ShipOrientation.HORIZONTAL) {
    this.orientation = orientation;
    return this;
  }

  setLocation({column, row, orientation}) {
    this.column = column;
    this.row = row;
    this.orientation = orientation;
    this.coordinates = undefined;
    return this;
  }

  getShipMapCoordinates() {
    if (this.coordinates !== undefined) {
      return this.coordinates
    }
    const coordinates = []
    for (let i = 0; i < this.size; i++) {
      if (this.orientation === ShipOrientation.HORIZONTAL) {
        coordinates.push({ row: this.row, column: this.column + i })
      } else {
        coordinates.push({ row: this.row + i, column: this.column })
      }
    }
    this.coordinates = coordinates
    return this.coordinates
  }

  attemptStrike(targetRow: number, targetColumn: number) {
    this.getShipMapCoordinates().forEach(({ row, column }, index) => {
      if (targetRow === row && targetColumn === column) {
        this.gridState[index] = GridHealth.KILLED
        this.strike()
      }
    })
  }

  strike(): void {
    if (this.health === ShipState.KILLED) {
      throw new Error('Illegal state. Ship already killed');
    }
    this.hitcount++;
    if (this.isKilled() === true) {
      this.health = ShipState.KILLED;
    } else {
      this.health = ShipState.WOUNDED;
    }
  }

  isKilled(): boolean {
    return this.hitcount === this.size;
  }
}

export class Carrier extends Ship {
  constructor(id: string) {
    super(id, 4);
    this.gridState = [
      GridHealth.ALIVE,
      GridHealth.ALIVE,
      GridHealth.ALIVE,
      GridHealth.ALIVE,
    ];
  }
}

export class Cruiser extends Ship {
  constructor(id: string) {
    super(id, 3);
    this.gridState = [GridHealth.ALIVE, GridHealth.ALIVE, GridHealth.ALIVE];
  }
}

export class Destroyer extends Ship {
  constructor(id: string) {
    super(id, 2);
    this.gridState = [GridHealth.ALIVE, GridHealth.ALIVE];
  }
}

export class TorpedoBoat extends Ship {
  constructor(id: string) {
    super(id, 1);
    this.gridState = [GridHealth.ALIVE];
  }
}
