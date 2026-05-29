import { FleetLayoutService } from '../../app/game/fleet-layout-service.js';

describe('FleetLayoutService', () => {
  it('registers, sorts, and unregisters fleet ship controllers', () => {
    const service = new FleetLayoutService();
    const shipTwo = fleetShipController('2');
    const shipZero = fleetShipController('0');

    service.registerFleetShip(shipTwo);
    const unregister = service.registerFleetShip(shipZero);

    expect(service.fleetShips()).toEqual([shipZero, shipTwo]);

    unregister();

    expect(service.fleetShips()).toEqual([shipTwo]);
  });

  it('resets all registered ships to placeholders', () => {
    const service = new FleetLayoutService();
    const shipOne = fleetShipController('1');
    const shipZero = fleetShipController('0');
    service.registerFleetShip(shipOne);
    service.registerFleetShip(shipZero);

    service.resetFleetToPlaceholders();

    expect(shipZero.setOnPlaceholder).toHaveBeenCalledOnceWith();
    expect(shipOne.setOnPlaceholder).toHaveBeenCalledOnceWith();
  });

  it('places ships using explicit coordinates and filters invalid values', () => {
    const service = new FleetLayoutService();
    const shipZero = fleetShipController('0');
    const unbound = {
      ship: undefined,
      setOnPlaceholder: jasmine.createSpy('setOnPlaceholder'),
      placeOnBoardCoordinates: jasmine.createSpy('placeOnBoardCoordinates'),
    };
    service.registerFleetShip(shipZero);
    service.registerFleetShip(unbound);

    service.placeFleetShips({
      0: [
        { row: 1, column: 2 },
        { row: Number.NaN, column: 3 },
        { row: 4, column: 5 },
      ],
    });

    expect(shipZero.placeOnBoardCoordinates).toHaveBeenCalledOnceWith([
      { row: 1, column: 2 },
      { row: 4, column: 5 },
    ]);
    expect(unbound.placeOnBoardCoordinates).not.toHaveBeenCalled();
  });

  it('passes an empty coordinate list for missing ship ids', () => {
    const service = new FleetLayoutService();
    const shipZero = fleetShipController('0');
    service.registerFleetShip(shipZero);

    service.placeFleetShips({});

    expect(shipZero.placeOnBoardCoordinates).toHaveBeenCalledOnceWith([]);
  });
});

function fleetShipController(id) {
  return {
    ship: { id },
    setOnPlaceholder: jasmine.createSpy(`setOnPlaceholder-${id}`),
    placeOnBoardCoordinates: jasmine.createSpy(`placeOnBoardCoordinates-${id}`),
  };
}
