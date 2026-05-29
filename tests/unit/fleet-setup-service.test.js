import { FleetSetupService } from '../../app/game/fleet-setup-service.js';

describe('FleetSetupService', () => {
  it('randomizes game state and mirrors placements to layout', () => {
    const gameState = {
      shipPlacements: {
        0: [{ row: 1, column: 2 }],
      },
      randomizeFleet: jasmine.createSpy('randomizeFleet').and.callFake(() => {
        gameState.shipPlacements = {
          1: [{ row: 3, column: 4 }],
        };
      }),
    };
    const fleetLayout = {
      placeFleetShips: jasmine.createSpy('placeFleetShips'),
    };

    new FleetSetupService(gameState, fleetLayout).randomize();

    expect(gameState.randomizeFleet).toHaveBeenCalledOnceWith();
    expect(fleetLayout.placeFleetShips).toHaveBeenCalledOnceWith({
      1: [{ row: 3, column: 4 }],
    });
  });

  it('resets game state and layout placeholders', () => {
    const gameState = {
      resetSetupFleet: jasmine.createSpy('resetSetupFleet'),
    };
    const fleetLayout = {
      resetFleetToPlaceholders: jasmine.createSpy('resetFleetToPlaceholders'),
    };

    new FleetSetupService(gameState, fleetLayout).reset();

    expect(gameState.resetSetupFleet).toHaveBeenCalledOnceWith();
    expect(fleetLayout.resetFleetToPlaceholders).toHaveBeenCalledOnceWith();
  });
});
