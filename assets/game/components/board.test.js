import { BoardController } from './board.js';

describe('BoardController', () => {
  it('precomputes unit views with placement coordinates', () => {
    const controller = new BoardController();
    controller.units = [
      { id: '1', placeholderWidth: '20px' },
      { id: '2', placeholderWidth: '30px' },
    ];
    controller.unitPlacements = {
      2: [{ row: 4, column: 5 }],
    };

    controller.$onChanges();

    expect(controller.unitViews).toEqual([
      {
        id: '1',
        unit: { id: '1', placeholderWidth: '20px' },
        coordinates: undefined,
        placeholderStyle: { width: '20px' },
      },
      {
        id: '2',
        unit: { id: '2', placeholderWidth: '30px' },
        coordinates: [{ row: 4, column: 5 }],
        placeholderStyle: { width: '30px' },
      },
    ]);
  });

  it('exposes active room classes', () => {
    const controller = new BoardController();
    controller.active = true;

    expect(controller.cssClasses).toEqual({ 'active-room': true });
  });

  it('treats missing units and placements as empty inputs', () => {
    const controller = new BoardController();
    controller.units = undefined;
    controller.unitPlacements = undefined;

    controller.$onChanges();

    expect(controller.unitViews).toEqual([]);
  });
});
