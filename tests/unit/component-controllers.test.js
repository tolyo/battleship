import battleBoardComponent from '../../app/map/battle-board.js';
import { BoardGridController } from '../../app/map/board-grid.js';
import setupActionsComponent from '../../app/map/setup-actions.js';

const BattleBoardController = battleBoardComponent.controller;
const SetupActionsController = setupActionsComponent.controller;

describe('BoardGridController', () => {
  it('passes clicked tiles through optional output binding', () => {
    const controller = new BoardGridController();
    const tile = { row: 1, column: 2, state: 'empty' };
    controller.onTileClick = jasmine.createSpy('onTileClick');

    controller.tileClicked(tile);

    expect(controller.onTileClick).toHaveBeenCalledOnceWith({ tile });
  });

  it('allows missing click binding', () => {
    const controller = new BoardGridController();

    expect(() =>
      controller.tileClicked({ row: 1, column: 2, state: 'empty' })
    ).not.toThrow();
  });
});

describe('BattleBoardController', () => {
  it('passes strikes through its output binding', () => {
    const controller = new BattleBoardController();
    const tile = { row: 3, column: 4, state: 'empty' };
    controller.onStrike = jasmine.createSpy('onStrike');

    controller.strike(tile);

    expect(controller.onStrike).toHaveBeenCalledOnceWith({ tile });
  });

  it('allows missing strike binding', () => {
    const controller = new BattleBoardController();

    expect(() =>
      controller.strike({ row: 3, column: 4, state: 'empty' })
    ).not.toThrow();
  });
});

describe('SetupActionsController', () => {
  it('routes ready, random, and reset actions to output bindings', () => {
    const controller = new SetupActionsController();
    controller.onReady = jasmine.createSpy('onReady');
    controller.onRandom = jasmine.createSpy('onRandom');
    controller.onReset = jasmine.createSpy('onReset');

    controller.ready();
    controller.random();
    controller.reset();

    expect(controller.onReady).toHaveBeenCalledOnceWith();
    expect(controller.onRandom).toHaveBeenCalledOnceWith();
    expect(controller.onReset).toHaveBeenCalledOnceWith();
  });

  it('allows missing action bindings', () => {
    const controller = new SetupActionsController();

    expect(() => {
      controller.ready();
      controller.random();
      controller.reset();
    }).not.toThrow();
  });
});
