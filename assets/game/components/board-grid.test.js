import { BoardGridController } from './board-grid.js';

describe('BoardGridController', () => {
  it('prefers positive visibility when it is bound', () => {
    const controller = new BoardGridController();
    controller.hidden = true;
    controller.visible = true;

    expect(controller.isVisible).toBe(true);
  });

  it('falls back to the hidden binding when visibility is unbound', () => {
    const controller = new BoardGridController();
    controller.hidden = true;

    expect(controller.isVisible).toBe(false);
  });

  it('exposes stable CSS classes for disabled state', () => {
    const controller = new BoardGridController();
    controller.disabled = true;

    expect(controller.cssClasses).toEqual({ disabled: true });
  });

  it('forwards tile submissions when a callback is bound', () => {
    const controller = new BoardGridController();
    const tile = { id: 'target-board-1-2', row: 1, column: 2 };
    controller.onTileClick = jasmine.createSpy('onTileClick');

    controller.submitTile(tile);

    expect(controller.onTileClick).toHaveBeenCalledOnceWith({ tile });
  });

  it('ignores tile submissions without an optional callback', () => {
    const controller = new BoardGridController();

    expect(() => controller.submitTile({ id: 'own-board-1-2' })).not.toThrow();
  });
});
