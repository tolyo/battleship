import {
  login,
  room,
  setup_controls,
  status_panel,
  target_board,
} from '../_build/browser/js/modules/battleship/browser/Components.js';

const loginComponent = login();
const roomComponent = room();
const setupControlsComponent = setup_controls();
const statusPanelComponent = status_panel();
const targetBoardComponent = target_board();

describe('declarative component contracts', () => {
  it('keeps server-rendered page template routes explicit', () => {
    expect(loginComponent.templateUrl).toBe('/_login');
    expect(roomComponent.templateUrl).toBe('/_room');
  });

  it('declares setup, status, and target board bindings', () => {
    expect(Object.keys(setupControlsComponent.bindings)).toEqual([
      'setupActive',
      'boardReady',
      'onReady',
      'onRandom',
      'onReset',
    ]);
    expect(Object.keys(statusPanelComponent.bindings)).toEqual([
      'playerLabel',
      'status',
    ]);
    expect(Object.keys(targetBoardComponent.bindings)).toEqual([
      'rows',
      'hidden',
      'visible',
      'disabled',
      'onSubmit',
    ]);
  });
});
