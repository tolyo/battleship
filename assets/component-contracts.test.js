import loginComponent from './login/login.js';
import roomComponent from './room-page/room.js';
import setupControlsComponent from './game/components/setup-controls.js';
import statusPanelComponent from './game/components/status-panel.js';
import targetBoardComponent from './game/components/target-board.js';

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
