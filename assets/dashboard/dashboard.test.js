import dashboardComponent from './dashboard.js';

const DashboardController = dashboardComponent.controller;

describe('dashboard controller', () => {
  it('marks root scope as authenticated', () => {
    const rootScope = {};

    const controller = new DashboardController(rootScope);

    expect(controller).toBeDefined();
    expect(rootScope.authenticated).toBe(true);
  });
});
