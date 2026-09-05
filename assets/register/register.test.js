import registerComponent from './register.js';

const RegisterController = registerComponent.controller;

describe('register controller', () => {
  it('initializes success state', () => {
    const controller = new RegisterController();

    expect(controller.success).toBe(false);
  });
});
