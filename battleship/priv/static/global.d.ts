import { StateService, UIRouter } from '@uirouter/core';
import FormController from './utils/form-controller';
import { RouteConfig } from './utils/router';
import MapController from './map/map-controller.js';
import Ship from './model/ship';

declare global {
  interface Window {
    router: UIRouter;
    routes: RouteConfig[];
    crudRoutes: RouteConfig[];
    stateService: StateService;
    EventBus: EventTarget;
    FormControllers: Array<FormController>;
    App: {
      MapController: MapController;
    };
    Fleet: Ship[];
  }
}

export {};
