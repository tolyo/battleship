import { angular } from '@angular-wave/angular.ts';
import mapComponent from './map/map-controller.js';
import registerComponent from './register/register.js';
import loginComponent from './login/login.js';
import appConfig from './configuration.js';
import dashboardComponent from './dashboard/dashboard.js';
import roomComponent from './gameroom/room.js';
import { HeaderController } from './layout/header-ctrl.js';
import { GameStateService } from './game/game-state-service.js';
import fleetShipComponent from './game/fleet-ship.js';

angular
  .module('battleship', [])
  .config(appConfig)
  .config([
    '$stateProvider',
    /**
     *
     * @param {ng.StateProvider} $stateProvider
     */
    ($stateProvider) => {
      $stateProvider
        .state({
          name: 'dashboard',
          url: '/dashboard',
          component: 'dashboard',
        })
        .state({
          name: 'room',
          url: '/room',
          component: 'room',
        })
        .state({
          name: 'activeRoom',
          url: '/room/:roomId',
          component: 'room',
        })
        .state({
          name: 'register',
          url: '/register',
          component: 'register',
        })
        .state({
          name: 'login',
          url: '/login',
          component: 'login',
        })
        .state({
          name: 'home',
          url: '/',
          component: 'fleetSetup',
        });
    },
  ])
  .factory('gameState', () => new GameStateService())
  .controller('HeaderController', HeaderController)
  .component('dashboard', dashboardComponent)
  .component('fleetShip', fleetShipComponent)
  .component('fleetSetup', mapComponent)
  .component('login', loginComponent)
  .component('room', roomComponent)
  .component('register', registerComponent);

angular.bootstrap(document, ['battleship']);
