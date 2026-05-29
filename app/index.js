import { angular } from '@angular-wave/angular.ts';
import mapComponent from './map/map-controller.js';
import registerComponent from './register/register.js';
import loginComponent from './login/login.js';
import appConfig from './configuration.js';
import dashboardComponent from './dashboard/dashboard.js';
import roomComponent from './gameroom/room.js';
import { HeaderController } from './layout/header-ctrl.js';
import { GameStateService } from './game/game-state-service.js';
import { BattleRoomClientService } from './game/battle-room-client-service.js';
import { FleetLayoutService } from './game/fleet-layout-service.js';
import { FleetSetupService } from './game/fleet-setup-service.js';
import fleetShipComponent from './game/fleet-ship.js';
import battleBoardComponent from './map/battle-board.js';
import boardGridComponent from './map/board-grid.js';
import fleetBoardComponent from './map/fleet-board.js';
import gameStatusComponent from './map/game-status.js';
import setupActionsComponent from './map/setup-actions.js';
import { SessionService } from './session/session-service.js';
import { RoomSessionService } from './room/room-session-service.js';

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
  .service('gameState', GameStateService)
  .service('session', SessionService)
  .service('roomSession', RoomSessionService)
  .service('fleetLayout', FleetLayoutService)
  .service('fleetSetup', FleetSetupService)
  .service('battleRoomClient', BattleRoomClientService)
  .controller('HeaderController', HeaderController)
  .component('battleBoard', battleBoardComponent)
  .component('boardGrid', boardGridComponent)
  .component('dashboard', dashboardComponent)
  .component('fleetBoard', fleetBoardComponent)
  .component('fleetShip', fleetShipComponent)
  .component('fleetSetup', mapComponent)
  .component('gameStatus', gameStatusComponent)
  .component('login', loginComponent)
  .component('room', roomComponent)
  .component('register', registerComponent)
  .component('setupActions', setupActionsComponent);

angular.bootstrap(document, ['battleship']);
