import { angular } from '@angular-wave/angular.ts';
import setupPageComponent from './map/setup-page-controller.js';
import registerComponent from './register/register.js';
import loginComponent from './login/login.js';
import appConfig from './configuration.js';
import dashboardComponent from './dashboard/dashboard.js';
import roomComponent from './room-page/room.js';
import { HeaderController } from './layout/header-ctrl.js';
import { MatchStateService } from './game/services/match-state-service.js';
import { RoomClientService } from './game/services/room-client-service.js';
import { SetupService } from './game/services/setup-service.js';
import { TargetService } from './game/services/target-service.js';
import { MatchViewService } from './game/services/match-view-service.js';
import { RoomEntryService } from './game/services/room-entry-service.js';
import unitComponent from './game/components/unit.js';
import targetBoardComponent from './game/components/target-board.js';
import boardGridComponent from './game/components/board-grid.js';
import boardComponent from './game/components/board.js';
import statusPanelComponent from './game/components/status-panel.js';
import setupControlsComponent from './game/components/setup-controls.js';
import { ConnectionService } from './transport/connection-service.js';
import { RoomStoreService } from './room-store/room-store-service.js';

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
          component: 'setupPage',
        });
    },
  ])
  .service('matchState', MatchStateService)
  .service('connection', ConnectionService)
  .service('roomStore', RoomStoreService)
  .service('setup', SetupService)
  .service('target', TargetService)
  .service('matchView', MatchViewService)
  .service('roomEntry', RoomEntryService)
  .service('roomClient', RoomClientService)
  .controller('HeaderController', HeaderController)
  .component('targetBoard', targetBoardComponent)
  .component('boardGrid', boardGridComponent)
  .component('dashboard', dashboardComponent)
  .component('board', boardComponent)
  .component('unit', unitComponent)
  .component('setupPage', setupPageComponent)
  .component('statusPanel', statusPanelComponent)
  .component('login', loginComponent)
  .component('room', roomComponent)
  .component('register', registerComponent)
  .component('setupControls', setupControlsComponent);

angular.bootstrap(document, ['battleship']);
