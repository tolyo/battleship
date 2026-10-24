import {
  board,
  dashboard,
  board_grid,
  login,
  register,
  room,
  routes,
  target_board,
  status_panel,
  setup_controls,
  setup_page,
} from '../_build/browser/js/modules/battleship/browser/Components.js';
import { create as createSetupPage } from '../_build/browser/js/modules/battleship/client/SetupPage.js';
import appConfig from './configuration.js';
import { create as createDashboard } from '../_build/browser/js/modules/battleship/client/Dashboard.js';
import { HeaderController } from './layout/header-ctrl.js';
import { MatchStateService } from './game/services/match-state-service.js';
import { RoomClientService } from './game/services/room-client-service.js';
import { create as createConnectionLifecycle } from '../_build/browser/js/modules/battleship/browser/ConnectionLifecycle.js';
import { SetupService } from './game/services/setup-service.js';
import { create as createTarget } from '../_build/browser/js/modules/battleship/browser/Target.js';
import { create as createMatchSnapshots } from '../_build/browser/js/modules/battleship/client/MatchSnapshots.js';
import { matchViewModelFromView } from './game/domain/match-view-model.js';
import { create as createRoomEntry } from '../_build/browser/js/modules/battleship/client/RoomEntry.js';
import { create as createRoomRecovery } from '../_build/browser/js/modules/battleship/browser/RoomRecovery.js';
import { RoomRestoreService } from './game/services/room-restore-service.js';
import { RoomEventDispatcherService } from './game/services/room-event-dispatcher-service.js';
import unitComponent from './game/components/unit.js';
import { ConnectionService } from './transport/connection-service.js';
import { RoomStoreService } from './room-store/room-store-service.js';

/**
 * @param {typeof import('@angular-wave/angular.ts').angular} angular
 */
export function configureBattleship(angular) {
  return angular
    .module('battleship', [])
    .config(appConfig())
    .router(routes())
    .service('matchState', MatchStateService)
    .service('connection', ConnectionService)
    .service('roomStore', RoomStoreService)
    .service('setup', SetupService)
    .service('target', ['matchState', createTarget])
    .value('snapshotProjection', { project: matchViewModelFromView })
    .service('matchView', [
      'matchState',
      'snapshotProjection',
      createMatchSnapshots,
    ])
    .service('roomEntry', [
      'roomStore',
      'matchState',
      'matchView',
      createRoomEntry,
    ])
    .service('roomRecovery', ['roomStore', 'matchState', createRoomRecovery])
    .service('roomRestore', RoomRestoreService)
    .service('roomEventDispatcher', RoomEventDispatcherService)
    .service('roomConnectionLifecycle', [
      'roomRestore',
      'matchState',
      createConnectionLifecycle,
    ])
    .service('roomClient', RoomClientService)
    .controller('HeaderController', HeaderController)
    .component('targetBoard', target_board())
    .component('boardGrid', board_grid())
    .component('dashboard', dashboard(['$rootScope', createDashboard]))
    .component('board', board())
    .component('unit', unitComponent)
    .component(
      'setupPage',
      setup_page(['matchState', 'roomClient', 'setup', createSetupPage])
    )
    .component('statusPanel', status_panel())
    .component('login', login())
    .component('room', room())
    .component('register', register())
    .component('setupControls', setup_controls());
}
