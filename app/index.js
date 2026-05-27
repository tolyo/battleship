import { angular } from '@angular-wave/angular.ts';
import mapComponent from './map/map-controller.js';
import registerComponent from './register/register.js';
import loginComponent from './login/login.js';
import appConfig from './configuration.js';
import dashboardComponent from './dashboard/dashboard.js';
import { HeaderController } from './layout/header-ctrl.js';

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
          template: 'Prepare the fleet',
        });
    },
  ])
  .controller('HeaderController', HeaderController)
  .component('dashboard', dashboardComponent)
  .component('login', loginComponent)
  .component('register', registerComponent)
  .component('home', mapComponent);

  angular.bootstrap(document, ['battleship']);