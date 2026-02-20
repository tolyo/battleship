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

document.addEventListener('DOMContentLoaded', () => {
  if (window.location.hostname === 'localhost') {
    const script = document.createElement('script');
    script.src =
      'http://localhost:3000/browser-sync/browser-sync-client.js?v=3.0.3';
    if (document.body) {
      document.body.appendChild(script);
    } else if (document.head) {
      document.head.appendChild(script);
    }
  }
});
