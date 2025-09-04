import { angular } from '@angular-wave/angular.ts';
import mapComponent from './map/map-controller.js';
import registerComponent from './register/register.js';
import appConfig from './configuration.js';

angular
  .module('battleship', [])
  .filter('truthy', () => function (input) {
      console.log('truthy');
      return input ? 'true' : 'false';
    })
  .config(appConfig)
  .config([
    '$stateProvider',
    ($stateProvider) => {
      $stateProvider
        .state({
          name: 'register',
          url: '/register',
          component: 'register',
        })
        .state({
          name: 'home',
          url: '/',
          template: 'Prepare the fleet',
        });
    },
  ])
  .component('register', registerComponent)
  .component('home', mapComponent);

document.addEventListener('DOMContentLoaded', () => {
  if (window.location.hostname === 'localhost') {
    try {
      const script = document.createElement('script');
      script.src =
        'http://localhost:3000/browser-sync/browser-sync-client.js?v=3.0.3';
      if (document.body) {
        document.body.appendChild(script);
      } else if (document.head) {
        document.head.appendChild(script);
      }
    } catch (e) {
      console.error('Browsersync: could not append script tag', e);
    }
  }
});
