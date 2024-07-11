import mapComponent from './map/map-controller.js';
import appConfig from './configuration.js';

// @ts-ignore
window.angular
  .module('battleship', ['ng.router'])
  .config(appConfig)
  .config([
    '$stateProvider',
    ($stateProvider) => {
      $stateProvider.state({
        name: 'home',
        url: '/',
        component: 'home',
      });
    },
  ])
  .component('home', mapComponent);
