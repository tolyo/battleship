import './node_modules/@angular-wave/angular.ts/dist/angular-ts.esm.js';
import mapComponent from './map/map-controller.js';
import appConfig from './configuration.js';

// @ts-ignore
window.angular
  .module('battleship', [])
  .config(appConfig)
  .config([
    '$stateProvider',
    ($stateProvider) => {
      $stateProvider.state({
        name: 'home',
        url: '',
        component: 'home',
      })
    },
  ])
  .component('home', mapComponent);
