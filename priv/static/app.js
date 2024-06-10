import mapComponent from './map/map-controller.js';
import appConfig from './configuration.js';

// @ts-ignore
window.angular
  .module('battleship', ['ui.router'])
  .config(appConfig)
  .config([
    '$stateProvider',

    // TODO fix type imports
    // /**
    //  * @param {import("@uirouter/angularjs").StateProvider} $stateProvider
    //  */
    ($stateProvider) => {
      $stateProvider.state({
        name: 'home',
        url: '/',
        component: 'home',
      });
    },
  ])
  .component('home', mapComponent);
