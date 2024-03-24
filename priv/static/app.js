import mapComponent from './map/map-controller.js';
import appConfig from './configuration.js';

window.angular
  .module('battleship', ['ui.router'])
  .config(appConfig)
  .config([
    '$stateProvider',
    /**
     * @param {import("@uirouter/angularjs").StateProvider} $stateProvider
     */
    ($stateProvider) => {
      $stateProvider.state({
        name: 'home',
        url: '/',
        component: 'home',
      });
    },
  ])
  .component('home', mapComponent);

window.addEventListener('DOMContentLoaded', () => {
  window.angular.bootstrap(document, ['battleship'], { strictDi: true });
});
