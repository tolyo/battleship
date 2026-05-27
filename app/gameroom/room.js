export default {
  templateUrl: '/_room',
  controller: class {
    static $inject = ['$rootScope'];

    /**
     * @param {ng.RootScopeService} $rootScope
     */
    constructor($rootScope) {
      $rootScope.authenticated = true;
    }
  },
};
