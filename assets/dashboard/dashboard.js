export default {
  templateUrl: '/_dashboard',
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
