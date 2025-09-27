export default {
  templateUrl: '/_dashboard',
  controller: class {
    static $inject = ['$rootScope'];

    constructor($rootScope) {
      $rootScope.authenticated = true;
    }
  },
};
