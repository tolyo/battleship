export default {
  templateUrl: '/_room',
  controller: class {
    static $inject = ['$rootScope'];

    constructor($rootScope) {
      $rootScope.authenticated = true;
    }
  },
};
