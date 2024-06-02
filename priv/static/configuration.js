export default function appConfig(
  $locationProvider,
  $httpProvider,
  $urlServiceProvider
) {
  $locationProvider.hashPrefix('').html5Mode({
    enabled: true,
    requireBase: false,
    rewriteLinks: false,
  });
  $httpProvider.defaults.withCredentials = true;
  // Error intercepter for ajax requests
  $httpProvider.interceptors.push([
    '$q',
    ($q) => ({
      // eslint-disable-next-line consistent-return
      responseError: (response) => {
        if (response.status === 401) {
          // should redirect to error handler
          window.location.reload();
        } else {
          return $q.reject(response);
        }
      },
    }),
  ]);

  $urlServiceProvider.config.strictMode(true);
}

appConfig.$inject = [
  '$locationProvider',
  '$httpProvider',
  '$urlServiceProvider',
];
