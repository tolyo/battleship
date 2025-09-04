/* eslint-disable no-param-reassign */
export default function appConfig($httpProvider) {
  $httpProvider.defaults.withCredentials = true;
  // Error intercepter for ajax requests
  $httpProvider.interceptors.push([
    () => ({
      // eslint-disable-next-line consistent-return
      responseError: (response) => {
        if (response.status === 401) {
          // should redirect to error handler
          window.location.reload();
        } else {
          return Promise.reject(response);
        }
      },
    }),
  ]);
}

appConfig.$inject = ['$httpProvider'];
