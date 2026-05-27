/**
 * @param {ng.HttpProvider} $httpProvider
 */
export default function appConfig($httpProvider) {
  $httpProvider.defaults.withCredentials = true;
  // Error intercepter for ajax requests
  $httpProvider.interceptors.push([
    () => ({
      /**
       * @template T
       * @param {unknown} response
       * @returns {Promise<ng.HttpResponse<T>>}
       */
      responseError: (response) => {
        const httpResponse = /** @type {ng.HttpResponse<T>} */ (response);
        if (httpResponse.status === 401) {
          // should redirect to error handler
          window.location.href = '/login';
        }
        return Promise.reject(httpResponse);
      },
    }),
  ]);
}

appConfig.$inject = ['$httpProvider'];
