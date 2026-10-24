export default function appConfig() {
  return {
    $http: {
      defaults: { withCredentials: false },
      // Redirect unauthorized requests to sign-in.
      interceptors: [
        () => ({
          /**
           * @template T
           * @param {unknown} response
           * @returns {Promise<ng.HttpResponse<T>>}
           */
          responseError: (response) => {
            const httpResponse = /** @type {ng.HttpResponse<T>} */ (response);
            if (httpResponse.status === 401) {
              window.location.href = '/login';
            }
            return Promise.reject(httpResponse);
          },
        }),
      ],
    },
  };
}
