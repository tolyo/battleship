export class HeaderController {
  static $inject = ['$cookie'];

  /**
   * @param {ng.CookieService} cookieService
   */
  constructor(cookieService) {
    /** @type {ng.CookieService} */
    this.cookieService = cookieService;
  }

  logout() {
    this.cookieService.remove('SEC_USER');
    window.location.replace('/login');
  }
}
