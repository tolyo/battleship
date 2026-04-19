import { angular } from '@angular-wave/angular.ts';

export class HeaderController {
  static $inject = [angular.$t.$cookie];

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
