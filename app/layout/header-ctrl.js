export class HeaderController {
  logout() {
    deleteCookie('SEC_USER');
    window.location.replace('/login');
  }
}

function deleteCookie(name) {
  document.cookie = `${name}=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; SameSite=Lax;`;
}
