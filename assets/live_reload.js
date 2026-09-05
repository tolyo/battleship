const LIVE_RELOAD_ENABLED = String('__LIVE_RELOAD_ENABLED__') === '1';

const LIVE_RELOAD_PORT = '__LIVE_RELOAD_PORT__';

const LIVE_RELOAD_URL = `http://127.0.0.1:${LIVE_RELOAD_PORT}/sse`;

const NAVIGATION_SUPPRESSION_MS = 2_000;

/**
 * @param {EventTarget | null} target
 * @returns {HTMLAnchorElement | null}
 */
function sameOriginLink(target) {
  if (!(target instanceof Element)) {
    return null;
  }

  const anchor = target.closest('a[href]');

  if (!(anchor instanceof HTMLAnchorElement)) {
    return null;
  }

  if (anchor.target && anchor.target !== '_self') {
    return null;
  }

  if (anchor.hasAttribute('download')) {
    return null;
  }

  const url = new URL(anchor.href, window.location.href);

  if (url.origin !== window.location.origin) {
    return null;
  }

  return anchor;
}

function setupLiveReload() {
  if (!LIVE_RELOAD_ENABLED) {
    return;
  }

  let suppressReloadUntil = 0;

  const liveReload = new EventSource(LIVE_RELOAD_URL);

  document.addEventListener(
    'click',
    (event) => {
      if (event.defaultPrevented) {
        return;
      }

      if (
        event.button !== 0 ||
        event.metaKey ||
        event.ctrlKey ||
        event.shiftKey ||
        event.altKey
      ) {
        return;
      }

      if (sameOriginLink(event.target) === null) {
        return;
      }

      suppressReloadUntil = Date.now() + NAVIGATION_SUPPRESSION_MS;
    },
    { capture: true }
  );

  window.addEventListener('beforeunload', () => {
    suppressReloadUntil = Number.POSITIVE_INFINITY;
    liveReload.close();
  });

  liveReload.onmessage = () => {
    if (Date.now() < suppressReloadUntil) {
      return;
    }

    if (document.visibilityState === 'hidden') {
      return;
    }

    window.location.reload();
  };
}

setupLiveReload();
