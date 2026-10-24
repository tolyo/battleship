import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import path from 'node:path';
import { chromium } from 'playwright';

const root = process.env.TERLAN_WEB_DIR ?? '_build/.terlan/application/web';
const manifest = JSON.parse(
  await readFile(path.join(root, 'manifest.json'), 'utf8')
);
const assets = manifest.assets.filter((asset) => asset.kind === 'css');
assert.ok(assets.length > 0, 'the web package must contain CSS');
const styles = await Promise.all(
  assets.map((asset) =>
    readFile(path.join(root, asset.web_relative_path), 'utf8')
  )
);
const browser = await chromium.launch({
  headless: true,
  ...(process.env.CHROMIUM_EXECUTABLE_PATH
    ? { executablePath: process.env.CHROMIUM_EXECUTABLE_PATH }
    : {}),
});
try {
  const page = await browser.newPage({
    viewport: { width: 1280, height: 720 },
  });
  await page.setContent(`<!doctype html><html><body>
    <main><div id="map"><div id="units"><div class="place-holder"></div></div></div></main>
    <div id="own-board" class="disabled"><div class="own-board-tile"></div></div>
    <div id="target-board" class="disabled"><div class="target-board-tile"></div></div>
    <div id="setup-controls"><button>Ready</button><button>Clear</button></div>
    <div class="unit" data-unit="normal"></div>
    <div class="unit killed" data-unit="killed"></div>
    <div class="unit dragged" data-unit="dragged"></div>
    <div class="unit error" data-unit="error"></div>
    <div class="unit killed dragged" data-unit="killed-dragged"></div>
    <div class="unit killed dragged error" data-unit="combined"></div>
  </body></html>`);
  for (const content of styles) await page.addStyleTag({ content });
  const actual = await page.evaluate(() => {
    const pick = (selector, keys) => {
      const style = getComputedStyle(document.querySelector(selector));
      return Object.fromEntries(keys.map((key) => [key, style[key]]));
    };
    return {
      own: pick('#own-board', ['width', 'height', 'opacity', 'cursor']),
      target: pick('#target-board', ['width', 'height', 'opacity', 'cursor']),
      ownTile: pick('.own-board-tile', ['width', 'height', 'boxSizing']),
      targetTile: pick('.target-board-tile', ['width', 'height', 'boxSizing']),
      controls: pick('#setup-controls', ['display', 'gap']),
      placeholder: pick('#units .place-holder', [
        'marginTop',
        'marginRight',
        'marginBottom',
        'marginLeft',
      ]),
      main: pick('main', [
        'marginTop',
        'marginRight',
        'marginBottom',
        'marginLeft',
      ]),
      rem: Number.parseFloat(
        getComputedStyle(document.documentElement).fontSize
      ),
      halfRem: `${Number.parseFloat(getComputedStyle(document.documentElement).fontSize) / 2}px`,
      units: Object.fromEntries(
        [...document.querySelectorAll('[data-unit]')].map((node) => {
          const style = getComputedStyle(node);
          return [
            node.dataset.unit,
            {
              width: style.width,
              height: style.height,
              boxSizing: style.boxSizing,
              borders: ['Top', 'Right', 'Bottom', 'Left'].map((side) => ({
                width: style[`border${side}Width`],
                style: style[`border${side}Style`],
                color: style[`border${side}Color`],
              })),
            },
          ];
        })
      ),
    };
  });
  const board = {
    width: '300px',
    height: '300px',
    opacity: '0.8',
    cursor: 'not-allowed',
  };
  const tile = { width: '30px', height: '30px', boxSizing: 'border-box' };
  assert.deepEqual(actual.own, board);
  assert.deepEqual(actual.target, board);
  assert.deepEqual(actual.ownTile, tile);
  assert.deepEqual(actual.targetTile, tile);
  assert.deepEqual(actual.controls, { display: 'flex', gap: actual.halfRem });
  assert.deepEqual(actual.placeholder, {
    marginTop: `${actual.rem}px`,
    marginRight: '0px',
    marginBottom: `${actual.rem * 1.5}px`,
    marginLeft: `${actual.rem}px`,
  });
  assert.deepEqual(actual.main, {
    marginTop: '0px',
    marginRight: '75px',
    marginBottom: '0px',
    marginLeft: '75px',
  });
  for (const [state, color] of Object.entries({
    normal: 'rgb(127, 136, 26)',
    killed: 'rgb(141, 33, 36)',
    dragged: 'rgb(2, 255, 0)',
    error: 'rgb(244, 54, 63)',
    'killed-dragged': 'rgb(2, 255, 0)',
    combined: 'rgb(244, 54, 63)',
  })) {
    assert.deepEqual(
      actual.units[state],
      {
        width: '30px',
        height: '30px',
        boxSizing: 'border-box',
        borders: Array.from({ length: 4 }, () => ({
          width: '2px',
          style: 'solid',
          color,
        })),
      },
      `unit border state: ${state}`
    );
  }
  await page.setViewportSize({ width: 800, height: 720 });
  const responsive = await page.evaluate(() => {
    const main = getComputedStyle(document.querySelector('main'));
    return {
      width: main.width,
      left: main.marginLeft,
      right: main.marginRight,
    };
  });
  assert.deepEqual(responsive, { width: '700px', left: '50px', right: '50px' });
  console.log(
    'Typed CSS reference passed: dimensions, controls, disabled states, compound spacing, centering, and unit border states'
  );
} finally {
  await browser.close();
}
