import fs from 'fs';
import path from 'path';
import commonjs from '@rollup/plugin-commonjs';
import resolve from '@rollup/plugin-node-resolve';
import { bundle } from 'lightningcss';

const isWatch = process.argv.includes('-w') || process.argv.includes('--watch');

const isDev = process.env.DEV === '1' || isWatch;

const liveReloadEnabled = /^(1|true|yes|on)$/i.test(
  process.env.LIVE_RELOAD_ENABLED ?? ''
);

const liveReloadPort = process.env.LIVE_RELOAD_PORT ?? '35729';

const outputDir = 'priv/static';

function walkFiles(rootDir) {
  const watchPaths = [];

  function walk(currentDir) {
    watchPaths.push(currentDir);

    for (const entry of fs.readdirSync(currentDir, { withFileTypes: true })) {
      const entryPath = path.join(currentDir, entry.name);

      if (entry.isDirectory()) {
        walk(entryPath);
        continue;
      }

      if (entry.isFile()) {
        watchPaths.push(entryPath);
      }
    }
  }

  walk(rootDir);

  return watchPaths;
}

function watchFrontendFilesPlugin(
  paths = [{ path: './app', extensions: ['.css', '.html', '.html.dt', '.js'] }]
) {
  const resolvedPaths = paths
    .map((entry) => ({
      ...entry,
      resolvedPath: path.resolve(entry.path),
    }))
    .filter((entry) => fs.existsSync(entry.resolvedPath));

  return {
    name: 'watch-frontend-files',
    buildStart() {
      for (const entry of resolvedPaths) {
        const stats = fs.statSync(entry.resolvedPath);

        if (stats.isDirectory()) {
          for (const watchPath of walkFiles(entry.resolvedPath)) {
            const shouldWatchFile =
              !entry.extensions ||
              fs.statSync(watchPath).isDirectory() ||
              entry.extensions.includes(path.extname(watchPath));

            if (shouldWatchFile) {
              this.addWatchFile(watchPath);
            }
          }
          continue;
        }

        this.addWatchFile(entry.resolvedPath);
      }
    },
  };
}

function emitCssBundlePlugin(inputFile = './app/styles.css') {
  const resolvedInputFile = path.resolve(inputFile);

  return {
    name: 'emit-css-bundle',
    buildStart() {
      this.addWatchFile(resolvedInputFile);
    },
    generateBundle() {
      const { code } = bundle({
        filename: resolvedInputFile,
        minify: !isDev,
      });

      this.emitFile({
        type: 'asset',
        fileName: 'styles.css',
        source: new TextDecoder('utf-8').decode(code),
      });
    },
  };
}

function copyHtmlTemplatesPlugin(sourceRoot = './app') {
  const resolvedSourceRoot = path.resolve(sourceRoot);

  return {
    name: 'copy-html-templates',
    buildStart() {
      for (const watchPath of walkFiles(resolvedSourceRoot)) {
        if (
          fs.statSync(watchPath).isDirectory() ||
          ['.html', '.dt'].includes(path.extname(watchPath))
        ) {
          this.addWatchFile(watchPath);
        }
      }
    },
    generateBundle() {
      for (const sourcePath of walkFiles(resolvedSourceRoot)) {
        if (!fs.statSync(sourcePath).isFile()) {
          continue;
        }

        if (!['.html', '.dt'].includes(path.extname(sourcePath))) {
          continue;
        }

        this.emitFile({
          type: 'asset',
          fileName: path.relative(resolvedSourceRoot, sourcePath),
          source: fs.readFileSync(sourcePath, 'utf8'),
        });
      }
    },
  };
}

function replaceLiveReloadConfigPlugin() {
  return {
    name: 'replace-live-reload-config',
    transform(code, id) {
      if (!id.endsWith(`${path.sep}live_reload.js`)) {
        return null;
      }

      return {
        code: code
          .replaceAll('__LIVE_RELOAD_ENABLED__', liveReloadEnabled ? '1' : '0')
          .replaceAll('__LIVE_RELOAD_PORT__', liveReloadPort),
        map: null,
      };
    },
  };
}

const plugins = [
  watchFrontendFilesPlugin(),
  replaceLiveReloadConfigPlugin(),
  emitCssBundlePlugin(),
  copyHtmlTemplatesPlugin(),
  resolve(),
  commonjs(),
];

export default [
  {
    input: ['app/index.js', 'app/live_reload.js'],
    output: {
      dir: outputDir,
      entryFileNames: '[name].js',
      format: 'es',
    },
    plugins,
  },
];
