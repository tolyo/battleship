import http from 'http';
import path from 'path';

import chokidar from 'chokidar';

const HEARTBEAT_INTERVAL_MS = 15_000;
const RELOAD_DEBOUNCE_MS = 100;
const DEFAULT_PORT = 35_729;
const HTTP_NOT_FOUND = 404;

const IGNORED_PATTERNS = [
  /(^|[/\\])\.git([/\\]|$)/,
  /(^|[/\\])_build([/\\]|$)/,
  /(^|[/\\])node_modules([/\\]|$)/,
  /\.beam$/,
  /\.py[co]$/,
  /\.swp$/,
  /\.tmp$/,
  /~$/,
];

function parseArgs(argv) {
  let host = '127.0.0.1';
  let port = DEFAULT_PORT;
  const paths = [];

  for (let index = 0; index < argv.length; index += 1) {
    const argument = argv[index];

    if (argument === '--host') {
      host = argv[index + 1] ?? host;
      index += 1;
      continue;
    }

    if (argument === '--port') {
      port = Number(argv[index + 1] ?? port);
      index += 1;
      continue;
    }

    paths.push(path.resolve(argument));
  }

  if (paths.length === 0) {
    throw new Error('At least one watch path is required.');
  }

  return { host, paths, port };
}

function createReloadBroadcaster() {
  const clients = new Set();
  let reloadTimer = null;

  function broadcastReload() {
    reloadTimer = null;

    for (const client of clients) {
      client.write('data: reload\n\n');
    }
  }

  return {
    addClient(response) {
      clients.add(response);
      response.write(': connected\n\n');

      const heartbeat = setInterval(() => {
        response.write(': keepalive\n\n');
      }, HEARTBEAT_INTERVAL_MS);

      response.on('close', () => {
        clearInterval(heartbeat);
        clients.delete(response);
      });
    },
    scheduleReload() {
      if (reloadTimer !== null) {
        clearTimeout(reloadTimer);
      }

      reloadTimer = setTimeout(broadcastReload, RELOAD_DEBOUNCE_MS);
    },
  };
}

function createServer(host, port, broadcaster) {
  const server = http.createServer((request, response) => {
    if (request.url !== '/sse') {
      response.writeHead(HTTP_NOT_FOUND);
      response.end();
      return;
    }

    response.writeHead(200, {
      'Access-Control-Allow-Origin': '*',
      'Cache-Control': 'no-cache',
      Connection: 'keep-alive',
      'Content-Type': 'text/event-stream',
    });

    broadcaster.addClient(response);
  });

  server.listen(port, host);

  return server;
}

function main() {
  const { host, paths, port } = parseArgs(process.argv.slice(2));
  const broadcaster = createReloadBroadcaster();

  createServer(host, port, broadcaster);

  const watcher = chokidar.watch(paths, {
    awaitWriteFinish: {
      pollInterval: 50,
      stabilityThreshold: 100,
    },
    ignoreInitial: true,
    ignored: IGNORED_PATTERNS,
  });

  watcher.on('add', () => broadcaster.scheduleReload());
  watcher.on('change', () => broadcaster.scheduleReload());
  watcher.on('unlink', () => broadcaster.scheduleReload());
  watcher.on('addDir', () => broadcaster.scheduleReload());
  watcher.on('unlinkDir', () => broadcaster.scheduleReload());
}

main();
