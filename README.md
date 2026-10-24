# Battleship on Terlan

Battleship is a Terlan VM application with a browser frontend packaged by
`terlc`. The backend source lives under `src/battleship`, PostgreSQL migrations
live under `sql`, and browser assets live under `assets`.

The application does not own a process-level `main()` function. `terlc serve`
starts the HTTP runtime from `_build/web/manifest.json`; `terlc build` generates
that manifest from the public `router/0` declarations in `battleship.Web` and
`battleship.WebSocket`.

## Development

Requirements: a local Terlan checkout at `../terlan/terlan`, Node.js, Docker
Compose, and Goose.

```sh
make setup
make compile
make start
```

The frontend and backend are served at `http://127.0.0.1:8080`. `make start`
also starts PostgreSQL and keeps the browser package and Terlan server together.

Board dimensions, tile sizing, setup controls, disabled-state styles, spacing, and
unit-state borders live in `src/battleship/Styles.terl`.
The module composes specification-generated CSS
property structs with `includes` and constructs typed CSS values. Compound margins
use `Margin.pair` and `Margin.quad` with typed components, including `auto`.
Unit borders use `Border.compose` with distinct width and style parts and a
validated RGB color. Their normal, killed, dragged, and error rules preserve
the original cascade order. The module's pure `stylesheet/0` function is evaluated
during the web build. `[web.assets].styles`
lists the handwritten CSS entry followed by this module; Terlan bundles them
with its managed Lightning CSS toolchain. `StylesTest.terl` checks the shared
defaults and invalid dimensions. Other styles remain in `assets`.
After a web build, `npm run test:styles` checks the emitted CSS in Chromium
without starting PostgreSQL or the application server.
`npm run test:styles:build` builds the real stylesheets in a temporary CSS-only
project and runs the same browser checks. This also verifies the styles while
backend work is in progress. Set `TERLC` to select a compiler executable and
`CHROMIUM_EXECUTABLE_PATH` to use an installed Chromium browser.

## Verification

```sh
make quality
make integration-test
```

The integration suite resets its Compose database, applies the SQL migrations,
builds native VM and browser targets, verifies registration and login, and
exercises two-client WebSocket matchmaking, a move, and restoration of a
disconnected seat through the browser's `room_id`/`player_id` URL. Paired
sessions retain move history in the runtime hub, rebuild the immutable Terlan
rules model from each submitted board, and emit a player-specific public view
after every accepted move and successful restore.
Rooms with both sockets disconnected remain restorable for five minutes; the
runtime retains at most 1,024 abandoned rooms for this endpoint.

`make coverage` first records callable hits from the real HTTP/WebSocket
integration server, merges them with the VM unit-test run, and enforces 100%
Terlan source-declaration coverage. It also enforces 100% statements, branches,
functions, and lines across the browser application modules. The browser-only
`index.js` bootstrap and the development `live_reload.js` client are excluded;
route and dependency registration lives in the separately tested
`application.js` module.

`make format` also formats PostgreSQL migrations with the pinned
`sql-formatter` configuration. `make lint` verifies the formatted result and
enforces a 100-column maximum for migration files.
