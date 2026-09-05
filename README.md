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
