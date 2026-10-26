# Battleship on Terlan

Battleship is a Terlan VM application with a browser frontend packaged by
`terlc`. The backend source lives under `src/battleship`, PostgreSQL migrations
live under `sql`, and browser assets live under `assets`.

The application does not own a process-level `main()` function. The native executable
`_build/bin/battleship` starts the HTTP runtime with embedded web resources.
`terlc build` discovers the routes from the public `router/0` declarations in
`battleship.Web` and `battleship.WebSocket` and precompiles their handlers.

## Development

Requirements: a local Terlan checkout at `../terlan/terlan`, Node.js, Docker
Compose. Database migrations run through `terlc db`. Build the compiler and its
paired runtimes with the default PostgreSQL features:

```sh
cargo build --manifest-path ../terlan/terlan/Cargo.toml -p terlan \
  --bin terlc --bin terlan-vm --bin terlan-native-worker
```

```sh
make setup
make compile
make start
```

The frontend and backend are served at `http://127.0.0.1:8080`. `make start`
also starts PostgreSQL, applies pending migrations, and serves the browser
package with the Terlan backend.
Make exports the PostgreSQL settings from `config/dev.env` to Docker and the
server. `DATABASE_URL` can override the connection URI used by the server.
The native executable includes the PostgreSQL capability adapter.
Run `make compile` after source changes or a compiler upgrade, then restart
`make start` so it loads the rebuilt executable and its embedded browser resources.

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
make functional-test
```

The browser test builds and runs `_build/bin/battleship` with the same compiler
selected by Make, then plays a complete anonymous game in Chromium.
Terlan unit tests live beside their modules as `Account.terl` and
`AccountTest.terl`; `make test` discovers them under `src` and `browser`.
Frontend unit and interop tests live beside their modules under `assets`,
`browser`, and `client`; browser specs live beside the game adapters and host modules.

Portable browser models live under `browser/battleship/browser` with colocated
`*Test.terl` modules. Make compiles them to JavaScript before bundling the UI.
Angular Wave and the browser-facing adapters under `assets` consume those
generated modules; fleet definitions, board creation, geometry, placement validation,
random fleet placement, setup mutations, match-view projection, tile decoration,
preview markers, and sunk ship detection and edge flags are implemented in Terlan.
Board and board-grid components are declared in `Components.terl` and use native
Terlan templates directly, without JavaScript controllers or copied input state.
`UnitDrag.terl` and `UnitLayout.terl` own pointer ownership, grab offsets, movement
rounding, orientation, preview state, visibility, and placeholder resets.
`BoardUnits.terl` projects fleet and placement data for the board template, using
the single fleet catalogue in `Fleet.terl`. Their tests live beside the modules.
The remaining unit adapter resolves DOM
anchors, handles pointer capture, and batches DOM movement with animation frames.
`RoomRecovery.terl` supplies the recovery controller through typed callback ports;
its native tests check status forwarding, and JavaScript interop tests check method
receivers and cleanup order. The unused legacy `GameState` JavaScript model has
been removed; `RoomState.terl` owns the live session transitions.

`ConnectionContext.terl` distinguishes lobby connections from restored seats.
`RoomClient.terl`, `RoomRestore.terl`, and `RoomEvents.terl` own connection setup,
restore deadlines, and incoming room event dispatch. `ConnectionLifecycle.terl`
handles connection close and error callbacks through typed service ports. Browser
host controllers live under `client/battleship/client`;
`SetupPage.terl` restores the current room on initialization and closes it on teardown.
Make generates Angular bindings from its pinned TypeScript declarations under
`_build/bindings/angular`. Generated `OnInit.with_members` and
`OnDestroy.with_members` attach the lifecycle callbacks using Angular's original
JavaScript names while preserving the Terlan controller's fields. These host
modules compile to JavaScript separately from the portable VM models. Their
interop tests live beside them and run with the frontend unit suite.

`bindings/Host.d.ts` declares application values passed through browser services.
Make runs `terlc bind typescript --input bindings/Host.d.ts --namespace battleship.host
--out _build/bindings/host` to generate their Terlan getters, supported setters,
and data record constructors, and record the input
hashes. The JavaScript declarations for the resulting controllers import the
original TypeScript types. `client/battleship/client/RoomEntry.terl` uses these
bindings to apply an optional room snapshot and persist room identity after the
state accepts entry. Its tests cover missing and null snapshots, service method
receivers, and callback order.

`Dashboard.terl` updates the injected root scope through a generated setter.
`MatchSnapshots.terl` retains pending snapshots until player identity is known,
then calls the view projection and match-state methods through generated bindings.
`OperationTimeout.terl` owns timer replacement and cleanup using a generated mutable
record; Angular supplies the browser timer host. `ClientProtocol.terl` constructs
move and sync objects with their original wire keys, ready for `JSON.stringify`.
Their colocated interop tests cover proxy receivers, payload identity, and timer
replacement. Readonly declaration fields have no generated setter.

`Target.terl` reads live match permissions for each target attempt and returns a typed
move decision. Its interop tests cover Angular constructor injection and changing
permissions without rebuilding the service.

The shared `src/battleship/shared/ClientMessages.terl` module names client protocol
messages for both VM handlers and the browser. A restored connection requests a
fresh snapshot for both players, clearing the peer's disconnected status. The
colocated `RoomEntry.spec.ts` checks room entry, reload, fleet restoration, and a
subsequent move in Chromium, Firefox, and WebKit.

Terlan also owns the room transition graph, session status messages, move permissions,
and shared board constants; Angular builds its callbacks from the generated rules. Angular adapters
convert the generated records to row arrays and class maps while preserving
existing tile identities. Random placement uses a browser-supplied seed so the same algorithm can
be tested reproducibly on the VM.

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

HTML is authored as typed templates in `browser/battleship/browser/templates/`.
`Templates.terl` renders both server partials and Angular Wave component markup.
The build renders the application shell to `_build/templates/page.html`; the
compiler adds the browser bundles when packaging it. Do not edit generated HTML.

Use `make db-validate`, `make db-status`, and `make db-up` to validate, inspect,
and apply Terlan migrations. Files use timestamped names and `-- +terlan Up` /
`-- +terlan Down` markers. `make db-down` stops PostgreSQL and retains its data;
`make db-rebuild` explicitly recreates the local development schema. The initial
migration also supports existing installations without deleting account rows.

The app pins Angular Wave 0.32.0 and uses its declarative router, state-machine,
and WebSocket APIs. `npm install` and web builds apply the reproducible object
input identity backport described in `patches/README.txt`. The backport is
implemented and checked by `tools/AngularCompatibilityTest.terl`.
