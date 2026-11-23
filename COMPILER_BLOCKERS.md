# Compiler compatibility status

## Current status: application compile succeeds

Verified November 24, 2026, with the current compiler working tree at checkout
`d4ae16eba` (`terlc 0.0.10`), including concurrent uncommitted compiler edits.
`terlc`, `terlan-vm`, and `terlan-native-worker` were rebuilt together with:

```sh
cargo build -p terlan --bin terlc --bin terlan-vm --bin terlan-native-worker
```

Host and Angular bindings were regenerated with those binaries. The complete
application build passes:

```sh
make compile TERLC=/home/anatoly/Applications/terlan/terlan/target/debug/terlc
```

This includes Angular compatibility, browser modules, native application
packaging, and the web router preflight. The resulting executable was built
November 24 and was used for browser verification. No compiler changes were
made by this application migration.

Local verification passed 84 focused JavaScript tests, the TypeScript check,
and 52 native model tests. Room entry and restoration pass in Chromium,
Firefox, and WebKit. Fleet placement, scrolling, random, reset, and ready pass
in Firefox and WebKit; Chromium failed its first drag during the initial run
and passed when rerun alone with tracing. The intermittent failure's cause is
unconfirmed; the first run overlapped native compilation tests.

The application migration remains incomplete: authored JavaScript and CSS
still remain. Keep reporting compiler blockers and stopping if a new compiler
issue appears. Always rebuild the current checkout before compatibility checks;
the version string alone does not establish freshness.

## Previous blocker: staged local dependency paths (resolved)

With freshly rebuilt compiler checkout `53bee4c48` on November 22, native
application packaging failed to resolve the Angular dependency from the web
staging directory:

```text
terlc build local path dependency `angular` cannot be resolved:
/home/anatoly/Applications/battleship/_build/.terlan/application/web.staging/../ng/angular.ts/integrations/terlan
(No such file or directory (os error 2))
```

The declared path `../ng/angular.ts/integrations/terlan` exists relative to the
application root. The latest build succeeds with that same declaration and no
application path workaround.

The backend Make target now supplies `TERLAN_JS_BINDING_MANIFESTS`, as required
by the Angular integration package. Application sources use explicit calls in
place of retired pipeline syntax and grouped `when` for refutable bindings.

## Previous blocker: OperationTimeout (resolved)

Before compiler commit `53bee4c48`, the unchanged
`client/battleship/client/OperationTimeout.terl` passed type checking but failed
JavaScript emission with `error[js_emit_unsupported]` at gate `J0.3`.
It creates timeout callbacks over generated `TimerState[JsNumber]` state and
uses `Some`/`None` to manage the timer ID. The failure was reproduced with a
fresh compiler build from `034841901` at 16:38 on November 22.

Commit `53bee4c48` fixes JavaScript option casts with lexical payloads. The
module now builds without an application change, and all four existing timeout
tests pass with the rebuilt compiler.
