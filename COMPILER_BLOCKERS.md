# Compiler compatibility status

## Current blocker: typed Angular application registration

Verified November 24, 2026, with the same freshly rebuilt compiler and generated
AngularTS 0.32.0 bindings used for the successful application build below.
`assets/application.js` registers setup and other services using
`angular.module(...).service(name, factory)`. Moving that registration into
Terlan requires the generated public `NgModule.service` operation.

The declaration graph is now followed, but registration methods are still
omitted. The generated `terlan.angular.ng.NgModule` exposes `name`, `config`,
and `component`; its skipped manifest reports:

```text
NgModule.service: ts_bindgen.unresolved_type_reference
member service references unavailable type
terlan.angular.declarations.file__40types.file_core.file_di.file_ng_2dmodule.file_ng_2dmodule.NamedServiceInjectable

Angular.module: ts_bindgen.unsupported_generic_constraint
generic method constraints must not be erased

NgModule.router: ts_bindgen.unsupported_generic_constraint
generic method constraints must not be erased
```

The installed declaration at
`node_modules/@angular-wave/angular.ts/@types/core/di/ng-module/ng-module.d.ts`
declares `service(name: string, serviceFunction: NamedServiceInjectable): this`.
`NamedServiceInjectable` includes injectable constructors and callback types.
The compiler must preserve those declaration shapes and generic constraints
through generated operations. This is a generic TypeScript binding requirement;
it does not require adding Angular-specific APIs to the compiler.

Minimal reproducer, from the application root after generating bindings:

```sh
TERLAN_JS_BINDING_MANIFESTS="$PWD/_build/bindings/angular/std/js/manifests/std_js_bindings.json:$PWD/_build/bindings/host/std/js/manifests/std_js_bindings.json" \
  /home/anatoly/Applications/terlan/terlan/target/debug/terlc check \
  probes/battleship/probes/AngularRegistration.terl \
  --cache-dir /tmp/battleship-angular-registration-cache
```

The probe fails with status 1:

```text
module `terlan.angular.ng.NgModule` has no public function `service/3`
```

The probe lives outside the application library roots and deliberately fails
until this compiler prerequisite is implemented. The existing application
still compiles and uses its JavaScript registration. Migration work stopped
at this issue as requested; no handwritten Angular declarations, dynamic
fallback, runtime shim, or compiler change was introduced.

## Application build and local verification

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
