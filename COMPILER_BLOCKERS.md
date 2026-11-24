# Compiler compatibility status

## Current build blocker: shared target inference in the Angular package

The latest `make compile` reaches the Angular integration library build and
fails at Makefile's `terlan-build-browser` target:

```text
terlc build target inference error: explicit target `js.shared` cannot satisfy browser-only typed evidence
```

The failing command is:

```sh
TERLAN_JS_BINDING_MANIFESTS="$PWD/_build/bindings/angular/std/js/manifests/std_js_bindings.json:$PWD/_build/bindings/host/std/js/manifests/std_js_bindings.json" \
  /home/anatoly/Applications/terlan/terlan/target/debug/terlc build \
  ../ng/angular.ts/integrations/terlan/src --target js.shared \
  --out-dir _build/browser --cache-dir .terlan --declarations
```

The integration working tree now includes `src/terlan/angular/Runtime.terl`,
which imports the generated Angular interface and exposes an injector facade.
The package Makefile still builds this `src` library as `js.shared`, with its
browser library built separately. The precise source of the browser evidence
has not been established. No target override or application workaround was
introduced. Work stopped at this new compatibility issue as requested.

The full log is `/tmp/battleship-service-application-compile.log`. A complete
application build passed earlier in this work, before the subsequent Angular
package changes and final application API updates. That earlier executable was
not treated as verification of the final source.

## Resolved: typed Angular service registration

Verified November 24, 2026, with the current compiler working tree and local
AngularTS 0.37.0 at `../ng/angular.ts`. The compiler, VM, and native worker were
rebuilt together. Bindings were regenerated from that checkout's declarations.

The public `NgModule.service` operation is now generated with its callback,
constructor, and dependency-annotated array alternatives preserved. The compiler
uses checked callable references for broad rest signatures. They preserve the
original function at runtime and cannot be created from arbitrary data or
`Dynamic`. No handwritten Angular declarations or custom runtime shim were added.

Two related generator/resolver bugs were fixed:

- A union containing a generic alias was incorrectly interpreted as one generic
  instance, producing malformed members and type arguments.
- A module exporting a same-named trait lost its ordinary function imports.
  This hid the checked callable conversion functions. Grouped and aliased
  imports now work while trait-only import behavior is preserved.

`probes/battleship/probes/AngularRegistration.terl` is now a positive probe. It
registers a checked factory and an annotated factory through the generated
public API. Its factory type has an explicit callable bound; it does not accept
an unconstrained arbitrary value.

```sh
TERLAN_JS_BINDING_MANIFESTS="$PWD/_build/bindings/angular/std/js/manifests/std_js_bindings.json:$PWD/_build/bindings/host/std/js/manifests/std_js_bindings.json" \
  /home/anatoly/Applications/terlan/terlan/target/debug/terlc build \
  probes/battleship/probes/AngularRegistration.terl --target js.shared \
  --out-dir _build/probes --cache-dir /tmp/battleship-service-probe-cache
```

That build passes. Local compiler validation passed 33 TypeScript binding tests,
the new mixed trait/module import regression, and both existing collapsed import
checks. The Angular package integration checks also exercised real service
registration, constructors, annotations, and lifecycle hooks successfully in
Chromium, Firefox, and WebKit. These results precede the separate shared-target
failure above; they do not establish complete Angular binding coverage.

Logs:

- `/tmp/battleship-typescript-binding-tests.log`
- `/tmp/battleship-callable-import-test.log`
- `/tmp/battleship-trait-import-compat-tests.log`
- `/tmp/battleship-service-probe.log`
- `/tmp/battleship-service-angular-integration-chromium.log`
- `/tmp/battleship-service-angular-integration-other-browsers.log`

## Application compatibility updates

Battleship uses `file:../ng/angular.ts`, not an older published runtime.
The application now calls `createModule`, replacing Angular's removed `module`
API. Its Terlan HTTP configuration uses the Fetch `credentials: "same-origin"`
setting with a literal type, replacing the retired `withCredentials` default.
PostCSS is declared directly because the application loads it through
`postcss.config.cjs` and Autoprefixer.

The four focused application/configuration tests and `tsc --noEmit` pass.
Fleet placement and room restoration remain to be rerun after resolving the
current package build failure. The first browser run identified the removed
`module` call and was stopped; it is not reported as passing.

## Earlier compiler failures (resolved)

- An earlier November 24 compiler rebuild failed with Rust errors E0432, E0433,
  and E0599 in `ts_parser_adapter/conditional_types.rs`. Subsequent rebuilds of
  all three binaries pass without changing compiler source for those errors.
- The November 22 staged relative dependency failure resolved with the current
  compiler. The Angular dependency remains relative to the application root;
  no path workaround was introduced.
- The `OperationTimeout.terl` JavaScript emission failure resolved in compiler
  commit `53bee4c48`, without an application workaround.

Authored JavaScript and CSS still remain, so the complete migration is not
finished. Continue to rebuild the latest compiler before compatibility checks
and report new compiler blockers rather than bypassing them.
