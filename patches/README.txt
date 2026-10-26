Angular Wave 0.32.0 input and computed-watch compatibility

Source fixes and regression tests live in ../ng/angular.ts:
  src/core/compile/compile.ts and compile.spec.ts
  src/core/scope/scope.ts and scope.spec.ts

Object input identity compares both parent and destination values after deProxy.
Scope storage unwraps proxies; comparing raw objects with scoped proxies otherwise
reschedules unchanged inputs indefinitely.

Computed member watches collect both container and selector dependencies, rather
than treating the expression as a dotted property path. Expressions such as
owner.placements[ship.id] then observe container replacement and selector changes.

The Terlan validator tools/AngularCompatibilityTest.terl backports these fixes to
the pinned ESM and modular bundles. npm postinstall and Make web builds run the
validator. It accepts the pinned original or repaired code and fails on unknown
versions. Its pure patch tests verify that repeated application is harmless.
Application source imports the ordinary package.

The fleet-setup browser regression checks nested inputs, drag placement, scrolling,
Random, Reset, and Ready in Chromium, Firefox, and WebKit.
