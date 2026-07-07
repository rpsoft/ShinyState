# ShinyState 0.6.0

## New features

* Hash-based URL routing for multi-page apps: `serve_dormant(routing = "hash")`
  syncs the active tab with a `#!/page` URL hash. Pages become bookmarkable,
  deep links (`https://myapp/#!/search`) open on the right tab, and the
  browser back/forward buttons navigate between tabs.
* New `router_server()` for wiring hash routing to any
  `navbarPage()`/`tabsetPanel()`, including apps that do not use the dormant
  lifecycle. Returns reactives `page` and `parts` (extra route segments such
  as `#!/search/term` are preserved and exposed).
* New `route_link()` renders plain `<a href="#!/page">` anchors for in-app
  navigation.

## Bug fixes

* Auto-preview slot outputs are now registered dynamically; previously only
  ten were registered and any further dynamic UI regions were silently
  dropped.
* Hook-style `useState()` now seeds missing state fields at any hook slot
  (previously only the first hook call could seed) and never overwrites
  existing values.
* Declarative `effect()` specs and hook-style `useEffect()` calls now use
  separate key spaces and can no longer overwrite each other's dependency
  tracking or cleanups.
* `useReducer()` preserves `NULL` reducer state instead of dropping it.
* `serve_dormant()` warns once per tab value that has no matching component
  and rejects arguments that are not components.
* `bindSlider()`'s value label updates live while dragging.

## Deprecations and documentation

* `debounce_ms` on `bindTextInput()`, `bindTextArea()`, and
  `bindNumericInput()` is deprecated and ignored; supplying it warns once.
* Documented the `is_active` dormant-lifecycle argument on the component
  server; removed a nonexistent parameter from the `componentUI()` docs.

# ShinyState 0.5.0

* New `serve_dormant()`: components on hidden tabs stay dormant (no
  rendering, effects, or event handling) while their state is preserved.
* Component servers accept an optional `is_active` reactive controlling the
  dormant lifecycle.

# ShinyState 0.4.0

* Automatic UI splitting: layouts containing typing controls are partitioned
  so text inputs keep focus while the rest of the UI live-updates.
* Optional `preview()` marks an explicit live-preview region.
* Bootstrap/html dependencies are preserved when partitioning `fluidPage()`
  layouts.

# ShinyState 0.3.1

* Live updates for bound inputs and initial preview support.

# ShinyState 0.2.0

* Bound input helpers (`bindTextInput()`, `bindSelect()`, `bindSlider()`,
  `bindCheckbox()`, and friends) plus the inputs-gallery example.

# ShinyState 0.1.0

* Initial release: `component()`, `useState()`, `effect()`/`useEffect()`,
  `useMemo()`, `useReducer()`, `useCallback()`, `mount()`/`serve()`.
