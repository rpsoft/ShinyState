# ShinyState 0.7.0

A major release that reworks the rendering model and adds a full
component/lifecycle system. **This release contains breaking API changes.**

## New features

* **DOM-patching renderer.** Components now render into a single
  `div.shinystate-output` that a custom Shiny output binding patches in place
  (like a virtual DOM) instead of replacing wholesale. Keyboard focus, cursor
  position, text selection, and scroll survive re-renders automatically. Opt a
  subtree out of patching with `data-shinystate-preserve`.
* **Zero-boilerplate bound inputs.** `bind*()` helpers are called inside
  `render()` with no `ns` argument, read their value from state, and register
  their state-write handler automatically. `bindButton()` takes an inline
  `onClick`.
* **`shinyStateApp()`** builds a complete app from components: a single
  component becomes a one-page app; several named components become a routed
  multi-page navbar app.
* **Shared stores.** `createStore()` / `useStore()` provide cross-component
  state with automatic subscription and cleanup.
* **Computed values.** `component(..., computed = list(name = function(state)))`
  exposes cached derived values as `state$name`.
* **Lifecycle hooks.** `onMounted()`, `onUnmounted()`, `onActivated()`,
  `onDeactivated()` (the latter two map to dormant tab wake/sleep).
* **`watch(fields, fn, immediate)`** runs `fn(new, old)` on field changes.
* **Child components.** `mount(child, props = list(...))` inside a parent's
  `render()` nests a component and passes reactive props.
* **`useRoute()`** reads the active hash route from any component.
* **Guardrails.** A warning fires when hooks are called in a different order
  between renders; `options(shinystate.debug = TRUE)` logs renders, dormant
  transitions, and navigation.

## Breaking changes

* `bind*()` and `bindButton()` no longer take a leading `ns` argument and must
  be called inside `render()`; argument order changed and `value` now defaults
  to the state field. `debounce_ms` is removed.
* `preview()` is removed — the DOM-patching renderer makes explicit preview
  regions unnecessary; put dynamic content anywhere in `render()`.
* Components render into one patched container; the auto-preview partition
  slots (`shinystate_auto_preview_N`) and the `.refresh_controls` flag are
  gone.
* `mount()` gained a `props` argument.

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
