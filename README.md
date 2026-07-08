# ShinyState

React- and Vue-inspired components for [Shiny](https://shiny.posit.co/). Write
components with `useState()`, `useEffect()`, computed values, lifecycle hooks,
and shared stores instead of wiring `reactive()`, `observe()`, and `renderUI()`
yourself. A client-side DOM-patching renderer keeps keyboard focus, cursor, and
scroll stable across updates — no boilerplate needed to protect text inputs.

## Mental model

```
state  →  render()  →  DOM patch  →  effects
  ▲                                     │
  └───────────  set() / events  ────────┘
```

1. Each component owns private **state**.
2. `render()` returns UI from the current state (like a React/Vue render).
3. The new UI is **patched** into the DOM in place — only changed nodes move,
   so the element you're typing in is never rebuilt.
4. **Effects** run after render when their dependencies change.

## Install

```r
# from GitHub
devtools::install_github("rpsoft/ShinyState")
```

## Quick start

```r
library(shiny)
library(ShinyState)

counter <- component(
  id = "counter",
  state = useState(count = 0L),
  computed = list(doubled = function(state) state$count * 2L),
  effect(deps = "count", function(state) message("Count: ", state$count)),
  render = function(state) {
    tagList(
      h3(paste("Count:", state$count)),
      p(paste("Doubled:", state$doubled)),
      bindButton("dec", "−", onClick = function(s) s$set(count = s$count - 1L)),
      bindButton("inc", "+", onClick = function(s) s$set(count = s$count + 1L))
    )
  }
)

shinyStateApp(counter, title = "Counter")
```

`shinyStateApp()` builds the whole app. Pass several **named** components to get
a multi-page navbar app with a dormant-tab lifecycle and `#!/page` URL routing:

```r
shinyStateApp(
  title = "My App",
  routing = "hash",
  dashboard = counter,
  search = filter_component
)
```

## Bound inputs

Call `bind*()` helpers inside `render()`. They read their value from state, wire
changes back automatically, and keep focus while typing:

```r
render = function(state) {
  tagList(
    bindTextInput("title", "Title"),        # reads/writes state$title
    bindSelect("region", "Region", choices = c(EU = "eu", US = "us")),
    p(paste("Hello", state$title))          # live-updates, cursor stays put
  )
}
```

Available: `bindTextInput()`, `bindTextArea()`, `bindNumericInput()`,
`bindCheckbox()`, `bindSwitch()`, `bindRadioButtons()`, `bindCheckboxGroup()`,
`bindSelect()`, `bindSlider()`, `bindDateInput()`, and `bindButton()`.

## Hooks and lifecycle

| Function | Purpose |
|----------|---------|
| `useState()` | Component state (declarative spec or runtime accessor) |
| `computed =` | Cached derived values, read as `state$name` |
| `effect()` / `useEffect()` | Run side effects when dependencies change |
| `watch(fields, fn)` | Run `fn(new, old)` when watched fields change |
| `useMemo()` | Memoize an expensive value |
| `useReducer()` | `(state, action) => new_state` state machines |
| `useCallback()` | Register an event handler (usually via `bindButton(onClick=)`) |
| `onMounted()` / `onUnmounted()` | Run on first render / session end |
| `onActivated()` / `onDeactivated()` | Run when a dormant tab shows / hides |

## Shared stores

```r
store <- createStore(count = 0L)

panel <- component(id = "panel", render = function(state) {
  s <- useStore(store)                       # subscribes; re-renders on change
  bindButton("inc", "+", onClick = function(x) s$set(count = s$count + 1L))
})
```

Any component using `useStore(store)` re-renders when the store changes, and the
subscription is cleaned up when the session ends.

## Child components with props

```r
card <- component(id = "card", state = useState(likes = 0L),
  render = function(state) tagList(
    h4(state$name),                          # a prop from the parent
    bindButton("like", "Like", onClick = function(s) s$set(likes = s$likes + 1L))
  ))

parent <- component(id = "parent", state = useState(who = "Ada"),
  render = function(state) mount(card, props = list(name = state$who)))
```

The child's server starts once; new props re-render it while its own state
(`likes`) survives.

## Multi-page apps and routing

Passing named components to `shinyStateApp()` (or calling `serve_dormant()`
directly) gives each page its own component. Hidden tabs stay **dormant** — no
rendering, effects, or events — while keeping their state. With
`routing = "hash"`, tabs get bookmarkable `#!/page` URLs and the browser
back/forward buttons navigate between them. `route_link("search")` links between
pages; `useRoute()` reads the current route inside a component.

## Examples

```r
shiny::runApp(system.file("examples/counter", package = "ShinyState"))
shiny::runApp(system.file("examples/multipage", package = "ShinyState"))
shiny::runApp(system.file("examples/shared-store", package = "ShinyState"))
shiny::runApp(system.file("examples/children", package = "ShinyState"))
shiny::runApp(system.file("examples/inputs-gallery", package = "ShinyState"))
```

## State accessor

Inside `render()`, `state` supports:

- `state$field` — read a value (or a `computed` value)
- `state$set(field = value)` — update and re-render
- `state$update(function(current) list(...))` — functional updates
- `state$all()` — snapshot of all fields

## Debugging

`options(shinystate.debug = TRUE)` logs renders, dormant transitions, and
navigation. A warning fires if a component calls its hooks in a different order
between renders (call hooks unconditionally, every render).

## License

MIT
