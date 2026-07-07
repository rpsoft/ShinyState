# ShinyState

React-inspired hooks for [Shiny](https://shiny.posit.co/) applications. Write components with `useState()`, `useEffect()`, `useMemo()`, and `useReducer()` instead of `reactive()`, `observe()`, and `renderUI()` yourself.

## Install

```r
# from source
devtools::install_local("path/to/ShinyState")
```

## Quick start

```r
library(shiny)
library(ShinyState)

counter <- component(
  id = "counter",
  state = useState(count = 0L),
  effect(
    deps = "count",
    function(state) {
      message("Count is now: ", state$count)
    }
  ),
  render = function(state, ns) {
    useCallback("inc", function(s) s$set(count = s$count + 1L))
    useCallback("dec", function(s) s$set(count = s$count - 1L))

    tagList(
      h3(paste("Count:", state$count)),
      bindButton(ns, "dec", "−"),
      bindButton(ns, "inc", "+")
    )
  }
)

shinyApp(
  ui = fluidPage(mount(counter)),
  server = function(input, output, session) serve(counter, input, output, session)
)
```

Run the bundled example:

```r
shiny::runApp(system.file("examples/counter", package = "ShinyState"))
```

Multi-page example with several components:

```r
shiny::runApp(system.file("examples/multipage", package = "ShinyState"))
```

Shared-state example across pages/components:

```r
shiny::runApp(system.file("examples/shared-store", package = "ShinyState"))
```

## API overview

### `component()`

Defines a Shiny module with declarative state, effects, and a render function:

```r
component(
  id = "table",
  state = useState(page = 1L, filter = NULL),
  effect(
    deps = "filter",
    function(state) {
      state$set(page = 1L)   # reset page when filter changes
    }
  ),
  render = function(state, ns) {
    # return htmltools / shiny UI
  }
)
```

### Hooks

| Hook | Purpose |
|------|---------|
| `useState()` | Component state (declarative spec or runtime accessor) |
| `effect()` / `useEffect()` | Side effects when dependencies change |
| `useMemo()` | Memoize computed values |
| `useReducer()` | State machines via `(state, action) => new_state` |
| `useCallback()` | Wire button clicks to state updates (use with [bindButton()]) |
| `bindButton()` | Action button safe inside re-rendering components |

### State accessor

Inside `render()`, `state` supports:

- `state$field` — read a value
- `state$set(field = value)` — update and re-render
- `state$update(function(current) list(...))` — functional updates
- `state$all()` — snapshot of all fields

## How it works

ShinyState hides Shiny's reactive primitives behind a hook runtime:

1. Each component owns a private state store (an environment).
2. `render()` runs inside a hook context (like React's render pass).
3. State changes bump an internal invalidation counter, triggering `renderUI()`.
4. Effects run after render when their dependency snapshot changes.
5. `useCallback()` registers `observeEvent` handlers once per input.

You write declarative components; the package handles invalidation and re-rendering.

## License

MIT
