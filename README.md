# ShinyState

React-inspired hooks for [Shiny](https://shiny.posit.co/) applications. Write components with `useState()`, `useEffect()`, `useMemo()`, and `useReducer()` instead of `reactive()`, `observe()`, and `renderUI()` yourself. Multi-page apps get a dormant tab lifecycle and hash-based URL routing out of the box.

## Install

```r
# from GitHub
devtools::install_github("rpsoft/ShinyState")

# or from a local checkout
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

Multi-page example with several components, dormant tabs, and URL routing:

```r
shiny::runApp(system.file("examples/multipage", package = "ShinyState"))
```

Shared-state example across pages/components:

```r
shiny::runApp(system.file("examples/shared-store", package = "ShinyState"))
```

Full interactive inputs gallery (text, toggles, radios, checkboxes, selects, slider, date):

```r
shiny::runApp(system.file("examples/inputs-gallery", package = "ShinyState"))
```

## Tutorial presentation

Slide deck: `inst/tutorial/shinystate-tutorial.Rmd`

```r
install.packages(c("rmarkdown", "revealjs"))
rmarkdown::render(system.file("tutorial/shinystate-tutorial.Rmd", package = "ShinyState"))
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

### Multi-page apps: dormant tabs

Give each `tabPanel()` a `value`, mount one component per tab, and call `serve_dormant()`. Hidden tabs stay **dormant** — no rendering, effects, or event handling — while their state is preserved:

```r
shinyApp(
  ui = navbarPage(
    id = "pages",
    tabPanel("Dashboard", mount(counter), value = "dashboard"),
    tabPanel("Search", mount(filter), value = "search")
  ),
  server = function(input, output, session) {
    serve_dormant(
      session = session, input = input, output = output,
      navbar = "pages",
      dashboard = counter,
      search = filter
    )
  }
)
```

### URL routing

Add `routing = "hash"` and every page gets a bookmarkable `#!/page` URL. Deep links like `https://myapp/#!/search` open on the right tab, and the browser back/forward buttons navigate between tabs:

```r
serve_dormant(
  session = session, input = input, output = output,
  navbar = "pages",
  routing = "hash",
  dashboard = counter,
  search = filter
)
```

Component names double as page names; unknown routes warn and fall back to the first component's page. Link between pages with `route_link("search", "Go to search")`. For apps that don't use the dormant lifecycle, wire routing directly with `router_server()`.

### Live preview while typing

Text inputs created with `bindTextInput(..., update = "input")` keep keyboard focus while the rest of the UI live-updates: ShinyState automatically splits the rendered UI into a stable controls shell and re-rendering preview regions. Wrap UI in `preview(...)` to mark the live region explicitly when you want manual control.

### Hooks

| Hook | Purpose |
|------|---------|
| `useState()` | Component state (declarative spec or runtime accessor) |
| `effect()` / `useEffect()` | Side effects when dependencies change |
| `useMemo()` | Memoize computed values |
| `useReducer()` | State machines via `(state, action) => new_state` |
| `useCallback()` | Wire button clicks to state updates (use with `bindButton()`) |
| `useInput()` | Wire bound inputs to state fields (use with `bind*()` helpers) |

### Components and lifecycle

| Function | Purpose |
|----------|---------|
| `component()` | Define a component (state + effects + render) |
| `mount()` | Place a component's UI in a page |
| `serve()` | Run a component's server logic |
| `serve_dormant()` | Serve tabbed components with the dormant lifecycle |
| `router_server()` / `route_link()` | Hash-based URL routing |
| `preview()` | Optional explicit live-preview region |
| `bindButton()` | Action button safe inside re-rendering components |
| `bindTextInput()`, `bindSelect()`, ... | Bound Shiny inputs safe inside `render()` |

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
6. Hidden tabs stay dormant via `serve_dormant()`; state survives tab switches.
7. With `routing = "hash"` the active tab syncs both ways with the URL hash.

You write declarative components; the package handles invalidation and re-rendering.

## License

MIT
