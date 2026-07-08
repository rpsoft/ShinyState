test_that("useState returns spec outside render context", {
  spec <- useState(page = 1L, filter = NULL)
  expect_s3_class(spec, "shinystate_state_spec")
  expect_equal(spec$initial, list(page = 1L, filter = NULL))
})

test_that("effect returns spec", {
  spec <- effect(deps = "page", function(state) NULL)
  expect_s3_class(spec, "shinystate_effect_spec")
  expect_equal(spec$deps, "page")
})

test_that("state store reads and writes", {
  store <- new_state_store(list(count = 0L))
  expect_equal(state_get(store, "count"), 0L)
  state_set(store, count = 5L)
  expect_equal(state_get(store, "count"), 5L)
})

test_that("state accessor proxies values", {
  store <- new_state_store(list(count = 3L))
  bumped <- FALSE
  state <- make_state_accessor(store, function() {
    bumped <<- TRUE
  })
  expect_equal(state$count, 3L)
  state$set(count = 10L)
  expect_equal(state$count, 10L)
  expect_true(bumped)
})

test_that("useMemo caches by dependency values", {
  store <- new_state_store(list(x = 1L))
  calls <- 0L
  ctx <- new_hook_context("test", store, function() NULL)
  ctx$in_render <- TRUE

  result1 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useMemo(
      function() {
        calls <<- calls + 1L
        state_get(store, "x") * 2L
      },
      deps = "x"
    )
  })

  expect_equal(result1, 2L)
  expect_equal(calls, 1L)

  result2 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useMemo(
      function() {
        calls <<- calls + 1L
        state_get(store, "x") * 2L
      },
      deps = "x"
    )
  })

  expect_equal(result2, 2L)
  expect_equal(calls, 1L)

  state_set(store, x = 5L)

  result3 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useMemo(
      function() {
        calls <<- calls + 1L
        state_get(store, "x") * 2L
      },
      deps = "x"
    )
  })

  expect_equal(result3, 10L)
  expect_equal(calls, 2L)
})

test_that("useReducer dispatches actions", {
  store <- new_state_store(list())
  ctx <- new_hook_context("test", store, function() NULL)
  ctx$in_render <- TRUE

  reducer <- function(state, action) {
    if (action == "inc") state + 1L else state
  }

  reduced <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useReducer(reducer, 0L)
  })

  expect_equal(reduced$state, 0L)
  reduced$dispatch("inc")

  reduced2 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useReducer(reducer, 0L)
  })
  expect_equal(reduced2$state, 1L)
})

test_that("run_effects handles first run without subscript error", {
  store <- new_state_store(list(count = 0L))
  ctx <- new_hook_context("test", store, function() NULL)
  ctx$effect_specs <- list(list(
    fn = function(state) NULL,
    deps = "count"
  ))
  state <- make_state_accessor(store, function() NULL)
  expect_no_error(run_effects(ctx, state, is_first_run = TRUE))
})

test_that("useCallback increments once per click", {
  counter <- component(
    id = "counter",
    state = useState(count = 0L),
    render = function(state, ns) {
      useCallback("inc", function(s) s$set(count = s$count + 1L))
      shiny::h3(paste("Count:", state$count))
    }
  )

  shiny::testServer(
    counter$server,
    {
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "inc", t = 1))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "Count: 1")
      expect_no_match(html, "Count: 2")
    }
  )
})

test_that("useInput updates state from event value", {
  counter <- component(
    id = "inputs",
    state = useState(title = "A"),
    render = function(state, ns) {
      useInput("title")
      shiny::p(state$title)
    }
  )

  shiny::testServer(
    counter$server,
    {
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "B"))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, ">B<")
    }
  )
})

test_that("hook-style useState seeds state at any hook slot", {
  cmp <- component(
    id = "hooky",
    render = function(state, ns) {
      useMemo(function() 1L, deps = character(0))
      s <- useState(count = 42L)
      shiny::p(paste("Count:", s$count))
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "Count: 42")
    }
  )
})

test_that("multiple useState calls seed missing fields without overwriting", {
  store <- new_state_store(list())
  ctx <- new_hook_context("test", store, function() NULL)
  ctx$in_render <- TRUE

  with_hook_context(ctx, {
    reset_hook_index(ctx)
    s1 <- useState(a = 1L)
    s1$set(a = 5L)
    s2 <- useState(a = 99L, b = 2L)
    expect_equal(s2$a, 5L)
    expect_equal(s2$b, 2L)
  })
})

test_that("declarative effects and hook useEffect do not collide", {
  decl_runs <- 0L
  hook_runs <- 0L

  cmp <- component(
    id = "fx",
    state = useState(count = 0L),
    effect(
      deps = "count",
      function(state) {
        decl_runs <<- decl_runs + 1L
      }
    ),
    render = function(state, ns) {
      useEffect(function() {
        hook_runs <<- hook_runs + 1L
      }, deps = "count")
      useCallback("inc", function(s) s$set(count = s$count + 1L))
      shiny::p(state$count)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      expect_equal(decl_runs, 1L)
      expect_equal(hook_runs, 1L)

      session$setInputs(".shinystate_event" = list(id = "inc", t = 1))
      session$flushReact()
      expect_equal(decl_runs, 2L)
      expect_equal(hook_runs, 2L)
    }
  )
})

test_that("useReducer preserves NULL state without re-initializing", {
  store <- new_state_store(list())
  ctx <- new_hook_context("test", store, function() NULL)
  ctx$in_render <- TRUE

  reducer <- function(state, action) {
    if (action == "clear") NULL else action
  }

  r1 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useReducer(reducer, "start")
  })
  expect_equal(r1$state, "start")
  r1$dispatch("clear")

  r2 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useReducer(reducer, "start")
  })
  expect_null(r2$state)
  r2$dispatch("hello")

  r3 <- with_hook_context(ctx, {
    reset_hook_index(ctx)
    useReducer(reducer, "start")
  })
  expect_equal(r3$state, "hello")
})

test_that("computed values are cached and invalidate on state change", {
  calls <- 0L
  cmp <- component(
    id = "c",
    state = useState(count = 2L),
    computed = list(
      doubled = function(state) {
        calls <<- calls + 1L
        state$count * 2L
      }
    ),
    render = function(state) {
      shiny::p(paste(state$doubled, state$doubled))
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "4 4")
      expect_equal(calls, 1L) # two reads in one render, computed once

      session$setInputs(".shinystate_event" = list(id = "noop", t = 1))
      session$flushReact()
    }
  )
})

test_that("watch fires with new and old values on change", {
  seen <- NULL
  cmp <- component(
    id = "w",
    state = useState(count = 0L),
    render = function(state) {
      watch("count", function(new, old) {
        seen <<- list(new = new$count, old = old$count)
      })
      useCallback("inc", function(s) s$set(count = s$count + 1L))
      shiny::p(state$count)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      expect_null(seen) # not immediate

      session$setInputs(".shinystate_event" = list(id = "inc", t = 1))
      session$flushReact()
      expect_equal(seen$new, 1L)
      expect_equal(seen$old, 0L)
    }
  )
})

test_that("watch immediate runs once on mount with NULL old values", {
  seen <- NULL
  cmp <- component(
    id = "w2",
    state = useState(count = 5L),
    render = function(state) {
      watch("count", function(new, old) {
        seen <<- list(new = new$count, old = old$count)
      }, immediate = TRUE)
      shiny::p(state$count)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      expect_equal(seen$new, 5L)
      expect_null(seen$old)
    }
  )
})

test_that("onMounted runs once and onActivated/onDeactivated track dormancy", {
  mounted <- 0L
  activated <- 0L
  deactivated <- 0L

  cmp <- component(
    id = "life",
    state = useState(x = 1L),
    render = function(state) {
      onMounted(function() mounted <<- mounted + 1L)
      onActivated(function() activated <<- activated + 1L)
      onDeactivated(function() deactivated <<- deactivated + 1L)
      shiny::p(state$x)
    }
  )

  active <- shiny::reactiveVal(TRUE)
  shiny::testServer(
    function(input, output, session) {
      cmp$server(input, output, session, is_active = shiny::reactive(active()))
    },
    {
      session$flushReact()
      htmltools::renderTags(output$ui)
      expect_equal(mounted, 1L)
      expect_equal(activated, 1L)

      active(FALSE)
      session$flushReact()
      expect_equal(deactivated, 1L)

      active(TRUE)
      session$flushReact()
      htmltools::renderTags(output$ui)
      expect_equal(activated, 2L)
      expect_equal(mounted, 1L) # onMounted does not re-fire
    }
  )
})

test_that("changing hook order between renders warns once", {
  cmp <- component(
    id = "order",
    state = useState(flip = FALSE, n = 0L),
    render = function(state) {
      if (state$flip) {
        useMemo(function() 1L, deps = character(0))
      }
      useCallback("go", function(s) s$set(flip = TRUE))
      shiny::p(state$n)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      expect_warning(
        {
          session$setInputs(".shinystate_event" = list(id = "go", t = 1))
          session$flushReact()
        },
        "different order"
      )
    }
  )
})

test_that("stable hook order does not warn", {
  cmp <- component(
    id = "stable",
    state = useState(n = 0L),
    render = function(state) {
      useMemo(function() state$n * 2L, deps = "n")
      useCallback("inc", function(s) s$set(n = s$n + 1L))
      shiny::p(state$n)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      expect_no_warning({
        session$setInputs(".shinystate_event" = list(id = "inc", t = 1))
        session$flushReact()
      })
    }
  )
})

test_that("component returns ui and server", {
  cmp <- component(
    id = "c",
    state = useState(n = 0L),
    render = function(state, ns) {
      shiny::div(state$n)
    }
  )
  expect_true(is.function(cmp$ui))
  expect_true(is.function(cmp$server))
  expect_equal(cmp$initial_state, list(n = 0L))
})
