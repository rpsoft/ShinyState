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
