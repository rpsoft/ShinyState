test_that("dormant component does not render or handle events", {
  calls <- 0L
  counter <- component(
    id = "counter",
    state = useState(count = 0L),
    effect(
      deps = "count",
      function(state) {
        calls <<- calls + 1L
      }
    ),
    render = function(state, ns) {
      useCallback("inc", function(s) s$set(count = s$count + 1L))
      shiny::h3(paste("Count:", state$count))
    }
  )

  active <- shiny::reactiveVal(FALSE)
  active_reactive <- shiny::reactive(active())

  shiny::testServer(
    function(input, output, session) {
      counter$server(input, output, session, is_active = active_reactive)
    },
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_equal(calls, 0L)
      expect_false(grepl("Count:", html))

      active(TRUE)
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_equal(calls, 1L)
      expect_match(html, "Count: 0")

      session$setInputs(".shinystate_event" = list(id = "inc", t = 1))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "Count: 1")

      active(FALSE)
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "inc", t = 2))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_false(grepl("Count: 2", html))
    }
  )
})

test_that("dormant transition runs effect cleanup", {
  cleaned <- FALSE
  cmp <- component(
    id = "panel",
    state = useState(x = 1L),
    effect(
      deps = NULL,
      function(state) {
        function() {
          cleaned <<- TRUE
        }
      }
    ),
    render = function(state, ns) {
      shiny::p(state$x)
    }
  )

  active <- shiny::reactiveVal(TRUE)
  active_reactive <- shiny::reactive(active())

  shiny::testServer(
    function(input, output, session) {
      cmp$server(input, output, session, is_active = active_reactive)
    },
    {
      session$flushReact()
      expect_false(cleaned)
      active(FALSE)
      session$flushReact()
      expect_true(cleaned)
    }
  )
})

test_that("serve_dormant wakes only active tab component", {
  counter_calls <- 0L
  other_calls <- 0L

  counter <- component(
    id = "counter",
    state = useState(count = 0L),
    effect(
      deps = "count",
      function(state) {
        counter_calls <<- counter_calls + 1L
      }
    ),
    render = function(state, ns) {
      shiny::p(state$count)
    }
  )
  other <- component(
    id = "other",
    state = useState(label = "idle"),
    effect(
      deps = "label",
      function(state) {
        other_calls <<- other_calls + 1L
      }
    ),
    render = function(state, ns) {
      shiny::p(state$label)
    }
  )

  shiny::testServer(
    function(input, output, session) {
      serve_dormant(
        session = session, input = input, output = output,
        navbar = "pages",
        dashboard = counter,
        search = other
      )
    },
    {
      session$setInputs(pages = "dashboard")
      session$flushReact()
      expect_equal(counter_calls, 1L)
      expect_equal(other_calls, 0L)

      session$setInputs(pages = "search")
      session$flushReact()
      expect_equal(counter_calls, 1L)
      expect_equal(other_calls, 1L)
    }
  )
})
