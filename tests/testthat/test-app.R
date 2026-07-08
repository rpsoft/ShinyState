test_that("prettify_page_name title-cases and splits separators", {
  expect_equal(prettify_page_name("dashboard"), "Dashboard")
  expect_equal(prettify_page_name("user_settings"), "User Settings")
  expect_equal(prettify_page_name("sales.report-2024"), "Sales Report 2024")
})

test_that("single component builds a single-page app", {
  cmp <- component(
    id = "counter",
    state = useState(count = 0L),
    render = function(state) shiny::p(state$count)
  )
  app <- shinyStateApp(cmp, title = "Solo")
  expect_s3_class(app, "shiny.appobj")

  html <- as.character(shinystate_app_ui(list(cmp), title = "Solo"))
  expect_match(html, "shinystate-output")
  expect_match(html, "Solo")
})

test_that("multiple named components build a routed navbar app", {
  counter <- component(
    id = "counter",
    state = useState(count = 0L),
    render = function(state) shiny::p(state$count)
  )
  other <- component(
    id = "other",
    state = useState(label = "idle"),
    render = function(state) shiny::p(state$label)
  )

  app <- shinyStateApp(dashboard = counter, search = other, title = "Multi")
  expect_s3_class(app, "shiny.appobj")

  html <- as.character(shinystate_app_ui(list(dashboard = counter, search = other), title = "Multi"))
  expect_match(html, "shinystate_pages")
  expect_match(html, 'data-value="dashboard"')
  expect_match(html, 'data-value="search"')
  expect_match(html, "Dashboard")
  expect_match(html, "Search")
})

test_that("multi-page app requires named components", {
  a <- component(id = "a", render = function(state) shiny::p("a"))
  b <- component(id = "b", render = function(state) shiny::p("b"))
  expect_error(shinyStateApp(a, b), "named")
})

test_that("shinyStateApp rejects non-components and empty input", {
  expect_error(shinyStateApp(), "at least one")
  expect_error(shinyStateApp(list(x = 1)), "component")
})
