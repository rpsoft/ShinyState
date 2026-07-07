test_that("inputs gallery example sources without error", {
  skip_if_not_installed("jsonlite")
  path <- system.file("examples/inputs-gallery/app.R", package = "ShinyState")
  skip_if_not(nzchar(path), "inputs-gallery example not installed")
  expect_no_error(source(path, local = TRUE))
})

test_that("bindTextInput accepts debounce_ms and renders", {
  cmp <- component(
    id = "form",
    state = useState(title = "Hello"),
    render = function(state, ns) {
      useInput("title")
      bindTextInput(ns, "title", "Title", state$title, update = "input", debounce_ms = 200)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "mock-session-title")
      expect_match(html, "shinystate_editing")
      expect_false(grepl("render error", html, ignore.case = TRUE))
    }
  )
})

test_that("text input updates on blur without double increment", {
  cmp <- component(
    id = "form",
    state = useState(title = "A"),
    render = function(state, ns) {
      useInput("title")
      bindTextInput(ns, "title", "Title", state$title, update = "blur")
      preview(shiny::p(state$title, class = "title-preview"))
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "ABC"))
      session$flushReact()
      html <- htmltools::renderTags(output$shinystate_preview)$html
      expect_match(html, "ABC")
    }
  )
})

test_that("preview updates while controls stay stable during text editing", {
  cmp <- component(
    id = "form",
    state = useState(title = "A"),
    render = function(state, ns) {
      useInput("title")
      tagList(
        bindTextInput(ns, "title", "Title", state$title, update = "input"),
        preview(shiny::p(state$title, class = "title-preview"))
      )
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      controls_before <- htmltools::renderTags(output$ui)$html
      session$setInputs(".shinystate_editing" = TRUE)
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "AB"))
      session$flushReact()
      controls_after <- htmltools::renderTags(output$ui)$html
      preview_html <- htmltools::renderTags(output$shinystate_preview)$html
      expect_equal(controls_before, controls_after)
      expect_match(preview_html, "AB")
    }
  )
})
