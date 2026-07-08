test_that("componentUI emits a shinystate-output container and dependency", {
  ui <- componentUI("form")()
  html <- as.character(ui)
  expect_match(html, "shinystate-output")
  expect_match(html, 'id="form-ui"')
})

test_that("inputs gallery renders controls and live summary into output$ui", {
  skip_if_not_installed("jsonlite")
  path <- system.file("examples/inputs-gallery/app.R", package = "ShinyState")
  skip_if_not(nzchar(path), "inputs-gallery example not installed")

  cmp <- local({
    source(path, local = TRUE)
    inputs_gallery
  })

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "mock-session-title")
      expect_match(html, "Interactive controls")
      expect_match(html, "Checkbox group")
      expect_match(html, "Live preview")
    }
  )
})

test_that("inputs gallery example sources without error", {
  skip_if_not_installed("jsonlite")
  path <- system.file("examples/inputs-gallery/app.R", package = "ShinyState")
  skip_if_not(nzchar(path), "inputs-gallery example not installed")
  expect_no_error(source(path, local = TRUE))
})

test_that("bindTextInput warns once that debounce_ms is deprecated but still renders", {
  ns <- shiny::NS("form")
  expect_warning(
    tag <- bindTextInput(ns, "title", "Title", "Hello", update = "input", debounce_ms = 200),
    "deprecated"
  )
  html <- as.character(tag)
  expect_match(html, "form-title")
  expect_match(html, "shinystate_editing")
})

test_that("text input updates on blur without double increment", {
  cmp <- component(
    id = "form",
    state = useState(title = "A"),
    render = function(state, ns) {
      useInput("title")
      tagList(
        bindTextInput(ns, "title", "Title", state$title, update = "blur"),
        shiny::p(state$title, class = "title-preview")
      )
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "ABC"))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "ABC")
    }
  )
})

test_that("live text edits re-render output$ui with the new value", {
  cmp <- component(
    id = "form",
    state = useState(title = "A", notes = "B"),
    render = function(state, ns) {
      useInput("title")
      useInput("notes")
      fluidPage(
        fluidRow(
          column(
            6,
            bindTextInput(ns, "title", "Title", state$title, update = "input"),
            bindTextArea(ns, "notes", "Notes", state$notes, update = "input")
          ),
          column(
            6,
            shiny::p(paste(state$title, "|", state$notes), class = "live-preview")
          )
        )
      )
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "Hello"))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "Hello")

      session$setInputs(".shinystate_event" = list(id = "notes", t = 2, value = "World"))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "Hello | World")
    }
  )
})
