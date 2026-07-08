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

test_that("bind helpers abort when called outside a render context", {
  expect_error(bindTextInput("title", "Title"), "render")
  expect_error(bindButton("go", "Go"), "render")
})

test_that("bound text input auto-binds and updates on blur", {
  cmp <- component(
    id = "form",
    state = useState(title = "A"),
    render = function(state) {
      tagList(
        bindTextInput("title", "Title", update = "blur"),
        shiny::p(state$title, class = "title-preview")
      )
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, 'value="A"')
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "ABC"))
      session$flushReact()
      html <- htmltools::renderTags(output$ui)$html
      expect_match(html, "ABC")
    }
  )
})

test_that("bound inputs read current value from state and re-render on change", {
  cmp <- component(
    id = "form",
    state = useState(title = "A", notes = "B"),
    render = function(state) {
      fluidPage(
        fluidRow(
          column(
            6,
            bindTextInput("title", "Title"),
            bindTextArea("notes", "Notes")
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
