test_that("partitioning preserves bootstrap dependencies from fluidPage", {
  ns <- shiny::NS("form")
  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        6,
        bindTextInput(ns, "title", "Title", "A", update = "input")
      ),
      shiny::column(6, shiny::p("preview"))
    )
  )

  partitioned <- partition_ui(ui, ns)
  dep_names <- vapply(htmltools::findDependencies(partitioned$ui), `[[`, character(1), "name")

  expect_true("bootstrap" %in% dep_names)
})

test_that("inputs gallery partitions into stable controls and preview column", {
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
      shell <- htmltools::renderTags(output$ui)$html
      preview <- htmltools::renderTags(output$shinystate_auto_preview_1)$html
      expect_match(shell, "mock-session-title")
      expect_match(shell, "Interactive controls")
      expect_match(shell, "Checkbox group")
      expect_match(preview, "Live preview")
      expect_match(preview, "col-sm-6")
      expect_match(preview, "table")
    }
  )
})

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
      html <- htmltools::renderTags(output$shinystate_auto_preview_1)$html
      expect_match(html, "ABC")
    }
  )
})

test_that("fluidPage layout live-updates preview while editing", {
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
      session$setInputs(".shinystate_editing" = TRUE)
      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "Hello"))
      session$flushReact()
      preview_html <- htmltools::renderTags(output$shinystate_auto_preview_1)$html
      expect_match(preview_html, "Hello")

      session$setInputs(".shinystate_event" = list(id = "notes", t = 2, value = "World"))
      session$flushReact()
      preview_html <- htmltools::renderTags(output$shinystate_auto_preview_1)$html
      expect_match(preview_html, "Hello | World")
    }
  )
})

test_that("auto preview updates while typing controls stay stable", {
  cmp <- component(
    id = "form",
    state = useState(title = "A"),
    render = function(state, ns) {
      useInput("title")
      tagList(
        bindTextInput(ns, "title", "Title", state$title, update = "input"),
        shiny::p(state$title, class = "title-preview")
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
      preview_html <- htmltools::renderTags(output$shinystate_auto_preview_1)$html
      expect_match(controls_before, "mock-session-title")
      expect_equal(controls_before, controls_after)
      expect_match(preview_html, "AB")
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
