test_that("partition_ui creates one slot per dynamic batch beyond ten", {
  ns <- shiny::NS("big")
  parts <- list()
  for (i in seq_len(12)) {
    parts[[length(parts) + 1L]] <- bindTextInput(ns, paste0("f", i), paste("Field", i), "x", update = "input")
    parts[[length(parts) + 1L]] <- shiny::p(paste0("dyn-", i))
  }
  ui <- htmltools::tagList(!!!parts)

  partitioned <- partition_ui(ui, ns)
  expect_length(partitioned$slots, 12L)
  expect_true("shinystate_auto_preview_12" %in% names(partitioned$slots))

  extracted <- extract_ui_slots(ui)
  expect_identical(names(extracted), names(partitioned$slots))
})

test_that("auto preview renders and live-updates slots beyond ten", {
  cmp <- component(
    id = "big",
    state = useState(title = "T"),
    render = function(state, ns) {
      useInput("title")
      parts <- list()
      for (i in seq_len(12)) {
        parts[[length(parts) + 1L]] <- bindTextInput(
          ns, paste0("f", i), paste("Field", i), state$title,
          update = "input"
        )
        parts[[length(parts) + 1L]] <- shiny::p(paste0("dyn-", i, "-", state$title))
      }
      htmltools::tagList(!!!parts)
    }
  )

  shiny::testServer(
    cmp$server,
    {
      session$flushReact()
      session$flushReact()
      html11 <- htmltools::renderTags(output$shinystate_auto_preview_11)$html
      html12 <- htmltools::renderTags(output$shinystate_auto_preview_12)$html
      expect_match(html11, "dyn-11-T")
      expect_match(html12, "dyn-12-T")

      session$setInputs(".shinystate_event" = list(id = "title", t = 1, value = "Z"))
      session$flushReact()
      html12 <- htmltools::renderTags(output$shinystate_auto_preview_12)$html
      expect_match(html12, "dyn-12-Z")
    }
  )
})
