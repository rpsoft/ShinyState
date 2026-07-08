#' Build a Shiny app from components
#'
#' Assembles a complete [shiny::shinyApp()] from one or more [component()]s,
#' wiring up the UI and server for you.
#'
#' * **One component** (named or not) becomes a single-page app: the component
#'   mounted in a [shiny::fluidPage()], served with [serve()].
#' * **Several named components** become a multi-page app: one
#'   [shiny::tabPanel()] per component in a [shiny::navbarPage()], served with
#'   [serve_dormant()] so hidden pages stay dormant. Component names become tab
#'   `value`s (and, prettified, tab labels). With `routing = "hash"` the active
#'   tab syncs with a `#!/page` URL.
#'
#' @param ... Components created with [component()]. For a multi-page app, name
#'   them (`dashboard = counter, search = filter`); names become page ids.
#' @param title App title, shown in the navbar / title panel.
#' @param routing `"hash"` (default) enables `#!/page` URL routing for
#'   multi-page apps; `"none"` disables it. Ignored for single-page apps.
#'
#' @return A [shiny::shinyApp()] object.
#'
#' @seealso [component()], [serve_dormant()], [router_server()]
#' @export
#'
#' @examples
#' \dontrun{
#' shinyStateApp(
#'   title = "My App",
#'   dashboard = counter_component,
#'   search = filter_component
#' )
#' }
shinyStateApp <- function(..., title = NULL, routing = c("hash", "none")) {
  routing <- match.arg(routing)
  components <- list(...)

  if (length(components) == 0L) {
    rlang::abort("`shinyStateApp()` requires at least one component.")
  }
  for (i in seq_along(components)) {
    cmp <- components[[i]]
    if (!is.list(cmp) || !is.function(cmp$server)) {
      rlang::abort("Every argument to `shinyStateApp()` must be a component from `component()`.")
    }
  }

  ui <- shinystate_app_ui(components, title)

  if (length(components) == 1L) {
    cmp <- components[[1L]]
    server <- function(input, output, session) {
      serve(cmp, input, output, session)
    }
  } else {
    server <- function(input, output, session) {
      do.call(
        serve_dormant,
        c(
          components,
          list(
            session = session, input = input, output = output,
            navbar = "shinystate_pages", routing = routing
          )
        )
      )
    }
  }
  shiny::shinyApp(ui = ui, server = server)
}

#' @keywords internal
shinystate_app_ui <- function(components, title = NULL) {
  if (length(components) == 1L) {
    return(shiny::fluidPage(
      if (!is.null(title)) shiny::titlePanel(title),
      mount(components[[1L]])
    ))
  }

  nms <- names(components)
  if (is.null(nms) || any(!nzchar(nms))) {
    rlang::abort(
      "Multi-page `shinyStateApp()` requires every component to be named (names become page ids)."
    )
  }
  tabs <- lapply(nms, function(nm) {
    shiny::tabPanel(prettify_page_name(nm), mount(components[[nm]]), value = nm)
  })
  do.call(
    shiny::navbarPage,
    c(list(title %||% "", id = "shinystate_pages"), tabs)
  )
}

#' @keywords internal
prettify_page_name <- function(name) {
  words <- strsplit(gsub("[_.-]+", " ", name), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) == 0L) {
    return(name)
  }
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}
